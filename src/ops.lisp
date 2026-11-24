;;;; Image operations (wave 2): resize, fit, crop, rotate.
;;;;
;;;; The operations are eager and functional. They never modify IMAGE and use no
;;;; global state other than *IMAGE-LIMITS* and the backend registry. They see the
;;;; stored pixels (types.lisp: interleaved, row-major, top-left first, no stride,
;;;; straight alpha). ORIENTATION is not applied; call APPLY-ORIENTATION first, or
;;;; decode with :ORIENT T. The four public operations are generic functions with
;;;; methods on IMAGE, so another argument has no applicable method.
;;;;
;;;; Common rules:
;;;;   - IMAGE is checked before the arguments. A pixel vector whose length is not
;;;;     width × height × (CHANNELS format) signals IMAGE-CORRUPT. A DEPTH other
;;;;     than 8 signals IMAGE-UNSUPPORTED-FEATURE, with FORMAT the pixel format and
;;;;     FEATURE (:DEPTH depth).
;;;;   - Then each argument is checked in order with CHECK-TYPE: a TYPE-ERROR with a
;;;;     STORE-VALUE restart.
;;;;   - When the result would equal IMAGE (rotate by a multiple of 360, crop of the
;;;;     whole image, resize to the same size, fit when IMAGE already fits), the
;;;;     result is IMAGE itself (EQ), as with CONVERT-IMAGE and APPLY-ORIENTATION.
;;;;   - Otherwise the result is a COPY-IMAGE of IMAGE with new WIDTH, HEIGHT and a
;;;;     fresh PIXELS vector. It has IMAGE's type (an IMAGE-FRAME stays a frame), and
;;;;     every other slot is carried over unchanged: FORMAT, DEPTH, ORIENTATION,
;;;;     ICC-PROFILE, GAMMA, COLOR-SPACE and the frame slots.
;;;;
;;;; CROP-IMAGE image x y width height → the WIDTH × HEIGHT region whose top-left
;;;; pixel is IMAGE's pixel (X, Y): result pixel (i, j) is IMAGE's (X + i, Y + j).
;;;; X and Y are INTEGERs; WIDTH and HEIGHT are (INTEGER 1). With W × H the size of
;;;; IMAGE, the region must satisfy 0 <= X, 0 <= Y, X + WIDTH <= W and
;;;; Y + HEIGHT <= H. Otherwise CROP-IMAGE signals IMAGE-INVALID-REGION with REGION
;;;; (x y width height) and BOUNDS (W H), inside two restarts:
;;;;   CLIP-REGION  Crop the intersection of the region and the image. It is offered
;;;;                only when that intersection is not empty.
;;;;   USE-VALUE    Crop another region, a list (x y width height). The region is
;;;;                checked like the original one, so a bad one signals again.
;;;; CLIP-REGION is also a function, like USE-VALUE: it invokes the restart of that
;;;; name, or returns NIL when none is active.
;;;;
;;;; ROTATE-IMAGE image degrees → IMAGE turned DEGREES clockwise. DEGREES is a
;;;; ROTATION-DEGREES: an integer multiple of 90, taken modulo 360, so -90 is 270
;;;; and 450 is 90. Anything else (45, 90.0, 1/2, "90") is a TYPE-ERROR. With W × H
;;;; the size of IMAGE, the result is H × W for 90 and 270, W × H for 180, and its
;;;; pixel (x, y) is IMAGE's pixel:
;;;;    90   (y, H-1-x)        the same as APPLY-ORIENTATION with orientation 6
;;;;   180   (W-1-x, H-1-y)    orientation 3
;;;;   270   (W-1-y, x)        orientation 8
;;;;
;;;; RESIZE-IMAGE image width height &key (filter :lanczos3) (limits *image-limits*)
;;;; backend → IMAGE resampled to WIDTH × HEIGHT, both (INTEGER 1). The aspect ratio
;;;; is not kept; FIT-IMAGE keeps it. FILTER is a RESIZE-FILTER, one of the keys of
;;;; +RESIZE-FILTERS+; LIMITS is an IMAGE-LIMITS; BACKEND is NIL, :PROTOCOL or an
;;;; IMAGE-BACKEND. In this order: the common checks (IMAGE, then WIDTH, HEIGHT,
;;;; FILTER, LIMITS, BACKEND); an IMAGE with zero width or height
;;;; signals IMAGE-UNSUPPORTED-FEATURE with FEATURE :EMPTY; (CHECK-DIMENSIONS width
;;;; height (CHANNELS format) :LIMITS limits) may signal IMAGE-LIMIT-EXCEEDED, before
;;;; anything is allocated or any backend runs; then the identity case.
;;;;
;;;;   Dispatch. A backend that resizes natively lists (filter :RESIZE priority) in
;;;;   BACKEND-FORMATS, one entry per filter it implements, and specializes
;;;;   BACKEND-RESIZE. The protocol's method on T is the resampler defined below.
;;;;     BACKEND NIL (default)  The registered backends that list FILTER for
;;;;                            :RESIZE, best first (priority, *BACKEND-PREFERENCE*,
;;;;                            then registration order, as for decoding), then the
;;;;                            protocol's resampler.
;;;;     BACKEND :PROTOCOL      The protocol's resampler only.
;;;;     an IMAGE-BACKEND       Its BACKEND-RESIZE, or the method on T when it has
;;;;                            none.
;;;;   When a backend signals an IMAGE-ERROR, RESIZE-IMAGE offers the restarts
;;;;   TRY-NEXT-BACKEND (the next candidate; the last one is the protocol's
;;;;   resampler) and USE-VALUE (a backend to use instead), as DECODE-IMAGE does.
;;;;   BACKEND-RESIZE receives checked arguments, never the identity case, and a
;;;;   straight-alpha IMAGE. It returns a fresh octet vector of WIDTH × HEIGHT
;;;;   pixels in IMAGE's format, with straight alpha, having resampled formats with
;;;;   alpha premultiplied. RESIZE-IMAGE builds the result from it (common rules);
;;;;   a vector of the wrong length signals IMAGE-CORRUPT.
;;;;
;;;;   The protocol's resampler gives the same octets as Pillow 13.0
;;;;   Image.resize((width, height), filter) with no reducing_gap, for every filter
;;;;   except :NEAREST (tests/fixtures/PROVENANCE.md). With W × H the source size:
;;;;
;;;;   :NEAREST. Result pixel (x, y) is the source pixel (sx, sy) under its centre,
;;;;     with exact rational arithmetic:
;;;;       sx = floor((2x + 1) W / (2 width)),  sy = floor((2y + 1) H / (2 height))
;;;;     Every channel, alpha included, is copied unchanged.
;;;;
;;;;   Other filters: a separable convolution in 22-bit fixed point. Floating-point
;;;;   values are DOUBLE-FLOATs. Evaluate each expression in the order written,
;;;;   rounding every operation as IEEE 754 does (no fused multiply-add). sin is the
;;;;   double-float sine, pi the double-float π. The kernel K and its support S:
;;;;     :BOX       S 0.5  K(x) = 1 if -0.5 < x <= 0.5, else 0
;;;;     :BILINEAR  S 1    K(x) = 1 - |x| if |x| < 1, else 0
;;;;     :BICUBIC   S 2    with a = |x|: ((1.5a - 2.5)a)a + 1 if a < 1,
;;;;                       (((a - 5)a + 8)a - 4)(-0.5) if a < 2, else 0
;;;;                       (Keys' cubic convolution with a = -0.5)
;;;;     :LANCZOS3  S 3    K(x) = sinc(x) sinc(x / 3) if -3 <= x < 3, else 0, where
;;;;                       sinc(0) = 1 and otherwise sinc(t) = sin(u) / u, u = t pi
;;;;   Coefficients for one axis, from N source samples to M destination samples:
;;;;     scale = N / M (a double), fs = max(scale, 1), support = S fs, inv = 1 / fs.
;;;;     For each destination index i, 0 <= i < M:
;;;;       center = (i + 0.5) scale
;;;;       lo     = max(0, truncate(center - support + 0.5))
;;;;       hi     = min(N, truncate(center + support + 0.5))
;;;;       w_j    = K(((lo + j) - center + 0.5) inv), for j = 0 … hi - lo - 1
;;;;       total  = w_0 + w_1 + …, added left to right; when total /= 0, each w_j
;;;;                becomes w_j / total
;;;;       q_j    = truncate(w_j 2^22 + 0.5) if w_j >= 0, else truncate(w_j 2^22 - 0.5)
;;;;   A pass along the axis sets each channel of destination sample i to the
;;;;   integer clamp(floor((2^21 + Σ_j q_j s_(lo+j)) / 2^22), 0, 255), where s is the
;;;;   same channel of the source samples along that row (horizontal pass) or column
;;;;   (vertical pass).
;;;;   Passes: when width /= W, a horizontal pass over every row (W → width); when
;;;;   height /= H, a vertical pass over every column (H → height). The horizontal
;;;;   pass runs first, unless H - height > 0 and H - height > 2 (W - width); then
;;;;   the vertical pass runs first. Each pass stores octets, so the second pass
;;;;   reads rounded, clamped samples.
;;;;   Alpha (:GRAY-ALPHA, :RGBA): first each colour sample c of a pixel with alpha a
;;;;   is premultiplied, c' = floor((floor(t / 256) + t) / 256) with t = c a + 128
;;;;   (c a / 255, rounded). Then every channel, alpha included, is resampled as
;;;;   above. Last each colour sample c' of a pixel whose new alpha is a is divided
;;;;   back: c' itself if a is 0 or 255, else min(255, floor(255 c' / a)).
;;;;
;;;; FIT-DIMENSIONS width height &key max-width max-height → (values width' height'):
;;;; the size FIT-IMAGE resizes to. WIDTH and HEIGHT are (INTEGER 1); each bound is
;;;; NIL (no bound) or an (INTEGER 1). When WIDTH <= MAX-WIDTH and
;;;; HEIGHT <= MAX-HEIGHT (a NIL bound always holds), the size is unchanged: images
;;;; are never enlarged. Otherwise, with exact rationals rw = MAX-WIDTH / WIDTH and
;;;; rh = MAX-HEIGHT / HEIGHT (no ratio for a NIL bound):
;;;;   rw <= rh, or no rh:  MAX-WIDTH × max(1, floor(HEIGHT × MAX-WIDTH / WIDTH + 1/2))
;;;;   otherwise:           max(1, floor(WIDTH × MAX-HEIGHT / HEIGHT + 1/2)) × MAX-HEIGHT
;;;; The result never exceeds a bound. 4000 × 3000 into 1500 × 1500 is 1500 × 1125;
;;;; 1080 × 1920 into 1000 × 1000 is 563 × 1000.
;;;;
;;;; FIT-IMAGE image &key max-width max-height (filter :lanczos3) backend → IMAGE
;;;; itself when it fits, else (RESIZE-IMAGE image w h :FILTER filter :BACKEND
;;;; backend) with (w h) from FIT-DIMENSIONS. Every argument is checked (common
;;;; rules: the bounds as for FIT-DIMENSIONS, FILTER and BACKEND as for
;;;; RESIZE-IMAGE) even when IMAGE fits. An empty IMAGE always fits.

(in-package #:image-protocol)

(defparameter +resize-filters+
  '((:nearest . nil) (:box . 0.5d0) (:bilinear . 1d0) (:bicubic . 2d0) (:lanczos3 . 3d0))
  "Resize filter → its kernel support S in source pixels (NIL: no kernel). Data.")

(deftype resize-filter ()
  "A key of +RESIZE-FILTERS+."
  '(member :nearest :box :bilinear :bicubic :lanczos3))

(defun %right-angle-p (object)
  (and (integerp object) (zerop (mod object 90))))

(deftype rotation-degrees ()
  "An integer multiple of 90."
  '(and integer (satisfies %right-angle-p)))

(defun %ops-pending (name)
  "Contract stub for the image ops. Every call is replaced by the implementation,
then this function is deleted."
  (error "image-protocol ops pending: ~A" name))

(defgeneric resize-image (image width height &key filter limits backend)
  (:documentation "IMAGE resampled to WIDTH × HEIGHT with FILTER (see the file header)."))

(defgeneric fit-image (image &key max-width max-height filter backend)
  (:documentation "IMAGE scaled down, aspect ratio kept, to fit MAX-WIDTH × MAX-HEIGHT."))

(defgeneric crop-image (image x y width height)
  (:documentation "The WIDTH × HEIGHT region of IMAGE whose top-left pixel is (X, Y)."))

(defgeneric rotate-image (image degrees)
  (:documentation "IMAGE turned DEGREES (a multiple of 90) clockwise."))

(defgeneric backend-resize (backend image width height filter)
  (:documentation "→ a fresh octet vector: IMAGE resampled to WIDTH × HEIGHT with FILTER,
straight alpha. Backends list (filter :RESIZE priority) in BACKEND-FORMATS for the
filters they implement. The method on T is the protocol's own resampler."))

(defmethod resize-image ((image image) width height
                         &key (filter :lanczos3) (limits *image-limits*) backend)
  (declare (ignorable width height filter limits backend))
  (%ops-pending 'resize-image))

(defmethod backend-resize (backend image width height filter)
  (declare (ignorable backend image width height filter))
  (%ops-pending 'backend-resize))

(defun fit-dimensions (width height &key max-width max-height)
  "→ (values width height) of a WIDTH × HEIGHT image scaled down to fit
MAX-WIDTH × MAX-HEIGHT, aspect ratio kept (see the file header)."
  (declare (ignorable width height max-width max-height))
  (%ops-pending 'fit-dimensions))

(defmethod fit-image ((image image) &key max-width max-height (filter :lanczos3) backend)
  (declare (ignorable max-width max-height filter backend))
  (%ops-pending 'fit-image))

(defmethod crop-image ((image image) x y width height)
  (declare (ignorable x y width height))
  (%ops-pending 'crop-image))

(defmethod rotate-image ((image image) degrees)
  (declare (ignorable degrees))
  (%ops-pending 'rotate-image))

(defun clip-region (&optional condition)
  "Invoke the CLIP-REGION restart of CROP-IMAGE (for CONDITION, if given), or
return NIL when none is active, like USE-VALUE."
  (let ((restart (find-restart 'clip-region condition)))
    (when restart (invoke-restart restart))))
