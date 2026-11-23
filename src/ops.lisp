(in-package #:image-protocol)

;;; Editing operations (image-protocol#2; the brief's image-ops wave).
;;; Eager and functional: each returns a new IMAGE and never changes its
;;; input (which comes back as is when there is nothing to do); no global
;;; state; 8-bit images. Resampling is Pillow's separable convolution (the
;;; kernel widens with the reduction factor, is clipped at the edges and
;;; normalised), on premultiplied alpha, optionally in linear light, with an
;;; optional integer box pre-reduce. Composite is Porter-Duff over.

(defun %ops-image (image operation)
  "IMAGE, checked for OPERATION: an 8-bit IMAGE whose pixels match its size."
  (unless (image-p image)
    (error 'image-invalid-argument :operation operation :argument 'image :value image
                                   :reason "not an image"))
  (unless (eql 8 (image-depth image))
    (error 'image-unsupported-feature
           :feature (format nil "~(~A~) of a depth-~A image" operation (image-depth image))))
  (%check-image image))

(defun %bad-argument (operation argument value reason)
  (error 'image-invalid-argument :operation operation :argument argument :value value
                                 :reason reason))

(defun %with-pixels (image width height pixels)
  "A copy of IMAGE (format and metadata) with new size and PIXELS."
  (let ((out (copy-image image)))
    (setf (image-width out) width (image-height out) height (image-pixels out) pixels)
    out))

;;; Crop, rotate, flip

(defun crop-image (image x y width height)
  "→ the WIDTH × HEIGHT region of IMAGE whose top-left corner is (X, Y), like
Pillow's crop((x, y, x + width, y + height)). The region must lie inside
IMAGE; otherwise IMAGE-INVALID-ARGUMENT."
  (%ops-image image :crop)
  (let ((w (image-width image)) (h (image-height image)))
    (loop for (name value) in `((x ,x) (y ,y) (width ,width) (height ,height))
          unless (typep value '(integer 0))
            do (%bad-argument :crop name value "a non-negative integer"))
    (unless (and (<= (+ x width) w) (<= (+ y height) h))
      (%bad-argument :crop 'region (list x y width height)
                     (format nil "outside the ~D × ~D image" w h)))
    (let* ((c (channels (image-format image)))
           (src (image-pixels image))
           (dst (make-array (* width height c) :element-type '(unsigned-byte 8)))
           (row (* width c)))
      (dotimes (r height)
        (let ((start (* (+ (* (+ y r) w) x) c)))
          (replace dst src :start1 (* r row) :start2 start :end2 (+ start row))))
      (%with-pixels image width height dst))))

(defun rotate-image (image degrees)
  "→ IMAGE turned DEGREES counter-clockwise, as Pillow's rotate(degrees,
expand=True) / transpose(ROTATE_90 …). DEGREES is a multiple of 90; a
negative one turns clockwise. Other angles: IMAGE-INVALID-ARGUMENT."
  (%ops-image image :rotate)
  (unless (and (realp degrees) (zerop (mod degrees 90)))
    (%bad-argument :rotate 'degrees degrees "a multiple of 90 (other angles are not supported)"))
  (ecase (mod (round degrees) 360)
    (0 image)
    (90 (%remap-oriented image 8))
    (180 (%remap-oriented image 3))
    (270 (%remap-oriented image 6))))

(defun flip-image (image direction)
  "→ IMAGE mirrored: DIRECTION :HORIZONTAL swaps left and right (Pillow's
FLIP_LEFT_RIGHT), :VERTICAL swaps top and bottom (FLIP_TOP_BOTTOM)."
  (%ops-image image :flip)
  (case direction
    (:horizontal (%remap-oriented image 2))
    (:vertical (%remap-oriented image 4))
    (t (%bad-argument :flip 'direction direction ":horizontal or :vertical"))))

;;; Resampling

(deftype %floats () '(simple-array single-float (*)))

(defun %box-kernel (x)
  (declare (double-float x))
  (if (and (> x -0.5d0) (<= x 0.5d0)) 1d0 0d0))

(defun %triangle-kernel (x)
  (declare (double-float x))
  (let ((x (abs x)))
    (if (< x 1d0) (- 1d0 x) 0d0)))

(defun %bicubic-kernel (x)
  "Keys cubic, a = -0.5 (Pillow's BICUBIC)."
  (declare (double-float x))
  (let ((x (abs x))
        (a -0.5d0))
    (cond ((< x 1d0) (+ (* (- (* (+ a 2d0) x) (+ a 3d0)) x x) 1d0))
          ((< x 2d0) (* (- (* (+ (* (- x 5d0) x) 8d0) x) 4d0) a))
          (t 0d0))))

(defun %sinc (x)
  (declare (double-float x))
  (if (zerop x)
      1d0
      (let ((px (* (coerce pi 'double-float) x)))
        (/ (sin px) px))))

(defun %lanczos3-kernel (x)
  (declare (double-float x))
  (if (and (<= -3d0 x) (< x 3d0))
      (* (%sinc x) (%sinc (/ x 3d0)))
      0d0))

(defun %resample-filter (filter)
  "(values support kernel) of a convolution FILTER, or NIL."
  (case filter
    (:box (values 0.5d0 #'%box-kernel))
    (:bilinear (values 1d0 #'%triangle-kernel))
    (:bicubic (values 2d0 #'%bicubic-kernel))
    (:lanczos3 (values 3d0 #'%lanczos3-kernel))
    (t nil)))

(defparameter +resize-filters+ '(:nearest :box :bilinear :bicubic :lanczos3)
  "Filters RESIZE-IMAGE and THUMBNAIL accept.")

(defun %coefficients (in-size in0 in1 out-size support kernel)
  "Pillow's precompute_coeffs: sampling [IN0, IN1) of an IN-SIZE axis at
OUT-SIZE points. → (values starts counts weights ksize); WEIGHTS holds KSIZE
normalised weights per output index."
  (let* ((scale (/ (- in1 in0) out-size))
         (filterscale (max scale 1d0))
         (support (* support filterscale))
         (ksize (1+ (* 2 (ceiling support))))
         (ss (/ 1d0 filterscale))
         (starts (make-array out-size :element-type 'fixnum))
         (counts (make-array out-size :element-type 'fixnum))
         (weights (make-array (* out-size ksize) :element-type 'single-float
                                                 :initial-element 0f0))
         (k (make-array ksize :element-type 'double-float)))
    (dotimes (xx out-size)
      (let* ((center (+ in0 (* (+ xx 0.5d0) scale)))
             (xmin (max 0 (truncate (+ (- center support) 0.5d0))))
             (count (max 0 (min ksize (- (min in-size (truncate (+ center support 0.5d0)))
                                         xmin))))
             (ww 0d0))
        (dotimes (i count)
          (let ((w (funcall kernel (* (+ i xmin (- center) 0.5d0) ss))))
            (setf (aref k i) w)
            (incf ww w)))
        (setf (aref starts xx) xmin
              (aref counts xx) count)
        (dotimes (i count)
          (setf (aref weights (+ (* xx ksize) i))
                (coerce (if (zerop ww) 0d0 (/ (aref k i) ww)) 'single-float)))))
    (values starts counts weights ksize)))

(defun %resample-horizontal (src w h c out-w starts counts weights ksize)
  "Each row of SRC (W × H × C) resampled to OUT-W columns. Results are
clamped to 0–255 like Pillow's 8-bit passes, so ringing below 0 or above
255 does not carry into the next pass."
  (declare (type %floats src weights) (type fixnum w h c out-w ksize)
           (type (simple-array fixnum (*)) starts counts)
           (optimize speed))
  (let ((dst (make-array (* h out-w c) :element-type 'single-float))
        (q 0))
    (declare (type fixnum q))
    (dotimes (y h dst)
      (let ((row (* y w c)))
        (declare (type fixnum row))
        (dotimes (xx out-w)
          (let ((base (+ row (* (aref starts xx) c)))
                (count (aref counts xx))
                (k (* xx ksize)))
            (declare (type fixnum base count k))
            (dotimes (ch c)
              (let ((acc 0f0)
                    (p (+ base ch)))
                (declare (type single-float acc) (type fixnum p))
                (dotimes (i count)
                  (incf acc (* (aref weights (+ k i)) (aref src p)))
                  (incf p c))
                (setf (aref dst q) (max 0f0 (min 255f0 acc)))
                (incf q)))))))))

(defun %resample-vertical (src w h c out-h starts counts weights ksize)
  "SRC (W × H × C) resampled to OUT-H rows, a weighted sum of whole source
rows per output row; clamped to 0–255 like %RESAMPLE-HORIZONTAL."
  (declare (type %floats src weights) (type fixnum w h c out-h ksize)
           (type (simple-array fixnum (*)) starts counts)
           (optimize speed)
           (ignore h))
  (let* ((stride (* w c))
         (dst (make-array (* out-h stride) :element-type 'single-float))
         (acc (make-array stride :element-type 'single-float)))
    (declare (type fixnum stride) (type %floats dst acc))
    (dotimes (yy out-h dst)
      (fill acc 0f0)
      (let ((start (aref starts yy))
            (k (* yy ksize)))
        (declare (type fixnum start k))
        (dotimes (i (aref counts yy))
          (let ((weight (aref weights (+ k i)))
                (row (* (+ start i) stride)))
            (declare (type single-float weight) (type fixnum row))
            (dotimes (j stride)
              (incf (aref acc j) (* weight (aref src (+ row j)))))))
        (let ((out (* yy stride)))
          (declare (type fixnum out))
          (dotimes (j stride)
            (setf (aref dst (+ out j)) (max 0f0 (min 255f0 (aref acc j))))))))))

(defun %alpha-index (format)
  "Index of the alpha channel within a pixel, or NIL."
  (case format (:gray-alpha 1) (:rgba 3) (t nil)))

(defun %srgb->linear (v)
  "0–255 sRGB-encoded → 0–255 linear light."
  (let ((s (/ v 255d0)))
    (* 255d0 (if (<= s 0.04045d0) (/ s 12.92d0) (expt (/ (+ s 0.055d0) 1.055d0) 2.4d0)))))

(defun %linear->srgb (v)
  (let ((l (/ (max 0d0 (min 255d0 v)) 255d0)))
    (* 255d0 (if (<= l 0.0031308d0)
                 (* 12.92d0 l)
                 (- (* 1.055d0 (expt l (/ 1d0 2.4d0))) 0.055d0)))))

(defun %image-floats (image linear)
  "IMAGE's samples as floats 0–255, colour premultiplied by alpha (and in
linear light when LINEAR)."
  (let* ((src (image-pixels image))
         (c (channels (image-format image)))
         (alpha (or (%alpha-index (image-format image)) -1))
         (lut (make-array 256 :element-type 'single-float))
         (dst (make-array (length src) :element-type 'single-float)))
    (declare (type octets src) (type %floats lut dst) (type fixnum c alpha))
    (dotimes (v 256)
      (setf (aref lut v) (coerce (if linear (%srgb->linear v) v) 'single-float)))
    (locally (declare (optimize speed))
      (loop for p of-type fixnum from 0 below (length src) by c
            do (let ((a (if (>= alpha 0) (/ (aref src (+ p alpha)) 255f0) 1f0)))
                 (declare (type single-float a))
                 (dotimes (ch c)
                   (setf (aref dst (+ p ch))
                         (if (= ch alpha)
                             (coerce (aref src (+ p ch)) 'single-float)
                             (* a (aref lut (aref src (+ p ch))))))))))
    dst))

(defun %round-octet (v)
  (let ((v (max 0d0 (min 255d0 (coerce v 'double-float)))))
    (values (floor (+ v 0.5d0)))))

(defun %floats-octets (buf format linear)
  "Inverse of %IMAGE-FLOATS: unpremultiply, back to sRGB when LINEAR, round."
  (let* ((c (channels format))
         (alpha (or (%alpha-index format) -1))
         (out (make-array (length buf) :element-type '(unsigned-byte 8))))
    (declare (type %floats buf) (type octets out) (type fixnum c alpha))
    (flet ((octet (v)
             (declare (type single-float v))
             (the (unsigned-byte 8) (values (floor (+ (max 0f0 (min 255f0 v)) 0.5f0))))))
      (declare (inline octet))
      (locally (declare (optimize speed))
        (loop for p of-type fixnum from 0 below (length buf) by c
              do (let* ((a (if (>= alpha 0) (max 0f0 (min 255f0 (aref buf (+ p alpha)))) 255f0))
                        (scale (cond ((< alpha 0) 1f0) ((> a 0f0) (/ 255f0 a)) (t 0f0))))
                   (declare (type single-float a scale))
                   (dotimes (ch c)
                     (setf (aref out (+ p ch))
                           (cond ((= ch alpha) (octet a))
                                 (linear (octet (coerce (%linear->srgb (* scale (aref buf (+ p ch))))
                                                        'single-float)))
                                 (t (octet (* scale (aref buf (+ p ch))))))))))))
    out))

(defun %box-reduce (buf w h c fx fy)
  "BUF averaged over FX × FY blocks (partial blocks at the edges average what
they hold), as Pillow's reduce(). → (values buf width height)."
  (declare (type %floats buf) (type fixnum w h c fx fy))
  (let* ((nw (ceiling w fx)) (nh (ceiling h fy))
         (dst (make-array (* nw nh c) :element-type 'single-float)))
    (declare (type fixnum nw nh) (type %floats dst))
    (locally (declare (optimize speed))
      (dotimes (ny nh)
        (dotimes (nx nw)
          (let* ((x0 (* nx fx)) (x1 (min w (+ x0 fx)))
                 (y0 (* ny fy)) (y1 (min h (+ y0 fy)))
                 (n (* (- x1 x0) (- y1 y0))))
            (declare (type fixnum x0 x1 y0 y1 n))
            (dotimes (ch c)
              (let ((acc 0f0))
                (declare (type single-float acc))
                (loop for y of-type fixnum from y0 below y1
                      do (loop for x of-type fixnum from x0 below x1
                               do (incf acc (aref buf (+ (* (+ (* y w) x) c) ch)))))
                (setf (aref dst (+ (* (+ (* ny nw) nx) c) ch)) (/ acc n))))))))
    (values dst nw nh)))

(defun %resize-nearest (image width height)
  "Pillow's NEAREST: each output pixel copies the source pixel under its
centre."
  (let* ((w (image-width image)) (h (image-height image))
         (c (channels (image-format image)))
         (src (image-pixels image))
         (dst (make-array (* width height c) :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (let ((sy (min (1- h) (floor (* (+ y 1/2) h) height))))
        (dotimes (x width)
          (let ((sx (min (1- w) (floor (* (+ x 1/2) w) width))))
            (replace dst src :start1 (* (+ (* y width) x) c)
                             :start2 (* (+ (* sy w) sx) c) :end2 (* (1+ (+ (* sy w) sx)) c))))))
    (%with-pixels image width height dst)))

(defun resize-image (image width height &key (filter :lanczos3) reducing-gap linear)
  "→ IMAGE resampled to WIDTH × HEIGHT, like Pillow's resize(). FILTER is one
of +RESIZE-FILTERS+: :NEAREST, :BOX, :BILINEAR, :BICUBIC or :LANCZOS3 (the
default). Colour is resampled premultiplied by alpha, so transparent pixels
do not bleed. LINEAR T resamples in linear light instead of sRGB.
REDUCING-GAP (a real >= 1, as Pillow's reducing_gap) first box-reduces by
the integer factor that leaves at least that much resampling to do; faster
for large reductions. The output size is checked against *IMAGE-LIMITS*."
  (%ops-image image :resize)
  (let ((w (image-width image)) (h (image-height image))
        (format (image-format image)))
    (loop for (name value) in `((width ,width) (height ,height))
          unless (typep value '(integer 1))
            do (%bad-argument :resize name value "a positive integer"))
    (unless (member filter +resize-filters+)
      (%bad-argument :resize 'filter filter (format nil "one of ~{~S~^ ~}" +resize-filters+)))
    (unless (or (null reducing-gap) (and (realp reducing-gap) (>= reducing-gap 1)))
      (%bad-argument :resize 'reducing-gap reducing-gap "NIL or a real >= 1"))
    (when (or (zerop w) (zerop h))
      (%bad-argument :resize 'image image "an empty image cannot be resampled"))
    (check-dimensions width height (channels format))
    (cond
      ((and (= width w) (= height h)) image)
      ((eq filter :nearest) (%resize-nearest image width height))
      (t
       (multiple-value-bind (support kernel) (%resample-filter filter)
         (let* ((c (channels format))
                (buf (%image-floats image linear))
                (fx (if reducing-gap (max 1 (floor w (* width reducing-gap))) 1))
                (fy (if reducing-gap (max 1 (floor h (* height reducing-gap))) 1))
                (in1-x (coerce w 'double-float))
                (in1-y (coerce h 'double-float))
                (cw w) (ch h))
           (when (or (> fx 1) (> fy 1))
             (multiple-value-setq (buf cw ch) (%box-reduce buf w h c fx fy))
             (setf in1-x (/ w (coerce fx 'double-float))
                   in1-y (/ h (coerce fy 'double-float))))
           (unless (and (= width cw) (= in1-x cw))
             (multiple-value-bind (starts counts weights ksize)
                 (%coefficients cw 0d0 in1-x width support kernel)
               (setf buf (%resample-horizontal buf cw ch c width starts counts weights ksize)
                     cw width)))
           (unless (and (= height ch) (= in1-y ch))
             (multiple-value-bind (starts counts weights ksize)
                 (%coefficients ch 0d0 in1-y height support kernel)
               (setf buf (%resample-vertical buf cw ch c height starts counts weights ksize)
                     ch height)))
           (%with-pixels image width height (%floats-octets buf format linear))))))))

(defun %thumbnail-size (w h max-width max-height)
  "Pillow's thumbnail() size: the largest size with W:H's aspect within
MAX-WIDTH × MAX-HEIGHT (each side rounded to the nearer aspect, at least 1),
never larger than W × H; NIL when W × H already fits."
  (when (and (>= max-width w) (>= max-height h))
    (return-from %thumbnail-size nil))
  (let ((aspect (/ w h)))
    (flet ((round-aspect (number key)
             (max 1 (if (<= (funcall key (floor number)) (funcall key (ceiling number)))
                        (floor number)
                        (ceiling number)))))
      (if (>= (/ max-width max-height) aspect)
          (list (round-aspect (* max-height aspect)
                              (lambda (n) (abs (- aspect (/ n max-height)))))
                max-height)
          (list max-width
                (round-aspect (/ max-width aspect)
                              (lambda (n) (if (zerop n) 0 (abs (- aspect (/ max-width n)))))))))))

(defun thumbnail (image max-width max-height &key (filter :lanczos3) (reducing-gap 2))
  "→ IMAGE scaled down to fit MAX-WIDTH × MAX-HEIGHT, keeping its aspect
ratio, never enlarged (Pillow's thumbnail(), but functional: IMAGE itself
comes back when it already fits). FILTER and REDUCING-GAP (default 2, as
Pillow) go to RESIZE-IMAGE."
  (%ops-image image :thumbnail)
  (loop for (name value) in `((max-width ,max-width) (max-height ,max-height))
        unless (typep value '(integer 1))
          do (%bad-argument :thumbnail name value "a positive integer"))
  (when (or (zerop (image-width image)) (zerop (image-height image)))
    (return-from thumbnail image))
  (let ((size (%thumbnail-size (image-width image) (image-height image) max-width max-height)))
    (if size
        (resize-image image (first size) (second size) :filter filter :reducing-gap reducing-gap)
        image)))

(defun fit-image (image &key max-width max-height enlarge (filter :lanczos3) (reducing-gap 2))
  "→ IMAGE scaled to fit MAX-WIDTH and / or MAX-HEIGHT (at least one),
keeping its aspect ratio: the scale is the smallest the given sides allow,
each new side rounded half up and at least 1. Smaller images are enlarged
only when ENLARGE; IMAGE itself comes back when the scale is 1. FILTER and
REDUCING-GAP go to RESIZE-IMAGE (image-protocol#3: sizing photos for
vision-model input, e.g. :MAX-WIDTH 1536 :MAX-HEIGHT 1536)."
  (%ops-image image :fit)
  (unless (or max-width max-height)
    (%bad-argument :fit 'max-width max-width "give :max-width, :max-height or both"))
  (loop for (name value) in `((max-width ,max-width) (max-height ,max-height))
        unless (or (null value) (typep value '(integer 1)))
          do (%bad-argument :fit name value "NIL or a positive integer"))
  (let ((w (image-width image)) (h (image-height image)))
    (when (or (zerop w) (zerop h))
      (return-from fit-image image))
    (let* ((scale (min (if max-width (/ max-width w) most-positive-fixnum)
                       (if max-height (/ max-height h) most-positive-fixnum)))
           (scale (if enlarge scale (min scale 1))))
      (if (= scale 1)
          image
          (resize-image image
                        (max 1 (floor (+ (* w scale) 1/2)))
                        (max 1 (floor (+ (* h scale) 1/2)))
                        :filter filter :reducing-gap reducing-gap)))))

;;; Composite

(defun composite-image (dst src &key (x 0) (y 0) (operator :over))
  "→ DST with SRC drawn over it, SRC's top-left corner at (X, Y): Porter-Duff
over on straight alpha, as Pillow's alpha_composite(). The result has DST's
format; SRC is converted to match (gray / colour, no alpha = opaque). Parts
of SRC outside DST are clipped, so X and Y may be negative. OPERATOR :OVER
is the only one so far."
  (%ops-image dst :composite)
  (%ops-image src :composite)
  (unless (eq operator :over)
    (%bad-argument :composite 'operator operator "only :over is supported"))
  (loop for (name value) in `((x ,x) (y ,y))
        unless (integerp value)
          do (%bad-argument :composite name value "an integer"))
  (let* ((dfmt (image-format dst))
         (dw (image-width dst)) (dh (image-height dst))
         (dc (channels dfmt))
         (colors (if (member dfmt '(:rgb :rgba)) 3 1))
         (dalpha (%alpha-index dfmt))
         (s (convert-image src (if (= colors 3) :rgba :gray-alpha)))
         (sw (image-width s)) (sh (image-height s))
         (sc (1+ colors))
         (spx (image-pixels s))
         (x0 (max 0 x)) (x1 (min dw (+ x sw)))
         (y0 (max 0 y)) (y1 (min dh (+ y sh))))
    (when (or (>= x0 x1) (>= y0 y1))
      (return-from composite-image dst))
    (let ((out (copy-seq (image-pixels dst))))
      (loop for py from y0 below y1
            do (loop for px from x0 below x1
                     do (let* ((sp (* (+ (* (- py y) sw) (- px x)) sc))
                               (dp (* (+ (* py dw) px) dc))
                               (sa (aref spx (+ sp colors))))
                          (unless (zerop sa)
                            (let* ((da (if dalpha (aref out (+ dp dalpha)) 255))
                                   (rest (* da (- 255 sa)))
                                   (a255 (+ (* sa 255) rest)))
                              ;; out = (src·sa·255 + dst·da·(255 − sa)) / a255, rounded
                              (dotimes (ch colors)
                                (setf (aref out (+ dp ch))
                                      (floor (+ (* 2 (+ (* (aref spx (+ sp ch)) sa 255)
                                                        (* (aref out (+ dp ch)) rest)))
                                                a255)
                                             (* 2 a255))))
                              (when dalpha
                                (setf (aref out (+ dp dalpha))
                                      (floor (+ (* 2 a255) 255) 510))))))))
      (%with-pixels dst dw dh out))))
