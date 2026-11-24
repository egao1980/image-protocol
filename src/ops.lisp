(in-package #:image-protocol)

;;; Editing operations (image-protocol#2; the brief's image-ops wave).
;;; Eager and functional: each returns a new IMAGE and never changes its
;;; input (which comes back as is when there is nothing to do); no global
;;; state; 8-bit images. Resampling is Pillow 11.3's, sample for sample:
;;; separable convolution (the kernel widens with the reduction factor, is
;;; clipped at the edges and normalised) in 22-bit fixed point, each pass
;;; rounded to 8 bits, alpha premultiplied in 8 bits, an optional integer
;;; box pre-reduce, and NEAREST's stepped coordinates. :LINEAR T (no Pillow
;;; equivalent) runs the same steps in floats. Composite is Pillow's
;;; Porter-Duff over in 7-bit fixed point.

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
OUT-SIZE points. IN0 and IN1 are single floats, as Pillow's C box; the rest
is in doubles in Pillow's order of operations. → (values starts counts
weights ksize); WEIGHTS holds KSIZE normalised double weights per output
index."
  (declare (type single-float in0 in1))
  (let* ((scale (/ (coerce (- in1 in0) 'double-float) out-size))
         (filterscale (max scale 1d0))
         (support (* support filterscale))
         (ksize (1+ (* 2 (ceiling support))))
         (ss (/ 1d0 filterscale))
         (in0 (coerce in0 'double-float))
         (starts (make-array out-size :element-type 'fixnum))
         (counts (make-array out-size :element-type 'fixnum))
         (weights (make-array (* out-size ksize) :element-type 'double-float
                                                 :initial-element 0d0)))
    (dotimes (xx out-size)
      (let* ((center (+ in0 (* (+ xx 0.5d0) scale)))
             (xmin (max 0 (truncate (+ (- center support) 0.5d0))))
             (count (max 0 (- (min in-size (truncate (+ center support 0.5d0))) xmin)))
             (k (* xx ksize))
             (ww 0d0))
        (dotimes (i count)
          (let ((w (funcall kernel (* (+ i xmin (- center) 0.5d0) ss))))
            (setf (aref weights (+ k i)) w)
            (incf ww w)))
        (unless (zerop ww)
          (dotimes (i count)
            (setf (aref weights (+ k i)) (/ (aref weights (+ k i)) ww))))
        (setf (aref starts xx) xmin
              (aref counts xx) count)))
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

(defun %single-weights (weights)
  "WEIGHTS (doubles) as single floats, for the float passes."
  (map '%floats (lambda (w) (coerce w 'single-float)) weights))

(defconstant +resample-bits+ 22
  "Pillow's PRECISION_BITS for 8-bit resampling (32 - 8 - 2).")

(deftype %fixed-weights () '(simple-array (signed-byte 32) (*)))

(defun %fixed-weights (weights)
  "Pillow's normalize_coeffs_8bpc: WEIGHTS × 2^22, rounded half away from
zero."
  (let ((out (make-array (length weights) :element-type '(signed-byte 32)))
        (one (coerce (ash 1 +resample-bits+) 'double-float)))
    (dotimes (i (length weights) out)
      (let ((w (aref weights i)))
        (setf (aref out i) (if (< w 0d0)
                               (truncate (+ -0.5d0 (* w one)))
                               (truncate (+ 0.5d0 (* w one)))))))))

(declaim (inline %clip8))

(defun %clip8 (acc)
  "Pillow's clip8: a fixed-point sum (with its half already added) → 0–255."
  (declare (type fixnum acc))
  (let ((v (ash acc (- +resample-bits+))))
    (cond ((< v 0) 0)
          ((> v 255) 255)
          (t v))))

(defun %resample-horizontal-8 (src w h c out-w starts counts weights ksize)
  "Pillow's ImagingResampleHorizontal_8bpc: each row of the octets SRC
(W × H × C) resampled to OUT-W columns in 22-bit fixed point, rounded and
clipped to 8 bits."
  (declare (type octets src) (type %fixed-weights weights)
           (type fixnum w h c out-w ksize)
           (type (simple-array fixnum (*)) starts counts)
           (optimize speed))
  (let ((dst (make-array (* h out-w c) :element-type '(unsigned-byte 8)))
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
              (let ((acc (ash 1 (1- +resample-bits+)))
                    (p (+ base ch)))
                (declare (type fixnum acc p))
                (dotimes (i count)
                  (setf acc (the fixnum (+ acc (* (aref weights (+ k i)) (aref src p)))))
                  (incf p c))
                (setf (aref dst q) (%clip8 acc))
                (incf q)))))))))

(defun %resample-vertical-8 (src w c out-h starts counts weights ksize)
  "Pillow's ImagingResampleVertical_8bpc: the octets SRC (W × … × C)
resampled to OUT-H rows, like %RESAMPLE-HORIZONTAL-8."
  (declare (type octets src) (type %fixed-weights weights)
           (type fixnum w c out-h ksize)
           (type (simple-array fixnum (*)) starts counts)
           (optimize speed))
  (let* ((stride (* w c))
         (dst (make-array (* out-h stride) :element-type '(unsigned-byte 8)))
         (acc (make-array stride :element-type 'fixnum)))
    (declare (type fixnum stride) (type octets dst)
             (type (simple-array fixnum (*)) acc))
    (dotimes (yy out-h dst)
      (fill acc (ash 1 (1- +resample-bits+)))
      (let ((start (aref starts yy))
            (k (* yy ksize)))
        (declare (type fixnum start k))
        (dotimes (i (aref counts yy))
          (let ((weight (aref weights (+ k i)))
                (row (* (+ start i) stride)))
            (declare (type (signed-byte 32) weight) (type fixnum row))
            (dotimes (j stride)
              (setf (aref acc j)
                    (the fixnum (+ (aref acc j) (* weight (aref src (+ row j)))))))))
        (let ((out (* yy stride)))
          (declare (type fixnum out))
          (dotimes (j stride)
            (setf (aref dst (+ out j)) (%clip8 (aref acc j)))))))))

(defun %reduce-multiplier (n)
  "Pillow's division_UINT32(N, 8): 2^32 / (256 N) in single floats,
truncated."
  (values (truncate (/ 4294967296f0 (coerce (* 256 n) 'single-float)))))

(defun %box-reduce-8 (src w h c fx fy)
  "Pillow's ImagingReduce on the octets SRC (W × H × C): the mean of each
FX × FY block (a partial block at the right or bottom edge averages what it
holds) as ((sum + n/2) × division_UINT32(n, 8)) >> 24 in 32-bit unsigned
arithmetic, which every one of Pillow's reduce variants computes.
→ (values octets width height)."
  (declare (type octets src) (type fixnum w h c fx fy))
  (let* ((nw (ceiling w fx)) (nh (ceiling h fy))
         (dst (make-array (* nw nh c) :element-type '(unsigned-byte 8)))
         (full (* fx fy))
         (full-multiplier (%reduce-multiplier full)))
    (declare (type fixnum nw nh full))
    (dotimes (ny nh)
      (dotimes (nx nw)
        (let* ((x0 (* nx fx)) (x1 (min w (+ x0 fx)))
               (y0 (* ny fy)) (y1 (min h (+ y0 fy)))
               (n (* (- x1 x0) (- y1 y0)))
               (multiplier (if (= n full) full-multiplier (%reduce-multiplier n)))
               (amend (floor n 2)))
          (declare (type fixnum x0 x1 y0 y1 n amend)
                   (type (unsigned-byte 32) multiplier))
          (dotimes (ch c)
            (let ((sum amend))
              (declare (type fixnum sum))
              (loop for y of-type fixnum from y0 below y1
                    do (loop for x of-type fixnum from x0 below x1
                             do (incf sum (aref src (+ (* (+ (* y w) x) c) ch)))))
              (setf (aref dst (+ (* (+ (* ny nw) nx) c) ch))
                    (ash (logand (* sum multiplier) #xFFFFFFFF) -24)))))))
    (values dst nw nh)))

(declaim (inline %muldiv255))

(defun %muldiv255 (a b)
  "Pillow's MULDIV255: A × B / 255, rounded, in integers."
  (declare (type (unsigned-byte 8) a b))
  (let ((tmp (+ (* a b) 128)))
    (ash (+ (ash tmp -8) tmp) -8)))

(defun %premultiply-8 (src c alpha)
  "A copy of the octets SRC with colour premultiplied by the alpha at index
ALPHA of each C-sample pixel, in 8 bits (Pillow's RGBA → RGBa, LA → La)."
  (declare (type octets src) (type fixnum c alpha))
  (let ((dst (copy-seq src)))
    (declare (type octets dst))
    (loop for p of-type fixnum from 0 below (length src) by c
          do (let ((a (aref src (+ p alpha))))
               (dotimes (ch c)
                 (unless (= ch alpha)
                   (setf (aref dst (+ p ch)) (%muldiv255 (aref src (+ p ch)) a))))))
    dst))

(defun %unpremultiply-8 (buf c alpha)
  "BUF (octets, premultiplied) unpremultiplied in place as Pillow's RGBa →
RGBA: colour × 255 / alpha truncated and clipped; alpha 0 and 255 keep the
colour as it is."
  (declare (type octets buf) (type fixnum c alpha))
  (loop for p of-type fixnum from 0 below (length buf) by c
        do (let ((a (aref buf (+ p alpha))))
             (unless (or (= a 0) (= a 255))
               (dotimes (ch c)
                 (unless (= ch alpha)
                   (setf (aref buf (+ p ch)) (min 255 (floor (* 255 (aref buf (+ p ch))) a))))))))
  buf)

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

(defun %nearest-indices (in-size out-size)
  "Pillow's NEAREST source index for each of OUT-SIZE output indices
(ImagingScaleAffine): the double xo starts at scale / 2 and steps by
scale = IN-SIZE / OUT-SIZE, truncated. The rounding error of the steps sends
a centre that lands on a pixel edge to the lower pixel, as in Pillow."
  (let* ((scale (/ (coerce in-size 'double-float) out-size))
         (xo (* scale 0.5d0))
         (indices (make-array out-size :element-type 'fixnum)))
    (dotimes (x out-size indices)
      (setf (aref indices x) (min (1- in-size) (max 0 (truncate xo))))
      (incf xo scale))))

(defun %resize-nearest (image width height)
  "Pillow's NEAREST: each output pixel copies the source pixel that
%NEAREST-INDICES picks; alpha is not premultiplied."
  (let* ((w (image-width image))
         (c (channels (image-format image)))
         (src (image-pixels image))
         (xs (%nearest-indices w width))
         (ys (%nearest-indices (image-height image) height))
         (dst (make-array (* width height c) :element-type '(unsigned-byte 8))))
    (dotimes (y height)
      (let ((row (* (aref ys y) w)))
        (dotimes (x width)
          (let ((from (* (+ row (aref xs x)) c)))
            (replace dst src :start1 (* (+ (* y width) x) c) :start2 from :end2 (+ from c))))))
    (%with-pixels image width height dst)))

(defun %decimal-double (number)
  "NUMBER as the double Python reads from the decimal a Lisp user writes for
it. A double stays; a rational is rounded to the nearest double (Python's
float()); another float is read back as a double from the shortest decimal
that prints it, so the single float 1.7 (1.70000005) gives Python's 1.7,
not 1.7000000476837158d0."
  (etypecase number
    (double-float number)
    (rational (coerce number 'double-float))
    (float
     ;; The first float type NUMBER belongs to. A runtime search, because an
     ;; ETYPECASE over these types has shadowed clauses wherever two of them
     ;; are the same type (SBCL: short = single, long = double).
     (let ((text (let ((*read-default-float-format*
                        (find-if (lambda (type) (typep number type))
                                 '(short-float single-float double-float long-float))))
                   (prin1-to-string number))))
       (let ((*read-default-float-format* 'double-float)
             (*read-eval* nil))
         (coerce (read-from-string text) 'double-float))))))

(defun %reduce-factor (size out-size reducing-gap)
  "Pillow's int(size / out_size / reducing_gap) or 1, in doubles; 1 without
REDUCING-GAP. The gap is the double Python reads from the same decimal
(%DECIMAL-DOUBLE), so a quotient that is exact in Pillow is exact here."
  (if reducing-gap
      (max 1 (truncate (/ (/ (coerce size 'double-float) out-size)
                          (%decimal-double reducing-gap))))
      1))

(defun %reduced-box-side (size factor)
  "Pillow's box side after reduce(): SIZE / FACTOR in doubles, then a C
float."
  (coerce (/ (coerce size 'double-float) factor) 'single-float))

(defun %resize-8bit (image width height support kernel reducing-gap)
  "Pillow 11.3's resize() of an 8-bit image, sample for sample: RGBA and LA
are premultiplied in 8 bits (Pillow's RGBa / La); an integer box reduce
first when REDUCING-GAP leaves room; then the fixed-point horizontal and
vertical passes, each rounded to 8 bits; then unpremultiplied."
  (let* ((w (image-width image)) (h (image-height image))
         (format (image-format image))
         (c (channels format))
         (alpha (%alpha-index format))
         (buf (if alpha (%premultiply-8 (image-pixels image) c alpha) (image-pixels image)))
         (fx (%reduce-factor w width reducing-gap))
         (fy (%reduce-factor h height reducing-gap))
         (in1-x (coerce w 'single-float))
         (in1-y (coerce h 'single-float))
         (cw w) (ch h))
    (when (or (> fx 1) (> fy 1))
      (multiple-value-setq (buf cw ch) (%box-reduce-8 buf w h c fx fy))
      (setf in1-x (%reduced-box-side w fx)
            in1-y (%reduced-box-side h fy)))
    (unless (and (= width cw) (= in1-x width))
      (multiple-value-bind (starts counts weights ksize)
          (%coefficients cw 0f0 in1-x width support kernel)
        (setf buf (%resample-horizontal-8 buf cw ch c width starts counts
                                          (%fixed-weights weights) ksize)
              cw width)))
    (unless (and (= height ch) (= in1-y height))
      (multiple-value-bind (starts counts weights ksize)
          (%coefficients ch 0f0 in1-y height support kernel)
        (setf buf (%resample-vertical-8 buf cw c height starts counts
                                        (%fixed-weights weights) ksize)
              ch height)))
    (when alpha
      (%unpremultiply-8 buf c alpha))
    (%with-pixels image width height buf)))

(defun %resize-linear (image width height support kernel reducing-gap)
  "RESIZE-IMAGE with :LINEAR T (no Pillow equivalent): the same coefficients
and steps as %RESIZE-8BIT, in single floats, on premultiplied linear-light
samples, rounded to 8 bits only at the end."
  (let* ((w (image-width image)) (h (image-height image))
         (format (image-format image))
         (c (channels format))
         (buf (%image-floats image t))
         (fx (%reduce-factor w width reducing-gap))
         (fy (%reduce-factor h height reducing-gap))
         (in1-x (coerce w 'single-float))
         (in1-y (coerce h 'single-float))
         (cw w) (ch h))
    (when (or (> fx 1) (> fy 1))
      (multiple-value-setq (buf cw ch) (%box-reduce buf w h c fx fy))
      (setf in1-x (%reduced-box-side w fx)
            in1-y (%reduced-box-side h fy)))
    (unless (and (= width cw) (= in1-x width))
      (multiple-value-bind (starts counts weights ksize)
          (%coefficients cw 0f0 in1-x width support kernel)
        (setf buf (%resample-horizontal buf cw ch c width starts counts
                                        (%single-weights weights) ksize)
              cw width)))
    (unless (and (= height ch) (= in1-y height))
      (multiple-value-bind (starts counts weights ksize)
          (%coefficients ch 0f0 in1-y height support kernel)
        (setf buf (%resample-vertical buf cw ch c height starts counts
                                      (%single-weights weights) ksize)
              ch height)))
    (%with-pixels image width height (%floats-octets buf format t))))

(defun resize-image (image width height &key (filter :lanczos3) reducing-gap linear)
  "→ IMAGE resampled to WIDTH × HEIGHT, like Pillow 11.3's resize(), and
sample for sample equal to it. FILTER is one of +RESIZE-FILTERS+: :NEAREST,
:BOX, :BILINEAR, :BICUBIC or :LANCZOS3 (the default). Colour is resampled
premultiplied by alpha, so transparent pixels do not bleed. REDUCING-GAP (a
real >= 1, as Pillow's reducing_gap) first box-reduces by the integer factor
that leaves at least that much resampling to do; faster for large
reductions. A float gap is taken as the decimal it prints as, so 1.7 (a
single float) is Python's 1.7. As in Pillow, it applies only to images without alpha (Pillow
resizes the premultiplied copy of an RGBA or LA image without it) and not
to :NEAREST. LINEAR T resamples in linear light instead of sRGB, in floats
(Pillow has no equivalent). The output size is checked against
*IMAGE-LIMITS*."
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
        (let ((gap (and (null (%alpha-index format)) reducing-gap)))
          (if linear
              (%resize-linear image width height support kernel gap)
              (%resize-8bit image width height support kernel gap))))))))

(defun %thumbnail-size (w h max-width max-height)
  "Pillow's thumbnail() size: the largest size with W:H's aspect within
MAX-WIDTH × MAX-HEIGHT, never larger than W × H; NIL when W × H already
fits. The free side is the floor or the ceiling of its exact value,
whichever gives the nearer aspect in doubles (the floor on a tie), at least
1: Pillow's round_aspect, in Python's float arithmetic, so near ties go the
same way."
  (when (and (>= max-width w) (>= max-height h))
    (return-from %thumbnail-size nil))
  (let ((aspect (/ (coerce w 'double-float) h)))
    (flet ((round-aspect (number key)
             (let ((lo (floor number)) (hi (ceiling number)))
               (max 1 (if (<= (funcall key lo) (funcall key hi)) lo hi))))
           (ratio (a b) (/ (coerce a 'double-float) b)))
      (if (>= (ratio max-width max-height) aspect)
          (list (round-aspect (* max-height aspect)
                              (lambda (n) (abs (- aspect (ratio n max-height)))))
                max-height)
          (list max-width
                (round-aspect (/ max-width aspect)
                              (lambda (n) (if (zerop n) 0 (abs (- aspect (ratio max-width n)))))))))))

(defun thumbnail (image max-width max-height &key (filter :lanczos3) (reducing-gap 2))
  "→ IMAGE scaled down to fit MAX-WIDTH × MAX-HEIGHT, keeping its aspect
ratio, never enlarged (Pillow's thumbnail(), but functional: IMAGE itself
comes back when it already fits). FILTER and REDUCING-GAP (default 2, as
Pillow) go to RESIZE-IMAGE, so an image with alpha is resized without the
gap, as in Pillow. The filter defaults to :LANCZOS3 (Pillow: bicubic)."
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

(declaim (inline %shift-div255 %over-coefficient))

(defun %shift-div255 (a)
  "Pillow's SHIFTFORDIV255: A / 255 for A already offset by half."
  (declare (type (unsigned-byte 32) a))
  (ash (+ (ash a -8) a) -8))

(defun %over-coefficient (sa outa255)
  "Pillow's AlphaComposite.c weight of the source colour, in 7-bit fixed
point: SA · 255² · 2^7 / OUTA255, truncated, where OUTA255 = SA · 255 +
DA · (255 − SA) is the output alpha × 255. The blend is then
SHIFTFORDIV255(src · coef1 + dst · (255 · 2^7 − coef1) + 2^14) >> 7."
  (floor (* sa 255 255 128) outa255))

(defun composite-image (dst src &key (x 0) (y 0) (operator :over))
  "→ DST with SRC drawn over it, SRC's top-left corner at (X, Y): Porter-Duff
over on straight alpha, as Pillow 11.3's alpha_composite() and equal to it
sample for sample (its 7-bit fixed-point blend, see %OVER-COEFFICIENT). The
result has DST's format; SRC is converted to match (gray / colour, no
alpha = opaque; a DST without alpha counts as opaque). Parts of SRC outside
DST are clipped, so X and Y may be negative. OPERATOR :OVER is the only one
so far."
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
                                   (outa255 (+ (* sa 255) (* da (- 255 sa))))
                                   (coef1 (%over-coefficient sa outa255))
                                   (coef2 (- (* 255 128) coef1)))
                              (dotimes (ch colors)
                                (setf (aref out (+ dp ch))
                                      (ash (%shift-div255 (+ (* (aref spx (+ sp ch)) coef1)
                                                             (* (aref out (+ dp ch)) coef2)
                                                             (ash #x80 7)))
                                           -7)))
                              (when dalpha
                                (setf (aref out (+ dp dalpha))
                                      (%shift-div255 (+ outa255 #x80)))))))))
      (%with-pixels dst dw dh out))))
