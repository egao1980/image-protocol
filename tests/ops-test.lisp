;;;; Image ops (wave 2): resize, fit, crop, rotate. Rules: src/ops.lisp.
;;;; Fixture vectors come from tests/fixtures/generate-ops.py, which computes each
;;;; row twice, with an independent transcription of the rules and with Pillow
;;;; 13.0.0, and stops unless they agree (tests/fixtures/PROVENANCE.md lists the
;;;; two places where our rule differs from Pillow on purpose). Hand-written rows
;;;; were checked the same way.

(in-package #:image-protocol/tests)

;;; Helpers -------------------------------------------------------------------

(defun fixture (name)
  "Read the single top-level list in tests/fixtures/NAME."
  (with-open-file (in (asdf:system-relative-pathname "image-protocol/tests"
                                                     (format nil "tests/fixtures/~A" name))
                      :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package '#:image-protocol/tests)))
        (read in)))))

(defun report (bad)
  (format nil "~D mismatches~@[, first: ~S~]" (length bad)
          (and bad (subseq bad 0 (min 4 (length bad))))))

(defun lcg-octets (n seed)
  "N octets from the LCG s ← (1103515245 s + 12345) mod 2^31, octet = bits 16–23."
  (let ((out (make-array n :element-type '(unsigned-byte 8))) (s seed))
    (dotimes (i n out)
      (setf s (mod (+ (* s 1103515245) 12345) (expt 2 31))
            (aref out i) (ldb (byte 8 16) s)))))

(defparameter +alpha-steps+ '(0 1 64 128 254 255))

(defun pattern-image (format kind w h seed)
  ":NOISE: every octet from the LCG. :ALPHA-STEPS: the same, but the alpha of
pixel p is (nth (mod p 6) +alpha-steps+)."
  (let* ((c (img:channels format))
         (px (lcg-octets (* w h c) seed)))
    (when (eq kind :alpha-steps)
      (dotimes (p (* w h))
        (setf (aref px (+ (* p c) c -1)) (nth (mod p 6) +alpha-steps+))))
    (img:make-image :width w :height h :format format :pixels px)))

(defun byte-grid (format w h)
  "W × H image of FORMAT whose octet i is i mod 256."
  (let ((n (* w h (img:channels format))))
    (img:make-image :width w :height h :format format
                    :pixels (octets (loop for i below n collect (mod i 256))))))

(defun image-of (format w h list)
  (img:make-image :width w :height h :format format :pixels (octets list)))

(defun fnv1a (octets)
  "32-bit FNV-1a of OCTETS."
  (let ((h 2166136261))
    (loop for b across octets
          do (setf h (ldb (byte 32 0) (* (logxor h b) 16777619))))
    h))

(defun px (image) (coerce (img:image-pixels image) 'list))

(defun shape (image)
  (list (img:image-width image) (img:image-height image) (img:image-format image)))

(defun signals-type (thunk type)
  "True when THUNK signals a condition of TYPE (any other condition: false)."
  (handler-case (progn (funcall thunk) nil)
    (condition (c) (typep c type))))

(defun condition-of (thunk)
  "The condition THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (condition (c) c)))

(defun call (function &rest args)
  "APPLY FUNCTION to ARGS at run time, so that a deliberately ill-typed argument
reaches the type check instead of a compile-time warning."
  (apply function args))

(defun rows-mismatching (rows test)
  "Rows for which TEST returns false or signals."
  (remove-if (lambda (row) (ignore-errors (funcall test row))) rows))

;;; A backend that resizes: answers FILL-OCTET everywhere, counts its calls.

(defclass resize-backend (img:image-backend)
  ((filters :initarg :filters :initform '(:bilinear) :reader backend-filters)
   (priority :initarg :priority :initform 60 :reader priority)
   (fill-octet :initarg :fill-octet :initform 7 :reader fill-octet)
   (calls :initform 0 :accessor calls)))

(defmethod img:backend-formats ((b resize-backend))
  (loop for f in (backend-filters b) collect (list f :resize (priority b))))

(defmethod img:backend-resize ((b resize-backend) image width height filter)
  (declare (ignore filter))
  (incf (calls b))
  (make-array (* width height (img:channels (img:image-format image)))
              :element-type '(unsigned-byte 8) :initial-element (fill-octet b)))

(defclass other-resize-backend (resize-backend) ())

(defclass failing-resize-backend (resize-backend) ())
(defmethod img:backend-resize ((b failing-resize-backend) image width height filter)
  (declare (ignore image width height filter))
  (incf (calls b))
  (error 'img:image-error :backend b))

(defclass short-resize-backend (resize-backend) ())
(defmethod img:backend-resize ((b short-resize-backend) image width height filter)
  (declare (ignore image filter))
  (incf (calls b))
  (make-array (1- (* width height)) :element-type '(unsigned-byte 8)))

(defun all-octets-p (image value)
  (every (lambda (b) (= b value)) (img:image-pixels image)))

;;; Resize --------------------------------------------------------------------

(deftest-parametrize resize-golden
    ((filter) :ids ("nearest" "box" "bilinear" "bicubic" "lanczos3")
     (:nearest) (:box) (:bilinear) (:bicubic) (:lanczos3))
  ;; 56 rows per filter: 4 formats × 12 sizes, alpha steps, two larger goldens.
  (let* ((rows (remove filter (fixture "ops-resize.sexp") :key #'second :test-not #'eq))
         (bad (with-backends ()
                (rows-mismatching
                 rows
                 (lambda (row)
                   (destructuring-bind (format f kind w h seed nw nh sum agrees pixels) row
                     (declare (ignore f agrees))
                     (let ((out (img:resize-image (pattern-image format kind w h seed) nw nh
                                                  :filter filter)))
                       (and (equal (shape out) (list nw nh format))
                            (= sum (fnv1a (img:image-pixels out)))
                            (or (null pixels) (equal pixels (px out)))))))))))
    (ok (= 56 (length rows)))
    (ok (null bad) (report bad))))

(deftest-parametrize resize-exact
    ((format filter w h source nw nh expected)
     ;; Box 2:1 is the mean, halves rounded up.
     (:gray :box 4 1 '(0 10 20 31) 2 1 '(5 26))
     (:gray :bilinear 2 1 '(0 255) 4 1 '(0 64 191 255))
     (:gray :bicubic 2 1 '(0 255) 4 1 '(0 53 202 255))
     ;; Lanczos rings; the window is renormalised at the edges.
     (:gray :lanczos3 4 1 '(0 0 255 255) 8 1 '(10 0 0 54 201 255 255 245))
     (:gray :lanczos3 8 1 '(0 0 0 0 255 255 255 255) 3 1 '(0 127 255))
     (:gray :nearest 2 2 '(1 2 3 4) 4 4 '(1 1 2 2 1 1 2 2 3 3 4 4 3 3 4 4))
     (:rgb :nearest 3 1 '(1 2 3 4 5 6 7 8 9) 2 1 '(1 2 3 7 8 9))
     ;; Ties go right: (2x + 1) 2 / 14 = 1 at x = 3 (Pillow gives 10 there).
     (:gray :nearest 2 1 '(10 20) 7 1 '(10 10 10 20 20 20 20))
     ;; Premultiplied: transparent blue adds no blue to the blend.
     (:rgba :bilinear 2 1 '(255 0 0 255 0 0 255 0) 3 1 '(255 0 0 255 255 0 0 128 0 0 0 0))
     ;; Premultiplied: 200 at alpha 255 with 0 at alpha 0 is 199 at alpha 128, not 100.
     (:gray-alpha :box 2 1 '(200 255 0 0) 1 1 '(199 128))
     (:gray-alpha :nearest 2 1 '(200 255 0 0) 1 1 '(0 0))
     (:gray :bilinear 1 1 '(77) 3 2 '(77 77 77 77 77 77))
     ;; Height only: one vertical pass.
     (:gray :lanczos3 3 1 '(0 255 0) 3 2 '(0 255 0 0 255 0))
     (:rgb :bicubic 2 2 '(0 0 0 255 255 255 255 0 0 0 0 255) 3 3
      '(0 0 0 128 136 128 255 255 255 128 0 0 128 64 128 128 128 255 255 0 0 128 0 128 0 0 255)))
  (with-backends ()
    (let ((out (img:resize-image (image-of format w h source) nw nh :filter filter)))
      (ok (equal (shape out) (list nw nh format)))
      (ok (equal expected (px out))))))

(deftest-parametrize resize-dimensions
    ((w h nw nh)
     (100 50 30 40)                     ; the aspect ratio is not kept
     (8 8 1 16)
     (5 7 5 3)
     (3 9 12 1)
     (1 1 1 9)
     (17 3 2 2))
  (with-backends ()
    (dolist (format img:pixel-formats)
      (dolist (filter '(:nearest :bilinear :lanczos3))
        (let ((out (img:resize-image (pattern-image format :noise w h 3) nw nh :filter filter)))
          (ok (equal (shape out) (list nw nh format)) (format nil "~A ~A" format filter))
          (ok (= (* nw nh (img:channels format)) (length (img:image-pixels out)))))))))

(deftest-parametrize resize-keeps-constant-images
    ((filter) :ids ("nearest" "box" "bilinear" "bicubic" "lanczos3")
     (:nearest) (:box) (:bilinear) (:bicubic) (:lanczos3))
  ;; Weights sum to 2^22 within rounding, so a flat opaque image stays flat:
  ;; every colour sample 200, every alpha 255 (premultiplying by 255 is exact).
  (with-backends ()
    (dolist (format img:pixel-formats)
      (dolist (size '((5 4 9 2) (9 2 5 4) (6 6 6 1) (3 3 13 13)))
        (destructuring-bind (w h nw nh) size
          (let* ((c (img:channels format))
                 (alpha (member format '(:gray-alpha :rgba)))
                 (src (img:make-image :width w :height h :format format
                                      :pixels (make-array (* w h c) :element-type '(unsigned-byte 8)
                                                                    :initial-element 200))))
            (when alpha
              (dotimes (p (* w h)) (setf (aref (img:image-pixels src) (+ (* p c) c -1)) 255)))
            (let ((out (ignore-errors (img:resize-image src nw nh :filter filter))))
              (ok (and out
                       (equal (shape out) (list nw nh format))
                       (loop for i below (* nw nh c)
                             always (= (aref (img:image-pixels out) i)
                                       (if (and alpha (= (mod i c) (1- c))) 255 200))))
                  (format nil "~A ~A" format size)))))))))

(deftest resize-identity-and-copies
  (let ((mock (make-instance 'resize-backend :filters '(:lanczos3))))
    (with-backends (mock)
      (let* ((src (pattern-image :rgb :noise 5 4 11))
             (before (copy-seq (img:image-pixels src))))
        (ok (eq src (img:resize-image src 5 4)))
        (ok (eq src (img:resize-image src 5 4 :filter :nearest)))
        (ok (= 0 (calls mock)))
        (let ((out (img:resize-image src 3 2 :backend :protocol)))
          (ok (not (eq out src)))
          (ok (not (eq (img:image-pixels out) (img:image-pixels src))))
          (ok (equalp before (img:image-pixels src)) "the source is unchanged"))))))

(deftest resize-carries-metadata
  (with-backends ()
    (let* ((src (pattern-image :rgba :noise 6 4 5))
           (icc (octets '(1 2 3))))
      (setf (img:image-orientation src) 6 (img:image-icc-profile src) icc
            (img:image-gamma src) 0.45 (img:image-color-space src) :display-p3)
      (let ((out (img:resize-image src 3 3)))
        (ok (equal (shape out) '(3 3 :rgba)))
        (ok (eql 6 (img:image-orientation out)) "orientation is not applied")
        (ok (equalp icc (img:image-icc-profile out)))
        (ok (eql 0.45 (img:image-gamma out)))
        (ok (eq :display-p3 (img:image-color-space out)))
        (ok (= 8 (img:image-depth out)))))
    (let* ((frame (img:make-image-frame :width 4 :height 2 :format :gray
                                        :pixels (lcg-octets 8 9) :delay-ms 40
                                        :frame-rect '(0 0 4 2) :disposal :none :blend :over))
           (out (img:resize-image frame 2 1)))
      (ok (img:image-frame-p out) "a frame stays a frame")
      (ok (= 40 (img:image-frame-delay-ms out)))
      (ok (eq :over (img:image-frame-blend out))))))

(deftest resize-limits
  (let ((mock (make-instance 'resize-backend :filters '(:lanczos3 :nearest))))
    (with-backends (mock)
      (let ((one (image-of :gray 1 1 '(9))))
        (let ((c (condition-of (lambda () (img:resize-image one 20000 1)))))
          (ok (typep c 'img:image-limit-exceeded))
          (ok (and (typep c 'img:image-limit-exceeded)
                   (equal '(:width 16384 20000)
                          (list (img:image-limit-exceeded-limit c)
                                (img:image-limit-exceeded-allowed c)
                                (img:image-limit-exceeded-actual c))))))
        (let* ((limits (img:make-image-limits :max-pixels 100))
               (c (condition-of (lambda () (img:resize-image one 11 10 :limits limits)))))
          (ok (typep c 'img:image-limit-exceeded))
          (ok (and (typep c 'img:image-limit-exceeded)
                   (equal '(:pixels 100 110)
                          (list (img:image-limit-exceeded-limit c)
                                (img:image-limit-exceeded-allowed c)
                                (img:image-limit-exceeded-actual c)))))
          (ok (= 0 (calls mock)) "refused before any backend runs")
          (ok (equal '(10 10 :gray)
                     (ignore-errors (shape (img:resize-image one 10 10 :limits limits))))))
        (let ((img:*image-limits* (img:make-image-limits :max-height 5)))
          (ok (signals-type (lambda () (img:resize-image one 1 6 :backend :protocol))
                            'img:image-limit-exceeded)
              "the default is *IMAGE-LIMITS* at call time"))
        (ok (equal '(20000 1 :gray)
                   (ignore-errors
                    (shape (img:resize-image one 20000 1 :filter :nearest :backend :protocol
                                             :limits (img:make-image-limits :unlimited t))))))))))

(deftest-parametrize resize-argument-checks
    ((args type)
     ('(0 2) 'type-error)
     ('(2 0) 'type-error)
     ('(-1 2) 'type-error)
     ('(2.5 2) 'type-error)
     ('(2 nil) 'type-error)
     ('(2 2 :filter :mitchell) 'type-error)
     ('(2 2 :filter "box") 'type-error)
     ('(2 2 :filter nil) 'type-error)
     ('(2 2 :limits nil) 'type-error)
     ('(2 2 :backend "stb") 'type-error)
     ('(2 2 :backend :auto) 'type-error))
  (with-backends ()
    (ok (signals-type (lambda () (apply #'img:resize-image (pattern-image :rgb :noise 3 3 1) args))
                      type))))

(deftest resize-image-checks
  (with-backends ()
    (let ((corrupt (img:make-image :width 3 :height 3 :format :rgb :pixels (lcg-octets 26 1)))
          (deep (img:make-image :width 2 :height 2 :format :gray :depth 16 :pixels (lcg-octets 4 1)))
          (empty (img:make-image :width 0 :height 4 :format :gray)))
      (ok (signals-type (lambda () (img:resize-image corrupt 2 2)) 'img:image-corrupt))
      (ok (signals-type (lambda () (call #'img:resize-image corrupt 0 2)) 'img:image-corrupt)
          "IMAGE is checked before the arguments")
      (let ((c (condition-of (lambda () (img:resize-image deep 1 1)))))
        (ok (typep c 'img:image-unsupported-feature))
        (ok (and (typep c 'img:image-unsupported-feature)
                 (equal '(:depth 16) (img:image-error-feature c)))))
      (let ((c (condition-of (lambda () (img:resize-image empty 2 2)))))
        (ok (typep c 'img:image-unsupported-feature))
        (ok (and (typep c 'img:image-unsupported-feature)
                 (eq :empty (img:image-error-feature c)))))
      (ok (signals-type (lambda () (call #'img:resize-image "image" 2 2)) 'error)
          "methods on IMAGE only")
      (ok (equal '(3 2 :rgb)
                 (ignore-errors
                  (handler-bind ((type-error (lambda (c) (store-value 3 c))))
                    (shape (call #'img:resize-image (pattern-image :rgb :noise 4 4 2) 0 2)))))
          "CHECK-TYPE's STORE-VALUE restart"))))

(deftest resize-dispatch
  (let* ((mock (make-instance 'resize-backend :filters '(:bilinear)))
         (src (pattern-image :rgb :noise 4 4 21))
         (expected (with-backends () (ignore-errors (px (img:resize-image src 3 2 :filter :bilinear))))))
    (ok (= 18 (length expected)) "the protocol's own result")
    (with-backends (mock)
      (ok (all-octets-p (img:resize-image src 3 2 :filter :bilinear) 7) "a backend listing the filter wins")
      (ok (= 1 (calls mock)))
      (ok (not (all-octets-p (img:resize-image src 3 2 :filter :lanczos3) 7))
          "a filter the backend does not list stays in the protocol")
      (ok (equal expected (px (img:resize-image src 3 2 :filter :bilinear :backend :protocol))))
      (ok (= 1 (calls mock)))
      (ok (equal expected (px (img:resize-image src 3 2 :filter :bilinear
                                                    :backend (make-instance 'raw-backend))))
          "a forced backend without BACKEND-RESIZE uses the method on T")
      (ok (all-octets-p (img:resize-image src 3 2 :filter :lanczos3 :backend mock) 7)
          "a forced backend is used for any filter")))
  ;; Two classes: registering a backend replaces one of the same class.
  (let ((low (make-instance 'other-resize-backend :priority 10 :fill-octet 1))
        (high (make-instance 'resize-backend :priority 90 :fill-octet 2))
        (src (pattern-image :gray :noise 4 4 22)))
    (with-backends (low high)
      (ok (all-octets-p (img:resize-image src 2 2 :filter :bilinear) 2) "priority")
      (let ((img:*backend-preference* '(other-resize-backend)))
        (ok (all-octets-p (img:resize-image src 2 2 :filter :bilinear) 1) "*BACKEND-PREFERENCE*, a class"))
      (let ((img:*backend-preference* (list low)))
        (ok (all-octets-p (img:resize-image src 2 2 :filter :bilinear) 1) "*BACKEND-PREFERENCE*, an instance"))))
  (let* ((failing (make-instance 'failing-resize-backend :priority 90))
         (fallback (make-instance 'resize-backend :priority 60 :fill-octet 5))
         (src (pattern-image :gray :noise 4 4 23))
         (own (with-backends () (ignore-errors (px (img:resize-image src 2 2 :filter :bilinear))))))
    (with-backends (failing fallback)
      (ok (signals-type (lambda () (img:resize-image src 2 2 :filter :bilinear)) 'img:image-error)
          "no silent fallback")
      (ok (all-octets-p (handler-bind ((img:image-error
                                         (lambda (c) (declare (ignore c)) (invoke-restart 'img:try-next-backend))))
                          (img:resize-image src 2 2 :filter :bilinear))
                        5)
          "TRY-NEXT-BACKEND"))
    (with-backends (failing)
      (ok (equal own (ignore-errors
                      (px (handler-bind ((img:image-error
                                           (lambda (c) (declare (ignore c)) (invoke-restart 'img:try-next-backend))))
                            (img:resize-image src 2 2 :filter :bilinear)))))
          "after the last backend comes the protocol's resampler")
      (ok (all-octets-p (handler-bind ((img:image-error (lambda (c) (use-value fallback c))))
                          (img:resize-image src 2 2 :filter :bilinear))
                        5)
          "USE-VALUE with another backend")))
  (with-backends ((make-instance 'short-resize-backend))
    (ok (signals-type (lambda () (img:resize-image (pattern-image :gray :noise 4 4 1) 2 2 :filter :bilinear))
                      'img:image-corrupt)
        "a result of the wrong length"))
  (with-backends ((make-instance 'resize-backend))
    (let* ((src (pattern-image :rgb :noise 4 4 24)))
      (setf (img:image-orientation src) 3 (img:image-gamma src) 0.5)
      (let ((out (img:resize-image src 2 3 :filter :bilinear)))
        (ok (equal '(2 3 :rgb) (shape out)))
        (ok (and (eql 3 (img:image-orientation out)) (eql 0.5 (img:image-gamma out)))
            "the protocol builds the result from IMAGE")))))

;;; Crop ----------------------------------------------------------------------

(deftest crop-and-rotate-fixtures
  (let* ((rows (fixture "ops-geometry.sexp"))
         (bad (rows-mismatching
               rows
               (lambda (row)
                 (destructuring-bind (op format w h args nw nh pixels) row
                   (let* ((src (byte-grid format w h))
                          (out (ecase op
                                 (:crop (apply #'img:crop-image src args))
                                 (:rotate (img:rotate-image src (first args))))))
                     (and (equal (shape out) (list nw nh format))
                          (equal pixels (px out)))))))))
    (ok (= 30 (length rows)))
    (ok (null bad) (report bad))))

(deftest-parametrize crop-exact
    ((args expected)
     ;; 4 × 3 gray: 0 1 2 3 / 4 5 6 7 / 8 9 10 11
     ('(1 1 2 2) '(2 2 (5 6 9 10)))
     ('(0 0 4 1) '(4 1 (0 1 2 3)))
     ('(3 0 1 3) '(1 3 (3 7 11)))
     ('(0 2 4 1) '(4 1 (8 9 10 11)))
     ('(2 1 1 1) '(1 1 (6))))
  (let ((out (apply #'img:crop-image (byte-grid :gray 4 3) args)))
    (ok (equal expected (list (img:image-width out) (img:image-height out) (px out))))))

(deftest crop-identity-and-metadata
  (let ((src (byte-grid :rgba 3 2)))
    (setf (img:image-orientation src) 8 (img:image-icc-profile src) (octets '(4 5)))
    (ok (eq src (img:crop-image src 0 0 3 2)))
    (let ((out (img:crop-image src 1 0 2 2)))
      (ok (equal '(2 2 :rgba) (shape out)))
      (ok (not (eq (img:image-pixels out) (img:image-pixels src))))
      (ok (eql 8 (img:image-orientation out)))
      (ok (equalp (octets '(4 5)) (img:image-icc-profile out)))))
  (let ((frame (img:make-image-frame :width 2 :height 2 :format :gray :pixels (octets '(1 2 3 4))
                                     :delay-ms 70)))
    (let ((out (img:crop-image frame 1 1 1 1)))
      (ok (img:image-frame-p out))
      (ok (equal '(4) (px out)))
      (ok (= 70 (img:image-frame-delay-ms out))))))

(deftest-parametrize crop-invalid-region
    ((region)
     ('(-1 0 2 2)) ('(0 -1 2 2)) ('(3 0 2 1)) ('(0 2 1 2)) ('(4 0 1 1))
     ('(0 0 5 3)) ('(0 0 4 4)) ('(10 10 1 1)) ('(-5 -5 2 2)))
  (let ((c (condition-of (lambda () (apply #'img:crop-image (byte-grid :gray 4 3) region)))))
    (ok (typep c 'img:image-invalid-region))
    (ok (typep c 'img:image-error))
    (ok (and (typep c 'img:image-invalid-region)
             (equal region (img:image-invalid-region-region c))
             (equal '(4 3) (img:image-invalid-region-bounds c))))))

(deftest-parametrize crop-argument-checks
    ((args)
     ('(0 0 0 1)) ('(0 0 1 0)) ('(0 0 1 -2)) ('(1.5 0 1 1)) ('(0 nil 1 1)) ('(0 0 2.0 1)))
  (ok (signals-type (lambda () (apply #'img:crop-image (byte-grid :gray 4 3) args)) 'type-error)))

(deftest crop-restarts
  (let ((g (byte-grid :gray 4 3)))
    (flet ((clipped (&rest region)
             (ignore-errors
              (handler-bind ((img:image-invalid-region (lambda (c) (img:clip-region c))))
                (let ((out (apply #'img:crop-image g region)))
                  (list (img:image-width out) (img:image-height out) (px out)))))))
      (ok (equal '(2 1 (0 1)) (clipped -1 -1 3 2)))
      (ok (equal '(1 1 (11)) (clipped 3 2 5 5)))
      (ok (equal '(4 3 (0 1 2 3 4 5 6 7 8 9 10 11)) (clipped -2 -2 9 9))))
    (let ((offered :unset))
      (ok (equal '(5 6 9 10)
                 (ignore-errors
                  (px (handler-bind ((img:image-invalid-region
                                       (lambda (c)
                                         (setf offered (and (find-restart 'img:clip-region c) t))
                                         (use-value '(1 1 2 2) c))))
                        (img:crop-image g 10 10 1 1))))))
      (ok (null offered) "CLIP-REGION only when the region overlaps the image"))
    (let ((seen '()))
      (ok (equal '(0)
                 (ignore-errors
                  (px (handler-bind ((img:image-invalid-region
                                       (lambda (c)
                                         (push (img:image-invalid-region-region c) seen)
                                         (use-value (if (rest seen) '(0 0 1 1) '(9 9 1 1)) c))))
                        (img:crop-image g 7 0 1 1))))))
      (ok (equal '((9 9 1 1) (7 0 1 1)) seen) "a USE-VALUE region is checked in turn"))
    (ok (null (img:clip-region)) "no active restart: NIL, like USE-VALUE")))

;;; Rotate --------------------------------------------------------------------

(deftest-parametrize rotate-exact
    ((format w h source degrees expected)
     ;; 3 × 2 gray: 0 1 2 / 3 4 5
     (:gray 3 2 '(0 1 2 3 4 5) 90 '(2 3 (3 0 4 1 5 2)))
     (:gray 3 2 '(0 1 2 3 4 5) 180 '(3 2 (5 4 3 2 1 0)))
     (:gray 3 2 '(0 1 2 3 4 5) 270 '(2 3 (2 5 1 4 0 3)))
     (:gray 3 2 '(0 1 2 3 4 5) -90 '(2 3 (2 5 1 4 0 3)))
     (:gray 3 2 '(0 1 2 3 4 5) 450 '(2 3 (3 0 4 1 5 2)))
     (:rgb 2 1 '(1 2 3 4 5 6) 90 '(1 2 (1 2 3 4 5 6)))
     (:rgb 2 1 '(1 2 3 4 5 6) 270 '(1 2 (4 5 6 1 2 3)))
     (:gray-alpha 1 2 '(1 2 3 4) 90 '(2 1 (3 4 1 2))))
  (let ((out (img:rotate-image (image-of format w h source) degrees)))
    (ok (equal expected (list (img:image-width out) (img:image-height out) (px out))))
    (ok (eq format (img:image-format out)))))

(deftest rotate-matches-orientation
  (dolist (format img:pixel-formats)
    (dolist (size '((3 2) (4 3) (1 5)))
      (destructuring-bind (w h) size
        (loop for (degrees orientation) in '((90 6) (180 3) (270 8))
              do (let ((src (byte-grid format w h))
                       (oriented (byte-grid format w h)))
                   (setf (img:image-orientation oriented) orientation)
                   (let ((want (img:apply-orientation oriented))
                         (got (ignore-errors (img:rotate-image src degrees))))
                     (ok (and got (equal (shape got) (shape want))
                              (equalp (img:image-pixels got) (img:image-pixels want)))
                         (format nil "~A ~D×~D ~D°" format w h degrees)))))))))

(deftest rotate-composition
  (let ((src (byte-grid :rgb 5 3)))
    (flet ((turn (image &rest steps)
             (dolist (d steps image) (setf image (img:rotate-image image d)))))
      (ok (equalp (img:image-pixels src) (img:image-pixels (turn src 90 90 90 90))))
      (ok (equalp (img:image-pixels src) (img:image-pixels (turn src 90 270))))
      (ok (equalp (img:image-pixels src) (img:image-pixels (turn src 180 180))))
      (ok (equalp (img:image-pixels (turn src 270)) (img:image-pixels (turn src 90 90 90))))
      (ok (equalp (img:image-pixels (turn src -90)) (img:image-pixels (turn src 270))))
      (ok (equal '(3 5 :rgb) (shape (turn src 90)))))))

(deftest rotate-identity-and-metadata
  (let ((src (byte-grid :rgb 3 2)))
    (setf (img:image-orientation src) 2 (img:image-gamma src) 1/2)
    (dolist (d '(0 360 -360 720))
      (ok (eq src (img:rotate-image src d)) (format nil "~D°" d)))
    (let ((out (img:rotate-image src 90)))
      (ok (not (eq (img:image-pixels out) (img:image-pixels src))))
      (ok (eql 2 (img:image-orientation out)) "orientation is carried over, not changed")
      (ok (eql 1/2 (img:image-gamma out))))))

(deftest-parametrize rotate-argument-checks
    ((degrees)
     (45) (100) (90.0) (1/2) ("90") (nil) (:cw))
  (ok (signals-type (lambda () (call #'img:rotate-image (byte-grid :gray 3 2) degrees)) 'type-error)))

(deftest rotate-image-checks
  (ok (signals-type (lambda () (img:rotate-image (img:make-image :width 2 :height 2 :format :rgb
                                                                 :pixels (lcg-octets 11 1))
                                                 90))
                    'img:image-corrupt))
  (ok (equal '(2 3 (3 0 4 1 5 2))
             (ignore-errors
              (handler-bind ((type-error (lambda (c) (store-value 90 c))))
                (let ((out (call #'img:rotate-image (byte-grid :gray 3 2) 45)))
                  (list (img:image-width out) (img:image-height out) (px out))))))
      "CHECK-TYPE's STORE-VALUE restart")
  (let ((empty (img:make-image :width 0 :height 3 :format :gray)))
    (ok (equal '(3 0 :gray) (ignore-errors (shape (img:rotate-image empty 90))))
        "an empty image can be rotated")))

;;; Fit -----------------------------------------------------------------------

(deftest fit-dimensions-fixtures
  (let* ((rows (fixture "ops-fit.sexp"))
         (bad (rows-mismatching
               rows
               (lambda (row)
                 (destructuring-bind (w h mw mh rw rh) row
                   (equal (list rw rh)
                          (multiple-value-list (img:fit-dimensions w h :max-width mw :max-height mh))))))))
    (ok (= 19 (length rows)))
    (ok (null bad) (report bad))))

(deftest-parametrize fit-dimensions-unchanged
    ((w h mw mh)
     (100 50 100 50) (100 50 200 200) (100 50 nil nil) (100 50 100 nil) (100 50 nil 50) (1 1 1 1))
  (ok (equal (list w h)
             (multiple-value-list (img:fit-dimensions w h :max-width mw :max-height mh)))))

(deftest-parametrize fit-dimensions-argument-checks
    ((args)
     ('(0 10 :max-width 5)) ('(10 0 :max-width 5)) ('(10.0 10 :max-width 5))
     ('(10 10 :max-width 0)) ('(10 10 :max-height -1)) ('(10 10 :max-width 2.5))
     ('(10 10 :max-height "5")))
  (ok (signals-type (lambda () (apply #'img:fit-dimensions args)) 'type-error)))

(deftest fit-image
  (with-backends ()
    (let ((src (pattern-image :rgb :noise 8 4 31)))
      (let ((out (img:fit-image src :max-width 4 :max-height 4)))
        (ok (equal '(4 2 :rgb) (shape out)))
        (ok (equal (ignore-errors (px (img:resize-image src 4 2))) (px out))
            "the default filter is :LANCZOS3"))
      (ok (equal '(6 3 :rgb) (ignore-errors (shape (img:fit-image src :max-height 3 :filter :box))))
          "8 × 3/4 = 6")
      (ok (eq src (img:fit-image src :max-width 8 :max-height 4)) "already fits")
      (ok (eq src (img:fit-image src :max-width 100 :max-height 100)) "never enlarges")
      (ok (eq src (img:fit-image src)) "no bounds")
      (ok (signals-type (lambda () (img:fit-image src :max-width 0)) 'type-error))
      (ok (signals-type (lambda () (img:fit-image src :max-width 100 :filter :mitchell)) 'type-error)
          "arguments are checked even when IMAGE fits")
      (ok (signals-type (lambda () (call #'img:fit-image src :max-width 100 :backend "x")) 'type-error))))
  (let ((empty (img:make-image :width 0 :height 500 :format :gray)))
    (ok (eq empty (img:fit-image empty :max-height 10)) "an empty image always fits"))
  (let ((mock (make-instance 'resize-backend :filters '(:bilinear))))
    (with-backends (mock)
      (let ((src (pattern-image :gray :noise 10 10 32)))
        (ok (all-octets-p (img:fit-image src :max-width 5 :filter :bilinear) 7) "FILTER is passed on")
        (ok (not (all-octets-p (img:fit-image src :max-width 5 :filter :bilinear :backend :protocol) 7))
            "BACKEND is passed on")
        (ok (= 1 (calls mock)))))))

(deftest fit-image-photo-sizes
  ;; Typical camera sizes down to a 1568 px long edge, with the cheap filter.
  (with-backends ()
    (loop for (w h rw rh) in '((4032 3024 1568 1176) (3024 4032 1176 1568) (1600 1200 1568 1176))
          do (let* ((src (img:make-image :width w :height h :format :gray
                                         :pixels (make-array (* w h) :element-type '(unsigned-byte 8)
                                                                     :initial-element 90)))
                    (out (img:fit-image src :max-width 1568 :max-height 1568 :filter :nearest)))
               (ok (equal (list rw rh :gray) (shape out)) (format nil "~D×~D" w h))
               (ok (all-octets-p out 90))))))
