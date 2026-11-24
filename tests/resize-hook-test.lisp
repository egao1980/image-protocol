(in-package #:image-protocol/tests)

;;; image-protocol 0.3.0: BACKEND-RESIZE, a backend's native fast path for
;;; RESIZE-IMAGE (and for THUMBNAIL and FIT-IMAGE through it). The rules R1–R8
;;; are in src/ops.lisp, "Backend fast path". The backends below only resize:
;;; each records its calls and answers with a marker image, declines, or
;;; misbehaves on purpose. Results are computed inside OK forms, so a failing
;;; call counts as one failed check and the rest of the test still runs.

(defparameter *marker-sample* 77
  "Every sample of a marker image. No resize of the noise inputs below is flat 77.")

(defclass resize-backend (img:image-backend)
  ((answer :initarg :answer :initform :marker :reader resize-answer)
   (calls :initform '() :accessor resize-calls))
  (:documentation "A resize-only test backend. Each BACKEND-RESIZE call is
recorded as (image width height . keys), newest first. ANSWER :MARKER gives a
marker image of the requested size, :DECLINE gives NIL, and a function is
called with (image width height) and its value returned."))

(defmethod img:backend-formats ((backend resize-backend)) '())

(defmethod img:backend-resize ((backend resize-backend) image width height
                               &rest keys &key filter reducing-gap linear)
  (declare (ignore filter reducing-gap linear))
  (push (list* image width height keys) (resize-calls backend))
  (let ((answer (resize-answer backend)))
    (case answer
      (:marker (%marker image width height))
      (:decline nil)
      (t (funcall answer image width height)))))

(defclass plain-backend (img:image-backend) ()
  (:documentation "No BACKEND-RESIZE method of its own: the one on IMAGE-BACKEND declines."))

(defmethod img:backend-formats ((backend plain-backend)) '())

(defun %marker (image width height &key (format (img:image-format image)) (depth 8))
  "WIDTH × HEIGHT, every sample *MARKER-SAMPLE*, with metadata unlike any input's."
  (img:make-image :width width :height height :format format :depth depth
                  :pixels (make-array (* width height (img:channels format))
                                      :element-type '(unsigned-byte 8)
                                      :initial-element *marker-sample*)
                  :orientation 3 :gamma 1 :icc-profile (octets '(9 9 9)) :color-space :marker))

(defun %marker-p (image)
  (and (img:image-p image)
       (plusp (length (img:image-pixels image)))
       (every (lambda (v) (= v *marker-sample*)) (img:image-pixels image))))

(defun %backend (&optional (answer :marker))
  (make-instance 'resize-backend :answer answer))

(defun %calls (backend)
  "BACKEND's calls, oldest first."
  (reverse (resize-calls backend)))

(defun %photo (format w h &key (seed 5))
  "Noise in FORMAT carrying metadata, as a decoder returns it."
  (let ((image (%noise-image format w h seed)))
    (setf (img:image-orientation image) 6
          (img:image-gamma image) 0.45
          (img:image-icc-profile image) (octets '(1 2 3)))
    image))

(defun %pure (image width height &rest keys)
  "0.2.0's result: RESIZE-IMAGE with :BACKEND NIL."
  (apply #'img:resize-image image width height :backend nil keys))

(defun %same-pixels-p (a b)
  (and (img:image-p a) (img:image-p b) (equalp (img:image-pixels a) (img:image-pixels b))))

(defun %condition (thunk)
  "The IMAGE-ERROR THUNK signals, or NIL."
  (handler-case (progn (funcall thunk) nil)
    (img:image-error (c) c)))

(defun %bad-backend-p (thunk operation designator)
  "R1: THUNK signals IMAGE-INVALID-ARGUMENT for OPERATION, argument BACKEND,
value DESIGNATOR (EQ)."
  (let ((c (%condition thunk)))
    (and (typep c 'img:image-invalid-argument)
         (eq operation (img:image-invalid-argument-operation c))
         (string= "BACKEND" (symbol-name (img:image-invalid-argument-argument c)))
         (eq designator (img:image-invalid-argument-value c)))))

(defun %argument-name (thunk)
  "The name of the argument THUNK's IMAGE-INVALID-ARGUMENT reports, or NIL."
  (let ((c (%condition thunk)))
    (and (typep c 'img:image-invalid-argument)
         (symbol-name (img:image-invalid-argument-argument c)))))

(deftest resize-hook-takes-the-backend-pixels
  "R4–R6: the result is a copy of IMAGE with the backend's pixels."
  (let* ((backend (%backend))
         (src (%photo :rgb 9 7))
         (before (copy-seq (img:image-pixels src)))
         (r nil))
    (ok (setf r (img:resize-image src 4 3 :filter :bicubic :backend backend)) "resizes")
    (ok (= 1 (length (resize-calls backend))) "one call")
    (ok (and (= 4 (img:image-width r)) (= 3 (img:image-height r))
             (eq :rgb (img:image-format r)) (eql 8 (img:image-depth r)))
        "4 × 3 RGB, depth 8")
    (ok (%marker-p r) "the backend's samples")
    (ok (and (eql 6 (img:image-orientation r)) (eql 0.45 (img:image-gamma r))
             (equalp (octets '(1 2 3)) (img:image-icc-profile r))
             (eq :srgb (img:image-color-space r)))
        "IMAGE's metadata, not the marker's")
    (ok (and (not (eq r src)) (not (eq (img:image-pixels r) (img:image-pixels src))))
        "a fresh image")
    (ok (and (equalp before (img:image-pixels src)) (= 9 (img:image-width src))
             (eql 6 (img:image-orientation src)))
        "IMAGE unchanged"))
  (let* ((backend (%backend))
         (frame (img:make-image-frame :width 6 :height 4 :format :gray-alpha
                                      :pixels (img:image-pixels (%noise-image :gray-alpha 6 4 9))
                                      :delay-ms 40 :disposal :background))
         (r nil))
    (ok (setf r (img:resize-image frame 3 2 :backend backend)) "resizes a frame")
    (ok (and (typep r 'img:image-frame) (= 40 (img:image-frame-delay-ms r))
             (eq :background (img:image-frame-disposal r)) (%marker-p r))
        "a frame stays a frame, with its frame slots")))

(deftest resize-hook-arguments
  "R4: what BACKEND-RESIZE receives."
  (flet ((call (thunk)
           "The one call THUNK makes to a fresh marker backend, as (image width height . keys)."
           (let ((backend (%backend)))
             (funcall thunk backend)
             (let ((calls (%calls backend)))
               (and (= 1 (length calls)) (first calls)))))
         (keys (call) (cdddr call)))
    (let* ((src (%photo :rgb 9 7))
           (c (ignore-errors
               (call (lambda (b) (img:resize-image src 4 3 :filter :bicubic :reducing-gap 2
                                                           :linear :yes :backend b))))))
      (ok c "one call")
      (ok (and c (eq src (first c)) (= 4 (second c)) (= 3 (third c)))
          "the caller's IMAGE itself, WIDTH and HEIGHT")
      (ok (and c (equal '(:filter :linear :reducing-gap)
                        (sort (loop for k in (keys c) by #'cddr collect k) #'string<)))
          "exactly :filter, :reducing-gap and :linear")
      (ok (and c (eq :bicubic (getf (keys c) :filter)) (eql 2 (getf (keys c) :reducing-gap))
               (eq t (getf (keys c) :linear)))
          ":filter as given, the gap as given, :linear T for a true value"))
    (let ((c (ignore-errors (call (lambda (b) (img:resize-image (%photo :gray 9 7) 4 3 :backend b))))))
      (ok (and c (equal '(:lanczos3 nil nil)
                        (list (getf (keys c) :filter :missing) (getf (keys c) :reducing-gap :missing)
                              (getf (keys c) :linear :missing))))
          "defaults: :lanczos3, no gap, :linear NIL, all passed"))
    (let ((c (ignore-errors
              (call (lambda (b) (img:resize-image (%photo :rgb 34 3) 10 3 :filter :bilinear
                                                                         :reducing-gap 1.7 :backend b))))))
      (ok (and c (eql 1.7 (getf (keys c) :reducing-gap))) "a gap is passed as written, not converted"))
    (dolist (format '(:rgba :gray-alpha))
      (let ((c (ignore-errors
                (call (lambda (b) (img:resize-image (%photo format 9 7) 4 3 :reducing-gap 3/2 :backend b))))))
        (ok (and c (null (getf (keys c) :reducing-gap :missing)))
            (format nil "~(~a~): the gap is NIL, as the pure-Lisp path drops it" format))))
    (let ((c (ignore-errors
              (call (lambda (b) (img:resize-image (%photo :gray 9 7) 4 3 :filter :nearest
                                                                       :reducing-gap 2 :backend b))))))
      (ok (and c (eq :nearest (getf (keys c) :filter)) (null (getf (keys c) :reducing-gap :missing)))
          ":nearest: the gap is NIL"))
    (let ((c (ignore-errors (call (lambda (b) (img:thumbnail (%photo :rgb 40 20) 10 10 :backend b))))))
      (ok (and c (= 10 (second c)) (= 5 (third c)) (eq :lanczos3 (getf (keys c) :filter))
               (eql 2 (getf (keys c) :reducing-gap)))
          "thumbnail: its size, its filter and its gap 2"))
    (let ((c (ignore-errors (call (lambda (b) (img:thumbnail (%photo :rgba 40 20) 10 10 :backend b))))))
      (ok (and c (null (getf (keys c) :reducing-gap :missing))) "thumbnail of RGBA: no gap"))
    (let ((c (ignore-errors
              (call (lambda (b) (img:fit-image (%photo :rgb 40 20) :max-width 20 :filter :bilinear
                                                                  :backend b))))))
      (ok (and c (= 20 (second c)) (= 10 (third c)) (eq :bilinear (getf (keys c) :filter))
               (eql 2 (getf (keys c) :reducing-gap)))
          "fit-image: its size, its filter and its gap 2"))))

(deftest resize-hook-designators
  "R1: NIL, a backend or a list of backends; *RESIZE-BACKENDS* is the default.
   R5, R8: nothing is consulted unless the caller names it."
  (ok (null img:*resize-backends*) "*resize-backends* is NIL by default")
  (ok (typep #'img:backend-resize 'generic-function))
  (ok (null (img:backend-resize (make-instance 'plain-backend) (%photo :rgb 4 3) 2 2
                                :filter :bilinear :reducing-gap nil :linear nil))
      "the method on IMAGE-BACKEND declines")
  (let ((backend (%backend))
        (r nil))
    (with-backends (backend)
      (ok (setf r (img:resize-image (%photo :rgb 9 7) 4 3)) "resizes")
      (ok (null (resize-calls backend)) "registering a backend does not make it resize")
      (ok (%same-pixels-p r (%pure (%photo :rgb 9 7) 4 3)) "the pure-Lisp result")))
  (let ((src (%photo :rgb 9 7)))
    (let ((backend (%backend)) (r nil))
      (let ((img:*resize-backends* backend))
        (ok (setf r (img:resize-image src 4 3)) "resizes"))
      (ok (and (= 1 (length (resize-calls backend))) (%marker-p r))
          "*resize-backends* is the default :backend"))
    (let ((backend (%backend)) (r nil))
      (let ((img:*resize-backends* (list backend)))
        (ok (setf r (img:resize-image src 4 3 :backend nil)) "resizes"))
      (ok (and (null (resize-calls backend)) (%same-pixels-p r (%pure src 4 3)))
          ":backend NIL overrides *resize-backends*"))
    (let ((first (%backend :decline)) (second (%backend)) (third (%backend)) (r nil))
      (ok (setf r (img:resize-image src 4 3 :backend (list first second third))) "resizes")
      (ok (and (= 1 (length (resize-calls first))) (= 1 (length (resize-calls second)))
               (null (resize-calls third)))
          "in list order, until one answers")
      (ok (%marker-p r) "the answer of the first that does not decline"))
    (let ((twice (%backend :decline)) (r nil))
      (ok (setf r (img:resize-image src 4 3 :backend (list twice twice))) "resizes")
      (ok (= 2 (length (resize-calls twice))) "a backend listed twice is consulted twice"))
    (let ((backend (%backend)))
      (dolist (bad (list 42 :protocol 'resize-backend (list backend 42) (cons backend backend)
                         (make-instance 'standard-object)))
        (ok (%bad-backend-p (lambda () (img:resize-image src 4 3 :backend bad)) :resize bad)
            (format nil "resize-image refuses ~s" bad)))
      (let ((bad (list backend :protocol)))
        (ok (%bad-backend-p (lambda () (img:thumbnail src 4 4 :backend bad)) :thumbnail bad)
            "thumbnail refuses a bad designator")
        (ok (%bad-backend-p (lambda () (img:fit-image src :max-width 4 :backend bad)) :fit bad)
            "fit-image refuses a bad designator"))
      (let ((img:*resize-backends* :protocol))
        (ok (%bad-backend-p (lambda () (img:resize-image src 4 3)) :resize :protocol)
            "a bad *resize-backends* is refused too"))
      (ok (%bad-backend-p (lambda () (img:resize-image src 9 7 :backend 42)) :resize 42)
          "R2: refused even when nothing would be resized")
      (ok (%bad-backend-p (lambda () (img:thumbnail src 100 100 :backend 42)) :thumbnail 42)
          "thumbnail: refused even when IMAGE fits")
      (ok (%bad-backend-p (lambda () (img:fit-image src :max-width 9 :backend 42)) :fit 42)
          "fit-image: refused even when the scale is 1")
      (ok (null (resize-calls backend)) "no call on a refused designator"))))

(deftest resize-hook-checks-come-first
  "R2: every 0.2.0 error, with its condition, before any backend is consulted;
   the designator after the operation's own arguments, before the limits."
  (let ((backend (%backend))
        (src (%photo :rgb 9 7)))
    (flet ((refused (thunk type)
             (let ((c (%condition thunk)))
               (and (typep c type) (null (resize-calls backend))))))
      (ok (refused (lambda () (img:resize-image :not-an-image 4 3 :backend backend))
                   'img:image-invalid-argument)
          "not an image")
      (ok (refused (lambda () (img:resize-image (img:make-image :width 2 :height 1 :format :gray
                                                                :depth 16 :pixels (octets '(1 2)))
                                                1 1 :backend backend))
                   'img:image-unsupported-feature)
          "depth 16")
      (ok (refused (lambda () (img:resize-image (img:make-image :width 3 :height 3 :format :rgb
                                                                :pixels (octets '(1 2 3)))
                                                2 2 :backend backend))
                   'img:image-corrupt)
          "pixels that do not match the size")
      (dolist (args '((0 3) (4 -1) (2.5 3) (4 3 :filter :hamming) (4 3 :reducing-gap 1/2)
                      (4 3 :reducing-gap :two)))
        (ok (refused (lambda () (apply #'img:resize-image src (append args (list :backend backend))))
                     'img:image-invalid-argument)
            (format nil "resize-image ~s" args)))
      (ok (refused (lambda () (img:resize-image (img:make-image :width 0 :height 0 :format :gray)
                                                2 2 :backend backend))
                   'img:image-invalid-argument)
          "an empty image")
      (let ((img:*image-limits* (img:make-image-limits :max-width 10)))
        (ok (refused (lambda () (img:resize-image src 11 3 :backend backend))
                     'img:image-limit-exceeded)
            "the output size against *image-limits*")
        (ok (refused (lambda () (img:fit-image src :max-width 11 :enlarge t :backend backend))
                     'img:image-limit-exceeded)
            "fit-image: the limits too"))
      (ok (refused (lambda () (img:thumbnail src 0 5 :backend backend)) 'img:image-invalid-argument)
          "thumbnail: a bad bound")
      (ok (refused (lambda () (img:fit-image src :backend backend)) 'img:image-invalid-argument)
          "fit-image: no bound"))
    (ok (equal "WIDTH" (%argument-name (lambda () (img:resize-image src 0 3 :backend 42))))
        "WIDTH is checked before BACKEND")
    (ok (equal "REDUCING-GAP" (%argument-name (lambda () (img:resize-image src 4 3 :reducing-gap 0
                                                                              :backend 42))))
        "REDUCING-GAP before BACKEND")
    (ok (equal "MAX-HEIGHT" (%argument-name (lambda () (img:thumbnail src 4 0 :backend 42))))
        "thumbnail: its bounds before BACKEND")
    (ok (equal "BACKEND" (%argument-name (lambda () (img:resize-image (img:make-image :width 0 :height 0
                                                                                      :format :gray)
                                                                      2 2 :backend 42))))
        "BACKEND before the empty image")
    (let ((img:*image-limits* (img:make-image-limits :max-width 10)))
      (ok (%bad-backend-p (lambda () (img:resize-image src 11 3 :backend 42)) :resize 42)
          "BACKEND before the limits"))))

(deftest resize-hook-nothing-to-do
  "R3: IMAGE itself comes back, and no backend is consulted."
  (let ((backend (%backend))
        (src (%photo :rgb 9 7)))
    (ok (eq src (img:resize-image src 9 7 :backend backend)) "resize to the same size")
    (ok (eq src (img:thumbnail src 20 20 :backend backend)) "thumbnail that fits")
    (ok (eq src (img:fit-image src :max-width 9 :backend backend)) "fit-image with scale 1")
    (ok (eq src (img:fit-image src :max-width 20 :backend backend)) "fit-image, not enlarged")
    (let ((empty (img:make-image :width 0 :height 3 :format :gray)))
      (ok (eq empty (img:thumbnail empty 2 2 :backend backend)) "thumbnail of an empty image"))
    (ok (null (resize-calls backend)) "no call")))

(deftest resize-hook-declined-equals-0.2.0
  "R5: when every backend declines, the result is 0.2.0's, sample for sample:
   each case of tests/pillow-parity.sexp through a declining backend still
   equals Pillow 11.3."
  (let ((cases (%parity-cases :resize))
        (decliner (%backend :decline))
        (plain (make-instance 'plain-backend))
        (failures '()))
    (dolist (case cases)
      (destructuring-bind (format w h ow oh filter gap seed hex) case
        (let ((d (handler-case
                     (%difference (img:image-pixels
                                   (img:resize-image (%noise-image format w h seed) ow oh
                                                     :filter filter :reducing-gap gap
                                                     :backend (list decliner plain)))
                                  (%hex-octets hex))
                   (error (e) (list :error (princ-to-string e))))))
          (when d
            (push (list* format w h ow oh filter gap d) failures)))))
    (ok (and (plusp (length cases)) (null failures)) (%report (length cases) failures))
    (ok (= (length cases) (length (resize-calls decliner))) "the backend was consulted each time"))
  (dolist (format '(:gray :gray-alpha :rgb :rgba))
    (let ((src (%photo format 40 20)) (decliner (%backend :decline)))
      (ok (%same-pixels-p (ignore-errors (img:thumbnail src 10 10 :filter :bicubic :backend decliner))
                          (img:thumbnail src 10 10 :filter :bicubic :backend nil))
          (format nil "~(~a~) thumbnail: 0.2.0's" format))
      (ok (%same-pixels-p (ignore-errors (img:fit-image src :max-width 13 :backend decliner))
                          (img:fit-image src :max-width 13 :backend nil))
          (format nil "~(~a~) fit-image: 0.2.0's" format))
      (ok (%same-pixels-p (ignore-errors (img:resize-image src 13 7 :linear t :backend decliner))
                          (%pure src 13 7 :linear t))
          (format nil "~(~a~) linear: 0.2.0's" format))
      (ok (= 3 (length (resize-calls decliner))) "consulted each time"))))

(deftest resize-hook-bad-answers
  "R5: an answer that is not NIL or a fitting fresh IMAGE signals IMAGE-CORRUPT
   with BACKEND set."
  (let ((src (%photo :rgb 9 7)))
    (flet ((corrupt-p (answer &key (image src) (width 4) (height 3))
             (let* ((backend (%backend answer))
                    (c (%condition (lambda () (img:resize-image image width height :backend backend)))))
               (and (typep c 'img:image-corrupt)
                    (eq backend (img:image-error-backend c))
                    (stringp (img:image-error-cause c))
                    (= 1 (length (resize-calls backend)))))))
      (ok (corrupt-p (lambda (image w h) (%marker image (1+ w) h))) "the wrong width")
      (ok (corrupt-p (lambda (image w h) (%marker image w (1+ h)))) "the wrong height")
      (ok (corrupt-p (lambda (image w h) (declare (ignore image)) (%marker src w h :format :rgba)))
          "the wrong format")
      (ok (corrupt-p (lambda (image w h) (%marker image w h :depth 16))) "depth 16")
      (ok (corrupt-p (lambda (image w h)
                       (let ((r (%marker image w h)))
                         (setf (img:image-pixels r) (octets '(1 2 3)))
                         r)))
          "pixels that do not match the size")
      (ok (corrupt-p (lambda (image w h) (img:image-pixels (%marker image w h))))
          "octets instead of an image")
      (ok (corrupt-p (lambda (image w h) (declare (ignore image w h)) :done)) "a keyword")
      (let ((wide (%noise-image :gray 4 2 3)))
        (ok (corrupt-p (lambda (image w h)
                         (img:make-image :width w :height h :format :gray
                                         :pixels (img:image-pixels image)))
                       :image wide :width 2 :height 4)
            "IMAGE's own pixel vector")
        (ok (equalp (img:image-pixels wide) (img:image-pixels (%noise-image :gray 4 2 3)))
            "IMAGE unchanged")))))

(deftest resize-hook-restarts
  "R7: backend conditions reach the caller; TRY-NEXT-BACKEND falls back."
  (let* ((src (%photo :rgb 9 7))
         (pure (%pure src 4 3))
         (condition (make-condition 'img:image-native-unavailable :cause "no native resizer"))
         (failing (%backend (lambda (image w h) (declare (ignore image w h)) (error condition))))
         (wrong (%backend (lambda (image w h) (%marker image (1+ w) h)))))
    (ok (eq condition (handler-case (img:resize-image src 4 3 :backend failing)
                        (img:image-error (c) c)))
        "the backend's own condition, unchanged")
    (ok (handler-bind ((img:image-native-unavailable
                         (lambda (c)
                           (let ((restart (find-restart 'img:try-next-backend c)))
                             (when restart (invoke-restart restart))))))
          (%same-pixels-p (img:resize-image src 4 3 :backend failing) pure))
        "TRY-NEXT-BACKEND after the last backend: the pure-Lisp result")
    (let ((after (%backend)))
      (ok (handler-bind ((img:image-native-unavailable
                           (lambda (c)
                             (let ((restart (find-restart 'img:try-next-backend c)))
                               (when restart (invoke-restart restart))))))
            (%marker-p (img:resize-image src 4 3 :backend (list failing after))))
          "TRY-NEXT-BACKEND: the next backend answers")
      (ok (= 1 (length (resize-calls after))) "the next backend was called once"))
    (ok (handler-bind ((img:image-corrupt
                         (lambda (c)
                           (let ((restart (find-restart 'img:try-next-backend c)))
                             (when restart (invoke-restart restart))))))
          (%same-pixels-p (img:resize-image src 4 3 :backend wrong) pure))
        "TRY-NEXT-BACKEND also covers a bad answer")
    (ok (handler-bind ((img:image-native-unavailable
                         (lambda (c)
                           (let ((restart (find-restart 'img:try-next-backend c)))
                             (when restart (invoke-restart restart))))))
          (%same-pixels-p (img:thumbnail src 4 4 :filter :bilinear :backend failing)
                          (img:thumbnail src 4 4 :filter :bilinear :backend nil)))
        "thumbnail: the same restart")
    (ok (null (find-restart 'img:try-next-backend)) "no restart outside a call")))

(deftest resize-hook-leaves-no-global-state
  "R8: no special variable or registry change; a named backend is not registered."
  (let* ((src (%photo :rgba 9 7))
         (backends (img:image-backends))
         (preference img:*backend-preference*)
         (limits img:*image-limits*)
         (default img:*resize-backends*)
         (accept (%backend))
         (decline (%backend :decline))
         (results '()))
    (push (ignore-errors (img:resize-image src 4 3 :backend accept)) results)
    (push (ignore-errors (img:resize-image src 4 3 :backend (list decline accept))) results)
    (push (ignore-errors (img:thumbnail src 4 4 :backend decline)) results)
    (push (ignore-errors (img:fit-image src :max-height 3 :backend (list decline decline))) results)
    (let ((img:*resize-backends* accept))
      (push (ignore-errors (img:resize-image src 5 5)) results))
    (ok (every #'img:image-p results) "every call resized")
    (ok (= 3 (count-if #'%marker-p results)) "the accepting backend answered three times")
    (ok (= 3 (length (resize-calls accept))) "and was called three times")
    (ok (equal backends (img:image-backends)) "the registry is unchanged")
    (ok (not (member-if (lambda (b) (typep b 'resize-backend)) (img:image-backends)))
        "named backends are not registered")
    (ok (and (eq preference img:*backend-preference*) (eq limits img:*image-limits*)
             (eq default img:*resize-backends*))
        "*backend-preference*, *image-limits* and *resize-backends* are unchanged")))
