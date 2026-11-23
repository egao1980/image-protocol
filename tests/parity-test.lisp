(in-package #:image-protocol/tests)

;;; Pillow 11.3 parity, sample for sample (image-protocol#2). The cases in
;;; tests/pillow-parity.sexp come from tests/pillow-parity.py: noise images
;;; from the LCG below, in every format, resized with every filter, with and
;;; without reducing_gap; thumbnail() sizes; alpha_composite(). Set
;;; IMAGE_PROTOCOL_PARITY to another file (pillow-parity.py --random N) for
;;; a stress run.

(defun %lcg-noise (n seed)
  "N octets of pillow-parity.py's noise(): s = (s · 1103515245 + 12345)
   mod 2^31, each octet bits 16–23 of s."
  (let ((out (make-array n :element-type '(unsigned-byte 8)))
        (s seed))
    (dotimes (i n out)
      (setf s (logand (+ (* s 1103515245) 12345) #x7FFFFFFF)
            (aref out i) (logand (ash s -16) #xFF)))))

(defun %noise-image (format w h seed)
  "pillow-parity.py's make(): noise, alpha below 32 → 0, above 223 → 255."
  (let* ((c (ecase format (:gray 1) (:gray-alpha 2) (:rgb 3) (:rgba 4)))
         (px (%lcg-noise (* w h c) seed)))
    (when (member format '(:gray-alpha :rgba))
      (loop for p from (1- c) below (length px) by c
            do (let ((a (aref px p)))
                 (setf (aref px p) (cond ((< a 32) 0) ((> a 223) 255) (t a))))))
    (img:make-image :width w :height h :format format :pixels px)))

(defun %hex-octets (hex)
  (let ((out (make-array (floor (length hex) 2) :element-type '(unsigned-byte 8))))
    (dotimes (i (length out) out)
      (setf (aref out i) (parse-integer hex :start (* 2 i) :end (+ 2 (* 2 i)) :radix 16)))))

(defun %parity-cases (kind)
  "The cases of KIND (:resize, :thumbnail, :composite) in the parity file."
  (let* ((env (uiop:getenv "IMAGE_PROTOCOL_PARITY"))
         (path (if (and env (plusp (length env)))
                   (pathname env)
                   (asdf:system-relative-pathname "image-protocol" "tests/pillow-parity.sexp"))))
    (with-open-file (in path :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*read-eval* nil)
              (*package* (find-package '#:image-protocol/tests)))
          (loop for form = (read in nil in)
                until (eq form in)
                when (eq kind (first form))
                  collect (rest form)))))))

(defun %difference (got expected)
  "NIL when the octet vectors are equal, else (:max-diff D :samples N)."
  (unless (equalp got expected)
    (if (/= (length got) (length expected))
        (list :length (length got) :expected (length expected))
        (list :max-diff (reduce #'max (map 'list (lambda (a b) (abs (- a b))) got expected))
              :samples (count nil (map 'list #'= got expected))))))

(defun %report (n failures)
  (format nil "~d cases equal Pillow 11.3~@[; ~d differ, first: ~{~s~^ ~}~]"
          n (and failures (length failures))
          (subseq (reverse failures) 0 (min 8 (length failures)))))

(deftest pillow-parity-resize
  "resize() of L, LA, RGB and RGBA noise with every filter, with and
   without reducing_gap (1.5, 2, 3), down, up and one side only."
  (let ((cases (%parity-cases :resize))
        (failures '()))
    (dolist (case cases)
      (destructuring-bind (format w h ow oh filter gap seed hex) case
        (let ((d (%difference (img:image-pixels
                               (img:resize-image (%noise-image format w h seed) ow oh
                                                 :filter filter :reducing-gap gap))
                              (%hex-octets hex))))
          (when d
            (push (list* format w h ow oh filter gap d) failures)))))
    (ok (and (plusp (length cases)) (null failures)) (%report (length cases) failures))))

(deftest pillow-parity-thumbnail-sizes
  "thumbnail() sizes for random small images and boxes, near ties included."
  (let ((cases (%parity-cases :thumbnail))
        (failures '()))
    (dolist (case cases)
      (destructuring-bind (w h mw mh ow oh) case
        (let* ((src (img:make-image :width w :height h :format :gray
                                    :pixels (make-array (* w h) :element-type '(unsigned-byte 8)
                                                                :initial-element 9)))
               (tn (img:thumbnail src mw mh))
               (got (list (img:image-width tn) (img:image-height tn))))
          (unless (equal got (list ow oh))
            (push (list w h mw mh :got got :pillow (list ow oh)) failures)))))
    (ok (and (plusp (length cases)) (null failures)) (%report (length cases) failures))))

(deftest pillow-parity-composite
  "alpha_composite() of RGBA noise (alpha 0, 255 and in between)."
  (let ((cases (%parity-cases :composite))
        (failures '()))
    (dolist (case cases)
      (destructuring-bind (w h dseed sseed hex) case
        (let ((d (%difference (img:image-pixels
                               (img:composite-image (%noise-image :rgba w h dseed)
                                                    (%noise-image :rgba w h sseed)))
                              (%hex-octets hex))))
          (when d
            (push (list* dseed sseed d) failures)))))
    (ok (and (plusp (length cases)) (null failures)) (%report (length cases) failures))))
