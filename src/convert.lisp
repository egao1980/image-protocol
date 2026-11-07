(in-package #:image-protocol)

;;; All pixel conversions happen here, so every backend produces identical
;;; pixels for lossless formats. Gray is BT.601 luma with integer rounding.

(declaim (inline %luma))
(defun %luma (r g b)
  (declare (type (unsigned-byte 8) r g b))
  (floor (+ (* 299 r) (* 587 g) (* 114 b) 500) 1000))

(defun depth-16-to-8 (samples)
  "16-bit samples (a vector of integers 0–65535) → octets, rounded
(ROUND (* v 255) 65535), never truncated."
  (let ((out (make-array (length samples) :element-type '(unsigned-byte 8))))
    (dotimes (i (length samples) out)
      (setf (aref out i) (round (* (aref samples i) 255) 65535)))))

(defun convert-image (image format)
  "→ IMAGE in pixel FORMAT (:GRAY :GRAY-ALPHA :RGB :RGBA). Adding alpha makes
it 255; dropping alpha discards it (no compositing). Returns IMAGE itself if
it is already in FORMAT."
  (let ((from (image-format image)))
    (when (eq from format) (return-from convert-image image))
    (let* ((n (* (image-width image) (image-height image)))
           (src (image-pixels image))
           (sc (channels from)) (dc (channels format))
           (dst (make-array (* n dc) :element-type '(unsigned-byte 8))))
      (declare (type octets src dst))
      (dotimes (i n)
        (let* ((s (* i sc)) (d (* i dc))
               (r (aref src s))
               (g (if (>= sc 3) (aref src (+ s 1)) r))
               (b (if (>= sc 3) (aref src (+ s 2)) r))
               (a (case sc (2 (aref src (+ s 1))) (4 (aref src (+ s 3))) (t 255))))
          (ecase format
            (:gray (setf (aref dst d) (if (>= sc 3) (%luma r g b) r)))
            (:gray-alpha (setf (aref dst d) (if (>= sc 3) (%luma r g b) r)
                               (aref dst (+ d 1)) a))
            (:rgb (setf (aref dst d) r (aref dst (+ d 1)) g (aref dst (+ d 2)) b))
            (:rgba (setf (aref dst d) r (aref dst (+ d 1)) g (aref dst (+ d 2)) b
                         (aref dst (+ d 3)) a)))))
      (let ((out (copy-image image)))
        (setf (image-format out) format (image-pixels out) dst)
        out))))
