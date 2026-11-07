(in-package #:image-protocol)

;;; EXIF orientation (tag #x0112), read but never applied unless asked.
;;; Accepts a JPEG (APP1 "Exif\0\0") or a raw TIFF block (PNG eXIf).

(defun %u16 (v i big) (if big (logior (ash (aref v i) 8) (aref v (1+ i)))
                          (logior (aref v i) (ash (aref v (1+ i)) 8))))
(defun %u32 (v i big) (if big (logior (ash (%u16 v i t) 16) (%u16 v (+ i 2) t))
                          (logior (%u16 v i nil) (ash (%u16 v (+ i 2) nil) 16))))

(defun %tiff-orientation (v start end)
  "Orientation from the TIFF block V[START, END), or NIL. Never reads past END."
  (when (< (+ start 8) end)
    (let ((big (cond ((and (= (aref v start) #x4d) (= (aref v (1+ start)) #x4d)) t)
                     ((and (= (aref v start) #x49) (= (aref v (1+ start)) #x49)) nil)
                     (t (return-from %tiff-orientation nil)))))
      (let ((ifd (+ start (%u32 v (+ start 4) big))))
        (when (< (+ ifd 2) end)
          (let ((count (%u16 v ifd big)))
            (loop for k below (min count 512)
                  for e = (+ ifd 2 (* 12 k))
                  while (<= (+ e 12) end)
                  when (= (%u16 v e big) #x0112)
                    do (let ((o (%u16 v (+ e 8) big)))
                         (return (and (<= 1 o 8) o))))))))))

(defun exif-orientation (octets)
  "→ EXIF orientation 1–8 found in OCTETS (a JPEG, or a bare TIFF/EXIF
block), or NIL."
  (let ((n (length octets)))
    (cond
      ((and (> n 4) (= (aref octets 0) #xff) (= (aref octets 1) #xd8))
       ;; Walk JPEG markers until SOS.
       (loop with i = 2
             while (< (+ i 4) n)
             do (unless (= (aref octets i) #xff) (return nil))
                (let ((marker (aref octets (1+ i)))
                      (len (%u16 octets (+ i 2) t)))
                  (when (or (= marker #xda) (< len 2)) (return nil))
                  (when (and (= marker #xe1) (< (+ i 10) n)
                             (= (aref octets (+ i 4)) (char-code #\E))
                             (= (aref octets (+ i 5)) (char-code #\x))
                             (= (aref octets (+ i 6)) (char-code #\i))
                             (= (aref octets (+ i 7)) (char-code #\f)))
                    (return (%tiff-orientation octets (+ i 10) (min n (+ i 2 len)))))
                  (incf i (+ 2 len)))))
      (t (%tiff-orientation octets 0 n)))))

(defun apply-orientation (image)
  "→ IMAGE rotated / flipped upright per its ORIENTATION (1–8), with
ORIENTATION cleared. Returns IMAGE itself when there is nothing to do."
  (let ((o (image-orientation image)))
    (when (or (null o) (= o 1)) (return-from apply-orientation image))
    (let* ((w (image-width image)) (h (image-height image)) (c (channels (image-format image)))
           (transpose (>= o 5))
           (nw (if transpose h w)) (nh (if transpose w h))
           (src (image-pixels image))
           (dst (make-array (* nw nh c) :element-type '(unsigned-byte 8))))
      (dotimes (y nh)
        (dotimes (x nw)
          ;; Map the destination pixel back to the source.
          (multiple-value-bind (sx sy)
              (ecase o
                (2 (values (- w 1 x) y))
                (3 (values (- w 1 x) (- h 1 y)))
                (4 (values x (- h 1 y)))
                (5 (values y x))
                (6 (values y (- h 1 x)))
                (7 (values (- w 1 y) (- h 1 x)))
                (8 (values (- w 1 y) x)))
            (replace dst src :start1 (* (+ (* y nw) x) c)
                             :start2 (* (+ (* sy w) sx) c) :end2 (+ (* (+ (* sy w) sx) c) c)))))
      (let ((out (copy-image image)))
        (setf (image-width out) nw (image-height out) nh (image-pixels out) dst
              (image-orientation out) nil)
        out))))
