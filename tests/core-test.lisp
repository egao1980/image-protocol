(in-package #:image-protocol/tests)

(deftest sniffing
  (ok (eq :png (img:sniff-format (octets '(#x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a 0)))))
  (ok (eq :jpeg (img:sniff-format (octets '(#xff #xd8 #xff #xe0)))))
  (ok (eq :gif (img:sniff-format (map '(vector (unsigned-byte 8)) #'char-code "GIF89a.."))))
  (ok (eq :webp (img:sniff-format (map '(vector (unsigned-byte 8)) #'char-code "RIFF1234WEBPVP8 "))))
  (ok (eq :avif (img:sniff-format (map '(vector (unsigned-byte 8)) #'char-code "1234ftypavif"))))
  (ok (null (img:sniff-format (octets '(1 2 3)))))
  (ng (member :tga (list (img:sniff-format (octets '(0 0 2 0 0 0 0 0 0 0 0 0)))))))

(deftest conversions-are-exact
  (let ((rgb (img:make-image :width 2 :height 1 :format :rgb
                             :pixels (octets '(255 0 0 10 20 30)))))
    (ok (equalp (img:image-pixels (img:convert-image rgb :rgba)) #(255 0 0 255 10 20 30 255)))
    ;; BT.601 luma, rounded: 0.299*255 = 76.2 → 76; 0.299*10+0.587*20+0.114*30 = 18.16 → 18
    (ok (equalp (img:image-pixels (img:convert-image rgb :gray)) #(76 18)))
    (ok (eq rgb (img:convert-image rgb :rgb))))
  (let ((ga (img:make-image :width 1 :height 1 :format :gray-alpha :pixels (octets '(7 9)))))
    (ok (equalp (img:image-pixels (img:convert-image ga :rgba)) #(7 7 7 9))))
  (ok (equalp (img:depth-16-to-8 #(0 257 65535 32767 32768)) #(0 1 255 127 128))))

(defun grid (w h)
  "W × H gray image whose pixel value is its index."
  (img:make-image :width w :height h :format :gray
                  :pixels (coerce (loop for i below (* w h) collect i)
                                  '(simple-array (unsigned-byte 8) (*)))))

(deftest orientation-transforms
  ;; 3 × 2:  0 1 2 / 3 4 5
  (flet ((o (n) (let ((g (grid 3 2))) (setf (img:image-orientation g) n)
                  (let ((r (img:apply-orientation g)))
                    (list (img:image-width r) (img:image-height r)
                          (coerce (img:image-pixels r) 'list) (img:image-orientation r))))))
    (ok (equal (o 2) '(3 2 (2 1 0 5 4 3) nil)))     ; mirror
    (ok (equal (o 3) '(3 2 (5 4 3 2 1 0) nil)))     ; 180
    (ok (equal (o 4) '(3 2 (3 4 5 0 1 2) nil)))     ; flip
    (ok (equal (o 5) '(2 3 (0 3 1 4 2 5) nil)))     ; transpose
    (ok (equal (o 6) '(2 3 (3 0 4 1 5 2) nil)))     ; 90° clockwise
    (ok (equal (o 7) '(2 3 (5 2 4 1 3 0) nil)))     ; transverse
    (ok (equal (o 8) '(2 3 (2 5 1 4 0 3) nil))))    ; 90° counter-clockwise
  (let ((g (grid 2 2))) (ok (eq g (img:apply-orientation g)))))

(defun jpeg-with-orientation (o &key (big t))
  (let* ((tiff (if big
                   (list #x4d #x4d 0 #x2a 0 0 0 8 0 1 #x01 #x12 0 3 0 0 0 1 0 o 0 0 0 0 0 0)
                   (list #x49 #x49 #x2a 0 8 0 0 0 1 0 #x12 #x01 3 0 1 0 0 0 o 0 0 0 0 0 0 0)))
         (app1 (append (map 'list #'char-code "Exif") (list 0 0) tiff))
         (len (+ 2 (length app1))))
    (octets (append '(#xff #xd8 #xff #xe1) (list (ash len -8) (logand len 255)) app1
                    '(#xff #xda 0 2)))))

(deftest exif-orientation-reader
  (ok (= 6 (img:exif-orientation (jpeg-with-orientation 6))))
  (ok (= 8 (img:exif-orientation (jpeg-with-orientation 8 :big nil))))
  (ok (null (img:exif-orientation (octets '(#xff #xd8 #xff #xda 0 2)))))
  ;; Garbage and truncation never read out of bounds.
  (ok (null (img:exif-orientation (subseq (jpeg-with-orientation 6) 0 20))))
  (ok (null (img:exif-orientation (octets '(1 2 3))))))

(deftest limits
  (let ((l (img:make-image-limits :max-width 10)))
    (ok (img:check-dimensions 10 10 4 :limits l))
    (ok (signals (img:check-dimensions 11 1 4 :limits l) 'img:image-limit-exceeded))
    (ok (eq :width (handler-case (img:check-dimensions 11 1 4 :limits l)
                     (img:image-limit-exceeded (c) (img:image-limit-exceeded-limit c))))))
  (ok (signals (img:check-dimensions 50000 50000 4) 'img:image-limit-exceeded))
  (ok (img:check-dimensions 50000 50000 4 :limits (img:make-image-limits :unlimited t))))
