(in-package #:image-protocol/tests)

;;; Issue #2: image editing operations (Pillow's most used calls).
;;; Proposed API (names from #2, signatures from #3), eager and functional:
;;;   (resize-image image width height &key (filter :lanczos3))  ; :nearest :bilinear :lanczos3
;;;   (thumbnail image max-width max-height)   ; keep aspect, never enlarge (Pillow)
;;;   (crop-image image x y width height)
;;;   (rotate-image image degrees)             ; 90 180 270, counter-clockwise like Pillow
;;;   (flip-image image direction)             ; :horizontal (mirror) / :vertical
;;;   (composite-image dst src &key (x 0) (y 0) (operator :over))
;;; Calls go through OP so this file compiles before the API exists.

(defun %op (name)
  (multiple-value-bind (sym status) (find-symbol name :image-protocol)
    (when (and sym (eq status :external) (fboundp sym))
      (symbol-function sym))))

(defmacro with-ops ((&rest bindings) &body body)
  "Bind each (VAR NAME) to the exported image-protocol function NAME;
   FAIL once, naming every missing one, if any is not defined."
  `(let ,(loop for (var name) in bindings collect `(,var (%op ,name)))
     (let ((missing (loop for (var . name) in (list ,@(loop for (var name) in bindings
                                                             collect `(cons ,var ,name)))
                          unless var collect name)))
       (if missing
           (fail (format nil "not defined in image-protocol: ~{~(~a~)~^, ~}" missing))
           (progn ,@body)))))

(defun %px (image) (coerce (img:image-pixels image) 'list))

(defun %solid (w h format pixel)
  (img:make-image :width w :height h :format format
                  :pixels (octets (loop repeat (* w h) append pixel))))

(deftest ops-crop
  (with-ops ((crop "CROP-IMAGE"))
    (let* ((g (grid 3 2))                     ; 0 1 2 / 3 4 5
           (c (funcall crop g 1 0 2 2)))
      (ok (equal (list 2 2 '(1 2 4 5)) (list (img:image-width c) (img:image-height c) (%px c))))
      (ok (equal '(0 1 2 3 4 5) (%px g)) "input unchanged")
      (ok (signals (funcall crop g 2 0 2 2) 'img:image-error) "out of bounds"))))

(deftest ops-rotate-and-flip
  (with-ops ((rotate "ROTATE-IMAGE") (flip "FLIP-IMAGE"))
    (flet ((r (deg) (let ((i (funcall rotate (grid 3 2) deg)))
                      (list (img:image-width i) (img:image-height i) (%px i))))
           (f (dir) (%px (funcall flip (grid 3 2) dir))))
      (ok (equal (r 90) '(2 3 (2 5 1 4 0 3))) "90 = counter-clockwise (Pillow)")
      (ok (equal (r 180) '(3 2 (5 4 3 2 1 0))))
      (ok (equal (r 270) '(2 3 (3 0 4 1 5 2))))
      (ok (equal (f :horizontal) '(2 1 0 5 4 3)))
      (ok (equal (f :vertical) '(3 4 5 0 1 2))))))

(deftest ops-resize
  (with-ops ((resize "RESIZE-IMAGE"))
    (let ((n (funcall resize (grid 3 2) 6 4 :filter :nearest)))
      (ok (equal (list 6 4 :gray) (list (img:image-width n) (img:image-height n) (img:image-format n))))
      (ok (equal (%px n) '(0 0 1 1 2 2  0 0 1 1 2 2  3 3 4 4 5 5  3 3 4 4 5 5))
          "nearest doubles each pixel"))
    (dolist (filter '(:nearest :bilinear :lanczos3))
      (let ((r (funcall resize (%solid 8 6 :rgb '(10 128 250)) 3 2 :filter filter)))
        (ok (equal (list 3 2 :rgb) (list (img:image-width r) (img:image-height r) (img:image-format r))))
        (ok (every (lambda (row) (equal row '(10 128 250)))
                   (loop for (a b c) on (%px r) by #'cdddr collect (list a b c)))
            (format nil "~(~a~) keeps a flat colour flat" filter))))
    (let ((d (funcall resize (grid 4 4) 2 2)))
      (ok (= 2 (img:image-width d)) "default filter (Lanczos3) works"))
    ;; Premultiplied alpha: a transparent green pixel must not tint opaque red.
    (let* ((src (img:make-image :width 2 :height 1 :format :rgba
                                :pixels (octets '(255 0 0 255  0 255 0 0))))
           (p (%px (funcall resize src 1 1 :filter :bilinear))))
      (ok (and (= 255 (first p)) (= 0 (second p)) (<= 127 (fourth p) 128))
          (format nil "premultiplied resize, got ~s" p)))))

(deftest ops-thumbnail
  (with-ops ((thumb "THUMBNAIL"))
    (let ((tn (funcall thumb (%solid 400 200 :rgb '(1 2 3)) 100 100)))
      (ok (equal '(100 50) (list (img:image-width tn) (img:image-height tn))) "keeps aspect"))
    (let ((tn (funcall thumb (%solid 50 20 :rgb '(1 2 3)) 100 100)))
      (ok (equal '(50 20) (list (img:image-width tn) (img:image-height tn))) "never enlarges"))))

(deftest ops-composite-over
  (with-ops ((composite "COMPOSITE-IMAGE"))
    (let* ((dst (img:make-image :width 2 :height 1 :format :rgba
                                :pixels (octets '(0 0 255 255  0 0 255 255))))
           (src (img:make-image :width 1 :height 1 :format :rgba
                                :pixels (octets '(255 0 0 128))))
           (out (funcall composite dst src :x 1 :y 0)))
      (ok (equal (%px out) '(0 0 255 255  128 0 127 255))
          (format nil "Porter-Duff over at (1,0), got ~s" (%px out)))
      (ok (equal (%px dst) '(0 0 255 255  0 0 255 255)) "dst unchanged"))))

;;; Pillow parity (image-protocol#2 fix). Expected values come from Pillow
;;; 11.3 (tests/pillow-reference.py). Pillow resamples in 8-bit fixed point
;;; and premultiplies RGBA in 8 bits, so resampled samples may differ by 1
;;; (by 2 for RGBA). Sizes and composites are exact.

(defun %pattern (w h)
  "Pillow's Image.new('L', (w, h)) filled with (37x + 91y) mod 256."
  (img:make-image :width w :height h :format :gray
                  :pixels (octets (loop for y below h
                                        append (loop for x below w
                                                     collect (mod (+ (* x 37) (* y 91)) 256))))))

(defun %close-p (got expected tolerance)
  (and (= (length got) (length expected))
       (every (lambda (a b) (<= (abs (- a b)) tolerance)) got expected)))

(deftest-parametrize ops-resize-pillow-parity
    ((filter down up)
     (:box '(110 117 124 82 131 139)
           '(0 0 37 74 74 0 0 37 74 74 91 91 128 165 165 91 91 128 165 165))
     (:bilinear '(99 129 129 106 118 128)
                '(0 15 37 59 74 23 38 60 82 97 68 83 105 127 142 91 106 128 150 165))
     (:bicubic '(95 130 133 110 116 126)
               '(0 3 29 55 69 18 30 56 82 96 70 83 109 135 149 96 110 136 162 176))
     (:lanczos3 '(91 128 139 115 114 123)
                '(0 0 21 50 61 20 29 58 87 98 68 78 107 136 147 103 115 144 173 184)))
  (let ((d (%px (img:resize-image (%pattern 7 5) 3 2 :filter filter)))
        (u (%px (img:resize-image (%pattern 3 2) 5 4 :filter filter))))
    (ok (%close-p d down 1) (format nil "~(~a~) 7x5 -> 3x2: ~s, Pillow ~s" filter d down))
    (ok (%close-p u up 1) (format nil "~(~a~) 3x2 -> 5x4: ~s, Pillow ~s" filter u up))))

(deftest ops-resize-reducing-gap-and-rgba-parity
  (let ((gap (%px (img:resize-image (%pattern 40 30) 7 5 :reducing-gap 2)))
        (plain (%px (img:resize-image (%pattern 40 30) 7 5))))
    (ok (%close-p gap '(114 121 125 122 132 127 132 135 116 121 126 122 132 127 132 137 115 120
                        126 123 132 128 131 138 117 120 125 123 131 127 131 138 116 120 128)
                  1)
        (format nil "reducing_gap=2: ~s" gap))
    (ok (%close-p plain '(115 121 123 126 128 131 133 134 119 121 124 126 129 130 133 134 119 120
                          124 126 130 128 133 134 120 120 123 124 128 128 132 134 120 120 126)
                  1)
        (format nil "no gap: ~s" plain)))
  (let* ((src (img:make-image :width 4 :height 1 :format :rgba
                              :pixels (octets '(255 0 0 255  0 255 0 64  0 0 255 0  255 255 255 200))))
         (got (%px (img:resize-image src 2 1 :filter :bilinear))))
    (ok (%close-p got '(202 50 0 137  230 255 230 95) 2)
        (format nil "RGBA premultiplied: ~s" got))))

(deftest ops-thumbnail-pillow-sizes
  (dolist (row '((400 200 100 100 (100 50)) (50 20 100 100 (50 20)) (1000 333 128 128 (128 43))
                 (333 1000 128 128 (43 128)) (640 480 100 50 (67 50)) (3 1000 10 10 (1 10))
                 (1001 1000 100 100 (100 100))))
    (destructuring-bind (w h mw mh expected) row
      (let ((tn (img:thumbnail (%solid w h :gray '(7)) mw mh)))
        (ok (equal expected (list (img:image-width tn) (img:image-height tn)))
            (format nil "~dx~d into ~dx~d" w h mw mh))))))

(deftest ops-fit-image
  "image-protocol#3's fit-image: one or both sides, enlarge on request."
  (let ((src (%solid 400 300 :gray '(9))))
    (flet ((size (&rest keys)
             (let ((i (apply #'img:fit-image src keys)))
               (list (img:image-width i) (img:image-height i)))))
      (ok (equal '(154 116) (size :max-width 154)) "300 × 154/400 = 115.5, half up")
      (ok (equal '(67 50) (size :max-height 50)))
      (ok (equal '(100 75) (size :max-width 100 :max-height 100)) "the tighter side wins")
      (ok (eq src (img:fit-image src :max-width 1000)) "not enlarged by default")
      (ok (equal '(1000 750) (size :max-width 1000 :enlarge t)))
      (ok (every (lambda (v) (= v 9)) (%px (img:fit-image src :max-width 37))) "flat stays flat")
      (ok (signals (img:fit-image src) 'img:image-invalid-argument) "needs a side")
      (ok (signals (img:fit-image src :max-width 0) 'img:image-invalid-argument)))))

(deftest ops-composite-pillow-parity
  (let ((dst (img:make-image :width 3 :height 1 :format :rgba
                             :pixels (octets '(10 20 30 255  200 100 50 128  0 0 0 0))))
        (src (img:make-image :width 3 :height 1 :format :rgba
                             :pixels (octets '(255 255 255 77  0 128 255 200  90 80 70 33)))))
    (ok (equal '(84 91 98 255  24 125 230 228  90 80 70 33) (%px (img:composite-image dst src)))
        "Image.alpha_composite"))
  (let* ((dst (%solid 3 3 :rgb '(0 0 0)))
         (src (%solid 2 2 :rgba '(255 255 255 255)))
         (out (img:composite-image dst src :x -1 :y 2)))
    (ok (equal :rgb (img:image-format out)) "DST's format")
    (ok (equal '(0 0 0  0 0 0  0 0 0
                 0 0 0  0 0 0  0 0 0
                 255 255 255  0 0 0  0 0 0)
               (%px out))
        "clipped at the edges; negative offsets"))
  (let ((dst (%solid 2 1 :gray '(0))))
    (ok (equal '(76 0) (%px (img:composite-image dst (%solid 1 1 :rgb '(255 0 0)))))
        "opaque RGB over gray: BT.601 luma")
    (ok (eq dst (img:composite-image dst (%solid 1 1 :rgb '(255 0 0)) :x 5))
        "no overlap: DST itself")))

(deftest ops-arguments-and-identity
  (let ((g (grid 3 2)))
    (ok (eq g (img:resize-image g 3 2)) "same size: IMAGE itself")
    (ok (eq g (img:rotate-image g 360)))
    (ok (equal '(2 3 (3 0 4 1 5 2)) (let ((r (img:rotate-image g -90)))
                                      (list (img:image-width r) (img:image-height r) (%px r))))
        "-90 = 270 counter-clockwise")
    (ok (equal (%px (img:rotate-image g 90)) (%px (img:rotate-image g 90.0))))
    (ok (eq g (img:thumbnail g 10 10)) "fits: IMAGE itself")
    (ok (equal '(1 2 4 5) (%px (img:crop-image g 1 0 2 2))))
    (ok (= 0 (img:image-width (img:crop-image g 3 0 0 2))) "an empty crop at the edge")
    (dolist (bad (list (lambda () (img:rotate-image g 45))
                       (lambda () (img:flip-image g :diagonal))
                       (lambda () (img:crop-image g -1 0 1 1))
                       (lambda () (img:crop-image g 0 0 1 3))
                       (lambda () (img:resize-image g 0 2))
                       (lambda () (img:resize-image g 2 2 :filter :hamming))
                       (lambda () (img:resize-image g 2 2 :reducing-gap 1/2))
                       (lambda () (img:thumbnail g 0 5))
                       (lambda () (img:composite-image g g :operator :xor))
                       (lambda () (img:composite-image g g :x 1.5))
                       (lambda () (img:resize-image :not-an-image 2 2))))
      (ok (signals (funcall bad) 'img:image-invalid-argument)))
    (let ((c (handler-case (img:crop-image g 2 0 2 2)
               (img:image-invalid-argument (c) c))))
      (ok (and c (eq :crop (img:image-invalid-argument-operation c))
               (search "outside" (princ-to-string c)))
          (princ-to-string c))))
  (let ((wide (img:make-image :width 2 :height 1 :format :gray :depth 16
                              :pixels (octets '(1 2)))))
    (ok (signals (img:flip-image wide :vertical) 'img:image-unsupported-feature)
        "depth 16 is not edited yet"))
  (let ((img:*image-limits* (img:make-image-limits :max-width 100)))
    (ok (signals (img:resize-image (grid 3 2) 101 2) 'img:image-limit-exceeded)
        "output size within *image-limits*")))

(deftest ops-keep-format-metadata-and-flat-alpha
  (let* ((src (img:make-image :width 4 :height 4 :format :gray-alpha
                              :pixels (octets (loop repeat 16 append '(200 0)))
                              :orientation 6 :gamma 2.2))
         (r (img:resize-image src 2 2 :filter :lanczos3)))
    (ok (equal '(:gray-alpha 6 2.2) (list (img:image-format r) (img:image-orientation r)
                                          (img:image-gamma r))))
    (ok (every #'zerop (%px r)) "fully transparent stays transparent black"))
  (let ((r (img:resize-image (%solid 9 9 :rgba '(30 60 90 128)) 4 4 :linear t)))
    (ok (every (lambda (p) (equal p '(30 60 90 128)))
               (loop for (a b c d) on (%px r) by (lambda (l) (nthcdr 4 l))
                     collect (list a b c d)))
        "linear light keeps a flat colour flat")))
