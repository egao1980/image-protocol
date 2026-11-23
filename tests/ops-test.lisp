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
