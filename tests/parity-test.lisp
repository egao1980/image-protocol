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
   without reducing_gap (3/2, 2, 3, and decimals such as 1.7 read as single
   floats), down, up and one side only, and L and RGB reduced 4× and 8×
   with and without a gap of 2 (image-protocol#9)."
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

(deftest pillow-parity-reducing-gap-as-written
  "evidence (PR #4, round 2): :reducing-gap is documented as \"a real >= 1,
   as Pillow's reducing_gap\", but Pillow's 1.7 is a double and the Lisp
   1.7 a user writes is a single float (1.70000005). For 34 → 10 columns
   Pillow's int(34 / 10 / 1.7) is 2 (3.4 / 1.7 = 2.0), here
   (truncate (/ 3.4d0 1.7f0)) is 1, so the box reduce is skipped and the
   samples differ. Pillow 11.3, the L noise of %NOISE-IMAGE (seed 77):
     im.resize((10, 3), Image.BICUBIC, reducing_gap=1.7).tobytes().hex()"
  (let ((src (%noise-image :gray 34 3 77))
        (pillow (%hex-octets "a6789866558862564e50aaac9f885aaf7e4b6b59ac70ad90706c6090a03b")))
    (ok (equalp pillow (img:image-pixels
                        (img:resize-image src 10 3 :filter :bicubic :reducing-gap 1.7d0)))
        "1.7d0 (Python's double) equals Pillow")
    (let ((got (img:image-pixels
                (img:resize-image src 10 3 :filter :bicubic :reducing-gap 1.7))))
      (ok (equalp pillow got)
          (format nil ":reducing-gap 1.7 (a single float) equals Pillow: ~s"
                  (%difference got pillow))))))

(defun %box-reduce-by-definition (src w h c fx fy)
  "The block-by-block definition %BOX-REDUCE-8 keeps: for each FX × FY block
   (narrower or shorter at the right and bottom edges) of N pixels and each
   channel, ((sum + floor(N / 2)) × division_UINT32(N, 8) mod 2^32) >> 24."
  (let* ((nw (ceiling w fx)) (nh (ceiling h fy))
         (dst (make-array (* nw nh c) :element-type '(unsigned-byte 8))))
    (dotimes (ny nh dst)
      (dotimes (nx nw)
        (let* ((x0 (* nx fx)) (x1 (min w (+ x0 fx)))
               (y0 (* ny fy)) (y1 (min h (+ y0 fy)))
               (n (* (- x1 x0) (- y1 y0)))
               (multiplier (image-protocol::%reduce-multiplier n)))
          (dotimes (ch c)
            (let ((sum (floor n 2)))
              (loop for y from y0 below y1
                    do (loop for x from x0 below x1
                             do (incf sum (aref src (+ ch (* c (+ x (* y w))))))))
              (setf (aref dst (+ ch (* c (+ nx (* ny nw)))))
                    (ash (logand (* sum multiplier) #xFFFFFFFF) -24)))))))))

(deftest box-reduce-equals-definition
  "image-protocol#9: %BOX-REDUCE-8, the box reduce of :reducing-gap, reads
   the source row by row with one sum per channel. Its samples and size
   equal the block-by-block definition for 1-4 channels, every factor from
   1 to 9 on each axis, whole and partial edge blocks, and 1-pixel-wide or
   -high images. (Pillow parity of the gap itself: PILLOW-PARITY-RESIZE.)"
  (let ((failures '()) (n 0))
    (dolist (size '((1 1) (1 9) (9 1) (7 5) (37 23) (64 48) (65 49) (117 9)))
      (destructuring-bind (w h) size
        (loop for c from 1 to 4
              for src = (%lcg-noise (* w h c) (+ w (* 100 h) (* 10000 c)))
              do (loop for fx from 1 to (min w 9)
                       do (loop for fy from 1 to (min h 9)
                                do (incf n)
                                   (multiple-value-bind (got gw gh)
                                       (image-protocol::%box-reduce-8 src w h c fx fy)
                                     (unless (and (= gw (ceiling w fx)) (= gh (ceiling h fy))
                                                  (equalp got (%box-reduce-by-definition
                                                               src w h c fx fy)))
                                       (push (list w h c fx fy) failures))))))))
    (ok (and (plusp n) (null failures))
        (format nil "~d box reduces equal the definition~@[; these differ (w h c fx fy): ~s~]"
                n (reverse failures)))))
