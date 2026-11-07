(in-package #:image-protocol)

;;; Pixels: interleaved, row-major, top-left first, no stride, straight
;;; (non-premultiplied) alpha. Depth 8 in wave 1; 16 / :FLOAT reserved.

(defparameter pixel-formats '(:gray :gray-alpha :rgb :rgba))

(defun channels (format)
  (ecase format (:gray 1) (:gray-alpha 2) (:rgb 3) (:rgba 4)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defstruct image
  (width 0 :type (integer 0))
  (height 0 :type (integer 0))
  (format :rgba :type (member :gray :gray-alpha :rgb :rgba))
  (depth 8 :type (member 8 16 :float))
  (pixels (make-array 0 :element-type '(unsigned-byte 8)) :type octets)
  (orientation nil :type (or null (integer 1 8)))
  (icc-profile nil :type (or null octets))
  (gamma nil :type (or null real))
  (color-space :srgb :type keyword))

(defstruct (image-frame (:include image))
  "A composited canvas at the sequence's logical size."
  (delay-ms 0 :type (integer 0))
  (frame-rect nil :type list)        ; (x y w h) of the raw frame, if known
  (disposal nil :type (member nil :none :background :previous))
  (blend nil :type (member nil :source :over)))

(defstruct image-sequence
  (frames '() :type list)
  (loop-count nil :type (or null (integer 0))))   ; 0 = forever

(defstruct image-info
  (format nil :type (or null keyword))
  (width 0 :type (integer 0))
  (height 0 :type (integer 0))
  (pixel-format :rgba :type keyword)
  (depth 8)
  (animated-p nil :type boolean)
  (frame-count nil :type (or null (integer 0)))
  (orientation nil :type (or null (integer 1 8)))
  (icc-p nil :type boolean))

(defun %check-image (image)
  "Signal IMAGE-CORRUPT if IMAGE's pixel vector does not match its size."
  (let ((expected (* (image-width image) (image-height image) (channels (image-format image)))))
    (unless (= expected (length (image-pixels image)))
      (error 'image-corrupt :cause (format nil "~D pixel bytes, expected ~D"
                                           (length (image-pixels image)) expected))))
  image)
