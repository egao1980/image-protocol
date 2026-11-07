(in-package #:image-protocol)

(defstruct (image-limits (:constructor %make-image-limits))
  (max-width 16384) (max-height 16384) (max-pixels (expt 2 28)) (max-frames 1000)
  (max-bytes (* 512 1024 1024)) (max-input-bytes (* 256 1024 1024)))

(defun make-image-limits (&rest keys &key unlimited &allow-other-keys)
  "Limits for decoding untrusted input. (MAKE-IMAGE-LIMITS :UNLIMITED T)
turns every check off; other keys override single defaults."
  (let ((keys (copy-list keys)))
    (remf keys :unlimited)
    (if unlimited
        (%make-image-limits :max-width nil :max-height nil :max-pixels nil :max-frames nil
                            :max-bytes nil :max-input-bytes nil)
        (apply #'%make-image-limits keys))))

(defvar *image-limits* (make-image-limits)
  "Limits used when a call gives no :LIMITS.")

(defun %over (limit allowed actual format)
  (when (and allowed (> actual allowed))
    (error 'image-limit-exceeded :limit limit :allowed allowed :actual actual :format format)))

(defun check-dimensions (width height channels &key (limits *image-limits*) format (frames 1))
  "Signal IMAGE-LIMIT-EXCEEDED unless a WIDTH × HEIGHT image (FRAMES of
them) fits LIMITS. Backends call this after the header probe, before
allocating pixels."
  (%over :width (image-limits-max-width limits) width format)
  (%over :height (image-limits-max-height limits) height format)
  (%over :pixels (image-limits-max-pixels limits) (* width height) format)
  (%over :frames (image-limits-max-frames limits) frames format)
  (%over :bytes (image-limits-max-bytes limits) (* width height channels frames) format)
  t)

(defun check-frames (count &key (limits *image-limits*) format)
  (%over :frames (image-limits-max-frames limits) count format))
