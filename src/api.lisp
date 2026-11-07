(in-package #:image-protocol)

(defun %read-source (source limits)
  "SOURCE (octets, pathname or binary stream) → octets, at most
MAX-INPUT-BYTES."
  (let ((max (image-limits-max-input-bytes limits)))
    (flet ((from-stream (in)
             (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
                   (buf (make-array 65536 :element-type '(unsigned-byte 8))))
               (loop for n = (read-sequence buf in)
                     while (plusp n)
                     do (when (and max (> (+ (length out) n) max))
                          (error 'image-limit-exceeded :limit :input-bytes :allowed max
                                                       :actual (+ (length out) n)))
                        (loop for i below n do (vector-push-extend (aref buf i) out)))
               (coerce out 'octets))))
      (etypecase source
        ((simple-array (unsigned-byte 8) (*))
         (%over :input-bytes max (length source) nil)
         source)
        ((vector (unsigned-byte 8))
         (%over :input-bytes max (length source) nil)
         (coerce source 'octets))
        ((or pathname string)
         (with-open-file (in source :element-type '(unsigned-byte 8)) (from-stream in)))
        (stream (from-stream source))))))

(defun %resolve-format (octets format)
  "→ the format to decode as: FORMAT if given (warning on a mismatch with the
data's signature), else the sniffed one, else IMAGE-UNKNOWN-FORMAT."
  (let ((detected (sniff-format octets)))
    (cond ((and format detected (not (eq format detected)))
           (warn 'image-format-mismatch :declared format :detected detected)
           format)
          (format format)
          (detected detected)
          (t (error 'image-unknown-format)))))

(defun %with-backends (format operation formats function)
  "Call FUNCTION on the best backend for FORMAT / OPERATION. Restarts:
TRY-NEXT-BACKEND after a decode/encode error, USE-VALUE with a backend."
  (when (and (eq operation :decode) (not (member format formats)))
    (error 'image-unsupported-format :format format :operation operation))
  (let ((candidates (%candidates format operation)))
    (loop
      (when (null candidates)
        (restart-case (error 'image-unsupported-format :format format :operation operation)
          (use-value (backend)
            :report "Use another backend."
            (return (funcall function backend)))))
      (let ((backend (pop candidates)))
        (restart-case (return (funcall function backend))
          (try-next-backend ()
            :report "Try the next backend that supports this format."
            :test (lambda (c) (declare (ignore c)) candidates)
            nil)
          (use-value (other)
            :report "Use another backend."
            (return (funcall function other))))))))

(defun probe-image (source &key format (formats *enabled-formats*) (limits *image-limits*))
  "→ IMAGE-INFO for SOURCE without decoding pixels."
  (let* ((octets (%read-source source limits))
         (format (%resolve-format octets format)))
    (%with-backends format :decode formats
                    (lambda (backend)
                      (let ((info (backend-probe backend format octets)))
                        (unless (image-info-orientation info)
                          (setf (image-info-orientation info) (%orientation-for format octets)))
                        info)))))

(defun %orientation-for (format octets)
  (when (eq format :jpeg) (exif-orientation octets)))

(defun %precheck (format octets limits &key (frames 1))
  "Refuse oversized images from the header alone, before any backend runs."
  (multiple-value-bind (w h) (peek-dimensions format octets)
    (when w (check-dimensions w h 1 :limits limits :format format :frames frames))))

(defun %finish (image format octets orient)
  (%check-image image)
  (unless (image-orientation image)
    (setf (image-orientation image) (%orientation-for format octets)))
  (if orient (apply-orientation image) image))

(defun decode-image (source &key format (formats *enabled-formats*) (limits *image-limits*)
                                 orient backend)
  "Decode the first frame of SOURCE (octets, pathname or stream) → IMAGE.
FORMAT overrides sniffing. FORMATS is the allow-list. ORIENT T applies
EXIF orientation. BACKEND forces a backend."
  (let* ((octets (%read-source source limits))
         (format (%resolve-format octets format))
         (*image-limits* limits))
    (%precheck format octets limits)
    (flet ((run (b) (%finish (backend-decode b format octets :limits limits) format octets orient)))
      (if backend (run backend) (%with-backends format :decode formats #'run)))))

(defun decode-frames (source &key format (formats *enabled-formats*) (limits *image-limits*) backend)
  "Decode every frame of SOURCE → IMAGE-SEQUENCE of composited IMAGE-FRAMEs."
  (let* ((octets (%read-source source limits))
         (format (%resolve-format octets format))
         (*image-limits* limits))
    (%precheck format octets limits)
    (flet ((run (b)
             (let ((seq (backend-decode b format octets :limits limits :frames t)))
               (check-frames (length (image-sequence-frames seq)) :limits limits :format format)
               (mapc #'%check-image (image-sequence-frames seq))
               seq)))
      (if backend (run backend) (%with-backends format :decode formats #'run)))))

(defun map-frames (function source &rest keys &key &allow-other-keys)
  "Call FUNCTION on each frame of SOURCE in order. → the sequence's loop count."
  (let ((seq (apply #'decode-frames source keys)))
    (mapc function (image-sequence-frames seq))
    (image-sequence-loop-count seq)))

(defparameter +encode-defaults+
  ;; Subsampling has no default: a backend picks its own unless asked (at the
  ;; default quality 90, stb uses 4:2:0), so only explicit requests are checked.
  '((:jpeg :quality 90)
    (:png :compression-level 6)))

(defparameter +encode-options+
  '(:quality :compression-level :subsampling :progressive :loop-count :backend-options))

(defun %check-options (format options)
  (loop for key in options by #'cddr
        unless (member key +encode-options+)
          do (error 'image-unsupported-option :format format :option key))
  (destructuring-bind (&key quality compression-level subsampling progressive loop-count
                         backend-options) options
    (declare (ignore progressive backend-options))
    (when quality (check-type quality (integer 1 100)))
    (when compression-level (check-type compression-level (integer 0 9)))
    (when subsampling (check-type subsampling (member :444 :422 :420)))
    (when loop-count (check-type loop-count (integer 0))))
  (append options (rest (assoc format +encode-defaults+))))

(defun encode-image (image format &rest options &key backend &allow-other-keys)
  "Encode IMAGE as FORMAT → octets. Options: :QUALITY 1–100, :COMPRESSION-LEVEL
0–9, :SUBSAMPLING (:444 :422 :420), :PROGRESSIVE, :LOOP-COUNT,
:BACKEND-OPTIONS. Defaults: JPEG quality 90, PNG level 6.
No metadata is written."
  (%check-image image)
  (let ((options (copy-list options)))
    (remf options :backend)
    (let ((options (%check-options format options)))
      (flet ((run (b) (apply #'backend-encode b image format options)))
        (if backend (run backend) (%with-backends format :encode nil #'run))))))
