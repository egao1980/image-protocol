(in-package #:image-protocol)

;;; Masked magic signatures. A signature is (format offset bytes mask) where
;;; BYTES and MASK are octet lists; a mask byte of 0 matches anything.

(defvar *signatures* '())

(defun register-signature (format offset bytes &optional mask)
  "Teach SNIFF-FORMAT a signature. Later registrations are tried first."
  (push (list format offset (coerce bytes '(simple-array (unsigned-byte 8) (*)))
              (and mask (coerce mask '(simple-array (unsigned-byte 8) (*)))))
        *signatures*)
  format)

(defun %ascii (string) (map 'list #'char-code string))

(dolist (s `((:jxl 0 (#xff #x0a))
             (:ico 0 (0 0 1 0))
             (:qoi 0 ,(%ascii "qoif"))
             (:tiff 0 (#x4d #x4d 0 #x2a)) (:tiff 0 (#x49 #x49 #x2a 0))
             (:pnm 0 ,(%ascii "P1")) (:pnm 0 ,(%ascii "P2")) (:pnm 0 ,(%ascii "P3"))
             (:pnm 0 ,(%ascii "P4")) (:pnm 0 ,(%ascii "P5")) (:pnm 0 ,(%ascii "P6"))
             (:pnm 0 ,(%ascii "P7"))
             (:hdr 0 ,(%ascii "#?RADIANCE")) (:hdr 0 ,(%ascii "#?RGBE"))
             (:psd 0 ,(%ascii "8BPS"))
             (:avif 4 ,(%ascii "ftypavif")) (:avif 4 ,(%ascii "ftypavis"))
             (:heic 4 ,(%ascii "ftypheic")) (:heic 4 ,(%ascii "ftypheix"))
             (:heic 4 ,(%ascii "ftypmif1"))
             (:webp 0 ,(%ascii "RIFF....WEBP") ,(mapcar (lambda (c) (if (char= c #\.) 0 #xff))
                                                         (coerce "RIFF....WEBP" 'list)))
             (:bmp 0 ,(%ascii "BM"))
             (:gif 0 ,(%ascii "GIF87a")) (:gif 0 ,(%ascii "GIF89a"))
             (:jpeg 0 (#xff #xd8 #xff))
             (:png 0 (#x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a))))
  (apply #'register-signature s))

(defun %matches (octets offset bytes mask)
  (and (<= (+ offset (length bytes)) (length octets))
       (loop for i below (length bytes)
             always (let ((m (if mask (aref mask i) #xff)))
                      (= (logand m (aref octets (+ offset i))) (logand m (aref bytes i)))))))

(defun sniff-format (octets)
  "→ the format keyword whose signature matches the start of OCTETS, or NIL.
TGA has no signature and is never sniffed."
  (loop for (format offset bytes mask) in *signatures*
        when (%matches octets offset bytes mask) return format))

(defparameter +default-enabled-formats+ '(:png :jpeg :gif :bmp))

(setf *enabled-formats* (copy-list +default-enabled-formats+))
(setf (documentation '*enabled-formats* 'variable)
      "Formats decoders may handle. PSD, HDR, PNM and TGA are opt-in: their
loaders are where most decoder bugs have been.")
