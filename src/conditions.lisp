(in-package #:image-protocol)

(defvar *enabled-formats*)  ; set in sniff.lisp; the report below reads it

(define-condition image-error (error)
  ((format :initarg :format :initform nil :reader image-error-format)
   (backend :initarg :backend :initform nil :reader image-error-backend))
  (:report (lambda (c s) (format s "Image error~@[ (~(~A~))~]" (image-error-format c)))))

(define-condition image-unknown-format (image-error) ()
  (:report (lambda (c s) (declare (ignore c))
             (format s "Unknown image format (no signature matched and no :format given)"))))

(define-condition image-unsupported-format (image-error)
  ((operation :initarg :operation :initform :decode :reader image-error-operation))
  (:report (lambda (c s)
             (format s "No backend can ~(~A~) ~(~A~)~:[~; (format not enabled)~]"
                     (image-error-operation c) (image-error-format c)
                     (not (member (image-error-format c) *enabled-formats*))))))

(define-condition image-unsupported-feature (image-error)
  ((feature :initarg :feature :reader image-error-feature))
  (:report (lambda (c s) (format s "Unsupported ~(~A~) feature: ~A"
                                 (image-error-format c) (image-error-feature c)))))

(define-condition image-unsupported-option (image-error)
  ((option :initarg :option :reader image-error-option))
  (:report (lambda (c s) (format s "Option ~S is not supported for ~(~A~)~@[ by ~A~]"
                                 (image-error-option c) (image-error-format c)
                                 (image-error-backend c)))))

(define-condition image-limit-exceeded (image-error)
  ((limit :initarg :limit :reader image-limit-exceeded-limit)
   (allowed :initarg :allowed :reader image-limit-exceeded-allowed)
   (actual :initarg :actual :reader image-limit-exceeded-actual))
  (:report (lambda (c s) (format s "Image ~(~A~) ~:D exceeds the limit ~:D"
                                 (image-limit-exceeded-limit c) (image-limit-exceeded-actual c)
                                 (image-limit-exceeded-allowed c)))))

(define-condition image-decode-error (image-error)
  ((cause :initarg :cause :initform nil :reader image-error-cause))
  (:report (lambda (c s) (format s "Could not decode ~(~A~)~@[ with ~A~]~@[: ~A~]"
                                 (image-error-format c) (image-error-backend c)
                                 (image-error-cause c)))))

(define-condition image-truncated (image-decode-error) ())
(define-condition image-corrupt (image-decode-error) ())

(define-condition image-encode-error (image-error)
  ((cause :initarg :cause :initform nil :reader image-error-cause))
  (:report (lambda (c s) (format s "Could not encode ~(~A~)~@[: ~A~]"
                                 (image-error-format c) (image-error-cause c)))))

(define-condition image-native-unavailable (image-error)
  ((cause :initarg :cause :initform nil :reader image-error-cause))
  (:report (lambda (c s) (format s "Native image library unavailable~@[: ~A~]"
                                 (image-error-cause c)))))

(define-condition image-warning (warning) ())

(define-condition image-format-mismatch (image-warning)
  ((declared :initarg :declared :reader image-format-mismatch-declared)
   (detected :initarg :detected :reader image-format-mismatch-detected))
  (:report (lambda (c s) (format s "Data was declared ~(~A~) but looks like ~(~A~)"
                                 (image-format-mismatch-declared c)
                                 (image-format-mismatch-detected c)))))

(define-condition image-option-ignored (image-warning)
  ((format :initarg :format :initform nil :reader image-option-ignored-format)
   (option :initarg :option :reader image-option-ignored-option)
   (backend :initarg :backend :initform nil :reader image-option-ignored-backend))
  (:report (lambda (c s) (format s "Ignoring option ~S for ~(~A~)~@[ (~A cannot honour it)~]"
                                 (image-option-ignored-option c) (image-option-ignored-format c)
                                 (image-option-ignored-backend c)))))

(defvar *strict-options* nil
  "When true, an encode option a backend can't honour is an IMAGE-UNSUPPORTED-OPTION
error instead of an IMAGE-OPTION-IGNORED warning.")

(defun ignore-option (backend format option)
  "Backends call this for an encode OPTION they can't honour."
  (if *strict-options*
      (error 'image-unsupported-option :format format :option option :backend backend)
      (warn 'image-option-ignored :format format :option option :backend backend)))

(define-condition image-crc-mismatch (image-warning)
  ((chunk :initarg :chunk :initform nil :reader image-crc-mismatch-chunk))
  (:report (lambda (c s) (format s "CRC mismatch~@[ in chunk ~A~]" (image-crc-mismatch-chunk c)))))
