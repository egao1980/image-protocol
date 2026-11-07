(in-package #:image-protocol)

;;; Several backends at once. Each says which (format operation priority)
;;; it handles; the highest priority wins, *BACKEND-PREFERENCE* overrides,
;;; ties go to registration order. Re-registering a backend's class replaces
;;; its entry, so reloading a system doesn't duplicate it.

(defclass image-backend () ()
  (:documentation "An image codec implementation. Subclass and implement
BACKEND-FORMATS, BACKEND-PROBE, BACKEND-DECODE and/or BACKEND-ENCODE."))

(defgeneric backend-formats (backend)
  (:documentation "→ list of (format operation priority), OPERATION :DECODE or :ENCODE."))

(defgeneric backend-probe (backend format octets)
  (:documentation "→ IMAGE-INFO from the header, without decoding pixels."))

(defgeneric backend-decode (backend format octets &key limits frames)
  (:documentation "Decode OCTETS. Without FRAMES → an IMAGE (first frame). With
FRAMES T → an IMAGE-SEQUENCE of composited IMAGE-FRAMEs. Must call
CHECK-DIMENSIONS / CHECK-FRAMES with LIMITS before allocating pixels and
signal only IMAGE-ERROR conditions for bad input."))

(defgeneric backend-encode (backend image format &key &allow-other-keys)
  (:documentation "Encode IMAGE → octets. Options arrive with protocol defaults
applied; signal IMAGE-UNSUPPORTED-OPTION (via WARN or ERROR, see
*STRICT-OPTIONS*) for ones the backend can't honour."))

(defvar *backends* '() "Registered backends, newest first.")
(defvar *registry-lock* (bt2:make-lock :name "image-backends"))

(defvar *backend-preference* '()
  "Backend class names (or instances) tried first, in order, regardless of priority.")


(defun register-image-backend (backend)
  "Make BACKEND available. Replaces any backend of the same class."
  (bt2:with-lock-held (*registry-lock*)
    (setf *backends*
          (cons backend (remove (class-of backend) *backends* :key #'class-of))))
  backend)

(defun unregister-image-backend (backend-or-class)
  (let ((class (if (typep backend-or-class 'image-backend)
                   (class-of backend-or-class)
                   (find-class backend-or-class))))
    (bt2:with-lock-held (*registry-lock*)
      (setf *backends* (remove class *backends* :key #'class-of)))))

(defun image-backends () (bt2:with-lock-held (*registry-lock*) (copy-list *backends*)))

(defun %preference-rank (backend)
  (or (position-if (lambda (p) (if (symbolp p) (typep backend p) (eq p backend)))
                   *backend-preference*)
      most-positive-fixnum))

(defun %candidates (format operation)
  "Backends able to do OPERATION on FORMAT, best first."
  (let* ((backends (image-backends))
         (entries (loop for b in backends
                        for i from 0
                        for entry = (find-if (lambda (e) (and (eq (first e) format)
                                                              (eq (second e) operation)))
                                             (backend-formats b))
                        when entry collect (list b (third entry) i))))
    (mapcar #'first
            (stable-sort entries
                         (lambda (a b)
                           (let ((ra (%preference-rank (first a))) (rb (%preference-rank (first b))))
                             (cond ((/= ra rb) (< ra rb))
                                   ((/= (second a) (second b)) (> (second a) (second b)))
                                   ;; Ties: registered earlier wins (list is newest first).
                                   (t (> (third a) (third b))))))))))

(defun supported-formats (&key (operation :decode))
  (remove-duplicates
   (loop for b in (image-backends)
         append (loop for (f op) in (backend-formats b) when (eq op operation) collect f))))
