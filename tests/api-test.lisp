(in-package #:image-protocol/tests)

(defparameter *raw* (raw-encode 2 1 3 '(1 2 3 4 5 6)))

(deftest decode-and-probe
  (with-backends ((make-instance 'raw-backend))
    (let ((i (img:decode-image *raw*)))
      (ok (equal (list (img:image-width i) (img:image-height i) (img:image-format i)) '(2 1 :rgb)))
      (ok (equalp (img:image-pixels i) #(1 2 3 4 5 6))))
    (let ((info (img:probe-image *raw*)))
      (ok (equal (list (img:image-info-width info) (img:image-info-format info)) '(2 :raw))))))

(deftest unknown-and-disabled-formats
  (with-backends ((make-instance 'raw-backend))
    (ok (signals (img:decode-image (octets '(9 9 9 9))) 'img:image-unknown-format))
    (let ((img:*enabled-formats* img:+default-enabled-formats+))
      (ok (signals (img:decode-image *raw*) 'img:image-unsupported-format)))
    (ok (signals (img:decode-image (octets '(#xff #xd8 #xff 0))) 'img:image-unsupported-format))))

(deftest limits-before-allocation
  (with-backends ((make-instance 'raw-backend))
    ;; Header claims 60000 × 60000; only 9 bytes exist.
    (let ((bomb (raw-encode 60000 60000 4 '())))
      (ok (signals (img:decode-image bomb) 'img:image-limit-exceeded))
      (ok (signals (img:probe-image bomb :limits (img:make-image-limits :max-input-bytes 4))
                   'img:image-limit-exceeded)))
    (ok (signals (img:decode-image (subseq (raw-encode 4 4 3 (make-list 48 :initial-element 0)) 0 20))
                 'img:image-truncated))))

(deftest priority-and-preference
  (let ((low (make-instance 'raw-backend :priority 10))
        (high (make-instance 'broken-backend :priority 90)))
    (with-backends (low high)
      ;; The higher priority (broken) one is tried first: no silent fallback.
      (ok (signals (img:decode-image *raw*) 'img:image-corrupt))
      ;; TRY-NEXT-BACKEND falls back on request.
      (ok (img:image-p (handler-bind ((img:image-corrupt
                                        (lambda (c) (invoke-restart (find-restart 'img:try-next-backend c)))))
                         (img:decode-image *raw*))))
      ;; Preference overrides priority.
      (let ((img:*backend-preference* '(raw-backend)))
        ;; broken-backend is also a raw-backend subclass, so name the exact instance.
        (let ((img:*backend-preference* (list low)))
          (ok (img:image-p (img:decode-image *raw*)))))
      ;; Re-registering a class replaces it.
      (img:register-image-backend (make-instance 'raw-backend :priority 10))
      (ok (= 2 (length (img:image-backends)))))))

(deftest format-mismatch-warns
  (with-backends ((make-instance 'raw-backend))
    (ok (signals (img:decode-image *raw* :format :png :formats '(:png :raw)) 'img:image-format-mismatch))))

(deftest frames
  (with-backends ((make-instance 'raw-backend))
    (let* ((f1 (raw-encode 1 1 1 '(7))) (f2 (raw-encode 1 1 1 '(8)))
           (anim (octets (append (map 'list #'char-code "RAWA") (list 2)
                                 (coerce f1 'list) (coerce f2 'list)))))
      (let ((seq (img:decode-frames anim)))
        (ok (= 2 (length (img:image-sequence-frames seq))))
        (ok (equal (mapcar #'img:image-frame-delay-ms (img:image-sequence-frames seq)) '(10 20)))
        (ok (eql 0 (img:image-sequence-loop-count seq))))
      (let ((seen '()))
        (img:map-frames (lambda (f) (push (aref (img:image-pixels f) 0) seen)) anim)
        (ok (equal seen '(8 7))))
      (ok (signals (img:decode-frames anim :limits (img:make-image-limits :max-frames 1))
                   'img:image-limit-exceeded)))))

(deftest encoding
  (with-backends ((make-instance 'raw-backend))
    (let ((i (img:decode-image *raw*)))
      (ok (equalp (img:encode-image i :raw) *raw*))
      (ok (signals (img:encode-image i :raw :quality 50) 'img:image-option-ignored))
      (let ((img:*strict-options* t))
        (ok (signals (img:encode-image i :raw :quality 50) 'img:image-unsupported-option)))
      (ok (signals (img:encode-image i :raw :bogus 1) 'img:image-unsupported-option))
      (ok (signals (img:encode-image i :raw :quality 500) 'type-error))
      (ok (signals (img:encode-image i :jpeg) 'img:image-unsupported-format)))))

;;; A mock that claims :jpeg, to test EXIF orientation on real JPEG headers.
(defclass jpeg-mock (raw-backend) ())
(defmethod img:backend-formats ((b jpeg-mock)) '((:jpeg :decode 50)))
(defmethod img:backend-decode ((b jpeg-mock) format octets &key limits frames)
  (declare (ignore format octets limits frames))
  (grid 3 2))

(deftest orientation-from-jpeg-exif
  (with-backends ((make-instance 'jpeg-mock))
    (let ((j (jpeg-with-orientation 6)))
      (ok (= 6 (img:image-orientation (img:decode-image j))))
      (let ((upright (img:decode-image j :orient t)))
        (ok (equal (list (img:image-width upright) (img:image-height upright)) '(2 3)))
        (ok (null (img:image-orientation upright)))))))
