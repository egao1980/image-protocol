(in-package #:image-protocol)

;;; Header-only dimensions for the default formats, read by the protocol
;;; itself so limits are enforced the same way whatever the backend (and
;;; before a backend's own caps or allocations come into play).

(defun %be16 (v i) (logior (ash (aref v i) 8) (aref v (1+ i))))
(defun %be32 (v i) (logior (ash (%be16 v i) 16) (%be16 v (+ i 2))))
(defun %le16 (v i) (logior (aref v i) (ash (aref v (1+ i)) 8)))
(defun %le32s (v i)
  (let ((u (logior (%le16 v i) (ash (%le16 v (+ i 2)) 16))))
    (if (logbitp 31 u) (- u (expt 2 32)) u)))

(defun %peek-jpeg (v)
  (let ((n (length v)) (i 2))
    (loop while (< (+ i 9) n)
          do (unless (= (aref v i) #xff) (return nil))
             (let ((m (aref v (1+ i))))
               (cond ((= m #xff) (incf i))                  ; fill byte
                     ((or (<= #xd0 m #xd9) (= m #x01)) (incf i 2))
                     ((and (<= #xc0 m #xcf) (not (member m '(#xc4 #xc8 #xcc))))
                      (return (values (%be16 v (+ i 7)) (%be16 v (+ i 5)))))
                     (t (incf i (+ 2 (%be16 v (+ i 2))))))))))

(defun peek-dimensions (format octets)
  "→ (values width height) from FORMAT's header in OCTETS, or NIL when the
format isn't one the protocol reads or the header is too short."
  (let ((n (length octets)))
    (case format
      (:png (when (>= n 24) (values (%be32 octets 16) (%be32 octets 20))))
      (:gif (when (>= n 10) (values (%le16 octets 6) (%le16 octets 8))))
      (:bmp (when (>= n 26) (values (abs (%le32s octets 18)) (abs (%le32s octets 22)))))
      (:jpeg (%peek-jpeg octets)))))
