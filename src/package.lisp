(defpackage #:image-protocol
  (:use #:cl)
  (:nicknames #:stack-image)
  (:export
   ;; conditions
   #:image-error #:image-unknown-format #:image-unsupported-format
   #:image-unsupported-feature #:image-unsupported-option #:image-limit-exceeded
   #:image-decode-error #:image-truncated #:image-corrupt #:image-encode-error
   #:image-native-unavailable #:image-warning #:image-format-mismatch #:image-crc-mismatch
   #:image-option-ignored #:ignore-option
   #:image-error-format #:image-error-backend #:image-error-cause #:image-error-operation
   #:image-error-feature #:image-error-option
   #:image-limit-exceeded-limit #:image-limit-exceeded-allowed #:image-limit-exceeded-actual
   #:image-format-mismatch-declared #:image-format-mismatch-detected
   #:try-next-backend #:use-partial-image
   ;; types
   #:image #:make-image #:image-p #:copy-image
   #:image-width #:image-height #:image-format #:image-depth #:image-pixels
   #:image-orientation #:image-icc-profile #:image-gamma #:image-color-space
   #:image-frame #:make-image-frame #:image-frame-p
   #:image-frame-delay-ms #:image-frame-frame-rect #:image-frame-disposal #:image-frame-blend
   #:image-sequence #:make-image-sequence #:image-sequence-frames #:image-sequence-loop-count
   #:image-info #:make-image-info #:image-info-p
   #:image-info-format #:image-info-width #:image-info-height #:image-info-pixel-format
   #:image-info-depth #:image-info-animated-p #:image-info-frame-count
   #:image-info-orientation #:image-info-icc-p
   #:channels #:pixel-formats
   ;; limits
   #:image-limits #:make-image-limits #:image-limits-max-width #:image-limits-max-height
   #:image-limits-max-pixels #:image-limits-max-frames #:image-limits-max-bytes
   #:image-limits-max-input-bytes #:*image-limits* #:check-dimensions #:check-frames
   ;; formats
   #:sniff-format #:peek-dimensions #:register-signature #:*enabled-formats* #:+default-enabled-formats+
   ;; conversion / orientation
   #:convert-image #:depth-16-to-8 #:apply-orientation #:exif-orientation
   ;; registry
   #:image-backend #:backend-formats #:backend-probe #:backend-decode #:backend-encode
   #:register-image-backend #:unregister-image-backend #:image-backends
   #:*backend-preference* #:supported-formats #:*strict-options*
   ;; api
   #:probe-image #:decode-image #:decode-frames #:map-frames #:encode-image))
