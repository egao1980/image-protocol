(defsystem "image-protocol"
  :version "0.1.0"
  :description "CLOS image decoding/encoding protocol for cl-stack: pixels, limits, sniffing, conversion, orientation, multi-backend dispatch"
  :author "egao1980"
  :license "MIT"
  :depends-on ("bordeaux-threads")
  :serial t
  :pathname "src"
  :components ((:file "package")
               (:file "conditions")
               (:file "types")
               (:file "limits")
               (:file "sniff")
               (:file "peek")
               (:file "convert")
               (:file "exif")
               (:file "registry")
               (:file "api"))
  :in-order-to ((test-op (test-op "image-protocol/tests"))))

(defsystem "image-protocol/tests"
  :depends-on ("image-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "mock-backend")
               (:file "core-test")
               (:file "api-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
