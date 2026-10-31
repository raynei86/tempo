(defsystem "tempo"
  :version "0.0.1"
  :author "Lihui Zhang"
  :license "MIT"
  :depends-on ("metabang-bind")
  :serial t
  :components ((:module "src"
                :components
                ((:file "duration")
                 (:file "main"))))
  :description "A Date and Time Library"
  :in-order-to ((test-op (test-op "tempo/tests"))))

(defsystem "tempo/tests"
  :author "Lihui Zhang"
  :license "MIT"
  :depends-on ("tempo"
               "rove")
  :serial t
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for tempo"
  :perform (test-op (op c) (symbol-call :rove :run c)))
