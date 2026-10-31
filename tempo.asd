(defsystem "tempo"
  :version "0.0.1"
  :author "Lihui Zhang"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A Date and Time Library"
  :in-order-to ((test-op (test-op "tempo/tests"))))

(defsystem "tempo/tests"
  :author "Lihui Zhang"
  :license "MIT"
  :depends-on ("tempo"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for tempo"
  :perform (test-op (op c) (symbol-call :rove :run c)))
