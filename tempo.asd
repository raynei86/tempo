(defsystem "tempo"
  :version "0.5"
  :author "Lihui Zhang"
  :license "MIT"
  :serial t
  :components ((:module "src"
                :components
                ((:file "conditions")
                 (:file "duration")
                 (:file "date")
                 (:file "wall-time")
                 (:file "datetime")
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
                ((:file "date")
                 (:file "duration")
                 (:file "wall-time")
                 (:file "datetime")
                 (:file "main")
                 (:file "docs"))))
  :description "Test system for tempo"
  :perform (test-op (op c) (symbol-call :rove :run c)))
