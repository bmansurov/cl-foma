(defsystem "cl-foma"
  :version "0.1.0"
  :author "bmansurov"
  :license "lgpl-3"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Common Lisp bindings for Foma"
  :in-order-to ((test-op (test-op "cl-foma/tests"))))

(defsystem "cl-foma/tests"
  :author "bmansurov"
  :license "LGPL-3"
  :depends-on ("cl-foma"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-foma"
  :perform (test-op (op c) (symbol-call :rove :run c)))
