#|
  This file is a part of cl-9cc project.
  Copyright (c) 2018 kawakami
|#

(defsystem "cl-9cc-test"
  :defsystem-depends-on ("prove-asdf")
  :author "kawakami"
  :license "MIT"
  :depends-on ("cl-9cc"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "cl-9cc"))))
  :description "Test system for cl-9cc"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
