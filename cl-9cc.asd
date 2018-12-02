#|
  This file is a part of cl-9cc project.
  Copyright (c) 2018 kawakami
|#

#|
  9cc in Common Lisp

  Author: kawakami
|#

(defsystem "cl-9cc"
  :version "0.1.0"
  :author "kawakami"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "cl-9cc" :depends-on ("codegen" "ir" "parse" "regalloc" "token"))
							   (:file "codegen" :depends-on ("ir" "regalloc"))
							   (:file "ir" :depends-on ("parse" "util"))
							   (:file "parse" :depends-on ("token"))
							   (:file "regalloc" :depends-on ("ir" "util"))
							   (:file "token" :depends-on ("util"))
							   (:file "util"))))
  :description "9cc in Common Lisp"
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "cl-9cc-test"))))
