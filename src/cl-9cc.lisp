(defpackage cl-9cc
  (:use :cl
        :cl-9cc.ir
        :cl-9cc.parse
        :cl-9cc.regalloc
        :cl-9cc.token)
  (:import-from #:cl-9cc.codegen #:gen-x86)
  (:import-from #:cl-9cc.ir #:gen-ir)
  (:import-from #:cl-9cc.parse #:parse)
  (:import-from #:cl-9cc.regalloc
                #:alloc-regs
                #:*reg-map*)
  (:import-from #:cl-9cc.token
                #:tokenize
                #:*tokens*)
  (:export #:main))
(in-package :cl-9cc)

(defun main (code)
  (let ((node))
;;    (loop :for i :from 0 :below (length *reg-map*) :by 1
;;          :do (setf (aref *reg-map* i) -1))
    (setf *tokens* (tokenize code))
    (setf node (parse *tokens*))

    (let ((irv (gen-ir node)))
      (alloc-regs irv)

      (format t ".intel_syntax noprefix~%")
      (format t ".global main~%")
      (format t "main:~%")
      (gen-x86 irv))))

