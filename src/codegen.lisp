(in-package :cl-user)
(defpackage cl-9cc.codegen
  (:use #:cl
        #:cl-9cc.regalloc)
  (:import-from #:cl-9cc.ir
                #:ir-op
                #:ir-lhs
                #:ir-rhs
                #:+ir-imm+
                #:+ir-mov+
                #:+ir-nop+
                #:+ir-return+)
  (:import-from #:cl-9cc.regalloc #:*regs*)
  (:export #:gen-x86))
(in-package :cl-9cc.codegen)



(defun gen-x86 (irv)
  (loop :for i :from 0 :below (length irv) :by 1
        :do (let* ((ir (aref irv i)) (op (ir-op ir)))
              (cond ((eql op +ir-imm+)
                     (format t "  mov ~a, ~a~%" (aref *regs* (ir-lhs ir)) (ir-rhs ir)))
                    ((eql op +ir-mov+)
                     (format t "  mov ~a, ~a~%" (aref *regs* (ir-lhs ir)) (aref *regs* (ir-rhs ir))))
                    ((eql op +ir-return+)
                     (format t "  mov rax, ~a~%" (aref *regs* (ir-lhs ir)))
                     (format t "  ret~%"))
                    ((eql op #\+)
                     (format t "  add ~a, ~a~%" (aref *regs* (ir-lhs ir)) (aref *regs* (ir-rhs ir))))
                    ((eql op #\-)
                     (format t "  sub ~a, ~a~%" (aref *regs* (ir-lhs ir)) (aref *regs* (ir-rhs ir))))
                    ((= op +ir-nop+))
                    (t (assert (and 0 "unkonw operator")))))))


