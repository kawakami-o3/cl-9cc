(in-package :cl-user)
(defpackage cl-9cc.codegen
  (:use #:cl
        #:cl-9cc.regalloc)
  (:import-from #:cl-9cc.ir
                #:ir-op
                #:ir-lhs
                #:ir-rhs
                #:+ir-alloca+
                #:+ir-imm+
                #:+ir-add-imm+
                #:+ir-mov+
                #:+ir-nop+
                #:+ir-load+
                #:+ir-store+
                #:+ir-return+)
  (:import-from #:cl-9cc.regalloc #:*regs*)
  (:export #:gen-x86))
(in-package :cl-9cc.codegen)


(defparameter *n* -1)

(defun gen-label ()
  (incf *n*)
  (format nil ".L~a" *n*))

(defun gen-x86 (irv)
  (let ((ret (gen-label)))

    (format t "  push rbp~%")
    (format t "  mov rbp, rsp~%")

    (loop :for i :from 0 :below (length irv) :by 1
          :do (let* ((ir (aref irv i)) (op (ir-op ir)))
                (cond ((eql op +ir-imm+)
                       (format t "  mov ~a, ~a~%" (aref *regs* (ir-lhs ir)) (ir-rhs ir)))

                      ((eql op +ir-add-imm+)
                       (format t "  add ~a, ~a~%" (aref *regs* (ir-lhs ir)) (ir-rhs ir)))

                      ((eql op +ir-mov+)
                       (format t "  mov ~a, ~a~%" (aref *regs* (ir-lhs ir)) (aref *regs* (ir-rhs ir))))

                      ((eql op +ir-return+)
                       (format t "  mov rax, ~a~%" (aref *regs* (ir-lhs ir)))
                       (format t "  jmp ~a~%" ret))

                      ((eql op +ir-alloca+)
                       (if (not (eql 0 (ir-rhs ir)))
                         (format t "  sub rsp, ~a~%" (ir-rhs ir)))
                       (format t "  mov ~a, rsp~%" (aref *regs* (ir-lhs ir))))

                      ((eql op +ir-load+)
                       (format t "  mov ~a, [~a]~%" (aref *regs* (ir-lhs ir)) (aref *regs* (ir-rhs ir))))

                      ((eql op +ir-store+)
                       (format t "  mov [~a], ~a~%" (aref *regs* (ir-lhs ir)) (aref *regs* (ir-rhs ir))))

                      ((eql op #\+)
                       (format t "  add ~a, ~a~%" (aref *regs* (ir-lhs ir)) (aref *regs* (ir-rhs ir))))

                      ((eql op #\-)
                       (format t "  sub ~a, ~a~%" (aref *regs* (ir-lhs ir)) (aref *regs* (ir-rhs ir))))

                      ((eql op #\*)
                       (format t "  mov rax, ~a~%" (aref *regs* (ir-rhs ir)))
                       (format t "  mul ~a~%" (aref *regs* (ir-lhs ir)))
                       (format t "  mov ~a, rax~%" (aref *regs* (ir-lhs ir))))

                      ((eql op #\/)
                       (format t "  mov rax, ~a~%" (aref *regs* (ir-lhs ir)))
                       (format t "  cqo~%")
                       (format t "  div ~a~%" (aref *regs* (ir-rhs ir)))
                       (format t "  mov ~a, rax~%" (aref *regs* (ir-lhs ir))))

                      ((= op +ir-nop+))

                      (t (assert (and 0 "unknown operator"))))))
    
    (format t "~a:~%" ret)
    (format t "  mov rsp, rbp~%")
    (format t "  mov rsp, rbp~%")
    (format t "  pop rbp~%")
    (format t "  ret~%")))

