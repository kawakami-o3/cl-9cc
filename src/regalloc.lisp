(in-package :cl-user)
(defpackage cl-9cc.regalloc
  (:use #:cl
				#:cl-9cc.util)
	(:import-from #:cl-9cc.ir
                #:ir-op
                #:ir-lhs
                #:ir-rhs
                #:+ir-alloca+
                #:+ir-imm+
                #:+ir-kill+
                #:+ir-load+
                #:+ir-mov+
                #:+ir-nop+
                #:+ir-return+
                #:+ir-store+)
	(:import-from #:cl-9cc.util #:exit-error)
  (:export #:*reg-map*))
(in-package :cl-9cc.regalloc)


(defparameter *regs* (make-array 8 :initial-contents
                                 '("rdi" "rsi" "r10" "r11" "r12" "r13" "r14" "r15")))
(defparameter *used* (make-array (length *regs*) :initial-element nil))

(defparameter *reg-map* (make-array 0))

(defun alloc (ir-reg)
  (if (not (= (aref *reg-map* ir-reg) -1))
    (let ((r (aref *reg-map* ir-reg)))
      (assert (aref *used* r))
      r)
    (block exit
           (loop :for i :from 0 :below (length *regs*) :by 1
                 :do (progn
                       (if (not (aref *used* i))
                         (progn
                           (setf (aref *used* i) t)
                           (setf (aref *reg-map* ir-reg) i)
                           (return-from exit i)))))
           (exit-error "register exhausted"))))

(defun kill (r)
  (assert (aref *used* r))
  (setf (aref *used* r) nil))

(defun alloc-regs (irv)
  (setf *reg-map* (make-array (length irv)))
  (loop :for i :from 0 :below (length irv) :by 1
        :do (setf (aref *reg-map* i) -1))

  (loop :for i :from 0 :below (length irv) :by 1
        :do (let* ((ir (aref irv i)) (op (ir-op ir)))
              (cond ((or (eql op +ir-imm+)
                         (eql op +ir-alloca+)
                         (eql op +ir-return+))
                     (setf (ir-lhs ir) (alloc (ir-lhs ir))))
                    ((or (eql op +ir-mov+)
                         (eql op +ir-load+)
                         (eql op +ir-store+)
                         (eql op #\+)
                         (eql op #\-)
                         (eql op #\*)
                         (eql op #\/))
                     (setf (ir-lhs ir) (alloc (ir-lhs ir)))
                     (setf (ir-rhs ir) (alloc (ir-rhs ir))))
                    ;;((eql op +ir-return+)
                     ;;(kill (aref *reg-map* (ir-lhs ir))))
                     ;;(setf (ir-lhs ir) (alloc (ir-lhs ir))))
                    ((eql op +ir-kill+)
                     (kill (aref *reg-map* (ir-lhs ir)))
                     (setf (ir-op ir) +ir-nop+))
                    (t (exit-error "unkonw operator"))))))

