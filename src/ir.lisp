(in-package :cl-user)
(defpackage cl-9cc.ir
  (:use #:cl
        #:cl-9cc.util)
  (:import-from #:cl-9cc.parse
                #:node-lhs
                #:node-rhs
                #:node-val
                #:node-ty
                #:+nd-num+)
  (:import-from #:cl-9cc.util
                #:new-vec
                #:vec-push)
  (:export #:gen-ir))
(in-package :cl-9cc.ir)


;; Intermediate representation

(defconstant +ir-imm+ 0)
(defconstant +ir-mov+ 1)
(defconstant +ir-return+ 2)
(defconstant +ir-kill+ 3)
(defconstant +ir-nop+ 4)


(defstruct ir
  op
  lhs
  rhs)

(defun new-ir (op lhs rhs)
  (make-ir :op op :lhs lhs :rhs rhs))

(defparameter *regno* 0)

(defun gen-ir-sub (v node)
  (if (eql +nd-num+ (node-ty node))
    (let ((r *regno*))
      (vec-push v (new-ir +ir-imm+ r (node-val node)))
      (incf *regno*)
      r)
    (progn
      (assert (find (node-ty node) "+-*" :test #'eql))
      (let ((lhs (gen-ir-sub v (node-lhs node))) (rhs (gen-ir-sub v (node-rhs node))))
        (vec-push v (new-ir (node-ty node) lhs rhs))
        (vec-push v (new-ir +ir-kill+ rhs 0))
        lhs))))

(defun gen-ir (node)
  (let* ((v (new-vec)) (r (gen-ir-sub v node)))
    (vec-push v (new-ir +ir-return+ r 0))
    v))


