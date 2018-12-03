(in-package :cl-user)
(defpackage cl-9cc.ir
  (:use #:cl
        #:cl-9cc.util)
  (:import-from #:cl-9cc.parse
                #:node-lhs
                #:node-rhs
                #:node-val
                #:node-stmts
                #:node-expr
                #:node-ty
                #:+nd-comp-stmt+
                #:+nd-expr-stmt+
                #:+nd-return+
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

(defparameter *code* (new-vec))

(defstruct ir
  op
  lhs
  rhs)

(defun add (op lhs rhs)
  (let ((ir (make-ir :op op :lhs lhs :rhs rhs)))
    (vec-push *code* ir)
    ir))

(defparameter *regno* 0)

(defun gen-expr (node)
  (if (eql +nd-num+ (node-ty node))
    (let ((r *regno*))
      (add +ir-imm+ r (node-val node))
      (incf *regno*)
      r)
    (progn
      (assert (find (node-ty node) "+-*/" :test #'eql))
      (let ((lhs (gen-expr (node-lhs node))) (rhs (gen-expr (node-rhs node))))
        (add (node-ty node) lhs rhs)
        (add +ir-kill+ rhs 0)
        lhs))))

(defun gen-stmt (node)
  (cond ((eql +nd-return+ (node-ty node))
         (let ((r (gen-expr (node-expr node))))
           (add +ir-return+ r 0)
           (add +ir-kill+ r 0)))
        ((eql +nd-expr-stmt+ (node-ty node))
         (let ((r (gen-expr (node-expr node))))
           (add +ir-kill+ r 0)))
        ((eql +nd-comp-stmt+ (node-ty node))
         (loop :for s :across (node-stmts node)
               :do (gen-stmt s)))))

(defun gen-ir (node)
  (assert (eql +nd-comp-stmt+ (node-ty node)))
  (gen-stmt node)
  *code*)

