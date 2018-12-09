(in-package :cl-user)
(defpackage cl-9cc.ir
  (:use #:cl
        #:cl-9cc.util)
  (:import-from #:cl-9cc.parse
                #:node-lhs
                #:node-rhs
                #:node-val
                #:node-name
                #:node-stmts
                #:node-expr
                #:node-ty
                #:+nd-comp-stmt+
                #:+nd-expr-stmt+
                #:+nd-ident+
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
(defconstant +ir-alloca+ 3)
(defconstant +ir-load+ 4)
(defconstant +ir-store+ 5)
(defconstant +ir-kill+ 6)
(defconstant +ir-nop+ 7)

(defparameter *code* (new-vec))
(defparameter *regno* 1)
(defparameter *basereg* 0)
(defparameter *vars* (new-map))
(defparameter *bpoff* 0)

(defstruct ir
  op
  lhs
  rhs

  has-imm
  imm)

(defun add (op lhs rhs)
  (let ((ir (make-ir :op op :lhs lhs :rhs rhs)))
    (vec-push *code* ir)
    ir))

(defun add-imm (op lhs imm)
  (let ((ir (make-ir :op op :lhs lhs :has-imm t :imm imm)))
    (vec-push *code* ir)
    ir))

(defun gen-lval (node)
  (if (not (eql +nd-ident+ (node-ty node)))
    (exit-error "not an lvalue"))

  (if (not (map-exists *vars* (node-name node)))
    (progn
      (map-put *vars* (node-name node) *bpoff*)
      (setf *bpoff* (+ *bpoff* 8))))
  
  (let ((r (inc! *regno*)) (off (map-get *vars* (node-name node))))
    (add +ir-mov+ r *basereg*)
    (add-imm #\+ r off)
    r))

(defun gen-expr (node)
  (cond ((eql +nd-num+ (node-ty node))
         (let ((r (inc! *regno*)))
           (add +ir-imm+ r (node-val node))
           r))
        ((eql +nd-ident+ (node-ty node))
         (let ((r (gen-lval node)))
           (add +ir-load+ r r)
           r))
        ((eql #\= (node-ty node))
         (let ((rhs (gen-expr (node-rhs node)))
               (lhs (gen-lval (node-lhs node))))
           (add +ir-store+ lhs rhs)
           (add +ir-kill+ rhs -1)
           lhs))
        (t
          (progn
            (assert (find (node-ty node) "+-*/" :test #'eql))
            (let ((lhs (gen-expr (node-lhs node)))
                  (rhs (gen-expr (node-rhs node))))
              (add (node-ty node) lhs rhs)
              (add +ir-kill+ rhs -1)
              lhs)))))

(defun gen-stmt (node)
  (cond ((eql +nd-return+ (node-ty node))
         (let ((r (gen-expr (node-expr node))))
           (add +ir-return+ r -1)
           (add +ir-kill+ r -1)))
        ((eql +nd-expr-stmt+ (node-ty node))
         (let ((r (gen-expr (node-expr node))))
           (add +ir-kill+ r -1)))
        ((eql +nd-comp-stmt+ (node-ty node))
         (loop :for s :across (node-stmts node)
               :do (gen-stmt s)))))

(defun gen-ir (node)
  (assert (eql +nd-comp-stmt+ (node-ty node)))
  (let ((alloca (add +ir-alloca+ *basereg* -1)))
    (gen-stmt node)
    (setf (ir-rhs alloca) *bpoff*)
    (add +ir-kill+ *basereg* -1)
    *code*))

