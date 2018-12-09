(in-package :cl-user)
(defpackage cl-9cc.parse
  (:use #:cl
        #:cl-9cc.token)
  (:import-from #:cl-9cc.token
                #:token-name
                #:token-ty
                #:token-val
                #:token-str
                #:+tk-eof+
                #:+tk-ident+
                #:+tk-num+
                #:+tk-return+
                #:*tokens*)
  (:import-from #:cl-9cc.util
                #:inc!
                #:new-vec
                #:vec-push
                #:exit-error)
  (:export #:expr
           #:+nd-num+))
(in-package :cl-9cc.parse)


(defconstant +nd-num+ 256)
(defconstant +nd-ident+ 257)
(defconstant +nd-return+ 258)
(defconstant +nd-comp-stmt+ 259)
(defconstant +nd-expr-stmt+ 260)

(defparameter *pos* 0)

(defun expect (ty)
  (let ((tok (aref *tokens* *pos*)))
    (if (not (eql ty (token-ty tok)))
      (exit-error "~a (~a) expected, but got ~a (~a)" ty ty (token-ty tok) (token-str tok))
      (incf *pos*))))

(defun consume (ty)
  (let ((tok (aref *tokens* *pos*)))
    (if (not (eql (token-ty tok) ty))
      nil
      (progn
        (incf *pos*)
        t))))

(defstruct node
  ty
  lhs
  rhs
  val
  name
  expr
  stmts)

(defun new-node (op lhs rhs)
  (make-node :ty op :lhs lhs :rhs rhs))

(defun term ()
  (let ((node (make-node)) (tok (aref *tokens* (inc! *pos*))))
    (cond
      ((eql #\( (token-ty tok))
       (setf node (assign))
       (expect #\))
       node)
      ((eql +tk-num+ (token-ty tok))
       (setf (node-ty node) +nd-num+)
       (setf (node-val node) (token-val tok))
       node)
      ((eql +tk-ident+ (token-ty tok))
       (setf (node-ty node) +nd-ident+)
       (setf (node-name node) (token-name tok))
       node)
      (t
        (exit-error "number expected, but got ~a ~a" (token-str tok) tok)))))

(defun mul ()
  (let ((lhs (term)) (op))
    (loop :do (progn
                (setf op (token-ty (aref *tokens* *pos*)))
                (if (and (not (eql op #\*)) (not (eql op #\/)))
                  (return)
                  (progn
                    (incf *pos*)
                    (setf lhs (new-node op lhs (term)))))))
    lhs))

(defun expr ()
  (let ((lhs (mul)) (op))
    (loop :do (progn
                (setf op (token-ty (aref *tokens* *pos*)))
                (if (and (not (eql op #\+)) (not (eql op #\-)))
                  (return)
                  (progn
                    (incf *pos*)
                    (setf lhs (new-node op lhs (mul)))))))
    lhs))

(defun assign ()
  (let ((lhs (expr)))
    (if (consume #\=)
      (new-node #\= lhs (expr))
      lhs)))

(defun stmt ()
  (let ((node (make-node :ty +nd-comp-stmt+ :stmts (new-vec))))
    (loop :do (let ((tok (aref *tokens* *pos*)))
                (if (eql +tk-eof+ (token-ty tok))
                  (return)
                  (let ((e (make-node)))
                    (if (eql +tk-return+ (token-ty tok))
                      (progn
                        (incf *pos*)
                        (setf (node-ty e) +nd-return+)
                        (setf (node-expr e) (assign)))
                      (progn
                        (setf (node-ty e) +nd-expr-stmt+)
                        (setf (node-expr e) (assign))))
                    (vec-push (node-stmts node) e)
                    (expect #\;)))))
    node))

(defun parse (v)
  (setf *tokens* v)
  (setf *pos* 0)
  (stmt))

