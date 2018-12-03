(in-package :cl-user)
(defpackage cl-9cc.parse
  (:use #:cl
        #:cl-9cc.token)
  (:import-from #:cl-9cc.token
                #:token-ty
                #:token-val
                #:token-str
                #:+tk-eof+
                #:+tk-num+
                #:+tk-return+
                #:*tokens*)
  (:import-from #:cl-9cc.util
                #:new-vec
                #:vec-push
                #:exit-error)
  (:export #:expr
           #:+nd-num+))
(in-package :cl-9cc.parse)


(defconstant +nd-num+ 256)
(defconstant +nd-return+ 257)
(defconstant +nd-comp-stmt+ 258)
(defconstant +nd-expr-stmt+ 259)

(defparameter *pos* 0)

(defun expect (ty)
  (let ((tok (aref *tokens* *pos*)))
    (if (not (eql ty (token-ty tok)))
      (exit-error "~a (~a) expected, but got ~a (~a)" ty ty (token-ty tok) (token-ty tok))
      (incf *pos*))))

(defstruct node
  ty
  lhs
  rhs
  val
  expr
  stmts)

(defun new-node (op lhs rhs)
  (make-node :ty op :lhs lhs :rhs rhs))

(defun num ()
  (if (not (eql (token-ty (aref *tokens* *pos*)) +tk-num+))
    (exit-error "number expected, but got ~a" (token-str (aref *tokens* *pos*)))
    (let ((ret (make-node :ty +nd-num+ :val (token-val (aref *tokens* *pos*)))))
      (incf *pos*)
      ret)))

(defun mul ()
  (let ((lhs (num)) (op))
    (loop :do (progn
                (setf op (token-ty (aref *tokens* *pos*)))
                (if (and (not (eql op #\*)) (not (eql op #\/)))
                  (return)
                  (progn
                    (incf *pos*)
                    (setf lhs (new-node op lhs (num)))))))
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
                        (setf (node-expr e) (expr)))
                      (progn
                        (setf (node-ty e) +nd-expr-stmt+)
                        (setf (node-expr e) (expr))))
                    (vec-push (node-stmts node) e)
                    (expect #\;)))))
    node))

(defun parse (v)
  (setf *tokens* v)
  (setf *pos* 0)
  (stmt))
;;  (let ((node (expr)))
;;    (if (not (eql (token-ty (aref *tokens* *pos*)) +tk-eof+))
;;      (exit-error "stray token: ~a" (token-str (aref *tokens* *pos*))))
;;    node))

