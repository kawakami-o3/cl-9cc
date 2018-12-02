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
                #:*tokens*)
  (:import-from #:cl-9cc.util #:exit-error)
  (:export #:expr
           #:+nd-num+))
(in-package :cl-9cc.parse)

(defparameter *pos* 0)

(defconstant +nd-num+ 256)

(defstruct node
  ty
  lhs
  rhs
  val)

(defun new-node (op lhs rhs)
  (make-node :ty op :lhs lhs :rhs rhs))

(defun new-node-num (val)
  (make-node :ty +nd-num+ :val val))

(defun num ()
  (if (eql (token-ty (aref *tokens* *pos*)) +tk-num+)
    (let ((ret (new-node-num (token-val (aref *tokens* *pos*)))))
      (incf *pos*)
      ret)
    (exit-error "number expected, but got ~a" (token-str (aref *tokens* *pos*)))))

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

(defun parse (v)
  (setf *tokens* v)
  (setf *pos* 0)
  (let ((node (expr)))
    (if (not (eql (token-ty (aref *tokens* *pos*)) +tk-eof+))
      (exit-error "stray token: ~a" (token-str (aref *tokens* *pos*))))
    node))

