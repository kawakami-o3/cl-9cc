(in-package :cl-user)
(defpackage cl-9cc.util
  (:use #:cl)
  (:export
    #:inc!
    #:noop #:exit-error
    #:new-vec #:vec-push))
(in-package :cl-9cc.util)

(defmacro inc! (i)
  `(let ((j ,i))
     (incf ,i)
     j))

(defun noop ())

(defun exit-error (fmt &rest args)
  (apply #'format (cons t (cons fmt args)))
  (format t "~%")
  (abort))

;; Vector
(defun new-vec ()
  (make-array 16 :initial-element nil :fill-pointer 0 :adjustable t))

(defun vec-push (v elem)
  (vector-push-extend elem v))


