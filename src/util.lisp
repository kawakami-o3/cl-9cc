(in-package :cl-user)
(defpackage cl-9cc.util
  (:use #:cl)
  (:export
    #:inc!
    #:noop #:exit-error
    #:new-vec #:vec-push
    #:new-map #:map-get #:map-put))
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

;; Map

(defun new-map ()
  (make-hash-table :test #'equal))

(defun map-get (m key)
  (gethash key m 0))

(defun map-put (m key value)
  (setf (gethash key m) value))

