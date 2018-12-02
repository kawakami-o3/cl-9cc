(defpackage #:cl-9cc/util
  (:use :cl))
(in-package #:cl-9cc/util)



;; Vector
(defun new-vec ()
  (make-array 16 :initial-element nil :fill-pointer 0 :adjustable t))

(defun vec-push (v elem)
  (vector-push-extend elem v))


