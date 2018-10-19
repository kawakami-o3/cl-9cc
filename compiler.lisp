(defpackage :cl-go
  (:use :cl))
(in-package :cl-go)


;(defun tokenize (chars)
;  (flet ((make-token (acc-ch acc-token chs)
;	   (case (car chs)
;	     (#\space
;  (concatenate 'string chars))

(defun read-file (stream)
  ;;  (let (code buf tokenp stringp)
    (let (code)
    (loop
      :for ch = (read-char stream nil :eof)
      :until (eq ch :eof)
      :do (push ch code)
      :finally (return (nreverse code)))))

(defun main (&rest argv)
  (declare (ignorable argv))
  (let* ((file (string (car argv)))
	(in (open file :if-does-not-exist nil)))
    ;;    (format t "~a~%" (tokenize (read-file in)))))
    (format t "~a~%" (read-file in))))
 


;;; vim: set ft=lisp lisp:
