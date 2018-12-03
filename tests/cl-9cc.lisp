(defpackage cl-9cc-test
  (:use :cl
        :cl-9cc
        :prove))
(in-package :cl-9cc-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-9cc)' in your Lisp.

(defun vec-test ()
	(let ((vec (cl-9cc.util:new-vec)))
		(is 0 (length vec))

		(loop :for i :from 0 :below 100 :by 1
			 :do (cl-9cc.util:vec-push vec i))

		(is 100 (length vec))
		(is 0 (aref vec 0))
		(is 50 (aref vec 50))
		(is 99 (aref vec 99))))

(defun map-test ()
	(let ((map (cl-9cc.util:new-map)))
		(is 0 (cl-9cc.util:map-get map "foo"))

		(cl-9cc.util:map-put map "foo" 2)
		(is 2 (cl-9cc.util:map-get map "foo"))

		(cl-9cc.util:map-put map "bar" 4)
		(is 4 (cl-9cc.util:map-get map "bar"))

		(cl-9cc.util:map-put map "foo" 6)
		(is 6 (cl-9cc.util:map-get map "foo"))))

(plan nil)

(vec-test)
(map-test)

(finalize)
