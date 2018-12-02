(in-package :cl-user)
(defpackage cl-9cc.token
  (:use #:cl
        #:cl-9cc.util)
  (:import-from #:cl-9cc.util #:new-vec)
  (:export #:tokenize
           #:token-ty
           #:token-val
           #:+tk-eof+
           #:+tk-num+
           #:*tokens*))
(in-package :cl-9cc.token)

;; Tokenizer
(defconstant +tk-num+ 256)
(defconstant +tk-eof+ 257)

;; Token type
(defstruct token
  ty
  val
  str)

(defparameter *tokens* (make-array 100))

(defun add-token (v ty input)
  (let ((tok (make-token :ty ty :str input)))
    (vec-push v tok)
    tok))

(defun tokenize (code)
  (let ((v (new-vec)) (tok ""))
    (loop
      :for c :across code
      :do (cond
            ((string= " " c) (noop))
            ((or (string= "+" c) (string= "-" c))
             (let ((tk (add-token v +tk-num+ tok)))
               (setf (token-val tk) (parse-integer tok)))
             (add-token v c c)
             (setf tok ""))
            ;; digit-char-p
            (t (setf tok (format nil "~a~a" tok c)))))
    (if (> (length tok) 0)
      (let ((tk (add-token v +tk-num+ tok)))
        (setf (token-val tk) (parse-integer tok))))
    (add-token v +tk-eof+ "")
    v))


