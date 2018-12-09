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
(defconstant +tk-ident+ 257)
(defconstant +tk-return+ 258)
(defconstant +tk-eof+ 259)

;; Token type
(defstruct token
  ty
  val
  name
  str)

(defparameter *tokens* (make-array 100))
(defparameter *keywords* (new-map))

(defun add-token (v ty input)
  (let ((tok (make-token :ty ty :str input)))
    (vec-push v tok)
    tok))

(defun scan (code)
  (let ((v (new-vec)) (i 0) (c ""))
    (loop :while (< i (length code))
          :do (progn
                (setf c (aref code i))
                 
                (cond
                  ;; Skip whitespace
                  ((string= " " c)
                   (incf i))

                  ;; Single-letter token
                  ((find c "+-*/;=()" :test #'string=)
                   (add-token v c c)
                   (incf i))

                  ;; Identifier
                  ((or (alpha-char-p c) (eql #\_ c))
                   (let ((len 1) (name (format nil "~a" c)))
                     (setf c (aref code (+ i len)))
                     (loop :while (or (alpha-char-p c) (digit-char-p c) (eql #\_ c))
                           :do (progn
                                 (setf name (format nil "~a~a" name c))
                                 (incf len)
                                 (setf c (aref code (+ i len)))))
                     (let ((ty (map-get *keywords* name)))
                       (if (= 0 ty)
                         (setf ty +tk-ident+))
                       (let ((tok (add-token v ty name)))
                         (setf (token-name tok) name)
                         (setf i (+ i len))))))

                  ;; Number
                  ((digit-char-p c)
                   (let ((n (format nil "~a" c)))
                     (loop :while (and (< (1+ i) (length code)) (digit-char-p (aref code (1+ i))))
                           :do (progn
                                 (setf n (format nil "~a~a" n (aref code (1+ i))))
                                 (incf i)))
                     (incf i)
                     (let ((tok (add-token v +tk-num+ n)))
                       (setf (token-val tok) (parse-integer n)))))
                  (t (exit-error "cannot tokenize: ~a" c)))))
    (add-token v +tk-eof+ "")
    v))

(defun tokenize (code)
  (map-put *keywords* "return" +tk-return+)
  (scan code))

