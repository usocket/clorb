;;;-*- Mode: Lisp; Package: NET.CDDR.REDPAS -*-

(in-package "NET.CDDR.REDPAS")

;;; ==========================================================================

(defgeneric token (lexer))
(defgeneric token-value (lexer))
(defgeneric next-token (lexer))
(defgeneric lexer-print-pos (lexer stream))

(defclass lexer ()
  ((token :reader token)))

(defmethod token-value ((lexer lexer))
  (token lexer))

(defmethod lexer-print-pos ((lexer lexer) stream)
  (declare (ignore stream)))


;;; ==========================================================================

(defvar *lexer-read-table*  (copy-readtable nil))

(setf (readtable-case *lexer-read-table*) :preserve)

(defun single-char-macro (stream char)
  (declare (ignore stream))
  char)

(map nil (lambda (c)
           (set-macro-character c #'single-char-macro nil *lexer-read-table*))
     "!#%&/()=+?.,;:-*<>|[]{}")



(defclass readtable-lexer (lexer)
  ((readtable
    :initarg :readtable
    :initform *lexer-read-table*
    :accessor lexer-readtable)
   (package
    :initarg :package
    :initform *package*
    :accessor lexer-package)
   (stream
    :initarg :stream
    :accessor lexer-stream)))

(defmethod next-token ((lexer readtable-lexer))
  (setf (slot-value lexer 'token)
        (with-standard-io-syntax
          (let ((*package* (lexer-package lexer))
                (*readtable* (lexer-readtable lexer)))
            (read (lexer-stream lexer) nil :eof)))))

(defmethod lexer-print-pos ((lexer readtable-lexer) stream)
  (format stream "~&At file position: ~D." (file-position (lexer-stream lexer))))


;;; ===========================================================================

(defclass streamchar-lexer (lexer)
  ((stream
    :initarg :stream
    :accessor lexer-stream)))

(defmethod next-token ((lexer streamchar-lexer))
  (setf (slot-value lexer 'token) 
        (read-char (lexer-stream lexer) nil :eof)))

(defmethod lexer-print-pos ((lexer streamchar-lexer) stream)
  (format stream "~&At file position: ~D." (file-position (lexer-stream lexer))))

;;; ===========================================================================

(define-condition alt-error (error)
  ((lexer :initarg :lexer :reader error-lexer)
   (alt-tokens :initarg :alt-tokens :reader error-alt-tokens))
  (:report alt-error-print))

(defun alt-error-print (condition stream)
  (format stream "A '~A' token found when expected one of ~A"
          (token (error-lexer condition))
          (error-alt-tokens condition))
  (lexer-print-pos (error-lexer condition) stream))

(define-condition failed-match (error)
                  ((lexer :initarg :lexer :reader error-lexer)
                   (token :initarg :token
                          :initform nil
                          :reader error-token)
                   (predicate :initarg :predicate
                              :initform nil
                              :reader error-predicate))
  (:report failed-match-print))

(defun failed-match-print (condition stream)
  (if (error-predicate condition)
    (format stream "A '~A' token found when expected one of type ~A"
            (token (error-lexer condition))
            (error-predicate condition))
    (format stream "A '~A' token found when expected token ~A"
            (token (error-lexer condition))
            (error-token condition)))
  (lexer-print-pos (error-lexer condition) stream))



;;; ===========================================================================

(defvar *failable* nil)
(defvar *failed-tokens* nil)

(defun match-token (lexer token)
  (cond ((equal token (token lexer))
         (setq *failable* nil)
         (values t
                 (prog1 (token-value lexer)
                   (next-token lexer))))
        (*failable*
         (push token *failed-tokens*)
         nil)
        (t
         (error 'failed-match :lexer lexer :token token))))

(defun match-pred (lexer pred-name pred)
  (cond ((funcall pred (token lexer))
         (setq *failable* nil)
         (values t
                 (prog1 (token-value lexer)
                   (next-token lexer))))
        (*failable*
         (push pred-name *failed-tokens*)
         nil)
        (t
         (error 'failed-match :lexer lexer :predicate pred-name))))

