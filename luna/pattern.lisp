;;;; pattern.lisp -- pattern matcher for testing 

(in-package "NET.CDDR.LUNA")

;;;; match-fail condition

(define-condition match-fail (warning)
                  ((object :initarg :object :reader match-fail-object)
                   (message :initarg :message :reader match-fail-message)
                   (inside :initarg :indside  :initform nil :accessor match-fail-inside))
  (:report print-match-fail))

(defun print-match-fail (condition stream)
  (format stream "object ~S : ~A~@[ in ~S~]" 
          (match-fail-object condition)
          (match-fail-message condition)
          (match-fail-inside condition)))

(defvar *failed-match-warning* t)

(defun fail-match (object format &rest args)
  (funcall (if *failed-match-warning* #'warn #'signal)
           'match-fail
           :object object
           :message (apply #'cl:format nil format args)))


;;;; Basic pattern

(defgeneric match (pattern object))

(defun boolean-match (pattern object boolean)
  (unless boolean
    (fail-match object "~S does not match ~S" object pattern)))

(defmethod match ((pattern t) object)
  (boolean-match pattern object (equalp pattern object)))



(defclass PATTERN ()
  ((args :initarg :args :accessor pattern-args)))

(defun pattern (&rest args)
  (make-instance 'pattern :args args))

(defmethod match ((pattern pattern) object)
  (loop for (key value) on (pattern-args pattern) by #'cddr
        do (let ((attval 
                  (cond ((consp key)
                         (let ((obj-pos (position '* key)))
                           (cond (obj-pos
                                  (apply (car key)
                                         (loop for x in (cdr key)
                                               collect (if (eql x '*) object x))))
                                 (t (apply (car key) object (cdr key))))))
                        (t
                         (funcall key object)))))
             (handler-case 
               (match value attval)
               (match-fail (condition)
                           (fail-match object "~S ~A" key (match-fail-message condition)))))))



;;;; Sequence Pattern

(defclass SEQUENCE-PATTERN (pattern)
  ())

(defun sequence-pattern (&rest args)
  (make-instance 'sequence-pattern :args args))

(defmethod match ((pattern sequence-pattern) object)
  (unless (typep object 'sequence)
    (fail-match object "Not a sequence"))
  (let ((args (pattern-args pattern))
        (len (length object)))
    (unless (= len (length args))
      (fail-match object "Wrong length ~D /= ~D" len (length args)))
    (let ((i 0))
      (handler-case
        (map nil (lambda (pattern-element object-element)
                   (match pattern-element object-element)
                   (incf i))
             args object)
        (match-fail (condition)
                    (fail-match object "[~d] ~A" i (match-fail-message condition)))))))



;;;; Sexp pattern

(defclass SEXP-PATTERN (pattern)
  ())

(defun sexp-pattern (sexp)
  (make-instance 'sexp-pattern :args sexp))

(defmethod match ((pattern sexp-pattern) object)
  (match-sexp (pattern-args pattern) object))

(defun match-sexp (pattern object)
  (if (consp pattern)
    (progn
      (unless (listp object)
        (fail-match object "not a list (pattern ~S)" pattern))
      (handler-case
        (case (car pattern)
          (&key (match-keys (cdr pattern) object))
          (&any (match-sexp (cdr pattern) (cdr object)))
          (&rest (map nil (lambda (obj) (match-sexp (second pattern) obj)) object))
          (&any-rest)
          (otherwise
           (progn (match-sexp (car pattern) (car object))
                  (match-sexp (cdr pattern) (cdr object)))))
        
        (match-fail (condition)
                    (when (tailp (match-fail-inside condition) object)
                      (setf (match-fail-inside condition) object))
                    (signal condition))))
    (match pattern object)))
  
(defun match-keys (keys list)
  (let ((seen '()) (allow-other-keys nil))
    (when (eql (car keys) '&allow-other-keys)
      (setf allow-other-keys (pop keys)))
    (when (oddp (length list))
      (fail-match list "odd key-value list"))
    (loop for (key value) on list by #'cddr
          do (let ((spec (assoc key keys)))
               (unless (or spec allow-other-keys)
                 (fail-match key "illegal key"))
               (when (member key seen) (fail-match key "duplicated key"))
               (push key seen)
               (when (cddr spec)
                 (match-sexp (caddr spec) value))))
    (loop for (key option) in keys
          when (eq option :required)
          do (unless (member key seen)
               (fail-match list "missing required key ~S" key)))))


;;;; Eval-To pattern

(defclass EVAL-TO-PATTERN (pattern)
  ())

(defun eval-to (form)
  (make-instance 'eval-to-pattern :args form))

(defmethod match ((pattern eval-to-pattern) object)
  (match (pattern-args pattern)
         (eval object)))


;;;; OR-Pattern

(defclass or-pattern (pattern)
  ())

(defun or-pattern (&rest patterns)
  (make-instance 'or-pattern :args patterns))

(defmethod match ((pattern or-pattern) object)
  (let ((list (pattern-args pattern))
        (messages '()))
    (loop while list do 
          (handler-case 
            (progn (match (pop list) object)
                   (return t))
            (match-fail (condition)
                        (push (match-fail-message condition) messages)
                        (unless list
                          (fail-match object
                                      "~{~A~^ OR ~}" messages)))))))


