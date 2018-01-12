(in-package "NET.CDDR.LUNA")

(defvar *test-suite-debug* nil)
(defvar *test-suite-result* nil)
(defvar *tc-current* nil)
(defvar *tc-current-sub*)
(defvar *temporary-suite-variables* nil)

#|
(setq *test-suite-debug* t)
(setq *test-suite-debug* nil)
|#

(defgeneric add-error (self)
  (:method ((self null))))

(defgeneric start-test-case (self &optional name)
  (:method ((self null) &optional name) (declare (ignore name))))

(defgeneric end-test-case (self)
  (:method ((self null))))

(defclass test-result ()
  ((suite :initarg :suite
          :reader result-suite)
   (current :initform nil
            :accessor current-test-case)
   (count :initform 0
          :accessor result-count)
   (errors :initform 0
           :accessor result-errors)
   (parent :initarg :parent
           :reader result-parent)))


(defmethod add-error ((self test-result))
  (incf (result-errors self))
  (add-error (result-parent self)))

(defmethod start-test-case ((self test-result) &optional name)
  (incf (result-count self))
  (when name
    (setf (current-test-case self) name))
  (start-test-case (result-parent self)))

(defmethod end-test-case ((self test-result)))

(defmethod print-result ((self test-result))
  (format t "~&;;; ------------- Suite '~A' result -----------------~%"
          (result-suite self))
  (format t ";;; ~D test~:p executed with ~D error~:p~%"
          (result-count self)
          (result-errors self)))


(defun tc-report (msg &rest args)
  (add-error *test-suite-result*)
  (format *debug-io* "~&;;; In test case ~A~@[/~A~]~%;;;! ~A~%" 
          *tc-current* *tc-current-sub*
          (apply #'cl:format nil msg args))
  (when *test-suite-debug*
    (break "Failed ensure")))

(defmacro ensure (bool &optional description &rest args)
  `(unless ,bool
     ,(if description
        (if args 
          `(tc-report ,description ,@args)          
          `(tc-report "FAIL: ~A" ,description))
        `(tc-report "FAIL: ~S" ',bool))))

(defun ensure-eql (is shouldbe)
  (unless (eql is shouldbe)
    (tc-report "~S~_ should be~_ ~S"
               is shouldbe)))

(defun ensure-equalp (is shouldbe)
  (unless (equalp is shouldbe)
    (tc-report "~S~_ should be~_ ~S"
               is shouldbe)))

(defun ensure-typep (obj type)
  (unless (typep obj type)
    (tc-report "~S shoubd be of type ~S, but is ~S"
               obj type (type-of obj))))

(defun ensure-pattern (obj pattern)
  (handler-case 
    (match pattern obj)
    (match-fail (c) (tc-report "~A" c))))

(defmacro ensure-pattern* (obj &rest args)
  `(ensure-pattern ,obj (pattern ,@args)))

(defmacro ensure-values (exp &rest values)
  `(ensure-pattern 
    (multiple-value-list ,exp)
    (list ,@values)))

(defmacro ensure-exception (code exception &rest pattern)
  `(handler-case
     (progn ,code
            (tc-report "~S should raise exception" ',code))
     (,exception (exc)
                 (ensure-pattern* exc ,@pattern))
     (t (exc)
        (tc-report "Should raise ~A. Got: ~A" ',exception exc))))


(defmacro define-test-suite (name &body body)
  (let ((vars nil))
    (when (and (consp (first body))
               (eq 'variables (car (first body))))
      (setq vars (cdr (pop body))))
    `(eval-when (:load-toplevel :execute)
       (let ((*test-suite-result* (make-instance 'test-result
                                    :suite ,name
                                    :parent *test-suite-result*))
             (*tc-current* nil)
             (*tc-current-sub* nil))
  
         (macrolet ((define-test (name &body body)
                      `(let* ,',vars
                         (declare (ignorable . ,',(mapcar #'car vars)))
                         (block nil
                           (handler-bind ((serious-condition
                                           (lambda (exc) 
                                             (tc-report "Exception ~A" exc)
                                             (unless *test-suite-debug*
                                               (return)))))
                             (let ((*tc-current* ,(string name))
                                   (*tc-current-sub* nil))
                               (start-test-case *test-suite-result* ,(string name))
                               ,@body
                               (end-test-case *test-suite-result*)))))))
           ,@body
           (print-result *test-suite-result*))))))


(defmacro with-sub-test ((name) &body body)
  `(let ((*tc-current-sub* ,name))
     ,@body))

(defmacro define-test (name &body body)
  `(define-test-suite "Temporary Suit"
     (variables ,@*temporary-suite-variables*)
     (define-test ,name ,@body)))

(defmacro variables (&rest bindings)
  `(setq *temporary-suite-variables* ',bindings))
