;;;; clorb-macros2.lisp
;; Macros for defining CORBA types, implementing the Lisp mapping.
;; defina-alias
;; define-enum
;; define-struct
;; define-union
;; define-user-exception
;; define-interface
;;  define-attribute
;;  define-operation

(in-package :clorb)



;;;; Alias

#|
(DEFINE-ALIAS OMG.ORG/CORBA:REPOSITORYID
 :ID "IDL:omg.org/CORBA/RepositoryId:1.0"
 :NAME "RepositoryId"
 :TYPE OMG.ORG/CORBA:STRING
 :TYPECODE OMG.ORG/CORBA:TC_STRING)
|#

(defmacro define-alias (symbol &key id name type typecode
                        (tc-constant (tc-constant-name symbol)))
  `(progn (deftype ,symbol () ',type)
          (set-symbol-id/typecode ',symbol ,id
                                  (create-alias-tc ,id ,name ,typecode))
          (add-defining-repository ',symbol)
          (defparameter ,tc-constant (symbol-typecode ',symbol))))


;;;; Enum

;; enum AttributeMode { ATTR_NORMAL, ATTR_READONLY };
;; =>
;; (DEFINE-ENUM OMG.ORG/CORBA:ATTRIBUTEMODE
;;   :ID "IDL:omg.org/CORBA/AttributeMode:1.0"
;;   :NAME "AttributeMode"
;;   :MEMBERS ("ATTR_NORMAL" "ATTR_READONLY"))


(defmacro define-enum (symbol &key id name members
                       (tc-constant (tc-constant-name symbol)))
  (let ((keys (mapcar #'key members)))
    `(progn (deftype ,symbol () '(member ,@keys))
            (set-symbol-id/typecode ',symbol ,id
                                    (create-enum-tc ,id ,name
                                                   ',(coerce members 'vector)))
            (add-defining-repository ',symbol)
            (defparameter ,tc-constant (symbol-typecode ',symbol)))))


;;;; Struct Macrology


(defmacro define-struct (symbol &key id (name "") members read write
                         (tc-constant (tc-constant-name symbol)))
  "Define a CORBA structure with class, constructor, typecode etc.
  members = ((name type)*)"
  ;; OLD:
  ;;read = ((buffer) unmarshallingcode*)
  ;;write = ((obj buffer) marshallingcode*)
  (declare (ignore read write))
  (let* ((slot-names (mapcar #'first members))
         (slots (mapcar #'feature slot-names))
         (keys  (mapcar #'key slot-names)))
    `(progn
       (defclass ,symbol (corba:struct)
         ,(mapcar (lambda (slot key) (list slot :initarg key))
                  slots keys))
       (defun ,symbol (&key ,@slots)
         (make-instance ',symbol ,@(mapcan #'list keys slots)))
       ,@(mapcar (lambda (slot)
                   `(define-method ,slot ((s ,symbol)) (slot-value s ',slot)))
                 slots)
       ,@(mapcar (lambda (slot)
                   `(define-method (setf ,slot) (val (s ,symbol))
                      (setf (slot-value s ',slot) val)))
                 slots)
       (defmethod fields ((s ,symbol))
         (loop for f in ',keys for n in ',slots
            when (slot-boundp s n)
            collect (cons f (slot-value s n))))
       (set-symbol-id/typecode
        ',symbol ,id
        (create-struct-tc ,id ,name
                          (vector ,@(loop for (name type) in members
                                       collect `(list ,name ,type)))))
       (add-defining-repository ',symbol)
       (defparameter ,tc-constant (symbol-typecode ',symbol)))))



;;;; Union macrology

(defmacro define-union (symbol &key id (name "") discriminator-type members
                        (tc-constant (tc-constant-name symbol)))
  "members = (label type &key creator name default)*
     where labels = () is default member"
  (let ((used-names '())
        (code '())
        (tc-members '()) )
    (dolist (m members)
      (destructuring-bind (label type &key creator (name "") default
                                 (accessor (string-upcase name))) m
        (push `(list ,(if default ''default label) ,name ,type)
              tc-members)
        (unless (member name used-names :test #'equal)
          (push name used-names)
          (push `(progn
                   (define-method ,accessor ((obj ,symbol))
                     (union-value obj))
                   (define-method (setf ,accessor) (value (obj ,symbol))
                     (setf (union-discriminator obj) ,label)
                     (setf (union-value obj) value))
                   ,(if creator
                      `(defun ,creator (value)
                         (,symbol :union-value value :union-discriminator ,label))))
                code)
          (when default
            (push `(progn
                     (define-method default ((obj ,symbol)) (union-value obj))
                     (define-method (setf default) (value (obj ,symbol))
                       (setf (union-discriminator obj) ,label)
                       (setf (union-value obj) value))) code)))))

    `(progn
       (defclass ,SYMBOL (corba:union) ())
       (defun ,symbol (&key union-value union-discriminator)
         (make-instance ',symbol
           :value union-value
           :discriminator union-discriminator))
       (set-symbol-id/typecode ',symbol ,id
                               (create-union-tc ,id ,name
                                                ,discriminator-type
                                                (list ,@(nreverse tc-members))))
       (add-defining-repository ',symbol)
       (defparameter ,tc-constant (symbol-typecode ',symbol))
       ,@code)))


#|
(define-union omg.org/root::filter :name "filter" :id "idl:filter.1.0"
  :discriminator-type corba:tc_long
  :members ((0 corba:tc_string :name "foo" :creator filter/foo))
)
|#


;;;; Exceptions

(defmacro define-user-exception (symbol &key (name "") (id "") (version "1.0")
                                 defined_in members
                                 (tc-constant (tc-constant-name symbol)))
  "Syntax: scoped-symbol :id repo-id :name repo-name :members (name typecode)*
              :version version :defined_in parent-repo-id"
  (loop
     for member in members
     for slot-name = (string (car member))
     for initarg = (key slot-name)
     for slot = (feature slot-name)
     for getter-name = (intern (format nil "_~A" slot-name) :clorb)
     collect (list slot :initarg initarg :reader getter-name) into slot-defs
     collect `(define-method ,slot ((s ,symbol)) (,getter-name s))
     into getters
     collect `(list ,slot-name ,(second member)) into tc-members
     collect slot into args
     append (list initarg slot) into initargs
     finally
     (return
       `(progn
          (define-condition ,symbol (CORBA:UserException)
            (,@slot-defs))
          ,@getters
          (defun ,symbol (&key ,@args)
            (make-condition ',symbol ,@initargs))
          (defmethod exception-name ((exc ,symbol)) ',symbol)
          (set-ifr-info ',symbol
                        :id ,id
                        :typecode (create-exception-tc ,id ,name (vector ,@tc-members))
                        :version ,version
                        :defined_in ',defined_in)
          (defparameter ,tc-constant (symbol-typecode ',symbol))))))



;;;; Stub generation

(defmacro static-call ((op obj) &key output input exceptions no-response)
  (let ((req '#:REQ)
        (output-buf (or (caar output) '#:output-buf))
        (input-buf (or (caar input) '#:input-buf)))
    `(do-static-call
      ,obj ,op ,(if no-response `(not ,no-response) t)
      (lambda (,req ,output-buf)
        (declare (ignorable ,output-buf ,req))
        ,@(cdr output))
      (lambda (,req ,input-buf)
        (declare (ignorable ,input-buf ,req))
        (values ,@(cdr input)))
      (load-time-value
       (mapcar #'symbol-typecode ',exceptions)))))


(defmacro %jit-call (sym obj &rest args)
  `(funcall (the function (load-time-value (compute-static-call ',sym)))
            ,obj ,@args))

(defmacro %jit-get (sym obj)
  `(funcall (the function (load-time-value (compute-static-get ',sym)))
            ,obj))

(defmacro %jit-set (sym obj value)
  `(funcall (the function (load-time-value (compute-static-set ',sym)))
            ,obj ,value))



;;;; Interface

(defmacro define-interface (symbol super &key (id "") proxy (name "")
                            defined_in version
                            (tc-constant (tc-constant-name symbol)))
  `(progn
     (set-ifr-info ',symbol :id ,id :typecode (create-interface-tc ,id ,name)
                   :bases ',super :defined_in ',defined_in :version ,version)
     (defparameter ,tc-constant (symbol-typecode ',symbol))
     (defclass ,symbol ,super ())
     ,@(if proxy
         `((defclass ,(CAR PROXY) ,(cdr proxy) ())
           (register-proxy-class ,id ',(car proxy))))
     (defmethod interface-name ((obj ,symbol))
       ',symbol)
     (defmethod object-id ((obj ,symbol))
       ,id)
     (defmethod object-is-a or ((obj ,symbol) interface-id)
       (string= interface-id ,id))))


(defmacro define-operation (symbol &key id name defined_in (version "1.0")
                                    (result 'CORBA:tc_void) (mode :op_normal)
                                    parameters exceptions contexts)
  (declare (ignore contexts))
  `(set-ifr-info ',symbol
                 :id ,id :name ,name :defined_in ',defined_in :version ,version
                 :result ,result :mode ,mode
                 :parameters (list ,@(mapcar #'cons (repeated 'list) parameters))
                 :exceptions ',exceptions))


(defmacro define-attribute (symbol &key id name defined_in (version "1.0")
                            (mode :attr_normal) type)
  `(set-ifr-info ',symbol
                 :id ,id :name ,name :defined_in ',defined_in :version ,version
                 :mode ,mode :type ,type))
