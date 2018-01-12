(in-package :clorb)

(defvar *idef-write-container* nil
  "The innermost container beeing written by idef-write.
This guides the use of local or absolute names.")

(defvar *idef-write-default-prefix* nil)



(defmethod gen-iref ((pdef corba:primitivedef))
  (ecase (op:kind pdef)
    (:pk_void 'void)
    (:pk_short 'short)
    (:pk_long 'long)
    (:pk_ushort 'ushort)
    (:pk_ulong 'ulong)
    (:pk_float 'float)
    (:pk_double 'double)
    (:pk_boolean 'boolean)
    (:pk_char 'char)
    (:pk_octet 'octet)
    (:pk_any 'any)
    (:pk_typecode 'TypeCode)
    (:pk_string 'string)
    (:pk_objref 'object)
    (:pk_longlong 'longlong)
    (:pk_ulonglong 'ulonglong)
    (:pk_longdouble 'longdouble)
    (:pk_wchar 'wchar)
    (:pk_wstring 'wstring)))

(defmethod gen-iref ((idef corba:contained))
  (if (eq (op:defined_in idef) *idef-write-container*)
      (op:name idef)
    (op:absolute_name idef)))


(defmethod gen-iref ((seq corba:sequencedef))
  `(sequence ,(gen-iref (op:element_type_def seq))
             ,(op:bound seq)))

(defmethod gen-iref ((seq corba:arraydef))
  `(array ,(gen-iref (op:element_type_def seq))
          ,(op:length seq)))

(defmethod default-repoid ((obj corba:contained) &optional prefix)
  (let ((names (list ":" (op:version obj))))
    (loop for c = obj then (op:defined_in c)
        while (typep c 'CORBA:Contained)
        unless (eq c obj) do (push "/" names)
        do (push (op:name c) names))
    (when prefix
      (push "/" names)
      (push prefix names))
    (apply 'concatenate 'string
           "IDL:"
           names)))

(defun contained-id-info (obj)
  (append
   (unless (equal (op:version obj) "1.0")
     (list :version (op:version obj)))
   (unless (equal (op:id obj) (default-repoid obj *idef-write-default-prefix*))
     (list :id (op:id obj)))))


(defmethod gen-idef ((tdef corba:aliasdef))
  `(define-type ,(op:name tdef)
       ,(gen-iref (op:original_type_def tdef))))


;;    (define-attribute "name" string :readonly t)
(defmethod gen-idef ((adef corba:attributedef))
  `(define-attribute ,(op:name adef)
       ,(gen-iref (op:type_def adef))
     ,@(if (eq (op:mode adef) :attr_readonly)
           '(:readonly t))
     ,@ (contained-id-info adef)))


(defmethod gen-idef ((odef corba:operationdef))
  `(define-operation ,(op:name odef)
       ,(map 'list (lambda (param)
                     (list (op:mode param)
                           (op:name param)
                           (gen-iref (op:type_def param))))
             (op:params odef))
     :result-type ,(gen-iref (op:result_def odef))
     :exceptions ,(map 'list 'gen-iref (op:exceptions odef))
     ,@ (contained-id-info odef)))



(defmethod gen-idef ((idef corba:interfacedef))
  (let ((*idef-write-container* idef))
    `(define-interface ,(op:name idef) 
         (
          ,@(unless (zerop (length (op:base_interfaces idef)))
              (list :bases 
                    (map 'list 'op:absolute_name (op:base_interfaces idef))))
          ,@(contained-id-info idef))
       ,@(map 'list 'gen-idef (op:contents idef :dk_all t)))))

(defmethod gen-idef ((mdef corba:moduledef))
  (let ((*idef-write-container* mdef))
    `(define-module ,(op:name mdef) 
         (,@(contained-id-info mdef))
       ,@(map 'list 'gen-idef (op:contents mdef :dk_all t)))))

(defmethod gen-idef ((sdef corba:structdef))
  `(define-struct ,(op:name sdef) 
       ,(map 'list 
          (lambda (smember)
            (list (struct-get smember :name)
                  (gen-iref (op:type_def smember))))
          (op:members sdef))
     ,@(contained-id-info sdef)))

(defmethod gen-idef ((enum corba:enumdef))
  `(define-enum ,(op:name enum) 
       ,(coerce (op:members enum) 'list)
     ,@(contained-id-info enum)))

(defmethod gen-idef ((union corba:uniondef))
  `(define-union ,(op:name union) 
       ,(gen-iref (op:discriminator_type_def union))
     ,(map 'list  
        (lambda (m)
          (list (let ((label (struct-get m :label)))
                  (if (and (eq :tk_octet (op:kind (any-typecode label)))
                           (zerop (any-value label)))
                    'default
                    (any-value label)))
                (struct-get m :name)
                (gen-iref (op:type_def m))))
        (op:members union))
     ,@(contained-id-info union)))

(defmethod gen-idef ((exception corba:exceptiondef))
  `(define-exception ,(op:name exception) 
       ,(map 'list  
          (lambda (smember)
            (list (struct-get smember :name)
                  (gen-iref (op:type_def smember))))
          (op:members exception))
     ,@(contained-id-info exception)))

(defmethod gen-idef ((const corba:constantdef))
  `(define-constant ,(op:name const) ,(gen-iref (op:type_def const))
     ,(any-value (op:value const))
     ,@(contained-id-info const)))

(defun idef-write (obj &key default-prefix)
  (let ((*idef-write-default-prefix* default-prefix))
    (let ((idef 
           (typecase obj
             (CORBA:Repository
              (map 'list 'gen-idef (op:contents obj :dk_all t)))
             (cons
              (map 'list 'gen-idef obj))
             (otherwise 
              (list (gen-idef obj))))))
      (if default-prefix
          `((with-prefix ,default-prefix
             ,@idef))
        idef))))


(defun idef-file (obj file &key default-prefix)
  (let* ((idef (idef-write obj :default-prefix default-prefix)))
    (with-open-file (output file :direction :output
                            :if-exists :supersede)
      (loop for x in (list '(in-package :clorb) 
                           (cons 'idef-definitions idef))
            do (pprint x output)))
    (length idef)))

#|
(idef-file (op:lookup (vsns-get "ir") "CORBA") "clorb:src;ifr-idl.lisp" :default-prefix "omg.org")
(idef-file (get-ir) "clorb:src;x-all-idl.lisp")
(idef-file (op:lookup (get-ir) "CosNaming") "clorb:src;x-cosnaming-idl.lisp" :default-prefix "omg.org")
|#