;;;; clorb-target.lisp -- Code Generator for CLORB

(in-package :clorb)



(defgeneric target-typecode (obj target)
  (:documentation
   "The target code to compute the typecode for idltype object."))

(defgeneric target-code (obj target)
  (:documentation
   "The target code defining the object."))

(defgeneric target-type (idltype target)
  (:documentation
   "Lisp type mapping for IDL-type."))



;;;; Basic Target

(defclass code-target ()
  ((packages :initform nil)
   (symbols  :initform nil)
   (excludes :initarg :excludes  :initform nil  :reader target-excludes)
   (only     :initarg :only      :initform nil  :reader target-only)
   (struct-marshal
    :initarg :struct-marshal
    :initform nil
    :reader target-struct-marshal )))


(defun param-symbol (parameterdesc)
  (intern (format nil "_~:@(~A~)" (op:name parameterdesc)) :clorb))

(defun make-target-symbol (target name package)
  (let* ((package
          (or (if (packagep package) package)
              (find-package (setq package (string-upcase package)))
              (make-package package :use '())))
         (symbol (intern (string-upcase name) package)))
    (export symbol package)
    (pushnew package (slot-value target 'packages))
    (pushnew symbol  (slot-value target 'symbols))
    symbol))


(defun scoped-symbol-name (obj)
  (scoped-subsymbol-name (op:defined_in obj) obj))

(defgeneric scoped-subsymbol-name (container obj)
  ;; values package-name, symbol-name
  ;; Top-level definitions
  (:method ((container CORBA:Repository) (obj CORBA:Contained))
           (values "OMG.ORG/ROOT" (op:name obj)))
  ;; Definitions in a Module
  (:method ((module CORBA:ModuleDef) (obj CORBA:Contained))
           (values (scoped-symbol-name module) (op:name obj)))
  ;; Nested definitions (in interface, struct, union, etc.)
  (:method ((container CORBA:Container) (obj CORBA:Contained))
           (multiple-value-bind (package name) (scoped-symbol-name container)
             (values package (concatenate-name name (op:name obj)))))
  ;; Top-level modules
  (:method ((container CORBA:Repository) (module CORBA:ModuleDef))
           (concatenate 'string (package-prefix module) (op:name module)))
  ;; Nested modules
  (:method ((container CORBA:ModuleDef) (module CORBA:ModuleDef))
           (concatenate-name (scoped-symbol-name container) (op:name module))))

(defun concatenate-name (name1 name2)
  (concatenate 'string name1 "/" name2))

(defmethod package-prefix ((obj CORBA:IRObject))
  "")


(defgeneric scoped-target-symbol (target obj)
  (:method (target (obj t))
           (multiple-value-bind (package-name symbol-name) (scoped-symbol-name obj)
             (if symbol-name
               (make-target-symbol target symbol-name package-name)
               (make-target-symbol target package-name :keyword))))
  (:method (target (obj CORBA:Repository))
           (declare (ignore target))
           nil))


(defun scoped-target-symbol-in (target name container)
  ;; used for subnames for non IDL-types, e.g. union members
  (multiple-value-bind (package-name symbol-name) (scoped-symbol-name container)
    (make-target-symbol target
                        (concatenate-name symbol-name name)
                        package-name)))


(defun make-progn (l)
  (cond ((and l (null (cdr l)))
         (car l))
        (t
         (cons 'progn
               (loop for (x . more) on (loop for x in l
                                             append (if (and (consp x) (eq 'progn (car x)))
                                                      (cdr x)
                                                      (list x)))
                     when (or x (not more))
                     collect x )))))

(defun make-progn* (&rest l)
  (make-progn l))

(defun make-target-ensure-package (package target)
  (let* ((package-name (package-name package))
         (exports (loop for sym in (slot-value target 'symbols)
                        when (string= package-name (package-name (symbol-package sym)))
                        collect (symbol-name sym))))
    `(ensure-corba-package ,package-name
                           :export ',exports)))

(defun make-always-eval (code)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,code))


(defun target-base-list (target bases make-symbol-fun root-class)
  (if (zerop (length bases))
      `(,root-class)
      (map 'list (lambda (base)
                   (funcall make-symbol-fun target base))
           bases)))

(defun target-class-symbol (target idef suffix)
  (let ((scoped-symbol (scoped-target-symbol target idef)))
    (make-target-symbol target
                        (concatenate 'string
                                     (symbol-name scoped-symbol)
                                     suffix )
                        (symbol-package scoped-symbol))))

(defun target-proxy-class-symbol (target idef)
  (target-class-symbol target idef "-PROXY"))

(defun target-servant-class-symbol (target idef)
  (target-class-symbol target idef "-SERVANT"))


(defun setter-name (name)
  (concatenate 'string "_set_" name))

(defun getter-name (name)
  (concatenate 'string "_get_" name))



;;;; Target Type Methods

(defmethod target-type ((obj CORBA:PrimitiveDef) target)
  (declare (ignore target))
  (ecase (op:kind obj)
    (:pk_short 'CORBA:short)
    (:pk_long 'CORBA:long)
    (:pk_ushort 'CORBA:ushort)
    (:pk_ulong 'CORBA:ulong)
    (:pk_float 'CORBA:float)
    (:pk_double 'CORBA:double)
    (:pk_boolean 'CORBA:boolean)
    (:pk_char 'CORBA:char)
    (:pk_octet 'CORBA:octet)
    (:pk_any 'CORBA:any)
    (:pk_TypeCode 'CORBA:TypeCode)
    (:pk_string 'CORBA:string)
    (:pk_objref 'CORBA:Object)
    (:pk_longlong 'CORBA:longlong)
    (:pk_ulonglong 'CORBA:ulonglong)
    (:pk_longdouble 'CORBA:longdouble)
    (:pk_wchar 'CORBA:wchar)
    (:pk_wstring 'CORBA:wstring)))


(defmethod target-type ((obj CORBA:FixedDef) target)
  (declare (ignore target))
  'corba:fixed)

(defmethod target-type ((obj CORBA:SequenceDef) target)
  (declare (ignore target))
  `sequence)

(defmethod target-type ((obj CORBA:ArrayDef) target)
  (declare (ignore target))
  ;;FIXME: handle multi dim arrays
  `(array t (,(op:length obj))))

(defmethod target-type ((obj CORBA:StringDef) target)
  (declare (ignore target))
  'CORBA:string)


(defmethod target-type ((obj CORBA:Contained) target)
  (scoped-target-symbol target obj))



;;;; Target-typecode Methods

(defmethod target-typecode ((obj CORBA:PrimitiveDef) target)
  (declare (ignore target))
  (ecase (op:kind obj)
   (:pk_any `CORBA:tc_any)
   (:pk_boolean `CORBA:tc_boolean)
   (:pk_char `CORBA:tc_char)
   (:pk_double `CORBA:tc_double)
   (:pk_float `CORBA:tc_float)
   (:pk_long `CORBA:tc_long)
   (:pk_longdouble `CORBA:tc_longdouble)
   (:pk_longlong `CORBA:tc_longlong)
   (:pk_objref `CORBA:tc_object)
   (:pk_octet `CORBA:tc_octet)
   (:pk_short `CORBA:tc_short)
   (:pk_string `CORBA:tc_string)
   (:pk_wstring `CORBA:tc_wstring)
   (:pk_typecode `CORBA:tc_typecode)
   (:pk_ulong `CORBA:tc_ulong)
   (:pk_ulonglong `CORBA:tc_ulonglong)
   (:pk_ushort `CORBA:tc_ushort)
   (:pk_wchar `CORBA:tc_wchar)
   (:pk_void `CORBA:tc_void)
   (:pk_value_base `CORBA:tc_valuebase)))


(defmethod target-typecode ((obj CORBA:FixedDef) target)
  (declare (ignore target))
  `(create-fixed-tc ,(op:digits obj) ,(op:scale obj)))


(defmethod target-typecode ((x CORBA:StringDef) target)
  (declare (ignore target))
  (if (zerop (op:bound x))
     `CORBA:tc_string
     `(create-string-tc  ,(op:bound x))))

(defmethod target-typecode ((x CORBA:SequenceDef) target)
  `(create-sequence-tc
    ,(op:bound x)
    ,(target-typecode (op:element_type_def x) target)))

(defmethod target-typecode ((x CORBA:Contained) target)
  `(symbol-typecode ',(scoped-target-symbol target x)))

(defmethod target-typecode ((x CORBA:ExceptionDef) target)
  `(symbol-typecode ',(scoped-target-symbol target x)))

(defmethod target-typecode ((x CORBA:ArrayDef) target)
  `(create-array-tc ,(op:length x)
                    ,(target-typecode (op:element_type_def x) target)))



;;;; target-code methods

(defmethod target-code ((x CORBA:IRObject) target)
  (declare (ignore target))
  '(progn))


(defmethod target-code :around ((mdef CORBA:Container) target)
  (make-progn
   (cons (call-next-method)
         (map 'list (lambda (contained)
                      (if (and (not (member contained (target-excludes target)))
                               (or (null (target-only target))
                                   (member contained (target-only target))))
                        (target-code contained target)))
              (sort (op:contents mdef :dk_All t)
                    #'<  :key #'target-sort-key )))))

(defmethod target-sort-key ((def CORBA:InterfaceDef))
  (1+ (reduce #'max (map 'list #'target-sort-key (op:base_interfaces def))
              :initial-value 0)))

(defmethod target-sort-key ((x t)) 0)



;; In some cases code depends on the container. I.e. an operation in a
;; ValueType is different from an operation in an Interface

(defgeneric target-code-contained (container def target))

(defmethod target-code-contained ((c t) (op t) (target code-target))
  nil)



;;;; IFR info target

(defclass ifr-info-target (code-target)
  ())

(defmethod target-code-contained ((c CORBA:InterfaceDef) (op CORBA:OperationDef) (target ifr-info-target))
  (let ((desc (any-value (op:value (op:describe op)))))
    (assert (typep desc 'corba:operationdescription))
    (make-progn*
     (call-next-method)
     `(define-operation ,(scoped-target-symbol target op)
        :id ,(op:id desc)
        :name ,(op:name desc)
        :defined_in ,(scoped-target-symbol target (op:defined_in op))
        :version ,(op:version desc)
        :result ,(target-typecode (op:result_def op) target)
        :mode ,(op:mode desc)
        :contexts ,(op:contexts desc)
        :parameters ,(map 'list (lambda (param)
                                  (list (op:name param)
                                        (op:mode param)
                                        (target-typecode (op:type_def param) target)))
                          (op:parameters desc))
        :exceptions ,(map 'list (lambda (exc) (scoped-target-symbol target exc))
                          (op:exceptions op))))))


(defmethod target-code-contained ((c CORBA:InterfaceDef) (attr CORBA:AttributeDef) (target ifr-info-target))
  (let ((desc (corba:any-value (op:value (op:describe attr)))))
    (make-progn*
     (call-next-method)
     `(define-attribute ,(scoped-target-symbol target attr)
        :id ,(op:id desc)
        :name ,(op:name desc)
        :version ,(op:version desc)
        :defined_in ,(scoped-target-symbol target (op:defined_in attr))
        :mode ,(op:mode desc)
        :type ,(target-typecode (op:type_def attr) target)))))



;;;; Stub Target

(defclass stub-target (ifr-info-target code-target)
  ())


(defclass static-stub-target (stub-target)
  ())



(defmethod target-code ((x CORBA:Contained) (target stub-target))
  (declare (ignorable target)) (call-next-method))


(defmethod target-code ((x CORBA:AliasDef) (target stub-target))
  (let ((symbol (scoped-target-symbol target x)))
    `(define-alias ,symbol
       :id ,(op:id x)
       :name ,(op:name x)
       :type ,(target-type (op:original_type_def x) target)
       :typecode ,(target-typecode (op:original_type_def x) target) )))

(defun enum-constant-p (const)
  "True if the constant definition is for a generated constant for enum members."
  (and (eq :tk_enum (op:kind (op:type const)))
       (eq (op:defined_in const)
           (op:defined_in (op:type_def const)))
       (string-equal (any-value (op:value const))
                     (op:name const))))

(defmethod target-code ((const CORBA:ConstantDef) (target stub-target))
  (unless (enum-constant-p const)
    `(defconstant ,(scoped-target-symbol target const)
       ',(any-value (op:value const)))))


(defmethod target-code ((idef CORBA:InterfaceDef) (target stub-target))
  (make-progn*
   (let ((bases (op:base_interfaces idef))
         (class-symbol (scoped-target-symbol target idef))
         (proxy-symbol (target-proxy-class-symbol target idef)))
     `(define-interface ,class-symbol
          ,(target-base-list target bases #'scoped-target-symbol 'CORBA:Object)
        :proxy ,(list* proxy-symbol class-symbol
                       (target-base-list target bases #'target-proxy-class-symbol
                                         'CORBA:Proxy))
        :id ,(op:id idef)
        :name ,(op:name idef)
        :version ,(op:version idef)
        :defined_in ,(scoped-target-symbol target (op:defined_in idef))))
   (call-next-method)))


(defun in-param-list (params)
  (loop for p in params
        unless (eq (op:mode p) :param_out)
        collect (param-symbol p)))


(defmethod target-code ((op CORBA:OperationDef) (target stub-target))
  (target-code-contained (op:defined_in op) op target))

(defmethod target-code ((op CORBA:AttributeDef) (target stub-target))
  (target-code-contained (op:defined_in op) op target))


(defmethod target-code-contained ((c CORBA:InterfaceDef) (attr CORBA:AttributeDef) (target static-stub-target))
  (let* ((att-name (op:name attr))
         (lisp-name (string-upcase att-name))
         (class (target-proxy-class-symbol target (op:defined_in attr)))
         (symbol (scoped-target-symbol target attr)))
    (make-progn*
     (call-next-method)
     (if (eq (op:mode attr) :attr_normal)
       `(define-method (setf ,lisp-name) (newval (obj ,class))
          (%jit-set ,symbol obj newval)))
     `(define-method ,lisp-name ((obj ,class))
        (%jit-get ,symbol obj)))))


(defmethod target-code-contained ((c CORBA:InterfaceDef) (opdef CORBA:OperationDef) (target static-stub-target))
  (let ((name-string (string-upcase (op:name opdef)) )
        (in-params (loop for p in (coerce (op:params opdef) 'list)
                         unless (eq :param_out (op:mode p))
                         collect (param-symbol p)))
        (class-name (target-proxy-class-symbol target c))
        (op-name    (scoped-target-symbol target opdef)))
    (make-progn*
     (call-next-method)
     `(define-method ,name-string ((obj ,class-name) ,@in-params)
        (%jit-call ,op-name obj ,@in-params)))))


(defmethod target-code ((sdef CORBA:StructDef) (target stub-target))
  (let ((sym (scoped-target-symbol target sdef)))
    `(define-struct ,sym
       :id ,(op:id sdef)
       :name ,(op:name sdef)
       :members ,(map 'list
                      (lambda (smember)
                        (list (op:name smember)
                              (target-typecode (op:type_def smember) target)))
                      (op:members sdef))
       ,@(if (target-struct-marshal target)
           `(
             :read ((buffer)
                    (,sym
                     ,@(loop for member in (coerce (op:members sdef) 'list)
                             collect (key (op:name member))
                             collect (target-unmarshal (op:type_def member) target 'buffer))))
             :write ((obj buffer)
                     ,@(loop for member in (coerce (op:members sdef) 'list)
                             collect (target-marshal (op:type_def member) target
                                                     `(,(feature (op:name member)) obj)
                                                     'buffer))))))))


(defmethod target-code ((enum CORBA:EnumDef) (target stub-target))
  `(define-enum ,(scoped-target-symbol target enum)
     :id ,(op:id enum)
     :name ,(op:name enum)
     :members ,(coerce (op:members enum) 'list)))


(defmethod target-code ((exc CORBA:ExceptionDef) (target stub-target))
  (let ((desc (any-value (op:value (op:describe exc)))))
    `(define-user-exception ,(scoped-target-symbol target exc)
       :id ,(op:id desc)
       :name ,(op:name desc)
       :version ,(op:version desc)
       :defined_in ,(scoped-target-symbol target (op:defined_in exc))
       :members ,(map 'list
                      (lambda (smember)
                        (list (op:name smember)
                              (target-typecode (op:type_def smember) target)))
                      (op:members exc)))))


(defmethod target-code ((def CORBA:UnionDef) (target stub-target))
  (let ((collected-members '())
        (default-member nil)
        (used-labels '())
        (default-index (op:default_index (op:type def))))
    (doseq (m (op:members def))
      (let* ((name (op:name m))
             (raw-label (op:label m))
             (label (any-value raw-label))
             (defaultp (zerop default-index)))
        (push (list label (target-typecode (op:type_def m) target)
                    :name name :default defaultp
                    :creator (scoped-target-symbol-in target name def))
              collected-members)
        (cond (defaultp
                (setq default-member collected-members))
              (t
               (push label used-labels))))
      (decf default-index))
    (when default-member
      (setf (caar default-member)
            (block nil
              (typecode-values-do (lambda (x) (unless (member x used-labels) (return x)))
                                  (op:discriminator_type def)))))
    `(define-union ,(scoped-target-symbol target def)
       :id   ,(op:id def)
       :name ,(op:name def)
       :discriminator-type ,(target-typecode (op:discriminator_type_def def) target)
       :members ,(nreverse collected-members))))


(defmethod target-code ((def CORBA:ValueDef) (target stub-target))
  (flet ((scoped-symbol (obj)
           (scoped-target-symbol target obj)))
    (make-progn*
     `(define-value ,(scoped-symbol def)
        :id ,(op:id def)
        :name ,(op:name def)
        ,@(let ((base (op:base_value def)))
            (if base
              `(:base_value ,(scoped-symbol base))))
        :is_abstract ,(op:is_abstract def)
        :is_custom ,(op:is_custom def)
        :is_truncatable ,(op:is_truncatable def)
        :supported_interfaces
        ,(map 'list #'scoped-symbol (op:supported_interfaces def))
        :abstract_base_values
        ,(map 'list #'scoped-symbol (op:abstract_base_values def))
        ,@(if (not (op:is_abstract def))
            `(:members
              ,(map 'list
                    (lambda (statedef)
                      `(,(op:name statedef)
                         ,(target-typecode (op:type_def statedef) target)
                         ,(op:access statedef)))
                    (op:contents def :dk_valuemember t)))))
     (call-next-method))))


(defmethod target-code ((def corba:valuememberdef) (target stub-target))
  nil)

(defmethod target-code ((def corba:valueboxdef) (target stub-target))
  (let* ((original-type (op:original_type_def def)))
    `(define-value-box ,(scoped-target-symbol target def)
         :id ,(op:id def)
         :name ,(op:name def)
         :version ,(op:version def)
         :original_type ,(target-typecode original-type target))))


(defmethod target-code ((def corba:abstractinterfacedef) (target stub-target))
  (make-progn*
    (let ((bases (op:base_interfaces def))
          (class-symbol (scoped-target-symbol target def))
          (proxy-symbol (target-proxy-class-symbol target def)))
      `(define-abstract-interface ,class-symbol
         ,(target-base-list target bases #'scoped-target-symbol 'corba:abstractbase)
         :proxy ,(list* proxy-symbol (target-base-list target bases #'target-proxy-class-symbol
                                                       'CORBA:Proxy))
         :mixin ,(target-class-symbol target def "-MIXIN")
         :id ,(op:id def)
         :name ,(op:name def)))
    (call-next-method)))


;;;; Marshalling

;;; Marshal

(defmethod target-marshal ((def CORBA:IRObject) target obj buffer)
  `(%jit-marshal ,obj ,(target-typecode def target) ,buffer))

(defmethod target-marshal ((def CORBA:Contained) target obj buffer)
  `(%jit-marshal ,obj (symbol-typecode ',(scoped-target-symbol target def)) ,buffer))

(defmethod target-marshal ((def corba:primitivedef) target obj buffer)
  (declare (ignore target))
  (let ((func (case (op:kind def)
                (:pk_string 'marshal-string)
                (:pk_long   'marshal-long)
                (:pk_ulong  'marshal-ulong)
                (:pk_short  'marshal-short)
                (:pk_ushort 'marshal-ushort)
                (:pk_octet  'marshal-octet)
                (:pk_boolean 'marshal-bool)
                (:pk_objref 'marshal-object)
                (:pk_char 'marshal-char)
                (:pk_any 'marshal-any)
                (:pk_float 'marshal-float)
                (:pk_typecode 'marshal-typecode))))
    (if func
      `(,func ,obj ,buffer)
      (call-next-method))))

(defmethod target-marshal ((def corba:interfacedef) target obj buffer)
  (declare (ignore target))
  `(marshal-object ,obj ,buffer))


;;; Unmarshal

(defmethod target-unmarshal ((def CORBA:IRObject) target buffer)
  `(%jit-unmarshal ,(target-typecode def target) ,buffer))

(defmethod target-unmarshal ((def corba:primitivedef) target buffer)
  (declare (ignore target))
  (let ((func (case (op:kind def)
                (:PK_STRING 'unmarshal-string)
                (:pk_long   'unmarshal-long)
                (:pk_ulong  'unmarshal-ulong)
                (:pk_short  'unmarshal-short)
                (:pk_ushort 'unmarshal-ushort)
                (:pk_octet  'unmarshal-octet)
                (:pk_boolean 'unmarshal-bool)
                (:pk_any    'unmarshal-any)
                (:pk_char   'unmarshal-char)
                (:pk_float  'unmarshal-float)
                (:pk_typecode 'unmarshal-typecode))))
    (if func
      `(,func ,buffer)
      (call-next-method))))

(defmethod target-unmarshal ((alias CORBA:AliasDef) target buffer)
  (target-unmarshal (op:original_type_def alias) target buffer))

(defmethod target-unmarshal ((def corba:interfacedef) target buffer)
  (declare (ignore target))
  `(unmarshal-object ,buffer ,(op:id def)))


;;;; Servants


(defclass servant-target (code-target)
  ())


(defmethod target-code ((idef CORBA:InterfaceDef) (target servant-target))
  (let ((bases (op:base_interfaces idef))
        (class-symbol (target-servant-class-symbol target idef)))
    (make-progn*
     (call-next-method)
     `(define-corba-class ,class-symbol
        ,(list* (scoped-target-symbol target idef)
                (target-base-list target bases #'target-servant-class-symbol
                                  'PortableServer:Servant))
        :attributes ,(servant-attribute-declaration idef)))))


(defun servant-attribute-declaration (idef)
  (map 'list
       (lambda (attdef)
         (list* (feature (op:name attdef))
                (if (eq (op:mode attdef) :attr_readonly)
                  '(:readonly) )))
       (op:contents idef :dk_attribute t)))


(defmethod target-code ((op CORBA:OperationDef) (target servant-target))
    (target-code-contained (op:defined_in op) op target))

(defmethod target-code-contained ((c CORBA:InterfaceDef) (op CORBA:OperationDef) (target servant-target))
  (let ((class (target-servant-class-symbol target (op:defined_in op)))
        (feature (feature (op:name op))))
    (make-progn*
     (call-next-method)
     `(defmethod ,feature ((self ,class) &rest -args-)
        (declare (ignore -args-))
        (raise-system-exception 'CORBA:NO_IMPLEMENT)))))

#+ignore
(defmethod target-code ((att CORBA:AttributeDef) (target servant-target))
  (let ((class (target-servant-class-symbol target (op:defined_in att)))
        (feature (feature (op:name att))))
    (make-progn*
     (call-next-method)
     (if (eql :attr_normal (op:mode att))
       `(defmethod (setf ,feature) (val (self ,class))
          (declare (ignore val))
          (raise-system-exception 'CORBA:NO_IMPLEMENT)))
     `(defmethod ,feature ((self ,class) &rest -args-)
        (declare (ignore -args-))
        (raise-system-exception 'CORBA:NO_IMPLEMENT)))))




;;;; Local Interfaces


(defmethod target-code ((def corba:localinterfacedef) target)
  `(defclass ,(scoped-target-symbol target def)
     ,(let ((bases (op:base_interfaces def)))
        (unless (zerop (length bases))
          (target-base-list target bases #'scoped-target-symbol nil)))
     ()))

(defmethod target-code-contained ((container corba:localinterfacedef)
                                  (obj corba:operationdef)
                                  (target stub-target))
  (let ((param-list (in-param-list (op:params obj))))
  `(define-method ,(string-upcase (op:name obj))
                  ((self ,(scoped-target-symbol target container)) ,@param-list)
     (declare (ignore ,@param-list))
     (raise-system-exception 'CORBA:no_implement))))

(defmethod target-code-contained ((container corba:localinterfacedef)
                                  (obj corba:attributedef)
                                  (target stub-target))
  (let* ((class (scoped-target-symbol target container))
         (name (string-upcase (op:name obj)))
         (reader `(define-method ,name ((self ,class)) (raise-system-exception 'CORBA:no_implement))))
    (cond ((eql (op:mode obj) :attr_readonly)
           reader)
          (t
           `(progn ,reader
                   (define-method (setf ,name) (newval (self ,class))
                     (declare (ignore newval))
                     (raise-system-exception 'CORBA:no_implement)))))))

(defmethod target-code-contained ((container corba:localinterfacedef)
                                  (obj corba:idltype)
                                  (target stub-target))
  (target-code obj target))

(defmethod target-code-contained ((container corba:localinterfacedef)
                                  (obj corba:exceptiondef)
                                  (target stub-target))
  (target-code obj target))



;;;; Stub code generator

(defclass all-target (servant-target static-stub-target)
  ())


(defparameter *stub-code-ignored-packages*
  (list (find-package :keyword)
        (find-package :clorb)
        (find-package :op)))

(defmethod target-ensure-packages ((target code-target))
  (make-always-eval
   (make-progn
    (loop for package in (slot-value target 'packages)
          unless (member package *stub-code-ignored-packages*)
          collect (make-target-ensure-package package target)))))



;; ----------------------------------------------------------------------
;;;; Configure the pretty printer
;; ----------------------------------------------------------------------

(defun pprint-def-and-keys (*standard-output* list)
  (pprint-logical-block (nil list :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-exit-if-list-exhausted)
    (write (pprint-pop))
    (pprint-indent :block 1)
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (pprint-newline :linear)
      (write (pprint-pop))
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (write (pprint-pop)))))

(defun pprint-apply-keys (*standard-output* list)
  (pprint-logical-block (nil list :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (pprint-newline :linear)
      (write (pprint-pop))
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (write (pprint-pop)))))

(defun pprint-def2-and-keys (*standard-output* list)
  (pprint-logical-block (nil list :prefix "(" :suffix ")")
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-exit-if-list-exhausted)
    (write (pprint-pop))
    (write-char #\Space)
    (pprint-exit-if-list-exhausted)
    (write (pprint-pop))
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (pprint-newline :linear)
      (write (pprint-pop))
      (pprint-exit-if-list-exhausted)
      (write-char #\Space)
      (write (pprint-pop)))))


(defun struct-name-p (x)
  (and (symbolp x) (find-class x nil)
       (subtypep x 'CORBA:Struct)))


(defvar *target-pprint-dispatch*
  (copy-pprint-dispatch))

(let ((*print-pprint-dispatch* *target-pprint-dispatch*))

  (set-pprint-dispatch '(cons (member define-method))
                       (pprint-dispatch '(defmethod foo ()) ))

  (set-pprint-dispatch '(cons (member define-user-exception define-struct
                                      define-union define-enum define-alias
                                      define-value define-operation define-attribute
                                      define-value-box))
                       #'pprint-def-and-keys)

  (set-pprint-dispatch '(cons (satisfies struct-name-p))
                       #'pprint-apply-keys)

  (set-pprint-dispatch '(cons (member define-interface define-abstract-interface))
                       #'pprint-def2-and-keys))



;;; clorb-target.lisp ends here
