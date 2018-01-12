;;;; clorb-ifr.lisp -- An Interface Repository Implementation

(in-package :clorb)


;;;; Registry of Ir class for DefinitionKind

(defun register-ir-class (def-kind class)
  (setf (get def-kind 'ir-class) class))



;;;; Generics

(defgeneric moveto (contained container new-name new-version)
  (:documentation "Move the contained object to the container,
with new-name and new-version. Checks that the name does not collide 
with the contents of container and check the container allows the type 
of contained."))


;;;; Base Clases

(defmacro define-ir-class (name supers
                           &rest args
                           &key id def_kind &allow-other-keys)
  (declare (ignore id))
  (setq args
    (loop for x on args by #'cddr
        unless (member (car x) '(:id :def_kind))
        nconc (list (car x) (cadr x))))
  `(progn (define-corba-class ,name ,supers ,@args)
          ,@(if def_kind
                `((define-method def_kind ((x ,name)) ,def_kind)
                  (register-ir-class ,def_kind ',name)))))


(define-ir-class IRObject (CORBA:IRObject)
  :id "IDL:omg.org/CORBA/IRObject:1.0"
  :attributes ()
  :defaults ())


(define-method op:destroy ((obj IRObject))
  nil)


(defclass IRTypeCode-Mixin ()
  ((type)))

(defgeneric idltype-tc (irtypecode-mixin)
  (:documentation
   "Compute the TypeCode from the attributes other than type."))

(define-method type ((def irtypecode-mixin))
  ;; Get the typecode for an IDL construct.
  ;; The typecode is lazily constructed and recursive typecodes handled.
  ;; The specific typcodes are handled by the idltype-tc function.
  (cond ((slot-boundp def 'type)
         ;; Either the typecode is computed already or this is a recursive call
         ;; during computation.
         (let ((tc (slot-value def 'type)))
           (cond ((eq t tc)
                  ;; Beeing computed, make a placeholder typecode.
                  ;; This typecode will be filled when the computation is done.
                  (setf (slot-value def 'type) (make-typecode :recursive)))
                 (t
                  tc))))
        (t
         ;; Typecode not defined, start computation
         ;; mark computation with a t in the slot
         (setf (slot-value def 'type) t)
         (let* ((new-tc (idltype-tc def))
                (old-tc (slot-value def 'type)))
           (cond ((eq old-tc t)
                  (setf (slot-value def 'type) new-tc))
                 (t
                  ;; therse was a recurive access during computation
                  (typecode-smash old-tc new-tc)
                  old-tc))))))


(defmethod slot-updated :after ((obj irtypecode-mixin))
  (slot-makunbound obj 'type))


(define-ir-class IDLType (CORBA:IDLType IRObject irtypecode-mixin)
  :id "IDL:omg.org/CORBA/IDLType:1.0"
  :attributes ()
  :defaults ())


;;;; Contained

(define-ir-class Contained (CORBA:Contained irobject)
  :attributes ((id :readonly            ; setter defined below
                   :initarg :subject-id
                   :accessor subject-id)
               (name "")
               (version "1.0")
               (defined_in nil :readonly)
               ;;(absolute_name "" :readonly)
               (containing_repository :readonly))
  :slots ((absolute_name :initarg :absolute_name)
          (package-prefix :initform "" :accessor package-prefix)))

(defmethod print-object ((obj contained) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (slot-value obj 'name))))

(defmethod (setf op:id) (new-id (obj contained))
  (let ((repository (op:containing_repository obj))
        (old-id (op:id obj)))
    (unless (equal new-id old-id)
      (when (gethash new-id (idmap repository))
        (error (system-exception 'CORBA:BAD_PARAM 2)))
      (remhash old-id (idmap repository))
      (setf (gethash new-id (idmap repository)) obj)
      (setf (slot-value obj 'id) new-id))))

(defmethod (setf op:name) :before (value (obj contained))
  (loop for peer in (contents (op:defined_in obj))
        unless (eq obj peer)
        do (when (string-equal value (op:name peer))
             (raise-system-exception 'CORBA:bad_param 1))))


(define-method absolute_name ((obj contained))
  (unless (and (slot-boundp obj 'absolute_name)
               (not (equal (slot-value obj 'absolute_name)
                           "")))
    (setf (slot-value obj 'absolute_name)
          (concatenate 'string
                       (op:absolute_name (op:defined_in obj)) "::" (op:name obj))))
  (slot-value obj 'absolute_name))

(defmethod slot-updated :after ((obj contained))
  (slot-makunbound obj 'absolute_name))

(define-method describe ((obj contained))
  (CORBA:Contained/Description
   :kind (op:def_kind obj)
   :value (describe-contained obj)))

(define-method "MOVE" ((obj contained) to-container new-name new-version)
  (check-type to-container CORBA:Container)
  (check-type new-name string)
  (check-type new-version string)
  (unless (eql (op:containing_repository obj)
               (op:containing_repository to-container))
    (raise-system-exception 'CORBA:BAD_PARAM 4))
  (moveto to-container obj new-name new-version))

(define-method op:destroy ((obj contained))
  (remhash (subject-id obj) (idmap (op:containing_repository obj)))
  (let ((container (op:defined_in obj)))
    (when container
      (setf (contents container)
            (delete obj (contents container))))))



;;;; Container

(define-ir-class Container (CORBA:Container irobject)
  :slots ((contents :initarg :contents
                    :initform '()
                    :accessor contents)))


(defmethod slot-updated :after ((obj Container))
  (dolist (c (contents obj)) (slot-updated c)))


(defun addto (container object)
  "Add a contained object to a container"
  (let ((repository (op:containing_repository container))
        (id (op:id object)))
    ;; A BAD_PARAM exception is raised with minor code 2 if an object with
    ;; the specified id already exists in the Repository.
    (when (gethash id (idmap repository))
      (raise-system-exception 'CORBA:BAD_PARAM 2))
    (moveto container object (op:name object) (op:version object))
    (setf (gethash id (idmap repository)) object)))


(defmethod moveto ((c container) (object contained) new-name new-version)
  ;; A BAD_PARAM exception with minor code 3 is raised if the specified
  ;; name already exists within this Container and multiple versions are
  ;; not supported.
  (when (member new-name (contents c) :key #'op:name :test #'string-equal)
    (raise-system-exception 'CORBA:bad_param 3))
  (with-slots (defined_in containing_repository name version) 
              object
    (let ((old-container defined_in))
      (when old-container
        (setf (contents old-container) (delete object (contents old-container)))))
    (setf (contents c) (nconc (contents c) (list object)))
    (setf containing_repository (op:containing_repository c))
    (setf defined_in c)
    (setf name new-name)
    (setf version new-version)
    (slot-updated object))
  object)


(define-method contents ((obj container) limit-type exclude-inherit)
  (declare (ignore exclude-inherit))
  ;; exclude-inherit only aplicable for operations???
  (loop for contained in (contents obj)
      when (and (or (eql limit-type :dk_all)
                    (eql limit-type (op:def_kind contained))))
      collect contained))

(define-method lookup_name
    ((obj container) search-name levels-to-search limit-type exclude-inherit)
  (loop for contained in (op:contents obj limit-type exclude-inherit)
      when (equal search-name (op:name contained))
      collect contained into top-level
      when (and (containerp contained)
                (or (> levels-to-search 1) (= levels-to-search -1)))
      collect (op:lookup_name contained search-name
                              (if (= levels-to-search -1)
                                -1
                                (1- levels-to-search))
                              limit-type exclude-inherit)
      into sub-levels
      finally (return (mapcan #'identity (cons top-level sub-levels)))))

(defgeneric containerp (x)
  (:method ((x t)) nil)
  (:method ((x container)) t))


(define-method lookup ((obj null) search_name)
  (declare (ignore search_name))
  nil)

(define-method lookup ((obj container) search_name)
  (cond ((string-starts-with search_name "::")
         (op:lookup (op:containing_repository obj)
                    (subseq search_name 2)))
        (t
         (multiple-value-bind (first-name rest-name)
                              (split-name search_name)
           (let ((contained
                  (find first-name (op:contents obj :dk_all nil)
                        :key #'op:name
                        :test #'string-equal)))
             (cond ((null contained)
                    (op:lookup (op:defined_in obj) search_name))
                   (rest-name
                    (op:lookup contained rest-name))
                   (t
                    contained)))))))

(defun split-name (str)
  (let ((method-end (position #\: str)))
    (if method-end
      (values (subseq str 0 method-end)
              (subseq str (+ method-end 2)))
      str )))


(define-method op:describe_contents ((self container) limit-type exclude-inherited max-return-objects)
  (when (eql max-return-objects -1)
    (setq max-return-objects most-positive-fixnum))
  (loop for c in (op:contents self limit-type exclude-inherited)
        repeat max-return-objects
        collect (CORBA:Container/Description
                 :contained_object c
                 :kind (op:def_kind c)
                 :value (describe-contained c))))


;;;  ModuleDef create_module (
;;;                           in RepositoryId id,
;;;                           in Identifier name,
;;;                           in VersionSpec version

(define-method create_module ((self container) id name version)
  (addto self (make-instance 'module-def
                :id id :name name :version version)))


;;;  ConstantDef create_constant (
;;;                               in RepositoryId id,
;;;                               in Identifier name,
;;;                               in VersionSpec version,
;;;                               in IDLType type,
;;;                               in any value                  )

(define-method create_constant ((self container)
                                id name version type value)
  (addto self (make-instance 'constant-def
                :id id :name name :version version
                :type_def type :value value)))

;;;  StructDef create_struct (
;;;                           in RepositoryId id,
;;;                           in Identifier name,
;;;                           in VersionSpec version,
;;;                           in StructMemberSeq members

(define-method create_struct ((self container)
                              id name version members)
  (let ((obj (make-instance 'struct-def
               :id id :name name :version version)))
    (setf (op:members obj) members)
    (addto self obj)))


;;;  /* This operation is missing in the CORBA2 spec! */
;;;  ExceptionDef create_exception (
;;;                                 in RepositoryId id,
;;;                                 in Identifier name,
;;;                                 in VersionSpec version,
;;;                                 in StructMemberSeq members
;;;                                 );

(define-method create_exception ((self container)
                                 id name version members)
  (addto self (make-instance 'exception-def
                :id id
                :name name
                :version version
                :members members)))



;;;  UnionDef create_union (
;;;                         in RepositoryId id,
;;;                         in Identifier name,
;;;                         in VersionSpec version,
;;;                         in IDLType discriminator_type,
;;;                         in UnionMemberSeq members           );

(define-method create_union ((self container)
                             id name version discriminator_type members)
  (check-type discriminator_type CORBA:IDLType)
  (assert (and (typep members 'sequence)
               (every (lambda (x) (typep x 'CORBA:UnionMember))
                      members))
          (members)
          'type-error :datum 'members :expected-type 'corba:unionmemberseq)
  (addto self (make-instance 'union-def
                :id id :name name :version version
                :discriminator_type_def discriminator_type
                :members members)))


;;;  EnumDef create_enum (
;;;                       in RepositoryId id,
;;;                       in Identifier name,
;;;                       in VersionSpec version,
;;;                       in EnumMemberSeq members
;;;                       );

(define-method create_enum ((self container)
                              id name version members)
  (addto self (make-instance 'enum-def
                :id id :name name :version version
                :members members)))


;;;  AliasDef create_alias (
;;;                         in RepositoryId id,
;;;                         in Identifier name,
;;;                         in VersionSpec version,
;;;                         in IDLType original_type    );

(define-method create_alias ((self container)
                             id name version original_type)
  (addto self (make-instance 'alias-def
                :id id :name name :version version
                :original_type_def original_type)))


;;;  InterfaceDef create_interface (
;;;            in RepositoryId id,
;;;            in Identifier name,
;;;            in VersionSpec version,
;;;            in InterfaceDefSeq base_interfaces );

(define-method create_interface ((self container)
                                 id name version base_interfaces)
  (addto self (make-instance 'interface-def
                :id id :name name :version version
                :base_interfaces base_interfaces)))


;;;    ValueBoxDef create_value_box(
;;;                                 in RepositoryId id,
;;;                                 in Identifier name,
;;;                                 in VersionSpec version,
;;;                                 in IDLType original_type_def

(define-method create_value_box ((self container)
                                 id name version original_type_def)
  (addto self (make-instance 'valuebox-def
                :id id :name name :version version
                :original_type_def original_type_def)))

;;;    NativeDef create_native(
;;;                            in RepositoryId id,
;;;                            in Identifier name,
;;;                            in VersionSpec version

(define-method create_native ((self container) id name version )
  (addto self (make-instance 'native-def
                :id id :name name :version version )))

;;;    AbstractInterfaceDef create_abstract_interface(
;;;                                          in RepositoryId id,
;;;                                          in Identifier name,
;;;                                          in VersionSpec version,
;;;                                          in AbstractInterfaceDefSeq base_interfaces

(define-method create_abstract_interface ((self container)
                                 id name version base_interfaces)
  (addto self (make-instance 'abstractinterface-def
                :id id :name name :version version
                :base_interfaces base_interfaces)))
		
;;;    LocalInterfaceDef create_local_interface(
;;;                                             in RepositoryId id,
;;;                                             in Identifier name,
;;;                                             in VersionSpec version,
;;;                                             in InterfaceDefSeq base_interfaces

(define-method create_local_interface ((self container)
                                 id name version base_interfaces)
  (addto self (make-instance 'localinterface-def
                :id id :name name :version version
                :base_interfaces base_interfaces)))


;;; ValueDef create_value

(define-method create_value ((self container)
                              id name version is_custom is_abstract
                              base_value is_truncatable abstract_base_values
                              supported_interfaces initializers)
  (check-type id CORBA:RepositoryId)
  (check-type name CORBA:Identifier)
  (check-type version CORBA:VersionSpec)
  (check-type is_custom CORBA:boolean)
  (check-type is_abstract CORBA:boolean)
  (check-type base_value (or null CORBA:ValueDef))
  (check-type is_truncatable CORBA:boolean)
  (check-type abstract_base_values CORBA:ValueDefSeq)
  (check-type supported_interfaces CORBA:InterfaceDefSeq)
  (check-type initializers CORBA:InitializerSeq)
  (addto self (make-instance 'value-def
                :id id :name name :version version
                :is_custom is_custom  :is_abstract is_abstract
                :base_value base_value :is_truncatable is_truncatable
                :abstract_base_values abstract_base_values
                :supported_interfaces supported_interfaces
                :initializers initializers)))


(define-method op:destroy ((obj container))
  (dolist (x (contents obj))
    (op:destroy x))
  (call-next-method))

		

;;;; interface Repository : Container

(define-ir-class Repository (CORBA:Repository Container)
  :def_kind :dk_repository
  :slots ((idmap :initform (make-hash-table :test #'equal)
                 :reader idmap)
          (primitives-cache
           :initform nil
           :accessor primitives-cache )))


;;; Method to simplify computation of absolute_name
(define-method absolute_name ((obj Repository))
  "")

(define-method containing_repository ((r Repository))
  r)

(defmethod subject-id ((r repository))
  "")

;;;  Contained lookup_id (in RepositoryId search_id);

(define-method lookup_id ((rep repository) id)
  (gethash id (idmap rep)))


;;; TypeCode get_canonical_typecode(in TypeCode tc);

(define-method get_canonical_typecode ((container Repository) tc)
  (flet ((reconstruct ()
           (map-typecode (lambda (tc) (op:get_canonical_typecode container tc))
                         tc)))
    (handler-case
      (let ((id (op:id tc)))
        (let ((obj (op:lookup_id container id)))
          (if obj
            (op:type obj)
            (reconstruct))))
      (CORBA:TYPECODE/BADKIND () (reconstruct)))))



;;;  PrimitiveDef get_primitive (in PrimitiveKind kind);

(define-method get_primitive ((container Repository) kind)
  (with-accessors ((cache primitives-cache)) container
    (unless cache
      (setf cache
            (loop for (kind tc) 
                  in '((:pk_null CORBA:tc_null)
                       (:pk_void CORBA:tc_void)
                       (:pk_short CORBA:tc_short)
                       (:pk_long CORBA:tc_long)
                       (:pk_ushort CORBA:tc_ushort)
                       (:pk_ulong CORBA:tc_ulong)
                       (:pk_float CORBA:tc_float)
                       (:pk_double CORBA:tc_double)
                       (:pk_boolean CORBA:tc_boolean)
                       (:pk_char CORBA:tc_char)
                       (:pk_octet CORBA:tc_octet)
                       (:pk_any CORBA:tc_any)
                       (:pk_typecode CORBA:tc_TypeCode)
                       ;;(:pk_Principal CORBA:tc_Principal)
                       (:pk_string CORBA:tc_string)
                       (:pk_objref CORBA:tc_object)
                       (:pk_longlong CORBA:tc_longlong)
                       (:pk_ulonglong CORBA:tc_ulonglong)
                       (:pk_longdouble CORBA:tc_longdouble)
                       (:pk_wchar CORBA:tc_wchar)
                       (:pk_wstring CORBA:tc_wstring)
                       (:pk_value_base corba:tc_valuebase))
                  collect (make-instance 'primitive-def :kind kind 
                                         :type (symbol-value tc)))))
    (find kind cache :key #'op:kind)))


;;;  StringDef create_string (in unsigned long bound);

(define-method create_string ((container Repository) bound)
  (make-instance 'string-def
    :bound bound))

;;;  WstringDef create_wstring (in unsigned long bound);

(define-method create_wstring ((container Repository) bound)
  (make-instance 'wstring-def
    :bound bound))


;;;  SequenceDef create_sequence
;;;     (in unsigned long bound,in IDLType element_type);

(define-method create_sequence ((container Repository) bound element-type)
  (check-type bound integer)
  (check-type element-type corba:idltype)
  (make-instance 'sequence-def
    :bound bound
    :element_type_def element-type))


;;;  ArrayDef create_array
;;;     (in unsigned long length,in IDLType element_type);

(define-method create_array ((container Repository) length element_type)
  (make-instance 'array-def
    :length length
    :element_type_def element_type))


;;;  FixedDef create_fixed
;;;     (in unsigned short digits,in short scale);

(define-method create_fixed ((container Repository) digits scale)
  (make-instance 'fixed-def
    :digits digits
    :scale scale))


(define-method op:destroy ((obj repository))
  (raise-system-exception 'omg.org/corba:bad_inv_order
                          2 :completed_yes))



;;;; ModuleDef

(define-ir-class module-def (CORBA:ModuleDef container contained)
  :def_kind :dk_module)


(defmethod describe-contained ((module module-def))
  (CORBA:ModuleDescription
   :name (op:name module)
   :id (op:id module)
   :defined_in (subject-id (op:defined_in module))
   :version (op:version module)))

;;;; interface ConstantDef : Contained
;;;   readonly attribute TypeCode type;
;;;   attribute IDLType type_def;
;;;   attribute any value;

(define-ir-class constant-def (CORBA:ConstantDef contained)
  :def_kind :dk_constant
  :attributes ((type_def)
               (value)))


(define-method type ((obj constant-def))
  (op:type (op:type_def obj)))

(defmethod describe-contained ((def constant-def))
  (CORBA:ConstantDescription
   :name (op:name def)
   :id   (subject-id def)
   :defined_in (subject-id (op:defined_in def))
   :version (op:version def)
   :type (op:type def)
   :value (op:value def) ))

;;;; InterfaceDef

(define-ir-class interface-def (CORBA:InterfaceDef container contained idltype)
  :def_kind :dk_interface 
  :attributes ((base_interfaces)))

(defmethod idltype-tc ((idef interface-def))
  (create-interface-tc (subject-id idef) (op:name idef)))

(define-method is_a ((def interface-def) interface-id)
  (or
   (equal (subject-id def) interface-id)
   (equal interface-id (symbol-ifr-id 'corba:object))
   (some (lambda (b) (equal interface-id (op:id b)))
         (op:base_interfaces def))))

(define-method describe_interface ((def interface-def))
  (CORBA:InterfaceDef/FullInterfaceDescription
   :name (op:name def)
   :id   (subject-id def)
   :defined_in (subject-id (op:defined_in def))
   :version (op:version def)
   :operations (map 'list #'operation-description
                    (op:contents def :dk_operation nil))
   :attributes (map 'list #'describe-contained
                    (op:contents def :dk_attribute nil))
   :base_interfaces (map 'list #'subject-id (op:base_interfaces def))
   :type (op:type def) ))

(defmethod describe-contained ((def interface-def))
  (CORBA:InterfaceDescription
   :name (op:name def)
   :id   (subject-id def)
   :defined_in (subject-id (op:defined_in def))
   :version (op:version def)
   :base_interfaces (map 'list #'subject-id (op:base_interfaces def))))

(define-method contents ((obj interface-def) limit-type exclude-inherit)
  (if exclude-inherit
      (call-next-method)
    (append (call-next-method)
            (loop for x in (coerce (op:base_interfaces obj) 'list)
                append (op:contents x limit-type nil)))))


(defun check-unique-name (container name &key (minor 3))
  (when (op:lookup_name container name 1 :dk_all nil)
    (raise-system-exception 'CORBA:bad_param minor)))

(define-method (setf op:base_interfaces) :before (value (self interface-def))
  (loop for obj in (contents self) do
        (loop for base in value do
              (check-unique-name base (op:name obj) :minor 5))))


;;;  AttributeDef create_attribute

(define-method create_attribute ((self interface-def)
                                 id name version type mode)
  (check-type id string)                ; RepositoryId
  (check-type name string)              ; Identifier
  (check-type version string)           ; VersionSpec
  (check-type type IDLType)
  (check-type mode CORBA:AttributeMode)
  (check-unique-name self name)
  (addto self (make-instance 'attribute-def
                :id id :name name :version version
                :type_def type :mode mode)))


;;;  OperationDef create_operation

(define-method create_operation ((self interface-def)
                                 id name version
                                 result mode params exceptions contexts)
  (check-type id string)                ; RepositoryId
  (check-type name string)              ; Identifier
  (check-type version string)           ; VersionSpec
  (check-type result IDLType)
  (check-type mode CORBA:OperationMode)
  (check-type params CORBA:ParDescriptionSeq)
  (check-type exceptions CORBA:ExceptionDefSeq)
  (check-type contexts CORBA:ContextIdSeq)
  (check-unique-name self name)
  (addto self (make-instance 'operation-def
                :id id :name name :version version
                :result_def result :mode mode
                :params params :exceptions exceptions
                :contexts contexts)))


;;;; AttributeDef

(define-ir-class attribute-def (CORBA:AttributeDef contained)
  :def_kind :dk_attribute
  :attributes ((type_def)
               (mode)))


(define-method type ((adef attribute-def))
  (op:type (op:type_def adef)))

(defmethod describe-contained ((adef attribute-def))
  (CORBA:AttributeDescription
   ;; Identifier name
   :name (op:name adef)
   ;; RepositoryId id
   :id (op:id adef)
   ;; RepositoryId defined_in
   :defined_in (subject-id (op:defined_in adef))
   ;; VersionSpec version
   :version (op:version adef)
   ;; TypeCode type
   :type (op:type adef)
   ;; AttributeMode mode
   :mode (op:mode adef)))


;;;; OperationDef

(define-ir-class operation-def (CORBA:OperationDef contained)
  :def_kind :dk_operation
  :attributes ((result :readonly :virtual result)
               (result_def)
               (params nil)
               (mode ':op_normal)
               (contexts nil)
               (exceptions nil)))


(defun validate-operation-def (mode result params exceptions)
  (when (eq mode :op_oneway)
    (unless (and (eq :tk_void (op:kind (op:type result)))
                 (zerop (length exceptions))
                 (every (lambda (p)
                          (eq :param_in (op:mode p)))
                        params))
      (raise-system-exception 'CORBA:bad_param 31 :completed_yes))))

(defun validate-operation-def-change (def &key 
                                            (mode (op:mode def))
                                            (result (op:result_def def))
                                            (params (slot-value def 'params))
                                            (exceptions (op:exceptions def)))
  (validate-operation-def mode result params exceptions))

(defmethod initialize-instance :before ((def operation-def)
                                        &key result_def mode params exceptions)
  (validate-operation-def mode result_def params exceptions))

(defmethod (setf op:result_def) :before (new (def operation-def)) 
  (validate-operation-def-change def :result new))

(defmethod (setf op:mode) :before (new (def operation-def))
  (validate-operation-def-change def :mode new))               

(defmethod (setf op:params) :before (new (def operation-def))
  (validate-operation-def-change def :params new))               

(defmethod (setf op:exceptions) :before (new (def operation-def))
  (validate-operation-def-change def :exceptions new))               

(defmethod result ((opdef operation-def))
  (op:type (slot-value opdef 'result_def)))

(defmethod op:params :before ((opdef operation-def) &key)
  (doseq (pd (slot-value opdef 'params))
         (setf (op:type pd) (op:type (op:type_def pd)))))


(defmethod opdef-inparam-typecodes ((opdef operation-def))
  (loop for param in (op:params opdef)
      unless (eq (op:mode param) :param_out)
      collect (op:type param)))

#+unused-defuns
(defmethod opdef-outparam-typecodes ((opdef operation-def))
  (let ((real-params
         (loop for param in (op:params opdef)
             unless (eq (op:mode param) :param_in)
             collect (op:type param)))
        (result (op:result opdef)))
    (if (eq :tk_void (typecode-kind result))
        real-params
      (cons result real-params))))




(defun operation-description (opdef)
  ;;  struct OperationDescription
  (CORBA:OperationDescription
   ;;  Identifier name;
   :name (op:name opdef)
   ;;  RepositoryId id;
   :id   (op:id opdef)
   ;;  RepositoryId defined_in;
   :defined_in (op:id (op:defined_in opdef))
   ;;  VersionSpec version;
   :version (op:version opdef)
   ;;  TypeCode result;
   :result (op:result opdef)
   ;;  OperationMode mode;
   :mode (op:mode opdef)
   ;;  ContextIdSeq contexts;
   :contexts (op:contexts opdef)
   ;;  ParDescriptionSeq parameters;
   :parameters (op:params opdef)
   ;;  ExcDescriptionSeq exceptions;
   :exceptions (map 'list
                    'describe-contained (op:exceptions opdef))))

(defmethod describe-contained ((obj operation-def))
  (operation-description obj))

;;;; TypedefDef

(define-ir-class typedef-def (CORBA:TypedefDef contained idltype)
  :def_kind :dk_typedef)


(defmethod describe-contained ((obj typedef-def))
  (CORBA:TypeDescription
   ;; Identifier name
   :name (op:name obj)
   ;; RepositoryId id
   :id (op:id obj)
   ;; RepositoryId defined_in
   :defined_in (subject-id (op:defined_in obj))
   ;; VersionSpec version
   :version (op:version obj)
   ;; TypeCode type
   :type (op:type obj)))

;;;; interface AliasDef : TypedefDef
;;  attribute IDLType original_type_def;

(define-ir-class alias-def (corba:AliasDef typedef-def)
  :def_kind :dk_alias
  :attributes ((original_type_def)))


(defmethod idltype-tc ((def alias-def))
  (create-alias-tc (op:id def) (op:name def)
                   (op:type (op:original_type_def def))))

(define-method op:destroy ((obj alias-def))
  (let ((orig (op:original_type_def obj)))
    (when (and (typep orig 'CORBA:IDLtype)
               (typep orig 'CORBA:Contained))
      (op:destroy orig)))
  (call-next-method))




;;;; interface StructDef : TypedefDef
;; attribute StructMemberSeq members;
;; struct StructMember {
;;  Identifier name;
;;  TypeCode type;
;;  IDLType type_def;

(define-ir-class struct-def (corba:StructDef typedef-def container)
  :def_kind :dk_struct
  :attributes ((members)))


(defmethod op:members :before ((obj struct-def) &rest x)
  (declare (ignore x))
  (doseq (m (slot-value obj 'members))
    (setf (op:type m) (op:type (op:type_def m)))))


(defmethod idltype-tc ((def struct-def))
  (create-struct-tc (op:id def) (op:name def) (simple-struct-members (op:members def))))


;;;; interface UnionDef : TypedefDef
;;;   readonly attribute TypeCode discriminator_type;
;;;   attribute IDLType discriminator_type_def;
;;;   attribute UnionMemberSeq members;

;;; struct UnionMember {
;;;   Identifier name;
;;;   any label;
;;;   TypeCode type;
;;;   IDLType type_def;
;;; typedef sequence <UnionMember> UnionMemberSeq;

(define-ir-class union-def (CORBA:UnionDef typedef-def container)
  :def_kind :dk_union
  :attributes ((discriminator_type_def)
               (members)))


(define-method discriminator_type ((obj union-def))
  (op:type (op:discriminator_type_def obj)))

(defmethod op:members :before ((self union-def) &rest args)
  (declare (ignore args))
  (doseq (member (slot-value self 'members))
    (setf (op:type member) (op:type (op:type_def member)))))

(defmethod idltype-tc ((obj union-def))
  ;; FIXME: should be simpler to use op:create_union_tc
  (flet ((default-label-p (label)
           (and (not (symbolp label))
                (eql :tk_octet (op:kind (any-typecode label)))
                (= 0 (any-value label)))))
    (create-union-tc (op:id obj) (op:name obj)
                     (op:discriminator_type obj)
                     (map 'list (lambda (m)
                                  (list (if (default-label-p (op:label m))
                                          'default
                                          (op:label m))
                                        (op:name m)
                                        (op:type m)))
                          (op:members obj)))))


;;;; interface EnumDef : TypedefDef
;;;    attribute EnumMemberSeq members;


(define-ir-class enum-def (corba:EnumDef typedef-def)
  :def_kind :dk_enum
  :attributes ((members)))


(defmethod idltype-tc ((obj enum-def))
  (create-enum-tc (op:id obj) (op:name obj) (op:members obj)))

;;;; interface ExceptionDef : Contained, Container
;;;  readonly attribute TypeCode type;
;;;  attribute StructMemberSeq members;

(define-ir-class exception-def (corba:ExceptionDef contained container
                                                   irtypecode-mixin)
  :def_kind :dk_exception
  :attributes ((members)))


(defmethod op:members :before ((obj exception-def) &rest x)
  (declare (ignore x))
  (doseq (m (slot-value obj 'members))
    (setf (op:type m) (op:type (op:type_def m)))))


(defmethod idltype-tc ((obj exception-def))
  (create-exception-tc (op:id obj) (op:name obj)
                       (simple-struct-members (op:members obj))))


(defmethod describe-contained ((obj exception-def))
  (CORBA:ExceptionDescription
   ;;  Identifier name;
   :name (op:name obj)
   ;;  RepositoryId id;
   :id (op:id obj)
   ;;  RepositoryId defined_in;
   :defined_in (subject-id (op:defined_in obj))
   ;;  VersionSpec version;
   :version (op:version obj)
   ;;  TypeCode type;
   :type (op:type obj)))

;;;; moveto restrictions - for IDL-types union, struct, execption

(defmethod moveto :before ((c union-def) (object contained) new-name new-version)
  (declare (ignore new-name new-version))
  (typecase object 
    ((or Struct-Def Union-Def Enum-Def))
    (otherwise
     (raise-system-exception 'CORBA:BAD_PARAM 4))))


(defmethod moveto :before ((c struct-def) (object contained) new-name new-version)
  (declare (ignore new-name new-version))
  (typecase object 
    ((or Struct-Def Union-Def Enum-Def))
    (otherwise
     (raise-system-exception 'CORBA:BAD_PARAM 4))))


(defmethod moveto :before ((c exception-def) (object contained) new-name new-version)
  (declare (ignore new-name new-version))
  (typecase object 
    ((or Struct-Def Union-Def Enum-Def))
    (otherwise
     (raise-system-exception 'CORBA:BAD_PARAM 4))))



;;;; PrimitiveDef

(define-ir-class primitive-def (corba:PrimitiveDef idltype)
  :def_kind :dk_primitive
  :attributes ((kind))
  :slots ((type :initarg :type)) ; add initarg
)


(defmethod print-object ((obj primitive-def) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A" (slot-value obj 'kind))))

(define-method op:destroy ((obj primitive-def))
  (raise-system-exception 'omg.org/corba:bad_inv_order
                          2 :completed_yes))


;;;; interface StringDef : IDLType
;;;  attribute unsigned long bound;

(define-ir-class string-def (corba:StringDef idltype)
  :def_kind :dk_string
  :attributes ((bound 0)))

(defmethod idltype-tc ((obj string-def))
  (create-string-tc (op:bound obj)))


;;;; interface WstringDef : IDLType
;;;  attribute unsigned long bound;

(define-ir-class wstring-def (corba:WstringDef idltype)
  :def_kind :dk_wstring
  :attributes ((bound 0)))


(defmethod idltype-tc ((obj wstring-def))
  (create-wstring-tc (op:bound obj)))

;;;; SequenceDef
;;interface SequenceDef : IDLType
;;  attribute unsigned long bound;
;;  readonly attribute TypeCode element_type;
;;  attribute IDLType element_type_def;

(define-ir-class sequence-def (corba:SequenceDef IDLType)
  :def_kind :dk_sequence
  :attributes ((bound 0)                  ; long
               ;;(element_type :readonly) ; TypeCode
               (element_type_def))      ; IDLType
  :defaults ())

(define-method element_type ((obj sequence-def))
  (op:type (op:element_type_def obj)))

(defmethod idltype-tc ((obj sequence-def))
  (create-sequence-tc (op:bound obj) (op:element_type obj)))


;;;; ArrayDef : IDLType
;;        attribute unsigned long         length;
;;        readonly attribute TypeCode     element_type;
;;        attribute IDLType               element_type_def;

(define-ir-class array-def (CORBA:ArrayDef idltype)
  :def_kind :dk_array
  :attributes ((length 0)
               (element_type_def)))

(define-method element_type ((obj array-def))
  (op:type (op:element_type_def obj)))

(defmethod idltype-tc ((obj array-def))
  (create-array-tc (op:length obj) (op:element_type obj)))


;;;; interface FixedDef : IDLType
;;   attribute unsigned short digits;
;;   attribute short scale;

(define-ir-class fixed-def (CORBA:FixedDef idltype)
  :def_kind :dk_fixed
  :attributes ((digits 1)
               (scale 0)))

(defmethod idltype-tc ((obj fixed-def))
  (create-fixed-tc (op:digits obj) (op:scale obj)))



;;;; interface ValueBoxDef : TypedefDef 
;;    attribute IDLType original_type_def;

(define-ir-class valuebox-def (corba:valueboxdef typedef-def)
  :def_kind :dk_valuebox
  :attributes ((original_type_def)))

(defmethod idltype-tc ((def valuebox-def))
  (create-value-box-tc (op:id def) (op:name def)
                       (op:type (op:original_type_def def))))



;;;;  interface AbstractInterfaceDef : InterfaceDef { };
;;
;; An AbstractInterfaceDef object represents a CORBA 2.3 abstract
;; interface definition. It can contain constants, typedefs,
;; exceptions, operations, and attributes. Its base interfaces can
;; only contain AbstractInterfaceDefs.

(define-ir-class AbstractInterface-Def (CORBA:AbstractInterfaceDef interface-def)
  :def_kind :dk_abstractinterface)

(defmethod idltype-tc ((def AbstractInterface-Def))
  (create-abstract-interface-tc (op:id def) (op:name def)))

(define-method is_a ((def AbstractInterface-Def) interface-id)
  (or
   (equal (subject-id def) interface-id)
   (equal interface-id "IDL:omg.org/CORBA/AbstractBase:1.0")
   (some (lambda (b) (op:is_a b interface-id ))
         (op:base_interfaces def))))


;;;;  interface LocalInterfaceDef : InterfaceDef { };
;; 
;; An LocalInterfaceDef object represents a local interface definition. It can
;; contain constants, typedefs, exceptions, operations, and attributes. Its
;; base interfaces can only contain InterfaceDefs or LocalInterfaceDefs.

(define-ir-class LocalInterface-Def (CORBA:LocalInterfaceDef interface-def)
  :def_kind :dk_localinterface)

(defmethod idltype-tc ((def LocalInterface-Def))
  (create-local-interface-tc (op:id def) (op:name def)))

(define-method is_a ((def LocalInterface-Def) interface-id)
  (or
   (equal (subject-id def) interface-id)
   (equal interface-id "IDL:omg.org/CORBA/LocalBase:1.0")
   (some (lambda (b) (op:is_a b interface-id ))
         (op:base_interfaces def))))


;;;;  interface NativeDef : TypedefDef { };

(define-ir-class Native-Def (CORBA:NativeDef typedef-def)
  :def_kind :dk_native)

(defmethod idltype-tc ((def Native-Def))
  (create-native-tc (op:id def) (op:name def)))


;;;;  interface ValueMemberDef : Contained
;;    readonly attribute TypeCode type;
;;    attribute IDLType type_def;
;;    attribute Visibility access;

(define-ir-class ValueMember-Def (CORBA:ValueMemberDef Contained)
  :def_kind :dk_valuemember
  :attributes (;;(type :readonly)
               (type_def)
               (access)))

(define-method "TYPE" ((def ValueMember-Def))
  (op:type (op:type_def def)))

(defmethod describe-contained ((def ValueMember-Def))
  (corba:ValueMember
   ;; Identifier name
   :name (op:name def)
   ;; RepositoryId id
   :id (op:id def)
   ;; RepositoryId defined_in
   :defined_in (subject-id (op:defined_in def))
   ;; VersionSpec version
   :version (op:version def)
   ;; TypeCode type
   :type (op:type def)
   ;; IDLType type_def
   :type_def (op:type_def def)
   ;; Visibility access
   :access (op:access def)))



;;;;  interface ValueDef : Container, Contained, IDLType
;;  attribute InterfaceDefSeq supported_interfaces;
;;  attribute InitializerSeq initializers;
;;  attribute ValueDef base_value;
;;  attribute ValueDefSeq abstract_base_values;
;;  attribute boolean is_abstract;
;;  attribute boolean is_custom;
;;  attribute boolean is_truncatable;

(define-ir-class Value-Def (CORBA:ValueDef Container Contained IDLType)
  :def_kind :dk_value
  :attributes ((supported_interfaces)
               (initializers)
               (base_value)
               (abstract_base_values)
               (is_abstract)
               (is_custom)
               (is_truncatable)))

(defmethod idltype-tc ((self Value-Def))
  (create-value-tc (op:id self) (op:name self)
                   (cond ((op:is_abstract self) corba:vm_abstract)
                         ((op:is_custom self) corba:vm_custom)
                         ((op:is_truncatable self) corba:vm_truncatable)
                         (t corba:vm_none))
                   (let ((base (op:base_value self)))
                     (and base (op:type base)))
                   (simple-value-members
                    (op:contents self :dk_valuemember t))))

(define-method contents ((obj Value-Def) limit-type exclude-inherit)
  (if exclude-inherit
      (call-next-method)
    (append (and (op:base_value obj)
                 (op:contents (op:base_value obj) limit-type nil))
            (call-next-method)
            (loop for x in (coerce (op:abstract_base_values obj) 'list)
                append (op:contents x limit-type nil))
            (loop for x in (coerce (op:supported_interfaces obj) 'list)
                append (op:contents x limit-type nil)))))

(define-method initializers :before ((self Value-Def))
  (doseq (i (slot-value self 'initializers))
    (doseq (m (op:members i))
      (setf (op:type m) (op:type (op:type_def m))))))

(defun check-only-one-non-abstract (interface-seq)
  (let ((non-abstract 0))
    (doseq (i interface-seq)
      (unless (typep i 'corba:abstractinterfacedef)
        (when (> (incf non-abstract) 1)
          (raise-system-exception 'CORBA:BAD_PARAM 12))))))

(define-method (setf op:supported_interfaces) :before (value (self value-def))
  (check-only-one-non-abstract value)
  (loop for obj in (contents self) 
        for name = (op:name obj) 
        do (loop for base in value do
                 (check-unique-name base name :minor 5))))


(defmethod initialize-instance :before ((def Value-Def) &key supported_interfaces)
  (check-only-one-non-abstract supported_interfaces))

(defmethod moveto :before ((c Value-Def) (object contained) new-name new-version)
  (declare (ignore new-name new-version))
  (typecase object 
    ((or Operation-Def ValueMember-Def Attribute-Def
         Typedef-Def Constant-Def Exception-Def))
    (otherwise
     (raise-system-exception 'CORBA:BAD_PARAM 4))))


(defmethod describe-contained ((def Value-Def))
  (corba:ValueDescription 
   ;; Identifier name
   :name (op:name def)
   ;; RepositoryId id
   :id (op:id def)
   ;; boolean is_abstract
   :is_abstract (op:is_abstract def)
   ;; boolean is_custom
   :is_custom (op:is_custom def)
   ;; RepositoryId defined_in
   :defined_in (subject-id (op:defined_in def))
   ;; VersionSpec version
   :version (op:version def)
   ;; RepositoryIdSeq supported_interfaces
   :supported_interfaces (map 'vector #'op:id (op:supported_interfaces def))
   ;; RepositoryIdSeq abstract_base_values
   :abstract_base_values (map 'vector #'op:id (op:abstract_base_values def))
   ;; boolean is_truncatable
   :is_truncatable (op:is_truncatable def)
   ;; RepositoryId base_value
   :base_value (subject-id (op:base_value def))))

(defmethod subject-id ((obj null))
  "")


;;; FullValueDescription describe_value();
(define-method "DESCRIBE_VALUE" ((def value-def))
  (corba:ValueDef/FullValueDescription
   ;; Identifier name
   :name (op:name def)
   ;; RepositoryId id
   :id (op:id def)
   ;; boolean is_abstract
   :is_abstract (op:is_abstract def)
   ;; boolean is_custom
   :is_custom (op:is_custom def)
   ;; RepositoryId defined_in
   :defined_in (subject-id (op:defined_in def))
   ;; VersionSpec version
   :version (op:version def)
   ;; OpDescriptionSeq operations
   :operations (mapcar #'operation-description (op:contents def :dk_operation nil))
   ;; AttrDescriptionSeq attributes
   :attributes (mapcar #'describe-contained (op:contents def :dk_attribute nil))
   ;; ValueMemberSeq members
   :members (mapcar #'describe-contained (op:contents def :dk_valuemember nil))
   ;; InitializerSeq initializers
   :initializers (op:initializers def)
   ;; RepositoryIdSeq supported_interfaces
   :supported_interfaces (map 'vector #'op:id (op:supported_interfaces def))
   ;; RepositoryIdSeq abstract_base_values
   :abstract_base_values (map 'vector #'op:id (op:abstract_base_values def))
   ;; boolean is_truncatable
   :is_truncatable (op:is_truncatable def)
   ;; RepositoryId base_value
   :base_value (subject-id (op:base_value def))
   ;; TypeCode type
   :type (op:type def)))


;;;  boolean is_a(in RepositoryId id);

(define-method is_a ((self Value-Def) id)
  "The is_a operation returns TRUE if the value on which it is
invoked either is identical to or inherits, directly or
indirectly, from the interface or value identified by its id
parameter or if the value of id is
IDL:omg.org/CORBA/ValueBase:1.0. Otherwise it returns FALSE."
  (check-type id string)
  (or (equal id (op:id self))
      (equal id "IDL:omg.org/CORBA/ValueBase:1.0")
      (some (lambda (def) (op:is_a def id))
            ;; FIXME: there is more to consider
            (op:abstract_base_values self))))
  

;;;  ValueMemberDef create_value_member 

(define-method create_value_member ((def Value-Def)
                                    id name version type access)
  (check-type id string)                ; RepositoryId
  (check-type name string)              ; Identifier
  (check-type version string)           ; VersionSpec
  (check-type type IDLType)
  (check-type access corba:visibility)
  (addto def (make-instance 'valuemember-def
                        :id id :name name :version version
                        :type_def type :access access)))


;;;  AttributeDef create_attribute

(define-method create_attribute ((self value-def)
                                 id name version type mode)
  (check-type id string)                ; RepositoryId
  (check-type name string)              ; Identifier
  (check-type version string)           ; VersionSpec
  (check-type type IDLType)
  (check-type mode CORBA:AttributeMode)
  (addto self (make-instance 'attribute-def
                             :id id :name name :version version
                             :type_def type :mode mode)))


;;;  OperationDef create_operation

(define-method create_operation ((self value-def)
                                 id name version
                                 result mode params exceptions contexts)
  (check-type id string)                ; RepositoryId
  (check-type name string)              ; Identifier
  (check-type version string)           ; VersionSpec
  (check-type result IDLType)
  (check-type mode CORBA:OperationMode)
  (check-type params CORBA:ParDescriptionSeq)
  (check-type exceptions CORBA:ExceptionDefSeq)
  (check-type contexts CORBA:ContextIdSeq)
  (addto self (make-instance 'operation-def
                :id id :name name :version version
                :result_def result :mode mode
                :params params :exceptions exceptions
                :contexts contexts)))


;;; clorb-ifr.lisp ends here
