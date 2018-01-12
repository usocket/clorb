(in-package :clorb)


(defun symbol-ifr-parent-id (symbol)
  (let ((parent (get symbol 'ifr-parent)))
    (if parent (symbol-ifr-id parent) "")))

(defun ifr-version (symbol)
  (get symbol 'ifr-version "1.0"))

(defun ifr-kind (symbol)
  (case (get-properties (symbol-plist symbol) '(ifr-result ifr-type ifr-bases))
    (ifr-result :dk_operation)
    (ifr-type   :dk_attribute)
    (ifr-bases  :dk_interface)
    (otherwise
     (let ((tc (get symbol 'typecode)))
       (when tc
         (case (typecode-kind tc)
           (:tk_alias  :dk_alias)
           (:tk_except :dk_exception)
           (:tk_struct :dk_struct)
           (:tk_enum   :dk_enum)
           (:tk_value_box :dk_valuebox)
           (:tk_value  :dk_value)))))))



(defmethod generate-ifr-description ((tc except-typecode) symbol)
  (corba:exceptiondescription
   :name (op:name tc)
   :id (op:id tc)
   :version (ifr-version symbol)
   :defined_in (symbol-ifr-parent-id symbol)
   :type tc))


(defmethod generate-ifr-description ((tc objref-typecode) symbol)
  (let (opdesc atdesc)
    (labels ((operations-and-attributes (symbol)
               (loop for sym in (get symbol 'ifr-contents)
                     for kind = (ifr-kind sym)
                     when (eq kind :dk_operation)
                     do (push (ifr-description sym) opdesc)
                     when (eq kind :dk_attribute)
                     do (push (ifr-description sym) atdesc))
               (mapc #'operations-and-attributes (get symbol 'ifr-bases))))
      (operations-and-attributes symbol)
      (corba:interfacedef/fullinterfacedescription
       :name (or (get symbol 'ifr-name) (op:name tc))
       :id   (symbol-ifr-id symbol)
       :defined_in ""
       :version (ifr-version symbol)
       :operations opdesc
       :attributes atdesc
       :base_interfaces (mapcar #'symbol-ifr-id (get symbol 'ifr-bases)) 
       :type (symbol-typecode symbol)))))


(defmethod generate-ifr-description ((tc null) symbol)
  ;; For Contained that hasn't got a TypeCode: 
  ;;  ModuleDef, OperationDef, AttributeDef
  (multiple-value-bind (indicator value)
                       (get-properties (symbol-plist symbol) '(ifr-result ifr-type))
    (case indicator
      (ifr-result                       ; it's an operation
       (CORBA:OperationDescription
        :id (symbol-ifr-id symbol)
        :name (get symbol 'ifr-name)
        :version (ifr-version symbol)
        :defined_in (symbol-ifr-parent-id symbol)
        :result value
        :mode (get symbol 'ifr-mode)
        :parameters (loop for (param-name param-mode param-type) in (get symbol 'ifr-params)
                          collect (corba:parameterdescription
                                   :name param-name
                                   :type param-type
                                   :mode param-mode))
        :exceptions (mapcar #'ifr-description (get symbol 'ifr-exceptions))))
      (ifr-type                         ; it's an attribute
       (CORBA:AttributeDescription
        :id (symbol-ifr-id symbol)
        :name (get symbol 'ifr-name)
        :version (ifr-version symbol)
        :defined_in (symbol-ifr-parent-id symbol)
        :mode (get symbol 'ifr-mode)
        :type value))
      ((nil) 
       (error "Can't generate a description for ~S" symbol)))))
  





;;;; interface ExceptionDef : Contained, Container
;;;  readonly attribute TypeCode type;
;;;  attribute StructMemberSeq members;

;; EXPERIMENTAL - IFR-objects that are adapters for (get information
;; from) description structs.

(defclass desc-exceptiondef (CORBA:ExceptionDef)
  ((desc  :initarg :description  :accessor description)))

#| #<EXCEPTIONDESCRIPTION
    :NAME "InvalidTypeForEncoding"
    :ID "IDL:omg.org/IOP/Codec/InvalidTypeForEncoding:1.0"
    :DEFINED_IN "IDL:omg.org/IOP/Codec:1.0"
    :VERSION "1.0"
    :TYPE #<EXCEPT-TYPECODE "InvalidTypeForEncoding" #x651B106>>
|#

;;; readonly attribute TypeCode type;
(define-method op:type ((obj desc-exceptiondef)) (op:type (description obj)))
;;; attribute StructMemberSeq members;
(define-method op:members ((obj desc-exceptiondef))
  (let* ((tc (op:type (description obj)))
         (n  (op:member_count tc)))
    (loop for i from 0 below n
         collect (let ((name (op:member_name tc i))
                       (mtc  (op:member_type tc i)))
                   (CORBA:StructMember
                    :name name
                    :type mtc
                    :type_def nil)))))


;;; CONTAINED
;;; // read/write interface
;;; attribute RepositoryId id;
(define-method op:id ((obj desc-exceptiondef)) (op:id (description obj)))
;;; attribute Identifier name;
(define-method op:name ((obj desc-exceptiondef)) (op:name (description obj)))
;;; attribute VersionSpec version;
(define-method op:version ((obj desc-exceptiondef)) (op:version (description obj)))
;;; // read interface
;;; readonly attribute Container defined_in;
;;; readonly attribute ScopedName absolute_name;
;;; readonly attribute Repository containing_repository;
;;; struct Description {
;;;   DefinitionKind kind;
;;;   any value;
;;; };
;;; Description describe ();
(define-method op:describe
  ((obj desc-exceptiondef))
  (corba:contained/description
   :kind :dk_exception
   :value (description obj)))


;;; CONTAINER
;;; Contained lookup(in ScopedName search_name);
;;; ContainedSeq contents(in DefinitionKind limit_type,
;;;                       in boolean exclude_inherited);
;;; ContainedSeq lookup_name(in Identifier search_name,
;;;                          in long levels_to_search,
;;;                          in DefinitionKind limit_type,
;;;                          in boolean exclude_inherited );
;;; struct Description {
;;;   Contained contained_object;
;;;   DefinitionKind kind;
;;;   any value;
;;; };
;;; typedef sequence<Description> DescriptionSeq;
;;; DescriptionSeq describe_contents(in DefinitionKind limit_type,
;;;                                  in boolean exclude_inherited,
;;;                                  in long max_returned_objs);


;;; interface IRObject {
;;;   // read interface
;;;   readonly attribute DefinitionKind def_kind;
(define-method op:def_kind ((obj desc-exceptiondef)) :dk_exception)

;;;   // write interface
;;;   void destroy ();
(define-method op:destroy ((obj desc-exceptiondef)) nil)


#|

(CORBA:IDL "clorb:idl;interface_repository.idl" 
           :output "clorb:src;y-ifr-base.lisp" 
           :package-decl nil
           :eval nil
           :exclude '("::CORBA::TypeCode")
           :skeleton nil)

(CORBA:IDL "clorb:idl;CosNaming.idl" 
           :output "clorb:src;y-cosnaming.lisp" 
           :package-decl t
           :eval nil
           :skeleton nil)


|#


