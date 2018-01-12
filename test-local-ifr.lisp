(in-package :clorb)

(define-test-suite "Local IFR test"
  (variables
   (repository (make-instance 'repository))
   (a-ulong (op:get_primitive repository :pk_ulong))
   (a-string (op:get_primitive repository :pk_string)))
  
  (define-test "General"
    (ensure-typep :dk_constant 'CORBA:DefinitionKind))
  
  
  (define-test "get_canonical_typecode"
    (let* ((mylong 
            (op:create_alias repository "IDL:mylong:1.0" "mylong" "1.0"
                             a-ulong))
           (myseq (op:create_alias repository "IDL:myseq:1.0" "myseq" "1.0"
                                   (op:create_sequence repository 0 mylong))))
      (let* ((ptc-mylong (create-alias-tc "" "" CORBA:tc_ulong))
             (ntc-mylong (create-alias-tc "IDL:mylong:1.0" "" CORBA:tc_ulong))
             (tc-myseq-ptc-mylong
              (create-alias-tc "IDL:myseq:1.0" ""
                               (create-sequence-tc 0 ptc-mylong))))
        (ensure-typecode
         (op:get_canonical_typecode repository tc-myseq-ptc-mylong)
         (op:type myseq))
        (ensure-typecode
         (op:get_canonical_typecode repository (create-sequence-tc 0 ntc-mylong))
         (op:type (op:original_type_def myseq))))))
  
  
  (define-test "Contained"
    (let* ((name "foo")
           (id "IDL:foo:1.0")
           (sub-id "IDL:mod/foo:1.0")
           (obj (op:create_enum repository id name "1.0" '("fie" "fum")))
           (module (op:create_module repository "my-module" "mod" "1.1"))
           (sub-obj (op:create_enum module sub-id name "1.0" '("fie" "fum"))))
      (ensure-eql (op:lookup repository name) obj)
      (ensure-eql (op:lookup_id repository id) obj)
      (ensure-pattern* obj 'op:id id 'op:name name 'op:version "1.0"
                       'op:defined_in repository
                       'op:containing_repository repository
                       'op:absolute_name (concatenate 'string "::" name)
                       'op:describe (struct-pattern
                                     'struct-class-name 'CORBA:Contained/Description
                                     'op:kind :dk_enum 'op:value (struct-pattern)))
      (ensure-pattern* sub-obj 'op:defined_in module 'op:containing_repository repository
                       'op:absolute_name (concatenate 'string (op:absolute_name module) "::" name))
      (ensure-eql (op:lookup_id repository sub-id) sub-obj)
      ;; Change ID
      (let ((new-id "IDL:foob:1.1"))
        (setf (op:id obj) new-id)
        (ensure-equalp (op:id obj) new-id)
        (ensure-eql (op:lookup_id repository new-id) obj)
        (ensure-eql (op:lookup_id repository id) nil)
        (ensure-exception (setf (op:id obj) sub-id)
                          corba:bad_param 'op:minor (std-minor 2)))
      ;; Change Name
      (let ((new-name "barbar"))
        (ensure-exception (setf (op:name obj) "mod")
                          corba:bad_param 'op:minor (std-minor 1))
        (setf (op:name obj) new-name)
        (ensure-pattern* obj 
                         'op:absolute_name (format nil "::~A" new-name)
                         'op:type (pattern 'op:name new-name))
        (setf (op:name module) "xmod")
        (ensure-pattern* sub-obj 'op:absolute_name "::xmod::foo"))

      ;; Destroy
      (op:destroy sub-obj)
      (ensure-pattern (op:contents module :dk_all t) (sequence-pattern))
      (ensure (null (op:lookup_id repository sub-id)))))
  
  
  
  (define-test "Contained Move" ()
    (let ((container1 (op:create_module repository "IDL:mod1:1.0" "mod1" "1.0"))
          (container2 (op:create_module repository "IDL:mod2:1.0" "mod2" "1.0")))
      (let ((obj (op:create_native container1 "IDL:n:1.0" "n" "1.0")))
        (op:create_native container2 "IDL:n2:1.0" "n" "1.0")
        ;; The move operation atomically removes this object from its
        ;; current Container, and adds it to the Container specified
        ;; by new_container
        (op:move obj container2 "nn" "1.0")
        (ensure-repository "mod2::nn" 
                           (pattern 'identity obj 'op:defined_in container2))
        ;; must satisfy the following conditions:
        ;; - It must be in the same Repository. If it is not, then
        ;; BAD_PARAM exception is raised with minor code 4.
        (ensure-exception
         (op:move obj (make-instance 'repository) "foo" "1.0")
         CORBA:BAD_PARAM 'op:minor (std-minor 4))
        ;; - It must be capable of containing this object's type (see
        ;; Section 10.4.4, "Structure and Navigation of the Interface
        ;; Repository," on page 10-7). If it is not, then BAD_PARAM
        ;; exception is raised with minor code 4.
        (let ((struct (op:create_struct repository "IDL:s:1.0" "s" "1.0" nil)))
          (ensure-exception
           (op:move obj struct "nn" "1.0")
           CORBA:BAD_PARAM 'op:minor (std-minor 4)))
        ;; - It must not already contain an object with this object's
        ;; name (unless multiple versions are supported by the IR). If
        ;; this condition is not satisfied, then BAD_PARAM exception
        ;; is raised with minor code 3.
        (op:create_native container1 "IDL:n3:1.0" "n" "1.0")
        (ensure-exception
         (op:move obj  container1 "n" "1.0")
         CORBA:BAD_PARAM 'op:minor (std-minor 3)))))
  


  (define-test "Container"
    (let* ((module (op:create_module repository "my-module" "mod" "1.1"))
           (obj (op:create_enum module "IDL:mod/foo:1.0" "my-enum" "1.0" '("fie" "fum")))
           (obj2 (op:create_enum repository "IDL:foo:1.0" "foo" "1.0" '("fie" "fum")))
           (obj3 (op:create_alias module "IDL:mod/num:1.0" "num" "1.0"
                                  (op:get_primitive repository :pk_long))))
      (ensure-repository
       "mod" module   "foo" obj2   "mod::my-enum" obj   "::mod::my-enum" obj 
       "mod" (def-pattern :dk_module
               'identity (repository-pattern "my-enum" obj  
                                             "::mod::my-enum" obj "::foo" obj2 )))
      (ensure-equalp (op:lookup module "fie") nil)
      (ensure-pattern (op:describe_contents module :dk_Enum t -1)
                      (sequence-pattern (pattern 'op:kind :dk_enum
                                                 'op:value (pattern 'op:name "my-enum")
                                                 'op:contained_object obj)))
      (ensure-pattern (op:describe_contents module :dk_All t -1)
                      (sequence-pattern (pattern 'op:kind :dk_enum
                                                 'op:value (pattern 'op:name "my-enum")
                                                 'op:contained_object obj)
                                        (pattern 'op:kind :dk_alias
                                                 'op:value (pattern 'op:name "num")
                                                 'op:contained_object obj3)))
      (ensure-pattern (op:describe_contents module :dk_All t 1)
                      (sequence-pattern (pattern 'op:kind :dk_enum
                                                 'op:value (pattern 'op:name "my-enum")
                                                 'op:contained_object obj)))
      (op:destroy module)
      (ensure (null (op:lookup_id repository "IDL:mod/foo:1.0")))
      (ensure (null (op:lookup_id repository "IDL:mod/num:1.0")))))


  
  
  (define-test "StructDef" 
    (let* ((id "IDL:foo/Struct:1.1") (name "aStruct") (version "1.1")
           (members (list (CORBA:StructMember :name "aa"  :type_def a-string)
                          (CORBA:StructMember :name "bb"  :type_def a-ulong )))
           (obj (op:create_Struct repository id name version members)))
      (ensure-pattern obj (def-pattern :dk_Struct
                            'op:id id  'op:version version
                            'op:members (sequence-pattern (pattern 'op:name "aa"
                                                                   'op:type_def a-string
                                                                   'op:type corba:tc_string )
                                                          (pattern 'op:name "bb" ))
                            'op:type (pattern 'op:kind :tk_Struct
                                              'op:member_count (length members))))
      ;; Setting the members attribute also updates the type attribute. When setting the
      ;; members attribute, the type member of the StructMember structure should be set
      ;; to TC_void.
      (setf (op:members obj) (list (car members)))
      (ensure-pattern* (op:type obj)
                       'op:member_count 1 '(op:member_name * 0) "aa")
      ;; A StructDef used as a Container may only contain StructDef, UnionDef, or
      ;; EnumDef definitions.
      (op:create_enum obj "IDL:foo/Struct/aenum:1.0" "aenum" "1.0" '("A_A" "A_B"))
      (ensure-pattern obj (repository-pattern "aenum" (def-pattern :dk_enum)))
      (ensure-exception
       (op:create_alias obj "IDL:foo/Struct/all:1.0" "all" "1.0" a-ulong)
       CORBA:BAD_PARAM 'op:minor (std-minor 4))))
  
  
  (define-test "UnionDef"
    (let* ((id "IDL:foo/Union:1.1") (name "aunion") (version "1.1")
           (desc-type a-ulong)
           (members (list (CORBA:UnionMember :name "aa"  :label 1  :type_def a-string)
                          (CORBA:UnionMember :name "_def_"  :type_def a-ulong 
                                             :label (CORBA:Any :any-typecode CORBA:tc_octet
                                                               :any-value 0))
                          (CORBA:UnionMember :name "bb"  :label 2  :type_def a-ulong)))
           (obj (op:create_union repository id name version desc-type members)))
      (ensure-pattern obj (def-pattern :dk_union
                            'op:id id  'op:version version
                            'op:members (sequence-pattern (pattern 'op:name "aa"
                                                                   'op:label (pattern 'any-value 1)
                                                                   'op:type_def a-string
                                                                   'op:type corba:tc_string )
                                                          (pattern 'op:label (pattern 'any-typecode CORBA:tc_octet 'any-value 0)
                                                                   'op:type_def a-ulong )
                                                          (pattern 'op:name "bb" ))
                            'op:type (pattern 'op:kind :tk_union
                                              'op:member_count (length members)
                                              'op:default_index 1)
                            'op:discriminator_type (pattern 'op:kind :tk_ulong)))
      ;; update
      (setf (op:discriminator_type_def obj)
            (op:get_primitive repository :pk_ushort))
      (ensure-equalp (op:kind (op:discriminator_type obj))
                     :tk_ushort)
      ;; A UnionDef used as a Container may only contain StructDef, UnionDef, or
      ;; EnumDef definitions.
      (op:create_enum obj "IDL:foo/Union/aenum:1.0" "aenum" "1.0" '("A_A" "A_B"))
      (ensure-pattern obj (repository-pattern "aenum" (def-pattern :dk_enum)))
      (ensure-exception
       (op:create_alias obj "IDL:foo/Union/all:1.0" "all" "1.0" a-ulong)
       CORBA:BAD_PARAM 'op:minor (std-minor 4))))
  
  
  (define-test "EnumDef"
    (let ((obj (op:create_enum repository "IDL:foo:1.0" "foo" "1.0" '("fie" "fum"))))
      (ensure-eql (op:lookup repository "foo") obj)
      (let ((tc (op:type obj)))
        (ensure-equalp (op:kind tc) :tk_enum)
        (ensure-equalp (op:member_count tc) 2)
        (ensure-equalp (op:member_name tc 0) "fie"))
      (setf (op:members obj) '("a" "b" "c"))
      (let ((tc (op:type obj)))
        (ensure-equalp (op:member_count tc) 3))))
  
  
  (define-test "AliasDef"
    (let* ((name "Fie") (ver "1.1")
           (id (format nil "IDL:~A:~A" name ver))
           (alias (op:create_alias repository id name ver a-string)))
      (ensure-pattern 
       alias
       (def-pattern :dk_alias
         'op:name name 'op:version ver 'op:id id
         'op:absolute_name "::Fie"
         'op:type (pattern 'op:kind :tk_alias
                           'op:content_type CORBA:tc_string )
         'op:original_type_def a-string ))
      (ensure-pattern repository (repository-pattern name alias))
      (setf (op:original_type_def alias) a-ulong)
      (ensure-pattern* alias 'op:type (pattern 'op:content_type CORBA:tc_ulong))))
  
  
  (define-test "SequenceDef"
    (let ((obj (op:create_sequence repository 0 a-ulong)))
      (ensure-pattern obj (def-pattern :dk_sequence 
                            'op:bound 0 'op:element_type CORBA:tc_ulong
                            'op:type (create-sequence-tc 0 CORBA:tc_ulong)))
      ;; Write interface
      (setf (op:element_type_def obj) a-string)
      (ensure-typecode (op:element_type obj) :tk_string)
      (setf (op:bound obj) 10)
      (ensure-typecode (op:type obj) (create-sequence-tc 10 CORBA:tc_string))
      (op:destroy obj)))
  
  
  (define-test "ArrayDef"
    (let ((obj (op:create_array repository 10 a-string)))
      (ensure-equalp (op:def_kind obj) :dk_array)
      (ensure-equalp (op:length obj) 10)
      (ensure-equalp (op:kind (op:element_type obj)) :tk_string)
      ;; Write interface
      (setf (op:element_type_def obj) a-ulong)
      (ensure-equalp (op:kind (op:element_type obj)) :tk_ulong)
      (setf (op:length obj) 11)
      (ensure-typecode (op:type obj) (create-array-tc 11 corba:tc_ulong))))
  
  
  (define-test "ExceptionDef"
    (let* ((members (list (CORBA:StructMember :name "a"
                                              :type CORBA:tc_void :type_def a-string)
                          (CORBA:StructMember :name "b"
                                              :type CORBA:tc_void :type_def a-ulong)))
           (obj (op:create_exception repository "IDL:my/Exception:1.0" "Exception" "1.0" members)))
      (ensure-repository
       "Exception" (def-pattern :dk_exception
                     'identity obj
                     'op:name "Exception"
                     'op:id "IDL:my/Exception:1.0"
                     'op:type (pattern 'op:kind :tk_except 
                                       'op:member_count (length members))
                     'op:members (sequence-pattern (pattern 'op:type corba:tc_string)
                                                   (pattern 'op:type corba:tc_ulong))
                     'op:describe (struct-pattern
                                   'struct-class-name 'CORBA:Contained/Description
                                   'op:kind :dk_exception
                                   'op:value (struct-pattern
                                              'struct-class-name 'CORBA:ExceptionDescription
                                              'op:name (op:name obj)
                                              'op:id (op:id obj)))))
      ;; Write interface
      (setf (op:members obj) (cdr members))
      (ensure-equalp (length (op:members obj)) (1- (length members)))
      (ensure-equalp (op:member_count (op:type obj)) (1- (length members)))
      (ensure-equalp (op:kind (op:member_type (op:type obj) 0)) :tk_ulong)
      (ensure-eql (op:member_count (op:type obj)) (1- (length members)))
      ;; An ExceptionDef used as a Container may only contain StructDef, UnionDef, or
      ;; EnumDef definitions.
      (op:create_enum obj "IDL:my/Exception/aenum:1.0" "aenum" "1.0" '("A_A" "A_B"))
      (ensure-pattern obj (repository-pattern "aenum" (def-pattern :dk_enum)))
      (ensure-exception
       (op:create_alias obj "IDL:my/Exception/all:1.0" "all" "1.0" a-ulong)
       CORBA:BAD_PARAM 'op:minor (std-minor 4))))
  

  (define-test "AttributeDef"
    (let* ((idef (op:create_interface repository "IDL:my/Interface:1.1" "Interface" "1.1" '()))
           (obj (op:create_attribute idef "IDL:my/Att:1.1" "Att" "1.1"
                                     a-string :attr_readonly)))
      (ensure-repository
       "Interface::Att" (def-pattern :dk_attribute
                          'op:name "Att"  'op:mode :attr_readonly
                          'op:type_def a-string  'op:type corba:tc_string
                          'op:describe (struct-pattern 'op:kind :dk_attribute 
                                                       'op:value (struct-pattern 'struct-class-name'CORBA:AttributeDescription))))
      (setf (op:type_def obj) a-ulong)
      (ensure-equalp (op:kind (op:type obj)) :tk_ulong)))


  (define-test "OperationDef"
    (let* ((idef (op:create_interface repository "IDL:my/Interface:1.1" "Interface" "1.1" '()))
           (id "IDL:my/Interface/Op1:1.1")
           (name "Op1")
           (version "1.1")
           (result a-ulong)
           (mode :OP_NORMAL)
           (params (list (CORBA:ParameterDescription
                          :name "a"
                          :type CORBA:tc_void
                          :type_def a-string
                          :mode :param_in)))
           (exceptions '())
           (contexts '())
           (obj (op:create_operation idef id name version result mode params exceptions contexts)))
      (ensure-repository
       "Interface::Op1" (def-pattern :dk_operation
                          'identity obj
                          'op:result corba:tc_ulong
                          'op:params (sequence-pattern (pattern 'op:name "a"
                                                           'op:type corba:tc_string))
                          'op:describe (struct-pattern
                                        'op:kind :dk_operation
                                        'op:value (struct-pattern
                                                   'struct-class-name 'CORBA:OperationDescription
                                                   'op:name name 'op:id id))) )
      ;; Write interface
      (setf (op:result_def obj) a-string)
      (ensure-equalp (op:kind (op:result obj)) :tk_string)
      (setf (op:mode obj) :op_normal)
      (ensure-exception
       (setf (op:mode obj) :op_oneway)
       CORBA:BAD_PARAM  'op:minor (std-minor 31))))


  (define-test "InterfaceDef"
    (let* ((id "IDL:my/Interface:1.1") (id2 "IDL:my/Interface2:1.0")
           (name "Interface")
           (version "1.1")
           (obj (op:create_interface repository id name version '()))
           (obj2 (op:create_interface repository id2 "Interface2" "1.0" '())))
      (ensure (op:is_a obj id) "isa Self")
      (ensure (op:is_a obj "IDL:omg.org/CORBA/Object:1.0") "isa Object")
      (ensure (not (op:is_a obj id2)) "not yet isa base")
      (op:create_attribute obj "IDL:my/a:1.0" "a" "1.0" a-string :attr_normal)
      (ensure-exception 
       (op:create_operation obj "IDL:my/a:1.0" "a" "1.0" a-string :op_normal nil nil nil)
       corba:bad_param 'op:minor (std-minor 3))
      (setf (op:base_interfaces obj) (list obj2))
      (ensure (op:is_a obj id2) "isa base")
      (ensure-pattern* 
       obj
       'op:describe (pattern 'op:value (struct-pattern
                                        'struct-class-name 'CORBA:InterfaceDescription
                                        'op:name name 'op:id id 'op:version version
                                        'op:base_interfaces (sequence-pattern id2)))
       'op:base_interfaces (sequence-pattern (def-pattern :dk_interface 'op:id id2))
       'op:describe_interface (struct-pattern
                               'struct-class-name 'CORBA:InterfaceDef/FullInterfaceDescription
                               'op:operations (sequence-pattern)
                               'op:attributes (sequence-pattern
                                               (struct-pattern 
                                                'struct-class-name 'CORBA:AttributeDescription
                                                'op:name "a"))
                               'op:type (pattern 'op:kind :tk_objref)))
      (setf (op:base_interfaces obj) nil)
      (op:create_attribute obj2 "IDL:my2/a:1.0" "a" "1.0" a-string :attr_normal)
      (ensure-exception 
       (setf (op:base_interfaces obj) (list obj2))
       corba:bad_param 'op:minor (std-minor 5))))
  



  (define-test "PrimitiveDef"
    (loop for kind in '(:PK_VOID :PK_SHORT :PK_LONG :PK_USHORT :PK_ULONG :PK_FLOAT :PK_DOUBLE
                        :PK_BOOLEAN :PK_CHAR :PK_OCTET :PK_ANY :PK_TYPECODE ;:PK_PRINCIPAL
                        :PK_STRING :PK_OBJREF :PK_LONGLONG :PK_ULONGLONG :PK_LONGDOUBLE 
                        :PK_WCHAR :PK_WSTRING :PK_VALUE_BASE)
          do (with-sub-test (kind)
               (ensure-pattern* (op:get_primitive repository kind)
                                'op:def_kind :dk_primitive
                                'op:kind kind
                                'op:type (isa 'CORBA:TypeCode)))))



  (define-test "StringDef"
    (ensure-pattern* (op:create_string repository 10)
                     'op:def_kind :dk_string
                     'op:bound 10
                     'op:type (make-typecode :tk_string 10)))

  (define-test "WStringDef"
    (ensure-pattern* (op:create_wstring repository 10)
                     'op:def_kind :dk_wstring
                     'op:bound 10
                     'op:type (create-wstring-tc 10)))
  

  (define-test "FixedDef"
    (let ((obj (op:create_fixed repository 10 2)))
      (ensure-equalp (op:digits obj) 10)
      (ensure-equalp (op:scale obj) 2)
      (ensure-typecode (op:type obj) :tk_fixed)
      (setf (op:scale obj) 3)
      (ensure-equalp (op:fixed_scale (op:type obj)) 3)))

  (define-test "ValueBox"
    (let* ((type a-ulong)
           (id "IDL:my/ValueBox:1.1")
           (name "ValueBox")
           (version "1.1")
           (obj (op:create_value_box repository id name version type)))
      (ensure-pattern obj
                      (def-pattern :dk_valuebox
                        'op:id id 'op:name name 'op:version version
                        'op:original_type_def type
                        'op:type (pattern 'op:kind :tk_value_box
                                          'op:id id)))))


  (define-test "Native"
    (let* ((id "IDL:my/servant:1.1")
           (name "servant")
           (version "1.1")
           (obj (op:create_native repository id name version)))
      (ensure-pattern obj
                      (def-pattern :dk_native
                        'op:id id 'op:name name 'op:version version
                        'op:type (pattern 'op:kind :tk_native
                                          'op:id id)))))


  (define-test "AbstractInterfaceDef"
    (let* ((id "IDL:my/Interface:1.1")
           (name "Interface")
           (version "1.1")
           (obj (op:create_abstract_interface repository id name version '())))
      (ensure (op:is_a obj id) "isa Self")
      (ensure (op:is_a obj "IDL:omg.org/CORBA/AbstractBase:1.0") "isa AbstractBase")
      (op:create_attribute obj "IDL:my/a:1.0" "a" "1.0" a-string :attr_normal)))


  (define-test "LocalInterfaceDef"
    (let* ((id "IDL:my/Interface:1.1")
           (name "Interface")
           (version "1.1")
           (obj (op:create_local_interface repository id name version '())))
      (ensure (op:is_a obj id) "isa Self")
      (ensure (op:is_a obj "IDL:omg.org/CORBA/LocalBase:1.0") "isa LocalBase"))
    ;; Setting the inherited base_interfaces attribute causes a
    ;; BAD_PARAM exception with standard minor code 5 to be raised if
    ;; the name attribute of any object contained by this
    ;; LocalInterfaceDef conflicts with the name attribute of any
    ;; object contained by any of the specified base InterfaceDefs
    ;; (local or otherwise).
    )
    


  (define-test "ValueDef"
    (let* ((name "val") (ver "1.0") (id "IDL:my/val:1.0")
           (name2 "val2") (id2 "IDL:my/val2:1.0")
           (init (list (CORBA:Initializer 
                        :name "make-val"
                        :members (list (CORBA:StructMember :name "name" :type_def a-string)
                                       (CORBA:StructMember :name "size" :type_def a-ulong)))))
           (val (op:create_value repository id name ver nil nil nil nil nil nil init))
           (val2 (op:create_value repository id2 name2 ver nil nil val nil nil nil nil))
           (vm1 (op:create_value_member val "IDL:my/val/a:1.0" "a" "1.0" a-ulong corba:private_member))
           (vm2 (op:create_value_member val2 "IDL:my/val/b:1.0" "b" "1.0" val corba:public_member))
           (a1  (op:create_attribute val "IDL:my/val/at:1.0" "at" ver a-string :attr_readonly))
           (op1 (op:create_operation val "IDL:my/val/op1:1.0" "op1" ver a-ulong :op_normal nil nil nil)))
      (ensure-repository 
       "val" (def-pattern :dk_value  'identity val
               'op:id id 'op:name name 'op:version ver
               'op:is_abstract nil 'op:is_custom nil 'op:is_truncatable nil
               'op:supported_interfaces (sequence-pattern)
               'op:initializers (sequence-pattern (struct-pattern 'op:name "make-val"
                                                                  'op:members (sequence-pattern
                                                                               (struct-pattern 'op:name "name")
                                                                               (struct-pattern 'op:name "size"))))
               'op:base_value nil
               'op:abstract_base_values (sequence-pattern)
               'op:type (pattern 'op:kind :tk_value 'op:member_count 1
                                 '(op:member_name * 0) "a"
                                 '(op:member_visibility * 0) CORBA:private_member)
               'op:describe_value (struct-pattern
                                   'struct-class-name 'CORBA:ValueDef/FullValueDescription
                                   'op:id id 'op:name name 'op:version ver
                                   'op:is_abstract nil 'op:is_custom nil 'op:is_truncatable nil
                                   'op:operations (sequence-pattern
                                                   (struct-pattern 'op:name "op1"))
                                   'op:initializers (sequence-pattern (car init)))
               'op:describe (pattern
                             'op:value (struct-pattern
                                        'struct-class-name 'CORBA:ValueDescription
                                        'op:id id 'op:name name 'op:version ver
                                        'op:is_abstract nil 'op:is_custom nil 'op:is_truncatable nil )))
       
       "val2" (def-pattern :dk_value 'identity val2
                'op:supported_interfaces (sequence-pattern)
                'op:base_value val
                'op:type (pattern 'op:kind :tk_value 'op:member_count 1
                                  '(op:member_name * 0) "b"
                                  '(op:member_visibility * 0) CORBA:public_member)))
      (ensure-pattern
       (op:contents val :dk_attribute nil)
       (sequence-pattern a1))
      (ensure-pattern
       (op:contents val :dk_valuemember nil)
       (sequence-pattern vm1))
      (ensure-pattern
       (op:contents val2 :dk_valuemember t)
       (sequence-pattern vm2))
      (ensure-pattern
       (op:contents val2 :dk_valuemember nil)
       (sequence-pattern vm1 vm2))
      (ensure-pattern
       (op:contents val2 :dk_operation nil)
       (sequence-pattern op1))
      
      ;; Setting supported interfaces
      (let ((i1 (op:create_interface repository "IDL:my/Interface:1.1" "Interface" "1.1" '()))
            (i2 (omg.org/features:create_abstract_interface repository "IDL:my/Interface2:1.1" "IDL:my/Interface2:1.1" "Interface2" '()))
            (i3 (op:create_interface repository "IDL:my/Interface3:1.0" "Interface2" "1.0" '())))
        (setf (op:supported_interfaces val) (list i1 i2))
        ;; Only one non-abstract interface
        (ensure-exception
         (setf (op:supported_interfaces val) (list i1 i2 i3))
         corba:bad_param 'op:minor (std-minor 12))
        (ensure-exception 
         (op:create_value repository "IDL:my/ValX1:1.0" "ValX1"
                          ver nil nil nil nil nil (list i1 i2 i3) init)
         corba:bad_param 'op:minor (std-minor 12))
        ;; Can't have same name in ValueDef as in some supported interface
        (setf (op:supported_interfaces val) (list))
        (op:create_attribute i1 "IDL:my/a:1.0" "a" "1.0" a-string :attr_normal)
        (ensure-exception 
         (setf (op:supported_interfaces val) (list i1))
         corba:bad_param 'op:minor (std-minor 5)))

      ;; Creating ValueMember
      (ensure-pattern* vm1 'op:id "IDL:my/val/a:1.0" 'op:name "a"
                       'op:type_def a-ulong 'op:type CORBA:Tc_Ulong
                       'op:defined_in val)
      (ensure-exception                 ; same id
       (op:create_value_member val "IDL:my/val/a:1.0" "a2" "1.0" a-ulong corba:private_member)
       CORBA:BAD_PARAM 'op:minor (std-minor 2))
      (ensure-exception                 ; same name
       (op:create_value_member val "IDL:my/val/a2:1.0" "a" "1.0" a-ulong corba:private_member)
       CORBA:BAD_PARAM 'op:minor (std-minor 3))
      ;; same name as inherited, OK ?
      (op:create_value_member val2 "IDL:my/val/a2:1.0" "a" "1.0" a-ulong corba:private_member)

      ;; Create attribute
      (ensure-pattern* a1 'op:id "IDL:my/val/at:1.0" 'op:name "at" 'op:version ver
                       'op:type_def a-string 'op:mode :attr_readonly
                       'op:type CORBA:tc_string 'op:defined_in val)
      (ensure-exception                 ; same name
       (op:create_attribute val "IDL:my/val/at2:1.0" "at" ver a-string :attr_readonly)
       CORBA:BAD_PARAM 'op:minor (std-minor 3))
      ;; same name as inherited, OK ?
      (op:create_attribute val2 "IDL:my/val/at2:1.0" "at" ver a-string :attr_readonly)

      ;; Create Operation
      (ensure-pattern* op1 'op:id "IDL:my/val/op1:1.0" 'op:name "op1" 'op:version ver
                       'op:result_def a-ulong 'op:mode :op_normal
                       'op:params (sequence-pattern) 'op:exceptions (sequence-pattern)
                       'op:contexts (sequence-pattern)
                       'op:result CORBA:Tc_ulong 'op:defined_in val )
      (ensure-exception                 ; same name
       (op:create_operation val "IDL:my/val/op1x:1.0" "op1" ver a-ulong :op_normal nil nil nil)
       CORBA:BAD_PARAM 'op:minor (std-minor 3))
      ;; same name as inherited, OK ?
      (op:create_operation val2 "IDL:my/val/op1x:1.0" "op1" ver a-ulong :op_normal nil nil nil)

      ;; ValueDef cannot contain ModuleDef, InterfaceDef, AbstractInterfaceDef..
      ;; A ValueDef used as a Container may only contain TypedefDef, (including
      ;; definitions derived from TypedefDef), ConstantDef, and ExceptionDef definitions.
      (ensure-exception
       (op:create_module val "my-module" "mod" "1.1")
       CORBA:BAD_PARAM 'op:minor (std-minor 4))
      (ensure-exception
       (op:create_interface val "IDL:my/InterfaceXX:1.1" "InterfaceXX" "1.1" '())
       CORBA:BAD_PARAM 'op:minor (std-minor 4))
      

      #| end valuedef test |#))
  

)
