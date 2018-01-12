(in-package :clorb) 


(define-test-suite "Typecodes"
  (variables )

  (define-test "constants"
    (ensure-equalp (op:name CORBA:tc_object) "Object")
    (ensure-equalp (op:id CORBA:tc_object) "IDL:omg.org/CORBA/Object:1.0")
    (ensure-equalp (op:length CORBA:tc_string) 0))

  (define-test "Enum"
    (let* ((id "IDL:enum:1.0")
           (name "enumen")
           (members '("fie" "foe"))
           (tc (create-enum-tc id name members)))
      (ensure-pattern tc (pattern 'op:kind :tk_enum  'op:id id  'op:name name
                                  'op:member_count (length members)))
      (loop for m in members for i from 0 do
            (ensure-equalp (op:member_name tc i) m))))

  (define-test "Alias"
    (let* ((id "IDL:alias:1.0") (name "alias"))
      (ensure-pattern (create-alias-tc id name corba:tc_long)
                      (pattern 'op:kind :tk_alias 'op:id id 'op:name name
                               'op:content_type corba:tc_long))))

  (define-test "Sequence"
    (let* ((el-type corba:tc_string)
           (seq-tc (create-sequence-tc 100 el-type))) 
      (ensure-pattern* seq-tc  'op:kind :tk_sequence  'op:length 100
                       'op:content_type el-type)
      (ensure (not (op:equal el-type seq-tc)))))

  (define-test "Union"
    (let* ((d-type corba:tc_ushort)
           (e1-type corba:tc_ulong)
           (e2-type corba:tc_string)
           (e3-type (create-sequence-tc 0 corba:tc_octet))
           (members `((10 "e1" ,e1-type)
                      (20 "e2" ,e2-type)
                      (33 "e3" ,e3-type) ))
           (id "IDL:my/Union:1.1")
           (name "Union")
           (tc (make-typecode :tk_union id name d-type 0 members)))
      (ensure-pattern* tc  'op:kind :tk_union  'op:id id  'op:name name
                       'op:member_count 3  'op:discriminator_type d-type  
                       'op:default_index 0 )
      (loop for m in members and i from 0 do
            (ensure-equalp (op:member_name tc i) (second m))
            (ensure-equalp (op:member_type tc i) (third m))
            (ensure-equalp (op:member_label tc i) (first m)))
      (ensure (op:equal tc (make-typecode :tk_union id name d-type 0 (copy-list members))))
      (ensure (not (op:equal tc (make-typecode :tk_union id name d-type 0 (cdr members)))))))

  (define-test "Recursive"
    (let ((sym (gensym)))
      (set-symbol-typecode sym 
                           (lambda () 
                             (create-struct-tc
                              "IDL:Recursive_1/Node:1.0"
                              "Node"
                              (list (list "children"
                                          (create-sequence-tc 0 (symbol-typecode sym)))))))
      (let ((tc (symbol-typecode sym)))
        (ensure-typep tc 'corba:typecode)
        (ensure-eql (typecode-kind tc) :tk_struct)
        (ensure-eql (op:content_type (op:member_type tc 0))
                    tc))))

  (define-test "Fixed"
    (ensure-pattern*
     (create-fixed-tc 10 2)
     'op:kind :tk_fixed
     'op:fixed_digits 10 'op:fixed_scale 2))

  (define-test "Exception"
    (let* ((members (list (list "foo" CORBA:tc_string)
                          (list "bar" CORBA:tc_ulong))))
      (ensure-pattern*
       (create-exception-tc "IDL:exc:1.0" "exc" members)
       'op:kind :tk_except
       'op:id "IDL:exc:1.0" 'op:name "exc"
       'tc-members (apply #'vector members)
       'op:member_count (length members)
       '(op:member_name * 0) "foo"
       '(op:member_type * 0) CORBA:tc_string
       '(op:member_name * 1) "bar"
       '(op:member_type * 1) CORBA:tc_ulong)))

  (define-test "Value Box"
    (let* ((id "IDL:Foo/Box:1.0"))
      (ensure-pattern* 
       (create-value-box-tc id "Box" corba:tc_string)
       'identity (isa 'corba:typecode)  'op:kind :tk_value_box 
       'op:id id 'op:name "Box"
       'op:content_type corba:tc_string) ))

  (define-test "Native"
    (let* ((id "IDL:CORBA/nat:1.0")
           (tc (create-native-tc id "nat")))
      (ensure-typep tc 'corba:typecode)
      (ensure-pattern* tc
                       'op:kind :tk_native
                       'op:id id 'op:name "nat")))

  (define-test "create-abstract-interface-tc"
    (let* ((id "IDL:x:1.0") (name "y")
           (tc (create-abstract-interface-tc id name)))
      (ensure-typep tc 'corba:typecode)
      (ensure-pattern* tc
                       'op:kind :tk_abstract_interface
                       'op:id id 'op:name name)))

  (define-test "create-local-interface-tc"
    (let* ((id "IDL:x:1.0") (name "y")
           (tc (create-local-interface-tc id name)))
      (ensure-typep tc 'corba:typecode)
      (ensure-pattern* tc
                       'op:kind :tk_local_interface
                       'op:id id 'op:name name)))
  
  (define-test "create-value-tc"
    (let* ((id "IDL:x:1.0") (name "y")
           (tc (create-value-tc id name 0 nil
                                `(("foo" ,CORBA:tc_long 1)
                                  ("bar" ,CORBA:tc_string 0)))))
      (ensure-typep tc 'corba:typecode)
      (ensure-pattern* tc
                       'op:kind :tk_value
                       'op:id id 'op:name name
                       'op:member_count 2
                       'op:type_modifier 0
                       'op:concrete_base_type corba:tc_null)
      (ensure-equalp (op:member_name tc 0) "foo")
      (ensure-equalp (op:member_name tc 1) "bar")
      (ensure-equalp (op:member_visibility tc 0) 1)
      (ensure-equalp (op:member_visibility tc 1) 0)))


  (define-test "TypeCodeFactory"
    (let ((factory (make-instance 'CORBA:TYPECODEFACTORY)))
      (ensure-pattern (op:create_array_tc factory 2 CORBA:tc_short)
                      (pattern 'op:kind :tk_array 'op:length 2 'op:content_type CORBA:tc_short))
      (ensure-pattern (op:create_sequence_tc factory 0 CORBA:tc_short)
                      (pattern 'op:kind :tk_sequence))
      (ensure-pattern (op:create_fixed_tc factory 10 2)
                      (pattern 'op:kind :tk_fixed 'op:fixed_digits 10 'op:fixed_scale 2))
      (ensure-pattern (op:create_wstring_tc factory 0)
                      (pattern 'op:kind :tk_wstring 'op:length 0))
      (ensure-pattern (op:create_string_tc factory 12)
                      (pattern 'op:kind :tk_string 'op:length 12))
      (ensure-pattern (op:create_interface_tc factory "IDL:I:1.0" "I")
                      (pattern 'op:kind :tk_objref 'op:name "I"))
      (ensure-pattern (op:create_exception_tc factory "IDL:e:1.0" "e" nil)
                      (pattern 'op:kind :tk_except 'op:name "e"))
      (ensure-pattern (op:create_alias_tc factory "IDL:a:1.0" "a" CORBA:tc_ushort)
                      (pattern 'op:kind :tk_alias 'op:id "IDL:a:1.0" 'op:name "a" ))
      (ensure-pattern (op:create_enum_tc factory "IDL:e:1.0" "e" '("A" "B"))
                      (pattern 'op:kind :tk_enum 'op:name "e" 'op:member_count 2))
      (ensure-pattern (op:create_union_tc factory "IDL:u:1.0" "u" corba:tc_boolean
                                          (list (corba:unionmember
                                                 :name "a" :type corba:tc_string
                                                 :label (any :any-value t :any-typecode corba:tc_boolean))
                                                (corba:unionmember
                                                 :name "b" :type corba:tc_long
                                                 :label (any :any-value nil :any-typecode corba:tc_boolean))))
                      (pattern 'op:kind :tk_union 'op:member_count 2))
      (ensure-pattern (op:create_struct_tc factory "IDL:s:1.0" "s"
                                           (list (corba:structmember :name "a" :type  CORBA:tc_long)
                                                 (corba:structmember :name "b" :type CORBA:tc_long)))
                      (pattern 'op:kind :tk_struct 'op:name "s" 
                               'op:member_count 2))
      (ensure-pattern* (op:create_value_box_tc factory "IDL:Foo/Box:1.0" "Box" corba:tc_string)
                       'op:kind :tk_value_box
                       'op:id "IDL:Foo/Box:1.0" 'op:name "Box"
                       'op:content_type corba:tc_string)

      (ensure-pattern* (op:create_native_tc factory "IDL:CORBA/nat:1.0" "nat")
                       'op:kind :tk_native
                       'op:id "IDL:CORBA/nat:1.0" 'op:name "nat")

      (ensure-pattern* (op:create_abstract_interface_tc factory "IDL:CORBA/abs:1.0" "abs")
                       'op:kind :tk_abstract_interface
                       'op:id "IDL:CORBA/abs:1.0" 'op:name "abs")

      (ensure-pattern* (op:create_local_interface_tc factory "IDL:CORBA/local:1.0" "local")
                       'op:kind :tk_local_interface
                       'op:id "IDL:CORBA/local:1.0" 'op:name "local")

      (ensure-pattern* (op:create_value_tc factory "IDL:CORBA/vt:1.0" "vt" 
                                           corba:vm_none
                                           nil
                                           (list (corba:valuemember
                                                  :name "m1" 
                                                  :id "IDL:Hoopp:1.0"
                                                  :version "1.0"
                                                  :type CORBA:tc_long
                                                  :type_def nil
                                                  :access corba:public_member)))
                       'op:kind :tk_value)

      (let ((recursive_tc (op:create_recursive_tc factory "IDL:V:1.0")))
        (let ((tc (op:create_struct_tc factory "IDL:V:1.0" "V" 
                                       (list (CORBA:StructMember
                                              :name "member"
                                              :type recursive_tc)))))
          (ensure-typecode recursive_tc tc)))))


  (define-test "Create name/id error checking"
    (let ((factory (make-instance 'CORBA:TYPECODEFACTORY)))
      ;; valid name and id
      (dolist (case `((op:create_interface_tc factory "IDL:I:1.0" "I")
                      (op:create_exception_tc factory "IDL:e:1.0" "e" nil)
                      (op:create_alias_tc factory "IDL:a:1.0" "a" ,CORBA:tc_ushort)
                      (op:create_enum_tc factory "IDL:e:1.0" "e" ("A" "B"))
                      (op:create_union_tc factory "IDL:u:1.0" "u" ,corba:tc_boolean nil)
                      (op:create_struct_tc factory "IDL:s:1.0" "s" nil)
                      (op:create_value_box_tc factory "IDL:Foo/Box:1.0" "Box" ,corba:tc_string)
                      (op:create_native_tc factory "IDL:CORBA/nat:1.0" "nat")
                      (op:create_abstract_interface_tc factory "IDL:CORBA/abs:1.0" "abs")
                      (op:create_local_interface_tc factory "IDL:CORBA/local:1.0" "local")
                      (op:create_value_tc factory "IDL:CORBA/vt:1.0" "vt" ,corba:vm_none nil nil)))
        (let ((op (car case)))
          (with-sub-test ((symbol-name op))
            (apply op factory (cddr case))
            (ensure-exception 
             (apply op factory "IDL:x:1.0" "not valid" (cddddr case))
             CORBA:BAD_PARAM 'op:minor (std-minor 15))
            (ensure-exception
             (apply op factory "bad id" (cdddr case))
             CORBA:BAD_PARAM 'op:minor (std-minor 16)))))))


  (define-test "Create tc member name error checking"
    (let ((factory (make-instance 'CORBA:TYPECODEFACTORY)))
      (macrolet ((check (form) `(ensure-exception ,form CORBA:BAD_PARAM 'op:minor ,(std-minor 17))))
        (macrolet 
          ((check-names (n1 n2)
             `(progn
                (check (op:create_struct_tc factory "IDL:s:1.0" "s"
                                            (list (CORBA:StructMember 
                                                   :name ,n1 :type corba:tc_long :type_def nil)
                                                  (CORBA:StructMember 
                                                   :name ,n2 :type corba:tc_long :type_def nil))))
                (check (op:create_exception_tc factory "IDL:s:1.0" "s"
                                               (list (CORBA:StructMember 
                                                      :name ,n1 :type corba:tc_long :type_def nil)
                                                     (CORBA:StructMember 
                                                      :name ,n2 :type corba:tc_long :type_def nil))))
                (check (op:create_enum_tc factory "IDL:e:1.0" "e" '(,n1 ,n2)))
                (check (op:create_union_tc factory "IDL:u:1.0" "u" corba:tc_boolean
                                           (list (corba:unionmember
                                                  :name ,n1 :type corba:tc_string
                                                  :label (any :any-value t :any-typecode corba:tc_boolean))
                                                 (corba:unionmember
                                                  :name ,n2 :type corba:tc_long
                                                  :label (any :any-value nil :any-typecode corba:tc_boolean)))))
                (check (op:create_value_tc factory "IDL:CORBA/vt:1.0" "vt" 
                                           corba:vm_none nil
                                           (list (corba:valuemember
                                                  :name ,n1 :id "IDL:Hoopp1:1.0" :version "1.0"
                                                  :type CORBA:tc_long :type_def nil
                                                  :access corba:public_member)
                                                 (corba:valuemember
                                                  :name ,n2 :id "IDL:Hoopp2:1.0" :version "1.0"
                                                  :type CORBA:tc_long :type_def nil
                                                  :access corba:public_member)))))))
          (with-sub-test ("duplicated names")
            (check-names "foo" "foo"))
          (with-sub-test ("invalid name") 
            (check-names "12f" "foo"))))))
        

  (define-test "Create tc content type error checking"
    (let ((factory (make-instance 'CORBA:TYPECODEFACTORY)))
      (macrolet ((check (form)
                   `(ensure-exception ,form
                                      CORBA:BAD_TYPECODE 'op:minor ,(std-minor 2))))
        (dolist (case `(("tk_null" ,corba:tc_null)
                        ("tk_void" ,corba:tc_void)
                        ("tk_except" ,(create-exception-tc "IDL:foo:1.0" "foo" nil))))
          (let ((name (car case)) (type (cadr case)))
            (with-sub-test (name)
              (check (op:create_alias_tc factory "IDL:a:1.0" "a" type))
              (check (op:create_struct_tc factory "IDL:s:1.0" "s"
                                          (list (CORBA:StructMember
                                                 :name "a" :type type :type_def nil))))
              (check (op:create_exception_tc factory "IDL:s:1.0" "s"
                                             (list (CORBA:StructMember
                                                    :name "a" :type type :type_def nil))))
              (check (op:create_value_box_tc factory "IDL:a:1.0" "a" type))
              (check (op:create_array_tc factory 2 type))
              (check (op:create_sequence_tc factory 0 type))
              (check (op:create_union_tc factory "IDL:u:1.0" "u" corba:tc_boolean
                                         (list (corba:unionmember
                                                :name "a" :type type :label (any :any-value t :any-typecode corba:tc_boolean)))))
              (check (op:create_value_tc factory "IDL:CORBA/vt:1.0" "vt" 
                                         corba:vm_none nil
                                         (list (corba:valuemember
                                                :name "m1" :id "IDL:Hoopp:1.0" :version "1.0"
                                                :type type :type_def nil
                                                :access corba:public_member))))))))))


  (define-test "get_compact_typecode"
    (let ((tc (create-sequence-tc
               12 (create-alias-tc "IDL:alias:1.0" "alias"
                                   (create-struct-tc
                                    "IDL:struct:1.0" "struct"
                                    (list (list "foo" CORBA:tc_long))) ))))
      (let ((ctc (op:get_compact_typecode tc)))
        (ensure-pattern*
         ctc
         'op:kind :tk_sequence
         'op:length 12
         'op:content_type
         (pattern 'op:kind :tk_alias
                  'op:id (isa 'string)
                  'op:id "IDL:alias:1.0"
                  'op:name (isa 'string)
                  'op:name ""
                  'op:content_type
                  (pattern 'op:kind :tk_struct
                           'op:id "IDL:struct:1.0"
                           'op:name ""
                           '(op:member_name * 0) ""
                           '(op:member_type * 0) CORBA:tc_long ))) ))
    (let ((tc (create-value-tc "xx" "foo" CORBA:VM_NONE nil nil)))
      (ensure-pattern* (op:get_compact_typecode tc)
                       'op:id "xx" 'op:name ""
                       'op:type_modifier CORBA:VM_NONE
                       'op:concrete_base_type corba:tc_null
                       'op:member_count 0)))


  (define-test "equivalent"
    (ensure (op:equivalent CORBA:tc_ulong (make-typecode :tk_ulong)))
    (ensure (op:equivalent CORBA:tc_string (create-string-tc 0)))
    (ensure (op:equivalent (create-fixed-tc 12 3) (create-fixed-tc 12 3)))
    (ensure (not (op:equivalent CORBA:tc_ulong CORBA:tc_short)))
    (ensure (not (op:equivalent (create-fixed-tc 12 3) (create-fixed-tc 12 -3))))
    (ensure (op:equivalent CORBA:tc_ulong
                           (create-alias-tc "" "" CORBA:tc_ulong)))
    (ensure (op:equivalent (create-alias-tc "" "" CORBA:tc_ulong)
                           CORBA:tc_ulong))
    (ensure (not (op:equivalent CORBA:tc_ulong 
                                (create-alias-tc "" "" CORBA:tc_short))))
    (let* ((id "IDL:test/value:1.0") 
           (members (list (list "a" CORBA:tc_long 0) (list "b" CORBA:tc_string 1)))
           (v1 (create-value-tc id "foo" CORBA:VM_NONE nil members))
           (v2 (create-value-tc "" "foo" CORBA:VM_TRUNCATABLE v1 nil)))
      (ensure (op:equivalent v1 (op:get_compact_typecode v1)))
      (ensure (op:equivalent v1 (create-value-tc id "bar" CORBA:VM_NONE nil nil)))
      (ensure (not (op:equivalent v1 (create-value-tc "x" "foo" CORBA:VM_NONE nil members))))
      (ensure (op:equivalent v2 (create-value-tc "" "fum" CORBA:VM_TRUNCATABLE v1 nil)))
      (ensure (not (op:equivalent v2 (create-value-tc "" "foo" CORBA:VM_NONE v1 nil))))))
      

  #|end-suite|#)
