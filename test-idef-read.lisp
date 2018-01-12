(in-package :clorb)

(defmacro test-idef-read (idef &rest pattern)
  `(ensure-pattern
    (let ((repository (make-instance 'repository)))
      (idef-read ',idef repository)
      repository)
    (repository-pattern ,@pattern)))


(define-test-suite "IDEF Read Test"
  (variables 
    (r (make-instance 'repository)))

  (define-test "Read Module" 
    (idef-read '((define-module "Foo" ())) r)
    (let ((m (op:lookup r "Foo")))
      (ensure m "lookup name")
      (ensure-equalp (op:def_kind m) :dk_module)))
  
  (define-test "Read Interface 1" 
    (idef-read '((define-interface "Foo" ())) r)
    (let ((m (op:lookup r "Foo")))
      (ensure m "lookup name")
      (ensure-equalp (op:def_kind m) :dk_interface)
      (ensure-equalp (op:id m) "IDL:Foo:1.0")))
  
  (define-test "Read Interface 2" 
    (idef-read '((define-module "Foo" ()
                   (define-interface "Bar" ())
                   (define-interface "F" ()
                     (define-exception "e" ()))
                   (define-interface "Bar" (:bases ("F"))
                     (define-operation "op" ()
                       :result-type void
                       :exceptions ("e")))))
               r)
    (ensure-pattern 
     r
     (repository-pattern
      "Foo" (def-pattern :dk_module)
      "Foo::Bar" (def-pattern :dk_interface
              'op:id "IDL:Foo/Bar:1.0"
              'op:type (pattern 'op:kind :tk_objref))
      "Foo::Bar::op" (def-pattern :dk_operation
                       'op:exceptions (sequence-pattern (pattern 'op:name "e"))))))

  (define-test "Read Interface 3"
    (idef-read '((define-interface "A" (:bases ("B"))
                   (define-type "x" "y"))
                 (define-interface "B" ()
                   (define-type "y" long))
                 (define-interface "C" (:bases ("A"))
                   (define-type "z" "x")))
               r)
    (ensure-pattern r (repository-pattern 
                       "A::x" (def-pattern :dk_alias)
                       "C::z" (def-pattern :dk_alias))))

  (define-test "Read Operation" 
    (idef-read '((define-module "Foo" ()
                     (define-interface "Bar" ()
                       (define-operation "greet" ((:param_in level long))
                         :result-type string)
                       (define-operation "note" ((:param_in level long))
                         :result-type void
                         :mode :op_oneway))))
                 r)
    (let* ((m (op:lookup r "Foo"))
           (i (op:lookup m "Bar"))
           (o (op:lookup i "greet"))
           (n (op:lookup i "note")))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind o) :dk_operation)
      (ensure-equalp (op:kind (op:result o)) :tk_string)
      (ensure-equalp (op:mode o) :op_normal)
      (ensure-equalp (op:mode n) :op_oneway)))

  (define-test "Read Type" 
    (idef-read '((define-type "Foo" short)
                 (define-type "s0" string)
                 (define-type "s10" (string 10))
                 (define-type "f10_2" (fixed 10 2)))
                 r)
    (let* ((o (op:lookup r "Foo")))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind o) :dk_alias)
      (ensure-equalp (op:def_kind (op:original_type_def o)) :dk_primitive)
      (ensure-equalp (op:kind (op:original_type_def o)) :pk_short))
    (let ((o (op:lookup r "s0")))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind (op:original_type_def o)) :dk_primitive)
      (ensure-equalp (op:kind (op:original_type_def o)) :pk_string))
    (let ((o (op:lookup r "s10")))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind (op:original_type_def o)) :dk_string)
      (ensure-equalp (op:bound (op:original_type_def o)) 10))
    (let ((o (op:lookup r "f10_2")))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind (op:original_type_def o)) :dk_fixed)
      (ensure-equalp (op:digits (op:original_type_def o)) 10)
      (ensure-equalp (op:scale (op:original_type_def o)) 2)))
  
  
  (define-test "Read Enum"
    (idef-read '((define-enum "F"
                       ("NORMAL" "DIRECTORY" "SYMLINK")))
                 r)
    (let* ((o (op:lookup r "F")))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind o) :dk_enum)
      (ensure-equalp (op:members o) '("NORMAL" "DIRECTORY" "SYMLINK"))))
  
  (define-test "Read Constant"
    (idef-read '((define-constant "a" long 123)
                 (define-constant "b" char #\X))
               r)
    (let* ((o (op:lookup r "a")))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind o) :dk_constant)
      (ensure-equalp (any-value (op:value o)) 123)
      (ensure-typecode (op:type o) :tk_long)
      (ensure-typecode (any-typecode (op:value o)) :tk_long))
    (let* ((o (op:lookup r "b")))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind o) :dk_constant)
      (ensure-equalp (any-value (op:value o)) #\X)
      (ensure-typecode (op:type o) :tk_char)
      (ensure-typecode (any-typecode (op:value o)) :tk_char)))


  (define-test "Read Constant Expr"
    (test-idef-read
     ((define-constant "a" long (+ 10 5))
      (define-constant "b" long (+ "a" 1))
      (define-type "c" (array short (* 2 "a"))))
     "a" (def-pattern :dk_constant 'op:value (pattern 'any-value 15))
     "b" (def-pattern :dk_constant 'op:value (pattern 'any-value 16))      
     "c" (def-pattern :dk_alias 'op:original_type_def
           (def-pattern :dk_array 'op:length 30))))


  (define-test "Read Struct"
    (test-idef-read
     ((define-struct "S"
        (("a" long)
         ("b" string))))
     "S" (def-pattern :dk_struct
           'op:members 
           (sequence-pattern 
            (struct-pattern 'struct-class-name 'CORBA:StructMember
                            'op:name "a"
                            'op:type CORBA:tc_long)
            (struct-pattern 'op:name "b"
                            'op:type CORBA:tc_string)))))

  (define-test "Read Union 1"
    (idef-read '((define-union "MyUnion" long
                   ((0 "foo" string)
                    (1 "bar" long))))
               r)
    (ensure (op:lookup r "MyUnion"))
    (let ((obj (op:lookup r "MyUnion")))
      (ensure-equalp (op:member_count (op:type obj)) 2)
      (ensure-equalp (op:name (elt (op:members obj) 1))
                     "bar")
      (ensure-typecode (any-typecode (op:label (elt (op:members obj) 1)))
                       CORBA:tc_long)))

  (define-test "Read Union 2"
    (idef-read '((define-enum "status" ("aa" "bb"))
                 (define-union "MyUnion2" "status"
                   ((:aa "foo" string)
                    (default "bar" long))))
               r)
    (let ((obj (op:lookup r "MyUnion2")))
      (ensure-eql (any-value (op:label (elt (op:members obj) 0))) :aa)
      (let ((l2 (op:label (elt (op:members obj) 1))))
        (ensure-typep l2 'CORBA:Any)
        (ensure-typecode (any-typecode l2) :tk_octet)
        (ensure-eql (any-value l2) 0))))

  (define-test "Read Attribute"
    (idef-read '((define-interface "I" ()
                     (define-attribute "a" string
                       :id "IDL:my/M/I/a:1.0" )
                     (define-attribute "b" string :readonly t)))
                 r)
    (let* ((o (op:lookup (op:lookup r "I") "a") ))
      (ensure o "lookup name")
      (ensure-equalp (op:def_kind o) :dk_attribute)
      (ensure-equalp (op:mode o) :attr_normal )
      (ensure-equalp (op:id o) "IDL:my/M/I/a:1.0")
      (ensure-equalp (op:kind (op:type_def o)) :pk_string))
    (let* ((o (op:lookup (op:lookup r "I") "b") ))
      (ensure-equalp (op:mode o) :attr_readonly)
      (ensure-equalp (op:id o) "IDL:I/b:1.0")))


  (define-test "Complex example"
    (test-idef-read
     ((define-module "FileSys" ()
        (define-enum "FileType"
          ("NORMAL" "DIRECTORY" "SYMLINK"))
        (define-struct "Stat"
          (("mtime" long)
           ("ctime" long)
           ("inode" long)
           ("type" "FileType")))
        (define-interface "FileDescription" ()
          (define-attribute "name" string :readonly t)
          (define-type "Buffer" string)
          (define-operation "read" ((:param_in "size" long) 
                                    (:param_out "buf" "Buffer"))
            :result-type void
            :exceptions nil)
          (define-operation "destroy" ()
            :result-type void))
        (define-interface "FileSystem" ()
          (define-operation "open" ((:param_in file_name string)
                                    (:param_in flags long))
            :result-type "::FileSys::FileDescription"))))
     ;;
     "FileSys" (def-pattern :dk_module)
     "FileSys::FileType" (def-pattern :dk_enum
                           'op:defined_in (pattern 'op:name "FileSys"))
     "FileSys" (repository-pattern
                "FileType" (def-pattern :dk_enum)
                "Stat" (def-pattern :dk_struct)
                "FileDescription" (def-pattern :dk_interface
                                    'op:id "IDL:FileSys/FileDescription:1.0" )
                "FileSystem" (def-pattern :dk_interface))))


)


