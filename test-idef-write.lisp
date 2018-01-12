(in-package :clorb)

(define-test-suite "IDEF Write Test"
  (variables 
    (repository (make-instance 'repository)))

  (define-test "UnionDef"
    (let* ((a-ulong (op:get_primitive repository :pk_ulong))
           (a-string (op:get_primitive repository :pk_string))
           (id "IDL:foo/Union:1.1")
           (members (list (CORBA:UnionMember :name "aa"
                                             :label (any :any-value 1 :any-typecode corba:tc_ulong)
                                             :type_def a-string)
                          (CORBA:UnionMember :name "_def_"
                                             :label (CORBA:Any :any-typecode CORBA:tc_octet
                                                               :any-value 0)
                                             :type_def a-ulong )
                          (CORBA:UnionMember :name "bb"
                                             :label (any :any-value 2 :any-typecode corba:tc_ulong)
                                             :type_def a-ulong)))
           (obj (op:create_union repository id "aunion" "1.1"
                                 a-ulong members)))
      (let* ((idef (idef-write obj))
             (new-rep (make-instance 'repository)))
        (idef-read (read-from-string (write-to-string idef
                                                      :readably t :escape t ))
                   new-rep)
        (loop for old in members
              for new in (op:members (op:lookup_id new-rep id))
              do (ensure-equalp (any-value (op:label new))
                                (any-value (op:label old)))
              (ensure-typecode (any-typecode (op:label new))
                               (any-typecode (op:label old)))))))

)
