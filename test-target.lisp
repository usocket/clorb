;;;; test-target

(in-package :clorb)

(define-test-suite "Target Code generator"
  
  (variables
   (repository (make-instance 'repository))
   (module (op:create_module repository "IDL:test/Mod:1.0" "Mod" "1.0"))
   (interface (op:create_interface module "IDL:test/Mod/Intf:1.0" "Intf" "1.0" nil)))
  
  
  (define-test "constant"
    (let ((const-def (op:create_constant repository "IDL:test/a:1.0" "a" "1.0"
                                         (op:get_primitive repository :pk_long)
                                         (CORBA:Any :any-value 192
                                                    :any-typecode CORBA:tc_long))))
      (ensure-pattern (target-code const-def (make-instance 'stub-target))
                      (sexp-pattern `(defconstant omg.org/root::a ,(eval-to 192))))))
  
  (define-test "Fixed"
    (let ((fixed-def (op:create_fixed repository 10 2))
          (target (make-instance 'stub-target)))
      (ensure (subtypep (target-type fixed-def target) 'rational))
      (ensure-typecode (eval (target-typecode fixed-def target))
                       (create-fixed-tc 10 2))))

  (define-test "struct"
    (let* ((members (list (CORBA:StructMember :name "a"
                                              :type_def (op:get_primitive repository :pk_long))))
           (struct-def (op:create_struct repository "IDL:test/s:1.0" "s" "1.0" members)))
      (ensure-pattern (target-code struct-def (make-instance 'stub-target))
                      (sexp-pattern `(define-struct omg.org/root::s
                                       &key
                                       (:id :required "IDL:test/s:1.0")
                                       (:name :required "s")
                                       (:members :required (("a" &any)))
                                       (:read :optional )
                                       (:write :optional ) )))))

  (define-test "alias-def"
    (let ((alias-def (op:create_alias repository "IDL:test/a:1.0" "a" "1.0" 
                                      (op:get_primitive repository :pk_long))))
      (ensure-pattern (target-code alias-def (make-instance 'stub-target))
                      (sexp-pattern `(define-alias omg.org/root::a 
                                       &key 
                                       (:id :required "IDL:test/a:1.0")
                                       (:name :required "a")
                                       (:type :required CORBA:Long)
                                       (:typecode :required CORBA:tc_long))))
      (setf (op:original_type_def alias-def) interface)
      (ensure-pattern (target-code alias-def (make-instance 'stub-target))
                      (let ((intf-symbol (intern "INTF" "MOD")))
                        (sexp-pattern `(define-alias omg.org/root::a 
                                           &key 
                                         (:id :required "IDL:test/a:1.0")
                                         (:name :required "a")
                                         (:type :required ,intf-symbol)
                                         (:typecode :required (symbol-typecode ',intf-symbol))))))))

  (define-test "exception"
    (let* ((members (list (CORBA:StructMember :name "a"
                                              :type_def (op:get_primitive repository :pk_long))))
           (exc (op:create_exception repository "IDL:test/e:1.0" "e" "1.0" members)))
      (ensure-pattern (target-code exc (make-instance 'stub-target))
                      (sexp-pattern `(define-user-exception omg.org/root::e &key
                                       (:name :required "e")
                                       (:id :required "IDL:test/e:1.0")
                                       (:members :required (("a" CORBA:tc_long)))
                                       (:version :optional "1.0")
                                       (:defined_in :required nil))))))

  (define-test "Interface"
    (let* ((a-string (op:get_primitive repository :pk_string))
           (intf (op:create_interface repository "IDL:if1:1.0" "if1""1.0" nil))
           (op1  (op:create_operation intf "IDL:if1/op1:1.0" "op1" "1.0"
                                      a-string :op_normal
                                      nil nil nil))
           (at1  (op:create_attribute intf "IDL:if1/at1:1.0" "at1" "1.0"
                                      a-string :attr_normal))
           (stub-target (make-instance 'stub-target))
           (static (make-instance 'static-stub-target)))
      (ensure-pattern
       (target-code intf stub-target)
       (sexp-pattern 
        '(progn
           (define-interface omg.org/root::if1 (CORBA:Object) &key
             (:id :required "IDL:if1:1.0")
             (:name :required "if1")
             (:version :optional "1.0")
             (:proxy :required (omg.org/root::if1-proxy omg.org/root::if1 corba:PROXY))
            (:defined_in :required nil))
           &any-rest)))
      (ensure-pattern
       (target-code op1 static)
       (sexp-pattern 
        `(progn
           (define-operation OMG.ORG/ROOT::IF1/OP1
             :ID "IDL:if1/op1:1.0" :NAME "op1" 
             :DEFINED_IN OMG.ORG/ROOT::IF1 :VERSION "1.0" 
             :RESULT OMG.ORG/CORBA:TC_STRING 
             :MODE :OP_NORMAL :CONTEXTS NIL :PARAMETERS NIL :EXCEPTIONS NIL )
           (define-method "OP1" ((&any omg.org/root::if1-proxy)) &any-rest))))
      (ensure-pattern
       (target-code at1 static)
       (sexp-pattern 
        `(progn
           (DEFINE-ATTRIBUTE OMG.ORG/ROOT::IF1/AT1 :ID "IDL:if1/at1:1.0" :NAME "at1" 
             :VERSION "1.0" :DEFINED_IN OMG.ORG/ROOT::IF1 :MODE :ATTR_NORMAL
             :TYPE OMG.ORG/CORBA:TC_STRING)
           (define-method (SETF "AT1") (&any (&any OMG.ORG/ROOT::IF1-PROXY)) &any-rest)
           (define-method "AT1" ((&any omg.org/root::if1-proxy)) &any-rest))))))


  (define-test "Value"
    (let* ((abv (op:create_value repository
                                 "IDL:abv:1.0" "abv" "1.0"
                                 nil t  nil nil  nil nil nil))
           (v (op:create_value repository
                               "IDL:v:1.0" "v" "1.0"
                               nil nil  nil nil  (list abv) nil nil))
           (target (make-instance 'stub-target)))
      (op:create_value_member v
                              "IDL:v/a:1.0" "a" "1.0"
                              v corba:public_member)
      (ensure-pattern
       (target-code abv target)
       (sexp-pattern 
        `(progn (define-value omg.org/root::abv &key
                  (:id :required "IDL:abv:1.0")
                  (:name :required "abv")
                  (:base_value :optional nil)
                  (:is_abstract :required t)
                  (:is_custom :optional nil)
                  (:is_truncatable :optional nil)
                  (:supported_interfaces :optional nil)
                  (:abstract_base_values :optional nil)
                  (:members :optional nil))
                &rest nil)))
      (ensure-pattern
       (target-code v target)
       (sexp-pattern
        `(progn (define-value omg.org/root::v &key
                  (:id :required "IDL:v:1.0")
                  (:name :required "v")
                  (:base_value :optional nil)
                  (:is_abstract :optional nil)
                  (:is_custom :optional nil)
                  (:is_truncatable :optional nil)
                  (:supported_interfaces :optional nil)
                  (:abstract_base_values :required (omg.org/root::abv))
                  (:members :optional (("a" (symbol-typecode 'omg.org/root::v) ,corba:public_member))))
                &rest nil)))))



#| end test suite |#)
