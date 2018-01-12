(in-package :clorb)

(define-test-suite "Test Union" 
  (variables )

  (define-test "Create Union TC"
    (let* ((members (list (list 1 "lt" corba:tc_long)
                          (list 2 "lt" corba:tc_long)
                          (list 3 "name" corba:tc_string)
                          (list 'default "expr" corba:tc_string)))
           (tc (create-union-tc "IDL:filter:1.0" "filter" corba:tc_long members)))
      (ensure-typep tc 'CORBA:TypeCode)
      (ensure-eql (op:kind tc) :tk_union)
      (ensure-equalp (op:name tc) "filter")
      (ensure-equalp (op:id tc) "IDL:filter:1.0")
      (ensure-equalp (op:member_count tc) (length members))
      (ensure-equalp (op:default_index tc) (position 'default members :key #'car))
      (ensure-equalp (op:discriminator_type tc) corba:tc_long)
      (loop for i from 0 below (op:member_count tc)
            for m in members
            do 
            (unless (eq i (op:default_index tc))
              (ensure-eql (op:member_label tc i) (first m)))
            (ensure-equalp (op:member_name tc i) (second m))
            (ensure-equalp (op:member_type tc i) (third m)))))

  (define-test "Create Union TC 2"
    (let* ((members (list (list t "on" corba:tc_long)
                          (list nil "off" corba:tc_string)))
           (tc (create-union-tc "IDL:Two:1.0" "Two" corba:tc_boolean members)))
      (ensure-typep tc 'CORBA:TypeCode)
      (ensure-equalp (op:member_count tc) (length members))
      (ensure-equalp (op:default_index tc) -1)
      (loop for i from 0 below (op:member_count tc)
            for m in members
            do 
            (unless (eq i (op:default_index tc))
              (ensure-eql (op:member_label tc i) (first m)))
            (ensure-equalp (op:member_name tc i) (second m))
            (ensure-equalp (op:member_type tc i) (third m)))))


  (define-test "Definer macro"
    (let* ((type-sym (gensym "UNION"))
           (tc-sym (gensym "TC_UNION"))
           (field1 (gensym "A"))
           (field2 (gensym "D"))
           
           (expr `(define-union ,type-sym :name "aunion" :id "IDL:myUnion:1.0"
                    :tc-constant ,tc-sym
                    :discriminator-type corba:tc_long
                    :members (( 0 corba:tc_short :name ,(symbol-name field1)
                                :creator ,field1 )
                              ( 3 corba:tc_short :name ,(symbol-name field1)
                                :creator ,field1 )
                              ( 1 corba:tc_string :name ,(symbol-name field2)
                                :creator ,field2 :default t)))))
      
      ;;(pprint (macroexpand expr))
      (eval expr)
      ;; creator
      (ensure (functionp (symbol-function type-sym)))
      (let ((x (funcall type-sym :union-discriminator 0 
                        :union-value 17)))
        (ensure-eql (funcall (intern (symbol-name field1) :op) x) 17))
      ;; field creators
      (ensure (functionp (symbol-function field1)))
      (let ((x (funcall field1 18)))
        (ensure-eql (union-discriminator x) 0))
      (ensure (functionp (symbol-function field2)))
      (let ((x (funcall field2 "hej")))
        (ensure-eql (union-discriminator x) 1))
      ;; created typecode
      (let ((tc (symbol-typecode type-sym)))
        (ensure-typep tc 'corba:typecode)
        (loop for i from 0 for l in '(0 3) 
              do (ensure-equalp (op:member_label tc i) l))
        (ensure-eql (op:default_index tc) 2))))


  (define-test "Union marshalling"
    (let* ((type-sym (gensym "UNION"))
           (expr
            `(define-union ,type-sym :name "aunion" :id "IDL:myUnion:1.0"
                           :tc-constant ,type-sym
                           :discriminator-type corba:tc_long
                           :members (( 0 corba:tc_short :name "a" )
                                     ( 3 corba:tc_short :name "a" )
                                     ( 1 corba:tc_string :name "b" :default t)))))
      (eval expr)
      (let ((buffer (get-work-buffer *the-orb*))
            (u1 (funcall type-sym :union-value 12 :union-discriminator 0))
            (u2 (funcall type-sym)))
        (setf (op:default u2) "hello")
        (marshal u1 (symbol-typecode type-sym) buffer)
        (marshal u2 (symbol-typecode type-sym) buffer)
        (let ((u (unmarshal (symbol-typecode type-sym) buffer)))
          (ensure-pattern* u
                          'op::a 12 
                          'union-discriminator 0))
        (let ((u (unmarshal (symbol-typecode type-sym) buffer)))
          (ensure-pattern* u
                          'union-discriminator 1
                          'op::b "hello")))))
  
)
  
