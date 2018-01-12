(in-package :clorb)

(define-abstract-interface test-abint-1 (corba:abstractbase)
  :mixin test-abint-1-mixin
  :proxy (test-abint-1-PROXY CORBA:PROXY)
  :id "IDL:test/abint1:1.0"
  :name "abint1")

(define-value test-value-1
  :id "IDL:test/value1:1.0"
  :name "value1"
  :supported_interfaces (test-abint-1)
  :members (("name" CORBA:tc_string 0)))

(define-value test-value-2
  :id "IDL:test/value2:1.0"
  :name "value2"
  :base_value test-value-1
  :members (("left" (symbol-typecode 'test-value-2) 0)
            ("right" (symbol-typecode 'test-value-2) 0)))

(defclass test-value-2-user (test-value-2)
  ())

(define-method "DEPTH" ((v test-value-2-user))
  (do ((count 1 (1+ count))
       (hare (op:left v) (op:left hare))
       (turtle v (if turtle-move (op:left turtle) turtle))
       (turtle-move nil (not turtle-move)))
      ((or (not (typep hare 'test-value-2))
           (eql hare turtle))
       (if hare (1+ count) count))))

(define-value test-value-3
  :id "IDL:test/value3:1.0"
  :name "value3"
  :base_value test-value-1 
  :is_truncatable t
  :members (("next" corba:tc_valuebase 0)))


(define-value-box test-box-1 
  :id "IDL:test/box1:1.0"
  :name "box1"
  :version "1.0"
  :original_type CORBA:tc_long )

(define-value-box test-box-2
  :id "IDL:test/box2:1.0"
  :name "box2"
  :version "1.0"
  :original_type CORBA:tc_string
  :type string)

(define-value test-box-in-value
  :id "IDL:test/boxinvalue:1.0"
  :name "boxinvalue"
  :base_value test-value-1
  :is_truncatable t
  :members (("box" (symbol-typecode 'test-box-1) 0)))

(define-test-suite "Value"
  (variables
   (tc1 (symbol-typecode 'test-value-1))
   (id1 (symbol-ifr-id 'test-value-1))
   (v1 (make-instance 'test-value-1 :name "v1"))
   (tc2 (symbol-typecode 'test-value-2))
   (id2 (symbol-ifr-id 'test-value-2))
   (v2 (make-instance 'test-value-2 :name "root" :left v1 :right nil))
   (v2b (make-instance 'test-value-2 :name "v2b" :left v1 :right v1))
   (tc3 (let ((tc (symbol-typecode 'test-value-3)))
          (setf (gethash (op:id tc) *ifr-id-symbol*) 'test-value-3)
          tc))
   (buffer (get-work-buffer *the-orb*)))

  (define-test "marshal value typecodes"
    (marshal-typecode (symbol-typecode 'test-abint-1) buffer)
    (marshal-typecode tc1 buffer)
    (marshal-typecode tc2 buffer)
    (ensure-pattern*
     buffer
     'unmarshal-typecode (pattern 'identity (isa 'abstract_interface-typecode)
                                  'op:id "IDL:test/abint1:1.0"
                                  'op:name "abint1")
     'unmarshal-typecode (pattern 'op:id (op:id tc1)
                                  'op:type_modifier corba:vm_none
                                  'op:concrete_base_type corba:tc_null )
     'unmarshal-typecode (pattern 'op:id (op:id tc2)
                                  'op:concrete_base_type (pattern 'op:id (op:id tc1)))))

  (define-test "simple"
    (marshal v1 (symbol-typecode 'test-value-1) buffer)
    (ensure-pattern* (unmarshal (symbol-typecode 'test-value-1) buffer) 
                     'identity (isa 'test-value-1)
                     'op:name (op:name v1)))

  (define-test "derived"
    ;; marshalling a derived valuetype for a base type
    (marshal v2 tc1 buffer)
    (ensure-pattern* (unmarshal tc1 buffer) 
                     'identity (isa 'test-value-2)
                     'op:name (op:name v2)
                     'op:left (isa 'test-value-1)
                     'op:right (isa 'null)))

  (define-test "Indirection unmarshal"
    (let (ifr-pos v-pos)
      (with-out-buffer (buffer)
        ;; first value
        (align 4)
        (setq v-pos pos)
        (marshal-long (logior #x7fffff00 2) buffer)
        (setq ifr-pos pos)
        (marshal-string (object-id v1) buffer)
        (marshal-string (op:name v1) buffer)
        ;; another value
        (marshal-long (logior #x7fffff00 2) buffer)
        (marshal-long -1 buffer) (marshal-long (- ifr-pos pos) buffer)      ; indirection to ifr id
        (marshal-string "2" buffer)
        ;; first value again
        (marshal-long -1 buffer) (marshal-long (- v-pos pos) buffer)))
    (let ((r1 (unmarshal tc1 buffer))
          (r2 (unmarshal tc1 buffer))
          (r3 (unmarshal tc1 buffer)))
      (ensure-eql r1 r3)
      (ensure-equalp (op:name r2) "2")))

  (define-test "sharing"
    (marshal v2b tc2 buffer)
    (let ((obj (unmarshal tc2 buffer)))
      (ensure-eql (op:left obj) (op:right obj))))

  (define-test "cyclic"
    (setf (op:left v2b) v2b)
    (marshal v2b tc2 buffer)
    (let ((obj (unmarshal tc2 buffer)))
      (ensure-eql (op:left obj) obj)))

  (define-test "ValueFactory"
    (let ((orb (CORBA:ORB_init)))
      (op:register_value_factory orb id2 'test-value-2-user))
    (marshal v2 tc1 buffer)
    (let ((obj (unmarshal tc1 buffer)))
      (ensure-typep obj 'test-value-2-user)
      (ensure-eql (op:depth obj) 2)))

  (define-test "boxed value"
    (let ((n1 (test-box-1 123))
          (n2 (test-box-1 99))
          (n3 (test-box-1 333))
          (tcn (symbol-typecode 'test-box-1))
          (s1 (test-box-2 "Hello World"))
          (s2 (test-box-2 "Foo"))
          (s3 (test-box-2 "Fie"))
          (tcs (symbol-typecode 'test-box-2)))
      (marshal n1 tcn buffer) (marshal n2 tcn buffer) (marshal n1 tcn buffer)
      (marshal s1 tcs buffer) (marshal s2 tcs buffer) (marshal s1 tcs buffer)
      ;; Marshal valuebox as a ValueBase
      (marshal n3 corba:tc_valuebase buffer)
      (marshal s3 corba:tc_valuebase buffer)
      (let ((r1 (unmarshal tcn buffer))
            (r2 (unmarshal tcn buffer))
            (r3 (unmarshal tcn buffer)))
        (ensure-eql (op:data r1) (op:data n1))
        (ensure-eql (op:data r2) (op:data n2))
        (ensure-eql r1 r3))
      (let ((r1 (unmarshal tcs buffer))
            (r2 (unmarshal tcs buffer))
            (r3 (unmarshal tcs buffer)))
        (ensure-equalp (op:data r1) (op:data s1))
        (ensure-equalp (op:data r2) (op:data s2))
        (ensure-eql r1 r3))
      
      (let ((r1 (unmarshal corba:tc_valuebase buffer)))
        (ensure-eql (op:data r1) (op:data n3)))
      (let ((r2 (unmarshal corba:tc_valuebase buffer)))
        (ensure-equalp (op:data r2) (op:data s3)))))


  (define-test "chunked"
    (let ((v3 (make-instance 'test-value-3 :name "foo" 
                             :next (make-instance 'test-value-3 :name "bar" 
                                                  :next nil))))
      (marshal v3 tc1 buffer)
      (ensure-pattern* (unmarshal tc1 buffer) 
                       'op:name "foo"
                       'op:next (pattern 'identity (isa 'test-value-3)
                                         'op:name "bar"
                                         'op:next nil))))
  (define-test "chunked box"
    (let ((v (make-instance 'test-box-in-value :name "foo" 
                             :box (make-instance 'test-box-1 :data 989))))
      (marshal v tc1 buffer)
      (ensure-pattern* (unmarshal (symbol-typecode 'test-box-in-value) buffer) 
                       'op:name "foo"
                       'op:box (pattern 'op:data 989))))
  

  (define-test "truncated"
    (let ((v3 (make-instance 'test-value-3 :name "foo" 
                             :next (make-instance 'test-value-3 :name "bar" 
                                                  :next nil)))
          (tc3 (symbol-typecode 'test-value-3)))
      (setf (gethash (op:id tc3) *ifr-id-symbol*) 'test-value-3)
      (marshal v3 tc1 buffer)
      (marshal v3 tc1 buffer)
      (remhash (op:id tc3) *ifr-id-symbol*)
      (let ((obj (unmarshal tc1 buffer)))
        (ensure-pattern* obj 'op:name "foo")
        (ensure-eql (unmarshal tc1 buffer) obj))))

  (define-test "mid state chunking"
    (flet ((l (x) (marshal-long x buffer))
           (o (x) (marshal-octet x buffer))
           (c (x) (marshal-octet (char-code x) buffer)))
      ;; value tag
      (l (+ min-value-tag value-flag-chunking))
      ;; state: (string) "hx"
      (l 5)                             ; chunk length
      (l 3) (c #\h)                     ; first chunk
      (l 2)                             ; chunk length
      (c #\x) (o 0)                     ; second chunk
      (l -1)                            ; end tag
      ;; --
      (let ((obj (unmarshal tc1 buffer)))
        (ensure-pattern* obj 
                         'identity (isa 'test-value-1)
                         'op:name "hx"))))

  (define-test "any"
    (let ((v3 (make-instance 'test-value-3 :name "foo" 
                             :next (make-instance 'test-value-3 :name "bar" 
                                                  :next nil))))
      (marshal v3 CORBA:tc_any buffer)
      (ensure-pattern* 
       (unmarshal corba:tc_any buffer)
       'identity (isa 'CORBA:Any)
       'corba:any-value
       (pattern 'op:name "foo"
                'op:next (pattern 'identity (isa 'test-value-3)
                                  'op:name "bar"
                                  'op:next nil)))))

  (define-test "Abstract Interface"
    (let ((ab-tc (symbol-typecode 'test-abint-1))
          (abi (make-instance 'test-abint-1-proxy
                 :id ""
                 :the-orb *the-orb*
                 :profiles (list (make-iiop-profile :version (make-iiop-version  1 0)
                                                    :host "he" :port 98
                                                    :key (string-to-oid "hej"))))))
      (marshal v1 ab-tc buffer)
      (marshal nil ab-tc buffer)
      (marshal abi ab-tc buffer)
      (ensure-pattern* (unmarshal ab-tc buffer)
                       'op:name (op:name v1))
      (ensure-eql (unmarshal ab-tc buffer) nil)
      (ensure-pattern* (unmarshal ab-tc buffer)
                       'identity (isa 'CORBA:Proxy)
                       `(op:_is_a * ,(op:id ab-tc)) t)))


#| end test suite |#)
