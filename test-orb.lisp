(in-package :clorb)

(defclass test-orb (clorb-orb)
  (resolve-namecontext resolve-name))

(defmethod orb-resolve ((orb test-orb) namecontext namestr)
  (setf (slot-value orb 'resolve-namecontext) namecontext)
  (setf (slot-value orb 'resolve-name) namestr))




(define-test-suite "Test ORB Pseudo object"
  
  (define-test "Can't marshal local objects"
    (let ((orb (CORBA:ORB_init)))
      (ensure-exception
       (op:object_to_string orb orb)
       corba:marshal 'op:minor (std-minor 4))
      (ensure-exception
       (op:_create_request orb nil "foo" nil nil 0)
       CORBA:NO_IMPLEMENT 'op:minor (std-minor 4))))
  
  
  (define-test "Initializing initial references"
    (let ((orb (CORBA:ORB_init)))
      (setf (orb-initial-references orb)
            (delete-if (lambda (entry)
                         (string-starts-with (car entry) "x-"))
                       (orb-initial-references orb)))
      (CORBA:ORB_init (list "-ORBInitRef" "x-a=corbaloc::x/x-a"
                            "-ORBInitRef x-b=corbaloc::x/x-b"
                            "-ORBInitRefx-c=corbaloc::x/x-c" ))
      (loop with names = (op:list_initial_references orb)
            for n in '("x-a" "x-b")
            do (ensure (member n names :test #'equal)
                       "Name '~A' should be in initial references" n)
            (let ((obj (op:resolve_initial_references orb n)))
              (ensure-equalp (oid-to-string 
                              (iiop-profile-key (first (object-profiles obj))))
                             n)))))


  (define-test "Initializing port and host"
    (let ((orb (CORBA:ORB_init)))
      (let ((host (orb-host orb))
            (port (orb-port orb)))
        (unwind-protect
          (progn (CORBA:ORB_init
                  '("-ORBPort 98" "-ORBHostname lagostz")
                  "")
                 (ensure-pattern* orb
                                  'orb-host "lagostz"
                                  'orb-port 98)
                 ;; Alt syntax
                 (CORBA:ORB_init '("-ORBPort" "99") "")
                 (ensure-pattern* orb 'orb-port 99))
          (setf (orb-host orb) host
                (orb-port orb) port)))))

  
  (define-test "object_to_string"
    (let* ((orb (CORBA:ORB_init))
           (obj (test-object orb))
           (ior (op:object_to_string orb obj)))
      (ensure (string-starts-with ior "IOR:"))
      (ensure (evenp (length ior)))
      (let ((obj2 (op:string_to_object orb ior)))
        (ensure-pattern* obj2
                         'proxy-id (proxy-id obj)
                         'object-profiles (object-profiles obj)))))
  
  
  (define-test "Resolve corbaname URL"
    (let ((orb (make-instance 'test-orb)))
      (op:string_to_object orb "corbaname::example.com#a/str%20ing/path")
      (ensure-equalp (slot-value orb 'resolve-name) "a/str ing/path")
      (ensure-pattern* (slot-value orb 'resolve-namecontext)
                       'object-profiles (sequence-pattern
                                         (pattern 'iiop-profile-host "example.com"
                                                  'iiop-profile-port 2809
                                                  'iiop-profile-key (decode-objkey-vector "NameService"))))))
  

  (define-test "DII- list creation"
    (let ((orb (CORBA:ORB_init)))
      (ensure-pattern 
       (op:create_list orb 3)
       (sequence-pattern
        (isa 'CORBA:NamedValue)
        (isa 'CORBA:NamedValue)
        (isa 'CORBA:NamedValue)))
      (let* ((p-string (make-instance 'primitive-def
                         :kind :pk_string :type CORBA:tc_string))
             (p-long   (make-instance 'primitive-def
                         :kind :pk_long :type CORBA:tc_long))
             (oper (make-instance 'operation-def
                     :id "id" :name "op1" 
                     :result_def p-string
                     :params (list (corba:parameterdescription
                                    :name "a" :type_def p-long :mode :param_in)
                                   (corba:parameterdescription
                                    :name "b" :type_def p-string :mode :param_inout))
                     :mode :op_normal
                     :contexts nil
                     :exceptions nil )))
        (ensure-pattern
         (op:create_operation_list orb oper)
         (sequence-pattern
          (pattern 'op:name "a"
                   'op:argument (pattern 'any-typecode CORBA:tc_long)
                   'op:arg_modes CORBA:ARG_IN)
          (pattern 'op:name "b"
                   'op:argument (pattern 'any-typecode CORBA:tc_string)
                   'op:arg_modes CORBA:ARG_INOUT))))))


  (define-test "DII- send multiple requests"
    (setup-test-out)
    (let* ((orb (make-instance 'test-orb))
           (obj (test-object orb))
           (ops '("op1" "op2" "op3")))
      (flet ((req-list ()
               (loop for op in ops collect
                     (let ((args nil)
                           (result (CORBA:NamedValue :argument (CORBA:Any :any-typecode CORBA:tc_void))))
                       (nth-value 1 (op:_create_request obj nil op args result 0)))))
             (check-reqs (response)
               (let ((actual-requests
                      (loop repeat (length ops) collect
                            (test-read-request :request-keys `((:response ,response))))))
                 (ensure (null (set-difference ops (mapcar (lambda (r) (getf r :operation))
                                                           actual-requests)
                                               :test #'string=))))))
        (op:send_multiple_requests_oneway orb (req-list))
        (check-reqs 0)
        (op:send_multiple_requests_deferred orb (req-list))
        (check-reqs 1))))
    
  (define-test "DII- poll/get next response"
    (setup-test-out)
    (let* ((orb (make-instance 'test-orb))
           (obj (test-object orb))
           (req (nth-value 1 (op:_create_request obj nil "op" nil nil 0))))
      (op:send_deferred req)
      (test-write-response :request req)
      (orb-work orb nil t)
      (ensure (op:poll_next_response orb) "response should be ready")
      (let ((req1 (op:get_next_response orb)))
        (ensure-eql req1 req)
        (ensure-eql (request-status req1) :returned))
      (ensure-exception 
       (op:poll_next_response orb)
       CORBA:BAD_INV_ORDER 'op:minor (std-minor 11))
      (ensure-exception 
       (op:get_next_response orb)
       CORBA:BAD_INV_ORDER 'op:minor (std-minor 11))))
        
      

  
  #|end|#)
