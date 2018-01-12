;;; test-srv.lisp -- Test Server Request Processing

(in-package :clorb)


(ignore-errors
  (op:activate_object_with_id
   (boot-poa-of (adapter (CORBA:ORB_init)))
   (string-to-oid "_TEST_")
   (make-instance 'null-servant)))


(define-test-suite "Server Request Processing"
  (variables
   (*log-level* 5))

  (define-test "Process Cancel Request"
    (setup-test-in)
    
    ;; Create some dummy requests "in processing", we will cancel two of them
    (loop for (id state) in '((0 :wait) (1 :exec) (2 :wait) (3 :wait))
          do (push (create-server-request *the-orb* :request-id id
                                          :state state :response-flags 1)
                   (connection-server-requests *test-in-conn*)))
    ;; Write a Cancel Request message
    (test-write-request
     :message-type 2                   ; CancelRequest
     :message (list (giop:cancelrequestheader :request_id 1)))
    (test-write-request
     :message-type 2                   ; CancelRequest
     :message (list (giop:cancelrequestheader :request_id 2)))
    
    ;; Let the server code process the CancelRequest
    (orb-work *the-orb* nil t)
    
    ;; Check result
    (ensure-equalp 
     (sort (loop for req in (connection-server-requests *test-in-conn*)
                 collect (list (request-id req)
                               (response-expected req)
                               (request-state req)))
           #'< :key #'car)
     '((0 t :wait) (1 nil :exec) (2 nil :canceled) (3 t :wait))))


  (define-test "Simple request"
    (test-request-response
     :request-type 0
     :request (list (giop:requestheader_1_0
                     :request_id 0
                     :service_context nil
                     :response_expected t
                     :object_key #(0)
                     :operation "_non_existent"
                     :requesting_principal #()))
    :response (list :message-type :reply
                    :version giop-1-0
                    'giop:replyheader 
                    (pattern 'op:request_id 0
                             'op:reply_status :system_exception)
                    corba:tc_string 
                    "IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0" )))


  (define-test "Invalid version"
    (test-request-response
     :request (list (giop:messageheader_1_1
                     :magic "GIOP" 
                     :giop_version (giop:version :major 1 :minor 2)
                     :flags 1
                     :message_type 0
                     :message_size 0))
     :response (list :message-type :messageerror
                     :version giop-1-1)))

  (define-test "Respond with same version"
    (test-request-response
     :request-type 0
     :request (list :version giop-1-1
                    (giop:requestheader_1_1
                     :request_id 0
                     :service_context nil
                     :response_expected t
                     :reserved #(0 0 0)
                     :object_key #(0)
                     :operation "_non_existent"
                     :requesting_principal #()))
     :response (list :message-type :reply
                     :version giop-1-1
                     'giop:replyheader 
                     (pattern 'op:request_id 0
                              'op:reply_status :system_exception)
                     corba:tc_string 
                     "IDL:omg.org/CORBA/OBJECT_NOT_EXIST:1.0" )))

  (define-test "Locate request"
    (test-request-response
     :request-type :locaterequest
     :request (list :version giop-1-1
                    (giop:locaterequestheader
                     :request_id 0
                     :object_key #(0)))
     :response (list :message-type :locatereply
                     :version giop-1-1
                     'giop:locatereplyheader
                     (pattern 'op:request_id 0
                              'op:locate_status :unknown_object))))



  (define-test "Request response with argument"
    (test-request-response
     :request-type :request
     :request (list (giop:requestheader_1_0
                     :request_id 0
                     :service_context nil
                     :response_expected t
                     :object_key (string-to-oid "_TEST_")
                     :operation "_is_a"
                     :requesting_principal #())
                    "IDL:omg.org/CORBA/Object:1.0")
     :response (list :message-type :reply
                     'giop:replyheader
                     (pattern 'op:request_id 0
                              'op:reply_status :no_exception)
                     corba:tc_boolean
                     t)))
    

  (define-test "Fragmented request"
    (setup-test-in)
    (test-write-request
     :message-type :request
     :giop-version giop-1-1 :fragmented t
     :message (list (giop:requestheader_1_1
                     :request_id 0
                     :service_context nil
                     :response_expected t
                     :reserved #(0 0 0)
                     :object_key (string-to-oid "_TEST_")
                     :operation "_is_a"
                     :requesting_principal #())))
    (test-write-request
     :message-type :fragment
     :giop-version giop-1-1
     :message (list "IDL:omg.org/CORBA/Object:1.0"))
    (orb-work *the-orb* nil t)
    (test-read-response 
     :message (list :message-type :reply
                    'giop:replyheader
                    (pattern 'op:request_id 0
                             'op:reply_status :no_exception)
                    corba:tc_boolean
                    t)))

  (define-test "Fragmented request 2"
    ;; Break in the middle of the header
    (setup-test-in)
    (test-write-request
     :message-type :request
     :giop-version giop-1-1 :fragmented t
     :message (list (CORBA:Any :any-typecode (SYMBOL-TYPECODE 'OMG.ORG/IOP:SERVICECONTEXTLIST)
                               :any-value ())))
    (test-write-request
     :message-type :fragment
     :giop-version giop-1-1
     :message (list (CORBA:Any :any-typecode CORBA:tc_ulong
                               :any-value 0)    ; request id
                    (CORBA:Any :any-typecode corba:tc_boolean
                               :any-value t)    ; response expected
                    (CORBA:Any :any-typecode (CREATE-SEQUENCE-TC 0 OMG.ORG/CORBA:TC_OCTET)
                               :any-value (string-to-oid "_TEST_"))                    
                    (CORBA:Any :any-typecode OMG.ORG/CORBA:TC_STRING
                               :any-value "_non_existent")
                    (CORBA:Any :any-typecode (SYMBOL-TYPECODE 'OMG.ORG/GIOP:PRINCIPAL)
                               :any-value ())))
    (orb-work *the-orb* nil t)
    (test-read-response 
     :message (list :message-type :reply
                    'giop:replyheader
                    (pattern 'op:request_id 0
                             'op:reply_status :no_exception)
                    corba:tc_boolean
                    nil)))


#|end|# )

