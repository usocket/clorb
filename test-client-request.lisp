(in-package :clorb)


(define-operation test-op-1
  :id "IDL:test/op1:1.0"
  :name "op1"
  :version "1.0"
  :mode :op_oneway
  :parameters (("x" :param_in corba:tc_string)))

(define-operation test-op-2
  :id "IDL:test/op2:1.0"
  :name "op2"
  :version "1.0"
  :mode :op_normal
  :result CORBA:tc_long
  :parameters (("x" :param_inout corba:tc_string)))

(define-attribute test-at-1
  :id "IDL:test/at1:1.0"
  :name "at1"
  :mode :attr_normal
  :type CORBA:tc_string)


(defmacro with-hidden-events (&body body)
  `(progn ,@body))


(define-test-suite "Client Request"
  (variables
   (orb (CORBA:ORB_init)))

  (define-test "one way static call"
    (setup-test-out)
    (let ((obj (test-object orb (make-iiop-version 1 0)))
          (n -129988))
      (with-hidden-events
        (static-call ("foo" obj)
                     :output ((buffer) (marshal-any-value n buffer))
                     :no-response t))
      (test-read-request
       :request-keys `((:response 0) (:operation "foo") (:version ,giop-1-0))
       :args (list n))))

  (define-test "static call with iiop 1.1"
    (setup-test-out)
    (let ((obj (test-object orb (make-iiop-version 1 1)))
          (n -129988))
      (with-hidden-events
        (static-call ("foo" obj)
                     :output ((buffer) (marshal-any-value n buffer))
                     :no-response t))
      (test-read-request
       :request-keys `((:response 0) (:operation "foo") (:version ,giop-1-1))
       :args (list n))))


  (define-test "DII" 
    (setup-test-out)
    (let* ((obj (test-object orb))
           (a1 "hepp")
           (args (list (CORBA:NamedValue :argument a1 :arg_modes CORBA:ARG_IN)))
           (ret (CORBA:NamedValue :argument (CORBA:Any :any-typecode CORBA:tc_long))))
      (multiple-value-bind (result req)
                           (op:_create_request obj nil "op" args ret 0)
        (declare (ignore result))
        (op:send_deferred req)
        (test-read-request
         :request-keys '((:response 1) (:operation "op") (:object-key #(17)))
         :args (list a1))
        (test-write-response :request req  :message '(224412))
        (orb-work orb nil t)
        (assert (op:poll_response req) () "should have gotten the response")
        (op:get_response req)
        (ensure-eql (corba:any-value (op:return_value req)) 224412))))


  (define-test "corba:funcall"
    (let* ((obj (test-object orb)))
      (setf (response-func *test-out-conn*)
            (lambda (req)
              (test-read-request :args '("IDL:test:1.0"))
              (test-write-response :request req 
                                   :message (list (corba:any :any-typecode corba:tc_boolean
                                                             :any-value t)))))
      (ensure-eql (corba:funcall "_is_a" obj "IDL:test:1.0") t)))

  (define-test "jit-call oneway"
    (setup-test-out)
    (let* ((obj (test-object orb)))
      (%jit-call test-op-1 obj "hej")
      (test-read-request
       :request-keys '((:response 0) (:operation "op1") (:object-key #(17)))
       :args '("hej"))))


  (define-test "jit-call normal"
    (setup-test-out)
    (let* ((obj (test-object orb)))
      (setf (response-func *test-out-conn*)
            (lambda (req)
              (test-read-request :args '("hej"))
              (test-write-response :request req :message '(9977 "jolly"))))
      (multiple-value-bind (r1 r2)
                           (%jit-call test-op-2 obj "hej")
        (ensure-equalp r1 9977)
        (ensure-equalp r2 "jolly"))))

  (define-test "jit attr"
    (setup-test-out)
    (let* ((obj (test-object orb)))
      (setf (response-func *test-out-conn*)
            (lambda (req)
              (test-read-request :request-keys '((:operation "_get_at1")))
              (test-write-response :request req :message '("jolly"))))
      (multiple-value-bind (r1) (%jit-get test-at-1 obj)
        (ensure-equalp r1 "jolly"))
      (setf (response-func *test-out-conn*) 
            (lambda (req)
              (test-read-request 
               :request-keys '((:operation "_set_at1"))
               :args '("fnord")) 
              (test-write-response :request req)))
      (%jit-set test-at-1 obj "fnord")
      (validate *test-out-conn*)))


  (define-test "framgmented reply"
    (setup-test-out)
    (setup-outgoing-connection *test-out-conn*)
    (let ((req (create-client-request
                orb :request-id 1)))
      (%add-client-request *test-out-conn* req)
      (let ((buffer (get-work-buffer orb)))
        (marshal-giop-header :REPLY buffer giop-1-1 t)
        (marshal-service-context nil buffer) 
        (marshal-ulong 1  buffer)       ;req id
        (marshal :no_exception (symbol-typecode 'GIOP:REPLYSTATUSTYPE) buffer)
        (marshal-giop-set-message-length buffer)
        (let ((octets (buffer-octets buffer)))
          (io-descriptor-set-write *test-response-desc* octets 0
                                   (length octets))))
      (orb-work orb nil t)
      (test-write-response :orb orb
                           :message-type :fragment :giop-version giop-1-1
                           :message '(1 "hello") )
      (orb-work orb nil t)
      (ensure-pattern* req
                       'request-status :no_exception
                       'request-buffer (pattern 'unmarshal-short 1
                                                'unmarshal-string "hello")) ))


  (define-test "Close Connection"
    ;; Test receiving a close connection message after sending a
    ;; request, and having the request automatically retried.
    (setup-test-out)
    (let* ((obj (test-object orb))
           (ret (CORBA:NamedValue :argument (CORBA:Any :any-typecode CORBA:tc_long))))
      (multiple-value-bind (result req) (op:_create_request obj nil "op" nil ret 0)
        (declare (ignore result))
        (op:send_deferred req)
        (test-read-request
         :request-keys '((:response 1) (:operation "op") (:object-key #(17))))
        (test-write-response :orb orb :message-type :closeconnection)
        (orb-work orb nil t)

        ;; hmm. this is very internal ..
        (ensure-eql (request-status req) :error)
        (ensure-typep (request-exception req) 'CORBA:TRANSIENT)

        ;; Sometime fails, WHY? -- connection closed message will
        ;; destroy io-descriptor and disconnect it from the
        ;; connection. The connection will therefor never get the
        ;; write-ready event.
        ;;(assert (not (write-buffer-of *test-out-conn*)))

        ;; Repair connection
        (setup-test-out)
        (setf (%object-connection obj) *test-out-conn*)

        (ensure (not (op:poll_response req)))  ; retry send

        (test-read-request 
         :request-keys '((:response 1) (:operation "op") (:object-key #(17))))
        (test-write-response :request req :message '(199199))

        (op:get_response req)
        (ensure-eql (corba:any-value (op:argument (op:result req))) 199199) )))


  (define-test "_non_existent"
    ;; Test that _non_existent doesn't return a OBJECT_NOT_EXIST exception
    ;; should return T. 
    (setup-test-out)
    (let ((obj (test-object orb)))
      (flet ((test-non_existent (result)
               ;; simulate calling op:_non_existent and server returning result
               (setf (response-func *test-out-conn*)
                     (lambda (req)
                       (test-read-request
                        :orb orb
                        :request-keys `((:operation "_non_existent")) )
                       (typecase result
                         (corba:systemexception  

                          (test-write-response
                           :orb orb :request req
                           :reply-status :system_exception
                           :message (test-system-exception-message result)))
                         (t
                          (test-write-response
                           :orb orb :request req
                           :message (list (CORBA:Any
                                           :any-value result
                                           :any-typecode corba:tc_boolean)))))))
               (op:_non_existent obj)))
        (ensure-eql (test-non_existent nil) nil)
        (ensure-eql (test-non_existent t)  t)
        (ensure-eql (test-non_existent
                     (system-exception 'CORBA:OBJECT_NOT_EXIST 0 :COMPLETED_NO))
                    t))))
  

#| end suite |# )
