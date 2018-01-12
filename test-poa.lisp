(in-package :clorb)


(setup-test-in)

(defun test-poa-invoke (poa &key operation oid args (kind :normal) (request nil request-p))
  (let ((buffer (get-work-buffer (the-orb poa))))
    (unless request-p
      (setq request
            (create-server-request
             (the-orb poa)
             :state :wait  :request-id 1  :connection *test-in-conn*
             :operation operation :kind kind  :response-flags 1
             :input buffer :giop-version giop-1-0 :object-id oid)))
    (dolist (a args)
      (marshal-any-value a buffer))
    (poa-invoke poa request)
    request))


(defun test-poa-dispatch (poa &key operation oid args (kind :normal)
                              (request nil request-p)
                              poa-spec)
  (let ((buffer (get-work-buffer (the-orb poa))))
    (unless request-p
      (setq request
            (create-server-request
             (the-orb poa)
             :state :wait  :request-id 1  :connection *test-in-conn*
             :operation operation :kind kind  :response-flags 1
             :input buffer :giop-version giop-1-0 :object-id oid :poa-spec poa-spec)))
    (dolist (a args)
      (marshal-any-value a buffer))
    (poa-dispatch poa request)
    request))


;; ==========================================================================

(defclass mock-activator (PORTABLESERVER:SERVANTACTIVATOR)
  ((adapter  :initarg :adapter)
   (expected-oid  :initarg :expected-oid)
   (created-servant)
   (etheralize-verifier :initarg :etheralize-verifier  :initform nil)
   (etheralize-called   :initform 0)))

(define-method incarnate ((s mock-activator) oid adapter)
  ;; Raises ForwardRequest
  (ensure (slot-boundp s 'expected-oid) "unexpected incarnate")
  (ensure-eql adapter (slot-value s 'adapter))
  (ensure-equalp oid (slot-value s 'expected-oid))
  (setf (slot-value s 'created-servant) (make-instance 'null-servant)))

(define-method etherealize ((s mock-activator) oid adapter servant cleanup-in-progress reamining-activations)
  (incf (slot-value s 'etheralize-called))
  (ensure-pattern
   (list oid adapter servant cleanup-in-progress reamining-activations)
   (slot-value s 'etheralize-verifier)))



;; ==========================================================================

(defclass mock-dsi-servant (portableserver:dynamicimplementation)
  ((plug-op :initform #'identity
            :initarg :plug-op
            :accessor plug-op)))

(define-method invoke ((servant mock-dsi-servant) req)
  (let ((op (op:operation req)))
    (cond ((equal op "op")
           (let ((args (op:arguments req
                                     (list (corba:namedvalue
                                            :name "x"
                                            :argument (CORBA:Any :any-typecode CORBA:tc_string)
                                            :arg_modes ARG_IN)))))
             (op:set_result req
                            (corba:any :any-typecode corba:tc_string
                                       :any-value (funcall (plug-op servant)
                                                           (any-value (op:argument (first args))))))))
          (t
           (error 'corba:bad_operation)))))


;; ==========================================================================

(corba:orb_init)
(unless (orb-host *the-orb*)
  (setf (orb-host *the-orb*) "localhost"))


(define-test-suite "POA Test"
  (variables
   (orb (make-instance 'clorb-orb :host "localhost" :port 9999 :active t))
   (root-adapter (create-root-adapter orb))
   (root-poa (root-poa-of root-adapter))
   (poa-map (poa-map-of root-adapter))
   (*log-level* 4))

  (define-test "poa-invoke"
    (let ((servant (make-instance 'null-servant))
          oid)
      (op:activate (op:the_poamanager root-poa))
      (setq oid (op:activate_object root-poa servant))
      ;; for locate request
      (ensure-pattern*
       (test-poa-invoke root-poa :oid oid :operation "_locate" :kind :locate))
      ;; standard request ops
      (ensure-pattern*
       (test-poa-invoke root-poa :oid oid :operation "_non_existent")
       'reply-status :no_exception
       'request-result '(nil))
      (ensure-pattern*
       (test-poa-invoke root-poa :oid oid :operation "_is_a" :args '("fooo"))
       'reply-status :no_exception
       'request-result '(nil))))


  (define-test "Policy creation"
    (let ((p1 (op:create_id_assignment_policy root-poa :user_id)))
      (ensure-typep p1 'CORBA:Policy)
      (ensure-typep p1 'portableserver:idassignmentpolicy)
      (ensure-eql (op:value p1) :user_id)
      (op:destroy p1))
    (let ((p1 (op:create_policy orb portableserver:id_assignment_policy_id
                                :system_id)))
      (ensure-typep p1 'CORBA:Policy)
      (ensure-typep p1 'portableserver:idassignmentpolicy)
      (ensure-eql (op:value p1) :system_id)
      (op:destroy p1))
    ;; Check that correct exception is signalled
    (handler-case
      (progn (op:create_id_assignment_policy root-poa :foo)
             (ensure nil))
      (CORBA:POLICYERROR (v)
                         (ensure-equalp (op:reason v) corba:bad_policy_type))))


  (define-test "Create POA"
    (let* ((p1 (op:create_id_assignment_policy root-poa :user_id))
           (p2 (op:create_request_processing_policy root-poa :use_default_servant))
           (name "TestP")
           (poa (op:create_poa root-poa name nil (list p1 p2))))
      (ensure-typep poa 'PortableServer:POA)

      ;; Unique POAManager created
      (ensure (not (eql (op:the_poamanager root-poa)
                        (op:the_poamanager poa))))

      (ensure-equalp (op:the_name poa) name)
      (ensure-eql (op:the_parent poa) root-poa)

      (let ((poa0 (op:find_POA root-poa name nil)))
        (ensure-eql poa0 poa))

      (ensure (member poa (managed-poas (op:the_poamanager poa)))
              "Registered with the manger")

      (handler-case
        (progn (op:create_poa root-poa name nil nil)
               (ensure nil))
        (PORTABLESERVER:POA/ADAPTERALREADYEXISTS ()))

      (let ((obj (op:create_reference_with_id poa (string-to-oid "Foo") "IDL:if:1.0")))
        (ensure-typep obj 'CORBA:Proxy))))


  (define-test "Destroy POA"
    (let* ((poa1 (op:create_poa root-poa "p1" nil nil))
           (id1 (poa-poaid poa1))
           (poa2 (op:create_poa poa1 "p2" nil nil))
           (id2 (poa-poaid poa2)))
      (ensure-eql (gethash id1 poa-map) poa1)
      (ensure-eql (gethash id2 poa-map) poa2)
      ;; Following method tests what happens if we call create_POA during
      ;; destruction, before the destruction is "apparent".
      (define-method op:destroy :before ((poa (eql poa2)) f1 f2)
        (declare (ignore f1 f2))
        (ensure-exception
         (op:create_POA poa1 "p9" nil nil)
         CORBA:BAD_INV_ORDER 'op:minor (std-minor 17)))
      (op:destroy poa1 t t)
      (handler-case
        (progn (op:find_POA root-poa "p1" nil)
               (ensure nil))
        (PORTABLESERVER:POA/ADAPTERNONEXISTENT ()))
      (ensure-eql (gethash id1 poa-map) nil)
      (ensure-eql (gethash id2 poa-map) nil)
      (ensure (not (member poa1 (managed-poas (op:the_poamanager poa1))))
              "UnRegistered with the manger")
      (ensure-exception
       (op:create_POA poa1 "p9" nil nil)
       CORBA:OBJECT_NOT_EXIST)))


  (define-test "POA Activator"
    (defclass test-activator (PortableServer:ADAPTERACTIVATOR)
      ())
    (define-method "UNKNOWN_ADAPTER" ((OBJ test-activator) _parent _name)
      (op:create_poa _parent _name nil nil )
      t)
    (setf (op:the_activator root-poa) (make-instance 'test-activator))
    (let ((name "foo"))
      (let ((poa (op:find_poa root-poa name t)))
        (ensure-typep poa 'PortableServer:POA)
        (ensure-equalp (op:the_name poa) name))))


  (define-test "set_servant_manager"
    (ensure-exception (op:set_servant_manager root-poa nil)
                      portableserver:poa/wrongpolicy  )
    (let ((poa (op:create_POA root-poa "p" nil
                              (list (op:create_request_processing_policy root-poa :use_servant_manager)
                                    (op:create_servant_retention_policy root-poa :retain)))))
      (ensure-exception
       (op:set_servant_manager poa nil)
       CORBA:OBJ_ADAPTER 'op:minor (std-minor 4))
      (let ((manager (make-instance 'PortableServer:ServantActivator)))
        (op:set_servant_manager poa manager)
        (ensure-exception   ; only set once
         (op:set_servant_manager poa manager)
         CORBA:BAD_INV_ORDER 'op:minor (std-minor 6)))))


  (define-test "get_servant"
    (handler-case (progn (op:get_servant root-poa)
                         (ensure nil "get_servant on Wrong Policy POA."))
      (portableserver:poa/wrongpolicy ()))
    (let ((poa (op:create_POA root-poa "p" nil
                              (list (op:create_request_processing_policy
                                     root-poa :use_default_servant)))))
      (ensure-exception (op:get_servant poa)
                        Portableserver:POA/Noservant)))


  (define-test "reference creation"
    (let ((id "IDL:myObj:1.0")
          (user-poa (op:create_POA root-poa "user" nil
                                   (list (op:create_id_assignment_policy root-poa :user_id)))))
      (let ((obj (op:create_reference root-poa id)))
        (ensure-pattern* obj 'proxy-id id 'the-orb orb)
        (ensure-typep obj 'CORBA:Proxy))
      (let* ((oid (string-to-oid "foo"))
             (obj (op:create_reference_with_id user-poa oid id)))
        (ensure-typep obj 'CORBA:Proxy)
        (ensure-equalp (proxy-id obj) id)
        (let ((ior-string (op:object_to_string orb obj)))
          (let ((obj2 (op:string_to_object orb ior-string)))
            (ensure (op:_is_equivalent obj obj2))
            ;;(break "obj: ~S ~S" obj obj2)
            (ensure-equalp (proxy-id obj2) id)))
        (ensure-equalp (op:reference_to_id user-poa obj) oid ))))


  (define-test "Identity mapping"
    (let ((ref (op:create_reference root-poa "IDL:foo:1.0")))
      (let ((id (op:reference_to_id root-poa ref)))
        (ensure-typep id 'sequence)
        (ensure (every (lambda (item) (typep item 'CORBA:Octet)) id)))))


;;;; Servant Activator
  (define-test "Servant Activator"
    (let* ((poa (op:create_POA root-poa "p" nil
                               (list (op:create_request_processing_policy root-poa :use_servant_manager)
                                     (op:create_servant_retention_policy root-poa :retain) )))
           (my-oid '(18))
           (activator (make-instance 'mock-activator
                        :adapter poa
                        :expected-oid my-oid )))
      (op:set_servant_manager poa activator)
      (op:activate (op:the_poamanager poa))
      (test-poa-invoke poa :operation "_non_existent" :oid my-oid :args '())
      ;; pattern for etheralize arguments
      (setf (slot-value activator 'etheralize-verifier)
            (sexp-pattern
             `(,(sequence-pattern 18) ,poa ,(slot-value activator 'created-servant) t nil)))
      (op:deactivate (op:the_poamanager poa) t t)
      (ensure-eql (slot-value activator 'etheralize-called) 1)
      ;; Again, but with destroy and extra servant
      (setq poa (op:create_POA root-poa "p2" nil
                               (list (op:create_request_processing_policy root-poa :use_servant_manager)
                                     (op:create_servant_retention_policy root-poa :retain)
                                     (op:create_id_uniqueness_policy root-poa :multiple_id))))
      (setf (slot-value activator 'adapter) poa)
      (op:set_servant_manager poa activator)
      (op:activate (op:the_poamanager poa))
      (test-poa-invoke poa :operation "_non_existent" :oid my-oid :args '())
      (op:activate_object_with_id poa '(17) (slot-value activator 'created-servant))
      (setf (slot-value activator 'etheralize-verifier)
            (sexp-pattern
             `(,(sequence-pattern 17) ,poa ,(slot-value activator 'created-servant) nil t)))
      (op:deactivate_object poa '(17))
      ;; pattern for etheralize arguments
      (setf (slot-value activator 'etheralize-verifier)
            (sexp-pattern
             `(,(sequence-pattern 18) ,poa ,(slot-value activator 'created-servant) t nil)))
      (op:destroy poa t t)
      (ensure-eql (slot-value activator 'etheralize-called) 3) ))


;;;; State changes

  (define-test "Hold and activate"
    (let* ((poa-1 (op:create_POA root-poa "a1" nil nil))
           (poa-2 (op:create_POA root-poa "a2" (op:the_poamanager poa-1) nil)))
      (op:activate (op:the_poamanager root-poa))
      (op:hold_requests (op:the_poamanager poa-1) nil)
      (let ((r1 (test-poa-dispatch root-poa :operation "foo" :poa-spec '("a1")))
            (r2 (test-poa-dispatch root-poa :operation "foo" :poa-spec '("a2"))))
        (ensure-eql (request-state r1) :wait)
        (ensure-eql (request-state r2) :wait)
        (ensure (queue-memeber-p (poa-request-queue poa-1) r1))
        (ensure (queue-memeber-p (poa-request-queue poa-2) r2))
        (op:activate (op:the_poamanager poa-1))
        ;; orb-wait ??
        (ensure-eql (request-state r1) :finished)
        (ensure-eql (request-state r2) :finished))))


  (define-test "Hold and discard"
    (let* ((oid #(1))
           (proxy (op:activate_object_with_id root-poa oid (make-instance 'null-servant)))
           (r1 (test-poa-dispatch root-poa :operation "_is_a" :args '("foo") :oid oid)))
      (declare (ignore proxy))
      (ensure-eql (request-state r1) :wait)
      (op:discard_requests (op:the_poamanager root-poa) t)
      (ensure-pattern* r1
                       'request-state :finished
                       'request-exception (pattern 'identity (isa 'CORBA:TRANSIENT)
                                                   'system-exception-minor (std-minor 1)))))


;;;; Reentrancy

  (define-test "Executing request recorded in POA"
    (let* ((servant (make-instance 'mock-dsi-servant ))
           (oid (op:activate_object root-poa servant)))
      ;; FIXME: perhaps check request id in executing-requests
      (setf (plug-op servant)
            (lambda (x)
              (ensure (executing-requests root-poa))
              (string-upcase x)))
      (setup-test-in)
      (test-poa-invoke root-poa
                       :operation "op"
                       :oid oid
                       :args (list "hello"))
      (test-read-response :orb orb
                          :message (list :message-type :reply
                                         'giop:replyheader
                                         (pattern 'op:reply_status :no_exception)
                                         corba:tc_string
                                         "HELLO"))
      (ensure (not (executing-requests root-poa)))))

  (define-test "Recursive call to :single_thread_model"
    (setup-test-in)
    (let* ((poa (op:create_poa root-poa "single" 
                               (op:the_poamanager root-poa)
                               (list (op:create_thread_policy root-poa :single_thread_model))))
           (oid #(1))
           (servant (make-instance 'mock-dsi-servant))
           (proxy (op:activate_object_with_id poa oid servant)))
      (declare (ignore proxy))
      (op:activate (op:the_poamanager root-poa))
      (flet ((call-it (arg)
               (test-poa-dispatch poa :poa-spec '()
                                  :operation "op" :oid oid :args (list arg))))
        (setf (plug-op servant)
              (lambda (x)
                (when (equal x "again")
                  (call-it "stop")
                  (test-read-response :orb orb
                                      :message 
                                      (list :message-type :reply
                                            'giop:replyheader
                                            (pattern 'op:reply_status :system_exception)
                                            corba:tc_string
                                            "IDL:omg.org/CORBA/TRANSIENT:1.0")))
                "foo"))
        (call-it "again")
        (test-read-response :orb orb
                            :message
                            (list :message-type :reply
                                  'giop:replyheader
                                  (pattern 'op:reply_status :no_exception)
                                  corba:tc_string
                                  "foo")))))
    
    
    
    #| end suite |# )
