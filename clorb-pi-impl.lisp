;;;; clorb-pi-impl.lisp -- PortableInterceptors implementation

(in-package :clorb)


;; clorb-pi-base.lisp consists of the generated code from the Portable
;; Interceptors IDL.

#| How to generate clorb-pi-base.lisp anew:
 (CORBA:IDL "clorb:idl;pi.idl" 
           :eval nil :package-decl nil
           :output "clorb:src;y-clorb-pi-base.lisp" )
|#



;;;; ORB Subclass for PortableInterceptors

(defclass pi-orb (clorb-orb)
  ((client-request-interceptors :accessor client-request-interceptors
                                :initform nil)
   (server-request-interceptors :accessor server-request-interceptors
                                :initform nil)))

;; use pi-orb as orb class
(when (not (subtypep *orb-class* 'pi-orb))
  (setq *orb-class* 'pi-orb))



;;;; local interface ORBInitInfo
;;    readonly attribute CORBA::StringSeq arguments;
;;    readonly attribute string orb_id;
;;    readonly attribute IOP::CodecFactory codec_factory;

(define-corba-class orb-init-info (PortableInterceptor:ORBInitInfo)
  :attributes ((arguments :readonly)
               (orb_id :readonly)
               (codec_factory :readonly))
  :slots ((proto-orb :initarg :orb :reader the-orb)))


(defmethod create-orb-init-info ((orb pi-orb) args orbid)
  (make-instance 'orb-init-info
    :orb orb
    :arguments args
    :orb_id orbid
    :codec_factory nil ))


;;;    void register_initial_reference (in ObjectId id, in Object obj)
;;      raises (InvalidName);

(define-method register_initial_reference ((orbinfo orb-init-info) id obj)
  (handler-case
      (op:register_initial_reference (the-orb orbinfo) id obj)
    (CORBA:ORB/InvalidName ()
      (error (PORTABLEINTERCEPTOR:ORBINITINFO/INVALIDNAME)))))
  

;;;    void resolve_initial_references (in ObjectId id)
;;      raises (InvalidName);

(define-method resolve_initial_references ((orbinfo orb-init-info) id)
  (handler-case
      (op:resolve_initial_references (the-orb orbinfo) id)
    (CORBA:ORB/InvalidName ()
      (error (PORTABLEINTERCEPTOR:ORBINITINFO/INVALIDNAME)))))




;;;; PortableInterceptor::ORBInitializer
;;  local interface ORBInitializer {
;;    void pre_init (in ORBInitInfo info);
;;    void post_init (in ORBInitInfo info);

;; void register_orb_initializer (in ORBInitializer init); 

(defun PortableInterceptor:register_orb_initializer (init)
  (pushnew init *orb-initializers*))



;;;; Request Info Classes
;; local interface RequestInfo {
;;   readonly attribute unsigned long request_id;
;;   readonly attribute string operation;
;;   readonly attribute Dynamic::ParameterList arguments;
;;   readonly attribute Dynamic::ExceptionList exceptions;
;;   readonly attribute Dynamic::ContextList contexts;
;;   readonly attribute Dynamic::RequestContext operation_context;
;;   readonly attribute any result;
;;   readonly attribute boolean response_expected;
;;   readonly attribute Messaging::SyncScope sync_scope;
;;   readonly attribute ReplyStatus reply_status;
;;   readonly attribute Object forward_reference;
;;   any get_slot (in SlotId id) raises (InvalidSlot);
;;   IOP::ServiceContext get_request_service_context (
;;              in IOP::ServiceId id);
;;   IOP::ServiceContext get_reply_service_context (
;;              in IOP::ServiceId id);


(defclass base-request-info ()
  ((request  :initarg :request  :accessor the-request)))


;; Helpers

(defun get-service-context (id service-context-list)
  (or (find id service-context-list :key #'op:context_id)
      (raise-system-exception 'CORBA:bad_param 23)))


;;   readonly attribute unsigned long request_id;

(define-method "REQUEST_ID" ((self base-request-info))
  (request-id (the-request self)))


;;   readonly attribute string operation;

(define-method "OPERATION" ((self base-request-info))
  (request-operation (the-request self)))


;;   readonly attribute Dynamic::ParameterList arguments;

(define-method "ARGUMENTS" ((self base-request-info))
  (loop for (any . mode) in (dynamic-arguments (the-request self))
        collect (Dynamic:Parameter :argument any :mode mode)))


;;   readonly attribute Dynamic::ExceptionList exceptions;

(define-method "EXCEPTIONS" ((self base-request-info))
   (request-exceptions (the-request self)))


;;   readonly attribute Dynamic::ContextList contexts;

(define-method "CONTEXTS" ((self base-request-info))
  (raise-system-exception 'CORBA:no_resources))


;;   readonly attribute Dynamic::RequestContext operation_context;

(define-method "OPERATION_CONTEXT" ((self base-request-info))
  (raise-system-exception 'CORBA:no_resources))


;;   readonly attribute any result;

(define-method "RESULT" ((self base-request-info))
  (dynamic-result (the-request self)))


;;   readonly attribute boolean response_expected;

(define-method "RESPONSE_EXPECTED" ((self base-request-info))
  (response-expected (the-request self)))


;;   readonly attribute Messaging::SyncScope sync_scope;

(define-method "SYNC_SCOPE" ((self base-request-info))
  ;;FIXME: Message::SYNE_NONE
  nil)


;;   readonly attribute ReplyStatus reply_status;

;;   readonly attribute Object forward_reference;

;;   any get_slot (in SlotId id) raises (InvalidSlot);

(define-method "GET_SLOT" ((self base-request-info) ID)
  (DECLARE (IGNORE ID))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))


;;   IOP::ServiceContext get_request_service_context (
;;              in IOP::ServiceId id);

(define-method "GET_REQUEST_SERVICE_CONTEXT" ((self base-request-info) id)
  (get-service-context id (service-context-list (the-request self))))


;;   IOP::ServiceContext get_reply_service_context (
;;              in IOP::ServiceId id);

(define-method "GET_REPLY_SERVICE_CONTEXT" ((self base-request-info) id)
  (get-service-context id (reply-service-context (the-request self))))








;;;; Request Classes 

;; Mixin for requests that support interceptors

(defclass pi-request ()
  ((flow-stack   :accessor flow-stack  :initform nil)
   (request-info)))

(defgeneric request-info (pi-request))


(defclass pi-client-request (pi-request client-request)
  ())

(defclass pi-server-request (pi-request server-request)
  ())



(defmethod request-info ((self pi-client-request))
  (with-cache-slot (self request-info)
    (make-instance 'client-request-info :request self)))

(defmethod request-info ((self pi-server-request))
  (with-cache-slot (self request-info)
    (make-instance 'server-request-info :request self)))


(defgeneric run-interceptors (req list operation))
(defgeneric rerun-interceptors (req operation))
(defgeneric pop-interceptors (req operation))

(defmethod run-interceptors ((self pi-request) interceptors operation)
  (setf (flow-stack self) nil)
  (dolist (interceptor interceptors)
    (funcall operation interceptor (request-info self))
    (push interceptor (flow-stack self))))

(defmethod rerun-interceptors ((self pi-request) operation)
  (dolist (interceptor (flow-stack self))
    (funcall operation interceptor (request-info self))))

(defmethod pop-interceptors ((self pi-request) operation)
  (loop while (flow-stack self)
        do (funcall operation (pop (flow-stack self)) (request-info self))))


;;; to handle locate request (for the time)
(defmethod rerun-interceptors ((self function) operation)
  (declare (ignore operation)))



;;;; ClientRequestInfo methods

(defclass client-request-info (base-request-info
                               PortableInterceptor:ClientRequestInfo)
  ())


;; Base overrides

(define-method "REPLY_STATUS" ((self client-request-info))
#| FIXME:
This attribute describes the state of the result of the operation invocation. 
Its value can be one of the following:
PortableInterceptor::SUCCESSFUL
PortableInterceptor::SYSTEM_EXCEPTION
PortableInterceptor::USER_EXCEPTION
PortableInterceptor::LOCATION_FORWARD
PortableInterceptor::TRANSPORT_RETRY
|#
  (ecase (request-status (the-request self))
    ((:user_exception) PortableInterceptor::USER_EXCEPTION)
    ((:no_exception) PortableInterceptor::SUCCESSFUL)
    ((:system_exception) PortableInterceptor::SYSTEM_EXCEPTION)
    ((:location_forward) PortableInterceptor::LOCATION_FORWARD)))


(define-method "FORWARD_REFERENCE" ((self client-request-info))
  (object-forward (request-target (the-request self))))  



;; Local methods


(define-method "TARGET" ((self client-request-info))
   (request-target (the-request self)))

(define-method "EFFECTIVE_TARGET" ((self client-request-info))
  (let ((target (request-target (the-request self))))
    (or (object-forward target) target)))

;; readonly attribute IOP::TaggedProfile effective_profile;
(define-method "EFFECTIVE_PROFILE" ((self client-request-info))
  (let* ((req (the-request self))
         (p (request-effective-profile req))
         (obj (op:effective_target self))
         (n (position p (object-profiles obj))))
    (elt (raw-profiles obj) n)))


;;; readonly attribute any received_exception;

;; This attribute is an any that contains the exception to be returned to the client.

;; If the exception is a user exception that cannot be inserted into an any (for
;; example, it is unknown or the bindings don't provide the TypeCode), then this
;; attribute will be an any containing the system exception UNKNOWN with 
;; a standard minor code of 1. However, the RepositoryId of the exception is available
;; in the received_exception_id attribute.

(define-method "RECEIVED_EXCEPTION" ((self client-request-info))
  (let ((exc (request-exception (the-request self))))
    (CORBA:Any :any-value exc
               :any-typecode (any-typecode exc))))


;;; readonly attribute CORBA::RepositoryId received_exception_id;
;; This attribute is the CORBA::RepositoryId of the exception to be returned to the client.

(define-method "RECEIVED_EXCEPTION_ID" ((self client-request-info))
  (request-exception-id (the-request self)))


(define-method "GET_EFFECTIVE_COMPONENT" ((self client-request-info) _ID)
  (DECLARE (IGNORE _ID))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method "GET_EFFECTIVE_COMPONENTS" ((self client-request-info) _ID)
  (DECLARE (IGNORE _ID))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method "GET_REQUEST_POLICY" ((self client-request-info)
                                     _TYPE)
  (DECLARE (IGNORE _TYPE))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(defmacro %add-service-context (req context-accessor service_context replace)
  `(let ((service_context ,service_context)
         (replace ,replace)
         (self ,req))
     (let* ((list (,context-accessor self))
            (old (find (op:context_id service_context) list
                       :key #'op:context_id)))
       (when old
         (unless replace
           (raise-system-exception 'CORBA:bad_inv_order 11 :completed_no))
         (setf list (delete old list)))
       (setf (,context-accessor self) (cons service_context list)))))

(define-method "ADD_REQUEST_SERVICE_CONTEXT" ((self client-request-info)
                                                    service_context replace)
  (%add-service-context (the-request self) service-context-list 
                        service_context replace))




;;;; ServerRequestInfo methods

(defclass server-request-info (base-request-info
                               PortableInterceptor:ServerRequestInfo)
  ())


;; Base overrides

(define-method "REPLY_STATUS" ((self server-request-info))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))


(define-method "FORWARD_REFERENCE" ((self server-request-info))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))


;; Local methods

(define-method "SENDING_EXCEPTION" ((self server-request-info))
  (request-exception (the-request self)))

(define-method "OBJECT_ID" ((self server-request-info))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method "ADAPTER_ID" ((self server-request-info))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method "TARGET_MOST_DERIVED_INTERFACE" ((self server-request-info))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method "GET_SERVER_POLICY" ((self server-request-info) _TYPE)
  (DECLARE (IGNORE _TYPE))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method "SET_SLOT" ((self server-request-info) _ID _DATA)
  (DECLARE (IGNORE _ID _DATA))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method "TARGET_IS_A" ((self server-request-info) _ID)
  (DECLARE (IGNORE _ID))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method "ADD_REPLY_SERVICE_CONTEXT" ((self server-request-info)
                                            service_context replace)
  (%add-service-context (the-request self) reply-service-context
                        service_context replace))



;;;; ORB Operations for interceptors


(defmethod create-client-request ((orb pi-orb) &rest initargs)
  (apply #'make-instance 'pi-client-request 
         :the-orb orb initargs))

(defmethod create-server-request ((orb pi-orb) &rest initargs)
  (apply #'make-instance 'pi-server-request 
         :the-orb orb initargs))


(defmethod will-send-request ((orb pi-orb) client-request)
  (run-interceptors client-request (client-request-interceptors orb)
                    #'op:send_request)
  (call-next-method))

(defmethod has-received-exception ((orb pi-orb) client-request)
  (call-next-method)
  (pop-interceptors client-request #'op:receive_exception))

(defmethod has-received-reply ((orb pi-orb) client-request)
  (call-next-method)
  (pop-interceptors client-request #'op:receive_reply))

(defmethod has-received-other ((orb pi-orb) client-request)
  (call-next-method)
  (pop-interceptors client-request #'op:receive_other))

(defmethod has-received-request-header ((orb pi-orb) server-request)
  (call-next-method)
  (run-interceptors server-request (server-request-interceptors orb)
                    #'op:receive_request_service_contexts))

(defmethod has-received-request ((orb pi-orb) server-request)
  (call-next-method)
  (rerun-interceptors server-request #'op:receive_request))

(defmethod will-send-exception ((orb pi-orb) server-request)
  (loop 
    while (handler-case
            (progn (pop-interceptors server-request #'op:send_exception)
                   nil)
            (systemexception (exc)
                             (set-request-exception server-request exc))))
  (call-next-method))

(defmethod will-send-reply ((orb pi-orb) server-request)
  (pop-interceptors server-request #'op:send_reply)
  (call-next-method))

(defmethod will-send-other ((orb pi-orb) server-request)
  (pop-interceptors server-request #'op:send_other)
  (call-next-method))



;;;; PortableInterceptor:ORBInitInfo operations


;;;    void add_client_request_interceptor (in ClientRequestInterceptor interceptor) 
;;      raises (DuplicateName);

(define-method "ADD_CLIENT_REQUEST_INTERCEPTOR" ((self orb-init-info) interceptor)
  (when (find (op:name interceptor) (client-request-interceptors (the-orb self)))
    (error (portableinterceptor:orbinitinfo/duplicatename 
            :name (op:name interceptor))))
  (push interceptor (client-request-interceptors (the-orb self))))


;;;    void add_server_request_interceptor (in ServerRequestInterceptor interceptor)
;;      raises (DuplicateName);

(define-method add_server_request_interceptor ((self orb-init-info) interceptor)
  (when (find (op:name interceptor) (server-request-interceptors (the-orb self)))
    (error (portableinterceptor:orbinitinfo/duplicatename 
            :name (op:name interceptor))))
  (push interceptor (server-request-interceptors (the-orb self))))


;;;    void add_ior_interceptor (in IORInterceptor interceptor)
;;      raises (DuplicateName);

;;;    SlotId allocate_slot_id ();

;;;    void register_policy_factory (in CORBA::PolicyType type,
;;                                  in PolicyFactory policy_factory);



;;;; Test interceptor

(defclass my-client-interceptor (portableinterceptor:clientrequestinterceptor)
  ((name :initarg :name)))

(define-method name ((self my-client-interceptor))
  (slot-value self 'name))

(defun args-except (info mode)
  (loop for a in (op:arguments info)
     unless (eql (op:mode a) mode)
     collect (any-value (op:argument a))))

(define-method "SEND_REQUEST" ((self my-client-interceptor) info)
  (mess 3 "SEND_REQUEST: ~S ~S ~S"
        (op:operation info)
        (op:effective_target info)
        (args-except info :param_out))
  (mess 3 "effective profile ~S" (op:effective_profile info))
  '(when (equal (op:operation info) "resolve")
    (describe info)
    (break "resolve"))
  (op:add_request_service_context 
   info 
   (iop:ServiceContext :context_id 17 :context_data #(1))
   nil))

;; (define-method "SEND_POLL" ((self my-client-interceptor) info)
;;  (declare (ignore info)))


(define-method "RECEIVE_REPLY" ((self my-client-interceptor) info)
  (mess 3 "RECEIVE_REPLY: ~S ~S Res=~S"
        (op:reply_status info)
        (ignore-errors (op:get_reply_service_context info 17))
        (op:result info)))


(define-method "RECEIVE_EXCEPTION" ((self my-client-interceptor) info)
  (mess 3 "RECEIVE_EXCEPTION: ~S ~S ~S"
        (op:reply_status info)
        (op:received_exception_id info)
        (ignore-errors (op:get_reply_service_context info 17))))


(define-method "RECEIVE_OTHER" ((self my-client-interceptor) info)
  (mess 3 "RECEIVE_OTHER: ~S ~S" (op:reply_status info)
        (case (op:reply_status info)
          ((#.PortableInterceptor::SYSTEM_EXCEPTION #.PortableInterceptor::USER_EXCEPTION)
           (op:received_exception_id info))
          ((#.portableinterceptor:location_forward)
           (op:forward_reference info)))))
  


(defclass my-server-interceptor (PortableInterceptor:ServerRequestInterceptor)
  ((name :initarg :name)))

(define-method RECEIVE_REQUEST_SERVICE_CONTEXTS ((self my-server-interceptor) info)
  (mess 3 "RECEIVE_REQUEST_SERVICE_CONTEXTS: ~S" 
        (ignore-errors (op:get_request_service_context info 17))
        info))

(define-method RECEIVE_REQUEST ((self my-server-interceptor) info)
  (mess 3 "RECEIVE_REQUEST: ~S ~S"
        (op:operation info) 
        (args-except info :param_out)))

(define-method SEND_REPLY ((self my-server-interceptor) info)
  (mess 3 "SEND_REPLY: Result=~S out-args=~S" 
        (op:result info)
        (args-except info :param_in))
  (op:add_reply_service_context
   info
   (iop:servicecontext
    :context_id 17
    :context_data #(17 47))
   nil))


(define-method SEND_EXCEPTION ((self my-server-interceptor) info)
  (mess 3 "SEND_EXCEPTION: ~S" info))

(define-method SEND_OTHER ((self my-server-interceptor) info)
  (mess 3 "SEND_OTHER: ~S" info))



(defvar *my-client-interceptor* (make-instance 'my-client-interceptor
                                      :name "Test client-interceptor"))

(defvar *my-server-interceptor* (make-instance 'my-server-interceptor
                                      :name "My Server Interceptor"))


#|
(change-class *the-orb* 'pi-orb)
(pushnew *my-client-interceptor* (client-request-interceptors *the-orb*))
(pushnew *my-server-interceptor* (server-request-interceptors *the-orb*))
|#
