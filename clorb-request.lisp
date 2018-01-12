;;;; clorb-request.lisp -- Client Request

(in-package :clorb)



;;;; CORBA:Request

(defclass CORBA:Request ()
  ())



;;;; Client-Request 

(defclass client-request (CORBA:Request synchronized-lazy)
  ((the-orb
    :initarg :the-orb
    :reader the-orb)
   (target
    :initarg :target
    :reader request-target)
   (operation
    :initarg :operation
    :reader request-operation)
   (paramlist
    :initarg :paramlist             :initform nil 
    :accessor request-paramlist
    :documentation "result + arguments") 
   (request-id
    :initarg :request-id
    :accessor request-id)
   (connection
    :initarg :connection            :initform nil
    :accessor request-connection)
   (service-context-list
    :initarg :service-context-list  :initform nil
    :accessor service-context-list)
   (reply-service-context
    :initarg :reply-service-context :initform nil
    :accessor reply-service-context)
   (response-expected
    :initarg :response-expected     :initform t
    :accessor response-expected)
   (effective-profile
    :accessor request-effective-profile)
   (status
    :initarg :status                :initform :initial
    :accessor request-status)
   (status-notify-callback
    :initarg status-notify-callback :initform 'synch-notify
    :accessor status-notify-callback)
   (buffer
    :initarg :buffer                :initform nil
    :accessor request-buffer)
   (output-func
    :initarg :output-func           :initform 'dii-output-func
    :accessor output-func)
   (args
    :initarg :args
    :accessor request-args)
   (params                              ; List of formals (name mode tc)*
    :initarg :params
    :accessor request-params)
   (input-func
    :initarg :input-func            :initform 'dii-input-func
    :accessor input-func)
   (error-handler
    :initarg :error-handler         :initform 'dii-error-handler
    :accessor error-handler)
   (exception-id
    :initarg :exception-id          :initform nil 
    :accessor request-exception-id
    :documentation "Reply exception repository ID")
   (exception
    :initarg :exception             :initform nil 
    :accessor request-exception
    :documentation "Reply exception")
   (exceptions
    :initarg :exceptions            :initform nil
    :accessor request-exceptions
    :documentation "Valid exceptions")))


(defmethod initialize-instance :before ((req client-request)
                                        &key (the-orb nil orb-p))
  (declare (ignore the-orb))
  (unless orb-p (error ":the-orb not supplied to client-request")))


(defmethod status-notify ((req client-request))
  (funcall (status-notify-callback req) req))


(defgeneric create-client-request (orb &rest initargs))


(define-method target ((r client-request))
  (request-target r))

(define-method operation ((r client-request))
  (request-operation r))

(define-method ctx ((r client-request))
  (raise-system-exception 'CORBA:no_implement))

(define-method result ((r client-request))
  (first (request-paramlist r)))

(define-method set_return_type ((r client-request) tc)
  (setf (any-typecode (op:argument (first (request-paramlist r)))) tc))

(define-method return_value ((r client-request))
  (let ((params (request-paramlist r)))
    (unless params
      (setf params (list (CORBA:NamedValue :argument (CORBA:Any)
                                           :arg_modes ARG_OUT)))
      (setf (request-paramlist r) params))
    (let ((result (op:argument (first params))))
      (when (request-exception r)
        (setf (any-value result) (request-exception r)))
      result)))

(define-method arguments ((r client-request))
  (cdr (request-paramlist r)))


(defmethod dynamic-arguments ((req client-request))
  "Arguments for the request.
List of (any . mode) for every argument. Only valid after arguments have 
been decoded."
  (cond ((request-paramlist req)        ; DII arguments
         (map 'list (lambda (nv) (cons (op:argument nv) (op:mode nv)))
              (op:arguments req)))
        ((slot-boundp req 'params)
         (let ((args (request-args req)))
           (loop for (nil mode tc) in (request-params req)
                 collect (cons (CORBA:Any :any-value (if (not (eql mode :param_out))
                                                       (pop args))
                                          :any-typecode tc)
                               mode))))
        (t (raise-system-exception 'CORBA:no_resources))))


(defmethod dynamic-result ((self client-request))
  (op:return_value self))


(defun add-arg (req name mode &optional typecode value)
  (when (or value typecode)
    (check-type typecode CORBA:TypeCode))
  (let ((arg (CORBA:Any :any-typecode typecode
                        :any-value value)))
    (setf (request-paramlist req)
      (nconc (request-paramlist req) 
             (list (CORBA:NamedValue
                    :name name
                    :argument arg
                    :arg_modes mode))))
    arg))

(define-method add_in_arg ((req client-request))
  (add-arg req nil ARG_IN))

(define-method add_named_in_arg ((req client-request) name)
  (add-arg req name ARG_IN))

(define-method add_inout_arg ((req client-request))
  (add-arg req nil ARG_INOUT))

(define-method add_named_inout_arg ((req client-request) name)
  (add-arg req name ARG_INOUT))

(define-method add_out_arg ((req client-request) &optional (name ""))
  (add-arg req name ARG_OUT))

(define-method add_named_out_arg ((req client-request) name)
  (add-arg req name ARG_OUT))

(defun add-exception (req typecode)
  (push typecode (request-exceptions req)))



;; Used in stubs, shorter code then op:_create_request
(defun request-create (obj operation result-type )
  (create-client-request
   (the-orb obj)
   :target obj
   :operation operation
   :paramlist (list (CORBA:NamedValue :arg_modes ARG_OUT
                                      :argument (CORBA:Any :any-typecode result-type)))))


;;;; Sending requests


(defun request-start-request (req)
  "Prepare for marshalling and sending a request.
Returns: connection, request-id, buffer"
  (setf (request-status req) nil)
  (let ((object (request-target req)))
    (let ((conn (get-object-connection object)))
      (unless conn
        (raise-system-exception 'corba:transient 2 :completed_no))
      (setf (request-connection req) conn)
      (let ((req-id (next-request-id conn)))
        (setf (request-id req) req-id)
        (setf (request-effective-profile req) (effective-profile object))
        (values conn req-id (connection-get-buffer conn))))))


(defun static-error-handler (condition)
  (error condition))


(defvar *call-hook* nil)

(defun do-static-call (obj operation response-expected
                       output-func input-func exceptions
                       &optional args params)
  (let ((req (create-client-request
              (the-orb obj)
              :target obj
              :operation operation
              :response-expected response-expected
              :exceptions exceptions
              :output-func output-func
              :input-func input-func 
              :args args
              :params params
              :error-handler #'static-error-handler)))
    (request-send req)
    (if *call-hook* 
      (funcall *call-hook* req)
      (when response-expected
        (request-get-response req)))))


(defun request-invoke (req)
  (request-send req)
  (request-get-response req))


(defvar *send-hook* nil)

(defun request-send (req)
  (multiple-value-bind (connection request-id buffer)
      (request-start-request req)
    (setf (service-context-list req) nil)
    (when *send-hook* (funcall *send-hook* req))
    (will-send-request (the-orb req) req)
    (marshal-request-message buffer request-id (service-context-list req)
                             (response-expected req)
                             (request-effective-profile req)
                             (request-operation req)
                             (output-func req) req)
    (connection-send-request connection buffer
                             (if (response-expected req) req))))


;;; void send_oneway ()

;; Calling send on a request after invoke, send, or
;; send_multiple_requests for that request was called raises
;; BAD_INV_ORDER with standard minor code 10.

(define-method send_oneway ((req client-request))
  (unless (eql (request-status req) :initial)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 10 :completed_no))
  (setf (response-expected req) nil)
  (request-send req))


;;; void send_deferred ()

(define-method send_deferred ((req client-request))
  (unless (eql (request-status req) :initial)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 10 :completed_no))
  (request-send req)
  (add-pending-client-request (the-orb req) req))




;;;; Response handling


(defun request-locate-reply (req status buffer)
  (with-synchronization req
    (setf (request-status req) status)
    (setf (request-buffer req) buffer)
    (status-notify req)))


(defun request-reply (req status buffer service-context)
  ;; Callback from reading and decoding reply message
  (with-synchronization req
    (setf (request-status req) status)
    (setf (request-buffer req) buffer)
    (setf (reply-service-context req) service-context)
    (case status
      (:system_exception
       (multiple-value-bind (exc id)
           (unmarshal-systemexception buffer)
         (setf (request-exception-id req) id)
         (setf (request-exception req) exc))))
    (status-notify req)))


(defun request-reply-exception (req status exception)
  ;; Connection close ..
  (with-synchronization req
    (unless (request-status req)
      (setf (request-status req) status)
      (setf (request-exception req) exception)
      (status-notify req))))


(defun request-no-write (req)
  (request-reply-exception req :error
                           (system-exception 'CORBA:TRANSIENT 0 :completed_no)))


(defun should-retry (req exc)
  ;; Returns Nil and retries OR returns status.
  ;;
  ;; This could be made more complicated. There could be per
  ;; object or per orb policies for retrying. Limit to the number
  ;; of retries. Backoff.
  (if (and (typep exc 'CORBA:TRANSIENT)
           (eql (system-exception-completed exc) :completed_no))
      (progn (has-received-other (the-orb req) req)
             (request-send req)
             nil)
      (request-status req)))

(defun request-poll (req)
  ;; Check if result of request is ready
  ;; might retransmit request, if redirected or transient exception
  (case (request-status req)
    (:location_forward
     (setf (object-forward (request-target req))
           (unmarshal-object (request-buffer req)))
     (has-received-other (the-orb req) req)
     (request-send req)
     nil)

    (:system_exception
     (should-retry req (request-exception req)))

    (:error                             ; Communication error
     (setf (request-status req) :system_exception)
     (let ((exc (request-exception req)))
       (setf (request-exception-id req)
             (exception-id exc))
       (should-retry req exc)))

    (otherwise 
     (request-status req))))


(defun request-wait-response (req)
  (loop
     ;; First wait till we have a (non-nil) status, then it is no
     ;; longer being read
     do (orb-condition-wait req #'request-status req)
     ;; Call request-poll to check if status requires immediate
     ;; action and retry
     until (request-poll req)))


(defun request-handle-error (req)
  (has-received-other (the-orb req) req)
  (setf (request-status req) :returned)
  (funcall (error-handler req) (request-exception req)))


(defun request-get-response (req)
  (request-wait-response req)
  (let ((buffer (request-buffer req)))
    (ecase (request-status req)
      (:initial
       (raise-system-exception 'CORBA:BAD_INV_ORDER))
      (:user_exception
       (let ((id (unmarshal-string buffer)))
         (setf (request-exception-id req) id)
         (let ((tc (find id (request-exceptions req)
                         :key #'op:id :test #'equal)))
           (setf (request-exception req) 
                 (cond (tc (unmarshal tc buffer))
                       (t
                        (setf (request-status req) :system_exception)
                        (system-exception 'corba:unknown 1 :completed_yes))))))
       (request-handle-error req))
      (:system_exception
       ;; unmarshalled and ready by request-poll
       (request-handle-error req))
      (:no_exception
       (multiple-value-prog1 (funcall (input-func req) req buffer)
         (has-received-reply (the-orb req) req)
         (setf (request-status req) :returned))))))


;;; void get_response () raises (WrongTransaction);

;; get_response returns the result of a request. If get_response is
;; called before the request has completed, it blocks until the
;; request has completed. Upon return, the out parameters and return
;; values defined in the Request are set appropriately and they may be
;; treated as if the Request invoke operation had been used to perform
;; the request.

;; A request has an associated transaction context if the thread
;; originating the request had a non-null transaction context and the
;; target object is a transactional object. The get_response operation
;; may raise the WrongTransaction exception if the request has an
;; associated transaction context, and the thread invoking
;; get_response either has a null transaction context or a non-null
;; transaction context that differs from that of the request.

(define-method op::get_response ((req client-request))
  (unless (remove-pending-request (the-orb req) req)
    ;; FIXME: ??
    (raise-system-exception 'CORBA:BAD_INV_ORDER))
  (request-get-response req)
  (values))


;;; boolean poll_response ();
;;
;; poll_response determines whether the request has completed. A TRUE return indicates
;; that it has; FALSE indicates it has not.
;;
;; Return is immediate, whether the response has completed or not. Values in the request are
;; not changed.

;; Exception BAD_INV_ORDER with minor
;;  * 11 - request not sendt
;;  * 12 - request already returned
;;  * 13 - request invoked

(define-method poll_response ((req client-request))
  ;; FIXME: 13
  (case (request-poll req)
    ((:initial) (raise-system-exception 'CORBA:BAD_INV_ORDER 11 :completed_no))
    ((:returned) (raise-system-exception 'CORBA:BAD_INV_ORDER 12 :completed_yes))
    ((nil) nil)
    (otherwise t)))


;;; void invoke ()

(define-method op::invoke ((req client-request))
  (request-send req)
  (request-get-response req)
  (values))


(defun request-funcall (req)
  (request-send req)
  (if *call-hook* 
      (funcall *call-hook* req)
      (when (response-expected req)
        (request-get-response req))))


(defun dii-output-func (req buffer)
  (loop for nv in (request-paramlist req)
        when (/= 0 (logand ARG_IN (op:arg_modes nv)))
        do (let ((any (op:argument nv)))
             (marshal (any-value any)
                      (any-typecode any)
                      buffer))))


(defun dii-input-func (req buffer)
  (let* ((paramlist (request-paramlist req))
         (results
          (loop for nv in paramlist
             when (/= 0 (logand ARG_OUT (op:arg_modes nv)))
             collect
             (let ((any (op:argument nv)))
               (setf (any-value any)
                     (unmarshal (any-typecode any) buffer)))))
         (result-type (any-typecode (op:argument (first paramlist)))))
    (values-list
     (if (eql :tk_void (typecode-kind result-type))
         (cdr results) results))))


(defun dii-error-handler (condition)
  (declare (ignore condition)))



;;;; Stub support


(defun symbol-op-info (sym)
  "Returns: name result params mode exc-syms"
  (values (get sym 'ifr-name)
          (get sym 'ifr-result)
          (get sym 'ifr-params)
          (get sym 'ifr-mode)
          (get sym 'ifr-exceptions)))


(defun symbol-attr-info (sym)
  "Returns: name type mode"
  (values (get sym 'ifr-name)
          (get sym 'ifr-type)
          (get sym 'ifr-mode)))


(defun compute-static-call (sym)
  (let (input-func output-func exceptions op response-expected params-list)
    (lambda (obj &rest args)
      (unless input-func
        (multiple-value-bind (name result params mode exc-syms)
                             (symbol-op-info sym)
          (setq params-list params)
          (setq input-func
                (let ((ufuns (loop for (nil pmode tc) in params
                                   unless (eql pmode :param_in) collect (unmarshal-function tc))))
                  (typecase result
                    (void-typecode)
                    (t (push (unmarshal-function result) ufuns)))
                  (lambda (req buffer)
                    (declare (ignore req))
                    (values-list (loop for u in ufuns collect (funcall u buffer))))))
          (setq output-func 
                (let ((mfuns (loop for (nil pmode tc) in params
                                   unless (eql pmode :param_out) collect (marshal-function tc))))
                  (lambda (req buffer)
                    (loop for a in (request-args req)
                          for m in mfuns do (funcall m a buffer)))))
          (setq exceptions (mapcar #'symbol-typecode exc-syms))
          (setq op name)
          (setq response-expected (eq mode :op_normal))))
      (do-static-call obj op response-expected output-func input-func
                      exceptions args params-list))))

(defun ignore2 (x y)
  (declare (ignore x y)))

(defun compute-static-get (sym)
  (let (op input-func)
    (lambda (obj)
      (unless input-func
        (multiple-value-bind (name type)
                             (symbol-attr-info sym)
          (setq op (getter-name name))
          (setq input-func
                (let ((mfun (unmarshal-function type)))
                  (lambda (req buffer)
                    (declare (ignore req))
                    (funcall mfun buffer))))))
      (do-static-call obj op t #'ignore2 input-func nil))))


(defun compute-static-set (sym)
  (let (op output-func params)
    (lambda (obj value)
      (unless output-func
        (multiple-value-bind (name type mode)
                             (symbol-attr-info sym)
          (assert (eql mode :attr_normal))
          (setq op (setter-name name))
          (setq params (list "value" type :param_in))
          (setq output-func
                (let ((mfun (marshal-function type)))
                  (lambda (req buffer)
                    (funcall mfun (request-args req) buffer))))))
      (do-static-call obj op t output-func #'ignore2 nil value params))))


;;;; Object stubs


(define-method _non_existent ((obj CORBA:Proxy))
  (handler-case
      (%jit-call CORBA::Object/_non_existent obj)
    (CORBA:OBJECT_NOT_EXIST ()
      t)))


(define-method _is_a ((obj CORBA:Proxy) interface-id)
  (or (object-is-a obj interface-id)
      (%jit-call CORBA::Object/_is_a obj interface-id)))


;;; clorb-request.lisp ends here
