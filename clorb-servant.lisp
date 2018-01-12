;;;; clorb-servant.lisp

(in-package :clorb)


;;;; Generic functions used by the POA implementation

(defgeneric servant-invoke (servant operation input handler)
  (:documentation "Called by the POA to perform an operation on the object"))

(defgeneric primary-interface (servant oid poa))



;;;; ServerRequest


(defclass CORBA:ServerRequest (CORBA:Object) ())




;;;; server-request class

(defclass server-request (CORBA:ServerRequest)
  ((the-orb         :initarg :the-orb         :reader the-orb)
   (poa                                       :accessor the-poa)
   (state           :initarg :state  :initform :wait  :accessor request-state)
   (request-id      :initarg :request-id      :accessor request-id)
   (operation       :initarg :operation       :accessor request-operation)
   (connection      :initarg :connection      :accessor request-connection)
   (response-flags  :initarg :response-flags  :accessor response-flags)
   (service-context :initarg :service-context :accessor service-context-list)
   (input           :initarg :input           :accessor request-input)
   (giop-version    :initarg :giop-version    :accessor giop-version)
   (object-id       :initarg :object-id       :accessor request-object-id)
   (poa-spec        :initarg :poa-spec        :accessor poa-spec)
   (kind            :initarg :kind  :initform :normal  :accessor request-kind)
   ;; Static skel
   (args            :initarg :args            :accessor request-args )
   (params          :initarg :params          :accessor request-params)
   (result-type     :initarg :result-type     :accessor request-result-type)
   (exceptions      :initarg :exceptions      :accessor request-exceptions)
   (out-funs        :initarg :out-funs        :accessor request-out-funs)
   ;; Dynamic
   (arguments       :initarg :arguments       :accessor request-arguments)
   ;; Common
   (result          :initarg :result          :accessor request-result)
   (reply-service-context :initform nil       :accessor reply-service-context)
   (exception             :initform nil       :accessor request-exception  )
   (reply-status          :initform nil       :accessor reply-status)
   (reply-buffer          :initform nil       :accessor reply-buffer)
   (forward               :initform nil       :accessor reply-forward)))

;; Request states:
;; - :wait  -- not yet started executing servant code
;; - :exec  -- executing
;; - :finished -- response sent (if any)
;; - :canceled -- request canceled, before it started executing
;; FIXME: should there be something between :exec and :finished ?


(defmethod create-server-request ((orb clorb-orb) &rest initargs)
  (apply #'make-instance 'server-request
         :the-orb orb initargs))


(defun set-request-args (sreq args params exc-list)
  (setf (request-args sreq) args
        (request-params sreq) params
        (request-exceptions sreq) exc-list)
  (has-received-request (the-orb sreq) sreq))

(defun set-request-result (sreq results result-type marshal-funs)
  (setf (request-result sreq) results
        (request-result-type sreq) result-type
        (request-out-funs sreq) marshal-funs))

(defun set-request-forward (req obj)
  (mess 3 "forwarding to ~A" obj)
  (setf (reply-forward req) obj))


(defun dsi-request-p (sreq)
  (not (slot-boundp sreq 'args)))

(defun arguments-set-p (self)
  (slot-boundp self 'arguments))

(defun exception-set-p (self)
  (request-exception self))

(defmethod set-request-exception ((req server-request) exc)
  (setf (request-exception req) exc))

(defmethod response-expected ((req server-request))
  (logbitp 0 (response-flags req)))


(defmethod dynamic-arguments ((req server-request))
  "Arguments for the request.
List of (any . mode) for every argument. Only valid after arguments have
been decoded."
  (cond ((arguments-set-p req)          ; DSI with arguments
         (map 'list (lambda (nv) (cons (op:argument nv) (op:mode nv)))
              (request-arguments req)))
        ((slot-boundp req 'params)
         (let* ((args (request-args req))
                (result-p (slot-boundp req 'result))
                (result (if result-p
                            (if (typep (request-result-type req) 'void-typecode)
                                (request-result req)
                                (cdr (request-result req))))))
           (loop for (nil mode tc) in (request-params req)
              collect (cons (CORBA:Any :any-value
                                       (if (or (eql mode :param_in)
                                               (not result-p))
                                           (pop args)
                                           (pop result))
                                       :any-typecode tc)
                            mode))))
        (t (raise-system-exception 'CORBA:no_resources))))


(defmethod dynamic-result ((req server-request))
  (cond ((not (slot-boundp req 'result))
         (raise-system-exception 'corba:no_resources))
        ((dsi-request-p req)
         (request-result req))
        (t
         (let ((type (request-result-type req)))
           (CORBA:Any :any-typecode type
                      :any-value (unless (typep type 'void-typecode)
                                   (first (request-result req))))))))


#+unused-function
(defun get-request-response (request status)
  (let* ((orb (the-orb request))
         (buffer (get-work-buffer orb)))
    (setf (reply-buffer request) buffer)
    (setf (reply-status request) status)
    (case status
      (:no_exception
       (will-send-reply orb request))
      (:user_exception
       (will-send-exception orb request))
      (:location_forward
       (will-send-other orb request))
      (:system_exception
       (will-send-exception orb request)))
    (marshal-giop-header :reply buffer)
    (marshal-service-context (reply-service-context request) buffer)
    (marshal-ulong (request-id request) buffer)
    (%jit-marshal status (symbol-typecode 'GIOP:REPLYSTATUSTYPE) buffer)
    buffer))


(defun compute-result-marshal (req)
  (cond ((dsi-request-p req)
         (lambda (req buffer)
           (let ((res (request-result req)))
             (when (and res (not (eq :tk_void (op:kind (any-typecode res)))))
               (marshal-any-value res buffer)))
           (loop for param in (request-arguments req)
                 unless (zerop (logand ARG_OUT (op:arg_modes param)))
                 do (marshal-any-value (op:argument param) buffer))))
        (t
         (lambda (req buffer)
           (loop for v in (request-result req)
                 for m in (request-out-funs req)
                 do (funcall m v buffer))))))


(defun check-valid-exception (req exception)
  (when (slot-boundp req 'exceptions)
    (unless (find (exception-id exception) (request-exceptions req)
                  :key #'op:id :test #'equal)
      (raise-system-exception 'CORBA:UNKNOWN 0 :completed_yes))))


(defun marshal-systemexception (condition buffer)
  (marshal-string (exception-id condition) buffer)
  (marshal-ulong  (system-exception-minor condition) buffer)
  (%jit-marshal (system-exception-completed condition) CORBA::TC_completion_status buffer))


(defun server-request-respond (req)
  (let ((orb (the-orb req))
        (conn (request-connection req))
        (exception (request-exception req))
        (forward (reply-forward req))
        (kind (request-kind req)))
    (labels ((send-reply (reply-type status result-func result-arg)
               (connection-reply conn (giop-version req) reply-type
                                 (request-id req) status (reply-service-context req)
                                 result-func result-arg)
               (setf (request-state req) :finished)
               (connection-remove-server-request conn req))
             (reply (status notifier result-func result-arg)
               (setf (reply-status req) status)
               (funcall notifier orb req)
               (send-reply :reply status result-func result-arg))
             (locate-reply (status &optional result-func result-arg)
               (send-reply :locatereply status result-func result-arg)))
      (cond ((eql kind :locate)
             (cond (exception (locate-reply :unknown_object))
                   (forward   (locate-reply :object_forward #'marshal-object forward))
                   (t         (locate-reply :object_here))))
            (forward
             (reply :location_forward #'will-send-other #'marshal-object forward))
            (exception
             (typecase exception
               (userexception
                (check-valid-exception req exception)
                (reply :user_exception #'will-send-exception #'marshal-any-value exception))
               (systemexception
                (reply :system_exception #'will-send-exception #'marshal-systemexception exception))))
            (t
             (reply :no_exception #'will-send-reply (compute-result-marshal req) req))))))


(defun server-request-systemexception-reply (req condition)
  (cond ((and (eql (request-kind req) :locate)
              (not (typep condition 'CORBA:OBJECT_NOT_EXIST)))
         (warn "A system exception in a locate request: ~A" condition)
         (setf (request-state req) :finished)
         (connection-shutdown (request-connection req)))
        (t
         (set-request-exception req condition)
         (server-request-respond req))))


(defun discard-request (req)
  (server-request-systemexception-reply
   req (system-exception 'CORBA:TRANSIENT 1 :completed_no)))



;;;; Standard ServerRequest interface
#|
      interface ServerRequest { // PIDL
        readonly attribute  Identifier operation;
        void                arguments    (inout NVList nv);
        Context             ctx();
        void                set_result   (in any val);
        void                set_exception(in any val);
    };
|#


(define-method operation ((self server-request))
  (request-operation self))


(define-method ctx ((self server-request))
  nil)


(define-method arguments ((self server-request) nv)
  (when (or (arguments-set-p self)
            (exception-set-p self))
    (raise-system-exception 'CORBA:bad_inv_order ))
  (setf (request-arguments self) nv)
  (loop for param in nv
        unless (zerop (logand ARG_IN (op:arg_modes param)))
        do (setf (any-value (op:argument param))
                 (unmarshal (any-typecode (op:argument param))
                            (request-input self)))
        unless (op:argument param)
        do (setf (op:argument param) (corba:any)))
  (has-received-request (the-orb self) self)
  nv )


(define-method set_result ((self server-request) val)
  (when (or (not (arguments-set-p self))
            (exception-set-p self))
    (raise-system-exception 'CORBA:bad_inv_order ))
  (setf (request-result self) val))


(define-method set_exception ((self server-request) val)
  (setq val (any-value val))
  (unless (typep val 'CORBA:Exception)
    (raise-system-exception 'CORBA:BAD_PARAM))
  (unless (or (arguments-set-p self)
              (exception-set-p self))
    (has-received-request (the-orb self) self))
  (setf (request-exception self) val))




;;;; Compute Skeleton Functions


(defvar *skel-version* 0)
(incf *skel-version*)

#+unused-functions
(defun handle-servant-exception (req exc exc-list)
  (let ((tc (find (exception-id exc) exc-list :key #'op:id :test #'string=)))
    (cond (tc
           (let ((out (get-exception-response req)))
             (marshal exc tc out)
             out))
          (t (raise-system-exception 'CORBA:UNKNOWN 0 :completed_yes)))))


(defun compute-skel-operation (name result params exceptions)
  (let ((operation (feature name))
        (in-funs (loop for p in params unless (eql :param_out (second p))
                       collect (unmarshal-function (third p))))
        (out-funs (nconc (if (not (eql :tk_void (op:kind result)))
                           (list (marshal-function result)))
                         (loop for p in params unless (eql :param_in (second p))
                               collect (marshal-function (third p)))))
        (exc-list  (mapcar #'symbol-typecode exceptions)))
    (lambda (servant sreq buffer)
      (let ((args (loop for ufun in in-funs
                        collect (funcall ufun buffer))))
        (set-request-args sreq args params exc-list)
        (handler-case
          (let ((res-list
                 (multiple-value-list (apply operation servant args))))
            (when (< (length res-list) (length out-funs))
              (raise-system-exception 'CORBA:MARSHAL 999 :completed_yes))
            (set-request-result sreq res-list result out-funs))
          (CORBA:UserException (exc) (set-request-exception sreq exc)))))))


(defun compute-skel-get-attribute (name type)
  (let ((operation (feature name))
        (mfun (marshal-function type)))
    (lambda (servant sreq buffer)
      (declare (ignore buffer))
      (set-request-args sreq nil nil nil)
      (let ((val (funcall operation servant)))
        (set-request-result sreq (list val) type (list mfun))))))


(defun compute-skel-set-attribute (name type)
  (let ((operation (fdefinition (list 'setf (feature name))))
        (ufun (unmarshal-function type))
        (params (list (list "v" :param_in type))))
    (lambda (servant sreq buffer)
      (let ((val (funcall ufun buffer)))
        (set-request-args sreq (list ufun) params nil)
        (funcall operation val servant)
        (set-request-result sreq nil CORBA:Tc_void nil)))))


(defun standard-skel-operations (table)
  (setf (gethash "_locate" table)
        (lambda (servant sreq buffer)
          (declare (ignore servant sreq buffer))))
  #+(or)
  (setf (gethash "_is_a" table)
        (let ((params (list (list "id" :param_in CORBA:Tc_string))))
          (lambda (servant sreq buffer)
            (let ((id (unmarshal-string buffer)))
              (set-request-args sreq (list id) params nil)
              (set-request-result sreq (list (op:_is_a servant id)) CORBA:TC_Boolean '(marshal-bool))))))
  #+(or)
  (setf (gethash "_non_existent" table)
        (lambda (servant sreq buffer)
          (declare (ignore buffer))
          (set-request-args sreq nil nil nil)
          (set-request-result sreq (list (op:_non_existent servant)) corba:tc_boolean '(marshal-bool))))
  (setf (gethash "_get_interface" table)
        (setf (gethash "_interface" table)
              (lambda (servant sreq buffer)
                (declare (ignore buffer))
                (set-request-args sreq nil nil nil)
                (set-request-result sreq (list (op:_get_interface servant)) corba:tc_object '(marshal-object))))))


(defun compute-iface-skel (isym &optional (table (make-hash-table :test #'equal)))
  (dolist (sym (get isym 'ifr-bases))
    (compute-iface-skel sym table))
  (dolist (sym (get isym 'ifr-contents))
    (let ((name   (get sym 'ifr-name))
          (result (get sym 'ifr-result))
          (type   (get sym 'ifr-type))
          (mode   (get sym 'ifr-mode)))
      (cond (result
             (let ((params (get sym 'ifr-params))
                   (exceptions (get sym 'ifr-exceptions)))
               (setf (gethash name table)
                     (compute-skel-operation name result params exceptions))))
            (type
             (setf (gethash (getter-name name) table)
                   (compute-skel-get-attribute name type))
             (when (eql mode :attr_normal)
               (setf (gethash (setter-name name) table)
                     (compute-skel-set-attribute name type)))))))
  table)


(defun get-skel-table (iface)
  (let ((skel-table (get iface 'skel-table)))
    (when (or (null skel-table)
              (< (gethash :version skel-table 0) *skel-version*))
      (setq skel-table (make-hash-table :test #'equal))
      (setf (gethash :version skel-table) *skel-version*)
      (setf (get iface 'skel-table) skel-table)
      (standard-skel-operations skel-table)
      (compute-iface-skel iface skel-table))
    skel-table))




;;;; Class: PortableServer:Servant

;; IDL/Lisp 6.2
;; System defined:
;;  op:_this SERVANT
;; User defined:
;;  op:_default_POA SERVANT
;; To support the Object interface:
;;  op:_non_existent, op:_is_a, op:_get_interface

(defclass PORTABLESERVER:SERVANT (corba:object) ())

(define-method _default_POA ((servant PortableServer:servant))
  (root-POA))

(define-method _non_existent ((servant PortableServer:servant))
  nil)

(defmethod primary-interface ((servant portableserver:servant) oid poa)
  (declare (ignore oid poa))
  (object-id servant))

(defmethod servant-invoke ((servant portableserver:servant) operation input request)
  (let ((iface (interface-name servant)))
    (let ((fun (gethash operation (get-skel-table iface))))
      (unless fun (raise-system-exception 'corba:bad_operation))
      (funcall fun servant request input))))



;;;; Class: DynamicImplementation

(defclass PORTABLESERVER:DYNAMICIMPLEMENTATION (portableserver:servant)
  ())

;; IDL/Lisp 6.3 DynamicImplementation
;; User defined:
;;  op:invoke SERVANT SERVERREQUEST
;;  op:primary_interface SERVANT OID POA

(define-method invoke ((servant PortableServer:DynamicImplementation) sreq)
  (declare (ignore sreq))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(define-method primary_interface ((servant PortableServer:DynamicImplementation) oid poa)
  (declare (ignore oid poa))
  (raise-system-exception 'CORBA:NO_IMPLEMENT))

(defmethod servant-invoke ((servant portableserver:dynamicimplementation) operation input req)
  (let ((fun (gethash operation (get-skel-table 'CORBA:Object))))
    (cond (fun (funcall fun servant req input))
          (t   (op:invoke servant req)))))

(defmethod primary-interface ((servant portableserver:dynamicimplementation) oid poa)
  (op:primary_interface servant oid poa))


;;; clorb-servant.lisp ends here
