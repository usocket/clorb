;;;; clorb-object.lisp --- CORBA:Object and other pseudo objects

(in-package :clorb)


;;;; Connection forward

(defgeneric profile-connection (profile orb))


;;;; CORBA Object Interface

;;;| InterfaceDef get_interface ();
;;; Strange that the lisp mapping does not rename this.
;;;| boolean	 is_nil();
;;; _is_nil in lisp mapping, could as well use null?
;;;| Object	 duplicate ();
;;;| void	 release ();
;;; duplicate, release not in lisp mapping
;;;| boolean	is_a (in string logical_type_id);
;;; _is_a in lisp mapping (in clorb-request)
;;;| boolean	non_existent();
;;; _non_existent in lisp mapping
;;;| boolean	is_equivalent (in Object other_object);
;;; _is_equivialent in lisp mapping
;;;| unsigned 	long hash(in unsigned long maximum);
;;; _hash in lisp mapping
;;;
;;;| Status create_request (
;;; _create_request in lisp mapping
;;;|     in Context	ctx,
;;;|     in Identifier	operation,

;;;|     in NVList	arg_list,
;;;|     inout NamedValue result,
;;;|     out Request	request,
;;;|     in Flags        req_flags    );
;;;
;;;| Policy get_policy (in PolicyType policy_type );
;;; _get_policy
;;;| DomainManagersList get_domain_managers ();
;;; _get_domain_managers
;;;| Object set_policy_overrides (in PolicyList policies,
;;;|             in SetOverrideType set_add);
;;; _set_policy_overrides



;;;; CORBA:NamedValue

(defconstant arg_in 1)
(defconstant arg_out 2)
(defconstant arg_inout 3)


(define-corba-class CORBA:NamedValue ()
  :attributes ((name)
               (argument)
               (arg_modes)))


(defun corba:namedvalue (&key (name "") argument (arg_modes ARG_IN))
  (make-instance 'CORBA:NamedValue
    :name name :argument argument :arg_modes arg_modes))


;;;; Generic Functions

(defgeneric object-is-a (object id)
  (:method-combination or))

(defgeneric object-id (object))


(define-feature "_THIS"
  :documentation "Used for implicit activation during marshalling.")

(define-method "_THIS" ((object t))
  (raise-system-exception 'CORBA:MARSHAL 4 :completed_no))



;;;; CORBA:Object

(define-interface CORBA:Object ()
  :id "IDL:omg.org/CORBA/Object:1.0"
  :name "Object")


;;;| boolean is_nil();

(define-method _is_nil ((x null))
  t)

(define-method _is_nil ((x CORBA:Object))
  nil)


;;;| boolean is_a (in string logical_type_id);

(DEFINE-OPERATION CORBA::Object/_is_a
  :ID "IDL:omg.org/CORBA/Object/_is_a:1.0"
  :NAME "_is_a"
  :DEFINED_IN CORBA:OBJECT
  :VERSION "1.0"
  :RESULT OMG.ORG/CORBA:TC_BOOLEAN
  :MODE :OP_NORMAL
  :CONTEXTS NIL
  :PARAMETERS (("obj" :param_in CORBA:Tc_string))
  :EXCEPTIONS NIL)

(define-method _is_a ((obj t) interface-id)
  (declare (ignore interface-id))
  (raise-system-exception 'CORBA:no_implement 3 :completed_no))

(define-method _is_a ((obj CORBA:Object) interface-id)
  (object-is-a obj interface-id))


;;;| boolean	non_existent();
;;; _non_existent in lisp mapping

(define-method _non_existent ((obj t))
  nil)


(DEFINE-OPERATION CORBA::Object/_non_existent
  :ID "IDL:omg.org/CORBA/Object/_non_existent:1.0"
  :NAME "_non_existent"
  :DEFINED_IN CORBA:OBJECT
  :VERSION "1.0"
  :RESULT OMG.ORG/CORBA:TC_BOOLEAN
  :MODE :OP_NORMAL
  :CONTEXTS NIL
  :PARAMETERS NIL
  :EXCEPTIONS NIL)



;;;| boolean	is_equivalent (in Object other_object);
;;; _is_equivalent in lisp mapping

(define-method _is_equivalent ((obj t) other)
  (eql obj other))



;;; Deferred from Any

(defmethod any-value ((obj corba:object))
  obj)



;;;; CORBA:Proxy


(defclass CORBA:PROXY (corba:object synchronized)
  ((id :initform nil :initarg :id :accessor proxy-id)
   (the-orb  :initarg :the-orb  :accessor the-orb)
   ;; Mutable slots
   (connection :initform nil :accessor %object-connection)
   (raw-profiles  :initarg :raw-profiles  )
   (profiles      :initarg :profiles      )
   (selected-profile :initform nil :accessor selected-profile)
   (forward :initform nil :initarg :forward :accessor object-forward)
   (forward-reset :initform nil :accessor object-forward-reset)))


(defgeneric profile-short-desc (profile stream)
  (:method ((profile t) stream) (write-string "*" stream)))


(defmethod print-object ((o corba:proxy) stream)
  (print-unreadable-object (o stream :type t)
    (when (eql (class-of o) (find-class 'corba:proxy))
      (format stream "~S @" (proxy-id o)))
    (let ((profile (loop with x = o
                         while (object-forward x)
                         do (setf x (object-forward x))
                         finally (return (or (selected-profile x)
                                             (first (object-profiles x)))))))
      (if profile
        (profile-short-desc profile stream)
        (write-string "--" stream))
      (when-let (conn (%object-connection o))
        (if (connection-working-p conn)
            (write-string " bound" stream)
            (write-string " broken-conn" stream))))))


(defmethod (setf object-forward) :before (val (proxy CORBA:Proxy))
  (declare (ignore val))
  ;; Forget old connection when forwarding
  (setf (%object-connection proxy) nil))


(defgeneric encode-profile (profile orb))

(defmethod raw-profiles ((obj CORBA:Proxy))
  (with-synchronization obj
    (with-cache-slot (obj raw-profiles)
      (let ((orb (the-orb obj)))
        (map 'list
             (lambda (profile) (encode-profile profile orb))
             (slot-value obj 'profiles))))))


(defmethod %object-profiles ((obj CORBA:Proxy))
  (with-cache-slot (obj profiles)
    (map 'list
         (lambda (tagged-profile)
           (decode-ior-profile (op:tag tagged-profile)
                               (op:profile_data tagged-profile)))
         (slot-value obj 'raw-profiles))))

(defmethod object-profiles ((obj CORBA:Proxy))
  (with-synchronization obj
    (%object-profiles obj)))


(defmethod effective-profile ((obj CORBA:Proxy))
  (with-synchronization obj
    (selected-profile (or (object-forward obj) obj))))


(defmethod marshal-object ((objref CORBA:Proxy) buffer)
  (marshal-string (proxy-id objref) buffer)
  (marshal-sequence (raw-profiles objref)
                    (marshal-function (symbol-typecode 'IOP:TAGGEDPROFILE))
                    buffer))


(defgeneric profile-component (profile tag))

(defmethod object-component ((obj CORBA:Proxy) tag)
  (loop for profile in (object-profiles obj)
        thereis (profile-component profile tag)))


(defun %existing-connection (proxy)
  (when-let (conn (%object-connection proxy))
    (if (connection-working-p conn)
        conn
        (setf (%object-connection proxy) nil))))

(defun %forward-connection (proxy)
  (when-let (forward (object-forward proxy))
    (cond ((setf (%object-connection proxy) (get-object-connection forward))
           (setf (object-forward-reset proxy) nil)
           (%object-connection proxy))
          ((object-forward-reset proxy)
           (setf (object-forward proxy) nil)
           (warn "Object forwarding fail")
           (raise-system-exception 'CORBA:TRANSIENT))
          (t
           (setf (object-forward-reset proxy) t)
           (setf (object-forward proxy) nil)))))

(defun %select-profile-and-connect (proxy)
  ;; select a profile and create a connection for that profile
  (dolist (profile (%object-profiles proxy))
    (let ((conn (profile-connection profile (the-orb proxy))))
      (when (and conn (connection-working-p conn))
        (setf (%object-connection proxy) conn)
        (setf (selected-profile proxy) profile)
        (return conn)))))


(defun get-object-connection (proxy)
  "Get the connection to use for a proxy object.
Should be called with proxy unlocked."
  (with-synchronization proxy
    (or (%existing-connection proxy)
        (%forward-connection proxy)
        (%select-profile-and-connect proxy))))


(defgeneric profile-equal (profile1 profile2)
  (:method ((profile1 t) (profile2 t)) (eq profile1 profile2)))

(define-method _is_equivalent ((obj corba:proxy) other)
  (let ((profile1 (car (object-profiles obj)))
        (profile2 (car (object-profiles other))))
    (and profile1 profile2
         (profile-equal profile1 profile2))))


;;;| unsigned 	long hash(in unsigned long maximum);
;;; _hash in lisp mapping

(define-method _hash ((obj t) maximum)
  (rem (sxhash obj)
       maximum))

(defgeneric profile-hash (profile))

(define-method _hash ((obj corba:proxy) maximum)
  (rem (if (object-profiles obj)
         (profile-hash (first (object-profiles obj)))
         0)
       maximum))


;;;| Status create_request (
;;; _create_request in lisp mapping
;;;|     in Context	ctx,
;;;|     in Identifier	operation,
;;;|     in NVList	arg_list,
;;;|     inout NamedValue result,
;;;|     out Request	request,
;;;|     in Flags        req_flags    );

(define-method _create_request ((obj t)
                                ctx operation arg_list result req_flags)
  (declare (ignore ctx operation arg_list result req_flags))
  (raise-system-exception 'CORBA:NO_IMPLEMENT 4 :completed_no))

(define-method _create_request ((obj CORBA:Proxy)
                                ctx operation arg_list result req_flags)
  (declare (ignorable req_flags ctx))
  (check-type operation string)
  (check-type arg_list sequence)
  (check-type result (or null CORBA:NamedValue))
  (if result
      (setf (op:arg_modes result) ARG_OUT)
    (setq result (CORBA:NamedValue :arg_modes ARG_OUT
                                   :argument (CORBA:Any :any-typecode CORBA:tc_void))))
  (values result
          (create-client-request (the-orb obj)
                                 :target obj
                                 :operation operation
                                 :paramlist (cons result (copy-seq arg_list))
                                 #| :ctx ctx |# )))



;;;; Registry for Proxy classes

(defvar *proxy-classes*
    (make-hash-table :test #'equal))

(defun find-proxy-class (id)
  (gethash id *proxy-classes* 'CORBA:Proxy))

(defun register-proxy-class (id class)
  (setf (gethash id *proxy-classes*) class))

(defun object-narrow (obj id &optional no-error)
  "Return an equivalent proxy with class for the repository id."
  (when (symbolp id)
    (setq id (symbol-ifr-id id)))
  (cond ((op:_is_a obj id)
         (let ((new (create-objref (the-orb obj)
                        :ior-id id :raw-profiles (raw-profiles obj))))
           (setf (object-forward new) (object-forward obj))
           new))
        (no-error nil)
        (t
         (error "Object of wrong type for narrowing"))))

(defun nobject-narrow (obj id &optional no-error)
  "Return an equivalent proxy with class for the repository id.
Might destructivley change the original object."
  (when (symbolp id)
    (setq id (symbol-ifr-id id)))
  (cond ((op:_is_a obj id)
         (setf (proxy-id obj) id)
         (change-class obj (find-proxy-class id)))
        (no-error nil)
        (t
         (error "Object of wrong type for narrowing"))))


;; Compatibility with LW CORBA

(define-method op::narrow ((class-symbol symbol) proxy)
  (object-narrow proxy class-symbol))



(defun auto-narrow (obj)
  ;; Try convert to a type-specific proxy
  (when (eql (class-of obj) (find-class 'CORBA:Proxy))
    (when (equal (proxy-id obj) "")
      (unless (object-forward obj)
        (locate obj))
      (let ((forward (object-forward obj)))
        (when forward
          (auto-narrow forward)
        (unless (eql (class-of forward) (find-class 'CORBA:Proxy))
          (setf (proxy-id obj) (proxy-id forward))
          (change-class obj (class-of forward))))))))


;;; Something like this, but with a clorb specific meta class for generic function
#+(or)
(defmethod no-applicable-method ((generic-function standard-generic-function) &rest args)
  (cond ((and (cdr args)
              (typep (car args) 'CORBA:Proxy)
              (auto-narrow (car args)))
         (apply generic-function args))
        (t
         (call-next-method))))



;;;; ValueBase

(defclass CORBA:ValueBase ()
  ())



;;;; AbstractBase

(defclass CORBA:AbstractBase (CORBA:Object)
  ())




;;; clorb-object.lisp ends here
