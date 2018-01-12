;;;; clorb-orb.lisp -- The ORB Interface

(in-package :clorb)

(defvar *orb-class* 'clorb-orb
  "The class (name) of the ORB instantiated by CORBA:ORB_init.")

(defvar *orb-initializers* nil)


(deftype CORBA:ORBId ()
  'string)



;;;;  interface ORB {				// PIDL

(defclass CORBA:TYPECODEFACTORY ()
  ())

(defclass CORBA:ORB ()
  ())

(defclass clorb-orb (CORBA:ORB CORBA:TypeCodeFactory synchronized)
  ((adapter :initarg :adapter :initform nil  :accessor adapter)
   (active  :initarg :active  :initform nil  :accessor orb-active)
   (host    :initarg :host    :initform nil  :accessor orb-host)
   (port    :initarg :port    :initform nil  :accessor orb-port)
   (initial-references :initform '()
                       :accessor orb-initial-references)
   (default-initial-reference
     :initform nil
     :accessor orb-default-initial-reference)
   (pending-client-request
    :initform nil
    :accessor pending-client-request)
   (work-queue  :initform nil  :accessor work-queue)))



;;;; Internal orb interface


(defmethod the-orb ((orb CORBA:ORB))
  orb)


(defmethod create-client-request ((orb clorb-orb) &rest initargs)
  (apply #'make-instance 'client-request 
         :the-orb orb initargs))


(defmethod create-objref ((orb clorb-orb) &key ior-id expected-id 
                           raw-profiles profiles)
  (assert (and (or raw-profiles profiles)
               (not (and raw-profiles profiles)))
          () "Need profiles or raw-profiles, but not both")
  (let ((key (if profiles :profiles :raw-profiles)))
    (make-instance (find-proxy-class 
                    (if (equal ior-id "") expected-id ior-id))
      :id ior-id
      :the-orb orb
      key (or profiles raw-profiles))))
        


;;; Work Queue

(defmethod enqueue-work ((orb clorb-orb) thunk)
  (setf (work-queue orb)
        (nconc (work-queue orb) (list thunk))))


(defmethod add-pending-client-request ((orb clorb-orb) client-request)
  (push client-request (pending-client-request orb)))

(defmethod remove-pending-request ((orb clorb-orb) client-request)
  (let ((pos (member client-request (pending-client-request orb))))
    (when pos
      (if (cdr pos)
        (setf (car pos) (cadr pos)
              (cdr pos) (cddr pos))
        (setf (pending-client-request orb)
              (delete client-request (pending-client-request orb)))))
    pos))

(defmethod has-pending-request ((orb clorb-orb) client-request)
  (member client-request (pending-client-request orb)))


;; initial ref:
;;  ( name  .  ( string . object  ))

(defun set-initial-reference (orb name string &optional object)
  (let ((old (assoc name (orb-initial-references orb)
                    :test #'string=)))
    (cond (old
           (setf (car (cdr old)) string
                 (cdr (cdr old)) object))
          (t
           (push (cons name (cons string object))
                 (orb-initial-references orb))))))

(defun refresh-initial-references (orb)
  (dolist (ref (orb-initial-references orb))
    (when (cadr ref) (setf (cddr ref) nil))))



;;; ORB Operations for interceptors

(defmethod has-received-exception ((orb clorb-orb) client-request)
  (mess 2 "#~S received exception: ~S, ~A" 
        (request-id client-request)
        (request-status client-request)
        (request-exception client-request)))

(defmethod has-received-reply ((orb clorb-orb) client-request)
  (mess 2 "#~S reply"
        (request-id client-request)))

(defmethod has-received-other ((orb clorb-orb) client-request)
  (mess 2 "#~S receive other: ~S" 
        (request-id client-request)
        (request-status client-request)))

(defmethod will-send-request ((orb clorb-orb) client-request)
  (mess 2 "#~S send-request ~A ~S"
        (request-id client-request)
        (request-operation client-request)
        (request-target client-request)))


(defmethod has-received-request-header ((orb clorb-orb) server-request)
  (declare (ignore server-request))
  nil)

(defmethod has-received-request ((orb clorb-orb) server-request)
  (declare (ignore server-request))
  nil)

(defmethod will-send-exception ((orb clorb-orb) server-request)
  (declare (ignore server-request))
  nil)

(defmethod will-send-reply ((orb clorb-orb) server-request)
  (declare (ignore server-request))
  nil)

(defmethod will-send-other ((orb clorb-orb) server-request)
  (declare (ignore server-request))
  nil)


;;;; CORBA:ORB Interface

;;;    exception InvalidName {};
(define-user-exception CORBA:ORB/InvalidName
  :id "IDL:omg.org/CORBA/ORB/InvalidName:1.0")


;;;    void shutdown( in boolean wait_for_completion );
(define-method shutdown ((orb orb) wait_for_completion)
  ;;FIXME: wait_for_completion
  (declare (ignore wait_for_completion))
  (setf (orb-active orb) nil))


;;;    Object string_to_object (in string str);
(define-method string_to_object ((orb orb) str)
  (string-to-object orb str))

;;;    string object_to_string (in Object obj);
(define-method object_to_string ((orb orb) obj)
  (format nil
	  "IOR:~{~2,'0X~}"
	  (map 'list #'identity (marshal-make-encapsulation
				 (lambda (buffer)
                                   (marshal-object obj buffer))
                                 orb))))



;;;; Initial reference operations


;;;    ObjectIdList list_initial_services ();

(define-method list_initial_references ((orb orb))
  (mapcar #'car (orb-initial-references orb)))


;;;    Object resolve_initial_references (in ObjectId identifier)
;;;      raises (InvalidName);

(define-method resolve_initial_references ((orb orb) name)
  (let ((ref-entry
         (assoc name (orb-initial-references orb)
                :test #'string=)))
    (unless ref-entry
      (when (orb-default-initial-reference orb)
        (setq ref-entry (list name (format nil "~A/~A"
                                           (orb-default-initial-reference orb)
                                           name)))
        (push ref-entry (orb-initial-references orb))))
    (unless ref-entry
      (error (CORBA:ORB/InvalidName)))
    (let ((obj (cddr ref-entry)))
      (unless obj
        (let ((designator (cadr ref-entry)))
          (cond ((stringp designator)
                 (setf obj (op:string_to_object orb designator)))
                (t
                 (setf obj (funcall designator orb)))))
        (setf (cddr ref-entry) obj))
      obj)))


;;; void register_initial_reference (in ObjectId id, in Object obj)
;;;    raises (InvalidName);

(define-method register_initial_reference ((orb clorb-orb) id obj)
  (when (or (not (stringp id)) (equal id "")
            (assoc id (orb-initial-references orb)
                    :test #'string=))
    (error (CORBA:ORB/InvalidName)))
  (unless obj
    (raise-system-exception 'CORBA:BAD_PARAM 24 :completed_no))
  (set-initial-reference orb id nil obj))



;;;; Thread related operations


;;;    boolean work_pending(  );

(define-method work_pending ((orb orb))
  (or (io-event-waiting-p)
      (work-queue orb)
      (progn (io-driver t) (io-event-waiting-p))))


;;;    void perform_work( );

(define-method perform_work ((orb orb))
  (let ((*running-orb* t))
    (orb-work orb t t)))


;; for interactive use
(defun poll (&optional (orb *the-orb*))
  (loop repeat 10
       while (op:work_pending orb)
       do (op:perform_work orb)))


;;;    void run();

(define-method run ((orb orb))
  (unless *running-orb*
    (cerror "Run anyway"
            "Seems the orb is already running"))
  (let ((old *running-orb*))
    (setq *running-orb* nil)
    (unwind-protect 
      (let ((*running-orb* t))
        (loop while (orb-active orb)
              do (orb-work orb t nil)))
      (setq *running-orb* old))))


;;; Non-standard: bg-run

(defun bg-run ()
  "Setup the ORB for background processing.
This is a replacement for op:run. It will return directly and the
ORB will coninue processing in the background.
Requires that a multi-threaded IO-system is configured."
  (assert (typep *io-system* 'io-system-mt-base))
  (setq *running-orb* nil)
  (when-let (adapter (adapter *the-orb*))
    (start-dispatcher-process adapter))
  (flet ((bg-run-event-handler (event)
           (process-event event)
           t))
    (setf (event-handler *io-system*) #'bg-run-event-handler))
  (io-start-bg-listen *io-system*))




;;;; Dynamic Invokation related operations


;;;    Status create_list ( in long    count,
;;;                         out NVList new_list );

(define-method create_list ((orb CORBA:ORB) count)
  (loop repeat count
        collect (CORBA:NamedValue :name "" 
                                  :argument (CORBA:Any :any-typecode CORBA:tc_void)
                                  :arg_modes CORBA:ARG_IN)))


;;;    Status create_operation_list ( in OperationDef oper,
;;;                                   out NVList      new_list );

(define-method create_operation_list ((orb CORBA:ORB) opdef)
  (map 'list
       (lambda (pd) 
         (CORBA:NamedValue
          :name (op:name pd)
          :argument (CORBA:Any :any-typecode (op:type pd))
          :arg_modes (ecase (op:mode pd)
                       (:param_in ARG_IN)
                       (:param_out ARG_OUT)
                       (:param_inout ARG_INOUT))))
       (op:params opdef)))


;;;    Status get_default_context (out Context ctx);

(define-method get_default_context ((orb clorb-orb))
  ;;FIXME
  (raise-system-exception 'corba:no_implement))


;;;    void send_multiple_requests_oneway(
;;;        in RequestSeq               req
;;;    );

(define-method op::send_multiple_requests_oneway ((orb clorb-orb) req-list)
  (map nil #'op:send_oneway req-list))


;;;    void send_multiple_requests_deferred(
;;;        in RequestSeq               req
;;;    );

(define-method op::send_multiple_requests_deferred ((orb clorb-orb) req-list)
  (map nil #'op:send_deferred req-list))


;;;    boolean poll_next_response();

(define-method op::poll_next_response ((orb clorb-orb))
  (unless (pending-client-request orb)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 11 :completed_no))
  (some #'request-status (pending-client-request orb)))


;;;    void get_next_response(
;;;        out Request                 req
;;;    ); // raises(WrongTransaction);

(define-method op::get_next_response ((orb clorb-orb))
  (unless (pending-client-request orb)
    (raise-system-exception 'CORBA:BAD_INV_ORDER 11 :completed_no))
  (orb-wait #'op:poll_next_response orb)
  (let ((result nil))
    (setf (pending-client-request orb)
          (loop for req in (pending-client-request orb)
                if (and (null result) (request-status req))
                do (setq result (progn (op:get_response req) req))
                else collect req))
    (or result 
        ;; OOPS, some other thread interfered ??
        (op::get_next_response orb))))



;;;; Service Information operations


;;;    boolean get_service_information (in ServiceType         service_type,
;;;                                     out ServiceInformation service_information );

;;;    ObjectIdList list_initial_services ();



;;;    // get_current deprecated operation - should not be used by new code
;;;    // new code should use resolve_initial_reference operation instead
;;;    Current get_current();
;;;    // deprecate get_current in the next major printing of CORBA




;;;; Value factory operations

;;; ValueFactory register_value_factory(
;; in RepositoryId id,
;; in ValueFactory factory

(define-method register_value_factory ((orb clorb-orb) id factory)
  (check-type id string)
  (check-type factory (or class symbol))
  (setf (gethash id *value-factory-registry*) factory))


;;; void unregister_value_factory(in RepositoryId id);

(define-method unregister_value_factory ((orb clorb-orb) id)
  (check-type id string)
  (remhash id *value-factory-registry*))


;;; ValueFactory lookup_value_factory(in RepositoryId id);

(define-method lookup_value_factory ((orb clorb-orb) id)
  (check-type id string)
  (gethash id *value-factory-registry*))



;;;; Initializing the ORB
;;; ORB CORBA:ORB_init (arg_list, orbid)

(defun CORBA:ORB_init (&optional args (orbid ""))
  (let ((info nil))
    (cond ((null *the-orb*)
           (setq *the-orb* (make-instance *orb-class* :active t))
           (setq info (create-orb-init-info *the-orb* args orbid))
           (dolist (fn *orb-initializers*)
             (op:pre_init fn info)))
          ((not (typep *the-orb* *orb-class*))
           (change-class *the-orb* *orb-class*)))
    (setq args (process-orb-args *the-orb* args))
    (io-init)
    (setf (orb-active *the-orb*) t)
    (when info
      (dolist (fn *orb-initializers*)
        (op:post_init fn info))))
  (values *the-orb* args))


(defmethod create-orb-init-info ((orb clorb-orb) args orbid)
  (declare (ignore args orbid))
  orb)


(defun process-orb-args (orb args)
  (let ((new-args '()))
    (loop while args do
          (let ((arg (pop args)))
            (if (string-starts-with arg "-ORB")
              (let ((value nil))
                (flet ((option-match (prefix)
                         (when (string-starts-with arg prefix)
                           (setq value (subseq arg (length prefix)))
                           (setq value (string-left-trim " 	" value))
                           (when (equal value "") 
                             (setq value (pop args)))
                           t)))
                  (cond ((option-match "-ORBInitRef")
                         (process-opt-initial-reference orb value))
                        ((option-match "-ORBDefaultInitRef")
                         (setf (orb-default-initial-reference orb) value))
                        ((option-match "-ORBPort")
                         (setf (orb-port orb) (parse-arg-integer value)))
                        ((option-match "-ORBHostname")
                         (setf (orb-host orb) value)) 
                        (t (raise-system-exception 'CORBA:BAD_PARAM)))))
              (push arg new-args))))
    (nreverse new-args)))

#|
(CORBA:ORB_init '("-ORBDefaultInitRef" "corbaloc::555objs.com"))
(CORBA:ORB_init '("-ORBInitRef" "NameService=corbaloc::1.2@localhost:2001/NameService"))
|#

(defun process-opt-initial-reference (orb arg)
  (let ((eq-pos (position #\= arg)))
    (unless eq-pos (error "Invalid InitialReferences option: ~A" arg))
    (let ((name (subseq arg 0 eq-pos))
          (ior  (subseq arg (+ eq-pos 1))))
      (set-initial-reference orb name ior))))

(defun parse-arg-integer (arg)
  (typecase arg 
    (number arg)
    (string (parse-integer arg))
    (t (error "Argument should be integer: ~A" arg))))




;; Backward compatiblity with functions as initializers

(define-method "PRE_INIT" ((init function) info)
  (funcall init (the-orb info)))

(define-method "POST_INIT" ((init function) info)
  (declare (ignore info)))


(define-method "PRE_INIT" ((init symbol) info)
  (op:pre_init (symbol-function init) info))

(define-method "POST_INIT" ((init symbol) info)
  (op:post_init (symbol-function init) info))






;;;; Parsing Stringified Object References

(defun string-to-object (orb str)
  (multiple-value-bind (method rest)
                       (split-url str)
    (cond
     ((null method) (error "Invalid object reference: ~A" str))
     ((string-equal method "corbaloc")
      (corbaloc-to-object orb rest))
     ((string-equal method "corbaname")
      (corbaname-to-object orb rest))
     ;; IOR:xx urls
     ((string-equal method "IOR")
      (unmarshal-encapsulation (decode-hex-string rest) #'unmarshal-object))
     ;; file:///foo/bar urls
     ((string-equal method "file")
      (let ((pathname (parse-file-url rest)))
        (let ((ior-string
               (with-open-file (fs pathname :direction :input)
                 (read-line fs))))
          (string-to-object orb (string-trim #.(vector #\Space #\Linefeed #\Return)
                                             ior-string)))))
     ((string-equal method "http")
      (multiple-value-bind (host port path)
                           (parse-http-url rest)
        (let ((ior (http-get-ior host port path)))
          (or ior (raise-system-exception 'CORBA:bad_param))
          (string-to-object orb ior))))
     (t
      (error "Unrecognized URL method: ~A" method)))))


(defun parse-file-url (rest)
  (let ((path (if (string-starts-with rest "//")
                ;; has host part, ignore
                (subseq rest (position #\/ rest :start 2))
                rest)))
    (let ((directory '())
          (start 0))
      (cond ((string-starts-with path "/")
             (setq directory (reverse (home-volume-prefix)))
             (incf start))
            (t
             (push :relative directory)))
      (loop for pos = (position #\/ path :start start)
            while pos
            do (push (subseq path start pos) directory)
            do (setf start (1+ pos)))
      (make-pathname :name (subseq path start)
                     :directory (nreverse directory)))))


(defun parse-http-url (rest)
  (let ((host "localhost")
        (port 80)
        (path rest))
    (when (string-starts-with rest "//")
      (let* ((path-end (length rest))
             (port-start (position #\: rest :start 2))
             (port-end (or (position #\/ rest :start 2) path-end))
             (host-end (or port-start port-end)))
        (setq host (subseq rest 2 host-end))
        (when port-start
          (setq port (parse-integer rest :start (1+ port-start) :end port-end)))
        (setq path (subseq rest port-end))))
  (values host port path)))


(defun corbaloc-to-object (orb rest &optional default-key)
  (multiple-value-bind (addrs key)
                       (parse-corbaloc rest)
    (if (and default-key (equal key ""))
      (setq key default-key))
    (cond
     ((eq (car addrs) :rir)
      (op:resolve_initial_references orb (decode-objkey-string key)))
     (t
      (let ((key (decode-objkey-vector key)))
        (flet ((make-profile (addr)
                 (assert (eq :iiop (car addr)))
                 (destructuring-bind (version host port) (cdr addr)
                   (make-iiop-profile
                    :version version :host host :port port :key key))))
          (create-objref orb :ior-id "" 
                         :profiles (mapcar #'make-profile addrs))))))))


(defun corbaname-to-object (orb rest)
  (let ((name-pos (position #\# rest))
        (name-part ""))
    (when name-pos
      (setq name-part (decode-objkey-string (subseq rest (1+ name-pos))))
      (setq rest (subseq rest 0 name-pos)))
    (let ((namecontext (corbaloc-to-object orb rest "NameService")))
      (orb-resolve orb namecontext name-part))))


(defun split-url (str)
  (let ((method-end (position #\: str)))
    (if method-end
      (values (subseq str 0 method-end)
              (subseq str (+ method-end 1)))
      nil)))

(defun string-starts-with (string prefix)
  (and (>= (length string) (length prefix))
       (string= string prefix :end1 (length prefix))))


(defun parse-corbaloc (str)
  ;; str = <obj_addr_list>["/"<key_string>]
  ;; <obj_addr_list> = [<obj_addr> ","]* <obj_addr>
  ;; - URL escaped.
  (let ((key-pos (position #\/ str))
        (key-string ""))
    (when key-pos
      (setq key-string (subseq str (+ key-pos 1)))
      (setq str (subseq str 0 key-pos)))
    (let ((addr-list '())
          (last-pos 0))
      (loop for comma-pos = (position #\, str :start last-pos)
            while comma-pos do
            (push (subseq str last-pos comma-pos) addr-list)
            (setq last-pos (+ comma-pos 1)))
      (push (subseq str last-pos) addr-list)
      (values
       (mapcar #'parse-obj-addr addr-list)
       key-string))))


(defun parse-obj-addr (str)
  (multiple-value-bind (prot-token rest)
      (split-url str)
    (cond
      ((null prot-token) (error "malformed obj-addr"))
      ((string-equal prot-token "rir")
       :rir)
      ((or (string-equal prot-token "iiop")
           (string-equal prot-token ""))
       ;; <iiop_addr> = [<version>] <host> [":" <port>]
       ;; <host> = DNS-style Host Name  or ip_address
       ;; <version> = <major> "." <minor> "@"
       (let ((version "1.0")
             (port "2809"))
         (let ((pos (position #\@ rest)))
           (when pos
             (setq version (subseq rest 0 pos))
             (setq rest    (subseq rest (+ pos 1)))))
         (let ((pos (position #\: rest)))
           (when pos
             (setq port (subseq rest (+ pos 1)))
             (setq rest (subseq rest 0 pos))))
         (list :iiop (parse-iiop-version version)
               (if (equal rest "")
                 "localhost" rest )
               (parse-integer port)))))))


(defun decode-hex-string (string)
  (declare (string string))
  (assert (evenp (length string)))
  (let ((ints
         (loop for i from 0 below (length string) by 2
               collect (parse-integer string :start i :end (+ i 2)
                                      :radix 16))))
    (make-array (length ints)
                :initial-contents ints
                :element-type '(unsigned-byte 8))))

(defun decode-objkey-string (string)
  (declare (string string))
  (with-output-to-string (out)
    (loop with state = 0
          for ch across string
          for i from 0
          do (ecase state
               (0 (if (eql ch #\%)
                      (setq state 1)
                      (princ ch out)))
               (1 (setq state 2)
                  (princ (code-char
                          (parse-integer string :start i :end (+ i 2)
                                         :radix 16))
                         out))
               (2 (setq state 0))))))

(defun decode-objkey-vector (string)
  (declare (string string))
  (let ((out (make-array 50 :adjustable t :fill-pointer 0
                         :element-type 'CORBA:Octet)))
    (loop with state = 0
          for ch across string
          for i from 0
          do (ecase state
               (0 (if (eql ch #\%)
                      (setq state 1)
                      (vector-push-extend (char-code ch) out)))
               (1 (setq state 2)
                  (vector-push-extend
                   (parse-integer string :start i :end (+ i 2)
                                  :radix 16)
                   out))
               (2 (setq state 0))))
    out))

(defun parse-iiop-version (str)
  (declare (string str))
  (multiple-value-bind (major pos)
      (parse-integer str :junk-allowed t)
    (assert (eq (char str pos) #\.))
    (let ((minor (parse-integer str :start (+ pos 1))))
      (make-iiop-version major minor))))




;;;; Object Narrowing

;; Compatibility with Franz OrbLink

(define-method op::_narrow ((orb clorb-orb) object class-symbol)
  (object-narrow object class-symbol))



;;;; CORBA::Current


(define-corba-class CORBA:Current ())
#+(or)
(DEFINE-INTERFACE OMG.ORG/CORBA:CURRENT (OBJECT)
  :proxy (OMG.ORG/CORBA:CURRENT-PROXY OMG.ORG/CORBA:CURRENT OMG.ORG/CORBA:PROXY)
  :id "IDL:omg.org/CORBA/Current:1.0"
  :name "Current")



;;;; Policy


(DEFINE-ALIAS OMG.ORG/CORBA:POLICYTYPE
  :id "IDL:omg.org/CORBA/PolicyType:1.0"
  :name "PolicyType"
  :type OMG.ORG/CORBA:ULONG
  :typecode OMG.ORG/CORBA:TC_ULONG)

(DEFINE-INTERFACE OMG.ORG/CORBA:POLICY (OBJECT)
  :proxy (OMG.ORG/CORBA:POLICY-PROXY OMG.ORG/CORBA:POLICY OMG.ORG/CORBA:PROXY)
  :id "IDL:omg.org/CORBA/Policy:1.0"
  :name "Policy")

(define-corba-class policy-impl (OMG.ORG/CORBA:POLICY)
  :attributes ((policy_type :readonly)) )

(define-method "DESTROY" ((OBJ policy-impl)))

(define-method "COPY" ((OBJ policy-impl))
  obj )


(DEFINE-METHOD "DESTROY" ((OBJ OMG.ORG/CORBA:POLICY-PROXY))
  (STATIC-CALL ("destroy" OBJ) :output ((OUTPUT)) :input ((INPUT)) :exceptions NIL))

(DEFINE-METHOD "COPY" ((OBJ OMG.ORG/CORBA:POLICY-PROXY))
  (STATIC-CALL ("copy" OBJ)
               :output ((OUTPUT))
               :input ((INPUT) (UNMARSHAL (SYMBOL-TYPECODE 'OMG.ORG/CORBA:POLICY) INPUT))
               :exceptions NIL))

(DEFINE-METHOD "POLICY_TYPE" ((OBJ OMG.ORG/CORBA:POLICY-PROXY))
  (GET-ATTRIBUTE OBJ "_get_policy_type" (SYMBOL-TYPECODE 'OMG.ORG/CORBA:POLICYTYPE)))


(DEFINE-ALIAS OMG.ORG/CORBA:POLICYLIST
  :id "IDL:omg.org/CORBA/PolicyList:1.0"
  :name "PolicyList"
  :type SEQUENCE
  :typecode (create-sequence-tc 0 (SYMBOL-TYPECODE 'OMG.ORG/CORBA:POLICY)))

(defconstant omg.org/corba:secconstruction (quote 11))
(defconstant omg.org/corba:unsupported_policy_value (quote 4))
(defconstant omg.org/corba:bad_policy_value (quote 3))
(defconstant omg.org/corba:bad_policy_type (quote 2))
(defconstant omg.org/corba:unsupported_policy (quote 1))
(defconstant omg.org/corba:bad_policy (quote 0))

(DEFINE-ALIAS OMG.ORG/CORBA:POLICYERRORCODE
  :id "IDL:omg.org/CORBA/PolicyErrorCode:1.0"
  :name "PolicyErrorCode"
  :type OMG.ORG/CORBA:SHORT
  :typecode OMG.ORG/CORBA:TC_SHORT)

(DEFINE-USER-EXCEPTION OMG.ORG/CORBA:POLICYERROR
  :id "IDL:omg.org/CORBA/PolicyError:1.0"
  :name "PolicyError"
  :members (("reason" (SYMBOL-TYPECODE 'OMG.ORG/CORBA:POLICYERRORCODE))))


(defgeneric create-policy (type value))

(define-method create_policy ((orb ORB) type val)
  (create-policy type (any-value val)))

(defmethod create-policy ((type t) value)
  (declare (ignore value))
  (error (OMG.ORG/CORBA:POLICYERROR :reason OMG.ORG/CORBA:BAD_POLICY)))



;;;; TypeCodeFactory


;;(DEFINE-INTERFACE CORBA:TYPECODEFACTORY (OBJECT)
;; :ID "IDL:omg.org/CORBA/TypeCodeFactory:1.0"
;; :NAME "TypeCodeFactory")

(defun check-tc-id (id)
  "Check valid id for typecode.
Should have form '<format>:<string>"
  (unless (and (stringp id)
               (let ((colon (position #\: id)))
                 (and colon
                      (not (find-if-not #'alpha-char-p  id :end colon)))))
    (raise-system-exception 'CORBA:BAD_PARAM 16)))

(defun check-tc-name (name &optional (minor 15))
  (unless (and (stringp name)
               (loop for c across name
                     for i from 0
                     always (or (eql c #\_)
                                (if (zerop i)
                                  (alpha-char-p c)
                                  (alphanumericp c)))))
    (raise-system-exception 'CORBA:BAD_PARAM minor)))

(defun check-tc-member-names (names)
  (loop for tail on names do
        (check-tc-name (car tail) 17)
        (when (member (car tail) (cdr tail) :test #'equal)
          (raise-system-exception 'CORBA:BAD_PARAM 17))))


(defun check-tc-content-type (type)
  (unless (and (typep type 'CORBA:TypeCode)
               (not (member (op:kind type)
                       '(:tk_null :tk_void :tk_except t))))
    (raise-system-exception 'CORBA:BAD_TYPECODE 2)))



(define-method "CREATE_RECURSIVE_TC" ((obj corba:typecodefactory) id)
  (make-typecode :recursive id))

(defun fix-recursive-tc (tc)
  (let ((recur-list '())
        (id (op:id tc)))
    (labels 
      ((rec (obj)
         (typecase obj
           (sequence
            (map nil #'rec obj))
           (CORBA:TypeCode
            (cond ((eql (op:kind obj) :recursive)
                   (when (equal id (first (typecode-params obj)))
                     (typecode-smash obj tc)))
                  ((not (member obj recur-list))
                   (push obj recur-list)
                   (rec (typecode-params obj))))))))
      (rec tc)
      tc)))
  


(define-method "CREATE_ARRAY_TC" ((obj corba:typecodefactory) length element_type)
  (check-tc-content-type element_type)
  (create-array-tc length element_type))

;; Deprecated
;;(DEFINE-METHOD "CREATE_RECURSIVE_SEQUENCE_TC" ((obj corba:typecodefactory) bound _OFFSET))

(DEFINE-METHOD "CREATE_SEQUENCE_TC" ((obj corba:typecodefactory) bound element_type)
  (check-tc-content-type element_type)
  (create-sequence-tc bound element_type))

(define-method "CREATE_FIXED_TC" ((obj corba:typecodefactory) digits scale)
  (create-fixed-tc digits scale))

(define-method "CREATE_WSTRING_TC" ((OBJ CORBA:TYPECODEFACTORY) bound)
  (create-wstring-tc bound))

(define-method "CREATE_STRING_TC" ((OBJ CORBA:TYPECODEFACTORY) bound)
  (create-string-tc bound))



(define-method "CREATE_INTERFACE_TC" ((OBJ CORBA:TYPECODEFACTORY) id name)
  (check-tc-id id)
  (check-tc-name name)
  (fix-recursive-tc (create-interface-tc id name)))

(define-method "CREATE_EXCEPTION_TC" ((obj corba:typecodefactory) id name members)
  (check-tc-id id)
  (check-tc-name name)
  (fix-recursive-tc (create-exception-tc id name (simple-struct-members members))))

(define-method "CREATE_STRUCT_TC" ((obj corba:typecodefactory) id name members)
  (check-tc-id id)
  (check-tc-name name)
  (fix-recursive-tc (create-struct-tc id name (simple-struct-members members))))

(defun simple-struct-members (members)
  (check-tc-member-names (map 'list 'op:name members))
  (map 'vector (lambda (m) 
                 (check-tc-content-type (op:type m))
                 (list (op:name m) (op:type m)))
       members))


(define-method "CREATE_UNION_TC" ((obj corba:typecodefactory) 
                                  id name discriminator-type members)
  (check-tc-id id)
  (check-tc-name name)
  (check-tc-member-names (map 'list 'op:name members))
  (fix-recursive-tc
   (create-union-tc id name discriminator-type 
                    (map 'list
                         (lambda (member)
                           (check-tc-content-type (op:type member))
                           (let* ((label (op:label member))
                                  (label-value (any-value label)))
                             (list (if (and (eq :tk_octet (any-typecode label))
                                            (= 0 label-value))
                                     'default
                                     label-value)
                                   (op:name member)
                                   (op:type member))))
                         members))))

(define-method "CREATE_ALIAS_TC" ((obj corba:typecodefactory) id name typecode)
  (check-tc-id id)
  (check-tc-name name)
  (check-tc-content-type typecode)
  (fix-recursive-tc (create-alias-tc id name typecode)))

(define-method "CREATE_ENUM_TC" ((OBJ CORBA:TYPECODEFACTORY) id name members)
  (check-tc-id id)
  (check-tc-name name)
  (check-tc-member-names (coerce members 'list))
  (fix-recursive-tc (create-enum-tc id name members)))


(define-method "CREATE_ABSTRACT_INTERFACE_TC" ((OBJ CORBA:TYPECODEFACTORY)
                                               id name)
  (check-tc-id id)
  (check-tc-name name)
  (create-abstract-interface-tc id name))

(define-method "CREATE_NATIVE_TC" ((OBJ CORBA:TYPECODEFACTORY) id name)
  (check-tc-id id)
  (check-tc-name name)
  (create-native-tc id name))

(define-method "CREATE_VALUE_BOX_TC" ((OBJ CORBA:TYPECODEFACTORY)
                                      id name boxed_type)
  (check-tc-id id)
  (check-tc-name name)
  (check-tc-content-type boxed_type)
  (fix-recursive-tc (create-value-box-tc id name boxed_type)))

(DEFINE-ALIAS OMG.ORG/CORBA:VALUEMODIFIER
  :ID "IDL:omg.org/CORBA/ValueModifier:1.0"
  :NAME "ValueModifier"
  :TYPE OMG.ORG/CORBA:SHORT
  :TYPECODE OMG.ORG/CORBA:TC_SHORT)

(define-method "CREATE_VALUE_TC" ((OBJ CORBA:TYPECODEFACTORY)
                                      id name type_modifier concrete_base members)
  (check-tc-id id)
  (check-tc-name name)
  (fix-recursive-tc
   (create-value-tc id name type_modifier concrete_base
                    (simple-value-members members))))

(defun simple-value-members (members)
  (check-tc-member-names (map 'list 'op:name members))
  (map 'vector
       (lambda (member)
         (check-tc-content-type (op:type member))
         (list (op:name member)
               (op:type member)
               (op:access member)))
       members))

(define-method "CREATE_LOCAL_INTERFACE_TC" ((OBJ CORBA:TYPECODEFACTORY) id name)
  (check-tc-id id)
  (check-tc-name name)
  (create-local-interface-tc id name))



;;;; Providing access to the orb 

;;(defmethod the-orb ((req client-request))
;;  *the-orb*)

(defmethod the-orb ((obj CORBA:Object))
  *the-orb*)
