;;;; clorb-srv.lisp --- CORBA server module

(in-package :clorb)


;;;; Default POA (boot objects)


(defclass BOOT-OBJECT-MANAGER (portableserver:servantactivator)
  ((boot-objects  :initform (make-hash-table :test #'equal)
                  :reader boot-objects-of)))

(defun create-default-poa (orb root-poa)
  (let ((poa
         (create-POA nil "_default_" (op:the_POAManager root-poa) 
              '(:use_servant_manager :user_id)
              orb
              :poaid 0)))
    (op:set_servant_manager poa (make-instance 'boot-object-manager))
    poa))

(define-method incarnate ((self boot-object-manager) oid adapter)
  (declare (ignore adapter))
  (let ((name (oid-to-string oid)))
    (let ((obj (gethash name (boot-objects-of self))))
      (cond (obj
             (signal (PortableServer:ForwardRequest
                      :forward_reference obj)))
            (t
             (raise-system-exception 'CORBA:OBJECT_NOT_EXIST
                                     0 :completed_no))))))


(defmethod set-boot-object ((self BOOT-OBJECT-MANAGER) name objref)
  (setf (gethash name (boot-objects-of self)) objref))



;;;; Root Adapter


(defclass root-adapter ()
  ((the-orb   :initarg :orb       :accessor the-orb)
   (root-poa  :initarg :root-poa  :accessor root-poa-of)
   (boot-poa  :initarg :boot-poa  :accessor boot-poa-of)
   ;; request dispatching
   (request-dispatcher :initarg :request-dispatcher
                       :initform #'default-dispatch-request
                       :accessor request-dispatcher)
   (dispatch-queue   :initform (make-instance 'shared-queue)
                     :accessor dispatch-queue-of)
   (dispatch-process :initform nil
                     :accessor dispatch-process-of)
   ;; registry
   (last-poaid :initform 0  :accessor last-poaid-of)
   (poa-map    :initform (make-hash-table :test #'eql) :accessor poa-map-of)))


(defmethod set-boot-object ((self root-adapter) name objref)
  (set-boot-object (op:get_servant_manager (boot-poa-of self)) name objref))

(defmethod set-boot-object ((self clorb-orb) name objref)
  (set-boot-object (adapter self) name objref))


(defun create-root-adapter (orb)
  (let ((adapter 
         (make-instance 'root-adapter
                        :orb orb)))
    (setf (adapter orb) adapter)
    (setf (root-poa-of adapter) (create-POA nil "root" nil nil orb))
    (setf (boot-poa-of adapter) (create-default-poa orb (root-poa-of adapter)))
    adapter))


(defmethod root-poa-of ((poa POA))
  (root-poa-of (the-orb poa)))
(defmethod root-poa-of ((orb clorb-orb))
  (root-poa-of (adapter orb)))


(defmethod next-poaid ((self root-adapter))
  (incf (last-poaid-of self)))

(defmethod adapter-register-poa ((adapter root-adapter) poa)
  (setf (gethash (poa-poaid poa) (poa-map-of adapter)) poa))

(defmethod adapter-unregister-poa ((adapter root-adapter) poa)
  (remhash (poa-poaid poa) (poa-map-of adapter)))

(defmethod poa-by-id ((adapter root-adapter) poaid)
  (gethash poaid (poa-map-of adapter)))



;;;; Request Dispatcher


(defun default-dispatch-request (root-adapter req objkey)
  (multiple-value-bind (type poa-spec object-id)
                       (decode-object-key objkey)
    (declare (ignore type))
    (setf (request-object-id req) object-id)
    (setf (poa-spec req) poa-spec)
    (poa-dispatch (root-poa-of root-adapter) req)))


(defun dispatch-request (orb req objkey)
  (let ((root-adapter (adapter orb)))
    (funcall (request-dispatcher root-adapter)
             root-adapter req objkey)))


(defun queued-dispatch (adapter req objkey)
  (enqueue (dispatch-queue-of adapter) (list req objkey)))

(defun bg-dispatcher (adapter)
  (loop
     (destructuring-bind (req objkey)
         (dequeue (dispatch-queue-of adapter))
       (default-dispatch-request adapter req objkey))))

(defun start-dispatcher-process (adapter)
  (unless (dispatch-process-of adapter)
    (setf (request-dispatcher adapter)
          #'queued-dispatch)
    (setf (dispatch-process-of adapter)
          (start-process "clorb dispatcher"
                         'bg-dispatcher adapter))))



;;;; Server proper


(defun setup-server (&optional (orb (CORBA:ORB_init)))
  (multiple-value-bind (port host)
      (io-create-listener (orb-port orb))
    (setf (orb-port orb) port)
    (unless (orb-host orb)
      (setf (orb-host orb) host))
    (setup-shortcut))
  (create-root-adapter orb))


(defun root-poa (&optional (orb (CORBA:ORB_init)))
  (unless (adapter orb)
    (setup-server orb))
  (root-poa-of (adapter orb)))


(defun setup-incoming-connection (conn)
  (connection-init-read conn nil +iiop-header-size+ #'poa-message-handler))


(defun setup-shortcut ()
  (let ((orb (CORBA:ORB_init)))
    (setq *io-loopback-p*
          (lambda (host port)
            (and (equal host (orb-host orb))
                 (= port (orb-port orb)))))))


(defun shortcut-off ()
  (setq *io-loopback-p* nil))


(defun poa-connection-handler (desc)
  (let ((conn (make-associated-connection *the-orb* desc :server-p t)))
    (setup-incoming-connection conn)))


(defun get-fragment-request (conn)
  (let ((buffer (read-buffer-of conn)))
    (connection-add-fragment conn buffer +iiop-header-size+)
    (setup-incoming-connection conn)))


(defun poa-message-handler (conn)
  (let ((buffer (read-buffer-of conn)))
    (multiple-value-bind (msgtype fragmented version)
                         (unmarshal-giop-header buffer)
      (setf (buffer-giop-version buffer) version)
      (let ((decode-fun
             (case msgtype
               ((:request)         #'poa-request-handler)
               ((:cancelrequest)   #'poa-cancelrequest-handler)
               ((:locaterequest)   #'poa-locaterequest-handler)
               ((:closeconnection) #'poa-closeconnection-handler)
               ((:messageerror)    #'poa-messageerror-handler)
               ((:fragment)        
                (prog1 (if fragmented #'get-fragment-request #'get-fragment-last)
                  (setq fragmented nil))))))
        (cond ((> (giop-version-minor version) 1)
               (warn "Receiving unsupported GIOP version: ~A" version)
               (connection-message-error conn giop-1-1))
              ((null decode-fun)
               (warn "Unknown message type: ~A" msgtype)
               (connection-message-error conn))
              (t
               (let ((size (unmarshal-ulong buffer)))
                 (mess 1 "Message type ~A size ~A" msgtype size)
                 (when fragmented
                   (connection-init-defragmentation conn decode-fun)
                   (setq decode-fun #'get-fragment-request))
                 (if (zerop size)
                   (funcall decode-fun conn)
                   (connection-init-read conn t (+ size +iiop-header-size+) decode-fun)))))))))




;;;; Request Handling


(defun poa-request-handler (conn)
  (let ((buffer (read-buffer-of conn)))
    (setup-incoming-connection conn)
    (let ((service-context (unmarshal-service-context buffer))
          (req-id (unmarshal-ulong buffer))
          (response (unmarshal-octet buffer))
          (object-key (unmarshal-osequence buffer))
          (operation (unmarshal-string buffer))
          (principal (unmarshal-osequence buffer)))
      (connection-receive-request conn req-id buffer
                                  service-context response
                                  object-key operation principal) )))


(defun connection-receive-request (conn req-id buffer
                                   service-context response object-key
                                   operation principal)
  (cond ((shutdown-status conn)
         (mess 4 "Receive request after shutdown on ~A"
               conn))
        (t
         (let* ((orb (the-orb conn))
                (request (create-server-request
                          orb :connection conn
                          :request-id req-id :operation operation
                          :service-context service-context :input buffer 
                          :giop-version (buffer-giop-version buffer)
                          :state :wait :response-flags response)))
           (connection-add-server-request conn request)
           (mess 3 "#~D op ~A on '~/clorb:stroid/' from '~/clorb:stroid/'"
                 req-id operation object-key principal)
           (has-received-request-header orb request)
           (dispatch-request orb request object-key) ))))


(defun poa-cancelrequest-handler (conn)
  (let ((buffer (read-buffer-of conn)))
    (setup-incoming-connection conn)
    (let* ((req-id (unmarshal-ulong buffer)))
      (mess 3 "#~D cancel" req-id)
      (loop for req in (connection-server-requests conn)
            when (eql req-id (request-id req))
            do (return 
                (progn (setf (response-flags req) 0)
                       (when (eql (request-state req) :wait)
                         (setf (request-state req) :canceled)) ))))))



(defun poa-locaterequest-handler (conn)
  (let ((buffer (read-buffer-of conn)))
    (setup-incoming-connection conn)
    (let* ((orb (the-orb conn))
           (req-id (unmarshal-ulong buffer))
           (object-key (unmarshal-osequence buffer))
           (operation "_locate")
           (request 
            (create-server-request
             orb :operation operation :request-id req-id :kind :locate
             :response-flags 1 :giop-version (buffer-giop-version buffer)
             :input buffer :connection conn)))
      (dispatch-request orb request object-key))))


(defun poa-closeconnection-handler (conn)
  (declare (ignore conn))
  ;;FIXME:
  ;; for giop 1.0 this is not expected on the server side
  (error "NYI"))

(defun poa-messageerror-handler (conn)
  (declare (ignore conn))
  ;;FIXME:
  (error "NYI"))


(defun initialize-poa (orb)
  (set-initial-reference orb "RootPOA" #'root-POA)
  (set-initial-reference orb "POACurrent" nil
                         (make-instance 'PortableServer::Current)))


(eval-when (:load-toplevel :execute)
  (setq *new-connection-callback* 'poa-connection-handler)
  (pushnew 'initialize-poa *orb-initializers* ))

(defun main-loop (&optional (orb (CORBA:ORB_init)))
  (root-POA)
  (op:run orb))

(defun recover (&optional (orb (CORBA:ORB_init)))
  (io-reset :listener nil)
  (main-loop orb))


;;; clorb-srv.lisp ends here
