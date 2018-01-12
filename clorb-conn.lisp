;;; clorb-conn.lisp --- Connection Layer

(in-package :clorb)

(defvar *request-id-seq* 0)

;;(defvar *default-trace-connection* nil)


;;; Managing connections
;; defun create-connection (orb host port)
;; defun make-associated-connection (orb desc)
;; defun connection-working-p (conn)
;; defun connection-destroy (conn)
;; defun connection-shutdown (conn)


;;; Sending
;; defun connection-send-buffer (conn buffer)

;;; Client interface
;; defmethod next-request-id ((conn Connection))
;;- defun connection-add-client-request (conn request)
;;- defun connection-remove-client-request (conn request)
;; defun connection-send-request (conn buffer req)


;;; IIOP interface
;; defun connection-init-read (conn continue-p n callback)
;; defun connection-receive-reply (conn request-id buffer status ..)
;; defun connection-receive-locate-reply (conn request-id buffer status)
;; -defun find-waiting-client-request (conn request-id)
;; defun connection-add-fragment (conn buffer header-size)
;; defun connection-error (conn)
;; defun connection-close (conn)
;; defun connection-read-ready (conn)
;; defmethod connection-write-ready ((conn connection))
;; defun connection-init-defragmentation (conn handler)

;;; Server interface
;; defun connection-add-server-request (conn request)
;; defun connection-remove-server-request (conn request)

;;; Gc-connections (also Managing connections ?)
;; defun gc-connections (&optional except n)
;; defun auto-gc-read-handler (q desc)


;;; From IIOP

;;new
;; (defun connection-get-buffer (conn)


;;; Sending messages
;; defun connection-reply (conn giop-version reply-type request-id status
;; defun connection-message-error (conn &optional (version giop-1-0))

;;; IIOP
;; defun get-fragment (conn)
;; defun get-fragment-last (conn)
;; defun get-response-0 (conn)
;; defun get-response-reply (conn)
;; defun get-response-locate-reply (conn &aux (buffer (read-buffer-of conn)))
;; defun setup-outgoing-connection (conn)

;;; Message Format
;; defun server-close-connection-msg (conn)
;; defun marshal-locate-request (buffer req-id profile)
;; defun marshal-request-message (buffer ..)



;;;; Connection Class


(defclass Connection (synchronized)
  ((the-orb         :initarg :orb                          :accessor the-orb)
   (read-buffer                                            :accessor read-buffer-of)
   (read-callback                                          :accessor connection-read-callback)
   (write-buffer                             :initform nil :accessor write-buffer-of)
   (write-request                            :initform nil :accessor write-request-of)
   (write-count                              :initform 0   :accessor write-count-of )
   (io-descriptor   :initarg :io-descriptor  :initform nil :accessor connection-io-descriptor)
   (client-requests                          :initform nil :accessor connection-client-requests)
   (server-requests                          :initform nil :accessor connection-server-requests)
   (activity        :initarg :activity       :initform t   :accessor activity)
   (server-p        :initarg :server-p       :initform nil :accessor server-p)
   (shutdown-status   :initform nil  :accessor shutdown-status
                      :documentation "State of shutdown nil, :sent, :closed")
   ;; Defrag support
   (assembled-handler  :initform nil  :accessor assembled-handler)
   (fragment-buffer    :initform nil  :accessor fragment-buffer)))


(defmethod print-object ((conn connection) stream)
  (print-unreadable-object (conn stream :identity t :type t)
    (let ((desc (connection-io-descriptor conn)))
      (when desc (write-string (io-describe-descriptor desc) stream)))))




;;;; Connection Methods


(defmethod next-request-id ((conn Connection))
  (incf *request-id-seq*))


;;(defvar *desc-conn* (make-hash-table))

(defun make-associated-connection (orb desc &key server-p)
  (let* ((conn (make-instance 'Connection :orb orb :io-descriptor desc
                              :server-p server-p)))
    (io-descriptor-associate-connection desc conn)
    conn))


(defun connection-destroy (conn)
  (with-synchronization conn
    (when-let (desc (connection-io-descriptor conn))
      (io-descriptor-destroy desc t))))


(defun create-connection (orb host port)
  (let ((desc (io-create-descriptor)))
    (handler-case
      (progn (io-descriptor-connect desc host port)
             (make-associated-connection orb desc))
      (error (err)
             (mess 4 "(connect ~S ~S): ~A" host port err)
             (setf (io-descriptor-error desc) err)
             (setf (io-descriptor-status desc) :broken)
             (io-descriptor-destroy desc)
             nil))))


(defun connection-working-p (conn)
  (when-let (desc (connection-io-descriptor conn))
    (io-descriptor-working-p desc)))


(defun connection-init-read (conn continue-p n callback)
  (setf (connection-read-callback conn) callback)
  (let ((desc (connection-io-descriptor conn)))
    (let* ((buffer (if continue-p
                     (read-buffer-of conn)
                     (get-work-buffer (the-orb conn))))
           (octets (buffer-octets buffer))
           (start (fill-pointer octets)))
      (unless continue-p
        (setf (read-buffer-of conn) buffer))
      (when (< (array-total-size octets) n)
        (adjust-array octets n))
      (setf (fill-pointer octets) n)
      (io-descriptor-set-read desc octets start n))))


(defun connection-shutdown (conn)
  (let ((status nil) (action nil))
    (with-synchronization conn
      (case (setq status (shutdown-status conn))
        ((nil)
         (setf action :send)
         (setq status :sent)
         (setf (shutdown-status conn) status))
        ((:sent) nil)
        ((:closed)
         (setq action :destroy)
         (setf status :destroyed))))
    (case action
      ((:send)
       (connection-send-buffer conn (server-close-connection-msg conn)))
      ((:destroy)
       (connection-destroy conn)))
    status))


(defun %add-client-request (conn request)
  (push request (connection-client-requests conn)))


(defun connection-send-buffer (conn buffer &optional request)
  (flet ((write-free-p (conn)
           (not (write-buffer-of conn))))
    (loop
       (with-synchronization conn
         (when (write-free-p conn)
           (setf (write-buffer-of conn) buffer
                 (write-request-of conn) request)
           (when request
             (%add-client-request conn request))
           (return)))
       (orb-condition-wait conn #'write-free-p conn))
    (let ((desc (connection-io-descriptor conn))
          (octets (buffer-octets buffer)))
      (cond ((io-descriptor-set-write desc octets 0 (length octets))
             (connection-write-ready conn))))))


(defun connection-send-request (conn buffer req)
  (connection-send-buffer conn buffer req))


(defun find-waiting-client-request (conn request-id)
  (with-synchronization conn
    (let ((req-list (connection-client-requests conn)))
      (let ((req (find request-id req-list :key #'request-id)))
        (if req
            (setf (connection-client-requests conn) (delete req req-list))
            (mess 4 "Unexpected response with request id ~d" request-id))
        req))))


(defun connection-receive-reply (conn request-id buffer status service-context)
  (let ((req (find-waiting-client-request conn request-id)))
    (when req
      (request-reply req status buffer service-context))))

(defun connection-receive-locate-reply (conn request-id buffer status)
  (let ((req (find-waiting-client-request conn request-id)))
    (when req
      (request-locate-reply req status buffer))))


(defun connection-add-server-request (conn request)
  (with-synchronization conn
    (push request (connection-server-requests conn))))

(defun connection-remove-server-request (conn request)
  (with-synchronization conn
    (setf (connection-server-requests conn)
          (delete request (connection-server-requests conn)))))


(defun connection-add-fragment (conn buffer header-size)
  (with-accessors ((fragment-buffer fragment-buffer)) conn
    (when fragment-buffer
      (setf (buffer-octets buffer)
            (concatenate 'octets
                         (buffer-octets fragment-buffer)
                         (subseq (buffer-octets buffer) header-size))))
    (setf fragment-buffer buffer)))


;;;; Cleaning and Garbing Connections


(defun gc-connections (&optional except n)
  (dolist (desc (io-descriptions-of *io-system*))
    (let ((conn (io-descriptor-connection desc)))
      (unless (or (null conn) (member desc except)
                  (io-descriptor-shortcut-p desc)
                  (write-buffer-of conn)
                  (connection-client-requests conn)
                  (connection-server-requests conn))
        (if (activity conn)
            (setf (activity conn) nil)
            (progn
              (if (server-p conn)
                  (connection-shutdown conn)
                  (io-descriptor-destroy desc t))
              (if (numberp n)
                  (if (< (decf n) 1)
                      (return)))))))))


(defun auto-gc-read-handler (q desc)
  ;;(declare (ignore q))
  ;; max-processes handler for read queue
  ;; Does a gc-connections and then allow new process any way!
  ;;
  (ignore-errors
    (gc-connections (list desc) 1))
  (unless (< (process-count q) (max-processes q))
    (ignore-errors
      (gc-connections (list desc) 1)))
  t)

;;(setf (max-handler (read-queue *io-system*)) 'auto-gc-read-handler)

(setq *io-mt-read-queue-garb* 'auto-gc-read-handler)



;;;; Connection events


(defun connection-error (conn)
  ;; Called when there is IO error
  (with-synchronization conn
    (dolist (req (connection-client-requests conn))
      (request-reply-exception req :error
                               (system-exception 'CORBA:COMM_FAILURE)))
    (setf (connection-client-requests conn) nil)))


(defun connection-close (conn)
  ;; Called on recipt of a connection close message
  (with-synchronization conn
    (io-descriptor-close (connection-io-descriptor conn))
    ;; The server should not have started on any of the outstanding requests.
    (dolist (req (connection-client-requests conn))
      (request-reply-exception req :error
                               (system-exception 'CORBA:TRANSIENT 3 :completed_no)))
    (setf (connection-client-requests conn) nil)))


(defun connection-read-ready (conn)
  (funcall (connection-read-callback conn) conn))


(defmethod connection-write-ready ((conn connection))
  (with-synchronization conn
    (setf (activity conn) t)
    (incf (write-count-of conn))
    (setf (write-buffer-of conn) nil
          (write-request-of conn) nil)
    (when (eql (shutdown-status conn) :sent)
      (io-descriptor-shutdown (connection-io-descriptor conn))
      (setf (shutdown-status conn) :closed))
    (synch-notify conn)))


(defun connection-no-write (conn)
  (with-synchronization conn
    (when-let (req (write-request-of conn))
      (when (connection-client-requests conn)
        ;; if (connection-client-requests conn) is nil then there must
        ;; have been a read error
        (request-no-write req)
        (deletef req (connection-client-requests conn))))
    (setf (write-buffer-of conn) nil
          (write-request-of conn) nil)
    (synch-notify conn)))


(defun connection-init-defragmentation (conn handler)
  (cond ((assembled-handler conn)
         (mess 5 "Fragment overrun")
         (connection-error conn)
         nil)
        (t
         (setf (assembled-handler conn) handler)
         (setf (fragment-buffer conn) nil)
         t)))



;;;; Event loop (orb-wait)

(defvar *new-connection-callback*
  (lambda (desc)
    (io-descriptor-destroy desc)))


(defvar *running-orb* t
  "Will be set to true in the process that is running the ORB server part.
If this is true, orb-wait will check server streams also.
Can be set to true globally for singel-process / development.")


(defun orb-wait (wait-func &rest wait-args)
  (if *running-orb*
    (loop until (apply wait-func wait-args) do (orb-work *the-orb* t nil))
    (apply #'process-wait "orb-wait" wait-func wait-args)))


(defun orb-condition-wait (obj wait-func &rest wait-args)
  "Wait for (wait-func . wait-args) to be true.
Use the syncronization of obj, expect obj not to be locked when called.
Obj should be synch-notify when the condition is possibly changed."
  (if *running-orb*
      ;; Also handle server stuff
      (loop until (apply wait-func wait-args) do (orb-work *the-orb* t nil))      
      ;; No server stuff
      (apply #'synch-wait-on-condition obj wait-func wait-args)))


(defun orb-run-queue (orb)
  (loop while (work-queue orb)
     do (funcall (pop (work-queue orb)))))


(defun process-event (event)
  (let* ((desc (second event))
         (conn (io-descriptor-connection desc)))
    (mess 1 "io-event: ~S ~A ~A" (car event) (io-descriptor-stream desc) conn)
    (case (car event)
      (:read-ready
       (when conn (connection-read-ready conn)))
      (:write-ready
       (io-descriptor-set-write desc nil 0 0)
       (when conn (connection-write-ready conn)))
      (:no-write
       (io-descriptor-set-write desc nil 0 0)
       (when conn (connection-no-write conn)))
      (:new
       (funcall *new-connection-callback* desc))
      (:connected
       ;; Not implemented yet..; for outgoing connections setup
       nil)
      (:error
       (mess 4 "Error: ~A" (io-descriptor-error desc))
       (if conn
           (connection-error conn)
           (io-descriptor-destroy desc))))))


(defun orb-work (orb run-queue poll)
  (when run-queue
    (orb-run-queue orb))
  (when poll
    (io-driver poll))
  (loop for event = (io-get-event poll)
     while event
     do (process-event event)
       (setq poll t)))

  

