;;;; clorb-io.lisp  --  a reactive IO layer for CLORB

;; io-init ()
;; io-create-listener (&optional port)
;; io-create-descriptor () => desc
;; io-descriptor-destroy (desc)
;; io-descriptor-connect (desc host port)
;; io-descriptor-working-p (desc) => boolean
;; io-descriptor-set-read (desc buf start end)
;; io-descriptor-set-write (desc buf start end)
;; io-descriptor-status (desc) => status
;; io-descriptor-error (desc) => condition or nil
;; io-descriptor-read-buffer (desc) => buffer
;; io-driver ()
;; io-describe-descriptor (desc)
;; io-get-event () => event, desc
;; io-event-waiting-p () => boolean

;; io-descriptor-shortcut-connect (desc)
;; io-fix-broken ()
;; io-reset (&key listener)
;; io-system-switch (new-class)


(in-package :clorb)

(defparameter *io-background-write-treshold* 200)

(defvar *io-system-default-class* 'io-system-select-blocking-write)

(defvar *io-system* nil)

(defvar *io-listener* nil
  "listener socket (or similar). Producer of connections.")



;;;; Event Queue

(defgeneric io-system-queue-event (system event))
(defgeneric io-system-event-waiting-p (system))
(defgeneric io-system-get-event (system &optional non-blocking))


(defun io-queue-event (type desc)
  (io-system-queue-event *io-system* (list type desc)))



;;;; IO-DESCRIPTOR

(defstruct io-descriptor
  id
  (lock   (make-lock "desc"))
  status
  error
  connection
  ;; -- internal --
  stream
  (read-buffer  nil :type (or null octets))
  (read-pos       0 :type buffer-index)
  (read-limit     0 :type buffer-index)
  read-process
  (write-buffer nil :type (or null octets))
  (write-pos      0 :type buffer-index)
  (write-limit    0 :type buffer-index)
  write-process
  shortcut-p )

;; FIXME: define and document status codes
;; - define what error contain (e.g. a condition)
;; ~ status member nil :connected :broken :destroyed


(defmethod lock ((obj io-descriptor))
  (io-descriptor-lock obj))

(defun io-describe-descriptor (desc)
  (if (eql (io-descriptor-status desc) :broken)
      "BROKEN"
      (let ((stream (io-descriptor-stream desc)))
        (flet ((describe-peer (stream)
                 (multiple-value-bind (host port)
                     (socket-peer stream)
                   (format nil "~A:~A" host port))))
          (typecase stream
            (function "loopback")
            (stream (if (open-stream-p stream)
                        (describe-peer stream)
                        "CLOSED"))
            (t (describe-peer stream)))))))

(defmethod print-object ((desc io-descriptor) stream)
  (print-unreadable-object (desc stream :type t)
    (format stream "~a ~s"
            (io-descriptor-id desc)
            (io-descriptor-status desc))
    (when (io-descriptor-error desc)
      (format stream " error: ~s" (io-descriptor-error desc)))
    (when (io-descriptor-shortcut-p desc)
      (write-string " shortcut" stream))
    (format stream " R:~a/~a W:~a/~a"
            (io-descriptor-read-pos desc)
            (io-descriptor-read-limit desc)
            (io-descriptor-write-pos desc)
            (io-descriptor-write-limit desc))
    (when (and (not (io-descriptor-shortcut-p desc))
               (eql (io-descriptor-status desc) :connected))
      (format stream " ep=~a" (io-describe-descriptor desc)))))



;;;; Shortcut stream


(defun make-octet-stream (&optional (label (gensym)))
  (let ((buffer nil))
    (labels 
      ((reader (buf start end)
         (let ((len (- end start)))
           (when (<= len (length buffer))
             (mess 1 "Reading ~A (~D ~D)" label start end)
             (replace buf buffer 
                      :start1 start :end1 end
                      :start2 0 :end2 len)
             (setf buffer (subseq buffer len))
             len)))
       (writer (buf start end)
         (mess 1 "Writing ~A (~D ~D)" label start end)
         (let ((len (- end start)))
           (setf buffer (concatenate 'vector buffer (subseq buf start end)))
           len)))
      (lambda (op)
        (ecase op
          ((reader) #'reader)
          ((writer) #'writer)
          ((buffer) buffer))))))


(defun make-shortcut-stream (input output)
  (let ((reader (funcall input 'reader))
        (writer (funcall output 'writer)))
    (lambda (op buf start end)
      (ecase op
        ((read) (funcall reader buf start end))
        ((write) (funcall writer buf start end))))))

(defun io-shortcut-write (shortcut-stream seq start end)
  (funcall shortcut-stream 'write seq start end))

(defun io-shortcut-read (shortcut-stream seq start end)
  (funcall shortcut-stream 'read seq start end))


(defun io-descriptor-shortcut-connect (desc)
  (let ((i-stream (make-octet-stream))
        (o-stream (make-octet-stream)))
    (let ((other (io-create-descriptor
                  :stream (make-shortcut-stream o-stream i-stream)
                  :status :connected
                  :shortcut-p desc)))
      (io-queue-event :new other)
      (setf (io-descriptor-stream desc) (make-shortcut-stream i-stream o-stream))
      (setf (io-descriptor-shortcut-p desc) other))))



;;;; IO-System

(defclass io-system ()
  ((event-queue
    :initform nil
    :accessor event-queue)
   (io-descriptions
    :initform nil
    :accessor io-descriptions-of
    :documentation "List of all io-descriptors that we will do I/O on.")))


(defmethod io-system-queue-event ((sys io-system) event)
  (enqf (event-queue sys) event))

(defmethod io-system-get-event ((system io-system) &optional (non-blocking t))
  (when (and (not non-blocking)
             (queue-empty-p (event-queue system)))
    (io-system-driver system nil ))
  (deqf (event-queue system)))

(defmethod io-system-event-waiting-p ((system io-system))
  (not (queue-empty-p (event-queue system))))


(defun io-system-switch (new-class)
  (io-reset)
  (setq *io-system* (make-instance new-class)))


(defgeneric io-ready-for-read (system desc)
  (:method ((system io-system) desc)
           (declare (ignore desc))))

(defgeneric io-ready-for-write (system desc)
  (:documentation
   "Do the policy specific thing to initiate writing of the write buffer
stored in descriptor. Should return true if written and no event generated and
return false if event will be generated for completion.")
  (:method ((system io-system) desc)
    (declare (ignore desc))
    nil))


(defgeneric io-system-reset (system)
  (:documentation
   "IO-System specific reset handling.")
  (:method ((system io-system))
    (setf (event-queue system) nil)
    (flet ((kill (maybe-thread)
           (unless (symbolp maybe-thread)
             (end-process maybe-thread))))
    (dolist (desc (io-descriptions-of system))
      (kill (shiftf (io-descriptor-read-process desc) nil))
      (kill (shiftf (io-descriptor-write-process desc) nil))
      (ignore-errors
        (close (io-descriptor-stream desc)))
      (setf (io-descriptor-status desc) :broken)))
    (setf (io-descriptions-of system) nil)))

(defgeneric io-system-add-descriptor (system desc)
  (:documentation
   "Add a io-descriptor to the system.")
  (:method ((system io-system) desc)
    (push desc (io-descriptions-of system))))

(defgeneric io-system-delete-descriptor (system desc)
  (:method ((system io-system) desc)
    (deletef desc (io-descriptions-of system))))




;;;; Recovery

(defvar *last-io-port* nil)

(defun io-reset (&key (listener nil))
  (when (and listener *io-listener*)
    (ignore-errors
      (listener-close *io-listener*))
    (setq *io-listener* (io-create-listener *last-io-port*)))
  (io-system-reset *io-system*))



;;;; Creating io-descriptors


(defun io-create-listener (&optional port)
  (setq *last-io-port* port)
  (setq *io-listener* (open-passive-socket port))
  (setq port (or port (passive-socket-port *io-listener*)))
  (mess 3 "listener created on ~A" port)
  (values port (passive-socket-host *io-listener*)))


(defvar *seq-io-desc* 1)

(defun io-create-descriptor (&key connection status shortcut-p stream)
  (let ((desc (make-io-descriptor
               :id (incf *seq-io-desc*)
               :connection connection
               :status status
               :shortcut-p shortcut-p
               :stream stream
               :lock (make-lock "desc"))))
    (io-system-add-descriptor *io-system* desc)
    desc))

(defun io-descriptor-associate-connection (desc conn)
  (setf (io-descriptor-connection desc) conn))


(defun %io-descriptor-close (desc)
  (when-let (socket (io-descriptor-stream desc))
    (ignore-errors (socket-close socket))
    (setf (io-descriptor-stream desc) nil))
  (io-system-delete-descriptor *io-system* desc))


(defun io-descriptor-destroy (desc &optional soft)
  (with-synchronization desc
    (unless (or (eql (io-descriptor-status desc) :destroyed)
                (if soft
                    (io-descriptor-write-process desc)))
      (setf (io-descriptor-status desc) :destroyed)
      (%io-descriptor-close desc)
      (flet ((kill (maybe-thread)
               (unless (or (symbolp maybe-thread)
                           (eql maybe-thread (current-process)))
                 (end-process maybe-thread))))
        (kill (shiftf (io-descriptor-read-process desc) nil))
        (kill (shiftf (io-descriptor-write-process desc) nil)))
      (setf (io-descriptor-connection desc) nil
            (io-descriptor-read-buffer desc) nil
            (io-descriptor-write-buffer desc) nil))
    nil))


(defun io-descriptor-close (desc)
  (with-synchronization desc
    (setf (io-descriptor-status desc) :broken)
    (%io-descriptor-close desc)))


(defun io-descriptor-shutdown (desc)
  "Close the writing end of the socket."
  (socket-shutdown (io-descriptor-stream desc)))


(defun io-descriptor-connected-p (desc)
  (and (eql (io-descriptor-status desc) :connected)
       (io-descriptor-stream desc)))



;;;; Making connections


(defvar *host-translations* '()
  "Association list mapping hostnames as found in IORs to some
other hostname or IP-number.")

(defvar *io-loopback-p* nil
  "Function called with host and port and answers if connection to this host
port should use loopback." )


(defun io-descriptor-connect (desc host port)
  (setq host
        (or (cdr (assoc host *host-translations* :test #'string=))
            host))
  (cond ((and *io-loopback-p*
              (funcall *io-loopback-p* host port))
         (io-descriptor-shortcut-connect desc)
         (mess 3 "made shortcut connection"))
        (t
         (setf (io-descriptor-stream desc) (open-active-socket host port))
         (mess 3 "connect to ~A:~A = ~A" host port (io-descriptor-stream desc))))
  (setf (io-descriptor-status desc) :connected))


(defun io-descriptor-working-p (desc)
  (and (not (eql (io-descriptor-status desc) :broken))
       (not (eql (io-descriptor-status desc) :destroyed))
       (or (io-descriptor-shortcut-p desc)
           (not (socket-stream-closed-p (io-descriptor-stream desc))))))




;;;; Reading 


(defun io-descriptor-read-ready (desc)
  (and (io-descriptor-read-buffer desc)
       (< (io-descriptor-read-pos desc)
          (io-descriptor-read-limit desc))))


(defun io-descriptor-set-read (desc buf start end)
  (declare (optimize (speed 2) (safety 3) (debug 1)))
  (with-synchronization desc
    (setf (io-descriptor-read-buffer desc) buf
          (io-descriptor-read-pos desc) start
          (io-descriptor-read-limit desc) end))
  (when (and buf (> end start))
    (io-ready-for-read *io-system* desc))
  nil)




;;;; Writing 


(defun io-descriptor-write-ready (desc)
  (and (io-descriptor-write-buffer desc)
       (< (io-descriptor-write-pos desc)
          (io-descriptor-write-limit desc))))


(defun io-descriptor-set-write (desc buf start end)
  "Initiate writing of buffer.
Only the part between START and END (exlusive) is written. Returns
true if write has been completed and no :write-ready event will be
generated. Otherwise a :write-ready event will signal the completion
of this write."
  (with-synchronization desc
    (setf (io-descriptor-write-buffer desc) buf
          (io-descriptor-write-pos desc) start
          (io-descriptor-write-limit desc) end))
  (cond ((null buf))
        ((not (io-descriptor-working-p desc))
         (io-queue-event :no-write desc))
        (t
         (io-ready-for-write *io-system* desc))))



;;;; Listening


(defun io-listen (blocking)
  (let ((new (accept-connection-on-socket *io-listener* blocking)))
    (when new
      (let ((desc (io-create-descriptor)))
        (setf (io-descriptor-stream desc) new)
        (setf (io-descriptor-status desc) :connected)
        (io-queue-event :new desc)))))


(defun io-bg-listen ()
  (loop (io-listen t)))



;;;; Shortcut driver


(defun io-poll-shortcut (desc &optional io)
  (let ((result nil))
    (flet ((out (from)
             (when (io-descriptor-write-ready from)
               (io-shortcut-write (io-descriptor-stream from)
                                  (io-descriptor-write-buffer from)
                                  (io-descriptor-write-pos from)
                                  (io-descriptor-write-limit from))
               (setf (io-descriptor-write-pos from) (io-descriptor-write-limit from))
               ;;(io-queue-event :write-ready from)
               (setq result t)))
           (in (to)
             (when (and (io-descriptor-read-ready to)
                        (io-shortcut-read (io-descriptor-stream to)
                                          (io-descriptor-read-buffer to)
                                          (io-descriptor-read-pos to)
                                          (io-descriptor-read-limit to)))
               (setf (io-descriptor-read-pos to) (io-descriptor-read-limit to))
               (io-queue-event :read-ready to)
               (setq result t))))
      (case io
        (:read (in desc))
        (:write (out desc)
                (let ((save-result result))
                  (in (io-descriptor-shortcut-p desc))
                  (setq result save-result)))
        (t (out desc) (in desc)))
      result)))



;;;; Blocking Write Mixin


(defclass io-system-blocking-write ()
  ())


(defun io-broken-descriptor (desc error &optional activity)
  (with-synchronization desc
    (let ((do-close
              (or (not (io-descriptor-connected-p desc))
                  (not (case activity
                         ((:write) (io-descriptor-read-process desc))
                         ((:read)  (io-descriptor-write-process desc)))))))
    (setf (io-descriptor-status desc) :broken)
    (unless (io-descriptor-error desc)
      (setf (io-descriptor-error desc)  error))
    (case activity
      ((:write) (setf (io-descriptor-write-process desc) nil))
      ((:read)  (setf (io-descriptor-read-process desc) nil)))
    (when do-close
      (%io-descriptor-close desc)))))


(defun io-read-broken-descriptor (desc error)
  (io-broken-descriptor desc error :read)
  (io-queue-event :error desc))


(defun io-write-broken-descriptor (desc error)
  (io-broken-descriptor desc error :write)
  (io-queue-event :write-ready desc))


(defmethod io-ready-for-write ((system io-system-blocking-write) desc)
  "This method does the actual writing."
  (cond ((io-descriptor-shortcut-p desc)
         (io-poll-shortcut desc :write))
        (t
         (handler-case
             (with-slots (stream write-buffer write-pos write-limit
                                 write-process) desc
               (with-synchronization desc
                 (unless (io-descriptor-connected-p desc)
                   (return-from io-ready-for-write nil))
                 (setf write-process (current-process)))
               (write-octets write-buffer write-pos write-limit stream)
               (setf write-pos write-limit)
               (setf write-process nil))
           (stream-error (e) (io-broken-descriptor desc e :write)))
         t)))



;;;; Select based IO system


(defclass io-system-select (io-system)
  ())


(defun io-poll-desc (desc status)
  (with-slots (stream read-buffer read-limit read-pos
                      write-buffer write-pos write-limit) desc
    (declare (type (or null octets)
                   read-buffer write-buffer)
             (type buffer-index read-limit read-pos
                   write-limit write-pos))
    (let ((writing nil))
      (cond ((eq status :error)
             (io-read-broken-descriptor desc nil)
             (when (io-descriptor-write-ready desc)
               (io-queue-event :write-ready desc)))
            (status
             (handler-case
                 (progn
                   (when (and (not (eql status :output))
                              (io-descriptor-read-ready desc))
                     (let ((n (read-octets-no-hang read-buffer read-pos
                                                   read-limit stream)))
                       (incf read-pos n)
                       (if (>= read-pos read-limit)
                           (io-queue-event :read-ready desc)))) 
                   (when (and (member status '(:output :io :append))
                              (io-descriptor-write-ready desc))
                     (setq writing t)
                     (let ((n (write-octets-no-hang write-buffer write-pos
                                                    write-limit stream)))
                       (incf write-pos n)
                       (if (>= write-pos write-limit)
                           (io-queue-event :write-ready desc)))))
               (stream-error (e)
                 (if writing
                     (io-write-broken-descriptor desc e)
                     (io-read-broken-descriptor desc e)))))))))


(defun io-poll-select (system poll no-write)
  (let ((select (make-select)))
    (dolist (desc (io-descriptions-of system))
      (unless (io-descriptor-shortcut-p desc)
        (let ((stream (io-descriptor-stream desc)))
          (when stream
            (let ((input (io-descriptor-read-ready desc))
                  (output (if no-write nil
                              (io-descriptor-write-ready desc))))
              (when (or input output)
                (select-add-stream select stream input output desc) ))))))
    (when *io-listener*
      (select-add-listener select *io-listener*))
    (setq select (select-wait select poll))
    (select-do-result select #'io-poll-desc)
    (when *io-listener*
      (io-listen (select-listener select *io-listener*)))))


(defmethod io-system-driver ((system io-system-select) poll)
  (io-poll-select system poll nil))


(defmethod io-ready-for-write ((system io-system-select) desc)
  (if (io-descriptor-shortcut-p desc)
    (io-poll-shortcut desc :write)))


(defmethod io-ready-for-read ((system io-system-select) desc)
  (if (io-descriptor-shortcut-p desc)
    (io-poll-shortcut desc :read)))



;;;; Select with Blocking Writes Driver


(defclass io-system-select-blocking-write (io-system-blocking-write
                                           io-system-select)
  ())


(defmethod io-system-driver ((system io-system-select-blocking-write) poll)
  (io-poll-select system poll t))



;;;; Multi-threaded System Base


(defclass io-system-mt-base (io-system)
  ((lock   :initform (make-lock "system")  :reader lock)
   (listener-process  :initform nil  :accessor listener-process)
   (event-handler :initarg :event-handler :initform nil :accessor event-handler)))


(defmethod initialize-instance :after ((system io-system-mt-base) &key)
  (setf (event-queue system) (make-instance 'shared-queue)))


(defmethod io-start-bg-listen ((system io-system-mt-base))
  (unless (listener-process system)
    (setf (listener-process system)
          (start-process "CORBA listen" #'io-bg-listen))))


(defmethod io-system-queue-event ((system io-system-mt-base) event)
  (let ((handler (event-handler system)))
    (unless (and handler (funcall handler event))
      (enqueue (event-queue system) event))))

(defmethod io-system-get-event ((system io-system-mt-base)
                                &optional (non-blocking t))
  (with-lock (lock system)
    (io-start-bg-listen system))
  (dequeue (event-queue system) non-blocking))


(defmethod io-system-driver ((system io-system-mt-base) poll)
  (declare (ignore poll)))


(defmethod io-system-reset ((system io-system-mt-base))
  (with-lock (lock system)
    (end-process (shiftf (listener-process system) nil))
    (call-next-method)))


(defmethod io-system-add-descriptor ((system io-system-mt-base) desc)
  (with-lock (lock system)
    (call-next-method)))

(defmethod io-system-delete-descriptor ((system io-system-mt-base) desc)
  (with-lock (lock system)
    (call-next-method)))



;;;; Multi-threaded system with blocking writes


(defvar *io-mt-read-queue-garb* nil
  "Function that cleans idle connections.
Called from read-queue max-handler." )


(defclass io-system-mt-blocking-write (io-system-blocking-write
                                       io-system-mt-base)
  ((read-queue 
    :reader read-queue
    :initform (make-instance 'execution-queue
                :max-idle 4 :max-processes 20
                :max-handler *io-mt-read-queue-garb*
                :name-template "CLORB Reader"
                :executor 'io-mt-read))))


(defun io-mt-read (desc)
  (handler-case
    (with-slots (stream read-buffer read-pos read-limit read-process) desc
      (assert (null read-process))
      (loop
         (with-synchronization desc
           (unless (io-descriptor-connected-p desc)
             (return-from io-mt-read nil))
           (setf read-process (current-process)))
         (assert read-buffer nil
                 "read-buffer is set")
         (read-octets read-buffer read-pos read-limit stream)
         (setf read-pos read-limit)
         (io-queue-event :read-ready desc)
         (with-synchronization desc
           (unless (eql read-process :continue)
             (setf read-process nil)
             (return)))))
    (error (e) (io-read-broken-descriptor desc e))))


(defmethod io-ready-for-read ((system io-system-mt-blocking-write) desc)
  (if (io-descriptor-shortcut-p desc)
    (io-poll-shortcut desc :read)
    (with-synchronization desc
      (if (io-descriptor-read-process desc)
          (setf (io-descriptor-read-process desc) :continue)
          (enqueue (read-queue system) desc)))))



;;;; Multi-threaded system with non-blocking writes


(defclass io-system-mt-non-blocking-write (io-system-mt-blocking-write)
  ((write-queue
    :reader write-queue
    :initform (make-instance 'execution-queue
                :max-idle 4 :max-processes 10
                :name-template "CLORB Writer"
                :executor 'io-mt-write))))

(defun io-mt-write (desc)
  (handler-case
      (with-slots (stream write-buffer write-pos write-limit write-process) desc
        (assert (null write-process))
        (with-synchronization desc
          (unless (io-descriptor-connected-p desc)
            (return-from io-mt-write nil))
          (setf write-process (current-process)))
        (write-octets write-buffer write-pos write-limit stream)
        (setf write-pos write-limit)
        (setf write-process nil))
    (error (e) (io-broken-descriptor desc e)))
  (io-queue-event :write-ready desc))


(defmethod io-ready-for-write ((system io-system-mt-non-blocking-write) desc)
  (if (io-descriptor-shortcut-p desc)
    (io-poll-shortcut desc :write)
    (progn (enqueue (write-queue system) desc)
           nil)))



;;;; Init


(defun io-init ()
  (unless *io-system*
    (setq *io-system* (make-instance *io-system-default-class*))))



;;;; Driver


(defun io-event-waiting-p ()
  (io-system-event-waiting-p *io-system*))

(defun io-get-event (&optional non-blocking)
  (io-system-get-event *io-system* non-blocking))

(defun io-driver (poll)
  (io-system-driver *io-system* poll))



;;; clorb-io.lisp ends here
