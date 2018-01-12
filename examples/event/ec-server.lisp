;;; ec-server.lisp  --  CORBA Event Channel

(in-package :net.cddr.clorb.event)


;;;; Utilities

(defun delete-swap (list object)
  (delete object list))

(define-modify-macro deletef (object) delete-swap  )


;;;; EVENT-CHANNEL CLASS ---------------------------------------------------

;;;  ------------------------------------------------- event-channel -------

(defclass EVENT-CHANNEL (CosEventChannelAdmin:EventChannel-servant)
  ((consumers  :initform '() :accessor consumers)
   (suppliers  :initform '() :accessor suppliers)
   (consumer-admin :initform nil :accessor consumer-admin)
   (supplier-admin :initform nil :accessor supplier-admin)))

(define-method for_consumers ((servant event-channel))
  (or (consumer-admin servant)
      (setf (consumer-admin servant) 
        (make-instance 'consumer-admin :channel servant))))

(define-method for_suppliers ((servant event-channel))
  (or (supplier-admin servant)
      (setf (supplier-admin servant)
        (make-instance 'supplier-admin :channel servant))))

(define-method destroy ((servant event-channel))
  (values))

(defmethod add-event ((ch event-channel) event)
  (mapc (lambda (subs) (add-event subs event))
        (consumers ch)))



;;;; ------------------------------------------------- consumer-admin ------

(defclass CONSUMER-ADMIN (CosEventChannelAdmin:ConsumerAdmin-servant)
  ((channel :initarg :channel :reader channel)))

(define-method obtain_push_supplier ((servant consumer-admin))
  (make-instance 'push-supplier :channel (channel servant)))

(define-method obtain_pull_supplier ((servant consumer-admin))
  (make-instance 'pull-supplier :channel (channel servant)))


;;;; ------------------------------------------------- supplier-admin ------

(defclass SUPPLIER-ADMIN (CosEventChannelAdmin:SupplierAdmin-servant)
  ((channel :initarg :channel :reader channel)))

(define-method obtain_push_consumer ((servant supplier-admin))
  (make-instance 'push-consumer :channel (channel servant)))

(define-method obtain_pull_consumer ((servant supplier-admin))
  (make-instance 'pull-consumer :channel (channel servant)))


;;;; SUPPLIER CLASSES ----------------------------------------------------

(defclass SUPPLIER () 
  ((channel :initarg :channel :accessor channel)
   (consumer :initform nil  :accessor consumer)))

(defmethod connect-consumer ((self supplier) consumer)
  (when (consumer self)
    (error (coseventchanneladmin:alreadyconnected)))
  (setf (consumer self) consumer)
  (pushnew self (consumers (channel self))))

(defmethod disconnect ((self supplier))
  (setf (consumer self) nil)
  (deletef (consumers (channel self)) self)
  (op:deactivate_object (op:_poa self) (op:_object_id self)))


;;;; ------------------------------------------------- push-supplier -------

(defclass PUSH-SUPPLIER (supplier coseventcomm:pushsupplier-servant)
  ())

(defmethod add-event ((self push-supplier) event)
  (ignore-errors
   (op:push (consumer self) event)))

(define-method connect_push_consumer ((self push-supplier) push-consumer)
  (connect-consumer self push-consumer))

(define-method disconnect_push_supplier ((self push-supplier))
  (disconnect self))


;;;; --------------------------------------------------pull-supplier -------

(defclass PULL-SUPPLIER (supplier CosEventComm:PullSupplier-servant)
  ((pending-pull :initform nil  :accessor pending-pull)
   (events       :initform '()  :accessor events)))

(defmethod add-event ((self pull-supplier) event)
  (let ((sreq (pending-pull self)))
    (if sreq
        (progn (setf (pending-pull self) nil)
               (set-request-result sreq (list event))
               (send-response sreq))
      (push event (events self)))))


(define-method pull ((self pull-supplier))
  (if (events self)
      (pop (events self))
    (progn (setf (pending-pull self) *current-server-request*)
           'defer)))

(define-method try_pull ((self pull-supplier))
  (let ((res (CORBA:Any :any-typecode CORBA:tc_null))
        (has-event nil))
    (when (events self)
      (setq res (pop (events self)))
      (setq has-event t))
    (values res has-event)))

(define-method connect_pull_consumer ((servant pull-supplier) pull-consumer)
  (connect-consumer servant pull-consumer))

(define-method disconnect_pull_supplier ((pull-supplier pull-supplier))
  (disconnect pull-supplier))


;;;; CONSUMER CLASSES ------------------------------------------------------

(defclass CONSUMER ()
  ((state :initarg :channel :accessor channel)
   (supplier :accessor supplier)))

(defmethod connect-supplier ((self consumer) supplier)
  (when (slot-boundp self 'supplier)
    (error 'userexception :id +already-connected-id+))
  (push self (suppliers (channel self)))
  (setf (supplier self) supplier))

(defmethod disconnect ((self consumer))
  (slot-makunbound self 'supplier)
  (deletef (suppliers (channel self)) self)
  (when *poa-current*
    (op:deactivate_object (poa-current-POA *poa-current*)
                       (poa-current-object-id *poa-current*))))


;;;; ------------------------------------------------- push-consumer -------

(defclass PUSH-CONSUMER (consumer push-consumer-servant)
  ())

(define-method push ((self push-consumer) any)
  (add-event (channel self) any))

(define-method connect_push_supplier ((self push-consumer) push-supplier)
  (connect-supplier self push-supplier))

(define-method disconnect_push_consumer ((self push-consumer))
  "Disconnect push consumer and clean up"
  (disconnect self))



;;;; ------------------------------------------------- pull-consumer -------

(defclass PULL-CONSUMER (consumer pull-consumer-servant)
  ())

(define-method connect_pull_supplier ((self pull-consumer) supplier)
  ;; FIXME: register for poll or start thread?
  (connect-supplier self supplier))

(defun do-pull (pull-consumer)
  (let ((event (op:pull (supplier pull-consumer))))
    (add-event (channel pull-consumer) event)))

(define-method disconnect_pull_consumer ((self pull-consumer))
  (disconnect self))


;;;; Misc ------------------------------------------------------------------

(defvar *my-ch* nil)

;;(defvar *my-pus* (let ((subs (op::obtain_pull_supplier *my-ch*)))
;;                   (op::connect_pull_consumer subs nil)
;;                   subs))

(defclass PUSH-TRACE (push-consumer) ())
(define-method push ((servant push-trace) event)
  (format t "~&>>> Push ~S~%" event))

(defun setup-ec ()
  (unless *my-ch*
    (setq *my-ch* (make-instance 'event-channel)))
  (rebind *my-ch* "ch1")
  ;;(rebind *my-pus* "ch1b")
  (rebind (make-instance 'push-consumer :channel *my-ch*) "ch1pc")
  (rebind (make-instance 'push-supplier :channel *my-ch*) "ch1ps"))

(defun test-ec ()
  (let ((consumer (make-instance 'push-trace))
        (ch (resolve "ch1")))
    (let ((in-adm (invoke ch "for_suppliers"))
          (out-adm (invoke ch "for_consumers")))
      (let ((in (invoke in-adm "obtain_push_consumer"))
            (out (invoke out-adm "obtain_push_supplier")))
        ;; ? (invoke in "connect_push_supplier" ?)
        (invoke out "connect_push_consumer" consumer)
        (invoke in "push" "Tjolla hoppsan")
        (invoke in "push" 345)
        (invoke in "push" '("how" "do"))
        (invoke out "disconnect_push_supplier")))))

(defun test-ec-1 ()
  (let ((ch (resolve "ch1")))
    (let ((in-adm (invoke ch "for_suppliers")))
      (let ((in (invoke in-adm "obtain_push_consumer")))
        ;; ? (invoke in "connect_push_supplier" ?)
        (invoke in "push" "Tjolla hoppsan")
        (invoke in "push" 345)
        (invoke in "push" '("how" "do"))
        ))))

(defun test-ec-2 ()
  (let ((consumer (make-instance 'push-trace))
        (ch (resolve "ch1")))
    (let ((out-adm (invoke ch "for_consumers")))
      (let ((out (invoke out-adm "obtain_push_supplier")))
        (invoke out "connect_push_consumer" consumer)
        (unwind-protect
             (op:run (corba:orb_init))
          (invoke out "disconnect_push_supplier"))))))
