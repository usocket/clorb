(in-package :cl-user)

(defclass HELLO-WORLD-DSI (portableserver:dynamicimplementation)
  ((motd :initform "Hello World")))

(corba:define-method primary_interface ((servant hello-world-dsi) oid poa)
  (declare (ignore servant oid poa))
  "IDL:Hello/World:1.0")

(corba:define-method invoke ((self hello-world-dsi) request)
  (let ((operation (op:operation request)))
    (cond 
     ((equal operation "greet")
      (op:arguments request nil)        ; No arguments
      (op:set_result request (slot-value self 'motd)))
     (t
      (op:set_exception request (make-condition 'CORBA:BAD_OPERATION
                                  :completed :completed_no))))))


(defvar *hello-dynservant* nil)

(defun setup-hellodyn (&key file name)
  (unless *hello-dynservant*
    (setq *hello-dynservant* 
      (make-instance 'hello-world-dsi)))
  (let ((orb (CORBA:ORB_init)))
    ;; Register the object servant with the ORB
    (let ((poa (op:resolve_initial_references orb "RootPOA")))
      ;; Optionally activate object (RootPOA has implicit activation)
      ;;(op:activate_object poa *hello-dynservant*)
      ;; Activate the POAManager to allow the POA to accept requests
      (op:activate (op:the_poamanager poa)))
    ;; Store object reference in file or in naming service
    (when file
      (with-open-file (wr file :direction :output
                       :if-exists :supersede)
        (format wr "~A~%" (op:object_to_string orb *hello-dynservant*))))
    (when name
      ;; Clorb utility function to access naming service
      (clorb:rebind *hello-dynservant* name))))
