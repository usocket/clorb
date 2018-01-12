(in-package :cl-user)

(defclass HELLO-WORLD (CLORB_EX:helloworld-servant)
  ((motd :initform 
         (format nil "Hello World from ~A" (lisp-implementation-type)))))

(corba:define-method greet ((self hello-world))
  (slot-value self 'motd))


(defvar *hello-servant* nil)

(defun setup-hello (&key file name)
  (unless *hello-servant*
    (setq *hello-servant* 
          (make-instance 'hello-world)))
  (let ((orb (CORBA:ORB_init)))
    ;; Register the object servant with the ORB
    (let ((object 
           (let ((poa (op:resolve_initial_references orb "RootPOA")))
             ;; Optionally activate object (RootPOA has implicit activation)
             ;;(op:activate_object poa *hello-servant*)
             ;; Activate the POAManager to allow the POA to accept requests
             (op:activate (op:the_poamanager poa))
             ;; Implicit activation
             (op:_this *hello-servant*))))
      ;; Store object reference in file or in naming service
      (when file
        (with-open-file (wr file :direction :output
                            :if-exists :supersede)
          (format wr "~A~%" (op:object_to_string orb object))))
      (when name
        ;; Clorb utility function to access naming service
        (clorb:rebind object name))
      object)))

(defun run-hello (&rest args &key file name)
  (declare (ignore file name))
  (let ((orb (CORBA:ORB_init)))
    (when args (apply #'setup-hello args))
    (op:run orb)))
