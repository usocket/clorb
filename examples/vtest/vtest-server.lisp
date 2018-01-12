(in-package :cl-user)

(defclass vtest-impl (clorb_ex:vtest-servant)
  ())


(corba:define-method read ((self vtest-impl) name)
  (cond ((equal name "") nil)
        (t (make-instance 'clorb_ex:fox :name name :value (length name)))))

(corba:define-method write ((self vtest-impl) box)
  (clorb::mess 5 "~A" box))

(corba:define-method repr ((self vtest-impl) box)
  (princ-to-string box))

(corba:define-method write2 ((self vtest-impl) box1 box2)
  (cond ((eql box1 box2) "They are the same")
        (t (format nil "~A and ~A" box1 box2))))

(corba:define-method longs ((self vtest-impl) box1 box2)
  (cond ((eql box1 box2) "They are the same")
        ((or (null box1) (null box2)) "One or both is null")
        (t (format nil "~A and ~A" box1 box2))))

(corba:define-method is_same ((self vtest-impl) v1 v2)
  (eql v1 v2))


(defmethod print-object ((fox clorb_ex:fox) stream)
  (print-unreadable-object (fox stream :type t :identity t)
    (format stream "~A #~A"
            (if (slot-boundp fox 'op:name) (op:name fox))
            (if (slot-boundp fox 'op:value) (op:value fox)))))


(defvar *vtest-servant* nil)

(defun setup-vtest (&key file name)
  (unless *vtest-servant*
    (setq *vtest-servant* 
          (make-instance 'vtest-impl)))
  (let ((orb (CORBA:ORB_init)))
    ;; Register the object servant with the ORB
    (let ((object 
           (let ((poa (op:resolve_initial_references orb "RootPOA")))
             ;; Optionally activate object (RootPOA has implicit activation)
             ;;(op:activate_object poa *vtest-servant*)
             ;; Activate the POAManager to allow the POA to accept requests
             (op:activate (op:the_poamanager poa))
             ;; Implicit activation
             (op:_this *vtest-servant*))))
      ;; Store object reference in file or in naming service
      (when file
        (with-open-file (wr file :direction :output
                            :if-exists :supersede)
          (format wr "~A~%" (op:object_to_string orb object))))
      (when name
        ;; Clorb utility function to access naming service
        (clorb:rebind object name))
      object)))

(defun run-vtest (&rest args)
  (let ((orb (CORBA:ORB_init)))
    (when args (apply #'setup-vtest args))
    (op:run orb)))
