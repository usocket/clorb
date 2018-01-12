(in-package :clorb)

(defun %invoke (op obj args)
  (unless (object-opdef obj op)
    (setq op (concatenate 'string "_get_" op)))
  (apply 'invoke obj op args))


(defun make-all-proxies ()
  (let ()
    (do-external-symbols (op :op)
      ;; Avoid the methods that belong to the Object interface
      ;; they should be implemented locally
      (unless (member op '(op:_is_a op:_non_existent op:_get_interface
                           op:_hash op:_is_equivalent 
                           op:_create_request
                           op::_get_policy
                           op::_get_domain_managers
                           op::_set_policy_overrides))
        (let ((name (string-downcase (symbol-name op))))
          (eval `(defmethod ,op ((obj corba:proxy) &rest args)
                   (%invoke ,name obj args)))
          (eval `(defmethod (setf ,op) (val (obj corba:proxy))
                   (invoke obj ,(concatenate 'string "_set_" name) val))))))))

(make-all-proxies)
