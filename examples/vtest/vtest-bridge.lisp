(in-package :cl-user)

(defclass vtest-bridge (CLORB_EX:vtest-servant)
  ((other :initarg :other :accessor other)))

(corba:define-method read ((self vtest-bridge) name)
  (let ((fox (op:read (other self) name)))
    (when fox
      (incf (op:value fox)))
    fox))

(corba:define-method write ((self vtest-bridge) box)
  (op:write (other self) box))

(corba:define-method repr ((self vtest-bridge) box)
  (op:repr (other self) box))

(corba:define-method write2 ((self vtest-bridge) box1 box2)
  (op:write2 (other self) box1 box2))

(corba:define-method longs ((self vtest-bridge) box1 box2)
  (op:longs (other self) box1 box2))



(setq *vtest-servant*
      (make-instance 'vtest-bridge
        :other (clorb:resolve "vtest")))


