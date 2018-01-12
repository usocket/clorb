(in-package :cl-user)


(defclass hello-world-bridge (CLORB_EX:helloworld-servant)
  ((other :initarg :other :accessor other)))


(corba:define-method greet ((self hello-world-bridge))
  (format nil "Bridge: '~A'" (op:greet (other self))))


(setq *hello-servant*
      (make-instance 'hello-world-bridge
        :other (clorb:resolve "hello")))


