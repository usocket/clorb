(in-package :cl-user)

(defparameter *calc-ior* 
  )

(defparameter *calc-idl*
  (merge-pathnames "calculator.idl" *calc-folder*))

(defvar *calc*)

(defun do-calc ()
  (setq *calc* (op:string_to_object *orb* *calc-ior*))
  (op:add *calc* 12.5 5.8)
  (op:div *calc* 9.9 3))
