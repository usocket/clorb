(in-package :cl-user)

(clorb::idef-code
  (clorb::define-module "poa" ()
    (clorb::define-module "rootPOA" ()
      (clorb::define-exception "DivideByZero" ())
      (clorb::define-interface "ICalculator" ()
        (clorb::define-operation "add" ((:param_in "nb1" float)
                                        (:param_in "nb2" float))
          :result-type float)
        (clorb::define-operation "div" ((:param_in "nb1" float)
                                        (:param_in "nb2" float))
          :result-type float)))))

(defclass CALC-IMPL (poa/rootpoa:icalculator-servant)
  ())

(corba:define-method add ((servant calc-impl) nb1 nb2)
  (+ nb1 nb2))

(corba:define-method div ((servant calc-impl) nb1 nb2)
  (when (zerop nb2)
    (error 'DivideByZero))
  (/ nb1 nb2))


(let ((poa (op:resolve_initial_references *orb* "RootPOA")))
  (let ((servant (make-instance 'calc-impl)))
    (op:activate_object poa servant)
    (op:object_to_string *orb* (op:servant_to_reference poa servant))))
