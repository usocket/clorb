(in-package :clorb)

(idef-definitions
  (define-module "poa" ()
    (define-module "rootPOA" ()
      (define-exception "DivideByZero" ())
      (define-interface "ICalculator" ()
        (define-operation "add" ((:param_in "nb1" float)
                                 (:param_in "nb2" float))
          :result-type float)
        (define-operation "div" ((:param_in "nb1" float)
                                 (:param_in "nb2" float))
          :result-type float)))))
    
