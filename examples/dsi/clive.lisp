
(in-package :cl-user)

(defclass CLIVE-CLASS (portableserver:dynamicimplementation)
  ())

(corba:define-method primary_interface ((x clive-class) oid poa)
  (declare (ignore oid poa))
  "IDL:some.example/MyClass:1.0")

(defmethod foobar ((self clive-class) x)
  (let ((result 123)
        (y (+ 99 x)))
    (values result y)))


(corba:define-method invoke ((self clive-class) r)
  (cond
   ((equal (op:operation r) "foobar")
    (let* (_x 
           _y
           (args
            (list (corba:NamedValue
                   :argument 
                   (setq _x 
                     (corba:any :any-typecode corba:tc_long))
                   :arg_modes corba:ARG_IN)
                  (corba:NamedValue 
                   :argument 
                   (setq _y
                     (corba:any :any-typecode corba:tc_long))
                   :arg_modes corba:ARG_OUT))))
      (setq args (op:arguments r args))
      ;; op:arguments destructively modifies the any's in the named
      ;; value list, so we kept hold of them above to avoid the need to
      ;; walk through the named values
      (multiple-value-bind (res y)
          (foobar self (corba:any-value _x))
        (setf (corba:any-value _y) y)
        (op:set_result r (corba:any :any-typecode corba:tc_long
                                    :any-value res)))))
   (t
    (op:set_exception r (make-condition 'CORBA:BAD_OPERATION
                          :completed :completed_no)))))
