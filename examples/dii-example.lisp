(in-package :cl-user)

;;; Calling a method with the following signature:
;;; long foobar(in long x, out long y);

(defun dii-call (obj)
  (multiple-value-bind (result req)
      (op:_create_request obj nil "foobar" nil nil 0)
    (declare (ignore result))

    (op:set_return_type req CORBA:tc_long)
    
    (let ((x (op:add_in_arg req)))
      (setf (corba:any-value x) 10)
      (setf (corba:any-typecode x) CORBA:tc_long))
        
    (let ((y (op:add_out_arg req)))
      (setf (corba:any-typecode y) CORBA:tc_long)
        
      (op:invoke req)

      (values (corba:any-value (op:return_value req))
              (corba:any-value y)))))



(defun do-dii-call ()
  (let ((obj (clorb:resolve "dsi")))
    (dii-call obj)))
