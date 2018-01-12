(in-package :cl-user)

;;(corba:idl "/Users/lenst/build/JacORB_2_2_1/demo/value/idl/server.idl")
;;(corba:idl "phome:build;JacORB_2_2_1;demo;value;idl;server.idl")

(defclass ValueServer-impl (demo/value/idl:valueserver-servant)
  ())

;; string receive_long   (in boxedLong p1, in boxedLong p2);
(corba:define-method op:receive_long
  ((obj ValueServer-impl) p1 p2)
  (cond ((not (and p1 p2)) "One or both is null")
        ((eql p1 p2) "Identical")
        (t (format nil "Values ~D and ~D"
                   (op:data p1) (op:data p2)))))



;; string receive_string (in boxedString s1, in boxedString s2);
(corba:define-method op:receive_string
  ((obj ValueServer-impl) p1 p2)
  (cond ((not (and p1 p2)) "One or both is null")
        ((eql p1 p2) "Identical")
        (t (format nil "Values ~S and ~S" p1 p2))))


;; string receive_list   (in Node node);
(corba:define-method op:receive_list
  ((obj ValueServer-impl) node)
  (let ((n 0))
    (loop repeat 99 while node do (setq node (op:next node)) (incf n))
    (format nil "Length ~D" n)))


(defvar *demo-value-servant*
  (make-instance 'ValueServer-impl))

(with-open-file (out "phome:Sites;V_ref" :direction :output
                 :if-exists :supersede)
  (princ (op:object_to_string *the-orb* (op:_this *demo-value-servant*))
         out))


(defun value-client (&optional (file "phome:Sites;V_ref"))
  (let ((obj (with-open-file (in file)
               (op:string_to_object *the-orb* (read-line in)))))
    (format t "Two integers: ~A~%"
            (op:receive_long obj (demo/value/idl:boxedlong 123) (demo/value/idl:boxedlong 123)))
    (format t "One integer twice: ~A~%"
            (let ((b (demo/value/idl:boxedlong 123)))
              (op:receive_long obj b b)))
    (format t "Two strings: ~A~%"
            (op:receive_string obj "hello" "world"))
    (let ((list nil))
      (dolist (x '(3 2 1))
        (setf list (make-instance 'demo/value/idl:node :id x :next list)))
      (format t "A list: ~A~%"
              (op:receive_list obj list)))))
  


;; (op:run *the-orb*)
