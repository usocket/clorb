(in-package :cl-user)

(defparameter *account-folder* 
    (or 
     #+mcl (pathname "phome:Library;Tcl;combat-tcl-0.7.2;demo;account;")
     #+unix "~/Library/Tcl/combat-tcl-0.7.2/demo/account/"))

(defparameter *account-ior*
  (clorb:pathname-url (truename (merge-pathnames "server.ior" *account-folder*))))

(CORBA:IDL (merge-pathnames "account.idl" *account-folder*))

(unless (boundp '*orb*)
  (defvar *orb* (CORBA:ORB_init)))

(defparameter *bank* (op:string_to_object *orb* *account-ior*))
(defparameter *acc*  (op:create *bank* "foo" "bar"))

(op:balance *acc*)
(handler-case 
  (op:withdraw *acc* 1000)
  (omg.org/root:account/bankrupt
   (c)
   (format t "Bankrupt withdawing ~D from ~D~%"
           (op:amount c)
           (op:balance c))))
