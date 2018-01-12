(in-package :cl-user)

(defun hello-client (&key file name count object)
  (let* ((orb (CORBA:ORB_init))
         (object (cond (object object)
                       (file
                        (op:string_to_object 
                         orb (with-open-file (rd file :direction :input)
                               (read-line rd))))
                       (name
                        (clorb:resolve name))
                       (t
                        (error "Supply :file or :name")))))
    (if count
        (dotimes (n count)
          (op:greet object))
      (op:greet object))))
