(in-package :cl-user)

(defun hh (&key (name nil) (file "hello.ior"))
  (unless (and (find-package "HELLO")
               (find-symbol "WORLD" "HELLO"))
    (load (load-time-value
           (make-pathname :name "loadup" :type nil
                          :defaults #.*load-pathname*))))

  (funcall 'setup-hello :file file :name name)
  (funcall 'hello-client :file file :name name))

(defun hhn ()
  (hh :file nil :name "hello"))

