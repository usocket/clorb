;;;
;;; Load the hello world example
;;;

(in-package :cl-user)

(defun hh (&key (name nil) (file nil))
  (let ((object (setup-hello :file file :name name)))
    (if (or name file)
      (hello-client :file file :name name)
      (hello-client :object object))))

(defun hhn ()
  (hh :file nil :name "hello"))

