;;;
;;; Load the hello world example
;;;

(in-package :cl-user)

(defvar *hello-defaults*
  (make-pathname :name nil :type nil :defaults *load-pathname*))


(load (merge-pathnames "idl" *hello-defaults*))
(load (merge-pathnames "hello-server" *hello-defaults*))
(load (merge-pathnames "hello-client" *hello-defaults*))
