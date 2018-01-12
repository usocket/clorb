;;;
;;; Load the hello world example
;;;

(in-package :cl-user)

(defparameter *vtest-base*
  (make-pathname :name nil :type nil
                 :defaults (or #.*compile-file-pathname*
                               *load-pathname*)))


(CORBA:IDL (merge-pathnames "vtest.idl" *vtest-base*))
(load (merge-pathnames "vtest-server" *vtest-base*))
