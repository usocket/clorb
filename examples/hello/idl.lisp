;;;
;;; Load the hello world example
;;;

(in-package :cl-user)

(CORBA:IDL (merge-pathnames "hello.idl"
                            (or #.*compile-file-pathname*
                                *load-pathname*)))
