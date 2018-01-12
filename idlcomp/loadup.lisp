;; loadup for mcl

(let ((*default-pathname-defaults*
       (make-pathname :name nil :type nil
                      :defaults (or *load-pathname*
                                    #+ccl (pathname (ccl:front-window))))))
  (load "pkgdcl")
  (load ":lisp-scanner:scanner-support")
  (load "idl-compiler")
  (load "idl-scanner-parser") )
