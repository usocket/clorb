(in-package :clorb)

(let ((*test-suite-result* (make-instance 'test-result
                             :parent nil
                             :suite "All Tests"))) 
  (loop for pn in (directory (merge-pathnames "test-*.lisp" *load-pathname*))
        unless (string= "test-suite" (pathname-name pn))
        do (format t "~&;;;;<< ~A~%" (namestring pn))
        (load pn))
  (format t "~&;;; ==================================================~%" )
  (print-result *test-suite-result*))
