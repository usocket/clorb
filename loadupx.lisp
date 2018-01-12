;;;; loadup.lisp --- Load the CLORB system

(load (merge-pathnames "clorb-files.lisp" *load-truename*))
(net.cddr.clorb.system:reload)

(provide :clorb)
