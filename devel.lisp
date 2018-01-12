(in-package :cl-user)

(format t ";;; Loading CLORB~%")


;;; MCL 5.0 - needs the BSD package

#+digitool
(unless (find-package "BSD")
  (cond ((find-package "NET.CDDR.PACKER")
         (funcall (intern "REQUIRE-PACKAGE" "NET.CDDR.PACKER") "BSD"))
        (t
         (require "BSD"))))


;;; Load the CLORB system

(load (merge-pathnames "clorb-files.lisp" *load-pathname*))
(net.cddr.clorb.system:set-load-opts
 :server t  :idlcomp nil  :my-idlparser t
 :portable-interceptor t  :support-test t
 :hello-world t)
(net.cddr.clorb.system:reload)


#+digitool
(import '(@ @@ @@@) "CLORB")

;; To easy switch between CLORB and CL-USER from the listener
(define-symbol-macro inpc (in-package :clorb))
(define-symbol-macro clorb::inpu (in-package :cl-user))


;;; SBCL repl support, runs the server parts using add-fd-handler
#+sbcl
(clorb::make-repl-happy)


;;; IO Policy

(setq clorb:*io-system-default-class*
      (or #+(or mcl openmcl sb-thread) 'clorb:io-system-mt-blocking-write
          'clorb:io-system-select-blocking-write))

;;; Initiating the ORB
;; provide initial references to services on the system

(defvar *the-orb*
  (CORBA:ORB_init
   (list #+digitool "-ORBPort 4711"
         ;;#+cmu      "-ORBPort 4713"
         ;;"-ORBInitRef NameService=corbaloc::/NameService" 
         "-ORBInitRef NameService=corbaloc:rir:/CLORB-PNS" )))


;;; Initiating the object adapter (the server part of the ORB)

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))


;;; Examples
;;
;;(load (merge-pathnames
;;       (make-pathname :name "auto" :type "lisp"
;;                      :directory '(:relative "examples" "hello"))
;;       clorb::*clorb-pathname-defaults*))
;;
;; run hello world with (hh)
;; or (hhn) using the name service


;;; Run test cases
#-clorb-no-test
(load (merge-pathnames "all-test.lisp" clorb::*clorb-pathname-defaults*))
