(setf (logical-pathname-translations "clorb")
  '(("src;**;*.*"  "~/src/clorb/**/*.*")
    ("**;*.*"      "~/src/clorb/**/*.*" )))

(load "clorb:src;clorb-files")
(pushnew :dummy-tcp *features*)

(net.cddr.clorb.system::reload)

(defvar *orb* (CORBA:ORB_init))

(format t "~&;;; Activating the POA~%")
(op:activate (op:the_poamanager (clorb::root-poa)))
(format t "~&;;; ORB listning on port ~A~%" (clorb::orb-port clorb::*the-orb*))


