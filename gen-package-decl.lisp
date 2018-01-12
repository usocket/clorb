;;; gen-package-decl.lisp

(packer:require-package "NET.CDDR.UTILS")


#|
(utils:gen-package-decl "CORBA")
(utils:gen-package-decl "OP")
(utils:gen-package-decl "IOP")
(utils:gen-package-decl "GIOP")
(utils:gen-package-decl "IIOP")
(utils:gen-package-decl "OMG.ORG/PORTABLESERVER")
(utils:gen-package-decl "PORTABLEINTERCEPTOR")
(utils:gen-package-decl "IOP_N")
(utils:gen-package-decl "DYNAMIC")
|#


#+(or)
(defun gen-package-decl (&optional (package :op))
  (let ((exports nil))
    (do-external-symbols (op package)
      (push op exports))
    (format t "(defpackage ~S~%  (:use)~%" (package-name package))
    (let ((nicks (package-nicknames package)))
      (when nicks
        (format t "  (:nicknames~{ ~S~})~%" nicks)))
    (setq exports (sort exports #'string<))
    (setq exports (mapcar #'symbol-name exports))
    (let ((*package* (find-package package)))
      (pprint-logical-block (*standard-output* exports
                                               :prefix "  ("
                                               :suffix ")")
        (princ ":export")
        (pprint-newline :mandatory *standard-output*)
        (pprint-fill *standard-output* exports nil)))
    (format t ")~%")
    (values)))
