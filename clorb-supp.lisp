;;;; clorb-supp.lisp

(in-package :clorb)


;; For lack of better place to put it:
;; Special variable: *THE-ORB*
;;  holds a reference to the singelton orb object when the orb has been initialized.

(defvar *the-orb* nil)



;;; Logging

(defvar *log-output* t)

(defun mess (level fmt &rest args)
  (when (>= level *log-level*)
    (apply #'cl:format *log-output*
           (format nil "~~&~A ~A~~%" 
                   (make-string level :initial-element #\;)
                   fmt)
           args)
    #-clisp
    (finish-output *log-output*)))



(defun stroid (stream oid colon-p at-p)
  (declare (ignore colon-p at-p))
  (map nil
    (lambda (octet)
      (if (< 31 octet 127)
          (princ (code-char octet) stream)
        (format stream "<~x>" octet)))
    oid))




;;;; Helper functions


(defun kwote (x)
  "Return an expression that quotes X."
  (list 'quote x))


(defun mklist (x)
  "Return the list for list designator X."
  (if (consp x) x (list x)))


(defun repeated (item)
  "Return a cyclic list with element item."
  (let ((x (list item)))
    (setf (cdr x) x)
    x))


(defun feature (name)
  "Return feature symbol for IDL name.
The name is upcased and interned in the features package (nick OP)."
  (intern (string-upcase name) :op))

(defun key (string)
  "Return keyword symbol with name string."
  (check-type string string)
  (intern (string-upcase string) :keyword))


(defun prefixed-name (prefix-string name-symbol)
  "Return symbol with same package as name-symbol and a name that is the
name of name-symbol prefixed with prefix-string."
  (intern (concatenate 'string prefix-string (symbol-name name-symbol))
          (symbol-package name-symbol)))


(defun tc-constant-name (symbol)
  "Return the name of the constant for the TypeCode of a type symbol."
  ;; FIXME: Should prehaps only be used by the code generator
  (prefixed-name "_TC_" symbol))



(defun ensure-corba-package (name &key nicknames export)
  (let ((package (find-package name)))
    (unless package
      (setq package (make-package name :nicknames nicknames :use '())))
    (export (mapcar (lambda (sym-name) (intern sym-name package)) export)
            package)))



;;; clorb-supp.lisp ends here
