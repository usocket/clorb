;; loadup for mcl

(in-package :cl-user)

(defvar *base-dir*
  (make-pathname :name nil :type nil
                 :defaults
                 (or *load-pathname*
                     #+ccl (pathname (ccl:front-window)))))

(setf (logical-pathname-translations "idlcomp")
      `(("**;*.*"      ,(merge-pathnames ":**:*.*" *base-dir*))
        ("*.*"         ,(merge-pathnames "*.*" *base-dir*))))

(defparameter *generator-files*
  '("idlcomp;lisp-scanner;nfa-compiler"
    "idlcomp;lisp-scanner;scanner-generator"
    "idlcomp;idl-scanner-spec"
    "idlcomp;lalr-generator;lalr" ))

(clorb::compile-changed *generator-files*)

(setf *lalr-debug* nil)  ; Inserts debugging code into parser if non-NIL

(defun add-ignorable-declaration (rules)
  (loop for (ident non-terminal . body) in rules
        collect (list* ident non-terminal
                       (loop for (rhs function) in body
                             unless function 
                             do (warn "Missformed rule: ~A" (list ident non-terminal ))
                             collect (list rhs
                                           (if (and (consp function)
                                                    (eq 'lambda (car function)))
                                             (list* (first function) (second function)
                                                    `(declare (ignorable ,@(second function)))
                                                    (cddr function))
                                             function))))))

(load "idlcomp:idl-grammar")
(with-open-stream (s (open "idlcomp:idl-scanner-parser.lisp" 
                           :direction :output
                           :if-exists :supersede))
  (print '(in-package "CLORB.IDLCOMP") s)
  (generate-to-stream  *token-list* nil s)
  (pprint '(defun def-lambda (&rest a) a) s)
  (pprint (rolands-make-parser (add-ignorable-declaration *rules*) *tokens*) s))
