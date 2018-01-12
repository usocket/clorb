
(defun load-2 (name)
  (let ((*source-extension* "lisp")
	(*compiled-extension* #+clisp "fas" #+gcl "o" #+cmu "x86f"))
    
    (flet ((string-append (&rest l)
			  (apply #'CONCATENATE 'string l))
	   
	   (newer-file-p (f1 f2)
			 (> (file-write-date f1) (file-write-date f2))))
      
      (let ((fs (string-append name "." *source-extension*))
	    (fc (string-append name "." *compiled-extension*)))
	
	(if (probe-file fs)
	    (if (probe-file fc)
		(if (newer-file-p fc fs)
		    (load fc)
	      (progn
		(princ (string-append "compiled file is out of date: " fc) *error-output*)
		(load fs)))
	      (progn
		(princ (string-append "compiled file does not exist: " fc) *error-output*)
		(load fs)))
	  (error (string-append "source file does not exist " fs)))))))


(compile-file "lisp-scanner/nfa-compiler.lisp")
(compile-file "lisp-scanner/scanner-support.lisp")

(compile-file "lalr-generator/lalr.lisp")


(load-2 "lisp-scanner/nfa-compiler")
(load-2 "lisp-scanner/scanner-generator")


(load-2 "idl-scanner-spec")



(with-open-stream (s (open "idl-scanner-parser.lisp" :direction :output))
		  (generate-to-stream  *token-list* nil s))

(load-2 "lalr-generator/lalr")

(load "idl-grammar.lisp")


(setf *lalr-debug* nil)  ; Inserts debugging code into parser if non-NIL

(let ((parser-procedure (rolands-make-parser *rules* *tokens*)))
  (with-open-stream (s (open "idl-scanner-parser.lisp" :direction :output :if-exists :append))
		    (pprint '(defun def-lambda (&rest a) a) s)
		    (pprint parser-procedure s)))



(compile-file "idl-scanner-parser.lisp")

(load-2 "idl-compiler")

