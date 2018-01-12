;; idl-compiler.lisp
;; Modified by lenst

(in-package "CLORB.IDLCOMP")


;;  this is a specialized reader for the idl-compiler

(defun make-cpp-line-reader (cpp)
  (lambda ()
    (let ((line (read-cpp-line cpp)))
      (and line
           (concatenate 'string line (string #\return))))))


(defun parse-cpp-stream (cpp)
  (let ((*current-cpp* cpp)
        (base-tokenizer
         (make-scanner *table* *action* *start*  
                       (make-cpp-line-reader cpp))))
    (labels ((tokenizer ()
               (let ((res (funcall base-tokenizer)))
                 ;;(format t "Tokenizer: ~S~%" res)
                 (cond
                  ((not res) (cons nil nil))
                  ((not (cdr res)) (error "unkown token"))
                  (t res))))
	     (parser-error (shifts reduces)
               (pprint shifts)
               (pprint reduces)
               (let ((source (idl-source-position cpp)))
                 (error "Parse error in ~S line ~S"
                        (car source) (cadr source)))))
      (lalr-parser #'tokenizer #'parser-error))))



(defun parse-file (name &key include-directories)
  (using-cpp-stream name #'parse-cpp-stream
                    :include-directories include-directories))

(defun save-idef (name1 name2)
  (let ((b (parse-file name1)))
    (with-open-stream (a (open name2 :direction :output))
      (pprint '(in-package :clorb) a)
      (pprint (append '(idef-definitions) b) a))))




;;;; Connect to CLORB

(defclass idl-compiler-impl (idl-compiler)
  ())

(defun convert-package (list)
  (with-standard-io-syntax
    (let ((string (with-output-to-string (s)
                    (let ((*package* (find-package :clorb.idlcomp)))
                      (print list s)))))
      (let ((*package* (find-package :clorb)))
        (read-from-string string)))))

(defmethod load-repository ((self idl-compiler-impl) repository file)
  (idef-read (convert-package 
              (parse-file file
                          :include-directories (clorb::include-directories self)))
             repository))

(unless *default-idl-compiler*
  (setq *default-idl-compiler* (make-instance 'idl-compiler-impl)))
