;; idl-compiler-support.lisp
;; code from idl-complier used by the genereated code

(in-package "CLORB.IDLCOMP")

; these procedures are needed by the parser, or will be someday

(defun id->string (id)
  (let ((s (if (eq (car id) 'colon) (concatenate 'string "::" (cadr id))  (cadr id))))
    (dolist (x (cddr id))
      (setf s (concatenate 'string s "::" x)))
    s))


(defun string->double (str)
  (read-from-string (substitute #\d #\e str)))

(defun string->integer (str)
  (if (equalp str "0") (return-from string->integer 0))
  (read-from-string 
   (if (equalp (char str 0) #\0)
       (if (member (char str 1) '(#\x #\X))
	   (concatenate 'string "#x" (subseq str 2))
	 (concatenate 'string "#o" (subseq str 1)))
	 str)))



; simple 
(defun string->string (str)
  (list 'string 
        (subseq str 1 (1- (length str)))))

(defun string->char (str)
  (char str 1))


;
(defun idl-expand (l pos)
  (mapcar #'(lambda (x) 
              (let ((new (copy-list l))) 
                (setf (elt new pos) x) new)) 
          (elt l pos)))

(defun expand-type (type array-sizes)
  (if (null array-sizes)
    type
    `(array ,(expand-type type (cdr array-sizes))
            ,(car array-sizes))))

(defun expand-declarators (type declarators)
  ;; declarator is name or (name size ..)
  ;; return ( (name type) .. )
  (loop for d in declarators 
        collect (if (consp d)
                  (list (car d) (expand-type type (cdr d)))
                  (list d type))))


(defun idl-flatten (l)
  (let ((ans nil))
    (do ((l l (cdr l)))
      ((endp l) (reverse ans))
      (if (consp (caar l))
	  (setf ans (append (reverse (car l)) ans))
	(push (car l) ans)))))


(defvar *current-cpp* nil
  "Current preprocessor stream. This tracks prefix and other pragma.")

(defvar *LALR-DEBUG* nil)


(defun id-adjustment ()
  ;; List of forms to adjust IDs of IDL types due to pragma
  (loop for (type name value) in (idl-repositoryid-pragmas *current-cpp*)
        collect (ecase type
                  (:id `(set-id ,name ,value))
                  (:version `(set-version ,name ,value)))))
