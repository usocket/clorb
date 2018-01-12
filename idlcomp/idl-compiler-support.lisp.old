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
  (subseq str 1 (1- (length str))))

(defun string->char (str)
  (char str 1))

;
(defun idl-expand (l pos)
  (mapcar #'(lambda (x) (let ((new (copy-list l))) (setf (elt new pos) x) new)) (elt l pos)))


(defun idl-flatten (l)
  (let ((ans nil))
    (do ((l l (cdr l)))
      ((endp l) (reverse ans))
      (if (consp (caar l))
	  (setf ans (append (reverse (car l)) ans))
	(push (car l) ans)))))

; the current prefix, this is a list because we have to deal with included files (not done yet)

(defvar *current-idl-prefix*)
(setf  *current-idl-prefix* (list nil))

(defvar *LALR-DEBUG* nil)

