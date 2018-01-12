(in-package "CLORB.IDLCOMP")

#|

#-gcl(defpackage :scanner-support
       (:use :common-lisp)
       (:export :make-scanner :make-char-reader :make-line-reader :make-string-reader :line-reader))

#-gcl(in-package :scanner-support)

#+gcl (eval-when (load eval)
	(unless (find-package :scanner-support)
	  (make-package :scanner-support)))

#+gcl(in-package  :scanner-support)

#+gcl(export '(make-scanner make-char-reader make-line-reader make-string-reader line-reader))

|#

  

(declaim (inline dfa-step))

(defun dfa-step (table state input)
  (let ((ch (char-code input)))
    (dolist (trans (aref (svref table state) 1))
      (when (= 1 (sbit  (svref trans 0) ch))
	(let ((newstate (svref trans 1)))
	  (return-from dfa-step (values newstate (svref (svref table newstate) 0))))))
    (values nil nil)))



; a buffer is a adjustable charracter array with a fillpointer

(declaim (inline get-char take-from-buffer append-to-buffer))

; read the next character from buffer, position must not be larger than the fill-pointer
; if theere no more chars in buffer the result of the call to next-proc is appended to the buffer
(defun get-char (buffer pos next-proc)
  (cond
   ((= pos (fill-pointer buffer)) ; are there chars in the buffer left?
    (do ((c (funcall next-proc) (funcall next-proc)))  ;; string length might be 0, so we have to loop, or should "" raise an error?
	((or (not c) (characterp c) (and (stringp c) (not (zerop (length c)))))
	 (and c
	      (progn
		(append-to-buffer buffer c)
		  (aref buffer pos))))))
   ((< pos (fill-pointer buffer))
    (aref buffer pos))
   (t (error "position to large when getting char from a buffer"))))

; take the first pos characters from buffer and return them
; change the buffer!
(defun take-from-buffer (buffer pos)
  (prog1
      (coerce (subseq buffer 0 pos) 'string)
    (replace buffer buffer :start2 pos :end2 (fill-pointer buffer))
    (setf (fill-pointer buffer) (- (fill-pointer buffer) pos))))


; appends a string or a character to an adjustable array with fill-pointer
(defun append-to-buffer (buffer c)
  (if (characterp c)
      (vector-push-extend c buffer)
    (let ((fp (fill-pointer buffer)))
	  (when (> (+ fp (length c)) (car (array-dimensions buffer)))
	    (adjust-array buffer (+ fp (length c))))
	  (setf (fill-pointer buffer) (+ fp (length c)))
	  (replace buffer c :start1 fp))))

; bla

(defun make-scanner (table action-table start-state nexter)
  (let ((buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer t)))
	;(position 0))
  
    (labels
	((next-token  ; the main procedure crank the aotomaton
	  ()
	  (let ((last-token nil)
		(last-token-pos nil)
		(i 0)
		(state start-state)
		(token nil)
		(c))
	    (loop
	      (setf c (get-char buffer i nexter))
	      ;(pprint c)
	      (if c
		  (progn
		    (incf i)
		    (multiple-value-setq (state token) (dfa-step table state c))
		    ;(pprint (list '++++ state token))
		    (if state   ; continue
			(when token
			  (setf last-token token)
			  (setf last-token-pos i))
		      (return-from next-token
			(if last-token
			    (values last-token (take-from-buffer buffer last-token-pos))
			  (values nil (take-from-buffer buffer 1)))))) ; return as string the first read char
					;EOF
		(return-from next-token
		  (if last-token
		      (values last-token (take-from-buffer buffer last-token-pos))
		    (if (= i 0) (values nil nil)
		      (values nil (take-from-buffer buffer 1)))))))))
	  

	 (compute-token (token str) ; given a token, lookup it up in action-table and perform the action
			(let ((x (svref action-table token)))
			  (if x 
			      (if (symbolp x)
				  (cons x str)
				(funcall  x str))
			    nil))))
      ;(declare (inline next-token compute-token ))
      #'(lambda ()
	(declare (inline next-token compute-token ))
	(block tokenizer
	  (loop
	    (multiple-value-bind
		(token str) (next-token)
					;(pprint (list '***** token  str))
	      (unless (or token str) (return-from tokenizer nil)) ; EOF --> return nil
	      (unless token (return-from tokenizer (cons nil str))) ; no token found
	      (let ((x (compute-token token str)))  ; compute the token action
		(if x (return-from  tokenizer x))
		)))))  ; if not x then loop again
      )))




; convenience functions
(defun make-char-reader (stream)
  (lambda () (read stream nil nil nil)))

#|
(defun make-line-reader (stream)
  #'(lambda () (let ((l (read-line stream nil nil nil)))
	       (and l (concatenate 'string l (string #\newline))))))
|#

(defmacro line-reader (stream &optional line-counter)
  (let ((temp (gensym))
	(stream-temp (gensym)))
    `(let ((,stream-temp ,stream))
       (lambda () (let ((,temp (read-line ,stream-temp nil nil nil)))
		    (and ,temp ,(if line-counter
				    `(progn (incf ,line-counter) (concatenate 'string ,temp (string #\newline)))
				  `(concatenate 'string ,temp (string #\newline)))))))))

(defun make-string-reader (str)
  #'(lambda ()
      (prog1 str (setf str nil))))

  
(export '(make-scanner make-char-reader make-line-reader make-string-reader line-reader))




