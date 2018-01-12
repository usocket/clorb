;;;; nfa compiler compiles a nfa to a dfa
;;; -*- Mode: lisp -*-;;


#+gcl
(eval-when (load eval)
  (unless (find-package :nfa-compiler)
    (make-package :nfa-compiler)))


;      :use '(#+:cltl2 common-lisp
;             #-:cltl2 lisp))))

#+gcl(in-package  :nfa-compiler)

#+gcl(export '(nfa->dfa make-nfa subnfa-start subnfa-end subnfa-simple subnfa-empty subnfa-serial subnfa-append subnfa-or subnfa-+ subnfa-? subnfa-* ))


#-gcl
(defpackage :nfa-compiler
  (:use :common-lisp)
  (:export :nfa->dfa :make-nfa :subnfa-start :subnfa-end :subnfa-simple :subnfa-empty :subnfa-serial :subnfa-append :subnfa-or :subnfa-+ :subnfa-? :subnfa-*))

#-gcl(in-package :nfa-compiler)

;; for large input sets (unicode characters) another representation should be chosen
;;   an set is either: 
;;   universe - smallset 
;; or 
;;   smallset
;; but then an a smallset should be implemented as a list, not a bitvector 
;; when an inputset it is either small set or "everything but this small set"
;;  (+ (- universe smallset1) (- universe smallset2))   =    (- universe (+ smallset1 smallset2))
;;  (+ (- universe smallset1) smallset2)                =    (- universe (- smallset1 smallset2))
;;  .......
;;
;;
;;    usage : first a general automaton data structure is created and then states can be added, to these states transitions and epsilon transitions can be added.
;;            an input-token is represented as an integer
;;            a state is represented as an integer, the transitions are kept inside the automaton datastructure
;;
;;            the users has to do the mapping between the input set and  integers


;;  input sets are implemented as bit vectors

(defun list->input-set (l size)
  (let ((v (make-array size :element-type 'bit :initial-element 0)))
    (dolist (i l) (setf (sbit v i) 1))
    v))

(defun  input-set->list (s)
  (let ((l nil))
    (dotimes (i (length s))
      (when (= (sbit s i ) 1) (Push i l)))
    l))

(defun input-empty-p (v)
  (dotimes (i (length v))
    (if (= (sbit v i) 1) (return-from input-empty-p nil)))
  1)

(defmacro input-intersect (x y)
  `(bit-and ,x ,y))
   
(defmacro input-union (x y)
  `(bit-ior ,x ,y))

(defmacro input-minus (s1 s2)
  `(bit-andc2 ,s1 ,s2))

(defmacro input-equal-p (s1 s2)
  `(equalp ,s1 ,s2))



;;; operations for the state-set

;; state sets  are implemented as bit vectors and these as integers

(defmacro  list->state-set (l size)
  `(list->input-set ,l ,size))

(defmacro state-set->list (s)
  `(input-set->list ,s))

(defmacro state-intersect (x y)
  `(bit-and ,x ,y))
   
(defmacro state-union (x y)
  `(bit-ior ,x,y))

(defmacro state-minus (s1 s2)
  `(bit-andc2 ,s1 ,s2))

(defmacro state-set-equal-p (s1 s2)
  `(equalp ,s1 ,s2))


;;;

;; the Non Determinitic Automaton
;;  size is the number of states and states is a vector with the state structs, states is resized if need arises
(defstruct (nfa (:constructor internal-make-nfa)) states input-size)

;; i want to export exported-make-nfa as make-nfa

(defun make-nfa (input-size)
  (internal-make-nfa :states (make-array 0 :fill-pointer t :adjustable :t )
	    :input-size input-size))

(defun nfa-size (nfa) (length (nfa-states nfa)))

;; the integer denoting the state, the transitiopns , the p
(defstruct state-struct       ; the struct in which transitions and epsilon transitions of the state are stord 
			     count ; the integr denoting the state
			     trans ; the transitions
			     eps) ; the epsilon transitions

(defstruct nfa-transition 
			       input      ; the input set 
			       newstate) ; the new state

(defstruct dfa-transition input newstate) ; actually the same as above

;; add a state to the nfa data structure, initially there are no transitions or epsilon transitions
(defun add-state! (nfa)
  (let ((len (length  (nfa-states nfa))))
    (vector-push-extend (make-state-struct :count len :trans nil :eps nil) (nfa-states nfa))))

;; add an epsilontrtansition for the state state in the nfa datastructure nfa
(defun add-eps! (nfa state eps)
  (when (>= state (nfa-size nfa))
    (error "add-eps!: state does not exits in nfa" state))
  (when (>= eps (nfa-size nfa))
    (error "add-eps!: state fro eps  does not exits in nfa" eps))
  (let ((s (aref (nfa-states nfa) state)))
    (push eps (state-struct-eps s))))

;; add an transition for the state state in the nfa datastructure nfa
(defun add-transition! (nfa state input-list newstate)
  (when (>= state (nfa-size nfa))
    (error "add-transition!: state does not exits in nfa" state))
  (when (>= newstate (nfa-size nfa))
    (error "add-transition!: new state does not exits in nfa" newstate))
  (let ((s (aref (nfa-states nfa) state)))
    (push (make-nfa-transition :input (list->input-set input-list (nfa-input-size nfa)) :newstate newstate) (state-struct-trans s))))


					; compile the nfa into an deterministic automaton, returned  is a table with the states and in each state the table of transitions and
					; a the list of corresponding states of the nfa intersected with the interesting states, 
					;    interesting states = the states you are interested in, end states etc
					; 


(defun state-eps (nfa state)
  (state-struct-eps (aref (nfa-states nfa) state)))

(defun state-transitions (nfa state) 
  (state-struct-trans (aref (nfa-states nfa) state)))


;; compute the closure of a state
;; add states of its epsilon transitions and their epsilon transitions and .........
	 
(defun closure (nfa state l)
  (if (member state l) l
    (do ((l (cons state l) (closure nfa (car new) l))
	(new (state-eps nfa state) (cdr new)))
	((null new) l))))


(defun add-1-nfa-transition (nfa dfa-trs nfa-tr)
  (let* ((inset (nfa-transition-input nfa-tr))
	 (cx (list->state-set (closure nfa (nfa-transition-newstate nfa-tr) '()) (length (nfa-states nfa))))
	 (rem inset)     ; finally the input tokens that are not used in another transitions
	 (l '()))         ; the list of dfa trasnitions so far
				   
    (flet ((add-dfa-trans! (input newstates); add an dfa transition to the l, if an transitions with the same newstate-set already exists, union the inputsets
					; the inputsets are disjoint 
			   (unless (input-empty-p input)
			     (do ((li l (cdr li)))
				 ((null li)
				  (push (make-dfa-transition :input input :newstate newstates) l)) ; a transition with this newstateset has not been found, just add it
			     (when (state-set-equal-p input (dfa-transition-newstate (car li)))
			       (setf (car li) (make-dfa-transition :input (input-union input (dfa-transition-input (car li))) :newstate newstates))
					; transition with this newstate has been found,
			       (return-from add-dfa-trans! nil))))))  ; we are done
					; add inputset 
    
    
    (dolist (tr dfa-trs)  ; for the dfas holds the following: the inputsets are disjunct and no two newstate-sets are equal
	     (let* ((tr-inset (dfa-transition-input tr))
		    (tr-ns (dfa-transition-newstate tr)))
	       (setf rem (input-minus rem tr-inset))
	       (add-dfa-trans! (input-minus tr-inset inset) tr-ns)
	       (add-dfa-trans! (input-intersect tr-inset inset) (state-union tr-ns cx))))
    (add-dfa-trans! rem cx)  ; finally add the remaining the input tokens
    l)))


(defun add-nfa-transitions (nfa s l)
  (do ((l1 l (cdr l1)))
      ((null l1) s)
    (setf s (add-1-nfa-transition nfa s (car l1))))
  s)

	   
					; to a list of dfa transitions add another nfa transition
					;  i.e. just add a dfa transition and make sure th	   
					; returns the main table and the starting states
(defun compute-dfa-states (nfa starts interesting-states)
  (declare (optimize (speed 3) (space 0) (safety 1)))
  (let  ((interesting-states-set (list->state-set interesting-states (length (nfa-states nfa))))
	 (todo '());(list (cons (list->state-set (closure start)) 0))) ; the list of statesets which still have to be computed
	 (done '())                                               ; the list of states which already have been computed
	 (count -1))
    
					;(start-states (map (lambda (start) (state-set->number (list->state-set (closure start)))) starts))) 
    
					; a stateset is a state in the dfa, these dfa states are labeled by numbers
					; if the  stateset is found than this number is returned otherwise a new the state has  not been computed yet, so add it and label it 
    (flet
	((state-set->number (s)      
			    (let ((a (assoc s todo :test 'equalp)))            ;DANGER !!!, the implemtation of stateset has to assoc'able i.e. comparable via equal !!!       
			      (if a (cdr a)        
				(let ((b (assoc s done :test 'equalp)))     ; DANGER !!!
				  (if b (cdr b)
				    (progn
				      (setf count (1+ count))
				      (push  (cons s count) todo)
				      count)))))))
	 
      
      (let ((start-states (mapcar #'(lambda (start) (state-set->number (list->state-set (closure nfa start '()) (length (nfa-states nfa))))) starts))
			  (states '()))
	    (do ()
	    ((null todo) (values states start-states))                          ; everything has been done
	  (let ((s (caar todo)))        ; the next state 
	    (push (pop todo) done)
	    (let ((trs '()))
	      (dolist (x (state-set->list s))
		(setf trs (add-nfa-transitions nfa trs (state-transitions nfa x)))) 
	      (dolist (tr trs)
		(let ((s (state-set->number (dfa-transition-newstate tr))))
		  (setf (dfa-transition-newstate tr) s)))
	      (push (vector (cdar done) ; the number of the state (this is redundant information)
			    (state-set->list (state-intersect s interesting-states-set)) ; the interesting states in this dfa state 
			    (mapcar #'(lambda (x) (vector (input-set->list (dfa-transition-input x)) (dfa-transition-newstate x))) trs) ; the transitions
			    )
		    states))))))))


(defun nfa->dfa (nfa starts interesting-states)
  (multiple-value-bind (a s) (compute-dfa-states nfa starts interesting-states)
		      (let ((v (make-array (length a))))
			(do ((l a (cdr l)))
			    ((null l)  (values v s))   ;; 0 is the start state
			  (setf (aref v (aref (car l) 0)) (car l))))))


;; these procedures and the type are for convenience, to ease the building of nfas for regular expressions
;; make subnfa just a pair ?
;; a subnfa is only meaningfull in connection with an nfa data structure, it just stores the start- and end state of an automaton
;; with the following procedures the states are changed when combing subnfas

(defstruct subnfa start end)

;; accept the inputset l
(defun subnfa-simple (nfa l)
  (let* ((s1 (add-state! nfa))
	 (s2 (add-state! nfa)))
    (add-transition! nfa s1 l s2)
    (make-subnfa :start s1 :end s2)))

; accpeted ""
(defun subnfa-empty (nfa)
  (let ((s (add-state! nfa)))
    (make-subnfa :start s :end s)))

;  the elements of l are input sets or input tokens, 
(defun subnfa-serial (nfa l)
  (let* ((start (add-state! nfa))
	 (end start)
	 (s start))
    
    (do () 
	((null l) (make-subnfa :start start :end end))
      (setf end (add-state! nfa)) 
      (add-transition! nfa s (if (consp (car l)) (car l) (list (car l))) end)
      (pop l)
      (setf s end))))

;
(defun subnfa-append-2 (nfa a1 a2)
  (add-eps! nfa (subnfa-end a1) (subnfa-start a2))
  (make-subnfa :start (subnfa-start a1) :end (subnfa-end a2)))

(defun subnfa-append (nfa l)
  (cond
   ((null l) (subnfa-empty nfa))
   ((null (cdr l)) (car l))
   (t (subnfa-append-2 nfa (car l) (subnfa-append nfa (cdr l))))))

(defun subnfa-or (nfa l)
  (let ((sa (add-state! nfa))
	(se (add-state! nfa)))
    (dolist (a l) 
      (add-eps! nfa sa (subnfa-start a))
      (add-eps! nfa (subnfa-end a) se))
    (make-subnfa :start sa :end se)))

; match a at least once
(defun  subnfa-+ (nfa a)
  (add-eps! nfa (subnfa-end a) (subnfa-start a))
  a)

; match a 0 or 1 time
(defun subnfa-? (nfa a)
  (add-eps! nfa (subnfa-start a) (subnfa-end a))
  a)

;;; match a 0 or 1 or 2 or .... times
(defun subnfa-* (nfa a)
  (add-eps! nfa (subnfa-end a) (subnfa-start a))
  (add-eps! nfa (subnfa-start a) (subnfa-end a))
  a)




