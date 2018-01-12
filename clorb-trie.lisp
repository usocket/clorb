(in-package :clorb)

;; trie will be a simple sexp
;;        trie = (val . a-list)
;;  where val = is value if key is empty
;;        a-list is an assoc list to lookup new tries with first
;;          part of key.
;;  Possible future extension is to replace a-list when it
;;  becomes too long. If whe know that the key parts are small integers
;;  we could use a vector.

;;
;; Space per key: 3n cons cells   
;;   where n = subkeys in key after any shared prefix has been removed
;; 

(defun make-trie ()
  (cons nil nil))

(defun trie-get (key trie &optional default)
  (declare (type sequence key)
           (type (or null cons) trie))
  (map nil
    (lambda (subkey)
      (when trie
        (setq trie (cdr (assoc subkey (cdr trie))))))
    key)
  (if trie 
      (car trie)
    default))

(defun trie-set (key trie value)
  (map nil
    (lambda (subkey)
      (assert trie)
      (let ((subtrie (cdr (assoc subkey (cdr trie)))))
        (unless subtrie
          (setq subtrie (cons nil nil))
          (push (cons subkey subtrie) (cdr trie)))
        (setq trie subtrie)))
    key)  
  (setf (car trie) value))

(defun trie-remove (key trie &optional (keypos 0))
  ;; Recursive solution
  (cond ((= keypos (length key))
         (setf (car trie) nil))
        (t 
         (let* ((pair (assoc (elt key keypos) (cdr trie)) )
                (subtrie (cdr pair)))
           (when subtrie
             (trie-remove key subtrie (1+ keypos))
             (unless (or (car subtrie) (cdr subtrie))
               (setf (cdr trie) (delete pair (cdr trie)))))))))

(defun maptrie (function trie)
  (declare (type (function (sequence t)) function))
  (labels ((recurse (key-so-far trie)
             (when (car trie)
               (funcall function (reverse key-so-far) (car trie)))
             (loop for pair in (cdr trie)
                 do (recurse (cons (car pair) key-so-far) (cdr pair)))))
    (recurse nil trie)))

(defun maptrie2 (function trie)
  (declare (type (function (sequence t)) function))
  (let ((key (make-array 20 :adjustable t :fill-pointer 0)))
    (labels ((rec (trie)
               (when (car trie)
                 (funcall function key (car trie)))
               (vector-push-extend 0 key)
               (loop for pair in (cdr trie) do
                     (setf (aref key (- (fill-pointer key) 1)) 
                       (car pair))
                     (rec (cdr pair)))
               (decf (fill-pointer key))))
      (rec trie))))

(defmethod dict-count ((trie cons))
  (let ((c 0))
    (maptrie (lambda (k v) (declare (ignore k v)) (incf c))
             trie)
    c))

#||
(defparameter a-trie 
    '( nil
      (1  nil 
       (0 nil
        (0 nil
         (0 hello))
        ))))
(trie-get '#(1 0 0 0) a-trie)           ; => HELLO
(trie-set '#(2 0 0 0) a-trie 'new-val)
(trie-get '#(2 0 0 0) a-trie)           ; => NEW-VAL
(maptrie2 (lambda (key val)
           (format t "MAPTRIE: ~S => ~S~%" key val))
         a-trie)
(trie-remove '#(1 0 0 0) a-trie)
(trie-get '#(1 0 0 0) a-trie)           ; => NIL

||#
