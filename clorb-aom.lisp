;;; clorb-aom.lisp

(in-package :clorb)

;;; Class for Active Object Map
;;
;; The object map is a set of activations, pairs of object id and servant object.
;; activation = (oid . servant)
;; Only one activation per oid is allowed. 
;; Currently not checked.
;;


(defclass object-map ()
  ((oid-activation-trie
    :initform (make-trie)
    :accessor oid-activation-trie)
   (servant-activation-hash
    :initform (make-hash-table :test #'eq) 
    :accessor servant-activation-hash)))

(defmethod servant-active-p ((aom object-map) servant)
  (not (null (gethash servant (servant-activation-hash aom)))))

(defmethod servant-oid-list ((aom object-map) servant)
  (mapcar #'car (gethash servant (servant-activation-hash aom))))

(defmethod add-activation ((aom object-map) oid servant)
  (let ((activation (cons oid servant)))
    (trie-set oid (oid-activation-trie aom) activation)
    (push activation (gethash servant (servant-activation-hash aom)))
    activation))

(defmethod remove-activation ((aom object-map) oid)
  (let ((oid-trie (oid-activation-trie aom))
        (servant-map (servant-activation-hash aom)))
    (let ((activation (trie-get oid oid-trie)))
      (trie-remove oid oid-trie)
      (let ((activations (gethash (cdr activation) servant-map)))
        (setq activations (delete activation activations))
        (if activations
          (setf (gethash (cdr activation) servant-map) activations)
          (remhash (cdr activation) servant-map)))
      activation)))

(defmethod oid-servant ((aom object-map) oid)
  (cdr (trie-get oid (oid-activation-trie aom))))

(defmethod servant-oid ((aom object-map) servant)
  "Return nil, if no activation; oid, if excactly one activation;
 t if more than one activation"
  (let ((activations (gethash servant (servant-activation-hash aom))))
    (cond ((null activations) nil)
          ((null (cdr activations)) (caar activations))
          (t t))))


(defmethod map-activations ((aom object-map) func &optional mutable)
  (cond (mutable
         (let ((servant-hash (servant-activation-hash aom)))
           (loop for servant
              in (loop for servant being the hash-keys of servant-hash
                    collect servant)
              do (loop for activation
                    in (copy-list (gethash servant servant-hash))
                    when (member activation (gethash servant servant-hash))
                    do (funcall func (car activation) (cdr activation))))))
        (t
         (loop for activation-list
            being the hash-values of (servant-activation-hash aom)
            do (loop for (oid . servant) in activation-list
                  do (funcall func oid servant)))) ))


(defmethod activation-count ((aom object-map))
  (dict-count (oid-activation-trie aom)))
