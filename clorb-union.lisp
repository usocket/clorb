;;;; clorb-union.lisp -- CORBA Union support

(in-package :clorb)

(defclass CORBA:UNION () 
  ((discriminator 
    :initarg :discriminator
    :accessor union-discriminator)
   (value
    :initarg :value
    :accessor union-value )))


(define-typecode union-typecode
  :kind :tk_union
  :cdr-syntax (complex :tk_string :tk_string :tk_typecode :tk_long     
                       (sequence (2 :tk_string :tk_typecode)))
  :params (id name discriminator_type default_index :members)
  :member-params (member_label member_name member_type))


(defun create-union-tc (id name discriminator-type members)
  "Create a TypeCode for union type.
members = ( (label name typecode)* )
where label = symbol clorb:default or value"
  (check-type id string)
  (check-type name string)
  (check-type discriminator-type corba:typecode)
  (setq members (coerce members 'list))
  (let* ((default-index -1)
         (massaged-members 
          (loop for (label name typecode) in members
                for i from 0
                do (when (eq label 'default)
                     (setq default-index i)
                     (setq label (arbritary-value discriminator-type)))
                collect (list label name typecode))))
    (make-typecode :tk_union id name
                   discriminator-type default-index 
                   (coerce massaged-members 'vector))))


(defmethod any-typecode ((obj corba:union))
  (symbol-typecode (class-name (class-of obj))))

(defmethod any-value ((obj corba:union))
  obj)

;; FIXME: this is not standard, should not be in CORBA package
(defun corba:union (&key union-discriminator union-value
                           id typecode)
  (let ((id (or id (and typecode (op:id typecode)))))
    (let ((name (ifr-id-symbol id)))
      (if name
        (funcall name 
                 :union-discriminator union-discriminator
                 :union-value union-value)
        (make-instance 'corba:union
          :discriminator union-discriminator
          :value union-value)))))

(define-method default ((obj corba:union)) (union-value obj))
(define-method (setf default) (value (obj corba:union)) 
  (setf (union-value obj) value))


(defun typecode-values-do (function typecode) 
  (case (op:kind typecode)
    (:tk_char 
     (loop for code from 0 below char-code-limit
           for char = (code-char code)
           when char do (funcall function char)))
    (:tk_boolean (funcall function nil) (funcall function t)) 
    (:tk_enum (doseq (sym (tc-keywords typecode))
                (funcall function sym)))
    (otherwise (loop for i from 0 do (funcall function i)))))



(defmethod compute-unmarshal-function ((tc union-typecode))
  (let* ((id (op:id tc))
         (discriminant-type (op:discriminator_type tc))
         (default-used (op:default_index tc))
         (members      (tc-members tc)))
    (lambda (buffer)
      (let* ((discriminant (unmarshal discriminant-type buffer))
             (index
              (do ((i 0 (1+ i)))
                  ((or (>= i (length members))
                       (and (not (eql i default-used))
                            (eql discriminant (first (aref members i)))))
                   (if (>= i (length members))
                     default-used
                     i))))
             (tc (third (aref members index))))
        (corba:union :id id
                     :union-discriminator discriminant
                     :union-value (unmarshal tc buffer))))))


(defmethod compute-marshal-function ((tc union-typecode))
  (let* ((discriminant-type (op:discriminator_type tc))
         (default-used (op:default_index tc))
         (members (tc-members tc)))
    (lambda (union buffer)
      (let* ((discriminant (union-discriminator union))
             (value (union-value union))
             (member (find discriminant members :key #'car)))
        (when (and (null member)
                   (>= default-used 0))
          (setq member (aref members default-used)))
        (unless member
          (raise-system-exception 'CORBA:MARSHAL))
        (marshal discriminant discriminant-type buffer)
        (marshal value (third member) buffer)))))



;;; clorb-union.lisp ends here
