;;;; Internal Interface Repository

(in-package :clorb)


;;; Interface type

(defstruct INTERFACE
  id
  operations
  inherit)

(define-slot-dumper interface)


(defmethod find-opdef ((interface interface) operation)
  "Find in INTERFACE the OPERATION and return the opdef struct."
  (or (find operation
	    (interface-operations interface)
	    :test #'equal
	    :key #'opdef-name)
      (loop for pint in (interface-inherit interface)
          thereis (find-opdef pint operation))))

(define-method is_a ((interface interface) id)
  (or (equal id (interface-id interface))
      (some (lambda (i) (op:is_a i id))
            (interface-inherit interface))))


(defun print-interface (intf &optional (stream *standard-output*) k)
  (declare (ignore k))
  (print-unreadable-object
   (intf stream :type t :identity t)
   (princ (interface-id intf) stream)))

(defmethod print-object ((intf interface) stream)
  (cond
   (*print-readably*
    (call-next-method))
   (t
    (print-interface intf stream))))



;;;; Internal Interface Repository

#|(eval-when (:load-toplevel)
  (makunbound '*repository*)
  (makunbound '*typecode-repository*))
|#

(defvar *repository*
  (make-hash-table :test #'equal))

(defvar *typecode-repository*
  (make-hash-table :test #'equal))

(defun add-interface (interface)
  (setf (gethash (interface-id interface)
		 *repository*)
	interface))

(defun get-interface (id)
  (assert (stringp id))
  (or (gethash id *repository*)
      (setf (gethash id *repository*)
            (interface-from-def (ir-lookup-id id) id))))

(defun known-interface (id)
  (gethash id *repository*))

(defun known-idl-type (id)
  (gethash id *typecode-repository*))

;;(defun add-typecode (typecode &optional (force t))
;;  (add-typecode-with-id (op:id typecode) typecode force))

(defun add-typecode-with-id (id typecode &optional (force t))
  (or (if (not force) (gethash id *typecode-repository*))
      (setf (gethash id *typecode-repository*) typecode)))

(defun get-typecode (id)
  (assert (stringp id))
  (or (gethash id *typecode-repository*)
      (setf (gethash id *typecode-repository*)
	    (typecode-from-def id))))


(defun simplify-type (typecode)
  (macrolet ((mush (id def)
               `(or (known-idl-type ,id)
                    (progn (add-typecode-with-id ,id typecode)
                           ,def)))
             (simplifyf (var)
               `(progn (setf ,var (simplify-type ,var))
                 typecode))
             (simplifyv (vec fun)
               `(progn (loop for el across ,vec do (simplifyf (,fun el)))
                 typecode)))
    (let ((params (typecode-params typecode)))
      (case (typecode-kind typecode)
        ((:tk_alias) (mush (first params) (simplifyf (third params))))
        ((:tk_sequence) (simplifyf (first params)))
        ((:tk_struct) (mush (first params) (simplifyv (third params) second)))
        ((:tk_except) (mush (first params) (simplifyv (third params) second)))
        (t  typecode)))))

;;;; IR -- initial repository contents

(defparameter *object-interface*
  (make-interface
   :id "IDL:omg.org/CORBA/Object:1.0"
   :operations (list
                (make-opdef :name "_is_a"
                            :params (list
                                     (make-param "id" :param_in corba:tc_string))
                            :result corba:tc_boolean
                            :raises '())
                (make-opdef :name "_interface"
                            :result corba:tc_object)
                (make-opdef :name "_non_existent"
                            :result corba:tc_boolean))))

(add-interface *object-interface*)


;;;; Using real IR

(defun get-ir ()
  (op:resolve_initial_references
   (CORBA:ORB_init) "InterfaceRepository"))

(defun ir-lookup-id (id)
  (multiple-value-bind (result req)
      (op:_create_request
       (get-ir) nil "lookup_id"
       (list
        (CORBA:NamedValue
         :name "id"
         :argument id
         :arg_modes ARG_IN))
       (CORBA:NamedValue
        :argument (CORBA:Any :any-typecode CORBA:tc_object)
        :arg_modes ARG_OUT)
       0)
    (declare (ignore result))
    (or (request-funcall req)
        (error "InterfaceRepository does not know about ~A" id))))

(defun opdef-from-attrdef (irdef)
  (let ((name (get-attribute irdef "_get_name" CORBA:tc_string))
        (mode (get-attribute irdef "_get_mode"
                             (get-typecode "IDL:omg.org/CORBA/AttributeMode:1.0")))
        (type (simplify-type
               (get-attribute irdef "_get_type" CORBA:tc_TypeCode))))
    (mess 3 " attribute ~A ~A" name mode)
    (cons (make-opdef
           :name (format nil "_get_~A" name)
           :result type)
          (if (eq mode :attr_normal)
              (list (make-opdef
                     :params (list (make-param "" :param_in type))
                     :name (format nil "_set_~A" name)
                     :result CORBA:tc_void))))))


(defun opdef-from-ir (irdef)
  (let ((parseq (get-typecode "IDL:omg.org/CORBA/ParDescriptionSeq:1.0"))
        (name (get-attribute irdef "_get_name" CORBA:tc_string))
	(result (get-attribute irdef "_get_result" CORBA:tc_typecode)))
    (mess 3 " operation ~A" name)
    (make-opdef
     :name name
     :params (map 'list (lambda (pardesc)
                          (make-param (op:name pardesc)
                                      (op:mode pardesc)
                                      (simplify-type (op:type pardesc))))
                  (get-attribute irdef "_get_params" parseq))
     :result (simplify-type result)
     :raises (map 'list
                  (lambda (exdef)
                    (get-attribute exdef "_get_type" CORBA:tc_typecode))
                  (get-attribute irdef "_get_exceptions" 
                                 (get-typecode "IDL:omg.org/CORBA/ExceptionDefSeq:1.0"))))))

(defun ir-contents (container limit-type exclude-inherit)
  (let* ((cseq (get-typecode "IDL:omg.org/CORBA/ContainedSeq:1.0")))
    (multiple-value-bind (result req)
        (op:_create_request
         container nil "contents" nil
         (CORBA:NamedValue
          :argument (CORBA:Any :any-typecode cseq)
          :arg_modes ARG_OUT)
         0)
      (declare (ignore result))
      ;; FIXME: definition kind
      (add-arg req "limit_type" ARG_IN CORBA:tc_long limit-type)
      (add-arg req "exclude_inherit" ARG_IN CORBA:tc_boolean exclude-inherit)
      (request-funcall req))))


(defun interface-from-def-cached (def)
  (let ((id (get-attribute def "_get_id" CORBA:tc_string)))
    (or (known-interface id)
        (add-interface (interface-from-def def id)))))

(defun interface-from-def (def id)
  (mess 3 "Getting interface ~A" id)
  (let ((idseq (get-typecode "IDL:omg.org/CORBA/InterfaceDefSeq:1.0")))
    (make-interface
     :id id
     :inherit
     (or (map 'list
              #'interface-from-def-cached
              (get-attribute def "_get_base_interfaces" idseq))
         (list *object-interface*))
     :operations
     (nconc (mapcan #'opdef-from-attrdef (coerce (ir-contents def 2 t) 'list))
            (map 'list #'opdef-from-ir (ir-contents def 7 t))))))

(defun typecode-from-def (def)
  (when (stringp def)
    (mess 3 "Getting type ~A" def)
    (setq def (ir-lookup-id def)))
  (simplify-type
   (get-attribute def "_get_type" CORBA:tc_typecode)))
