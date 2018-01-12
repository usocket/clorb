(in-package :clorb)

(defvar *idef-read-agenda* nil)
(defvar *idef-current-prefix* nil)

(defun idef-read (sexps repository)
  (let ((*idef-read-agenda* nil))
    (idef-read-contents sexps repository)
    (loop while *idef-read-agenda* do
          (let ((agenda (nreverse *idef-read-agenda*)))
            (setq *idef-read-agenda* nil)
            (mapc #'funcall agenda)))))


(defun idef-read-contents (list container)
  (dolist (sexp list)
    (let ((todo
           (idef-read-part (first sexp) (rest sexp) container)))
      (when todo (push todo *idef-read-agenda*)))))


(defmethod idef-read-part ((op (eql 'with-prefix))
                           sexp container)
  (destructuring-bind (prefix &rest forms) sexp
    (let ((*idef-current-prefix* prefix))
      (idef-read-contents forms container)
      nil)))

(defmethod idef-read-part ((op (eql 'set-version)) sexp container)
  (destructuring-bind (name version) sexp
    (let ((def (lookup-name-in container name)))
      (setf (op:version def) version)
      (let* ((old-id (op:id def))
             (last-colon (position #\: old-id :from-end t)))
        (assert last-colon nil "ill-formed ID")
        (setf (op:id def)
              (concatenate 'string (subseq old-id 0 (1+ last-colon))
                           version))))
    nil))

(defmethod idef-read-part ((op (eql 'set-id)) sexp container)
  (destructuring-bind (name id) sexp
    (let ((def (lookup-name-in container name)))
      (setf (op:id def) id))
    nil ))


(defmethod idef-read-part ((op (eql 'define-module))
                           sexp container)
  (destructuring-bind (name (&key version id) &rest forms) sexp
    (setq name (string name))
    (let ((module (op:lookup container name)))
      (unless module
        (setq module (create-contained container 'module-def
                                       :name name
                                       :version version
                                       :id id)))
      (idef-read-contents forms module)
      nil)))


(defmethod idef-read-part ((op (eql 'define-interface))
                           sexp container)
  (destructuring-bind (name (&key bases id version) &rest forms) sexp
    (setq name (string name))
    (let* ((base-interfaces
            (loop for i-name in bases
                  for iface = (lookup-name-in container i-name nil)
                  when (null iface) return :delayed
                  collect iface))
           (idef (or (lookup-name-in container name nil)
                     (create-contained
                      container 'interface-def
                      :name name :version version :id id
                      :base_interfaces '()))))
      (when (consp base-interfaces)
        (setf (op:base_interfaces idef) base-interfaces))
      (idef-read-contents forms idef)
      (when (eq base-interfaces :delayed)
        (setq *idef-read-agenda*
              (nconc *idef-read-agenda*
                     (list (lambda ()
                             (setf (op:base_interfaces idef)
                                   (mapcar #'(lambda (i-name)
                                               (lookup-name-in container i-name))
                                           bases)))))))
      nil)))



(defmethod idef-read-part ((op (eql 'define-type)) sexp container)
  (destructuring-bind (name typespec) sexp
    (setq name (string name))
    (let ((alias (create-contained container 'alias-def :name name)))
      (lambda ()
        (setf (op:original_type_def alias)
          (parse-type-in container typespec))))))


(defmethod idef-read-part ((op (eql 'define-operation)) sexp container)
  (destructuring-bind (name params &key result-type exceptions version id 
                            (mode :op_normal))
      sexp
    (setq name (string name))
    (let ((op (create-contained container 'operation-def
                                :name name :version version :id id
                                :mode :op_normal )))
      (lambda ()
        (setf (slot-value op 'params)
          (loop for p in params
              for type-def = (parse-type-in container (third p))
              collect (CORBA:ParameterDescription
                       :name (string (second p))
                       :mode (first p)
                       :type CORBA:tc_void
                       :type_def type-def)))
        (setf (slot-value op 'result_def)
          (parse-type-in container (or result-type 'void)))
        (setf (slot-value op 'exceptions)
          (mapcar #'(lambda (name)
                      (lookup-name-in container name))
                  exceptions))
        (setf (op:mode op) mode)))))


(defmethod idef-read-part ((op (eql 'define-enum)) sexp container)
  (destructuring-bind (name members &key version id expand) sexp
    (setq name (string name))
    (let ((def (create-contained container 'enum-def
                                 :name name :members members
                                 :version version :id id)))
      (when expand
        (dolist (m members)
          (create-contained container 'constant-def
                            :name m
                            :type_def def
                            :value (CORBA:Any :any-value (key m)
                                              :any-typecode CORBA:tc_void)))))
    nil))

(defmethod idef-read-part ((op (eql 'define-struct)) sexp container)
  (destructuring-bind (name members &key version id) sexp
    (setq name (string name))
    (let ((def (create-contained container 'struct-def
                                 :name name :version version :id id)))
      (lambda ()
        (setf (op:members def)
          (mapcar (lambda (x)
                    (CORBA:StructMember
                     :name (string (first x))
                     :type_def (parse-type-in container (second x))
                     :type CORBA:tc_void ))
                  members))))))

(defmethod idef-read-part ((op (eql 'define-union)) sexp container)
  (destructuring-bind (name discriminator-type members &key version id) sexp
    (setq name (string name))
    (let ((def (create-contained container 'union-def
                                 :name name :version version :id id)))
      (lambda ()
        (setf (op:discriminator_type_def def) (parse-type-in container discriminator-type))
        (setf (op:members def)
          (mapcar (lambda (x)
                    (destructuring-bind (label name type) x
                      (CORBA:UnionMember
                       :name (string name)
                       :label (if (eql label 'default)
                                (load-time-value
                                 (CORBA:Any :any-typecode CORBA:tc_octet
                                            :any-value 0))
                                (CORBA:Any :any-typecode (op:discriminator_type def)
                                           :any-value (eval-expr-in container label)))
                       :type_def (parse-type-in container type))))
                  members))))))

(defmethod idef-read-part ((op (eql 'define-exception)) sexp container)
  (destructuring-bind (name members &key version id) sexp
    (setq name (string name))
    (let ((def (create-contained container 'exception-def
                                 :name name :version version :id id)))
      (lambda ()
        (setf (op:members def)
          (mapcar (lambda (x)
                    (let ((type (parse-type-in container (second x))))
                      (CORBA:StructMember
                       :name (first x)
                       :type_def type
                       :type CORBA:tc_void)))
                  members))))))


(defmethod idef-read-part ((op (eql 'define-attribute)) sexp container)
  (destructuring-bind (name type &key readonly id) sexp
    (setq name (string name))
    (let ((def (create-contained container 'attribute-def
                                 :id id :name name 
                                 :mode (if readonly
                                         :attr_readonly
                                         :attr_normal))))
      (lambda ()
        (setf (op:type_def def)
          (parse-type-in container type))))))


(defmethod idef-read-part ((op (eql 'define-constant)) sexp container)
  (destructuring-bind (name type value &key id version) sexp
    (let ((def (create-contained container 'constant-def 
                                 :name name :id id :version version)))

      (lambda ()
        (let ((type (parse-type-in container type)))
          (setf (op:type_def def) type)
          (setf (op:value def) (corba:any :any-value (eval-expr-in container value)
                                          :any-typecode (op:type type))))))))


(defun create-contained (container class &rest args
                         &key name version id
                         &allow-other-keys)
  (setq name (string name))
  (setq version (or version "1.0"))
  (setq id (or id
               (apply #'concatenate 'string
                      "IDL:"
                      `(,@(if *idef-current-prefix*
                              (list *idef-current-prefix* "/"))
                          ,@(nreverse (repo-path container))
                          ,name ":" ,version))) )
  (let ((obj (apply #'make-instance class
                    :name name
                    :id id 
                    :version version
                    :defined_in container
                    args)))
    (addto container obj)
    obj))

(defun repo-path (module)
  (if (and module (not (typep module 'CORBA:Repository)))
      (list*
        "/" (op::name module)
        (repo-path (op::defined_in module)))
    nil))

(defun primitive-kind (type)
  (case type
    (null (values :pk_null CORBA:tc_null))
    (void (values :pk_void CORBA:tc_void))
    (short (values :pk_short CORBA:tc_short))
    (long (values :pk_long CORBA:tc_long))
    (ushort (values :pk_ushort CORBA:tc_ushort))
    (ulong (values :pk_ulong CORBA:tc_ulong))
    (float (values :pk_float CORBA:tc_float))
    (double (values :pk_double CORBA:tc_double))
    (boolean (values :pk_boolean CORBA:tc_boolean))
    (char (values :pk_char CORBA:tc_char))
    (octet (values :pk_octet CORBA:tc_octet))
    (any (values :pk_any CORBA:tc_any))
    (TypeCode (values :pk_typecode CORBA:tc_TypeCode))
    (string (values :pk_string CORBA:tc_string))
    (object (values :pk_objref CORBA:tc_object))
    (longlong (values :pk_longlong CORBA:tc_longlong))
    (ulonglong (values :pk_ulonglong CORBA:tc_ulonglong))
    (longdouble (values :pk_longdouble CORBA:tc_longdouble))
    (wchar (values :pk_wchar CORBA:tc_wchar))
    (wstring (values :pk_wstring CORBA:tc_wstring))))


(defun parse-type-in (container type-sexp)
  (let ((repository (op:containing_repository container))) 
    (or 
     (cond ((symbolp type-sexp)
            (let ((kind (primitive-kind type-sexp)))
              (and kind (op:get_primitive repository kind))))
           ((stringp type-sexp)
            (lookup-name-in container type-sexp))
           ((consp type-sexp)
            (case (car type-sexp)
              ((sequence)
               (destructuring-bind (member-type &optional (bound 0)) (cdr type-sexp)
                 (op:create_sequence repository (or bound 0) 
                                     (parse-type-in container member-type))))
              ((array)
               (destructuring-bind (member-type &optional (length 0)) (cdr type-sexp)
                 (when length
                   (setq length (eval-expr-in container length)))
                 (op:create_array repository (or length 0)
                                  (parse-type-in container member-type))))
              ((string wstring)
               (destructuring-bind (string-type &optional (bound 0))
                                   type-sexp
                 (if (zerop bound)
                   (op:get_primitive repository (primitive-kind string-type))
                   (if (eq string-type 'string)
                     (op:create_string repository bound)
                     (op:create_wstring repository bound)))))
              ((fixed)
               (destructuring-bind (digits scale) (cdr type-sexp)
                 (op:create_fixed repository digits scale))))))
     (error "Invalid type spec: ~S" type-sexp))))


(define-method defined_in ((r CORBA:Repository))
  nil)

(defun lookup-name-in (container qname &optional (default nil no-error-p))
  "Find an object given its qualified name"
  (let ((object (op:lookup container qname)))
    (if object
      object
      (if no-error-p
        default
        (error "Name '~A' not found" qname)))))  

(defun eval-expr-in (container expr)
  (cond ((stringp expr) 
         (let ((obj (lookup-name-in container expr)))
           (assert (eq (op:def_kind obj) :dk_constant))
           (any-value (op:value obj))))
        ((and (consp expr)
              (eq 'string (car expr)))
         (cadr expr))
        ((consp expr)
         (apply (car expr)
                (mapcar #'(lambda (x)
                            (eval-expr-in container x))
                        (cdr expr))))
        (t
         expr)))


(defun << (int n)
  (ash int n))

(defun >> (int n)
  (ash (logand int #xFFFFFFFF) (- n)))


(defun parse-name (name)
  (loop with parts = '()
      for cp = (position #\: name)
      while cp
      do (progn
           (push (if (zerop cp) :absolute
                   (subseq name 0 cp)) parts)
           (loop while (eql #\: (elt name cp)) do (incf cp))
           (setq name (subseq name cp)))
      finally (return
                (nreverse (cons name parts)))))


