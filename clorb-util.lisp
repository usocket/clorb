;;;; Nice interface to NameService etc.

(in-package :clorb)


;;;; Accessing Interface Repository ID

(define-method op::id ((type-symbol symbol))
  (symbol-ifr-id type-symbol))

(define-method op::type ((type-symbol symbol))
  (symbol-typecode type-symbol))


;;;; Accessing Interface Repositories

(defclass repository-facade (CORBA:Repository)
  ((loaded-idl-repositories 
    :initform (make-hash-table :test #'equal)
    :accessor loaded-idl-repositories)
   (repository-list
    :initarg :repository-list
    :initform '()
    :accessor repository-list)))


(defvar *internal-interface-repository* (make-instance 'repository-facade))

(defun map-repositories (proc facade)
  (loop for rep being the hash-values of (loaded-idl-repositories facade)
        do (funcall proc rep))
  (loop for candidate in (repository-list facade)
        for rep = (if (symbolp candidate)
                    (symbol-value candidate)
                    candidate)
        do (funcall proc rep)))

(define-method lookup_id ((rep repository-facade) id)
  (block search-loop
    (map-repositories 
     (lambda (r)
       (let ((obj (op:lookup_id r id)))
         (when obj (return-from search-loop obj)))) 
     rep)))


(defmethod add-repository ((facade repository-facade) repository-or-symbol)
  (pushnew repository-or-symbol (repository-list facade)))

(defmethod add-idl-repository ((facade repository-facade) file repository)
  (setf (gethash file (loaded-idl-repositories facade))
        repository))


;;;; file URL

(defun pathname-url (pathname)
  (setq pathname (translate-logical-pathname pathname))
  (format nil "file://~{/~A~}/~A~@[.~A~]"
          (cdr (pathname-directory pathname))
          (pathname-name pathname)
          (pathname-type pathname)))


;;;; Easy DII

(defun corba:funcall (op obj &rest args)
  (request-funcall
   (object-create-request obj op args)))

(defun invoke (obj op &rest args)
  (apply #'corba:funcall op obj args))


(defun object-interface (obj)
  (handler-case
    (static-call ("_interface" obj)
                 :input ((buffer)
                         (unmarshal (%symbol-typecode corba:interfacedef) buffer)))
    (CORBA:SystemException
     (exc)
     (if (member (exception-id exc)
                 '("IDL:omg.org/CORBA/BAD_OPERATION:1.0"
                   "IDL:CORBA/BAD_OPERATION:1.0" ; -dan / for ORBit
                   "IDL:omg.org/CORBA/NO_IMPLEMENT:1.0"
                   "IDL:omg.org/CORBA/INTF_REPOS:1.0"
                   "IDL:omg.org/CORBA/OBJ_ADAPTER:1.0" ; mico
                   )
                 :test #'equal)
       nil
       (error exc)))))


;;; from clorb-object.lisp
;;;| InterfaceDef get_interface ();
;;; Strange that the lisp mapping does not rename this.

(define-method _get_interface ((obj CORBA:Object))
  (or (op:lookup_id *internal-interface-repository* (object-id obj))
      (raise-system-exception 'CORBA:intf_repos)))

(define-method _get_interface ((obj CORBA:Proxy))
  (let ((id (proxy-id obj)))
    (or (unless (equal id "")
          (op:lookup_id *internal-interface-repository* id))
        (object-interface obj)
        (raise-system-exception 'CORBA:intf_repos))))


(define-method _get_value_def ((self CORBA:ValueBase))
  ;; Return ValueDef
  (let ((id (object-id self)))
    (or (op:lookup_id *internal-interface-repository* id)
        (raise-system-exception 'CORBA:intf_repos))))


(defun analyze-operation-name (name)
  (cond
   ((< (length name) 6) name)
   ((string= name "_get_" :end1 5)
    (values (subseq name 5) :getter))
   ((string= name "_set_" :end1 5)
    (values (subseq name 5) :setter))
   (t name)))




#|
This operation, which creates a pseudo-object, is defined in the ORB interface.
void create_operation_list ( // PIDL
in OperationDef oper, // operation
out NVList new_list // argument definitions
);
This operation returns an NVList initialized with the argument descriptions for a given
operation. The information is returned in a form that may be used in Dynamic
Invocation requests. The arguments are returned in the same order as they were defined
for the operation.
The list free operation is used to free the returned information.
|#

(defmethod create-operation-list ((opdef CORBA:OperationDef))
  (map 'list
       (lambda (pd) 
         (CORBA:NamedValue
          :name (op:name pd)
          :argument (CORBA:Any :any-typecode (op:type pd))
          :arg_modes (ecase (op:mode pd)
                       (:param_in ARG_IN)
                       (:param_out ARG_OUT)
                       (:param_inout ARG_INOUT))))
       (op:params opdef)))

(defmethod create-operation-list ((opdef corba:operationdescription))
  (map 'list
       (lambda (pd) 
         (CORBA:NamedValue
          :name (op:name pd)
          :argument (CORBA:Any :any-typecode (op:type pd))
          :arg_modes (ecase (op:mode pd)
                       (:param_in ARG_IN)
                       (:param_out ARG_OUT)
                       (:param_inout ARG_INOUT))))
       (op:parameters opdef)))


(defun lookup-in-memory (ifr-id name)
  (let ((symbol (find-contained (or (ifr-id-symbol ifr-id) 'corba:object) name)))
    (and symbol (ifr-description symbol))))


(defvar *description-cache*
  (make-hash-table :test #'equal))

(defun add-cache-entry (id interface-description)
  (setf (gethash id *description-cache*) interface-description))
  
(defun lookup-in-cache-entry (entry name)
  (and entry
       (or (find name (op:operations entry) :key #'op:name :test #'equal)
           (find name (op:attributes entry) :key #'op:name :test #'equal))))

(defun lookup-in-cache (id name)
  (let ((entry (gethash id *description-cache*)))
    (lookup-in-cache-entry entry name)))

(defun lookup-in-interface-def (id idef name)
  (and idef 
       (lookup-in-cache-entry 
        (add-cache-entry id (op:describe_interface idef))
        name)))

(defun lookup-idef-in-rir (orb id)
  (handler-case 
    (let ((ir (op:resolve_initial_references orb "InterfaceRepository")))
      (setq ir (nobject-narrow ir 'corba:repository))
      (when ir 
        (op:lookup_id ir id)))
    (corba:orb/invalidname nil nil)))


(defun lookup-object-operation (object name)
  (let ((id (proxy-id object)))
    (if (equal id "")
      (setq id (object-id object)))
    (or (lookup-in-memory id name)
        (lookup-in-cache id name)
        (lookup-in-interface-def id (object-interface object) name)
        (lookup-in-interface-def id (lookup-idef-in-rir (the-orb object) id) name))))


(defun object-create-request (object operation args)
  (let* ((orb (the-orb object))
         (result CORBA:tc_void)
         (paramlist nil)
         (raises nil))
    (multiple-value-bind (name type) (analyze-operation-name operation)
      (let ((def (lookup-object-operation object name)))
        (case type
          (:setter
           (unless def
             (error "Attribute ~A not defined for interface [in ~A]" name operation))
           ;;(assert (eq (op:def_kind def) :dk_attribute))
           (assert (eq (op:mode def) :attr_normal))
           (setf paramlist (list (CORBA:NamedValue :arg_modes ARG_IN
                                                   :argument (CORBA:Any :any-typecode (op:type def))))))
          (:getter
           (unless def
             (error "Attribute ~A not defined for interface" name))
           ;;(assert (eq (op:def_kind def) :dk_attribute))
           (setf result (op:type def)))
          (otherwise
           (unless def
             (error "Operation (~A) not defined for interface" operation))
           (assert (or (typep def 'corba:operationdescription)
                       (eq (op:def_kind def) :dk_operation)))
           (setf paramlist (create-operation-list def)
                 result (op:result def)
                 raises (map 'list #'op:type (op:exceptions def))))))
      (dolist (nv paramlist)
        (unless (zerop (logand ARG_IN (op:arg_modes nv)))
          (unless args 
            (error "To few arguments to operation: ~A" operation))
          (setf (any-value (op:argument nv)) (pop args))))
      (when args
        (error "To many arguments to operation: ~A" operation))
      (create-client-request
       orb
       :target object
       :operation operation
       :paramlist (cons (CORBA:NamedValue
                         :argument (CORBA:Any :any-typecode result)
                         :arg_modes ARG_OUT)
                        paramlist)
       :exceptions raises
       :error-handler 'static-error-handler))))



;;;; Easy name service access

(defun ns-name* (names)
  (mapcan #'ns-name names))

(defun ns-name (namestring)
  (let ((name nil)
        (id (make-array 50 :fill-pointer 0 :adjustable t
                        :element-type 'character))
        (kind (make-array 50 :fill-pointer 0 :adjustable t
                          :element-type 'character)))
    (loop for c across (concatenate 'string namestring "/")
          with escape = nil and part = id
          do
          (cond
            (escape
             (vector-push-extend c part)
             (setq escape nil))
            (t
             (case c
               ((#\\) (setq escape t))
               ((#\/)
                (push (CosNaming:NameComponent
                       :id (copy-seq id) :kind (copy-seq kind))
                      name)
                (setf (fill-pointer id) 0)
                (setf (fill-pointer kind) 0)
                (setq part id))
               ((#\.) (setq part kind))
               (otherwise
                (vector-push-extend c part))))))
    (nreverse name)))


;; Conveninent exported method, similar to op:to_name on NamingContextExt
;; but works directly on a string.
(define-method op:to_name ((s string))
  (ns-name s))


(defvar *pre-narrowed-ns* nil)
(defvar *narrowed-ns* nil)

(defun get-ns ()
  (let ((ns (op:resolve_initial_references (CORBA:ORB_init) "NameService")))
    (cond ((and (eq ns *pre-narrowed-ns*) *narrowed-ns*)
           *narrowed-ns*)
          (t
           (setq *pre-narrowed-ns* ns *narrowed-ns* nil)
           (setq *narrowed-ns* (or (nobject-narrow ns 'cosnaming:namingcontextext t)
                                   (nobject-narrow ns 'cosnaming:namingcontext)))))))
  

;;; To support corbaname: URLs
(defmethod orb-resolve ((orb clorb-orb) namecontext namestr)
  (op:resolve_str (object-narrow namecontext 'CosNaming:NamingContextExt)
                  namestr))


(defun resolve (&rest names)
  (op:resolve (get-ns) (ns-name* names)))

(define-method op:resolve ((name string))
  (resolve name))

(defun rebind (objref &rest names)
  (op:rebind (get-ns) (ns-name* names) objref))




;;;; Easy Object Creation


(defun obj (str &optional type)
  (let ((proxy (op:string_to_object *the-orb* str)))
    (if type
      (net.cddr.clorb::nobject-narrow proxy type)
      proxy)))



;;;; Cleaning

(defun dump-connections ()
  (format t "DID Con Srv Act #Write #Rq desc~%")
  (dolist (desc (io-descriptions-of *io-system*))
    (let ((conn (io-descriptor-connection desc)))
      (format t "~3A ~3A ~3A ~3A ~6@A ~3@A ~A~%"
              (io-descriptor-id desc)
              (not (null conn))
              (if conn (server-p conn))
              (if conn (activity conn))
              (if conn (write-count-of conn))
              (if conn (length (if (server-p conn)
                                   (connection-server-requests conn)
                                   (connection-client-requests conn))))
              (io-describe-descriptor desc)))))

(defun /desc (id)
  (find id (io-descriptions-of *io-system*) :key #'io-descriptor-id))
(defun /con (id)
  (io-descriptor-connection (/desc id)))


;;; clorb-util.lisp ends here
