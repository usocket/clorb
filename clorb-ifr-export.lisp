;; Servant wrapper for local IFR

(in-package :clorb)



;;;; Object indexer
;; (object-index oi obj) => integer
;; (index-object oi integer) => obj
;; --
(defclass OBJECT-INDEXER ()
  ((imap
    :initform (make-hash-table)
    :accessor imap)
   (omap
    :initform (make-hash-table)
    :accessor omap)
   (count
    :initform 0
    :accessor oi-count)))  

(defmethod object-index ((oi object-indexer) object)
  (or (gethash object (omap oi))
      (let ((i (incf (oi-count oi))))
        (setf (gethash object (omap oi)) i)
        (setf (gethash i (imap oi)) object)
        i)))

(defmethod index-object ((oi object-indexer) index)
  (gethash index (imap oi)))



;;;; Storing integers in object id

(defconstant +oid-radix+ 36)

(defun oid-integer (oid)
  (parse-integer (oid-to-string oid) :radix +oid-radix+))

(defun integer-oid (integer)
  (string-to-oid (format nil "~vR" +oid-radix+ integer)))



;;;; Object Translation

(defgeneric translate (translator object)
  (:method (x (obj number)) 
           (declare (ignore x))
           obj)
  (:method (x (obj symbol))
           (declare (ignore x))
           obj)
  (:method (x (obj string))
           (declare (ignore x))
           obj)
  (:method (x (obj CORBA:TypeCode))
           (declare (ignore x))
           obj)
  (:method (x (obj vector))
           (map 'vector #'(lambda (o) (translate x o)) obj))
  (:method (x (obj sequence))
           (map 'list #'(lambda (o) (translate x o)) obj))
  (:method (x (obj CORBA:struct))
           (map-struct (lambda (v) (translate x v)) obj))
  (:method (x (obj CORBA:Any))
           (CORBA:Any :any-typecode (corba:any-typecode obj)
                      :any-value (translate x (corba:any-value obj)))))


(defclass TRANSLATOR ()
  ((wrapper :initarg :wrapper :reader wrapper)))

(defmethod objmap ((x translator)) (objmap (wrapper x)))
(defmethod the-poa ((x translator)) (the-poa (wrapper x)))

(defclass LOCAL-TRANSLATOR (translator) ())

(defmethod translate ((x local-translator) (obj corba:proxy))
  (index-object (objmap x) (oid-integer (op:reference_to_id (the-poa x) obj))))

(defclass REMOTE-TRANSLATOR (translator) ())

(defmethod translate ((x remote-translator) (obj irobject))
  (op:create_reference_with_id (the-poa x)
                               (integer-oid (object-index (objmap x) obj))
                               (object-id obj)))

;;;; Servant 

(defclass SERVANT-WRAPPER (portableserver:dynamicimplementation)
  ((orb
    :initarg :orb
    :initform (CORBA:ORB_init)
    :reader the-orb)
   (poa
    :initarg :poa
    :reader the-poa)
   (objmap
    :initform (make-instance 'object-indexer)
    :reader objmap)
   (local-translator)
   (remote-translator)))

(define-method _default_poa ((self servant-wrapper))
  (the-poa self))

(define-method primary_interface ((self servant-wrapper) oid poa)
  (declare (ignore poa))
  (object-id (index-object (objmap self) (oid-integer oid))))

(define-method _is_a ((self servant-wrapper) id)
  (object-is-a (index-object (objmap self) (oid-integer (op:_object_id self)))
               id))

(defmethod local-translator ((self servant-wrapper))
  (with-cache-slot (self local-translator)
    (make-instance 'local-translator :wrapper self)))

(defmethod remote-translator ((self servant-wrapper))
  (with-cache-slot (self remote-translator)
    (make-instance 'remote-translator :wrapper self)))


(defun make-remote (wrapper obj)
  (translate (remote-translator wrapper) obj))

(defun make-local (wrapper obj)
  (translate (local-translator wrapper) obj))


(defun find-contained (sym name)
  (or (find name (get sym 'ifr-contents)
            :key (lambda (sym) (get sym 'ifr-name))
            :test #'equal)
      (some (lambda (base) (find-contained base name))
            (get sym 'ifr-bases))))


(defun wrapper-opinfo (self operation)
  ;; values: func type paramlist result-type
  (let* ((id (op:primary_interface self (op:_object_id self) (op:_poa self)))
         (interface (or (ifr-id-symbol id)
                        (error "can't find interface: ~S" id))))
    (multiple-value-bind (name type) (analyze-operation-name operation)
      (let ((def (find-contained interface name))
            (sym (feature name)))
        (assert def)
        (case type
          (:setter
           (assert (get def 'ifr-type))
           (values (fdefinition (list 'setf sym)) :setter
                   (list (corba:namedvalue 
                          :argument (corba:any :any-typecode (get def 'ifr-type))
                          :arg_modes ARG_IN))
                   CORBA:tc_void))
          (:getter
           (assert (get def 'ifr-type))
           (values sym nil nil (get def 'ifr-type)))
          (otherwise
           (values sym nil 
                   (loop for (name mode type) in (get def 'ifr-params)
                         collect (corba:namedvalue
                                  :name name
                                  :argument (corba:any :any-typecode type)
                                  :arg_modes (ecase mode
                                               (:param_in ARG_IN)
                                               (:param_out ARG_OUT)
                                               (:param_inout ARG_INOUT))))
                   (get def 'ifr-result))))))))


(define-method invoke ((self servant-wrapper) r)
  (multiple-value-bind (op type args result-type)
                       (wrapper-opinfo self (op:operation r))
    (setq args (op:arguments r args))
    (let* ((local-obj (make-local self (op:_this self)))
           (local-args 
            (loop for arg in args 
                  when (/= 0 (logand ARG_IN (op:arg_modes arg)))
                  collect (make-local self (any-value (op:argument arg)))))
           (res-list 
            (loop for x in (multiple-value-list
                            (if (eq type :setter)
                              (funcall op (first local-args) local-obj)
                              (apply op local-obj local-args)))
                  collect (make-remote self x)))
           (result (unless (eq :tk_void (op:kind result-type))
                     (pop res-list))))
      (loop for arg in args
            when (/= 0 (logand ARG_OUT (op:arg_modes arg)))
            do (setf (any-value (op:argument arg)) (pop res-list)))
      (op:set_result r (corba:any :any-typecode result-type
                                  :any-value result)))))



;;;; Set up a real thingy

#|

 (defvar *z-rep* (make-instance 'repository))
 (defvar *z-poa* (let ((rootpoa (op:resolve_initial_references *the-orb* "RootPOA")))
                   (op:create_poa rootpoa "IFWRAP"
                                  nil
                                  '(:use_default_servant
                                    :user_id
                                    :transient))))
 (defvar *z-serv* (make-instance 'servant-wrapper
                       :orb *the-orb*
                       :poa *z-poa*))

 (op:set_servant *z-poa* *z-serv*)
 (op:activate (op:the_poamanager *z-poa*))

 ;;(idef-read (idef-write *idef-repository*) *z-rep*)

 (define-method _this ((obj IRObject))
   (make-remote *z-serv* obj))

 ;;(rebind (make-remote *z-serv* *z-rep*) "zrep")

 (make-remote *z-serv* *internal-interface-repository*)

|#

(defvar *ifr-export-servant* nil)
(defvar *ifr-export-poa* nil)

(defun export-ifr ()
  (or *ifr-export-servant*
      (let ((orb (CORBA:ORB_init)))
        (or *ifr-export-poa*
            (setq *ifr-export-poa*
                  (op:create_poa (op:resolve_initial_references orb "RootPOA") 
                                 "_IFWRAP"
                                 nil
                                 '(:use_default_servant :user_id :transient))))
        (let ((servant (make-instance 'servant-wrapper
                         :orb orb :poa *ifr-export-poa*)))
          (op:set_servant *ifr-export-poa* servant)
          (op:activate (op:the_poamanager *ifr-export-poa*))
          (setq *ifr-export-servant* servant)))))

(define-method _this ((obj IRObject))
  (make-remote (export-ifr) obj))
