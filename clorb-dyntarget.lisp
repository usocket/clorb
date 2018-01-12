;;;; clorb-dyntarget.lisp -- Code Generator for CLORB

(in-package :clorb)


(defclass dynamic-1-stub-target (stub-target)
  ())


(defclass dynamic-2-stub-target (stub-target)
  ())




(defmethod target-code ((op CORBA:OperationDef) (target dynamic-1-stub-target))
  (let* ((op-name (op:name op))
         (class (target-proxy-class-symbol target (op:defined_in op)))
         (lisp-name (make-target-symbol target op-name :op))
         (args (in-param-list (coerce (op:params op) 'list))))
    `(defmethod ,lisp-name ((obj ,class) ,@args)
       (corba:funcall ,op-name obj ,@args))))


(defmethod target-code ((op CORBA:OperationDef) (target dynamic-2-stub-target))
  (let* ((op-name (op:name op))
         (class (target-proxy-class-symbol target (op:defined_in op)))
         (lisp-name (string-upcase op-name))
         (params (coerce (op:params op) 'list))
         (args (in-param-list params)))
    `(define-method ,lisp-name ((obj ,class) ,@args)
       (let ((request
              (request-create obj ,op-name ,(target-typecode (op:result_def op) target))))
         ,@(loop for pd in params
                 for mode = (op:mode pd)
                 collect `(add-arg _request ,(op:name pd)
                                   ,(ecase mode
                                      (:param_in 'ARG_IN)
                                      (:param_out 'ARG_OUT)
                                      (:param_inout 'ARG_INOUT))
                                   ,(target-typecode (op:type_def pd) target)
                                   ,@(if (eq mode :param_out) nil (list (pop args)))))
         ,@(map 'list
                (lambda (ed)
                  `(add-exception request ,(target-typecode ed target)))
                (op:exceptions op))
         (request-funcall request)))))



(defmethod target-code ((op CORBA:AttributeDef) (target dynamic-1-stub-target))
  (let* ((att-name (op:name op))
         (lisp-name (string-upcase att-name))
         (class (target-proxy-class-symbol target (op:defined_in op))))
    `(progn
       (define-method ,lisp-name ((obj ,class))
         (corba:funcall ,(getter-name att-name) obj))
       ,@(if (eq (op:mode op) :attr_normal)
           `((define-method (setf ,lisp-name) (newval (obj ,class))
               (corba:funcall ,(setter-name att-name) obj newval)))))))


(defmethod target-code ((op CORBA:AttributeDef) (target dynamic-2-stub-target))
  (let* ((att-name (op:name op))
         (lisp-name (string-upcase att-name))
         (class (target-proxy-class-symbol target (op:defined_in op)))
         (typecode (target-typecode (op:type_def op) target)))
    `(progn
       (define-method ,lisp-name ((obj ,class))
         (get-attribute obj ,(getter-name att-name) ,typecode))
       ,@(if (eq (op:mode op) :attr_normal)
           `((define-method (setf ,lisp-name) (newval (obj ,class))
               (set-attribute obj ,(setter-name att-name) ,typecode newval)))))))



(defclass dynamic-sevant-target (code-target)
  ())


(defmethod target-code ((idef CORBA:InterfaceDef) (target dynamic-sevant-target))
  (let ((bases (op:base_interfaces idef))
        (class-symbol (target-servant-class-symbol target idef)))
    (make-progn*
     (call-next-method)
     `(define-corba-class ,class-symbol
        ,(list* (scoped-target-symbol target idef)
                (target-base-list target bases #'target-servant-class-symbol
                                  'PortableServer:DynamicImplementation))
        :attributes (servant-attribute-declaration idef))
     `(define-method "INVOKE" ((servant ,class-symbol) request)
        (let ((op (op:operation request)))
          (cond ,@(remove nil
                          (map 'list
                               (lambda (def) (target-invoke-dynamic def target))
                               (op:contents idef :dk_all t)))
                (t 
                 (call-next-method))))))))


(defmethod target-invoke-dynamic ((def CORBA:IRObject) target)
  (declare (ignore target))
  nil)


(defmethod target-invoke-dynamic ((def CORBA:OperationDef) target)
  (let ((params (op:params def)))
    `((string= op ,(op:name def))
      (let (,@(map 'list 
                   (lambda (pd)
                     (list (param-symbol pd)
                           `(CORBA:Any :any-typecode ,(target-typecode (op:type_def pd) target))))
                   params))
        (op:arguments request
                      (list ,@(map 'list
                                   (lambda (pd)
                                     `(CORBA:NamedValue :argument ,(param-symbol pd)
                                                        :arg_mode ,(case (op:mode pd)
                                                                     (:param_in 'ARG_IN)
                                                                     (:param_out 'ARG_OUT)
                                                                     (:param_inout 'ARG_INOUT))))
                                   params) ))
        (multiple-value-bind (_res xx)
                             (,(feature (op:name def))
                              ,@(remove nil
                                        (map 'list 
                                             (lambda (pd)
                                               (unless (eq (op:mode pd) :param_out)
                                                 `(any-value ,(param-symbol pd))))
                                             params)))
          (op:set_result request _res))))))

