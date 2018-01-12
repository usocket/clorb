(in-package :clorb)


(defvar *the-repository* )
(defvar *container* )

(defun convert-to-array (type-def array-spec)
  (if array-spec
    (op:create_array *the-repository*
                     (car array-spec)
                     (convert-to-array type-def (cdr array-spec)))
    type-def))



(defun named-create (container create-op name &rest args)
  (let ((obj (apply create-op container (repo-id name *container*) name "1.0"
                    args)))
    (setf (package-prefix obj) (package-prefix *current-cpp*))
    obj))

(defun repo-id (name container &optional (version "1.0"))
  (apply #'concatenate 'string
         "IDL:"
         `(,@(let ((prefix (idl-prefix *current-cpp*)))
               (if (not (equal prefix ""))
                 (list prefix "/")))           
           ,@(nreverse (repo-path container))
           ,name ":" ,version)))

(defmacro parse-list (form &optional sep allow-empty)
  (let ((elem (gensym))
        (list (gensym)))
    `(let (,elem ,list)
       (seq (alt (seq (-> ,form ,elem)
                      (action (push ,elem ,list))
                      (seq* ,@(if sep (list sep))
                            (-> ,form ,elem)
                            (action (push ,elem ,list))))
                 ,@(if allow-empty '((seq))))
            (action (nreverse ,list))))))


;;;; Top Level

(defun <specification> ()
  (seq (seq+ (<definition>))
       :eof
       (action (id-adjustment))))

(defun <definition> nil
  (alt (seq (<type_dcl>) ";")
       (seq (<const_dcl>) ";")
       (seq (<except_dcl>) ";")
       (seq (<interface/value>) ";")
       (seq (<module>) ";")))




;;;; Names

(defun <scoped_name> ()
  (let ((start nil)
        (names nil))
    (seq (opt (-> "::" start))
         (-> (parse-list (<identifier>) "::") names)
         (action (format nil "~:[~;::~]~{~A~^::~}" start names)))))

(defun <scoped_name>-lookup ()
  (let (name)
    (seq (-> (<scoped_name>) name)
         (action (let ((obj (or (op:lookup *container* name)
                                (error "Undefined name: ~A" name))))
                   (if (string= (op:absolute_name obj) "::CORBA::TypeCode")
                     (op:get_primitive *the-repository* :pk_typecode)
                     obj))))))



;;;; Module

(defun <module> (&aux name)
  (seq "module" (-> (<identifier>) name) "{" 
       (let ((*container*
              (or (op:lookup *container* name)
                  (named-create *container* #'op:create_module name))))
         (seq (seq+ (<definition>))
              (action (id-adjustment))))
       "}")) 

(defun id-adjustment ()
  ;; List of forms to adjust IDs of IDL types due to pragma.
  ;; Should be done at the end of a scope.
  (loop for (type name value) in (idl-repositoryid-pragmas *current-cpp*)
        collect (let ((obj (lookup-name-in *container* name)))
                  (ecase type
                    (:id (setf (op:id obj) value))
                    (:version 
                     (setf (op:version obj) value)
                     (let* ((old-id (op:id obj))
                            (last-colon (position #\: old-id :from-end t)))
                       (assert last-colon nil "ill-formed ID")
                       (setf (op:id obj)
                             (concatenate 'string (subseq old-id 0 (1+ last-colon))
                                          value))))))))


;;;; Interface and ValueType

(defun <interface/value> ()
  (let (modifier valuep name)
    (seq 
     (-> (alt (seq "abstract" (action :abstract))
              (seq "local"    (action :local))
              (seq "custom"   (action :custom))
              (seq            (action nil)))
         modifier)
     (alt (seq (not (eq modifier :local))
               "valuetype"    (action (setq valuep t)))
          (seq (not (eq modifier :custom))
               "interface"))
     (-> (<identifier>) name)
     (if valuep
       (<value>- modifier name)
       (<interface>- modifier name)))))

(defun <value>- (modifier name)
  (alt 
   ;; value box, cant be abstract or custom
   (and (null modifier)
        (let (type)
          (seq (-> (<type_spec>) type)
               (action (named-create *container* #'op:create_value_box name type)))))
   ;; general valuetype
   (let ((obj (or (op:lookup *container* name)
                  (named-create *container* #'op:create_value name
                                (eq modifier :custom) (eq modifier :abstract)
                                ;; base_value is_truncatable 
                                nil nil 
                                ;; abstract_base_values supported_interfaces
                                nil nil 
                                ;; initializers
                                nil )))
         base-values supported-interfaces)
     ;; FIXME: check that obj is right type 
     (opt (opt ":" (opt "truncatable" (action (setf (op:is_truncatable obj) t)))
               (-> (parse-list (<scoped_name>-lookup) ",") base-values)
               (action 
                 (unless (op:is_abstract (first base-values))
                   (setf (op:base_value obj) (pop base-values)))
                 (assert (every #'op:is_abstract base-values))
                 (setf (op:abstract_base_values obj) base-values)))
          (opt "supports" (-> (parse-list (<scoped_name>-lookup) ",") supported-interfaces)
               (action (setf (op:supported_interfaces obj) supported-interfaces)))
          "{" 
          (let ((*container* obj))
            (seq* ;; if abstract value type, only export
             (<value_element>)))
          "}"))))


(defun <interface>- (modifier name)
  (let* (bases
         (obj (or (op:lookup *container* name)
                  (named-create *container* 
                                (case modifier
                                  (:abstract #'op:create_abstract_interface)
                                  (:local #'op:create_local_interface)
                                  (otherwise #'op:create_interface))
                                name bases))))
    (opt (seq (opt (-> (<interface_inheritance_spec>) bases)) "{"
              (action (setf (op:base_interfaces obj) bases))
              (let ((*container* obj))
                (seq (seq* (<export>))
                     (action (id-adjustment))))
              "}"))))


(defun <value_element> nil
  (alt (seq (<export>)) 
       (seq (<state_member>))
       (seq (<init_dcl>))))

(defun <state_member> ()
  (let (visibility type dlist)
    (seq (-> (alt (seq "public"  (action corba:public_member))
                  (seq "private" (action corba:private_member)))
             visibility)
         (-> (<type_spec>) type) (-> (<declarators>) dlist) ";"
         (action (loop for (name . array-spec) in dlist
                       do (named-create *container* #'op:create_value_member name
                                        (convert-to-array type array-spec) visibility))))))

(defun <init_dcl> (&aux name args)
  (seq "factory" (-> (<identifier>) name)
       "(" (-> (parse-list (<init_param_decl>) "," t) args) ")" ";"
       (action 
         (setf (op:initializers *container*)
               (nconc (op:initializers *container*)
                      (list (CORBA:Initializer :name name
                                               :members args)))))))

(defun <init_param_decl> (&aux name type)
  (seq "in" 
       (-> (<param_type_spec>) type)
       (-> (<simple_declarator>) name)
       (action (CORBA:StructMember :name name
                                   :type_def type
                                   :type CORBA:tc_void))))



(defun <interface_inheritance_spec> nil
  (seq ":" 
       (parse-list (<scoped_name>-lookup) ",")))

(defun <export> nil
  (alt (seq (<type_dcl>) ";")
       (seq (<const_dcl>) ";")
       (seq (<except_dcl>) ";")
       (seq (<attr_dcl>) ";")
       (seq (<op_dcl>) ";")))




;;;; Operation Declaration

(defun <op_dcl> nil
  (let (name result mode params exceptions contexts)
    (seq (-> (<op_attribute>) mode)
         (-> (<op_type_spec>) result)
         (-> (<identifier>) name)
         (-> (<parameter_dcls>) params)
         (opt (-> (<raises_expr>) exceptions))
         (opt (-> (<context_expr>) contexts))
         (action
           (named-create *container* #'op:create_operation name
                         result mode params exceptions contexts )))))

(defun <op_attribute> nil
  (alt (seq "oneway"  (action :op_oneway))
       (seq (action :op_normal))))

(defun <op_type_spec> nil
 (alt (seq (<param_type_spec>))
      (seq "void" (action (op:get_primitive *the-repository* :pk_void)))))

(defun <parameter_dcls> nil
 (let (list)
   (seq "("
        (-> (parse-list (<param_dcl>) "," t) list)
        ")"
        (action list))))

(defun <param_dcl> nil
  (let (name type-def mode)
    (seq (-> (<param_attribute>) mode)
         (-> (<param_type_spec>) type-def)
         (-> (<simple_declarator>) name)
         (action (CORBA:ParameterDescription 
                  :name name
                  :type_def type-def
                  :type CORBA:tc_void
                  :mode mode )))))

(defun <param_attribute> nil
  (alt (seq "in" (action :param_in))
       (seq "out" (action :param_out))
       (seq "inout" (action :param_inout))))

(defun <param_type_spec> nil
  ;; FIXME: any restrictions ?
  (<simple_type_spec>))


(defun <raises_expr> nil
  (let (list)
    (seq "raises" "(" 
         (-> (parse-list (<scoped_name>-lookup) "," t) list)
         ")"
         (action list))))

(defun <context_expr> nil
  (let (list)
    (seq "context" "(" 
         (-> (parse-list (<string_literal>) "," t) list)
         ")"
         (action list))))



;;;; Attribute Declaration

(defun <attr_dcl> nil
  (let (mode type dlist)
    (seq (-> (alt (seq "readonly" (action :attr_readonly))
                  (seq		  (action :attr_normal)))
             mode)
         "attribute"
         (-> (<param_type_spec>) type)
         (-> (parse-list (<simple_declarator>) ",")
             dlist)
         (action (loop for name in dlist
                       do (named-create *container* #'op:create_attribute name
                                        type mode))))))



;;;; Exception

(defun <except_dcl> (&aux name members)
  (seq "exception" (-> (<identifier>) name)
       "{" 
       (-> (parse-list (<member>) nil t) members)
       "}"
       (action 
         (setq members (apply #'nconc members))
         (named-create *container* #'op:create_exception name
                       members))))



;;;; Type Declarations

(defun <type_dcl> nil
 (alt (seq "typedef" (<type_declarator>))
      (<struct_type>)
      (<union_type>)
      (<enum_type>)
      (seq "native" 
           (let (name)
             (seq (-> (<simple_declarator>) name)
                  (action (named-create *container* #'op:create_native name)))))))


(defun <type_declarator> nil
  (let (type decl)
    (seq (-> (<type_spec>) type)
         (-> (<declarators>) decl)
         (action
           (loop for (name . array-spec) in decl
                 do (named-create *container* #'op:create_alias 
                                  name (convert-to-array type array-spec)))))))


(defun <declarators> nil
  (parse-list (<declarator>) ","))

(defun <declarator> nil
  (let (name array n)
    (seq (-> (<identifier>) name)
         (seq* "[" (-> (<positive_int_const>) n) "]"
               (action (push n array)))
         (action (cons name (nreverse array))))))


(defun <simple_declarator> nil
 (<identifier>))




;;;; Struct

(defun <struct_type> nil
  (let (name members)
    (seq "struct" (-> (<identifier>) name)
         "{"  (-> (parse-list (<member>)) members)  "}"
         (action (named-create *container* #'op:create_struct name
                               (apply #'nconc members))))))

(defun <member> (&aux type dlist)
  (seq (-> (<type_spec>) type)
       (-> (<declarators>) dlist)
       ";"
       (action (loop for (name . array-spec) in dlist
                     collect (corba:StructMember 
                              :name name
                              :type_def (convert-to-array type array-spec)
                              :type CORBA:tc_void )))))


;;;; Union 

(defun <union_type> nil
  (let (name discriminator_type members)
    (seq "union" (-> (<identifier>) name)
         "switch" "(" (-> (<switch_type_spec>) discriminator_type) ")"
         "{" (-> (<switch_body> (op:type discriminator_type)) members) "}"
         (action (named-create *container* #'op:create_union name
                               discriminator_type members )))))


(defun <switch_type_spec> nil
  (<simple_type_spec> :allow-kind '(:tk_short :tk_ushort :tk_long :tk_ulong :tk_longlong :tk_ulonglong
                                    :tk_boolean :tk_char :tk_enum )))

(defun <switch_body> (disc-type)
  (let (list)
    (seq (-> (parse-list (<case> disc-type)) list)
         (action (apply #'nconc list)))))

(defun <case> (disc-type)
  (let (labels element)
    (seq (-> (parse-list (<case_label>)) labels)
         (-> (<element_spec>) element)
         ";"
         (action (loop for label in labels
                       collect (CORBA:UnionMember 
                                :name (car element)
                                :label (if (eq label 'default)
                                         (CORBA:Any :any-value 0
                                                    :any-typecode CORBA:tc_octet)
                                         (CORBA:Any :any-value label
                                                    :any-typecode disc-type))
                                :type_def (cdr element)))))))

(defun <element_spec> nil
  (let (name type)
    (seq (-> (<type_spec>) type)
         (-> (<declarator>) name)
         (action (cons (car name)
                       (convert-to-array type (cdr name)))))))

(defun <case_label> nil
  (let ((value 'default))
    (seq (alt (seq "case" (-> (<const_exp>) value) ":")
              (seq "default" ":"))
         (action value))))



;;;; Enum

(defun <enum_type> nil
  (let (name members)
    (seq "enum"
         (-> (<identifier>) name)
         "{" (-> (parse-list (<identifier>) ",") members) "}"
         (action
           (let ((enum (named-create *container* #'op:create_enum name members)))
             ;; introduce the members as constants in current scope
             (loop for cname in members 
                   do (named-create *container* #'op:create_constant cname enum 
                                    (corba:any :any-value (key cname)
                                               :any-typecode (op:type enum)))))))))



;;;; Litterals

(defun <literal> nil
  (alt (seq (<number_literal>))
       (seq (<string_literal>))
       (seq (<boolean_literal>))
       (seq (<character_literal>))
       ;; not implemented yet:
       (seq (<wide_string_literal>))
       (seq (<wide_character_literal>))))

(defun <boolean_literal> nil
  (alt (seq "TRUE" 	(action t))
       (seq "FALSE"	(action nil))))



;;;; Constant Declaration


(defun <const_dcl> (&aux type name value)
  (seq "const" 
       (-> (<const_type>) type)
       (-> (<identifier>) name)
       "=" 
       (-> (<const_exp>) value)
       (action
         (cond ((equal type "fixed")
                (unless (idl-fixed-p value)
                  (error "A fixed literal needed: ~A" value))
                (multiple-value-bind (digits scale number) (idl-fixed-values value)
                  (setq type (op:create_fixed *the-repository* digits scale))
                  (setq value (rationalize number)))))
         (named-create *container* #'op:create_constant name
                       type 
                       (corba:any :any-typecode (op:type type)
                                  :any-value value)))))

(defun <const_type> ()
  ;; The <scoped_name> in the <const_type> production must be a previously defined
  ;; name of an <integer_type>, <char_type>, <wide_char_type>, <boolean_type>,
  ;; <floating_pt_type>, <string_type>, <wide_string_type>, <octet_type>, or
  ;; <enum_type> constant.
  (alt
   (seq "fixed")
   (<simple_type_spec>
    :allow-kind '(:tk_short :tk_ushort 
                  :tk_long :tk_ulong 
                  :tk_longlong :tk_ulonglong
                  :tk_char :tk_wchar :tk_boolean
                  :tk_float :tk_double :tk_longdouble
                  :tk_string :tk_wstring :tk_octet 
                  :tk_enum))))



;;;; Expressions

(defun <const_exp> nil
 (<or_expr>))

(defun <positive_int_const> nil
 (<const_exp>))

(defun <primary_expr> (&aux value)
  (alt (<literal>) 
       (let (obj)
         (seq (-> (<scoped_name>-lookup) obj)
              (action (unless (eql (op:def_kind obj) :dk_constant)
                        (error "Identifier '~A' should be a constant" (op:name obj)))
                      (let ((tc (op:type obj)))
                        (if (eq :tk_fixed (op:kind tc))
                          (make-idl-fixed (op:fixed_digits tc)
                                          (op:fixed_scale tc)
                                          (any-value (op:value obj)))
                          (any-value (op:value obj)))))))              
       (seq "(" (-> (<const_exp>) value) ")" (action value))))

(defun <or_expr> nil
  (let (n1 n2)
    (seq (-> (<xor_expr>) n1)
         (seq* "|" (-> (<xor_expr>) n2)
               (action (setq n1 (logior n1 n2))))
         (action n1))))

(defun <xor_expr> nil
  (let (x y)
    (seq (-> (<and_expr>) x)
         (seq* "^" (-> (<and_expr>) y) (action (setq x (logxor x y))))
         (action x))))

(defun <and_expr> nil
  (let (x y)
    (seq (-> (<shift_expr>) x)
         (seq* (seq "&" (-> (<shift_expr>) y) (action (setq x (logand x y)))))
         (action x))))

(defun <shift_expr> nil
  (let (x y)
    (seq (-> (<add_expr>) x)
         (seq* (alt (seq ">>" (-> (<add_expr>) y) (action (setq x (>> x y))))
                    (seq "<<" (-> (<add_expr>) y) (action (setq x (<< x y))))))
         (action x))))


(defmethod add ((n1 cons) (n2 cons))
  ;; Assume fixed
  (multiple-value-bind (d1 s1 m1) (idl-fixed-values n1)
    (multiple-value-bind (d2 s2 m2) (idl-fixed-values n2)
      (make-idl-fixed (+ (max (- d1 s1) (- d2 s2)) (max s1 s2) 1)
                      (max s1 s2)
                      (+ m1 m2)))))
  
(defmethod add (n1 n2)
  (+ n1 n2))

(defmethod sub ((n1 cons) (n2 cons))
  (multiple-value-bind (d1 s1 m1) (idl-fixed-values n1)
    (multiple-value-bind (d2 s2 m2) (idl-fixed-values n2)
      (make-idl-fixed (+ (max (- d1 s1) (- d2 s2)) (max s1 s2) 1)
                      (max s1 s2)
                      (- m1 m2)))))

(defmethod sub (n1 n2)
  (- n1 n2))


(defmethod mul ((n1 cons) (n2 cons))
  (multiple-value-bind (d1 s1 m1) (idl-fixed-values n1)
    (multiple-value-bind (d2 s2 m2) (idl-fixed-values n2)
      (make-idl-fixed (+ d1 d2)
                      (+ s1 s2)
                      (* m1 m2)))) )

(defmethod mul (n1 n2)
  (* n1 n2))



(defmethod div ((n1 cons) (n2 cons))
  (let ((sinf 9999999))
    (multiple-value-bind (d1 s1 m1) (idl-fixed-values n1)
      (multiple-value-bind (d2 s2 m2) (idl-fixed-values n2)
        (declare (ignore d2))
        (make-idl-fixed (+ d1 (- s1) s2 sinf)
                        sinf
                        (/ m1 m2)))) ))

(defmethod div (n1 n2)
  (/ n1 n2))


(defun <add_expr> nil
  (let (x y)
    (seq (-> (<mult_expr>) x)
         (seq* (alt (seq "+" (-> (<mult_expr>) y) (action (setq x (add x y))))
                    (seq "-" (-> (<mult_expr>) y) (action (setq x (sub x y))))))
         (action x))))

(defun <mult_expr> nil
  (let (x y)
    (seq (-> (<unary_expr>) x)
         (seq* (alt (seq "*" (-> (<unary_expr>) y) (action (setq x (mul x y))))
                    (seq "/" (-> (<unary_expr>) y) (action (setq x (div x y))))
                    (seq "%" (-> (<unary_expr>) y) (action (setq x (rem x y))))))
         (action x))))

(defun <unary_expr> nil
  (let (n op)
    (alt (seq (-> (<unary_operator>) op) (-> (<primary_expr>) n)
              (action (cond ((equal op "-") (- n))
                            ((equal op "~") (lognot n))
                            (t n))))
         (seq (<primary_expr>)))))

(defun <unary_operator> nil
  (alt "-" "+" "~"))



;;;; Type Specifications

(defun <type_spec> nil
  (alt (<simple_type_spec>) 
       (<constr_type_spec>)))

(defun <simple_type_spec> (&key allow-kind disallow-kind)
  (let (type-def)
    (seq
     (-> (alt (seq (<base_type_spec>))
              (seq (<sequence_type>))
              (seq (<string_type>))
              (seq (<wide_string_type>))
              (seq (<fixed_pt_type>))
              (<scoped_name>-lookup)) 
         type-def)
     (action
       (check-type type-def CORBA:IDLType)
       (let ((type-def (if (eql (op:def_kind type-def) :dk_alias)
                         (op:original_type_def type-def)
                         type-def)))
         (when allow-kind
           (unless (member (op:kind (op:type type-def)) allow-kind)
             (warn "Type ~A not allowed, allowed: ~S" type-def allow-kind)))
         (when disallow-kind
           (when (member (op:kind (op:type type-def)) disallow-kind)
             (warn "Type ~A not allowed, forbidden types: ~S" type-def disallow-kind))))
       type-def))))

(defun <integer_type> nil
 (<number_type>))


(defun <sequence_type> (&aux element-type bound)
  (seq "sequence"
       "<"
       (-> (<simple_type_spec>) element-type)
       (opt (seq "," (-> (<positive_int_const>) bound)))
       ">"
       (action (op:create_sequence *the-repository* (or bound 0) element-type))))

(defun <string_type> nil
  (seq "string" 
       (alt (let (len)
              (seq "<" (-> (<positive_int_const>) len) ">"
                   (action (op:create_string *the-repository* len))))
            (seq (action (op:get_primitive *the-repository* :pk_string))))))

(defun <wide_string_type> nil
  (seq "wstring"
       (alt (let (len)
              (seq "<" (-> (<positive_int_const>) len) ">" 
                   (action (op:create_wstring *the-repository* len))))
            (seq (action (op:get_primitive *the-repository* :pk_wstring))))))

(defun <fixed_pt_type> (&aux digits scale)
  (seq "fixed" "<" 
       (-> (<positive_int_const>) digits) "," (-> (<positive_int_const>) scale)
       ">"
       (action (op:create_fixed *the-repository* digits scale))))



(defun <number_type> (&aux pk)
  (seq (-> (alt (seq "float"			(action :pk_float))
                (seq "double"			(action :pk_double))
                (seq "short"			(action :pk_short))
                (seq "long" (alt (seq "double"	(action :pk_longdouble))
                                 (seq "long" 	(action :pk_longlong)) 
                                 (seq 		(action :pk_long) )))
                (seq "unsigned"
                     (alt (seq "short" 		(action :pk_ushort)) 
                          (seq "long"
                               (alt (seq "long" (action :pk_ulonglong)) 
                                    (seq	(action :pk_ulong)))))))
           pk)
       (action (op:get_primitive *the-repository* pk))))

(defun <base_type_spec> nil
  (alt (<number_type>)
       (misc-type)
       (seq "any"	(action (op:get_primitive *the-repository* :pk_any)))
       (seq "Object"	(action (op:get_primitive *the-repository* :pk_objref)))
       (seq "ValueBase" (action (op:get_primitive *the-repository* :pk_value_base)))))

(defun misc-type (&aux pk)
  (seq (-> (alt (seq "char"	(action :pk_char))
                (seq "wchar"	(action :pk_wchar))
                (seq "octet"	(action :pk_octet))
                (seq "boolean"	(action :pk_boolean))) pk)
       (action (op:get_primitive *the-repository* pk))))

(defun <constr_type_spec> nil
  (alt (seq (<struct_type>)) (seq (<union_type>)) (seq (<enum_type>))))



;;;; primitive tokens

(defparameter *reserved-words*
  '("abstract" "double" "local" "raises" "typedef" "any" "exception"
    "long" "readonly" "unsigned" "attribute" "enum" "module" "sequence" "union" "boolean" "factory"
    "native" "short" "ValueBase" "case" "FALSE" "Object" "string" "valuetype" "char" "fixed" "octet"
    "struct" "void" "const" "float" "oneway" "supports" "wchar" "context" "in" "out" "switch"
    "wstring" "custom" "inout" "private" "TRUE" "default" "interface" "public" "truncatable" ) )


(defun <identifier> ()
  (let ((tok (token *lexer*)))
    (cond ((and (stringp tok)
                (or (alpha-char-p (char tok 0))
                    (eql #\_ (char tok 0)))
                (not (member tok *reserved-words* :test #'string=)))
           (match-token *lexer* tok)
           (values t
                   (if (eql #\_ (char tok 0))
                       (subseq tok 1)
                       tok)))
          (t
           ;; Bit of a trick, there is no <identifier> token from
           ;; the lexer, but this will handled the error reporting.
           (match-token *lexer* '<identifier>)))))


(defun <number_literal> ()
  (alt #'numberp #'idl-fixed-p))

(defun <character_literal> ()
  (seq #'characterp))

(defun <string_literal> ()
  (let (s)
    (seq (-> #'(lambda (tok) (and (consp tok) (eq (car tok) 'string))) s)
         (action (cdr s)))))


(defun <wide_string_literal> ()
  ;; FIXME: check how this works
  nil)

(defun <wide_character_literal> ()
  nil)



;;;; Parser Class


(defclass MY-IDLPARSER (idl-compiler)
  ())

(defmethod load-repository ((self my-idlparser) repository file)
    (let* ((*the-repository* repository)
           (*container* *the-repository*))
      (using-idllex file #'<specification>
                    (include-directories self)
                    (defines self))))


(unless *default-idl-compiler*
  (setq *default-idl-compiler* 
        (make-instance 'my-idlparser
          :defines '("_CLORB"))))


#|
(with-input-from-string (s "TRUE 
   }; ")
  (let* ((*the-repository* (make-instance 'repository))
         (*container* *the-repository*))
    (let ((*lexer* (make-idllex s)))
      (next-token *lexer*)
      (<const_exp>)
   )))
|#
