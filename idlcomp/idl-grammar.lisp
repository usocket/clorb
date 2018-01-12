;;/*
;; *  MICO --- a free CORBA implementation
;; *  Copyright (C) 1997-98 Kay Roemer & Arno Puder
;; *
;; *  This program is free software; you can redistribute it and/or modify
;; *  it under the terms of the GNU General Public License as published by
;; *  the Free Software Foundation; either version 2 of the License, or
;; *  (at your option) any later version.
;; *
;; *  This program is distributed in the hope that it will be useful,
;; *  but WITHOUT ANY WARRANTY; without even the implied warranty of
;; *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; *  GNU General Public License for more details.
;; *
;; *  You should have received a copy of the GNU General Public License
;; *  along with this program; if not, write to the Free Software
;; *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;; *
;; *  Send comments and/or bug reports to:
;; *                 mico@informatik.uni-frankfurt.de
;; */

(defvar *rules*)
(defvar *tokens*)


(setf *tokens*
      '(T_AMPERSAND
	T_ANY
	T_ASTERISK
	T_ATTRIBUTE
	T_BOOLEAN
	T_CASE
	T_CHAR
	T_CHARACTER_LITERAL
	T_CIRCUMFLEX
	T_COLON
	T_COMMA
	T_CONST
	T_CONTEXT
	T_DEFAULT
	T_DOUBLE
	T_ENUM
	T_EQUAL
	T_EXCEPTION
	T_FALSE
	T_FIXED
	T_FIXED_PT_LITERAL
	T_FLOAT
	T_FLOATING_PT_LITERAL
	T_GREATER_THAN_SIGN
	T_IDENTIFIER
	T_IN
	T_INOUT
	T_INTEGER_LITERAL
	T_INTERFACE
	T_LEFT_CURLY_BRACKET
	T_LEFT_PARANTHESIS
	T_LEFT_SQUARE_BRACKET
	T_LESS_THAN_SIGN
	T_LONG
	T_MINUS_SIGN
	T_MODULE
	T_OCTET
	T_ONEWAY
	T_OUT
	T_PERCENT_SIGN
	T_PLUS_SIGN
	T_PRINCIPAL
	T_RAISES
	T_READONLY
	T_RIGHT_CURLY_BRACKET
	T_RIGHT_PARANTHESIS
	T_RIGHT_SQUARE_BRACKET
	T_SCOPE
	T_SEMICOLON
	T_SEQUENCE
	T_SHIFTLEFT
	T_SHIFTRIGHT
	T_SHORT
	T_SOLIDUS
	T_STRING
	T_STRING_LITERAL
	T_PRAGMA
	T_STRUCT
	T_SWITCH
	T_TILDE
	T_TRUE
	T_OBJECT
	T_TYPEDEF
	T_UNION
	T_UNSIGNED
	T_VERTICAL_LINE
	T_VOID
	T_WCHAR
	T_WSTRING
	T_UNKNOWN
	T_ABSTRACT
	T_VALUETYPE
	T_TRUNCATABLE
	T_SUPPORTS
	T_CUSTOM
	T_PUBLIC
	T_PRIVATE
	T_FACTORY
	T_NATIVE
	T_VALUEBASE
	t_with-prefix))


;;%%
(setf *rules*
      `((1 specification
	   
	   ;((definitions) identity)
	     ; have to deal with prefix pragma, make top level definitions differnt from other definitions
	   ((top-definitions) identity)
	   (() list))

	(1x top-definitions
	    ((top-definition) identity)
	    ((top-definition top-definitions) append))

	(1y top-definition
	    ((definition) (lambda (d) 
                            (let ((prefix (idl-prefix *current-cpp*)))
                              (if (and prefix (not (equal prefix "")))
                                `((with-prefix ,prefix ,@d))
                                d)))))
	    
	(1a definitions
	    ((definition) identity) 
	    ((definition definitions) append))
    
	(2 definition  ; type_dcl returns a list of typedcl "typedef string bla,blu,blub;" is possible, makes a list 
	   (( type_dcl T_SEMICOLON) (lambda (x y) x))
	   (( const_dcl T_SEMICOLON)  (lambda (x y) (list x)))
	   (( except_dcl T_SEMICOLON)  (lambda (x y) (list x)))
	   (( interface T_SEMICOLON)  (lambda (x y) (list x)))
	   (( module T_SEMICOLON)  (lambda (x y)  (list x)))
	   (( value T_SEMICOLON)  (lambda (x y) (list x))))
					;
    
	(3 module
	   (( T_MODULE T_IDENTIFIER T_LEFT_CURLY_BRACKET
		       definitions T_RIGHT_CURLY_BRACKET)
	    (lambda (m i l d r) `(define-module ,i nil ,@d
                                   ,@(id-adjustment)))))

	(4 interface
	   (( interface_dcl) identity)
	   (( forward_dcl) identity))
					;

	(5 interface_dcl
	   (( interface_header T_LEFT_CURLY_BRACKET interface_body
			       T_RIGHT_CURLY_BRACKET)
	    (lambda (h po body pc) 
              (append h body (id-adjustment)))))
					;

	(6  forward_dcl
	    (( T_INTERFACE T_IDENTIFIER) (lambda (x y) (list 'define-interface y '(:bases nil))))                    ;(list 'forward (car y))))
	    (( T_ABSTRACT T_INTERFACE T_IDENTIFIER) (lambda (x y z) (list 'abstract-forward y))))
					;

	(7 interface_header
	   (( T_INTERFACE T_IDENTIFIER) (lambda (x y) (list 'define-interface y '(:bases nil))))
	   (( T_INTERFACE T_IDENTIFIER interface_inheritance_spec) (lambda (x y s) (list 'define-interface y (list :bases s))))
	   (( T_ABSTRACT T_INTERFACE T_IDENTIFIER)  (lambda (x y) (list 'define-abstract-interface y :bases nil)))
	   (( T_ABSTRACT T_INTERFACE T_IDENTIFIER interface_inheritance_spec) (lambda (x y s) (list 'define-abstract-interface y :bases s))))
					;  

	(8 interface_body
	   (( exports) identity)
	   (()  (lambda () nil)))
					;

	(8a exports
	    (( export) identity)
	    (( export exports) append))
					;

	(9 export
	   (( type_dcl T_SEMICOLON) (lambda (x y) x))
	   (( const_dcl T_SEMICOLON )(lambda (x y) (list x)))
	   (( except_dcl T_SEMICOLON)(lambda (x y) (list x)))
	   (( attr_dcl T_SEMICOLON)(lambda (x y) (list x)))
	   (( op_dcl T_SEMICOLON )(lambda (x y) (list x))))
					;

	(10  interface_inheritance_spec
	     (( T_COLON interface_names) (lambda (x y) y)))
					;

	(10a interface_names
	     (( scoped_names) identity))
					;

	(10b scoped_names
	     (( scoped_name) list)
	     (( scoped_name T_COMMA scoped_names) (lambda (x y z) (cons x z))))
					;

	(11 interface_name
	    (( scoped_name) identity))

	(11a scoped_name
	 ((scoped_name1) id->string))    ;included by ra: turn an scoped_name into a string again
					;

	(12 scoped_name1
	    (( T_IDENTIFIER) (lambda (x) (list 'no-colon x)))
	    (( T_SCOPE T_IDENTIFIER) (lambda (x y) (list 'colon y)))
	    (( scoped_name1 T_SCOPE T_IDENTIFIER) (lambda (x y z) (append x (list z)))))
					;

	(13 value
	    (( value_dcl) identity)
	    (( value_abs_dcl) identity)
	    (( value_box_dcl) identity)
	    (( value_forward_dcl) identity))
					;

	(14 value_forward_dcl
	    (( T_VALUETYPE T_IDENTIFIER) (lambda (x y) (list 'forward-value y)))
	    (( T_ABSTRACT T_VALUETYPE T_IDENTIFIER) (lambda (x y) (list 'abstract-forward-value y))))
					;

	(15 value_box_dcl
	    (( T_VALUETYPE T_IDENTIFIER type_spec)
	     (lambda (to id spec) (list 'value-box-dcl id spec))))
					;

	(16 value_abs_dcl
	    (( T_ABSTRACT T_VALUETYPE T_IDENTIFIER
			  T_LEFT_CURLY_BRACKET value_body T_RIGHT_CURLY_BRACKET)
	     (lambda (ta ty id bo  body bc) (list 'abstract-value id nil body ))) 
	    (( T_ABSTRACT T_VALUETYPE T_IDENTIFIER value_inheritance_spec
			  T_LEFT_CURLY_BRACKET value_body T_RIGHT_CURLY_BRACKET)
	     (lambda (ta ty id inh bo  body bc) (list 'abstract-value id inh body )) ))
					;

	(16a value_body
					;empty
	     (( exports) identity)
	     (() list))
					;

	(17 value_dcl
	    (( value_header T_LEFT_CURLY_BRACKET value_elements
			    T_RIGHT_CURLY_BRACKET)
	     (lambda (he bo el bc) (list 'value-dcl he el)))
	    (( value_header T_LEFT_CURLY_BRACKET T_RIGHT_CURLY_BRACKET)
	     (lambda (he bo  bc) (list 'value-dcl he nil))))
					;

	(17a value_elements
	     (( value_element) list)
	     (( value_element value_elements) cons))
					;

	(18 value_header
	    (( T_VALUETYPE T_IDENTIFIER value_inheritance_spec)
	     (lambda (ty id inh) (list id inh))) 
	    (( T_CUSTOM T_VALUETYPE T_IDENTIFIER value_inheritance_spec)
	     (lambda (cu tu id inh) (list id inh)))
	    (( T_VALUETYPE T_IDENTIFIER) (lambda (ty id) (list id nil)))
	    (( T_CUSTOM T_VALUETYPE T_IDENTIFIER)
	     (lambda (cu ty id) (list id nil))))
					;

	(19 value_inheritance_spec
	    (( T_COLON value_inheritance_bases))
	    (( T_COLON value_inheritance_bases T_SUPPORTS interface_names))
	    (( T_SUPPORTS interface_names)))
					;

	(19a value_inheritance_bases
	     (( value_name) list)
	     (( value_name T_COMMA value_names) (lambda (x y z) (cons x z)))
	     (( T_TRUNCATABLE value_name) (lambda (x y) (list 'truncatable y)))
	     (( T_TRUNCATABLE value_name T_COMMA value_names) (lambda (tok v c vs) (list 'truncatable (cons v vs)))))
					;

	(19b value_names
	     (( scoped_names) identity))
					;

	(20 value_name
	    (( scoped_name) identity))
					;

	(21 value_element
	    (( export) identity)
	    (( state_member) identity)
	    (( init_dcl) identity))
					;

	(22 state_member
	    (( T_PUBLIC type_spec declarators T_SEMICOLON) (lambda (pu spec dec se) (list 'public spec dec)))
	    (( T_PRIVATE type_spec declarators T_SEMICOLON) (lambda (pu spec dec se) (list 'private spec dec))))
					;

	(23 init_dcl
	    (( T_FACTORY T_IDENTIFIER
			 T_LEFT_PARANTHESIS init_param_decls T_RIGHT_PARANTHESIS
			 T_SEMICOLON)
	     (lambda (fac id bo decl bc sc) (list 'factory id decl))))
					;

	(24 init_param_decls
	    (( init_param_decl) list)
	    (( init_param_decl T_COMMA init_param_decls) (lambda (x y z) (cons x z))))
					;

	(25 init_param_decl
	    (( init_param_attribute param_type_spec simple_declarator)
	     list))
					;

	(26 init_param_attribute
	    (( T_IN) (lambda (x) 'in)))
					;

	(27 const_dcl
	    (( T_CONST const_type T_IDENTIFIER T_EQUAL const_exp) 
             (lambda (t_const typ id t_equal expr) 
               (list 'define-constant id typ expr))))  
					;

	(28 const_type
	    (( integer_type) identity)
	    (( char_type) identity)
	    (( wide_char_type) identity)
	    (( boolean_type) identity)
	    (( floating_pt_type) identity)
	    (( string_type) identity)
	    (( wide_string_type) identity)
	    (( fixed_pt_const_type) identity)
	    (( scoped_name) identity)
	    (( octet_type) identity))
					;

	(29 const_exp
	    (( or_expr) identity))
					;

	(30 or_expr
	    (( xor_expr) identity)
            ;; lenst: fixed obvb
	    (( or_expr T_VERTICAL_LINE xor_expr) (lambda (a b c) `(or ,a ,c))))
					;

	(31 xor_expr
	    (( and_expr) identity)
            ;; lenst: fixed obvb
	    (( xor_expr T_CIRCUMFLEX and_expr) (lambda (a b c) `(xor ,a ,c))))
					;

	(32 and_expr
	    (( shift_expr) identity)
	    (( and_expr T_AMPERSAND shift_expr) (lambda (a b c) `(and ,a ,c))))
					;

	(33 shift_expr
	    (( add_expr) identity)
	    (( shift_expr T_SHIFTRIGHT add_expr)   (lambda (a b c) `(>> ,a ,c)))                 
	    (( shift_expr T_SHIFTLEFT add_expr)    (lambda (a b c) `(<< ,a ,c))))
					;

	(34 add_expr
	    (( mult_expr) identity)
	    (( add_expr T_PLUS_SIGN mult_expr)     (lambda (a b c) `(+ ,a ,c)))
	    (( add_expr T_MINUS_SIGN mult_expr)    (lambda (a b c) `(- ,a ,c))))
					;

	(35 mult_expr
	    (( unary_expr) identity)
	    (( mult_expr T_ASTERISK unary_expr)    (lambda (a b c) `(* ,a ,c)))
	    (( mult_expr T_SOLIDUS unary_expr)     (lambda (a b c) `(/ ,a ,c)))
	    (( mult_expr T_PERCENT_SIGN unary_expr)(lambda (a b c) `(% ,a ,c))))
					;


	(37 unary_expr
	    (( T_MINUS_SIGN primary_expr)   (lambda (a b) `(- ,b)))
	    (( T_PLUS_SIGN primary_expr)   (lambda (a b) `(+ ,b)))
	    (( T_TILDE primary_expr)       (lambda (a b) `(~ ,b)))
	    (( primary_expr) identity))
					;

	(38 primary_expr
	    (( scoped_name) identity)
	    (( literal) identity)
	    (( T_LEFT_PARANTHESIS const_exp T_RIGHT_PARANTHESIS) identity))
					;


	(40 literal
	    (( T_INTEGER_LITERAL)  string->integer)
	    (( T_string_literal)   string->string)
	    (( T_CHARACTER_LITERAL) string->char )
	    (( T_FIXED_PT_LITERAL)  string->string)
	    (( T_FLOATING_PT_LITERAL) string->double)
	    (( T_TRUE )      (lambda (x) t))		;boolean_literal 
	    (( T_FALSE )     (lambda (x) nil)))		;boolean_literal 
					;

	(41 positive_int_const
	    (( const_exp) identity))
					;


	(43 type_dcl
	    (( T_TYPEDEF type_spec declarators ) 
             (lambda (x y z) (mapcar (lambda (x) (cons 'define-type x))
                                     (expand-declarators y z))))
	    (( struct_type) list)
	    (( union_type) list)
	    (( enum_type) list)
	    (( T_NATIVE simple_declarator) (lambda (x y) (list (list 'native y)))))
					;

	(44 type_spec
	    (( simple_type_spec) identity)
	    (( constr_type_spec ) identity))
					;

	(45 simple_type_spec
	    (( base_type_spec) identity)
	    (( template_type_spec) identity)
	    (( scoped_name) identity))
					;

	(46 base_type_spec
	    (( floating_pt_type) identity)
	    (( integer_type) identity)
	    (( char_type) identity)
	    (( wide_char_type) identity)
	    (( boolean_type) identity)
	    (( octet_type) identity)
	    (( any_type) identity)
	    (( object_type) identity)
	    (( value_base_type) identity)
	    (( principal_type) identity)) ;   New 
					;

	(47 template_type_spec
	    (( sequence_type) identity)
	    (( string_type) identity)
	    (( wide_string_type) identity)
	    (( fixed_pt_type) identity))
					;

	(48 constr_type_spec
	    (( struct_type) identity)
	    (( union_type) identity)
	    (( enum_type) identity))
					;

	(49 declarators
	    (( declarator) list) 
	    (( declarator T_COMMA declarators) (lambda (x y z) (cons x z))))
					;

	(50 declarator
	    (( simple_declarator) identity)
	    (( complex_declarator) identity))
					;

	(51 simple_declarator
	    (( T_IDENTIFIER) identity))
					;

	(52 complex_declarator
	    (( array_declarator) identity))
					;

	(53 floating_pt_type
	    (( T_FLOAT) (lambda (x) 'float))
	    (( T_DOUBLE) (lambda (x) 'double))
	    (( T_LONG T_DOUBLE) (lambda (x y) 'longdouble)))
					;

	(54 integer_type
	    (( signed_int) identity)
	    (( unsigned_int) identity))
					;

	(55 signed_int
	    (( signed_long_int) identity)
	    (( signed_short_int) identity)
	    (( signed_longlong_int) identity))
					;

	(56 signed_short_int
	    (( T_SHORT) (lambda (x) 'short)))
					;

	(57 signed_long_int
	    (( T_LONG ) (lambda (x) 'long)))
					;

	(58 signed_longlong_int
	    (( T_LONG T_LONG) (lambda (x y) 'longlong)))
					;

	(59 unsigned_int
	    (( T_UNSIGNED T_LONG )        (lambda (u x) 'ulong))
	    (( T_UNSIGNED T_SHORT )       (lambda (u x) 'ushort))
	    (( T_UNSIGNED T_LONG T_LONG ) (lambda (u x y) 'ulonglong)))
					;
#|
	(60 unsigned_short_int
	    (( T_UNSIGNED T_SHORT)))
	(61 unsigned_long_int
	    (( T_UNSIGNED T_LONG)))
	(62 unsigned_longlong_int
	    (( T_UNSIGNED T_LONG T_LONG)))
|#

	(63 char_type
	    (( T_CHAR) (lambda (x) 'char)))
					;

	(64 wide_char_type
	    (( T_WCHAR)(lambda (x) 'wchar)))
					;

	(65 boolean_type
	    (( T_BOOLEAN) (lambda (x) 'boolean)))
					;

	(66 octet_type
	    (( T_OCTET) (lambda (x) 'octet)))
					;

	(67 any_type
	    (( T_ANY) (lambda (x) 'any)))
					;

	(68 object_type
	    (( T_OBJECT) (lambda (x) 'object)))
					;

	(69 struct_type
	    (( T_STRUCT T_IDENTIFIER T_LEFT_CURLY_BRACKET member_list 
			T_RIGHT_CURLY_BRACKET)
	     (lambda (ts id bo mem bc) (list 'define-struct id mem))))
					;

	(70 member_list
	    (( member) identity)
	    (( member member_list) append))
					;

	(71 member
	    ((type_spec declarators T_SEMICOLON)
	     (lambda (x y z) 
               (expand-declarators x y))))
					;

	(72 union_type
	    (( T_UNION T_IDENTIFIER T_SWITCH T_LEFT_PARANTHESIS
		       switch_type_spec T_RIGHT_PARANTHESIS T_LEFT_CURLY_BRACKET
		       switch_body T_RIGHT_CURLY_BRACKET)
             (lambda (t1 name t2 t3 disc-type t4 t5 body t6)
               (declare (ignore t1 t2 t3 t4 t5 t6))
               `(define-union ,name ,disc-type ,body))))
					; 

	(73 switch_type_spec
	    (( integer_type) identity)
	    (( char_type) identity)
	    (( boolean_type) identity)
	    (( enum_type) identity)
	    (( scoped_name) identity))
					;

	(74 switch_body
	    (( case) identity)
	    (( case switch_body) (lambda (x y) (append x y))))
					;

	(75 case	; let case return list of (label name type) tripplets
	    (( case_label case) 
             (lambda (label case) 
               (cons (cons label (cdr (car case)))
                     case )))
	    (( case_label element_spec T_SEMICOLON) 
             (lambda (label element t1)
               (declare (ignore t1))
               (list (list* label element))))
            ) 
					;

	(76 case_label
	    (( T_CASE const_exp T_COLON ) (lambda (t1 x t2) (declare (ignore t1 t2)) x))
	    (( T_DEFAULT T_COLON) (lambda (t1 t2) 'default)))
					;

	(77 element_spec
	    (( type_spec declarator) (lambda (type name) (list name type))))
					;

	(78 enum_type
	    (( T_ENUM T_IDENTIFIER T_LEFT_CURLY_BRACKET enumerators
		      T_RIGHT_CURLY_BRACKET)
	     (lambda (te id bo enums bc) (list 'define-enum id enums :expand t))))
					;

	(78a enumerators
	     (( enumerator) list)
	     (( enumerator T_COMMA enumerators) (lambda (x y z) (cons x z))))
					;

	(79 enumerator
	    (( T_IDENTIFIER) identity))
					;

	(80 sequence_type
	    (( T_SEQUENCE T_LESS_THAN_SIGN simple_type_spec T_COMMA
			  positive_int_const T_GREATER_THAN_SIGN)
	     (lambda (ts ls spec co int gs) (list 'sequence spec int)))
	    (( T_SEQUENCE T_LESS_THAN_SIGN simple_type_spec T_GREATER_THAN_SIGN)
	     (lambda (ts ls spec gs) (list 'sequence spec nil))))
					;

	(81 string_type
	    (( T_STRING T_LESS_THAN_SIGN positive_int_const T_GREATER_THAN_SIGN)
	     (lambda (ts ls int gs) (list 'string int)))
	    (( T_STRING) (lambda (x) 'string)))
					;

	(82 wide_string_type
	    (( T_WSTRING T_LESS_THAN_SIGN positive_int_const T_GREATER_THAN_SIGN)
	     (lambda (ts ls int gs) (list 'wstring int)))
	    (( T_WSTRING)(lambda (x) (list 'wstring 0))))
					;

	(83 array_declarator
	    (( T_IDENTIFIER fixed_array_sizes) 
             (lambda (name sizes) 
               (cons name sizes ))))
					;

	(83a fixed_array_sizes
	     (( fixed_array_size) list)
	     (( fixed_array_size fixed_array_sizes) cons))
					;

	(84 fixed_array_size
	    (( T_LEFT_SQUARE_BRACKET positive_int_const T_RIGHT_SQUARE_BRACKET)
	     (lambda (x y z) y)))
					;

	(85 attr_dcl
	    (( T_ATTRIBUTE param_type_spec simple_declarators) (lambda (x y z) (list 'define-attribute (car z) y)))  
	    (( T_READONLY T_ATTRIBUTE param_type_spec simple_declarators) (lambda (ro x y z)    (list 'define-attribute (car z) y :readonly t))))
					; 

	(85a simple_declarators
	     (( simple_declarator) list)
	     (( simple_declarator T_COMMA simple_declarators) (lambda (x y z) (cons x z))))
					;

	(86 except_dcl
	    (( T_EXCEPTION T_IDENTIFIER T_LEFT_CURLY_BRACKET members
			   T_RIGHT_CURLY_BRACKET)
	     (lambda (te id lcb mem rcb) (list 'define-exception id mem))))
					;

	(86a members
	     (() list)			;empty 
	     (( member members) append))
					;

	(87 op_dcl
	    (( op_attribute op_type_spec T_IDENTIFIER parameter_dcls
			    raises_expr context_expr)
	     (lambda (op-a op-ty id para raise con)
	       (list 'define-operation  id para 
		     :result-type op-ty  :exceptions raise ;:context con
                     :mode op-a ))))
					;

	(88 op_attribute
	    (( ) (lambda () :op_normal))
	    (( T_ONEWAY) (lambda (x) :op_oneway)))
					;

	(89 op_type_spec
	    (( param_type_spec) identity)
	    (( T_VOID) (lambda (x) 'void)))
					;

	(90 parameter_dcls
	    (( T_LEFT_PARANTHESIS param_dcls T_RIGHT_PARANTHESIS) (lambda (x y z) y))
	    (( T_LEFT_PARANTHESIS T_RIGHT_PARANTHESIS) (lambda (x y) nil)))
					;

	(90a param_dcls
	     (( param_dcl) list)
	     (( param_dcl T_COMMA param_dcls) (lambda (x y z) (cons x z))))
					;

	(91 param_dcl
	    (( param_attribute param_type_spec simple_declarator)
	     (lambda (attr ty decl) (list attr decl ty))))
					;

	(92 param_attribute
	    (( T_IN ) (lambda (x) :param_in))
	    (( T_OUT) (lambda (x) :param_out))
	    (( T_INOUT) (lambda (x) :param_inout)))
					;

	(93 raises_expr
	    (( ) list)			; empty
	    (( T_RAISES T_LEFT_PARANTHESIS scoped_names T_RIGHT_PARANTHESIS)
	     (lambda (ra bo na bc) na)))
					;

	(94 context_expr
	    (( ) list)			;empty 
	    (( T_CONTEXT T_LEFT_PARANTHESIS string_literals T_RIGHT_PARANTHESIS)
	     (lambda (con bo sl bc) sl)))
					;

	(94a string_literals
	     (( T_string_literal_1) list)
	     (( T_string_literal_1 T_COMMA string_literals) (lambda (x y z) (cons x z))))
					;

	(94b T_string_literal_1
	     (( T_STRING_LITERAL) list)
	     (( T_STRING_LITERAL T_string_literal_1) cons))
					;

	(95 param_type_spec
	    (( base_type_spec) identity)
	    (( string_type) identity)
	    (( wide_string_type) identity)
	    (( scoped_name) identity))
					;

	(96 fixed_pt_type
	    (( T_FIXED T_LESS_THAN_SIGN positive_int_const T_COMMA positive_int_const T_GREATER_THAN_SIGN)
	     (lambda (fi ls pos-int co int-l gs) (list 'fixed pos-int int-l))))
					;

	(97 fixed_pt_const_type
	    (( T_FIXED) (lambda (x) 'fixed)))
					;

	(98 value_base_type
	    (( T_VALUEBASE)
	     (lambda (x) 'value-base)))
					;

					;/* New production for Principal  */
	(98a principal_type
	     (( T_PRINCIPAL)
	      (lambda (x) 'principal)))))
					;

					;%%
