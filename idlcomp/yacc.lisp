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

(defconstant *tokens*
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
    T_with-prefix
    ))


;;%%
(defparameter rules
  `((1 specification
     
       ;((definitions) identity)
       ((defintions-a) identity)
       (()))

    (-1 definitions-a
	((definitions-b) list)
	((definitions-b definitions-a) cons))

    (-2 definitions-b
	((t_with-prefix definitions) (lambda (x y) (list 'with-prefix x y)))
	((definitions) identity))

    
    (1a definitions
	((definition) list)
	((definition definitions) cons))
    
    (2 definition
       (( type_dcl T_SEMICOLON) (lambda (x y) x))
       (( const_dcl T_SEMICOLON)  (lambda (x y) x))
       (( except_dcl T_SEMICOLON)  (lambda (x y) x))
       (( interface T_SEMICOLON)  (lambda (x y) x))
       (( module T_SEMICOLON)  (lambda (x y) x))
       (( value T_SEMICOLON)  (lambda (x y) x)))
					;
    
    (3 module
       (( T_MODULE T_IDENTIFIER T_LEFT_CURLY_BRACKET
		   definitions T_RIGHT_CURLY_BRACKET)
	(lambda (m i l d r) (list 'module (car i) d))))
					;

  (4 interface
     (( interface_dcl) identity)
     (( forward_dcl) identity))
					;

  (5 interface_dcl
     (( interface_header T_LEFT_CURLY_BRACKET interface_body
			 T_RIGHT_CURLY_BRACKET)
      (lambda (h po body pc) (append h (list body)))))
					;

  (6  forward_dcl
      (( T_INTERFACE T_IDENTIFIER) (lambda (x y) (list 'forward (car y))))
      (( T_ABSTRACT T_INTERFACE T_IDENTIFIER) (lambda (x y z) (list 'abstract-forward (car y)))))
					;

  (7 interface_header
     (( T_INTERFACE T_IDENTIFIER) (lambda (x y) (list 'interface (car y) nil)))
     (( T_INTERFACE T_IDENTIFIER interface_inheritance_spec) (lambda (x y s) (list 'interface (car y) s)))
     (( T_ABSTRACT T_INTERFACE T_IDENTIFIER)  (lambda (x y) (list 'abstract-interface (car y) nil)))
     (( T_ABSTRACT T_INTERFACE T_IDENTIFIER interface_inheritance_spec (lambda (x y s) (list 'abstract-interface (car y) s)))))
					;  

  (8 interface_body
     (( exports) identity)
     (()  (lambda () nil)))
					;

  (8a exports
      (( export) list)
      (( export exports) cons))
					;

  (9 export
     (( type_dcl T_SEMICOLON) (lambda (x y) x))
     (( const_dcl T_SEMICOLON )(lambda (x y) x))
     (( except_dcl T_SEMICOLON)(lambda (x y) x))
     (( attr_dcl T_SEMICOLON)(lambda (x y) x))
     (( op_dcl T_SEMICOLON )(lambda (x y) x)))
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
					;

  (12 scoped_name
      (( T_IDENTIFIER) (lambda (x) (list 'no-colon (car x))))
      (( T_SCOPE T_IDENTIFIER) (lambda (x y) (list (car y))))
      (( scoped_name T_SCOPE T_IDENTIFIER) (lambda (x y z) (append x (list (car z))))))
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
       (lambda (to id spec) (list 'value-box-dcl (cdr id) spec))))
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
      (( T_CONST const_type T_IDENTIFIER T_EQUAL const_exp)))
					;

  (28 const_type
      (( integer_type))
      (( char_type))
      (( wide_char_type))
      (( boolean_type))
      (( floating_pt_type))
      (( string_type))
      (( wide_string_type))
      (( fixed_pt_const_type))
      (( scoped_name ))
      (( octet_type)))
					;

  (29 const_exp
      (( or_expr)))
					;

  (30 or_expr
      (( xor_expr))
      (( or_expr T_VERTICAL_LINE xor_expr)))
					;

  (31 xor_expr
      (( and_expr))
      (( xor_expr T_CIRCUMFLEX and_expr)))
					;

  (32 and_expr
      (( shift_expr))
      (( and_expr T_AMPERSAND shift_expr)))
					;

  (33 shift_expr
      (( add_expr))
      (( shift_expr T_SHIFTRIGHT add_expr))
      (( shift_expr T_SHIFTLEFT add_expr)))
					;

  (34 add_expr
      (( mult_expr))
      (( add_expr T_PLUS_SIGN mult_expr))
      (( add_expr T_MINUS_SIGN mult_expr)))
					;

  (35 mult_expr
      (( unary_expr))
      (( mult_expr T_ASTERISK unary_expr))
      (( mult_expr T_SOLIDUS unary_expr))
      (( mult_expr T_PERCENT_SIGN unary_expr)))
					;


  (37 unary_expr
      (( T_MINUS_SIGN primary_expr))
      (( T_PLUS_SIGN primary_expr))
      (( T_TILDE primary_expr))
      (( primary_expr)))
					;

  (38 primary_expr
      (( scoped_name))
      (( literal))
      (( T_LEFT_PARANTHESIS const_exp T_RIGHT_PARANTHESIS)))
					;


  (40 literal
      (( T_INTEGER_LITERAL))
      (( T_string_literal))
      (( T_CHARACTER_LITERAL))
      (( T_FIXED_PT_LITERAL))
      (( T_FLOATING_PT_LITERAL))
      (( T_TRUE ))			;boolean_literal 
      (( T_FALSE )))			;boolean_literal 
					;

  (41 positive_int_const
      (( const_exp)))
					;


  (43 type_dcl
      (( T_TYPEDEF type_spec declarators) (lambda (x y z) (list 'typedef y z)))
      (( struct_type) identity)
      (( union_type) identity)
      (( enum_type) identity)
      (( T_NATIVE simple_declarator) (lambda (x y) (list ' native y))))
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
      (( principal_type) identity))		;   New 
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
      (( T_IDENTIFIER) car))
					;

  (52 complex_declarator
      (( array_declarator)))
					;

  (53 floating_pt_type
      (( T_FLOAT) (lambda (x) 'float))
      (( T_DOUBLE) (lambda (x) 'double))
      (( T_LONG T_DOUBLE) (lambda (x y) 'long-double)))
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
      (( T_LONG T_LONG) (lambda (x y) 'long-long)))
					;

  (59 unsigned_int
      (( unsigned_long_int) (lambda (x) 'ulong))
      (( unsigned_short_int) (lambda (x) 'ushort))
      (( unsigned_longlong_int) (lambda (x) 'ulong-long)))
					;

  (60 unsigned_short_int
      (( T_UNSIGNED T_SHORT)))
					;

  (61 unsigned_long_int
      (( T_UNSIGNED T_LONG)))
					;

  (62 unsigned_longlong_int
      (( T_UNSIGNED T_LONG T_LONG)))
					;

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
       (lambda (ts id bo mem bc) (list 'struct (car id) mem))))
					;

  (70 member_list
      (( member) list)
      (( member member_list) cons))
					;

  (71 member
      (( type_spec declarators T_SEMICOLON)
       (lambda (x y z) (list x y))))
					;

  (72 union_type
      (( T_UNION T_IDENTIFIER T_SWITCH T_LEFT_PARANTHESIS
		 switch_type_spec T_RIGHT_PARANTHESIS T_LEFT_CURLY_BRACKET
		 switch_body T_RIGHT_CURLY_BRACKET)))
					; 

  (73 switch_type_spec
      (( integer_type) identity)
      (( char_type) identity)
      (( boolean_type) identity)
      (( enum_type) identity)
      (( scoped_name) identity))
					;

  (74 switch_body
      (( case))
       (( case switch_body)))
					;

  (75 case	
      (( case_label case))
      (( case_label element_spec T_SEMICOLON))
      (( case_label T_PRAGMA element_spec T_SEMICOLON))) ;New  
					;

  (76 case_label
      (( T_CASE const_exp T_COLON ))
      (( T_DEFAULT T_COLON)))
					;

  (77 element_spec
      (( type_spec declarator) list))
					;

  (78 enum_type
      (( T_ENUM T_IDENTIFIER T_LEFT_CURLY_BRACKET enumerators
		T_RIGHT_CURLY_BRACKET)
       (lambda (te id bo enums bc) (list 'enum  (car id) enums))))
					;

  (78a enumerators
       (( enumerator) list)
       (( enumerator T_COMMA enumerators) (lambda (x y z) (cons x z))))
					;

  (79 enumerator
      (( T_IDENTIFIER) car))
					;

  (80 sequence_type
      (( T_SEQUENCE T_LESS_THAN_SIGN simple_type_spec T_COMMA
		    positive_int_const T_GREATER_THAN_SIGN)
       (lambda (ts ls spec int gs) (list 'sequence spec int)))
      (( T_SEQUENCE T_LESS_THAN_SIGN simple_type_spec T_GREATER_THAN_SIGN)
       (lambda (ts ls spec gs) (list 'sequence spec nil))))
					;

  (81 string_type
      (( T_STRING T_LESS_THAN_SIGN positive_int_const T_GREATER_THAN_SIGN)
           (lambda (ts ls int gs) (list 'string int)))
      (( T_STRING) (lambda (x) (list 'string nil))))
					;

  (82 wide_string_type
      (( T_WSTRING T_LESS_THAN_SIGN positive_int_const T_GREATER_THAN_SIGN)
        (lambda (ts ls int gs) (list 'wstring int)))
      (( T_WSTRING)(lambda (x) (list 'wstring nil))))
					;

  (83 array_declarator
      (( T_IDENTIFIER fixed_array_sizes) (lambda (x y) (append x y))))
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
      (( T_ATTRIBUTE param_type_spec simple_declarators) (lambda (x y z) (list 'attribute y z)))  
      (( T_READONLY T_ATTRIBUTE param_type_spec simple_declarators) (lambda (ro x y z) (list 'ro-attribute y z))))
					; 

  (85a simple_declarators
       (( simple_declarator) list)
       (( simple_declarator T_COMMA simple_declarators) (lambda (x y z) (cons x z))))
					;

  (86 except_dcl
      (( T_EXCEPTION T_IDENTIFIER T_LEFT_CURLY_BRACKET members
		     T_RIGHT_CURLY_BRACKET)
       (lambda (te id bc mem bc) (list 'exception id mem))))
					;

  (86a members
       (( ) list)			;empty 
       (( member members) cons))
	;

  (87 op_dcl
      (( op_attribute op_type_spec T_IDENTIFIER parameter_dcls
		      raises_expr context_expr)
       (lambda (op-a op-ty id para raise con) (list 'operation  (car id) op-a op-ty para raise con))))
					;

  (88 op_attribute
      (( ) list)
      (( T_ONEWAY) (lambda (x) 'oneway)))
					;

  (89 op_type_spec
      (( param_type_spec) identity)
      (( T_VOID) list))
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
       list))
					;

  (92 param_attribute
      (( T_IN ) (lambda (x) 'in))
      (( T_OUT) (lambda (x) 'out))
      (( T_INOUT) (lambda (x) 'inout)))
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
      (( T_FIXED T_LESS_THAN_SIGN positive_int_const T_COMMA
		 T_INTEGER_LITERAL T_GREATER_THAN_SIGN)
       (lambda (fi ls pos-int co int-l gs) (list pos-int int-l))))
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

(defun def-lambda (&rest a) a)
(defun flatten-rule (rule)
  (let ((no (car rule))
	(sym (cadr rule))
	(abl (cddr rule)))
    (mapcar (lambda (x) (append (list sym '-->) (car x) (list (if (null (cdr x)) `#'def-lambda (list 'function (cadr x)))))) abl)))

(setf grammar (apply #'append (mapcar #'flatten-rule rules)))



'(defun list-reader (l)
   (lambda ()
     (if (endp l) '($ $)
       (progn ()
					;(pprint (car l))
	      (pop l)))))

(load "lalr-2.lisp")


(setq  parser-procedure (make-parser grammar *tokens* '$))

(with-open-stream (s (open "idl-parser.lisp" :direction :output))
		  (pprint parser-procedure s))

(compile-file "idl-parser.lisp")


;(setf a (idl-lex (stream->string (open "workflow.txt"))))

;(eval (make-parser grammar *tokens* '$))

;(lalr-parser (list-reader a) #'gr-error) 