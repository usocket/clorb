;;;; test-idlcomp.lisp

(in-package :clorb)

(defvar *temporary-directory*
  (or #+unix "/tmp/"
      #+mcl
      (let ((base (truename "ccl:")))
        (make-pathname
         :directory (list :absolute (second (pathname-directory base)) "tmp")
         :defaults base)))
  "")

(defun repository-from-string (string)
  (let ((repository (make-instance 'repository))
        (temp-file (merge-pathnames "working.idl" *temporary-directory*)))
    (with-open-file (out temp-file :direction :output :if-exists :supersede)
      (princ string out)
      ;;(terpri out)
      )
    (load-repository *default-idl-compiler* repository temp-file)
    repository))

(defmacro define-idl-test (name idl &rest pattern)
  `(define-test ,name
     (let ((repository (repository-from-string ,idl)))
       (ensure-pattern repository
                       (make-instance 'repository-pattern :args (list ,@pattern))))))


;;(trace repository-from-string)
;;(trace load-repository <specification> shell-to-string shell-to-stream)

(define-test-suite "idlcomp"

  (define-idl-test "comments"
    "// Test
    module Foo { /* bar */ interface Bar; };
    // comment ending in EOF"
    "Foo" (def-pattern :dk_module))

  (define-idl-test "escaping identifiers"
      "typedef long _foo;  // normal identifier
       typedef long _long; // reserved word as identifier"
    "foo" (def-pattern :dk_alias)
    "long" (def-pattern :dk_alias))

  ;; ---------------------------------

  (define-idl-test "short"
    "typedef short s; typedef unsigned short us;"
    "s" (def-pattern :dk_alias
          'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_short
                                  'op:type corba:tc_short))
    "us" (def-pattern :dk_alias
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_ushort
                                   'op:type corba:tc_ushort)))

  (define-idl-test "long"
    "typedef long l; typedef unsigned long ul;"
    "l" (def-pattern :dk_alias
          'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_long))
    "ul" (def-pattern :dk_alias
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_ulong)))


  (define-idl-test "long long"
    "typedef unsigned long long ull; typedef long long ll; "
    "ll" (def-pattern :dk_alias
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_longlong))
    "ull" (def-pattern :dk_alias
            'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_ulonglong)))


  (define-idl-test "string"
    "typedef string s0; typedef string<10> s10;
	typedef wstring ws0; typedef wstring<10> ws10; "
    "s0" (def-pattern :dk_alias
           'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_string))
    "s10" (def-pattern :dk_alias
            'op:original_type_def (def-pattern :dk_string 'op:bound 10))
    "ws0" (def-pattern :dk_alias
            'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_wstring))
    "ws10" (def-pattern :dk_alias
             'op:original_type_def (def-pattern :dk_wstring 'op:bound 10)))


  (define-idl-test "misc types"
    "	typedef octet o;
	typedef char c;
	typedef wchar wc;
	typedef boolean b;
	typedef Object i;
	typedef any a; "
    ;;
    "o" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_octet))
    "c" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_char))
    "wc" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_wchar))
    "b" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_boolean))
    "i" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_objref))
    "a" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_any)))


  (define-idl-test "fixed point"
    "typedef fixed<10,2> fp;"
    "fp" (pattern 'op:original_type_def
                  (def-pattern :dk_fixed 'op:digits 10 'op:scale 2)))


  (define-idl-test "float types"
    "
typedef float f;
typedef double d;
typedef long double ld;
"
    "f" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_float))
    "d" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_double))
    "ld" (pattern 'op:original_type_def (def-pattern :dk_primitive 'op:kind :pk_longdouble)))


  (define-idl-test "sequence"
    "typedef sequence<string> ubss;
  typedef sequence<string,20> bss;
  typedef sequence<bss> sss; "
    "ubss" (def-pattern :dk_alias
             'op:original_type_def (def-pattern :dk_sequence
                                     'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_string)
                                     'op:bound 0))
    "bss" (def-pattern :dk_alias
            'op:original_type_def (def-pattern :dk_sequence
                                    'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_string)
                                    'op:bound 20))
    "sss" (def-pattern :dk_alias
            'op:original_type_def
            (def-pattern :dk_sequence
              'op:element_type_def
              (def-pattern :dk_alias
                'op:original_type_def (def-pattern :dk_sequence
                                        'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_string)
                                        'op:bound 20))
              'op:bound 0)))


  (define-idl-test "arrays"
    "
typedef long a[10+1];
typedef string aa[2][9];
"
    "a" (pattern 'op:original_type_def
                 (def-pattern :dk_array 'op:length 11
                   'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_long)))
    "aa" (pattern 'op:original_type_def
                  (def-pattern :dk_array 'op:length 2
                    'op:element_type_def
                    (def-pattern :dk_array 'op:length 9
                      'op:element_type_def (def-pattern :dk_primitive 'op:kind :pk_string)))))


  (define-idl-test "const and literals"
    "const long A = 1; const long O = 0177; const long H = 0xA9b;
     const char B = 'C';
     const octet C = 1;
     const string S = \"wibbel\";
     const boolean D = TRUE;
     const fixed F = 12.45D;"
    "A" (def-pattern :dk_constant
          'op:type_def (def-pattern :dk_primitive 'op:kind :pk_long)
          'op:value (pattern 'any-typecode CORBA:tc_long
                             'any-value 1))

    "O" (pattern 'op:value (pattern 'any-value #O177))
    "H" (pattern 'op:value (pattern 'any-value #xA9B))

    "B" (def-pattern :dk_constant
          'op:type_def (def-pattern :dk_primitive 'op:kind :pk_char)
          'op:value (pattern 'any-typecode CORBA:tc_char
                             'any-value #\C))

    "C" (def-pattern :dk_constant
        'op:type_def (def-pattern :dk_primitive 'op:kind :pk_octet)
        'op:value (pattern 'any-typecode CORBA:tc_octet
                           'any-value 1))

    "S" (def-pattern :dk_constant
          'op:type_def (def-pattern :dk_primitive 'op:kind :pk_string)
          'op:value (pattern 'any-typecode corba:tc_string
                             'any-value "wibbel"))

    "D" (def-pattern :dk_constant
          'op:type_def (def-pattern :dk_primitive 'op:kind :pk_boolean)
          'op:value (pattern 'any-typecode corba:tc_boolean
                             'any-value t))

    "F" (def-pattern :dk_constant
          'op:type_def (def-pattern :dk_fixed
                         'op:digits 4
                         'op:scale 2
                         'op:type (create-fixed-tc 4 2))
          'op:value (pattern 'any-value 1245/100)))


  (define-test "fixed arith"
    (let ((n1 (make-idl-fixed 4 2 1234/100))
          (n2 (make-idl-fixed 2 -1 560)))
      (multiple-value-bind (d s m) (idl-fixed-values (add n1 n2))
        (ensure-eql m 57234/100)
        (ensure-eql s 2)
        (ensure-eql d (+ 4 1 1)))
      (ensure-equalp (sub n1 n2)
                     (make-idl-fixed 6 2 (- 1234/100 560)))
      (ensure-equalp (mul n1 n2)
                     (make-idl-fixed 6 1 (* 1234/100 560)))))


  (define-idl-test "constant expressions"
    "
const long x = 123+4*9;
const long y = 1 << 8;
typedef long a[y];
const fixed u = 12.3D + 5.01D;
const fixed u2 = u + 2.00D;
const fixed u3 = 3.1d * 2.2d;
"
    "x" (pattern 'op:value (pattern 'any-value (+ 123 (* 4 9)))
                 'op:type CORBA:tc_long )
    "y" (pattern 'op:value (pattern 'any-value 256))
    "a" (pattern 'op:original_type_def
                 (def-pattern :dk_array 'op:length 256))
    "u" (pattern 'op:type_def (pattern 'op:digits 5 'op:scale 2)
                 'op:value (pattern 'any-value 1731/100))
    "u2" (pattern 'op:type_def (pattern 'op:digits 6 'op:scale 2)
                  'op:value (pattern 'any-value (+ 1731/100 2)))
    "u3" (pattern 'op:type_def (pattern 'op:digits 4 'op:scale 2)
                  'op:value (pattern 'any-value (* 31/10 22/10))))


  (define-idl-test "native"
    "native servant;"
    "servant" (def-pattern :dk_native 'op:name "servant"))


  (define-idl-test "struct 1"
    "module Bar { struct foo { long x, y; string s; }; };"
    "Bar" (def-pattern :dk_module)
    "Bar::foo" (def-pattern :dk_struct
                 'op:name "foo"
                 'op:absolute_name "::Bar::foo"
                 'op:members (sequence-pattern
                              (struct-pattern 'struct-class-name 'omg.org/corba:structmember
                                              'op:name "x"
                                              'op:type CORBA:tc_long
                                              'op:type_def (def-pattern :dk_primitive
                                                             'op:kind :pk_long))
                              (struct-pattern 'struct-class-name 'omg.org/corba:structmember
                                              'op:name "y"
                                              'op:type CORBA:tc_long)
                              (struct-pattern 'struct-class-name 'omg.org/corba:structmember
                                              'op:name "s"
                                              'op:type CORBA:tc_string))))


  (define-idl-test "union 1"
    "union u switch(boolean) {
   case TRUE: long x;
   case FALSE: unsigned long y; };"
    "u" (def-pattern :dk_union
          'op:discriminator_type_def (def-pattern :dk_primitive 'op:kind :pk_boolean)
          'op:members (sequence-pattern
                       (struct-pattern 'op:name "x"
                                       'op:label (pattern 'any-value t))
                       (struct-pattern 'op:name "y"
                                       'op:label (pattern 'any-value nil)
                                       'op:type CORBA:tc_ulong))))


  (define-idl-test "union 2"
    "const long y_tag = 1;
   union u switch(long) {
   case 0: long x;
   case y_tag: unsigned long y;
   default: boolean flag; };"
    ;;
    "u" (def-pattern :dk_union
          'op:discriminator_type_def (def-pattern :dk_primitive 'op:kind :pk_long)
          'op:members (sequence-pattern
                       (struct-pattern 'op:name "x"
                                       'op:label (pattern 'any-value 0
                                                          'any-typecode CORBA:tc_long))
                       (struct-pattern 'op:name "y"
                                       'op:label (pattern 'any-value 1)
                                       'op:type CORBA:tc_ulong)
                       (struct-pattern 'op:name "flag"
                                       'op:label (pattern 'any-typecode CORBA:tc_octet
                                                          'any-value 0)
                                       'op:type omg.org/corba:tc_boolean))))


  (define-idl-test "enum"
    "enum E { NISSE, OLLE };
	const E x = NISSE; "
    "E" (def-pattern :dk_enum
          'op:members (sequence-pattern "NISSE" "OLLE"))
    "x" (def-pattern :dk_constant
          'op:type_def (def-pattern :dk_enum)
          'op:value (pattern 'any-value :nisse)))


  (define-idl-test "exception def"
    "exception exc { string msg; };"
    "exc" (def-pattern :dk_exception
            'op:id "IDL:exc:1.0"
            'op:type (pattern 'op:kind :tk_except
                              'op:name "exc"
                              'op:id "IDL:exc:1.0"
                              'op:member_count 1)
            'op:members (sequence-pattern
                         (struct-pattern
                          'struct-class-name 'omg.org/corba:structmember
                          'op:name "msg"
                          'op:type CORBA:tc_string))))


  (define-idl-test "interface"
    "interface foo {
	readonly attribute string sa;
	exception ex {};
	foo maybe (in long n, out long rest);
	void check () raises (foo::ex);
	oneway void note (in string foo); };"
    "foo" (def-pattern :dk_interface
            'op:type (pattern 'op:kind :tk_objref 'op:name "foo"))
    "foo::sa" (def-pattern :dk_attribute
                'op:mode :attr_readonly
                'op:type CORBA:tc_string)
    "foo::maybe" (def-pattern :dk_operation
                   'op:mode :op_normal
                   'op:result_def (def-pattern :dk_interface)
                   'op:params (sequence-pattern
                               (struct-pattern 'op:name "n" 'op:mode :param_in)
                               (struct-pattern 'op:name "rest" 'op:mode :param_out))
                   'op:exceptions (sequence-pattern))
    "foo::check" (def-pattern :dk_operation
                   'op:exceptions (sequence-pattern (def-pattern :dk_exception 'op:name "ex")))
    "foo::note" (def-pattern :dk_operation 'op:mode :op_oneway))


  (define-idl-test "Interface 2"
    "interface c;
   interface a { attribute long n; exception e {}; };
   interface b : a { attribute c peer; void op1() raises (e); };
   interface c : a { exception e {}; };"
    ;;
    "a" (def-pattern :dk_interface)
    "a::n" (def-pattern :dk_attribute)
    "b" (def-pattern :dk_interface
          'op:base_interfaces (sequence-pattern (def-pattern :dk_interface 'op:name "a")))
    "b::peer" (def-pattern :dk_attribute
                'op:absolute_name "::b::peer"
                'op:id "IDL:b/peer:1.0"
                'op:type_def (def-pattern :dk_interface 'op:name "c")
                'op:type_def (repository-pattern "e" (def-pattern :dk_exception)))
    "c" (def-pattern :dk_interface))


  (define-idl-test "Local Interface"
    "local interface Foo {
       void load (in string name);
     };"
    "Foo" (def-pattern :dk_localinterface )
    "Foo::load" (def-pattern :dk_operation))


  (define-idl-test "Abstract Interface"
    "abstract interface Foo;
     abstract interface Foo {
       void load (in string name);
     };"
    "Foo" (def-pattern :dk_abstractinterface)
    "Foo::load" (def-pattern :dk_operation))


  (define-idl-test "ValueBox"
    "valuetype Sevol long;"
    "Sevol" (def-pattern :dk_valuebox
              'op:original_type_def (def-pattern :dk_primitive
                                      'op:type CORBA:tc_long)))


  (define-idl-test "valuetype"
    "valuetype Foo {public long n; void inc();
       factory init (in string x);};"
    "Foo" (def-pattern :dk_value
            'op:name "Foo" 'op:id "IDL:Foo:1.0"
            'op:is_abstract nil
            'op:is_custom nil
            'op:initializers
            (sequence-pattern
             (struct-pattern 'struct-class-name 'CORBA::Initializer
                             'op:name "init"
                             'op:members
                             (sequence-pattern
                              (struct-pattern 'op:name "x" 'op:type omg.org/corba:tc_string)))))
    "Foo::n" (def-pattern :dk_valuemember
               'op:access omg.org/corba:public_member
               'op:type CORBA:tc_long)
    "Foo::inc" (def-pattern :dk_operation))


  (define-idl-test "valuetype2"
    "
abstract valuetype ax {void op1();};
valuetype cx {private string name;};
valuetype bx : cx, ax {};
"
    "ax" (def-pattern :dk_value
           'op:is_abstract t
           'contents (sequence-pattern
                      (def-pattern :dk_operation)))
    "bx" (def-pattern :dk_value
           'op:is_abstract nil
           'op:base_value (pattern 'op:name "cx")
           'op:abstract_base_values (sequence-pattern
                                     (pattern 'op:name "ax")))
    "bx::name" (def-pattern :dk_valuemember
                 'op:access omg.org/corba:private_member))



  ;; TODO:
  ;; forward struct, union,..
  ;; preprocessing ifdef..
  ;; identifiers starting with _


  (define-idl-test "Prefix Pragma"
    "/* foo */
  # pragma prefix \"omg.org\"
	module CosNaming { typedef string Istring; }; "
    "CosNaming::Istring" (def-pattern :dk_alias
                           'op:id "IDL:omg.org/CosNaming/Istring:1.0" ))

  (define-idl-test "ID Pragma"
    "struct knoll {long n;};
     #pragma ID knoll \"DCE:1293789123791873892:1\"
     interface Foo {
	#pragma ID Foo \"DCE:1293789123791873891:1\"
     };
     valuetype Boo {
        #pragma ID Boo \"IDL:lenst/Boo:1.9\"
     };"
    "Foo" (pattern 'op:id "DCE:1293789123791873891:1")
    "knoll" (pattern 'op:id "DCE:1293789123791873892:1")
    "Boo" (pattern 'op:id "IDL:lenst/Boo:1.9"))


  (define-idl-test "Version Pragma"
    "interface Foo {
	#pragma version Foo 1.2
	}; "
    "Foo" (pattern 'op:id "IDL:Foo:1.2"))

  (define-idl-test "Package-prefix pragma"
    "#pragma package-prefix \"FOO-\"
module Bar { interface Fum { exception Ouch {}; }; };"
    "Bar::Fum" (pattern 'op:absolute_name "::Bar::Fum")
    "Bar::Fum::Ouch" (pattern
                      `(target-code * ,(make-instance 'stub-target))
                      (sexp-pattern
                       `(define-user-exception
                          ,(pattern 'symbol-name "FUM/OUCH"
                                    'symbol-package
                                    (pattern 'package-name "FOO-BAR"))
                          &any-rest))))

  #|end|# ) ; end test suite

