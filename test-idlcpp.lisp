;;;; test-idlcpp.lisp -- preprocessor interfrace test suite

(in-package :clorb)

(defparameter *cpp-test-source-1*
  (format nil "~@{~A~%~}"
          "# 1 \"file.idl\""
          "foo;"
          "#pragma prefix \"fig.org\""
          "module foo {"
          "  # pragma ID foo \\"
          "  \"DCE:21390128309812038:1\" "
          " const long xx = 1;"
          "#pragma version xx 1.1"
          "};" ))

(defparameter *cpp-test-source-2*
  (format nil "~@{~A~%~}"
          "# 1 \"foo.idl\""
          "// foo"
          "#pragma prefix \"foo\""
          "#pragma package-prefix \"pp\""
          "# 1 \"bar.idl\" 1"
          "// bar"
          "module Bar {"
          "  #pragma prefix \"bar\""
          "  const x=1;"
          "};"
          "# 4 \"foo.idl\" 2"
          "module Foo;"))


(define-test-suite "IDLCPP"

  (define-test "cpp-line-p"
    (ensure (cpp-line-p "#hej"))
    (ensure (cpp-line-p "# hej"))
    (ensure (cpp-line-p " # hej"))
    (ensure (cpp-line-p " 		# hej"))
    (ensure (not (cpp-line-p "/#hej"))))

  (define-test "read raw and continuation lines"
    (with-input-from-string (raw-stream 
                             "# Foo bar\\
bas bara" )
      (let ((cpp (make-instance 'preprocessed-stream
                   :cpp-stream raw-stream)))
        (let ((line (read-raw-line cpp)))
          (ensure-equalp line "# Foo bar\\")
          (setq line (read-cpp-continuation-lines cpp line))
          (ensure-equalp line "# Foo barbas bara")))))
  

  (define-test "parse-escape-sequence"
    (flet ((test (string start end char)
             (with-sub-test (string)
               (multiple-value-bind (res1 res2)
                                    (parse-escape-sequence string start)
                 (ensure-typep res1 'character)
                 (ensure-typep res2 'integer)
                 (ensure (char= res1 char)
                         "expected char ~S got ~S" char res1)
                 (ensure (= res2 end)
                         "expected parse end ~S got ~S" end res2)))))
      (test "\\t" 1 2 #\tab)
      (test "\\'" 1 2 #\')
      (test "014" 0 3 #\Page)
      (test "x0A" 0 3 #\Linefeed)
      (test "x41" 0 3 #\A)))
  
  (define-test "parse-string-literal"
    (flet ((test (string delimiter start end result)
             (with-sub-test (string)
               (multiple-value-bind (res1 res2)
                                    (parse-string-literal string delimiter :start start)
                 (ensure-typep res1 'string)
                 (ensure-typep res2 'integer)
                 (ensure (string= res1 result)
                         "expected string ~S got ~S" result res1)
                 (ensure (= res2 end)
                         "expected parse end ~S got ~S" end res2)))))
      (test "'foo'x" #\' 0 5 "foo")
      (test "'\\x41'" #\' 0 6 "A")
      (test " \"\\nh\" " #\" 1 6 (format nil "~%h"))))

  (define-test "parse-cpp-line" 
    (with-input-from-string (raw-stream *cpp-test-source-1*)
      (let ((cpp (make-instance 'preprocessed-stream
                   :cpp-stream raw-stream)))
        (let ((line (read-raw-line cpp)))
          (parse-cpp-line cpp line)
          (ensure-equalp (read-raw-line cpp) "foo;")
          (ensure-equalp (current-file cpp) "file.idl")
          (ensure-equalp (current-line cpp) 1))
        (let ((line (read-raw-line cpp)))
          (parse-cpp-line cpp line)
          (ensure-equalp (read-raw-line cpp) "module foo {")
          (ensure-equalp (current-file cpp) "file.idl")
          (ensure-equalp (current-line cpp) 3)
          (ensure-equalp (idl-prefix cpp) "fig.org"))
        (let ((line (read-raw-line cpp)))
          (parse-cpp-line cpp line)
          (ensure-equalp (read-raw-line cpp) " const long xx = 1;")
          (let ((sp (idl-source-position cpp)))
            (ensure-equalp (first sp) "file.idl")
            (ensure-equalp (second sp) 6))
          (ensure-equalp (idl-repositoryid-pragmas cpp)
                         '((:id "foo" "DCE:21390128309812038:1")))
          (ensure-equalp (idl-repositoryid-pragmas cpp) nil))
        (let ((line (read-raw-line cpp)))
          (parse-cpp-line cpp line)
          (ensure-equalp (read-raw-line cpp) "};")
          (ensure-equalp (idl-repositoryid-pragmas cpp)
                         '((:version "xx" "1.1")))))))


  (define-test "read-cpp-line"
    (with-input-from-string (raw-stream *cpp-test-source-1*)
      (let ((cpp (make-instance 'preprocessed-stream
                   :cpp-stream raw-stream)))
        (ensure-equalp (read-cpp-line cpp) "") 
        (ensure-equalp (read-cpp-line cpp) "foo;")
        (ensure-equalp (read-cpp-line cpp) "")
        (ensure-equalp (read-cpp-line cpp) "module foo {")
        (ensure-equalp (idl-prefix cpp) "fig.org")
        (ensure-equalp (read-cpp-line cpp) "")
        (ensure-equalp (read-cpp-line cpp) " const long xx = 1;")
        (ensure-equalp (read-cpp-line cpp) "")
        (ensure-equalp (read-cpp-line cpp) "};")
        (let ((list (idl-repositoryid-pragmas cpp)))
          (ensure-eql (length list) 2)))))

  (define-test "read-cpp-line - include"
    (with-input-from-string (raw-stream *cpp-test-source-2*)
      (let ((cpp (make-instance 'preprocessed-stream
                   :cpp-stream raw-stream)))
        (ensure-equalp (read-cpp-line cpp) "") 
        (ensure-equalp (read-cpp-line cpp) "// foo")
        (ensure-equalp (read-cpp-line cpp) "")
        (ensure-equalp (idl-prefix cpp) "foo")
        (ensure-equalp (read-cpp-line cpp) "") ; package-prefix "pp"
        (ensure-equalp (package-prefix cpp) "pp")
        (ensure-equalp (read-cpp-line cpp) "")
        (ensure-equalp (read-cpp-line cpp) "// bar")
        (ensure-equalp (current-file cpp) "bar.idl")
        (ensure-equalp (read-cpp-line cpp) "module Bar {")
        (ensure-equalp (idl-prefix cpp) "")
        (ensure-equalp (package-prefix cpp) "")
        (ensure-equalp (read-cpp-line cpp) "") ; #pragma prefix "bar"
        (ensure-equalp (read-cpp-line cpp) "  const x=1;")
        (ensure-equalp (idl-prefix cpp) "bar")
        (ensure-equalp (read-cpp-line cpp) "};")
        (ensure-equalp (read-cpp-line cpp) "")
        (ensure-equalp (read-cpp-line cpp) "module Foo;")
        (ensure-equalp (idl-prefix cpp) "foo")
        (ensure-equalp (package-prefix cpp) "pp"))))
      

  (define-test "using-cpp-stream"
      (using-cpp-stream
       (merge-pathnames
        (make-pathname :name "hello" :type "idl" 
                       :directory '(:relative "examples" "hello"))
        (truename *clorb-pathname-defaults*))
       (lambda (cpp)
         (ensure-typep (read-cpp-line cpp) 'string)
         (let ((file (current-file cpp)))
           (ensure (search "hello.idl" file)
                   "correct file name"))
         (loop for line = (read-cpp-line cpp)
             until (search "Hello" line))
         ;; read to end (end with nil)
         (loop for line = (read-cpp-line cpp)
             for i from 0
             while line
             when (> i 100) do (ensure nil)))))


  #|end|# ) ; end test-suit
