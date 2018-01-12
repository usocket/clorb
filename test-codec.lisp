;;;; test-codec.lisp  -- Codec and CodecFactory implementations

(in-package :clorb)

(define-test-suite "Codec&Factory"

  (variables
   (orb (CORBA:ORB_init))
   (factory (op:resolve_initial_references orb "CodecFactory"))
   (format (iop:encoding :format iop:encoding_cdr_encaps
                                 :major_version 1
                                 :minor_version 0)))

  (define-test "Create Codec"
    (ensure factory "Factory exists")
    (let ((codec (op:create_codec factory format)))
      (ensure codec "Codec created")
      (ensure-typep codec 'IOP:Codec)))

  (define-test "Encode"
    (let ((codec (op:create_codec factory format)))
      (let ((octets (op:encode codec "hello")))
        (ensure-typep octets '(vector CORBA:Octet))
        (let ((any (unmarshal-encapsulation octets  #'unmarshal-any )))
          (ensure-equalp (any-value any) "hello")))
      (let ((octets (op:encode_value codec "world")))
        (ensure-typep octets '(vector CORBA:Octet))
        (let ((obj (unmarshal-encapsulation octets #'unmarshal-string)))
          (ensure-equalp obj "world")))))

  (define-test "Decode"
    (let ((codec (op:create_codec factory format)))
      (ensure-pattern* (any-value (op:decode codec (op:encode codec format)))
                       'op:format (op:format format)
                       'op:major_version (op:major_version format)
                       'op:minor_version (op:minor_version format))
      (ensure-equalp (op:decode_value codec (op:encode_value codec "hello") CORBA:tc_string)
                     "hello"))))


