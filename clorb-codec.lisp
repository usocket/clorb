;;;; clorb-codec.lisp  -- Codec and CodecFactory implementations

(in-package :clorb)

;; 13.8 Coder/Decoder Interfaces
;;
;; The formats of IOR components and service context data used by
;; ORB services are often defined as CDR encapsulations encoding
;; instances of IDL defined data types. The Codec provides a
;; mechanism to transfer these components between their IDL data
;; types and their CDR encapsulation representations. A Codec is
;; obtained from the CodecFactory. The CodecFactory is obtained
;; through a call to ORB::resolve_initial_references ("CodecFactory").


;;;; local interface Codec

(defclass codec-impl (IOP:CODEC)
  ((the-orb :initarg :the-orb :accessor the-orb)))

;;	exception InvalidTypeForEncoding {};
;;	exception FormatMismatch {};
;;	exception TypeMismatch {};

;;; any decode (in CORBA::OctetSeq data)
;;		raises (FormatMismatch);

(define-method decode ((codec codec-impl) data)
  (unmarshal-encapsulation data #'unmarshal-any))

;;; any decode_value (
;;		in CORBA::OctetSeq data,
;;		in CORBA::TypeCode tc)
;;		raises (FormatMismatch, TypeMismatch);

(define-method decode_value ((codec codec-impl) data tc)
  (unmarshal-encapsulation data (lambda (buffer) (unmarshal tc buffer))))

;;; CORBA::OctetSeq encode (in any data)
;;		raises (InvalidTypeForEncoding);

(define-method encode ((codec codec-impl) data)
  (marshal-make-encapsulation
   (lambda (buffer)
     (marshal-any data buffer))
   (the-orb codec)))

;;; CORBA::OctetSeq encode_value (in any data)
;;		raises (InvalidTypeForEncoding);

(define-method encode_value ((codec codec-impl) data)
  (marshal-make-encapsulation
   (lambda (buffer)
     (marshal-any-value data buffer))
   (the-orb codec)))




;;;; local interface CodecFactory

(defclass codecfactory-impl (IOP:CodecFactory)
  ((the-orb  :initarg :the-orb  :accessor the-orb)
   (default-codec :reader default-codec)))


;;; Codec create_codec (in Encoding enc)
;;     raises (UnknownEncoding);

(define-method create_codec ((factory codecfactory-impl) enc)
  (check-type enc IOP:Encoding)
  (cond ((and (eql (op:format enc) iop:encoding_cdr_encaps)
              (eql (op:major_version enc) 1)
              (eql (op:minor_version enc) 0))
         (with-cache-slot (factory default-codec)
           (make-instance 'codec-impl :the-orb (the-orb factory))))
        (t
         (error (iop:codecfactory/unknownencoding)))))


(defun set-codec-factory (orb)
  (set-initial-reference orb "CodecFactory" nil
                         (make-instance 'codecfactory-impl :the-orb orb)))


(pushnew 'set-codec-factory *orb-initializers*)
(when *the-orb* (set-codec-factory *the-orb*))
