;;;; Basic CORBA types

(in-package :clorb)


;;;; Basic types

(deftype corba:boolean ()
  t)

(deftype corba:char ()
  'character)

(deftype corba:wchar ()
  ;; FIXME! is this wide ?
  'character)

(deftype corba:octet ()
  '(unsigned-byte 8))

(deftype corba:string ()
  'string)

(deftype corba:wstring ()
  ;; FIXME
  '(simple-array corba:wchar))

(deftype corba:short ()
  '(signed-byte 16))

(deftype corba:ushort ()
  '(unsigned-byte 16))

(deftype corba:long ()
  '(signed-byte 32))

(deftype corba:ulong ()
  '(unsigned-byte 32))

(deftype corba:longlong ()
  '(signed-byte 64))

(deftype corba:ulonglong ()
  '(unsigned-byte 64))

(deftype corba:float ()
  (%SYSDEP "24 bit float"
           (if (>= (float-precision short-float-epsilon) 24)
               'short-float
               'single-float)))

(deftype corba:double ()
  (%SYSDEP "53 bit float"
             'double-float))

(deftype corba:longdouble ()
  (%SYSDEP "112 bit float"
          'long-float))

(deftype corba:fixed ()
  'rational)

(deftype fixed (digits scale)
  (let ((limit (if (< scale 0)
                 (* (expt 10 digits) (expt 10 (- scale)))
                 (/ (expt 10 digits) (expt 10 scale)))))
    `(rational (,(- limit)) (,limit))))


;;;; Exception types

(define-condition corba:exception (serious-condition)
  ())

(define-condition corba:userexception (corba:exception)
                  ())
