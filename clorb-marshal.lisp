;;;; Marshal

(in-package :clorb)

(eval-when (:compile-toplevel :load-toplevel :execute) 
  (pushnew 'more-safe *features*))


(defun marshal-void (x buffer)
  (declare (ignore x buffer))
  nil)


(defun marshal-octet (n buffer)
  (declare (type buffer buffer)
           (optimize (speed 3) (debug 0)))
  (with-out-buffer (buffer) (put-octet n)))

(defun marshal-bool (s buffer)
  (marshal-octet (if s 1 0) buffer))


(defmacro %marshal-number (n size buffer &optional (align size))
  (let ((nvar '#:nvar)
        (nnvar '#:nnvar))
    `(with-out-buffer (,buffer)
       (let* (,@(if (> size 3) 
                  `((,nnvar ,n) 
                    (,nvar (logand ,nnvar #xFFFFFF)))
                  `((,nvar ,n))))
         (declare (type (integer #x-FFFFFF #xFFFFFF) ,nvar))
         (align ,align)
         (put-octet (logand #xFF ,nvar))
         ,@(loop for p from 8 by 8
                 for c from 1 below size
                 collect `(put-octet (ldb (byte 8 ,p) 
                                          ,(if (>= c 3) nnvar nvar))))))))


(defun marshal-short (n buffer)
  (declare (type CORBA:short n)
           (optimize speed #+clorb::more-safe safety))
  (%marshal-number n 2 buffer))

(defun marshal-ushort (n buffer)
  (declare (type (or CORBA:ushort CORBA:short) n)
           (optimize speed #+clorb::more-safe safety))
  (%marshal-number n 2 buffer))

(defun marshal-ulong (n buffer)
  (declare (type CORBA:ulong n)
           (optimize speed #+clorb::more-safe safety))
  (%marshal-number n 4 buffer))

(defun marshal-long (n buffer)
  (declare (type CORBA:long n)
           (optimize speed #+clorb::more-safe safety))
  (%marshal-number n 4 buffer))

(defun marshal-longlong (arg buffer)
  (%marshal-number arg 8 buffer))

(defun marshal-ulonglong (arg buffer)
  (%marshal-number arg 8 buffer))

(defun marshal-char (char buffer)
  (marshal-octet (char-code char) buffer))

(defun float-as-ieee-integer (number sign-bit fraction-bits bias)
  (multiple-value-bind (frac expn sign)
                       (integer-decode-float number)
    (if (zerop frac)
      (ash (if (< sign 0) 1 0) sign-bit)
      (let* ((len (integer-length frac))
             (shift (+ 1 (- fraction-bits len))))
        (unless (zerop shift)
          (mess 1 "Shift=~D" shift))
        (logior (ash (if (< sign 0) 1 0) sign-bit)
                (ash (+ expn (- shift) fraction-bits bias) fraction-bits)
                (- (ash frac shift) (ash 1 fraction-bits)))))))


(defun marshal-float (arg buffer)
  (marshal-ulong (float-as-ieee-integer (coerce arg 'corba:float)
                                        31 23 127)
                 buffer))

(defun marshal-double (arg buffer)
  (marshal-ulonglong (float-as-ieee-integer (coerce arg 'corba:double)
                                         63 52 1023)
                     buffer))

(defun marshal-longdouble (arg buffer)
  (%marshal-number (float-as-ieee-integer (coerce arg 'corba:longdouble)
                                         127 112 16383)
                  16 buffer 8))


;; From Paul Foley

(defun single-float-bits (float)
  (declare (type single-float float))
  (multiple-value-bind (significand exponent sign) (decode-float float)
    (unless (= (float-radix float) 2)
      (setq exponent (* exponent (floor (log (float-radix float) 2)))))
    (when (and (<= 0.5f0 significand) (< significand 1.0f0))
      (setq significand (* significand 2.0f0)
            exponent (1- exponent)))
    (unless (and (= significand 0f0) (= exponent 0))
      (decf significand 1.0) (incf exponent 127))
    (logior (if (> sign 0f0) 0 (ash -1 31))
            (ash exponent 23)
            (truncate (* significand 8388608f0)))))

(defun double-float-high-bits (float)
  (declare (type double-float float))
  (multiple-value-bind (significand exponent sign) (decode-float float)
    (unless (= (float-radix float) 2)
      (setq exponent (* exponent (floor (log (float-radix float) 2)))))
    (when (and (<= 0.5f0 significand) (< significand 1.0f0))
      (setq significand (* significand 2.0f0)
            exponent (1- exponent)))
    (unless (and (= significand 0f0) (= exponent 0))
      (decf significand 1.0) (incf exponent 1023))
    (logior (if (> sign 0d0) 0 (ash -1 31))
            (ash exponent 20)
            (truncate (* significand 1048576d0)))))

(defun double-float-low-bits (float)
  (declare (type double-float float))
  (multiple-value-bind (significand exponent) (decode-float float)
    (unless (= (float-radix float) 2)
      (setq exponent (* exponent (floor (log (float-radix float) 2)))))
    (when (and (<= 0.5f0 significand) (< significand 1.0f0))
      (setq significand (* significand 2.0f0)
            exponent (1- exponent)))
    (unless (and (= significand 0f0) (= exponent 0))
      (decf significand 1.0) (incf exponent 1023))
    (values (truncate (* (nth-value 1 (truncate (* significand 1048576d0)))
                         4294967296d0)))))


(defun marshal-string (s buffer)
  (check-type s string)
  (marshal-ulong (1+ (length s)) buffer)
  (with-out-buffer (buffer)
    (loop for c across s
          do (put-octet (char-code c)))
    (put-octet 0)))


(defun marshal-osequence (s buffer)
  (when (stringp s)
    (setq s (string->octets s)))
  (marshal-ulong (length s) buffer)
  (with-out-buffer (buffer)
    (etypecase s
      (vector
       (loop for c across s do (put-octet c)))
      (list
       (loop for c in s do (put-octet c))))))


(defun marshal-sequence (s el-cdr buffer)
  (marshal-ulong (length s) buffer)
  (doseq (e s) (funcall el-cdr e buffer)))

(defun marshal-make-encapsulation (closure orb)
  (let ((buffer (get-work-buffer orb)))
   (marshal-octet 1 buffer)			;byte order
   (funcall closure buffer)
   (buffer-contents buffer)))

(defun marshal-add-encapsulation (closure buffer)
  (declare (optimize speed)
           (type buffer buffer))
  (with-out-buffer (buffer)
    (align 4)
    (let ((len-pos pos)
          (old-start-pos start-pos))
      (cond ((< (array-total-size octets)
                (+ len-pos 50))
             (adjust-array octets (+ len-pos 200)
                           :fill-pointer (+ len-pos 4)))
            (t
             (incf pos 4)))
      (setf (buffer-start-pos buffer) pos)
      (put-octet 1)            ;byte order
      (funcall closure buffer)
      (let ((save-pos pos)
            (size (- pos (buffer-start-pos buffer))))
        (setf (buffer-start-pos buffer) old-start-pos)
        (setf pos len-pos)
        (marshal-ulong size buffer)
        (setf pos save-pos)))))


(defvar *marshal-typecode-record* nil)

(defvar *typecode-params*)

(defun marshal-typecode (tc buffer)
  (let ((recursive-typecode-pos
         (cdr (assoc tc *marshal-typecode-record*))))
    (cond
     (recursive-typecode-pos
      (marshal-ulong #xFFFFFFFF buffer)
      (marshal-long (- recursive-typecode-pos (buffer-out-pos buffer))
                    buffer))
     (t
      (let ((kind (typecode-kind tc))
            (*typecode-params* (typecode-params tc)))
        (marshal-ulong (position kind (the vector TCKind)) buffer)
        (let ((*marshal-typecode-record*
               (acons tc (- (buffer-out-pos buffer) 4)
                      *marshal-typecode-record*)))
          (marshal-spec *typecode-params* (get kind 'tk-params) buffer)))))))


(defun marshal-spec (params pspec buffer)
  (cond ((null pspec))
        ((numberp pspec)
         (marshal params (elt *typecode-params* pspec) buffer))
        ((consp pspec)
         (case (car pspec)
           (complex
            (marshal-add-encapsulation
             (lambda (buffer) (marshal-spec params (cdr pspec) buffer))
             buffer))
           (sequence
            (marshal-sequence params 
                              (lambda (val buf)
                                (marshal-spec val (second pspec) buf))
                              buffer))
           (otherwise
            (mapc #'marshal-spec params pspec (repeated buffer)))))
        (t
         (ecase pspec
           (:tk_string (marshal-string params buffer))
           (:tk_long   (marshal-long params buffer))
           (:tk_ulong  (marshal-ulong params buffer))
           (:tk_short  (marshal-short params buffer))
           (:tk_ushort (marshal-ushort params buffer))
           (:tk_typecode (marshal-typecode params buffer))))))


(defun marshal-tagged-component (component buffer)
  (marshal-ulong (car component) buffer)
  (marshal-osequence (cdr component) buffer))



;;(defparameter *nil-objref*
;;  (make-instance 'CORBA:Proxy :id "" :raw-profiles '()))

(defmethod marshal-object ((object null) buffer)
  (marshal-string "" buffer)
  (marshal-ulong 0 buffer))

(defmethod marshal-object ((object t) buffer)
  (marshal-object (op:_this object) buffer))



;;; clorb-marshal.lisp ends here
