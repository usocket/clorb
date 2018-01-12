;;;; CORBA:Any handling

(in-package :clorb)

(defclass CORBA:ANY ()
  ((typecode :initarg :typecode )
   (value    :initarg :value    )))

(defgeneric any-typecode (obj))

(defgeneric any-value (obj))


(defun corba:any (&key any-typecode any-value)
  (check-type any-typecode (or NULL CORBA:TypeCode))
  (make-instance 'CORBA:Any
   :typecode (or any-typecode (any-typecode any-value))
   :value any-value))


(defmethod initialize-instance :after ((any corba:any) &key &allow-other-keys)
  (unless (slot-boundp any 'typecode)
    (setf (slot-value any 'typecode)
      (any-typecode (slot-value any 'value)))))

(defmethod print-object ((any corba:any) stream)
  (print-unreadable-object (any stream :type t)
    (dolist (slot '(typecode nil value))
      (cond ((null slot) (princ " " stream))
            ((slot-boundp any slot) (prin1 (slot-value any slot) stream))
            (t (princ "?" stream))))))


(defmethod any-typecode ((obj corba:any))
  (slot-value obj 'typecode))

(defmethod any-value ((obj corba:any))
  (slot-value obj 'value))

(defmethod (setf any-value) (val (obj corba:any))
  (setf (slot-value obj 'value) val))

(defmethod (setf any-typecode) (val (obj corba:any))
  (setf (slot-value obj 'typecode) val))


;;; TypeCode accessor

(defmethod any-typecode ((obj t))
  (etypecase obj
    (character (if (< (char-code obj) 256) corba:tc_char corba:tc_wchar))
    #+clorb-distinct-wchar-type
    (corba:wchar     corba:tc_wchar)
    (CORBA:short     CORBA:tc_short)
    (CORBA:ushort    CORBA:tc_ushort)
    (CORBA:long      CORBA:tc_long)
    (CORBA:ulong     CORBA:tc_ulong)
    (CORBA:longlong  CORBA:tc_longlong)
    (CORBA:ulonglong CORBA:tc_ulonglong)
    (corba:float     CORBA:tc_float)
    (corba:double    CORBA:tc_double)
    #|(corba:longdouble CORBA:tc_longdouble)|#
    (corba:boolean   corba:tc_boolean)))


(defun member-typecode (sequence)
  (let ((max-num 0)
        (min-num 0)
        (non-string nil)
        (non-integer nil))
    (doseq (el sequence)
           (setf non-string (or non-string (not (stringp el))))
           (cond ((integerp el)
                  (setf max-num (max max-num el)
                        min-num (min min-num el)))
                 (t (setf non-integer t))))
    (cond ((not non-string)
           corba:tc_string)
          ((not non-integer)
           (if (< min-num 0)
               (any-typecode (- (max (- min-num) max-num)))
             (any-typecode max-num)))
          (t corba:tc_any))))


(defmethod any-typecode ((obj array))
  (create-array-tc (length obj) (member-typecode obj)))

(defmethod any-typecode ((obj list))
  (create-sequence-tc 0 (member-typecode obj)))

(defmethod any-typecode ((obj string))
  CORBA:tc_string)


;;; Value accessor

(defmethod any-value ((obj number))
  obj)

(defmethod any-value ((obj string))
  obj)

(defmethod any-value ((obj character))
  obj)

(defmethod any-value ((obj symbol))
  ;; ENUM
  obj)

(defmethod any-value ((obj sequence))
  obj)

(defmethod any-value ((obj t))
  (raise-system-exception 'CORBA:BAD_PARAM))



;;;; Marshal/Unmarshal


(defun marshal-any (arg buffer)
  (let ((tc (any-typecode arg)))
    (marshal-typecode tc buffer)
    (marshal (any-value arg) tc buffer)))

(defun unmarshal-any (buffer)
  (let ((tc (unmarshal-typecode buffer)))
    (if *explicit-any*
        (corba:any :any-typecode tc :any-value (unmarshal tc buffer))
      (unmarshal tc buffer))))


(defun marshal-any-value (any buffer)
  (marshal (any-value any)
           (any-typecode any)
           buffer))
