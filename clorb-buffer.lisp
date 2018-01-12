;;;; Buffers for marshaling

(in-package :clorb)

(deftype buffer-index ()
  '(integer 0 #.array-dimension-limit))

;;(deftype octets ()  '(vector CORBA:octet))

(defvar *empty-octets*
  (make-array 0 :element-type 'CORBA:octet))


(defvar *chunking-level* 0)
(declaim (type fixnum *chunking-level*))

(defvar *chunk-end* nil
  "Where current chunk is ending in the buffer. If nil, not in a chunk.")


(defstruct buffer
  (giop-version :giop-1-0 :type symbol)
  (orb          nil)
  (octets       *empty-octets* :type octets)
  (in-pos       0         :type buffer-index)
  (byte-order   1         :type bit)
  (start-pos    0         :type buffer-index)
  (ind-hash     nil))

(defmethod print-object ((buffer buffer) stream)
  (print-unreadable-object (buffer stream :identity t :type t)
    (format stream "~D/~S/~D ~A"
            (buffer-in-pos buffer)
            (ignore-errors (fill-pointer (buffer-octets buffer)))
            (buffer-start-pos buffer)
            (if (buffer-byte-order buffer) "L" "B"))))

(defmethod the-orb ((obj buffer))
  (or (buffer-orb obj)
      *the-orb*))

(defun buffer-contents (buffer)
  (copy-seq (buffer-octets buffer)))

(defun buffer-out-pos (buffer)
  (fill-pointer (buffer-octets buffer)))

(defun buffer-abs-pos (buffer)
  (buffer-in-pos buffer))

(defun buffer-rel-pos (buffer)
  (declare (optimize speed)
           (type buffer buffer))
  (- (buffer-in-pos buffer) (buffer-start-pos buffer)))

(defun buffer-length (buffer)
  (length (buffer-octets buffer)))


(defmacro with-in-chunking ((chunking) &body body)
  `(let ((*chunking-level* 
          (if (or ,chunking (not (zerop *chunking-level*)))
            (1+ (abs *chunking-level*))
            *chunking-level*))
         (*chunk-end* nil))
    ,@body))

(define-symbol-macro chunking-p (> *chunking-level* 0))

(defmacro without-chunking ((buffer) &body body)
  `(progn
     (when (and chunking-p *chunk-end*)
       (assert (= *chunk-end* (buffer-in-pos ,buffer)))
       (setq *chunk-end* nil))
     (assert (>= *chunking-level* 0))
     (let ((*chunking-level* (- *chunking-level*)))
       ,@body )))



(defmacro with-sub-buffer ((buffer length) &body body)
  (let ((old-start '#:old-start)
        (old-pos   '#:old-pos)
        (old-byteorder '#:old-byteorder)
        (buffervar '#:buffervar)
        (lengthvar '#:lengthvar))
    `(let* ((,buffervar ,buffer)
            (,lengthvar ,length)
            (,old-start (buffer-start-pos ,buffervar))
            (,old-pos   (buffer-in-pos ,buffervar))
            (,old-byteorder (buffer-byte-order ,buffervar)))
       (unwind-protect
           (progn
             (setf (buffer-start-pos ,buffervar) (buffer-in-pos ,buffervar))
             ,@body)
         (setf (buffer-start-pos ,buffervar) ,old-start)
         (setf (buffer-in-pos ,buffervar) (+ ,old-pos ,lengthvar))
         (setf (buffer-byte-order ,buffervar) ,old-byteorder)))))

(defun get-work-buffer (orb)
  (make-buffer
   :orb orb
   :octets (make-array 2000
                       :adjustable t
                       :fill-pointer 0
                       :element-type 'CORBA:octet)))


(defun buffer-record (buffer)
  (or (buffer-ind-hash buffer)
      (setf (buffer-ind-hash buffer) (make-hash-table :test #'eql))))


(defmacro with-out-buffer ((buffer &key (resize 400)) &body body)
  "Execute BODY with access to internals of buffer.
In body: 
- buffer     the buffer,
- octets     the octet vector,
- start-pos  start pos in octets for this work,
- pos        current pos in octets (setf-able),
- (align n)  align output to n octets (n must be litteral),
- (put-octet n)  output octet n. "
  `(let* ((buffer ,buffer)
          (start-pos (buffer-start-pos buffer))
          (octets (buffer-octets buffer)))
     (declare (ignorable start-pos octets)
              (type buffer-index start-pos))
     (macrolet ((put-octet (n)
                  `(vector-push-extend ,n octets ,',resize))
                (align (n) 
                  `(let ((skip (- ,n (logand (the buffer-index (- (fill-pointer octets) start-pos))
                                             (- ,n 1)))))
                     (declare (fixnum skip))
                     (when (< skip ,n)
                       (dotimes (x skip)
                         (declare (fixnum x))
                         (vector-push-extend 0 octets ,',resize))))))
       (symbol-macrolet ((pos (fill-pointer octets)))
         ,@body))))


(defun start-in-chunk (buffer)
  (let ((size (without-chunking (buffer) (unmarshal-long buffer))))
    (assert (< 0 size #x7FFFFFFF))
    (setf *chunk-end* (+ (buffer-in-pos buffer) size))))

(defmacro with-in-buffer ((buffer &key (check nil)) &body body)
  `(progn
     ,@(if check `((check-type ,buffer buffer "a buffer")))
     (let ((buffer ,buffer))
       (declare (type buffer buffer))
       (let ((octets (buffer-octets buffer)))
         (declare (type octets octets))
         (symbol-macrolet ((pos (buffer-in-pos buffer)))
           (macrolet 
             ((get-octet () 
                `(the CORBA:Octet
                   (prog1 (aref octets pos)
                     (setf pos (the buffer-index (1+ pos))))))
              
              (align (n)
                `(progn
                   (when (and chunking-p
                              (or (null *chunk-end*)
                                  (>= (buffer-in-pos buffer) (the fixnum *chunk-end*))))
                     (start-in-chunk buffer))
                   ,(unless (zerop (the fixnum n))
                      `(setf pos
                             (the buffer-index
                               (+ pos (the fixnum (logand (- ,n (the (integer 0 ,n)
                                                                  (logand (the buffer-index (buffer-rel-pos buffer))
                                                                          (- ,n 1))))
                                                          (- ,n 1))))))))))
             (multiple-value-prog1
               (progn ,@body)
               (when (and chunking-p *chunk-end*
                          (>= (buffer-in-pos buffer) (the fixnum *chunk-end*)))
                 (assert (eql *chunk-end* pos))
                 (setq *chunk-end* nil)))))))))


