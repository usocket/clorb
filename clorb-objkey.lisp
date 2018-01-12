(in-package :clorb)

#|
Layout for Object Key

Transient ior:
1. Magic number for transient IOR <UShort>
2. POA-id <UShort>
3. Unique number for this server instance <ULong>
4. ObjectId

Persistent ior (1):
1. Magic number identifiying the IOR as a persistent IOR
2. POA-id registered with a locator service <UShort>
3. ObjectId

Persistent ior (2):
1. Magic number identifiying the IOR as a persistent IOR
2. POA level <UShort> (1 if child of root poa)
3. poa-name (one per level) as CDR string
4. ObjectId

POA-ids are numbers and are mapped by the ORB to a POA. There should
be no need to store the actual path to the POA in the object key.
|#

(defconstant +transient-ior-magic+   #x4C43)
(defconstant +persistent-ior-magic1+ #x6C63)
(defconstant +persistent-ior-magic2+ #x6C43)

(defvar *instance-id* (random (expt 2 31) (make-random-state t)))

(defun decode-object-key-from-buffer (buffer)
  "Returns: type poa-spec object-id"
  (let* ((magic (if (>= (buffer-length buffer) 2)
                  (unmarshal-ushort buffer)
                  0)))
    (cond
     ((and (eql magic +transient-ior-magic+)
           (>= (buffer-length buffer) 8))
      (let* ((poaid (unmarshal-ushort buffer))
             (uniq (unmarshal-ulong buffer)))
        (if (= uniq *instance-id*)
          (values :transient
                  poaid
                  (subseq (buffer-octets buffer)
                          (buffer-in-pos buffer)))
          (progn
            (mess 2 "Invalid unique id, IOR from other instance")
            nil))))
     ((and (eql magic +persistent-ior-magic1+)
           (>= (buffer-length buffer) 4))
      (values :persistent
              (unmarshal-ushort buffer)
              (subseq (buffer-octets buffer)
                      (buffer-in-pos buffer))))
     ((and (eql magic +persistent-ior-magic2+)
           (>= (buffer-length buffer) 4))
      (values :persistent
              (loop repeat (unmarshal-ushort buffer)
                    collect (unmarshal-string buffer))
              (subseq (buffer-octets buffer)
                      (buffer-in-pos buffer))))
     (t
      ;;(warn "invalid magic=~S" magic)
      ;; default poa for boot objects
      (values :transient 0 (buffer-octets buffer))))))

(defun decode-object-key (octets)
  "Returns: type poa-spec object-id"
  (let ((buffer (make-buffer :octets octets)))
    (decode-object-key-from-buffer buffer)))


(defgeneric to-object-id (objid)
  (:documentation "Convert a lisp object to an object key.
An object key is a octet seqeunce. But for convenience some other lisp
types will be converterd by this GF."))

(defmethod to-object-id ((objid string))
  (map 'vector #'char-code objid))

(defmethod to-object-id ((objid vector))
  ;; Assume an octet vector
  objid)

(defmethod to-object-id ((objid sequence))
  (coerce objid 'vector))


(defmethod to-object-id ((objid integer))
  (string-to-oid (princ-to-string objid)))


(defun object-id-to-integer (objid)
  (let ((buf (make-buffer :octets objid)))
    (unmarshal-ulong buf)))

(defun make-object-key (type poaid oid orb poa-name
                        &key (uniq *instance-id*) )
  ;; If poa-name use persistance v2
  (declare (optimize debug))
  (let* ((buffer (get-work-buffer orb))
         (octets (buffer-octets buffer)))
    (setq oid (to-object-id oid))
    (ecase type
      (:transient
       (marshal-ushort +transient-ior-magic+ buffer)
       (marshal-ushort poaid buffer)
       (marshal-ulong  uniq buffer))
      (:persistent
       (cond
        (poa-name
         (marshal-ushort +persistent-ior-magic2+ buffer)
         (marshal-ushort (length poa-name) buffer)
         (dolist (n poa-name)
           (marshal-string n buffer)))
        (t
         (marshal-ushort +persistent-ior-magic1+ buffer)
         (marshal-ushort poaid buffer)))))
    (let* ((prefix-len (length octets))
           (new-len (+ prefix-len (length oid))))
      (when (> new-len (array-total-size octets))
        (adjust-array octets new-len))
      (setf (fill-pointer octets) new-len)
      (setf (subseq octets prefix-len) oid))
    (copy-seq octets)))


;;;; Lisp mapping convenice functions

(defun portableserver:oid-to-string (oid)
  (map 'string #'code-char oid))

(defun portableserver:string-to-oid (string)
  (to-object-id string))
