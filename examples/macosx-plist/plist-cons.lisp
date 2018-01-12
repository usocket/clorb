(in-package :cl-user)

(packer:require-package :net.cddr.cf)

;; need plist.idl
(CORBA:IDL "clorb:examples;macosx-plist;plist.idl")


(defun plist-valuetype-cons (type value)
  (ecase type
    (:number  (make-instance 'macosx:plistnumber :value value))
    (:string  (make-instance 'macosx:pliststring :value value))
    (:boolean (make-instance 'macosx:plistboolean :value value))
    (:array (make-instance 'macosx:plistarray :values value))
    (:dictionary
     (make-instance 'macosx:stringkeydictionary
       :keys (mapcar #'op:value (mapcar #'car value))
       :values (mapcar #'cdr value)))))



(corba:define-method lookup_string ((dict macosx:stringkeydictionary) key)
  (let ((index (position key (op:keys dict) :test #'equal)))
    (when index
      (elt (op:values dict) index))))

(corba:define-method lookup ((dict macosx:stringkeydictionary) key)
  (check-type key macosx:pliststring)
  (op:lookup_string dict (op:value key)))

(corba:define-method add ((dict macosx:stringkeydictionary) key value)
  (check-type key macosx:PListString)
  (check-type value macosx:PList)
  (flet ((scons (elem seq)
           (cons elem (if (consp seq)
                        seq
                        (coerce seq 'list)))))
    (setf (op:keys dict) (scons (op:value key) (op:keys dict))
          (op:values dict) (scons value (op:values dict))))
  (values))




(net.cddr.cf::read-plist "/Users/lenst/Library/Safari/Bookmarks.plist"
           :constructor #'plist-valuetype-cons)
