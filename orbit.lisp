
(in-package :clorb)

(defparameter *principal*
  #.(with-open-file (i "/tmp/orbit-lenst/cookie")
      (let* ((s (make-string 2048))      ;; FIXME: is it always <2k?
             (l (read-sequence s i)))
        (setf (elt s l) (code-char 0))   ;; extra \0 needed too ..
        (subseq s 0 (1+ l))))
  "Octet sequence used for the principal field in the GIOP message.
Used by ORBit for its cookie.")
