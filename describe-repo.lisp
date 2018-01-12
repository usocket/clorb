(in-package :clorb)

(defun describe-repo (r)
  (let ((l (coerce (op:contents (object-narrow r 'corba:container)
                                :dk_all t) 'list))
        (k (op:def_kind r)))
    (pprint-logical-block (*standard-output* l :prefix "  ")
      (loop for x in l
            for kind = (op:def_kind x)
            for f = nil then t
            do (when f (pprint-newline (if (eq k :dk_interface)
                                         :fill :mandatory)))
            (format *standard-output* "~A ~S  "
                    (op:name x) kind)
            (when (eq kind :dk_module)
              (pprint-newline :mandatory)
              (describe-repo x))))))
