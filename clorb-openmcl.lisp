;;; clorb-openmcl.lisp  --  OpenMCL special system depenent functions

(in-package :net.cddr.clorb)


;; unix-select -- similar to same named function in sbcl

#+openmcl
(defun unix-select (maxn rset wset xset timeout-sec &optional (to-usecs 0))
  (ccl::rletZ ((tv :timeval))
    (when timeout-sec
      (setf (ccl:pref tv :timeval.tv_sec) timeout-sec
            (ccl:pref tv :timeval.tv_usec) to-usecs))
    (ccl::%stack-block ((outfds ccl::*fd-set-size*)
                        (inpfds ccl::*fd-set-size*)
                        (errfds ccl::*fd-set-size*))
      (ccl::fd-zero outfds)
      (ccl::fd-zero inpfds)
      (ccl::fd-zero errfds)
      (loop for fd from 0 to maxn 
           when (logbitp fd wset) do (ccl::fd-set fd outfds)
           when (logbitp fd rset) do (ccl::fd-set fd inpfds)
           when (logbitp fd xset) do (ccl::fd-set fd errfds))
      (let* ((res (#_select maxn inpfds outfds errfds
			    (if timeout-sec tv (ccl::%null-ptr)))))
        (setq wset 0 rset 0 xset 0)
        (when (> res 0)
          (loop for fd from 0 to maxn 
               do (when (ccl::fd-is-set fd inpfds)
                    (setf rset (logior rset (ash 1 fd))))
               do (when (ccl::fd-is-set fd outfds)
                    (setf wset (logior wset (ash 1 fd))))
               do (when (ccl::fd-is-set fd errfds) 
                    (setf xset (logior xset (ash 1 fd))))))
        (values res rset wset xset)))))
