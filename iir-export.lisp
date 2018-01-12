(in-package :clorb)

(defclass IIRX-INTERFACE (interface-def)
  ((iir :initarg :iir :accessor iir)))

(defclass IIRX-OPERATION (operation-def)
  ((iir :initarg :iir :accessor iir)))

(define-method contents ((obj iirx-interface) kind ex)
  (unless (contents obj)
    (setf (contents obj)
      (map 'list
        (lambda (opdef)
          (make-instance 'iirx-operation
            :iir opdef
            :name (opdef-name opdef)))
        (interface-operations (iir obj)))))
  (call-next-method))


(define-method _get_result ((obj iirx-operation))
  (opdef-result (iir obj)))

(define-method _get_params ((obj iirx-operation))
  (opdef-params (iir obj)))


(defun get-iirx (interface)
  (make-instance 'iirx-interface
    :id (interface-id interface)
    :name "?"
    :iir interface
    :base_interfaces (map 'list 'get-iirx
                          (interface-inherit interface))))

(define-method _interface ((servant auto-servant))
  (let ((def (servant-interface servant)))
    (if (typep def 'interface)
        (get-iirx def)
      (call-next-method))))
