(in-package :clorb)

(define-servant event-channel-factory
    "CLORB_01::EventChannelFactory")

(define-method new_channel ((factory event-channel-factory))
  (make-instance 'event-channel))


(defun setup-ecf ()
  (rebind (make-instance 'event-channel-factory)
          "ec-factory"))
