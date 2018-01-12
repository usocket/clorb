(in-package :clorb)

;; (enqf qplace object)
;; (deqf qplace)

(define-test-suite "queue"
  (define-test "enqf/deqf"
    (dolist (ex '(nil (1) (1 2) (a b c)))
      (let ((q nil))
        (dolist (x ex) (enqf q x))
        (dolist (x ex)
          (multiple-value-bind (v f) (deqf q)
            (ensure-eql v x)
            (ensure f)))
        (multiple-value-bind (v f) (deqf q)
          (ensure-eql v nil)
          (ensure (not f)))))))
