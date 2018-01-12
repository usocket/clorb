(in-package :clorb)

(define-test-suite "mt queues"

  (define-test "shared-queue"
    (let ((q (make-instance 'shared-queue)))
      (start-process "test"
                     (lambda ()
                       (sleep 0.1)
                       (loop for i in '(1 2 3) do (enqueue q i))))
      (ensure-eql (dequeue q) 1)
      (ensure-eql (dequeue q) 2)
      (ensure-eql (dequeue q) 3)
      (ensure-eql (dequeue q t) nil)))

  (define-test "execution-queue"
    (let ((ansq (make-instance 'shared-queue))
          (eq (make-instance 'execution-queue
                             :max-idle 0 :max-processes 1)))
      (enqueue eq (lambda () (enqueue ansq 17)))
      (enqueue eq (lambda () (enqueue ansq 18)))
      (ensure-eql (dequeue ansq) 17)
      (ensure-eql (dequeue ansq) 18)
      (sleep 0.1)
      (ensure-eql (process-count eq) 0)))

  )
