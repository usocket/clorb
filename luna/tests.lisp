(in-package "NET.CDDR.LUNA")

(define-test-suite "Test Test"
  (variables
   (res (make-instance 'test-result :parent nil)))
  
  (define-test "tc-report"
    (let ((message
           (let ((*test-suite-result* res))
             (with-output-to-string (*standard-output*)
               ;; +++
               (tc-report "Foo ~A" "BAR")))))
      (ensure (search "Foo BAR" message)
              "should find 'Foo BAR' in message")
      (ensure-eql (result-errors res) 1)))
)
