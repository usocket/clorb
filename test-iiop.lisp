(in-package :clorb)

(define-test-suite "Misc IIOP"

  (define-test "Long iiop profile"
      (let* ((orb *the-orb*)
             (profile (IIOP:ProfileBody_1_1 
                       :iiop_version (IIOP:Version :major 1 :minor 3)
                       :host "host"
                       :port 2993
                       :object_key #(1 2 3)
                       :components ()))
             (ior (IOP:IOR :type_id "IDL:my-type:1.0"
                           :profiles
                           (list (iop:taggedprofile
                                  :tag iop:tag_internet_iop
                                  :profile_data
                                  (marshal-make-encapsulation 
                                   (lambda (buffer)
                                     (marshal-any-value profile buffer) 
                                     ;; Extra data
                                     (marshal-string "extra data" buffer))
                                   orb))))))
        ;; IOR constructed for future version of iiop with extra data in profile after components
        (let ((proxy (let ((buffer (get-work-buffer orb)))
                       (marshal-any-value ior buffer)
                       (unmarshal-object buffer))))
          ;; received future profile
          (let ((buffer (get-work-buffer orb)))
            ;; marshal it and check that the extra data is preserved
            (marshal-object proxy buffer)
            (let ((ior2 (unmarshal (symbol-typecode 'IOP:IOR) buffer)))
              (ensure-pattern* ior2 'op:type_id "IDL:my-type:1.0"
                               'op:profiles (sequence-pattern (isa 'iop:taggedprofile)))
              (unmarshal-encapsulation 
               (op:profile_data (elt (op:profiles ior) 0))
               (lambda (buffer)
                 (let ((profile2 (unmarshal (symbol-typecode 'IIOP:ProfileBody_1_1) buffer)))
                   (ensure-pattern* profile2 'op:host "host" 'op:port 2993)
                   (ensure-equalp (unmarshal-string buffer) "extra data")))))))))


#| end |# )



