;;;; Test clorb-util

(in-package :clorb)


(define-test-suite "Clorb Utils"
  
  
  ;; Accessing Interface Repositories
  
  (define-test "repository-facade"
    (let ((facade (make-instance 'repository-facade))
          (r1 (make-instance 'repository))
          (r2 (make-instance 'repository)))
      
      (add-repository facade r1)
      (add-idl-repository facade "fie-foe" r2)
      
      (op:create_module r1 "IDL:fie:1.0" "fie" "1.0")
      (op:create_module r2 "IDL:foo:1.0" "foo" "1.0")
      
      (ensure-typep (op:lookup_id facade "IDL:fie:1.0") 'CORBA:ModuleDef)
      (ensure-typep (op:lookup_id facade "IDL:foo:1.0") 'CORBA:ModuleDef)    
      
      (setq r2 (make-instance 'repository))
      (op:create_module r2 "IDL:bar:1.0" "bar" "1.0")
      
      (ensure (null (op:lookup_id facade "IDL:bar:1.0")))
      (add-idl-repository facade "fie-foe" r2)
      (ensure-typep (op:lookup_id facade "IDL:bar:1.0") 'CORBA:ModuleDef)
      (ensure (null (op:lookup_id facade "IDL:foo:1.0")))))
  
  
  ;; file URL
  
  (define-test "pathname-url"
    (let ((url (pathname-url (merge-pathnames "orb.idl"
                                              (first *default-include-directories*)))))
      (ensure (string-starts-with url "file:"))
      #|???|#))
  
  
  ;; name service
  
  (define-test "ns-name"
    (ensure-pattern 
     (ns-name "dev/radio/foo.serv")
     (sequence-pattern
      (struct-pattern 'struct-class-name 'CosNaming:NameComponent 'op:id "dev" 'op:kind "")
      (pattern 'op:id "radio" 'op:kind "")
      (pattern 'op:id "foo" 'op:kind "serv"))))
  
  )
