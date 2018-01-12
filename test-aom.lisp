(in-package :clorb)

(define-test-suite "Active Object Map"
  (variables
   (aom (make-instance 'object-map))
   (s1 "servant1")
   (s2 "servant2")
   (oid1 (string-to-oid "oid1"))
   (oid2 (string-to-oid "oid2"))
   (oid3 (string-to-oid "oid3")))
  
  (define-test "basics"
    (add-activation aom oid1 s1)
    (ensure-eql (activation-count aom) 1)
    (ensure (servant-active-p aom s1))
    (ensure (not (servant-active-p aom s2)))
    (ensure-eql (oid-servant aom oid1) s1)
    (ensure-eql (servant-oid aom s1) oid1)
    (ensure-eql (servant-oid aom s2) nil)
    (ensure-equalp (servant-oid-list aom s1) (list oid1))
    (add-activation aom oid2 s2)
    (add-activation aom oid3 s2)
    (ensure-eql (activation-count aom) 3)
    (ensure (servant-active-p aom s1))
    (ensure (servant-active-p aom s2))
    (ensure-eql (oid-servant aom oid1) s1)
    (ensure-eql (oid-servant aom oid2) s2)
    (ensure-eql (oid-servant aom oid3) s2)
    (ensure-eql (servant-oid aom s1) oid1)
    (ensure-eql (servant-oid aom s2) t)
    (ensure-equalp (servant-oid-list aom s1) (list oid1))
    (ensure (null (set-exclusive-or (servant-oid-list aom s2) (list oid2 oid3))))
    (remove-activation aom oid2)
    (ensure-eql (activation-count aom) 2)
    (ensure (servant-active-p aom s1))
    (ensure (servant-active-p aom s2))
    (ensure-eql (oid-servant aom oid1) s1)
    (ensure-eql (oid-servant aom oid2) nil)
    (ensure-eql (oid-servant aom oid3) s2)
    (ensure-eql (servant-oid aom s1) oid1)
    (ensure-eql (servant-oid aom s2) oid3))
  

  (define-test "Map"
    (add-activation aom oid1 s1)
    (add-activation aom oid2 s2)
    (add-activation aom oid3 s2)
    (let ((oids nil)
          (servants nil))
      (map-activations aom
                       (lambda (oid servant)
                         (push oid oids)
                         (push servant servants)))
      (ensure-eql (length oids) 3)
      (ensure (member oid1 oids))
      (ensure (member oid2 oids))
      (ensure (member oid3 oids))
      (ensure (null (set-exclusive-or servants (list s1 s2))))))
  
  
  (define-test "Map Mutable"
    (add-activation aom oid1 s1)
    (add-activation aom oid2 s2)
    (add-activation aom oid3 s2)
    (let ((oids nil)
          (servants nil))
      (map-activations aom
                       (lambda (oid servant)
                         (remove-activation aom oid1)
                         (remove-activation aom oid2)
                         (remove-activation aom oid3)
                         (push oid oids)
                         (push servant servants))
                       t)
      (ensure-eql (length oids) 1)
      (ensure-eql (length servants) 1) ))




#|end|#)
