(in-package :clorb)


(define-interface "outer_interface" ())

(define-module "example" ()
  (define-interface "inner_interface" ())
  (define-module "nested_inner_example" ()
    (define-interface "nested_inner_interface" ())
    (define-module "doubly_nested_inner_example" ())))


