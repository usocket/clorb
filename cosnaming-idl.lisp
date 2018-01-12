
(in-package :clorb)
(IDEF-DEFINITIONS
  (WITH-PREFIX "omg.org"
   (DEFINE-MODULE "CosNaming" NIL (DEFINE-TYPE "Istring" STRING)
    (DEFINE-STRUCT "NameComponent"
     (("id" "CosNaming::Istring") ("kind" "CosNaming::Istring")))
    (DEFINE-TYPE "Name" (sequence "CosNaming::NameComponent" 0))
    (DEFINE-ENUM "BindingType" ("nobject" "ncontext"))
    (DEFINE-STRUCT "Binding"
     (("binding_name" "CosNaming::Name")
      ("binding_type" "CosNaming::BindingType")))
    (DEFINE-TYPE "BindingList" (sequence "CosNaming::Binding" 0))
    (DEFINE-INTERFACE "NamingContext" NIL
     (DEFINE-ENUM "NotFoundReason" ("missing_node" "not_context" "not_object"))
     (DEFINE-EXCEPTION "NotFound"
      (("why" "CosNaming::NamingContext::NotFoundReason")
       ("rest_of_name" "CosNaming::Name")))
     (DEFINE-EXCEPTION "CannotProceed"
      (("cxt" "CosNaming::NamingContext") ("rest_of_name" "CosNaming::Name")))
     (DEFINE-EXCEPTION "InvalidName" NIL) (DEFINE-EXCEPTION "AlreadyBound" NIL)
     (DEFINE-EXCEPTION "NotEmpty" NIL)
     (DEFINE-OPERATION
       "bind"
       ((:param_in "n" "CosNaming::Name") (:param_in "obj" OBJECT))
       :result-type
       VOID
       :exceptions
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"
        "CosNaming::NamingContext::AlreadyBound"))
     (DEFINE-OPERATION
       "rebind"
       ((:param_in "n" "CosNaming::Name") (:param_in "obj" OBJECT))
       :result-type
       VOID
       :exceptions
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "bind_context"
       ((:param_in "n" "CosNaming::Name")
        (:param_in "nc" "CosNaming::NamingContext"))
       :result-type
       VOID
       :exceptions
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"
        "CosNaming::NamingContext::AlreadyBound"))
     (DEFINE-OPERATION
       "rebind_context"
       ((:param_in "n" "CosNaming::Name")
        (:param_in "nc" "CosNaming::NamingContext"))
       :result-type
       VOID
       :exceptions
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "resolve"
       ((:param_in "n" "CosNaming::Name"))
       :result-type
       OBJECT
       :exceptions
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "unbind"
       ((:param_in "n" "CosNaming::Name"))
       :result-type
       VOID
       :exceptions
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "new_context"
       NIL
       :result-type
       "CosNaming::NamingContext"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "bind_new_context"
       ((:param_in "n" "CosNaming::Name"))
       :result-type
       "CosNaming::NamingContext"
       :exceptions
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::AlreadyBound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "destroy"
       NIL
       :result-type
       VOID
       :exceptions
       ("CosNaming::NamingContext::NotEmpty"))
     (DEFINE-OPERATION
       "list"
       ((:param_in "how_many" ULONG) (:param_out "bl" "CosNaming::BindingList")
        (:param_out "bi" "CosNaming::BindingIterator"))
       :result-type
       VOID
       :exceptions
       NIL))
    (DEFINE-INTERFACE "BindingIterator" NIL
     (DEFINE-OPERATION
       "next_one"
       ((:param_out "b" "CosNaming::Binding"))
       :result-type
       BOOLEAN
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "next_n"
       ((:param_in "how_many" ULONG)
        (:param_out "bl" "CosNaming::BindingList"))
       :result-type
       BOOLEAN
       :exceptions
       NIL)
     (DEFINE-OPERATION "destroy" NIL :result-type VOID :exceptions NIL))
    (DEFINE-INTERFACE "NamingContextExt" (:bases ("CosNaming::NamingContext"))
     (DEFINE-TYPE "StringName" STRING) (DEFINE-TYPE "Address" STRING)
     (DEFINE-TYPE "URLString" STRING)
     (DEFINE-OPERATION
       "to_string"
       ((:param_in "n" "CosNaming::Name"))
       :result-type
       "CosNaming::NamingContextExt::StringName"
       :exceptions
       ("CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "to_name"
       ((:param_in "sn" "CosNaming::NamingContextExt::StringName"))
       :result-type
       "CosNaming::Name"
       :exceptions
       ("CosNaming::NamingContext::InvalidName"))
     (DEFINE-EXCEPTION "InvalidAddress" NIL)
     (DEFINE-OPERATION
       "to_url"
       ((:param_in "addr" "CosNaming::NamingContextExt::Address")
        (:param_in "sn" "CosNaming::NamingContextExt::StringName"))
       :result-type
       "CosNaming::NamingContextExt::URLString"
       :exceptions
       ("CosNaming::NamingContextExt::InvalidAddress"
        "CosNaming::NamingContext::InvalidName"))
     (DEFINE-OPERATION
       "resolve_str"
       ((:param_in "n" "CosNaming::NamingContextExt::StringName"))
       :result-type
       OBJECT
       :exceptions
       ("CosNaming::NamingContext::NotFound"
        "CosNaming::NamingContext::CannotProceed"
        "CosNaming::NamingContext::InvalidName"
        "CosNaming::NamingContext::AlreadyBound"))))))