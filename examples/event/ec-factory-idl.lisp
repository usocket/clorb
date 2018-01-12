(in-package :clorb)


;;(import-module :name "CosEventChannelAdmin"
;;               :id "IDL:omg.org/CosEventChannelAdmin:1.0")

(require-idl "CosEventChannelAdmin"
             :file "cosevent-idl")

(idef-definitions
(with-prefix "lennarts.infotek.no"
  (define-module "CLORB_01" ()
    (define-interface "EventChannelFactory" ()
      (define-operation "new_channel" ()
        :result-type "CosEventChannelAdmin::EventChannel")))))


