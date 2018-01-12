(in-package :clorb)

(IDEF-DEFINITIONS
(with-prefix "omg.org"
(DEFINE-MODULE "CosEventChannelAdmin" NIL
 (DEFINE-EXCEPTION "AlreadyConnected" NIL)
 (DEFINE-EXCEPTION "TypeError" NIL)

 (DEFINE-INTERFACE "ProxyPushConsumer" (:bases ("CosEventComm::PushConsumer"))
  (DEFINE-OPERATION "connect_push_supplier"
   ((:param_in "push_supplier" "CosEventComm::PushSupplier")) 
   :result-type VOID 
   :exceptions ("CosEventChannelAdmin::AlreadyConnected")))

 (DEFINE-INTERFACE "ProxyPullSupplier" (:bases ("CosEventComm::PullSupplier"))
  (DEFINE-OPERATION "connect_pull_consumer"
      ((:param_in "pull_consumer" "CosEventComm::PullConsumer")) 
    :result-type VOID
    :exceptions ("CosEventChannelAdmin::AlreadyConnected")))

 (DEFINE-INTERFACE "ProxyPullConsumer" (:bases ("CosEventComm::PullConsumer"))
  (DEFINE-OPERATION "connect_pull_supplier"
      ((:param_in "pull_supplier" "CosEventComm::PullSupplier")) 
    :result-type VOID 
    :exceptions ("CosEventChannelAdmin::AlreadyConnected"
    "CosEventChannelAdmin::TypeError")))

 (DEFINE-INTERFACE "ProxyPushSupplier" (:bases ("CosEventComm::PushSupplier"))
  (DEFINE-OPERATION "connect_push_consumer"
      ((:param_in "push_consumer" "CosEventComm::PushConsumer")) 
    :result-type VOID
    :exceptions ("CosEventChannelAdmin::AlreadyConnected"
                 "CosEventChannelAdmin::TypeError")))
 (DEFINE-INTERFACE "ConsumerAdmin" (:bases NIL)
   (DEFINE-OPERATION "obtain_push_supplier" NIL 
     :result-type "CosEventChannelAdmin::ProxyPushSupplier" 
     :exceptions NIL)
   (DEFINE-OPERATION "obtain_pull_supplier" NIL 
     :result-type "CosEventChannelAdmin::ProxyPullSupplier" 
     :exceptions NIL))

 (DEFINE-INTERFACE "SupplierAdmin" (:bases NIL)
   (DEFINE-OPERATION "obtain_push_consumer" NIL 
     :result-type "CosEventChannelAdmin::ProxyPushConsumer" 
     :exceptions NIL)
   (DEFINE-OPERATION "obtain_pull_consumer" NIL 
     :result-type "CosEventChannelAdmin::ProxyPullConsumer" 
     :exceptions NIL))

 (DEFINE-INTERFACE "EventChannel" (:bases NIL)
   (DEFINE-OPERATION "for_consumers" NIL 
     :result-type "CosEventChannelAdmin::ConsumerAdmin" 
     :exceptions NIL)
   (DEFINE-OPERATION "for_suppliers" NIL 
     :result-type "CosEventChannelAdmin::SupplierAdmin" 
     :exceptions NIL)
   (DEFINE-OPERATION "destroy" NIL 
     :result-type VOID 
     :exceptions NIL))) 

(DEFINE-MODULE "CosEventComm" ()
  (DEFINE-EXCEPTION "Disconnected" NIL)

  (DEFINE-INTERFACE "PushConsumer" (:bases NIL)
    (DEFINE-OPERATION "push" ((:param_in "data" ANY)) 
      :result-type VOID 
      :exceptions ("CosEventComm::Disconnected"))
    (DEFINE-OPERATION "disconnect_push_consumer" NIL 
      :result-type VOID 
      :exceptions NIL))

  (DEFINE-INTERFACE "PushSupplier" (:bases NIL)
    (DEFINE-OPERATION "disconnect_push_supplier" NIL 
      :result-type VOID
      :exceptions NIL))

  (DEFINE-INTERFACE "PullSupplier" (:bases NIL)
    (DEFINE-OPERATION "pull" NIL 
      :result-type ANY 
      :exceptions ("CosEventComm::Disconnected"))
    (DEFINE-OPERATION "try_pull" ((:param_out "has_event" BOOLEAN)) 
      :result-type ANY
      :exceptions ("CosEventComm::Disconnected"))
    (DEFINE-OPERATION "disconnect_pull_supplier" NIL 
      :result-type VOID
      :exceptions NIL))

  (DEFINE-INTERFACE "PullConsumer" (:bases NIL)
    (DEFINE-OPERATION "disconnect_pull_consumer" NIL 
      :result-type VOID
      :exceptions NIL))))) 
