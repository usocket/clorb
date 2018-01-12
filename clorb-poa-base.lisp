;;;; clorb-poa-base.lisp -- basic types in PortablServer Module
;;
;; Most of this code has been generated.
;;

(in-package :clorb)


(DEFINE-ALIAS OMG.ORG/PORTABLESERVER:OBJECTID
 :id "IDL:omg.org/PortableServer/ObjectId:1.0"
 :name "ObjectId"
 :type SEQUENCE
 :typecode (create-sequence-tc 0 OMG.ORG/CORBA:TC_OCTET))

(DEFINE-ALIAS OMG.ORG/PORTABLESERVER:POALIST
 :id "IDL:omg.org/PortableServer/POAList:1.0"
 :name "POAList"
 :type SEQUENCE)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:FORWARDREQUEST
 :id "IDL:omg.org/PortableServer/ForwardRequest:1.0"
 :name "ForwardRequest"
 :members (("forward_reference" OMG.ORG/CORBA:tc_object)))


(DEFINE-ENUM OMG.ORG/PORTABLESERVER:REQUESTPROCESSINGPOLICYVALUE
 :id "IDL:omg.org/PortableServer/RequestProcessingPolicyValue:1.0"
 :name "RequestProcessingPolicyValue"
 :members ("USE_ACTIVE_OBJECT_MAP_ONLY" "USE_DEFAULT_SERVANT"
           "USE_SERVANT_MANAGER"))

(DEFINE-ENUM OMG.ORG/PORTABLESERVER:SERVANTRETENTIONPOLICYVALUE
 :id "IDL:omg.org/PortableServer/ServantRetentionPolicyValue:1.0"
 :name "ServantRetentionPolicyValue"
 :members ("RETAIN" "NON_RETAIN"))

(DEFINE-ENUM OMG.ORG/PORTABLESERVER:IMPLICITACTIVATIONPOLICYVALUE
 :id "IDL:omg.org/PortableServer/ImplicitActivationPolicyValue:1.0"
 :name "ImplicitActivationPolicyValue"
 :members ("IMPLICIT_ACTIVATION" "NO_IMPLICIT_ACTIVATION"))

(DEFINE-ENUM OMG.ORG/PORTABLESERVER:IDASSIGNMENTPOLICYVALUE
 :id "IDL:omg.org/PortableServer/IdAssignmentPolicyValue:1.0"
 :name "IdAssignmentPolicyValue"
 :members ("USER_ID" "SYSTEM_ID"))

(DEFINE-ENUM OMG.ORG/PORTABLESERVER:IDUNIQUENESSPOLICYVALUE
 :id "IDL:omg.org/PortableServer/IdUniquenessPolicyValue:1.0"
 :name "IdUniquenessPolicyValue"
 :members ("UNIQUE_ID" "MULTIPLE_ID"))

(DEFINE-ENUM OMG.ORG/PORTABLESERVER:LIFESPANPOLICYVALUE
 :id "IDL:omg.org/PortableServer/LifespanPolicyValue:1.0"
 :name "LifespanPolicyValue"
 :members ("TRANSIENT" "PERSISTENT"))

(DEFINE-ENUM OMG.ORG/PORTABLESERVER:THREADPOLICYVALUE
 :id "IDL:omg.org/PortableServer/ThreadPolicyValue:1.0"
 :name "ThreadPolicyValue"
 :members ("ORB_CTRL_MODEL" "SINGLE_THREAD_MODEL"))




(DEFINE-INTERFACE OMG.ORG/PORTABLESERVER:REQUESTPROCESSINGPOLICY (OMG.ORG/CORBA:POLICY)
 :proxy (OMG.ORG/PORTABLESERVER:REQUESTPROCESSINGPOLICY-PROXY
         OMG.ORG/PORTABLESERVER:REQUESTPROCESSINGPOLICY
         OMG.ORG/CORBA:POLICY-PROXY)
 :id "IDL:omg.org/PortableServer/RequestProcessingPolicy:1.0"
 :name "RequestProcessingPolicy")

(DEFINE-METHOD "VALUE" ((OBJ
                         OMG.ORG/PORTABLESERVER:REQUESTPROCESSINGPOLICY-PROXY))
  (GET-ATTRIBUTE
    OBJ
    "_get_value"
    (SYMBOL-TYPECODE 'OMG.ORG/PORTABLESERVER:REQUESTPROCESSINGPOLICYVALUE)))

(DEFINE-INTERFACE OMG.ORG/PORTABLESERVER:SERVANTRETENTIONPOLICY (OMG.ORG/CORBA:POLICY)
 :proxy (OMG.ORG/PORTABLESERVER:SERVANTRETENTIONPOLICY-PROXY
         OMG.ORG/PORTABLESERVER:SERVANTRETENTIONPOLICY
         OMG.ORG/CORBA:POLICY-PROXY)
 :id "IDL:omg.org/PortableServer/ServantRetentionPolicy:1.0"
 :name "ServantRetentionPolicy")

(DEFINE-METHOD "VALUE" ((OBJ
                         OMG.ORG/PORTABLESERVER:SERVANTRETENTIONPOLICY-PROXY))
  (GET-ATTRIBUTE
    OBJ
    "_get_value"
    (SYMBOL-TYPECODE 'OMG.ORG/PORTABLESERVER:SERVANTRETENTIONPOLICYVALUE)))

(DEFINE-INTERFACE OMG.ORG/PORTABLESERVER:IMPLICITACTIVATIONPOLICY(OMG.ORG/CORBA:POLICY)
 :proxy (OMG.ORG/PORTABLESERVER:IMPLICITACTIVATIONPOLICY-PROXY
         OMG.ORG/PORTABLESERVER:IMPLICITACTIVATIONPOLICY
         OMG.ORG/CORBA:POLICY-PROXY)
 :id "IDL:omg.org/PortableServer/ImplicitActivationPolicy:1.0"
 :name "ImplicitActivationPolicy")

(DEFINE-METHOD "VALUE" ((OBJ
                         OMG.ORG/PORTABLESERVER:IMPLICITACTIVATIONPOLICY-PROXY))
  (GET-ATTRIBUTE
    OBJ
    "_get_value"
    (SYMBOL-TYPECODE 'OMG.ORG/PORTABLESERVER:IMPLICITACTIVATIONPOLICYVALUE)))

(DEFINE-INTERFACE OMG.ORG/PORTABLESERVER:IDASSIGNMENTPOLICY (OMG.ORG/CORBA:POLICY)
 :proxy (OMG.ORG/PORTABLESERVER:IDASSIGNMENTPOLICY-PROXY
         OMG.ORG/PORTABLESERVER:IDASSIGNMENTPOLICY OMG.ORG/CORBA:POLICY-PROXY)
 :id "IDL:omg.org/PortableServer/IdAssignmentPolicy:1.0"
 :name "IdAssignmentPolicy")

(DEFINE-METHOD "VALUE" ((OBJ OMG.ORG/PORTABLESERVER:IDASSIGNMENTPOLICY-PROXY))
  (GET-ATTRIBUTE
    OBJ
    "_get_value"
    (SYMBOL-TYPECODE 'OMG.ORG/PORTABLESERVER:IDASSIGNMENTPOLICYVALUE)))

(DEFINE-INTERFACE OMG.ORG/PORTABLESERVER:IDUNIQUENESSPOLICY (OMG.ORG/CORBA:POLICY)
 :proxy (OMG.ORG/PORTABLESERVER:IDUNIQUENESSPOLICY-PROXY
         OMG.ORG/PORTABLESERVER:IDUNIQUENESSPOLICY OMG.ORG/CORBA:POLICY-PROXY)
 :id "IDL:omg.org/PortableServer/IdUniquenessPolicy:1.0"
 :name "IdUniquenessPolicy")

(DEFINE-METHOD "VALUE" ((OBJ OMG.ORG/PORTABLESERVER:IDUNIQUENESSPOLICY-PROXY))
  (GET-ATTRIBUTE
    OBJ
    "_get_value"
    (SYMBOL-TYPECODE 'OMG.ORG/PORTABLESERVER:IDUNIQUENESSPOLICYVALUE)))

(DEFINE-INTERFACE OMG.ORG/PORTABLESERVER:LIFESPANPOLICY (OMG.ORG/CORBA:POLICY)
 :proxy (OMG.ORG/PORTABLESERVER:LIFESPANPOLICY-PROXY
         OMG.ORG/PORTABLESERVER:LIFESPANPOLICY OMG.ORG/CORBA:POLICY-PROXY)
 :id "IDL:omg.org/PortableServer/LifespanPolicy:1.0"
 :name "LifespanPolicy")

(DEFINE-METHOD "VALUE" ((OBJ OMG.ORG/PORTABLESERVER:LIFESPANPOLICY-PROXY))
  (GET-ATTRIBUTE
    OBJ
    "_get_value"
    (SYMBOL-TYPECODE 'OMG.ORG/PORTABLESERVER:LIFESPANPOLICYVALUE)))

(DEFINE-INTERFACE OMG.ORG/PORTABLESERVER:THREADPOLICY (OMG.ORG/CORBA:POLICY)
 :proxy (OMG.ORG/PORTABLESERVER:THREADPOLICY-PROXY
         OMG.ORG/PORTABLESERVER:THREADPOLICY OMG.ORG/CORBA:POLICY-PROXY)
 :id "IDL:omg.org/PortableServer/ThreadPolicy:1.0"
 :name "ThreadPolicy")

(DEFINE-METHOD "VALUE" ((OBJ OMG.ORG/PORTABLESERVER:THREADPOLICY-PROXY))
  (GET-ATTRIBUTE
    OBJ
    "_get_value"
    (SYMBOL-TYPECODE 'OMG.ORG/PORTABLESERVER:THREADPOLICYVALUE)))



(defconstant omg.org/portableserver:request_processing_policy_id (quote 22))

(defconstant omg.org/portableserver:servant_retention_policy_id (quote 21))

(defconstant omg.org/portableserver:implicit_activation_policy_id (quote 20))

(defconstant omg.org/portableserver:id_assignment_policy_id (quote 19))

(defconstant omg.org/portableserver:id_uniqueness_policy_id (quote 18))

(defconstant omg.org/portableserver:lifespan_policy_id (quote 17))

(defconstant omg.org/portableserver:thread_policy_id (quote 16))






(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/WRONGPOLICY
 :id "IDL:omg.org/PortableServer/POA/WrongPolicy:1.0"
 :name "WrongPolicy"
 :members NIL)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/WRONGADAPTER
 :id "IDL:omg.org/PortableServer/POA/WrongAdapter:1.0"
 :name "WrongAdapter"
 :members NIL)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/SERVANTNOTACTIVE
 :id "IDL:omg.org/PortableServer/POA/ServantNotActive:1.0"
 :name "ServantNotActive"
 :members NIL)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/SERVANTALREADYACTIVE
 :id "IDL:omg.org/PortableServer/POA/ServantAlreadyActive:1.0"
 :name "ServantAlreadyActive"
 :members NIL)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/OBJECTNOTACTIVE
 :id "IDL:omg.org/PortableServer/POA/ObjectNotActive:1.0"
 :name "ObjectNotActive"
 :members NIL)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/OBJECTALREADYACTIVE
 :id "IDL:omg.org/PortableServer/POA/ObjectAlreadyActive:1.0"
 :name "ObjectAlreadyActive"
 :members NIL)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/NOSERVANT
 :id "IDL:omg.org/PortableServer/POA/NoServant:1.0"
 :name "NoServant"
 :members NIL)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/INVALIDPOLICY
 :id "IDL:omg.org/PortableServer/POA/InvalidPolicy:1.0"
 :name "InvalidPolicy"
 :members (("index" OMG.ORG/CORBA:TC_USHORT)))

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/ADAPTERNONEXISTENT
 :id "IDL:omg.org/PortableServer/POA/AdapterNonExistent:1.0"
 :name "AdapterNonExistent"
 :members NIL)

(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:POA/ADAPTERALREADYEXISTS
 :id "IDL:omg.org/PortableServer/POA/AdapterAlreadyExists:1.0"
 :name "AdapterAlreadyExists"
 :members NIL)


(DEFINE-USER-EXCEPTION OMG.ORG/PORTABLESERVER:CURRENT/NOCONTEXT
 :id "IDL:omg.org/PortableServer/Current/NoContext:1.0"
 :name "NoContext"
 :members NIL)