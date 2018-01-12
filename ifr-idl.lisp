
(in-package :clorb)
(IDEF-DEFINITIONS
  (WITH-PREFIX "omg.org"
   (DEFINE-MODULE "CORBA" NIL (DEFINE-TYPE "Identifier" STRING)
    (DEFINE-TYPE "ScopedName" STRING) (DEFINE-TYPE "RepositoryId" STRING)
    (DEFINE-ENUM "DefinitionKind"
     ("dk_none" "dk_all" "dk_Attribute" "dk_Constant" "dk_Exception"
      "dk_Interface" "dk_Module" "dk_Operation" "dk_Typedef" "dk_Alias"
      "dk_Struct" "dk_Union" "dk_Enum" "dk_Primitive" "dk_String" "dk_Sequence"
      "dk_Array" "dk_Repository" "dk_Wstring" "dk_Fixed"))
    (DEFINE-INTERFACE "IRObject" NIL
     (DEFINE-ATTRIBUTE "def_kind" "CORBA::DefinitionKind" :readonly T)
     (DEFINE-OPERATION "destroy" NIL :result-type VOID :exceptions NIL))
    (DEFINE-TYPE "VersionSpec" STRING)
    (DEFINE-INTERFACE "Contained" (:bases ("CORBA::IRObject"))
     (DEFINE-ATTRIBUTE "id" "CORBA::RepositoryId")
     (DEFINE-ATTRIBUTE "name" "CORBA::Identifier")
     (DEFINE-ATTRIBUTE "version" "CORBA::VersionSpec")
     (DEFINE-ATTRIBUTE "defined_in" "CORBA::Container" :readonly T)
     (DEFINE-ATTRIBUTE "absolute_name" "CORBA::ScopedName" :readonly T)
     (DEFINE-ATTRIBUTE "containing_repository" "CORBA::Repository" :readonly T)
     (DEFINE-STRUCT "Description"
      (("kind" "CORBA::DefinitionKind") ("value" ANY)))
     (DEFINE-OPERATION
       "describe"
       NIL
       :result-type
       "CORBA::Contained::Description"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "move"
       ((:param_in "new_container" "CORBA::Container")
        (:param_in "new_name" "CORBA::Identifier")
        (:param_in "new_version" "CORBA::VersionSpec"))
       :result-type
       VOID
       :exceptions
       NIL))
    (DEFINE-TYPE "InterfaceDefSeq" (sequence "CORBA::InterfaceDef" 0))
    (DEFINE-TYPE "ContainedSeq" (sequence "CORBA::Contained" 0))
    (DEFINE-STRUCT "StructMember"
     (("name" "CORBA::Identifier") ("type" TYPECODE)
      ("type_def" "CORBA::IDLType")))
    (DEFINE-TYPE "StructMemberSeq" (sequence "CORBA::StructMember" 0))
    (DEFINE-STRUCT "UnionMember"
     (("name" "CORBA::Identifier") ("label" ANY) ("type" TYPECODE)
      ("type_def" "CORBA::IDLType")))
    (DEFINE-TYPE "UnionMemberSeq" (sequence "CORBA::UnionMember" 0))
    (DEFINE-TYPE "EnumMemberSeq" (sequence "CORBA::Identifier" 0))
    (DEFINE-INTERFACE "Container" (:bases ("CORBA::IRObject"))
     (DEFINE-OPERATION
       "lookup"
       ((:param_in "search_name" "CORBA::ScopedName"))
       :result-type
       "CORBA::Contained"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "contents"
       ((:param_in "limit_type" "CORBA::DefinitionKind")
        (:param_in "exclude_inherited" BOOLEAN))
       :result-type
       "CORBA::ContainedSeq"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "lookup_name"
       ((:param_in "search_name" "CORBA::Identifier")
        (:param_in "levels_to_search" LONG)
        (:param_in "limit_type" "CORBA::DefinitionKind")
        (:param_in "exclude_inherited" BOOLEAN))
       :result-type
       "CORBA::ContainedSeq"
       :exceptions
       NIL)
     (DEFINE-STRUCT "Description"
      (("contained_object" "CORBA::Contained") ("kind" "CORBA::DefinitionKind")
       ("value" ANY)))
     (DEFINE-TYPE "DescriptionSeq"
      (sequence "CORBA::Container::Description" 0))
     (DEFINE-OPERATION
       "describe_contents"
       ((:param_in "limit_type" "CORBA::DefinitionKind")
        (:param_in "exclude_inherited" BOOLEAN)
        (:param_in "max_returned_objs" LONG))
       :result-type
       "CORBA::Container::DescriptionSeq"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_module"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec"))
       :result-type
       "CORBA::ModuleDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_constant"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "type" "CORBA::IDLType") (:param_in "value" ANY))
       :result-type
       "CORBA::ConstantDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_struct"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "members" "CORBA::StructMemberSeq"))
       :result-type
       "CORBA::StructDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_exception"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "members" "CORBA::StructMemberSeq"))
       :result-type
       "CORBA::ExceptionDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_union"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "discriminator_type" "CORBA::IDLType")
        (:param_in "members" "CORBA::UnionMemberSeq"))
       :result-type
       "CORBA::UnionDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_enum"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "members" "CORBA::EnumMemberSeq"))
       :result-type
       "CORBA::EnumDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_alias"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "original_type" "CORBA::IDLType"))
       :result-type
       "CORBA::AliasDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_interface"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "base_interfaces" "CORBA::InterfaceDefSeq"))
       :result-type
       "CORBA::InterfaceDef"
       :exceptions
       NIL))
    (DEFINE-INTERFACE "IDLType" (:bases ("CORBA::IRObject"))
     (DEFINE-ATTRIBUTE "type" TYPECODE :readonly T))
    (DEFINE-ENUM "PrimitiveKind"
     ("pk_null" "pk_void" "pk_short" "pk_long" "pk_ushort" "pk_ulong"
      "pk_float" "pk_double" "pk_boolean" "pk_char" "pk_octet" "pk_any"
      "pk_TypeCode" "pk_Principal" "pk_string" "pk_objref" "pk_longlong"
      "pk_ulonglong" "pk_longdouble" "pk_wchar" "pk_wstring"))
    (DEFINE-INTERFACE "Repository" (:bases ("CORBA::Container"))
     (DEFINE-OPERATION
       "lookup_id"
       ((:param_in "search_id" "CORBA::RepositoryId"))
       :result-type
       "CORBA::Contained"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "get_primitive"
       ((:param_in "kind" "CORBA::PrimitiveKind"))
       :result-type
       "CORBA::PrimitiveDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_string"
       ((:param_in "bound" ULONG))
       :result-type
       "CORBA::StringDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_wstring"
       ((:param_in "bound" ULONG))
       :result-type
       "CORBA::WstringDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_sequence"
       ((:param_in "bound" ULONG) (:param_in "element_type" "CORBA::IDLType"))
       :result-type
       "CORBA::SequenceDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_array"
       ((:param_in "length" ULONG) (:param_in "element_type" "CORBA::IDLType"))
       :result-type
       "CORBA::ArrayDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_fixed"
       ((:param_in "digits" USHORT) (:param_in "scale" SHORT))
       :result-type
       "CORBA::FixedDef"
       :exceptions
       NIL))
    (DEFINE-INTERFACE "ModuleDef"
     (:bases ("CORBA::Container" "CORBA::Contained")))
    (DEFINE-STRUCT "ModuleDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")))
    (DEFINE-INTERFACE "ConstantDef" (:bases ("CORBA::Contained"))
     (DEFINE-ATTRIBUTE "type" TYPECODE :readonly T)
     (DEFINE-ATTRIBUTE "type_def" "CORBA::IDLType")
     (DEFINE-ATTRIBUTE "value" ANY))
    (DEFINE-STRUCT "ConstantDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("type" TYPECODE) ("value" ANY)))
    (DEFINE-INTERFACE "TypedefDef"
     (:bases ("CORBA::Contained" "CORBA::IDLType")))
    (DEFINE-STRUCT "TypeDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("type" TYPECODE)))
    (DEFINE-INTERFACE "StructDef" (:bases ("CORBA::TypedefDef"))
     (DEFINE-ATTRIBUTE "members" "CORBA::StructMemberSeq"))
    (DEFINE-INTERFACE "UnionDef" (:bases ("CORBA::TypedefDef"))
     (DEFINE-ATTRIBUTE "discriminator_type" TYPECODE :readonly T)
     (DEFINE-ATTRIBUTE "discriminator_type_def" "CORBA::IDLType")
     (DEFINE-ATTRIBUTE "members" "CORBA::UnionMemberSeq"))
    (DEFINE-INTERFACE "EnumDef" (:bases ("CORBA::TypedefDef"))
     (DEFINE-ATTRIBUTE "members" "CORBA::EnumMemberSeq"))
    (DEFINE-INTERFACE "AliasDef" (:bases ("CORBA::TypedefDef"))
     (DEFINE-ATTRIBUTE "original_type_def" "CORBA::IDLType"))
    (DEFINE-INTERFACE "PrimitiveDef" (:bases ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "kind" "CORBA::PrimitiveKind" :readonly T))
    (DEFINE-INTERFACE "StringDef" (:bases ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "bound" ULONG))
    (DEFINE-INTERFACE "WstringDef" (:bases ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "bound" ULONG))
    (DEFINE-INTERFACE "FixedDef" (:bases ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "digits" USHORT) (DEFINE-ATTRIBUTE "scale" SHORT))
    (DEFINE-INTERFACE "SequenceDef" (:bases ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "bound" ULONG)
     (DEFINE-ATTRIBUTE "element_type" TYPECODE :readonly T)
     (DEFINE-ATTRIBUTE "element_type_def" "CORBA::IDLType"))
    (DEFINE-INTERFACE "ArrayDef" (:bases ("CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "length" ULONG)
     (DEFINE-ATTRIBUTE "element_type" TYPECODE :readonly T)
     (DEFINE-ATTRIBUTE "element_type_def" "CORBA::IDLType"))
    (DEFINE-INTERFACE "ExceptionDef" (:bases ("CORBA::Contained"))
     (DEFINE-ATTRIBUTE "type" TYPECODE :readonly T)
     (DEFINE-ATTRIBUTE "members" "CORBA::StructMemberSeq"))
    (DEFINE-STRUCT "ExceptionDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("type" TYPECODE)))
    (DEFINE-ENUM "AttributeMode" ("ATTR_NORMAL" "ATTR_READONLY"))
    (DEFINE-INTERFACE "AttributeDef" (:bases ("CORBA::Contained"))
     (DEFINE-ATTRIBUTE "type" TYPECODE :readonly T)
     (DEFINE-ATTRIBUTE "type_def" "CORBA::IDLType")
     (DEFINE-ATTRIBUTE "mode" "CORBA::AttributeMode"))
    (DEFINE-STRUCT "AttributeDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("type" TYPECODE) ("mode" "CORBA::AttributeMode")))
    (DEFINE-ENUM "OperationMode" ("OP_NORMAL" "OP_ONEWAY"))
    (DEFINE-ENUM "ParameterMode" ("PARAM_IN" "PARAM_OUT" "PARAM_INOUT"))
    (DEFINE-STRUCT "ParameterDescription"
     (("name" "CORBA::Identifier") ("type" TYPECODE)
      ("type_def" "CORBA::IDLType") ("mode" "CORBA::ParameterMode")))
    (DEFINE-TYPE "ParDescriptionSeq"
     (sequence "CORBA::ParameterDescription" 0))
    (DEFINE-TYPE "ContextIdentifier" "CORBA::Identifier")
    (DEFINE-TYPE "ContextIdSeq" (sequence "CORBA::ContextIdentifier" 0))
    (DEFINE-TYPE "ExceptionDefSeq" (sequence "CORBA::ExceptionDef" 0))
    (DEFINE-TYPE "ExcDescriptionSeq"
     (sequence "CORBA::ExceptionDescription" 0))
    (DEFINE-INTERFACE "OperationDef" (:bases ("CORBA::Contained"))
     (DEFINE-ATTRIBUTE "result" TYPECODE :readonly T)
     (DEFINE-ATTRIBUTE "result_def" "CORBA::IDLType")
     (DEFINE-ATTRIBUTE "params" "CORBA::ParDescriptionSeq")
     (DEFINE-ATTRIBUTE "mode" "CORBA::OperationMode")
     (DEFINE-ATTRIBUTE "contexts" "CORBA::ContextIdSeq")
     (DEFINE-ATTRIBUTE "exceptions" "CORBA::ExceptionDefSeq"))
    (DEFINE-STRUCT "OperationDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("result" TYPECODE) ("mode" "CORBA::OperationMode")
      ("contexts" "CORBA::ContextIdSeq")
      ("parameters" "CORBA::ParDescriptionSeq")
      ("exceptions" "CORBA::ExcDescriptionSeq")))
    (DEFINE-TYPE "RepositoryIdSeq" (sequence "CORBA::RepositoryId" 0))
    (DEFINE-TYPE "OpDescriptionSeq" (sequence "CORBA::OperationDescription" 0))
    (DEFINE-TYPE "AttrDescriptionSeq"
     (sequence "CORBA::AttributeDescription" 0))
    (DEFINE-INTERFACE "InterfaceDef"
     (:bases ("CORBA::Container" "CORBA::Contained" "CORBA::IDLType"))
     (DEFINE-ATTRIBUTE "base_interfaces" "CORBA::InterfaceDefSeq")
     (DEFINE-OPERATION
       "is_a"
       ((:param_in "interface_id" "CORBA::RepositoryId"))
       :result-type
       BOOLEAN
       :exceptions
       NIL)
     (DEFINE-STRUCT "FullInterfaceDescription"
      (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
       ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
       ("operations" "CORBA::OpDescriptionSeq")
       ("attributes" "CORBA::AttrDescriptionSeq")
       ("base_interfaces" "CORBA::RepositoryIdSeq") ("type" TYPECODE)))
     (DEFINE-OPERATION
       "describe_interface"
       NIL
       :result-type
       "CORBA::InterfaceDef::FullInterfaceDescription"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_attribute"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "type" "CORBA::IDLType")
        (:param_in "mode" "CORBA::AttributeMode"))
       :result-type
       "CORBA::AttributeDef"
       :exceptions
       NIL)
     (DEFINE-OPERATION
       "create_operation"
       ((:param_in "id" "CORBA::RepositoryId")
        (:param_in "name" "CORBA::Identifier")
        (:param_in "version" "CORBA::VersionSpec")
        (:param_in "result" "CORBA::IDLType")
        (:param_in "mode" "CORBA::OperationMode")
        (:param_in "params" "CORBA::ParDescriptionSeq")
        (:param_in "exceptions" "CORBA::ExceptionDefSeq")
        (:param_in "contexts" "CORBA::ContextIdSeq"))
       :result-type
       "CORBA::OperationDef"
       :exceptions
       NIL))
    (DEFINE-STRUCT "InterfaceDescription"
     (("name" "CORBA::Identifier") ("id" "CORBA::RepositoryId")
      ("defined_in" "CORBA::RepositoryId") ("version" "CORBA::VersionSpec")
      ("base_interfaces" "CORBA::RepositoryIdSeq"))))))