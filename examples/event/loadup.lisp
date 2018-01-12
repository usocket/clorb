(in-package :cl-user)

(CORBA:IDL "clorb:idl;CosEventChannelAdmin.idl")

(defpackage :net.cddr.clorb.event
  (:use :cl)
  (:nicknames :ec)
  (:import-from "CORBA" "DEFINE-METHOD"))


