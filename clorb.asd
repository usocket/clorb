;; -*- Mode: Lisp; Package: ("CLORB.SYSTEM" :use ("CL" "ASDF")) -*-

(defpackage :clorb.system
  (:use "CL" "ASDF"))

(in-package :clorb.system)

(defsystem :clorb
  :name "CLORB"
  :description "A Common Lisp implementation of CORBA"
  :author "Lennart Staflin"
  :version (:read-file-form "version.sexp")
  :licence "LGPLv2"
  :depends-on (:usocket)
  :serial t
  :components
  ((:module "redpas"
            :serial t
            :components ((:file "package") (:file "lexer") (:file "parsys")))
   (:module "luna"
            :serial t
            :components ((:file "package") (:file "testsuite") (:file "pattern")))
   (:file "clorb-pkgdcl"     :depends-on ("redpas" "luna"))
   (:file "clorb-options")
   (:file "clorb-supp")
   (:file "clorb-macros")
   (:file "clorb-queue")
   #+openmcl (:file "clorb-openmcl")
   (:file "clorb-sysdep")
   (:file "clorb-mt")
   (:file "clorb-basetypes")
   (:file "clorb-typecode-1")
   (:file "clorb-buffer")
   (:file "clorb-marshal")
   (:file "clorb-unmarshal")
   (:file "clorb-typecode-2")
   (:file "clorb-macros2")
   (:file "clorb-any")
   (:file "clorb-struct")
   (:file "clorb-union")
   (:file "clorb-exceptions")
   (:file "clorb-io")
   (:file "clorb-conn")
   (:file "clorb-object")
   (:file "clorb-request")
   (:file "clorb-iop")
   (:file "clorb-value")
   (:file "clorb-ifr-base")
   (:file "clorb-orb")
   (:file "clorb-codec")
   (:file "clorb-iiop")
   (:file "cosnaming-stub")
   (:file "clorb-util")
   (:file "clorb-misc")
   (:file "clorb-ifr")
   (:file "clorb-target")
   (:file "clorb-idl")
   (:file "clorb-idlcpp")
   (:file "clorb-objkey")
   (:file "clorb-servant")
   (:file "clorb-trie")
   (:file "clorb-aom")
   (:file "clorb-poa-base")
   (:file "clorb-poa")
   (:file "clorb-srv")
   (:file "clorb-ifr-info")
   (:file "clorb-idllexer")
   (:file "clorb-idlparser")
   (:file "idef-read")
   (:file "idef-write")
   (:file "idef-macros")
   (:file "clorb-ifr-export")
   
   ;; could be a separate package
   (:file "cosnaming-skel")
   (:file "pns-server")
   
   ;;
   (:file "support-test")) )
