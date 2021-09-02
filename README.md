#  CLORB - a Common Lisp implementation of CORBA

                    Version 0.7 (an ALPHA release)

                          by Lennart Staflin

                              2006-06-09

CLORB is an Object Request Broker implementing CORBA 2. It currently
supports IIOP 1.0, IDL, DII, DSI, POA and value types. The goal is to
make the mapping follow the proposed mapping for LISP [1].

New in 0.7:

   * Multi-Threading support for MCL, OpenMCL and SBCL.
   * OpenMCL: real select

It lacks:

   * wide strings, chars
   * Request context
   * DynAny
   * CORBA Messaging
   * IIOP 1.2

CLORB is released under GNU Library General Public License see the
file COPYING.


Ports:
   MCL 5        only tested with Max OS X native
   OpenMCL      working (1.1) (Mac OS X)
   SBCL         working on Mac OS X and Debian GNU/Linux (i386)
   CLISP        works again (2.38)

Non-working ports:
   ACL 6.2      
   CMUCL        ?



[1] http://www.omg.org/technology/documents/formal/lisp_language_mapping.htm


## Compiling and Loading CLORB

The file clorb-files.lisp contains code to compile and load CLORB.
There is also an ASDF system definition file clorb.asd. The ASDF
definition is good enough for loading CLORB, but inconvenient for
hacking.

clorb-files.lisp defines a package NET.CDDR.CLORB.SYSTEM and a
function RELOAD (in that package) that will compile and load CLORB.
It will load files relative to the directory from where clorb-files
was loaded. 

The files:

- devel.lisp - Loads CLORB for development purpose, it also initiates
  the ORB and the object adaptor. It runs all the test cases and
  prepares the "hello world" example (use (hh) to run it or (hhn) to
  use the Name Service).

- loadup.lisp - Loads CLORB for normal use. The code will be loaded
  but not initialized.

It is not necessary to setup any logical pathname hosts to load
CLORB. But some extra features like test cases and examples might
depend on the "CLORB" host. It should then be set up to map
CLORB:SRC; to the main CLORB sources and allow relative directory
references.

When compiling and loading clorb some clorb specific features control
the process. 

    
Features to control what socket/tcp code gets used:

    :use-acl-socket     use the ACL-SOCKET package in preference to
                        native socket support (requires the acl
                        compatibility package)

    :dummy-tcp          don't include any socket code (can be used to
                        test the rest of the code)


Features that are automatically determined (in clorb-sysdep):

    clorb::cmucl-sockets
    clorb::db-sockets
    clorb::sb-bsd-sockets
    clorb::mcl-bsd


## Initializing the ORB

After CLORB has been loaded and before the ORB can be used it has to
be initialized. This is done with the CORBA:ORB_init function.
```
(CORBA:ORB_init arguments orb-id) => orb-object, arguments'
```

Where arguments is a list of strings (as would usually be passed to
a command line program in Unix) and orb-id is a string identifying
the particular orb (only one is supported pass "" for this). The
function can be called more than once, and will return the same
object. The argument list will be processed by every call.

In the arguments list, items starting with "-ORB" will be recognized
and removed (the item after will also be removed if it provides
argument to the -ORB argument). The remaining arguments will be
returned as a second return value. The recognized items are:

    -ORBInitRef Name=IOR

      This provides value for the initial references. A call to
      op:resolve_initial_references with Name will return the object
      references by the IOR.

    -ORBDefaultInitRef IOR

    -ORBPort port#

      Specify the port that the Object adaptor will listen on.

    -ORBHostname name

      Specify the hostname that will be included in object references,
      could also be an IP-number.


The clorb:*log-level* controls the amount of logging done by CLORB.
It should be an integer, lower value gives more logging. Logging is
written to the stream *log-output*.


## Loading/Compiling IDL files

    (CORBA:IDL pathname &key eval print output skeleton) => repository

Load and parse the IDL file referenced by PATHNAME. The interface
repository containing the parsed IDL file is returned. The necessary
code to use this interface is generated and loaded (unless the eval
key is nil).

Producing a Lisp file with all code for an IDL file:

    (CORBA:IDL "foo.idl" :output "foo.lisp")

The abow will also load the code. To only produce the file specify
:eval nil. (Some packages might still be created as a side effect.)


## Obtaining initial object references

```
(op:list_initial_services orb) => list-of-names

(op:resolve_initial_references orb "NameService") => object-reference

(op:string_to_object orb "IOR") => object-reference
```

The IOR could be on the IOR:xxx form or it could be a file or http
URL, in that case the references resource is should contain an IOR.
The IOR could also be a corbaloc or corbaname IOR.


CLORB specific utility functions:
```
(clorb:obj ior &optional type) => object-reference
```

Like string_to_object, but shorter and optionally narrowing the object
reference to the given type. The type should be the scoped symbol for
the interface. E.g:
`(clorb:obj "corbaloc::localhost:2700/NameService" 'cosnaming:namingcontextext)`

`(op:resolve name-string) => object-reference`
lookup the name in the name service. The syntax for name-string is as
described in the portable name service specification. E.g:
 `(op:resolve "contxt/name.kind")`
 

`(op:narrow type object-reference) => object-reference`
returns a type-specific proxy for the object-reference. Type should be
a scoped symbol for the interface. This is compatible with LW CORBA.
E.g: `(op:narrow 'clorb_ex:hello obj)`


## Client Operation

Initialize and get a reference to the ORB object.

    (defvar *the-orb* (CORBA:ORB_init))

Load the IDL for the interface.

    (corba:idl "clorb:idl;Random.idl")

Obtain an object reference and call methods on the reference.

    (let ((obj
           (op:string_to_object *the-orb* "http://www.random.org/Random.ior")))
      (op:lrand48 obj))


[Unfortunately the CORBA server for random number where not answering
the last time I tried it.]


## Server Operation

Initialize the ORB and load the IDL for the interface, as for Client
Operation.

Get a reference to the Root POA (Object Adapter).

    (defvar *poa* (op:resolve_initial_references *orb* "RootPOA"))

Possibly create another POA with policies tailored for the server
needs.

Activate the POA:

    (op:activate (op:the_poamanager *poa*))

Create and register a servant:

    (defvar *servant* (make-instance 'my-class))
    (defvar *object* (op:activate_object *poa* *servant*))

Get stringified IOR for object

    (op:object_to_string *orb* *object*)

The string can be transfered to the client in whatever way.
Or register with name service

    (clorb:rebind *object* "my-name")

## Platform-specific Notes

### MCL 5

MCL 5.0 is the current main development environment. You also need the
BSD package <URL:http://babs.cs.umass.edu/~bburns/software/bsd.lisp>.
The networking code can either use OpenTransport directly or use the
ACL-COMPAT code from portable allegro server [fix reference].

To load CLORB you need to do something like this:

    (require 'bsd)
    (load "CLORB:SRC;CLORB-FILES")
    (net.cddr.clorb.system:reload)


### OpenMCL

(load "clorb-files")
(net.cddr.clorb.system:reload)

;; Initializing the ORB
(defvar *orb* 
  (CORBA:ORB_init
   (list ;;"-ORBPort" "4712"
         ;;"-ORBInitRef" "NameService=corbaloc::127.0.0.1:4711/NameService"
         )))

### SBCL

Tried with version 0.9.0 on MacOS X, 0.8.16 on Debian GNU/Linux i386.

The system dependent part will (require :sb-bsd-sockets).

(load "clorb-files")
(net.cddr.clorb.system:reload)

;; Initializing the ORB
(defvar *orb* 
  (CORBA:ORB_init
   (list ;;"-ORBPort" "4712"
         ;;"-ORBInitRef" "NameService=corbaloc::127.0.0.1:4711/NameService"
         )))

### CLISP

You might need a recent version of CLISP for CLORB to work. I have
tested with 2.33 on both Debian GNU/Linux (i386) and on Mac OS X 10.2.8.

There is a pretty printer bug in CLISP 2.32 that cause problems for
the CORBA:IDL function with either :print or :output options. You can
get around this by also specifying :pprint-dispatch nil. (This seems
to be needed for 2.33 also.)

### About the CMUCL port

CLORB 0.6 seems to work with CMUCL 19a. Except for a problem loading
the fasl file for cosnaming-stub. A work-around is to

    (setf c::top-level-lambda-max 0)

before compiling.

Tested on Debian GNU/Linux (i386)
and Mac OS 10.3.9.



From: Daniel Barlow <dan@telent.net>
Date: 11 May 2000 00:39:38 +0100

[...]

2) The user has a choice of socket libraries: the standard CMUCL
socket code, or my replacement sockets library.  The standard code
doesn't support the socket option SO_REUSEADDR (without writing ffi
glue) so you need my sockets if you want to do server applications.
You can get it at http://ww.telent.net/lisp/sockets.html

3) The system is built using mk-defsystem, which comes with CMUCL
(if you use the Debian packages, at any rate).

4) Only tested on ix86 GNU/Linux so far.

5) It works, kind of.  It's not been extensively tested.  I can make
it talk as a client to the GNOME Help viewer (see
http://ww.telent.net/corba/gnome-lisp.html for a worked example), and
I can make the ORBit 'name-client' test program talk to your CosNaming
service implementation.  Serving is a bit CPU-intensive until I get it
actually blocking instead of busy-waiting when waiting for a connection.

6) It's not been tested recently in SBCL (Steel Bank Common Lisp) but
there's no particularly good reason for it not to work there too.
You'll have to use my socket library in that one, because it doesn't
have any socket code of its own at all.

[...]


## Examples

The subdirectory examples contain some examples.

1. Hello World

In examples/hello. This is a simple client/server example. The file
hello.idl describes the interface:

module CLORB_EX {
  interface HelloWorld {
    string greet ();
  };
};

The server implements a servant with a method "greet" that returns a
string. The client obtains an object reference and calls the greet
method.

Load examples/hello/loadup (loaded by default by devel.lisp), and the
in the server run

(run-hello :file "/tmp/hello.ior")


In the client run

(hello-client :file "/tmp/hello.ior")


The :file argument specifies how the object reference is exchanged.
If you have a name server configured you can use then :name argument
instead.


## Test Suite

CLORB comes with a test suite. The test coverage is currently spotty.

The support system for the test cases has now been put in its own
package. I have also used it for some other projects. The code is
however included in the luna subdirectory. Some CLORB specific
support is in the support-test.lisp. 

The files test-*.lisp contains test suits for parts of CLORB and the
file all-test.lisp runs all the test suites and aggreates the result.


## About the included files

- `clorb-*.lisp`

This is main part of the ORB.

- `clorb-options.lisp`

Some parameters that might need to be customized. Especially
host-name and how to find the Naming Service and Interface
Repository.

- `pns-server.lisp`

A new implementation of CosNaming, where the state is stored
in files (thus persistent).

- `idef-read.lisp`, `idef-write.lisp`

Read and write IDEF, an sexp version of IDL. These reads and
writes from the local Interface Repository implemention.

- `idef-macros.lisp`
Macro wrapper around idef-read to allow IDEF to be loaded from
file. Also macro for defining servant skeleton class for use
with the CLORB auto-servant class.

- `cosevent-idl.lisp`, `cosnaming-idl.lisp`

IDEF version of standard IDL for CosEventChannel and
CosNaming. (generated by idef-write)

- `cosnaming-stub.lisp`

The types, classes for CosNaming according to the OMG Lisp Mapping.
Proxy classes and operation methods.
(This partly hand created static stub.)

- `cosnaming-skel.lisp`

Servant classes for CosNaming.

- `orbit.lisp`

Set *principal* for communication with ORBit (GNOME) servers.


## EXAMPLE CODE (in examples)

- `examples/hello/`

Hello World example, autoloaded by devel.lisp (via examples/hello/auto.lisp)
Try: (hh) or (hhn)
Or, in server: (run-hello :name "hello")
in client:     (hello-client :name "hello")
assuming you have a running name server.

- `examples/dii-example.lisp`, `examples/dsi-example.lisp`, `examples/dsi/clive.lisp`

Example of using DSI (dynamic servant implementation) and DII
(dynamic invokation interface) using standard interfaces.
From a discussion on comp.lang.corba


