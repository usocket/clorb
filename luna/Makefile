all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.bak *.cfsl

setmcl:
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp

import:
#	cp ~/src/lisp/net/cddr/luna/{package,testsuite,pattern}.lisp .

export:
	cp {package,testsuite,pattern}.lisp orig/
