all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.bak *.cfsl

setmcl:
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp

import:
#	cp ~/src/lisp/net/cddr/redpas/{package,lexer,parsys}.lisp .
	cp orig/{package,lexer,parsys}.lisp .

export:
	test -d orig
#cvs -d ~/lib/cvsroot co -d orig lisp/net/cddr/redpas
	cp {package,lexer,parsys}.lisp orig/
