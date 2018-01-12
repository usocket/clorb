all:

clean:
	rm -f *.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.bak

setmcl:
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp

prep-cvs:
	perl -pe 's/\r/\n/g' < idl-scanner-parser.lisp > temp-file
	if cmp -s idl-scanner-parser.lisp temp-file ; \
	then rm temp-file; \
	else mv temp-file idl-scanner-parser.lisp; fi



