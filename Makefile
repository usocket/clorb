FASL="*.fasl *.fas *.lib *.x86f *.err *.pfsl *.ufsl *.dfsl *.nfasl *.cfsl"

all:

clean:
	eval rm -f $(FASL) *.bak
	$(MAKE) -C idlcomp clean
	$(MAKE) -C luna clean
	$(MAKE) -C redpas clean

clean-fasl:
	rm -r fasl; mkdir fasl


setmcl:
	/Developer/Tools/SetFile -t TEXT -c CCL2 *.lisp
	cd idlcomp; make setmcl
	/Developer/Tools/SetFile -t TEXT -c CCL2 examples/*/*.lisp

prep-cvs:
	cd idlcomp; make prep-cvs

tags:
	etags *.lisp redpas/*.lisp luna/*.lisp
