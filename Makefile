SHELL=/bin/sh

include $(CONFIGPATH)/paths
include $(CONFIGPATH)/makevars.$(COMPILERVARS)

all: default

default: lib 

lib: FORCE
	cd $(BASEPATH)/src; $(MAKE) all

examples: FORCE
	cd $(BASEPATH)/examples; $(MAKE) all

clean: FORCE
	cd $(BASEPATH)/src; $(MAKE) $@ ; \

cleanlib: FORCE
	rm -rf $(BASEPATH)/lib/*.a $(BASEPATH)/lib/*.so $(BASEPATH)/lib/*

cleanexample: FORCE
	rm -rf $(BASEPATH)/examples/*.x $(BASEPATH)/examples/*~ $(BASEPATH)/examples/*.g2

cleanconfig: FORCE
	rm -f $(BASEPATH)/config/*~

cleanmods: FORCE
	rm -f $(BASEPATH)/modules/*.$(MOD) $(BASEPATH)/modules/*.$(SMOD)

cleanbase: FORCE
	rm -f $(BASEPATH)/*~

cleanall: clean cleanlib cleanexample cleanmods cleanconfig cleanbase

FORCE:
