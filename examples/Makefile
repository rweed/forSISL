include $(CONFIGPATH)/paths
include $(CONFIGPATH)/makevars.$(COMPILERVARS)

FLIB=$(BASEPATH)/lib/forsisl.a
CLIB=${SISL_LIBPATH}/sisl.a

EXAMPLES = example01.x \
 example02.x \
 example03.x \
 example04.x \
 example05.x \
 example06.x \
 example07.x \
 example08.x \
 example09.x \
 example10.x \
 example11.x \
 example12.x \
 example13.x \
 example14.x \
 example15.x

all: $(EXAMPLES)

example01.x: example01.F90
	$(FC) -o ./example01.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example01.F90 $(FLIB) $(CLIB)

example02.x: example02.F90
	$(FC) -o ./example02.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example02.F90 $(FLIB) $(CLIB)

example03.x: example03.F90
	$(FC) -o ./example03.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example03.F90 $(FLIB) $(CLIB)

example04.x: example04.F90
	$(FC) -o ./example04.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example04.F90 $(FLIB) $(CLIB)

example05.x: example05.F90
	$(FC) -o ./example05.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example05.F90 $(FLIB) $(CLIB)

example06.x: example06.F90
	$(FC) -o ./example06.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example06.F90 $(FLIB) $(CLIB)

example07.x: example07.F90
	$(FC) -o ./example07.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example07.F90 $(FLIB) $(CLIB)

example08.x: example08.F90
	$(FC) -o ./example08.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example08.F90 $(FLIB) $(CLIB)

example09.x: example09.F90
	$(FC) -o ./example09.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example09.F90 $(FLIB) $(CLIB)

example10.x: example10.F90
	$(FC) -o ./example10.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example10.F90 $(FLIB) $(CLIB)

example11.x: example11.F90
	$(FC) -o ./example11.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example11.F90 $(FLIB) $(CLIB)

example12.x: example12.F90
	$(FC) -o ./example12.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example12.F90 $(FLIB) $(CLIB)

example13.x: example13.F90
	$(FC) -o ./example13.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example13.F90 $(FLIB) $(CLIB)

example14.x: example14.F90
	$(FC) -o ./example14.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example14.F90 $(FLIB) $(CLIB)

example15.x: example15.F90
	$(FC) -o ./example15.x $(FCFLAGS) $(FPEFLAGS) $(INCLUDES) $(DEFINES) ./example15.F90 $(FLIB) $(CLIB)


include $(CONFIGPATH)/makerules.$(RULES)
