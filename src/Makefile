include $(CONFIGPATH)/paths
include $(CONFIGPATH)/makevars.$(COMPILERVARS)

OBJS=forSISLdata.o \
 Curve_Utilities.o \
 Curve_Analysis.o \
 Curve_Definition.o \
 Curve_Interrogation.o \
 Surface_Utilities.o \
 Surface_Analysis.o \
 Surface_Definition.o \
 Surface_Interrogation.o \
 Data_Reduction.o \
 forSISLcurves.o \
 forSISLsurfaces.o \
 forSISLio.o \
 forSISL.o \
 C_utils.o

THISLIB=forsisl

all: $(THISLIB)

$(THISLIB):
	make "FCFLAGS=$(FCFLAGS)" "DEFINES=$(DEFINES)" $(THISLIB).a $(THISLIB).$(SO) 

$(THISLIB).a: $(OBJS)
	$(AR) $@ $(OBJS)
	$(RANLIB) $@
	rm -f $(LIBPATH)/$@
	mv ./*.a $(LIBPATH)
	cp ./*.$(MOD) $(MODPATH)

$(THISLIB).$(SO): $(OBJS)
	$(DYNLIB) ./$(THISLIB).$(SO) $(OBJS)
	rm -f $(LIBPATH)/*.$(SO)
	mv ./*.$(SO) $(LIBPATH)
	
include $(CONFIGPATH)/makerules.$(RULES)
