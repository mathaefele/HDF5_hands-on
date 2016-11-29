##############################################################################
## Makefile --- Course HDF5 : TP4 : Group and attribute creation
##
## Author          : Matthieu Haefele  
##                   High Level Support Team 
##                   Institut For Plasmaphysics (IPP)
##                   matthieu.haefele@ipp.mpg.de
## 
###############################################################################
include ../../../arch/make_inc

hdf5_ex2 : hdf5_ex2.o
	$(CF95_serial) $(FFLAGS_HDF5) hdf5_ex2.o -o $@ $(LDFLAGS_HDF5)


##### RULES #####

.SUFFIXES: .f90

.f90.o:
	$(CF95_serial) $(FFLAGS_HDF5) -c $< 



##### TARGETS #####


clean: 
	@-rm -f hdf5_ex2
	@-rm -f *.o *~ core* *.h5

