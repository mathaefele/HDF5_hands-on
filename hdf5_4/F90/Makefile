##############################################################################
## Makefile --- Course HDF5 : TP4 : Group and attribute creation
##
## Author          : Matthieu Haefele  
##                   High Level Support Team 
##                   Institut For Plasmaphysics (IPP)
##                   matthieu.haefele@ipp.mpg.de
## 
###############################################################################
include ../../arch/make_inc

hdf5_ex4 : hdf5_ex4.o
	$(CF95_serial) $(FFLAGS_HDF5) hdf5_ex4.o -o $@ $(LDFLAGS_HDF5)


##### RULES #####

.SUFFIXES: .f90

.f90.o:
	$(CF95_serial) $(FFLAGS_HDF5) -c $< 



##### TARGETS #####


clean: 
	@-rm -f hdf5_ex4
	@-rm -f *.o *~ core* *.h5

