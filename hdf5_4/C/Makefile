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
	$(CC_serial) $(CCFLAGS_HDF5) hdf5_ex4.o -o $@ $(CLDFLAGS_HDF5)


##### RULES #####

.SUFFIXES: .c 

.c.o:
	$(CC_serial) $(CCFLAGS_HDF5) -c $< 


##### TARGETS #####



clean: 
	@-rm -f hdf5_ex4 
	@-rm -f *.o *~ core* *.h5

