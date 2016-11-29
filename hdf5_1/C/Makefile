##############################################################################
## Makefile --- Hands on HDF5 1
##
## Author          : Matthieu Haefele  
##                   Maison de la Simulation 
##                   matthieu.haefele@maisondelasimulation
## 
###############################################################################
include ../../arch/make_inc

hdf5_ex1 : hdf5_ex1.o
	$(CC_serial) $(CCFLAGS_HDF5) hdf5_ex1.o -o $@ $(CLDFLAGS_HDF5)


##### RULES #####

.SUFFIXES: .c 

.c.o:
	$(CC_serial) $(CCFLAGS_HDF5) -c $< 


##### TARGETS #####



clean: 
	@-rm -f hdf5_ex1 
	@-rm -f *.o *~ core* *.h5

