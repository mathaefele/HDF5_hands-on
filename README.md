# HDF5 hands-on
4 simple exercice that manipulates dataspace with solution provided


*Environment required on the ruche machine with the installed modules*

Load the following modules
```bash

module load gcc/11.2.0/gcc-4.8.5
module load hdf5/1.12.0/gcc-11.2.0-openmpi
```

*Exercices*
0. Getting started
  * In directory `HDF5_hands-on/hdf5-1/C`, `HDF5_hands-on/hdf5-1/F90`, examine the source code, compile and run it
  * Examine the output file example.h5 with `h5ls` and `h5dump` command line tools
  * Compare this output with the output you should obtain in the solution directory

1. Modify the program to:
  * allocate and initialise a new 3D array of size Nx.Ny.Nz, leaving the existing 2D one
  * write an additional dataset containing this 3D array

2. Modify the program to write, in a new 2D dataset, a single 2D slice of the 3D array instead of the whole 3D array

3. Modify the program to write the previous 2D slice as an extension of the original 2D dataset IntArray

4. Play with chunks, groups and attributes and try the command h5ls and h5ls -v
