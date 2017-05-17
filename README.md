# HDF5 hands-on
4 simple exercice that manipulates dataspace with solution provided


*Environment required on the poincare machine*
module load gnu/4.7.2 hdf5/1.8.10_gnu47_serial

*Exercices*
0. Getting started
  * In directory HDF5 hands-on/hdf5-1, Examine the source code, compile and run it
  * Examine the output file example.h5 with h5ls and h5dump command line tools
  * Compare this output with the output you should obtain in the solution directory

1. Modify the program to:
  * allocate and initialise a 3D array of size Nx.Ny.Nz
  * write an additional dataset containing this 3D array

2. Modify the program to write, in a new 2D dataset, a single 2D slice of the 3D array instead of the whole 3D array

3. Modify the program to write the previous 2D slice as an extension of the original 2D dataset IntArray

4. Play with chunks, groups and attributes and try the command h5ls and h5ls -v
