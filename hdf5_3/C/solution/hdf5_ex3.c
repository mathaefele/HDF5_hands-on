/* ###################################################################
   #                       HDF5 hands on                             #
   #  Created : April 2011                                           #
   #                                                                 #
   #  Author:                                                        #
   #     Matthieu Haefele                                            #
   #     matthieu.haefele@ipp.mpg.de                                 #
   #     High Level Support Team (HLST)                              #
   #     Max-Planck Institut fuer Plasmaphysik                       #
   #                                                                 #
   ################################################################### */

#include <hdf5.h>
#define NX     10
#define NY     6
#define NZ     3

void init(int* data, int* data3d)
{
  int i,j,k;
  for(j=0; j < NY; j++)
    for(i=0; i < NX; i++)
      data[i + j*NX] = i + j*NX;

  for(k=0; k < NZ; k++)	  
    for(j=0; j < NY; j++)
      for(i=0; i < NX; i++)
        data3d[i + j*NX + k*NX*NY] = i + j*NX + k*NX*NY;
}

int main (void)
{
  hid_t       file, dataset, mem_space, file_space;
  hsize_t     dimsf[3];
  hsize_t array_size[3], array_subsize[3], array_start[3];
  herr_t      status;
  int         data[NX*NY];
  int         data3d[NX*NY*NZ];
  
  //Data initialization
  init(data,data3d);
  
  //HDF5 file creation
  file = H5Fcreate("example.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  dimsf[0] = 2*NY;
  dimsf[1] = NX;
  
  //Dataspace creation
  file_space = H5Screate_simple(2, dimsf, NULL); 
  
  //Dataset creation 
  dataset = H5Dcreate(file, "IntArray", H5T_NATIVE_INT, file_space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  // Creating the selection to write in the file
  array_subsize[0] = NY;
  array_subsize[1] = NX;
  array_start[0] = 0; 
  array_start[1] = 0; 
  status = H5Sselect_hyperslab (file_space, H5S_SELECT_SET, array_start, NULL, array_subsize, NULL);

  //Actual data IO
  status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, file_space, H5P_DEFAULT, data);
  
  //Closing file_space
  H5Sclose(file_space);

  //Dataspace creation for memory selection
  dimsf[0] = NZ;
  dimsf[1] = NY;
  dimsf[2] = NX;
  array_subsize[0] = 1;
  array_subsize[1] = NY;
  array_subsize[2] = NX;
  array_start[0] = 2; 
  array_start[1] = 0; 
  array_start[2] = 0; 
  mem_space = H5Screate_simple(3, dimsf, NULL); 
  status = H5Sselect_hyperslab (mem_space, H5S_SELECT_SET, array_start, NULL, array_subsize, NULL);

  dimsf[0] = 2*NY;
  dimsf[1] = NX;
  
  //Dataspace creation
  file_space = H5Screate_simple(2, dimsf, NULL); 
  
  // Creating the selection to write in the file
  array_subsize[0] = NY;
  array_subsize[1] = NX;
  array_start[0] = NY; 
  array_start[1] = 0; 
  status = H5Sselect_hyperslab (file_space, H5S_SELECT_SET, array_start, NULL, array_subsize, NULL);
  //Actual data IO
  status = H5Dwrite(dataset, H5T_NATIVE_INT, mem_space, file_space, H5P_DEFAULT, data3d);
  
  //Closing all opened HDF5 objects
  H5Sclose(mem_space);
  H5Sclose(file_space);
  H5Dclose(dataset);
  H5Fclose(file);
  

  return 0;
}     
