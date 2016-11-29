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
  hid_t       file, dataset, dataspace;
  hsize_t     dimsf[3];
  hsize_t array_size[3], array_subsize[3], array_start[3];
  herr_t      status;
  int         data[NX*NY];
  int         data3d[NX*NY*NZ];
  
  //Data initialization
  init(data,data3d);
  
  //HDF5 file creation
  file = H5Fcreate("example.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  dimsf[0] = NY;
  dimsf[1] = NX;
  
  //Dataspace creation
  dataspace = H5Screate_simple(2, dimsf, NULL); 
  
  //Dataset creation 
  dataset = H5Dcreate(file, "IntArray", H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  //Actual data IO
  status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,H5P_DEFAULT, data);
  
  //Closing dataset
  H5Dclose(dataset);

  //Dataset creation for the second 2D dataset by reusing the dataspace
  dataset = H5Dcreate(file, "IntArray2", H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  //Closing dataspace
  H5Sclose(dataspace);

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
  dataspace = H5Screate_simple(3, dimsf, NULL); 
  status = H5Sselect_hyperslab (dataspace, H5S_SELECT_SET, array_start, NULL, array_subsize, NULL);

  //Actual data IO
  status = H5Dwrite(dataset, H5T_NATIVE_INT, dataspace, H5S_ALL,H5P_DEFAULT, data3d);
  
  //Closing all opened HDF5 objects
  H5Sclose(dataspace);
  H5Dclose(dataset);
  H5Fclose(file);
  

  return 0;
}     
