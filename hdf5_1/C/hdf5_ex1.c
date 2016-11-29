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
#define RANK   2

void init(int* data)
{
  int i,j;
  for(j=0; j < NY; j++)
    for(i=0; i < NX; i++)
      data[i + j*NX] = i + j*NX;
}

int main (void)
{
  hid_t       file, dataset, dataspace;
  hsize_t     dimsf[RANK];
  herr_t      status;
  int         data[NX*NY];
  
  //Data initialization
  init(data);
  
  //HDF5 file creation
  file = H5Fcreate("example.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  dimsf[0] = NY;
  dimsf[1] = NX;
  
  //Dataspace creation
  dataspace = H5Screate_simple(RANK, dimsf, NULL); 
  
  //Dataset creation 
  dataset = H5Dcreate(file, "IntArray", H5T_NATIVE_INT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

  //Actual data IO
  status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,H5P_DEFAULT, data);
  
  //Closing all opened HDF5 objects
  H5Sclose(dataspace);
  H5Dclose(dataset);
  H5Fclose(file);
  

  return 0;
}     
