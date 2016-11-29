
/* ###################################################################
   #                       HDF5 hands on ex4                         #
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
#include <hdf5_hl.h>
#include <stdlib.h>

#define check_status(status, error_str) if(status < 0){printf("Check status error: %s\n", error_str);exit(1);}

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
  hid_t       file, dataset, dataspace, group, dataset_property;
  hsize_t     dimsf[RANK], chunk_dims[RANK];
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
  
  //Group creation
  group = H5Gcreate(file, "my_group", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
  
  //Dataset property creation with chunks and compression
  chunk_dims[0] = dimsf[0]/2;
  chunk_dims[1] = dimsf[1]/2;
  dataset_property = H5Pcreate(H5P_DATASET_CREATE);
  status = H5Pset_chunk(dataset_property, RANK, chunk_dims);
  check_status(status, "H5Pset_chunk")
  status = H5Pset_deflate(dataset_property, 1);
  check_status(status, "H5Pset_deflate")
  
  //Dataset creation 
  dataset = H5Dcreate(group, "IntArray", H5T_NATIVE_INT, dataspace, H5P_DEFAULT, dataset_property, H5P_DEFAULT);
  H5Pclose(dataset_property);

  //Actual data IO
  status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL,H5P_DEFAULT, data);
  check_status(status, "H5Dwrite")
  
  //Attribute creation with Lite high level library
  status = H5LTset_attribute_string(group, "IntArray", "Comment", "Just a simple array of integers");
  check_status(status, "H5LTset_attribute_string")
  
  //Closing all opened HDF5 objects
  H5Sclose(dataspace);
  H5Dclose(dataset);
  H5Gclose(group);
  H5Fclose(file);
  

  return 0;
}     
