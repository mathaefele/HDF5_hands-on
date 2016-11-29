
! ###################################################################
! #                       HDF5 hands on ex4                         #
! #  Created : April 2011                                           # 
! #                                                                 #
! #  Author:                                                        #
! #     Matthieu Haefele                                            #
! #     matthieu.haefele@ipp.mpg.de                                 #
! #     High Level Support Team (HLST)                              #
! #     Max-Planck Institut fuer Plasmaphysik                       #
! #                                                                 #
! ###################################################################

PROGRAM HDF5_EX4

     USE HDF5 ! This module contains all necessary modules for hdf5 calls
     USE H5LT ! This module contains the Lite high level API
        
     IMPLICIT NONE

     CHARACTER(LEN=10), PARAMETER :: filename = "example.h5" ! File name
     CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray"     ! Dataset name
     CHARACTER(LEN=7), PARAMETER :: attr_name = "Comment" ! Attribute name
     CHARACTER(LEN=31), PARAMETER :: attr_content = "Just a simple array of integers" ! Attribute content
     CHARACTER(LEN=8), PARAMETER :: groupname = "my_group" ! Group name
     INTEGER, PARAMETER     ::   rank = 2                        
     INTEGER, PARAMETER     ::   NX = 10                        
     INTEGER, PARAMETER     ::   NY = 6                        

     INTEGER(HID_T) :: file, group, dataset, dataspace, dataset_property
     INTEGER(HSIZE_T), DIMENSION(2) :: dims, chunk_dims
     INTEGER     ::   status
     INTEGER, DIMENSION(NX,NY) :: data
     INTEGER     :: i,j
     
     DO j=1,NY
       DO i=1,NX
        data(i,j) = i-1 + (j-1)*NX;
       ENDDO
     ENDDO
        
     !
     ! Initialize FORTRAN predefined datatypes.
     !
     CALL h5open_f(status)

     !
     ! Create a new file using default properties.
     ! 
     CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file, status)
     
     !
     ! Create a new group using default properties.
     ! 
     CALL h5gcreate_f(file, groupname, group, status, &
                        OBJECT_NAMELEN_DEFAULT_F, H5P_DEFAULT_F, H5P_DEFAULT_F, H5P_DEFAULT_F)


     ! 
     ! Create the dataspace.
     !
     dims(1) = NX
     dims(2) = NY
     CALL h5screate_simple_f(rank, dims, dataspace, status)
     
     
     !
     ! Create the dataset property with chunk and z compression.
     !
     chunk_dims(1) = dims(1)/2;
     chunk_dims(2) = dims(2)/2;
     
     CALL h5pcreate_f(H5P_DATASET_CREATE_F, dataset_property, status)
     CALL h5pset_chunk_f(dataset_property, rank, chunk_dims, status)
     CALL h5pset_deflate_f(dataset_property, 1, status)
  

     !
     ! Create the dataset with chunk and z compression properties.
     !
     CALL h5dcreate_f(group, dsetname, H5T_NATIVE_INTEGER, dataspace, &
                      dataset, status, dataset_property)
      
     
     
     CALL h5ltset_attribute_string_f(group, dsetname, attr_name, &
                                      attr_content, status )
                                      

     !
     ! Write the data actually
     !
     CALL h5dwrite_f(dataset, H5T_NATIVE_INTEGER, data, dims, status)


     !   
     ! End access to the dataset and release resources used by it.
     ! 
     CALL h5dclose_f(dataset, status)

     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(dataspace, status)

     ! 
     ! Close the file.
     !
     CALL h5fclose_f(file, status)

     !
     ! Close FORTRAN predefined datatypes.
     !
     CALL h5close_f(status)

     END PROGRAM HDF5_EX4
     
