
! ###################################################################
! #                       HDF5 hands on                             #
! #  Created : April 2011                                           # 
! #                                                                 #
! #  Author:                                                        #
! #     Matthieu Haefele                                            #
! #     matthieu.haefele@ipp.mpg.de                                 #
! #     High Level Support Team (HLST)                              #
! #     Max-Planck Institut fuer Plasmaphysik                       #
! #                                                                 #
! ###################################################################

PROGRAM HDF5_EX
     USE HDF5

     IMPLICIT NONE

     CHARACTER(LEN=10), PARAMETER :: filename = "example.h5" ! File name
     CHARACTER(LEN=8), PARAMETER :: dsetname = "IntArray"     ! Dataset name
     CHARACTER(LEN=10), PARAMETER :: dsetname2= "IntArray2"     ! Dataset name
     INTEGER, PARAMETER     ::   NX = 10                        
     INTEGER, PARAMETER     ::   NY = 6                        
     INTEGER, PARAMETER     ::   NZ = 3                        

     INTEGER(HID_T) :: file, dataset, dataspace
     INTEGER(HSIZE_T), DIMENSION(3) :: dims, array_subsize, array_start
     INTEGER     ::   status
     INTEGER, DIMENSION(NX,NY) :: data
     INTEGER, DIMENSION(NX,NY,NZ) :: data3d
     INTEGER     :: i,j,k
     
     DO j=1,NY
       DO i=1,NX
        data(i,j) = i-1 + (j-1)*NX;
       ENDDO
     ENDDO
        
     DO k=1,NZ
       DO j=1,NY
         DO i=1,NX
           data3d(i,j,k) = i-1 + (j-1)*NX + (k-1)*NX*NY
         ENDDO
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
     ! Create the dataspace.
     !
     dims(1) = NX
     dims(2) = NY
     CALL h5screate_simple_f(2, dims, dataspace, status)
     
     
     !
     ! Create the dataset default properties.
     !
     CALL h5dcreate_f(file, dsetname, H5T_NATIVE_INTEGER, dataspace, &
                      dataset, status)
      
     !
     ! Write the data actually
     !
     CALL h5dwrite_f(dataset, H5T_NATIVE_INTEGER, data, dims, status)


     !   
     ! End access to the dataset and release resources used by it.
     ! 
     CALL h5dclose_f(dataset, status)

     !
     ! Create the dataset for the second 2d array by reusing the dataspace
     !
     CALL h5dcreate_f(file, dsetname2, H5T_NATIVE_INTEGER, dataspace, &
                      dataset, status)
      
     !
     ! Terminate access to the data space.
     !
     CALL h5sclose_f(dataspace, status)

     ! Create the 3D dataspace to make the selection in memory.
     !
     dims(1) = NX
     dims(2) = NY
     dims(3) = NZ
     CALL h5screate_simple_f(3, dims, dataspace, status)
     array_subsize(1) = NX
     array_subsize(2) = NY
     array_subsize(3) = 1
     array_start(1) = 0 ! C convention 
     array_start(2) = 0 ! C convention 
     array_start(3) = 2 ! C convention 
     CALL h5sselect_hyperslab_f (dataspace, H5S_SELECT_SET_F, array_start, array_subsize, status)

    
     
     !
     ! Write the data actually
     !
     CALL h5dwrite_f(dataset, H5T_NATIVE_INTEGER, data3d, dims, status, mem_space_id=dataspace)


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

     END PROGRAM HDF5_EX
     
