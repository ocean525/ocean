 MODULE sfc_data
   !
   ! --- define prepared data
   real, allocatable, dimension(:,:) :: lon, lat, u10, v10, slp
   !   
 END MODULE sfc_data


 PROGRAM read_interpol_sfc
   !
   USE sfc_data
   !
   implicit none
   !
   character(len=20)  :: in_file_name
   !
   ! 
   ! === Part 1: read 
   !
   in_file_name = '20050429_08.sfc'
   CALL read_sfc(in_file_name)
   ! --- check
   write(*,*) 'size of lon, u10 = ', size(lon), size(u10)
   write(*,*) 'u10(1,1)         = ', u10(1,1) 
   write(*,*) 'slp(max,max)     = ', slp(size(slp,1),size(slp,2)) 
   !
   !
 END PROGRAM read_interpol_sfc


 SUBROUTINE read_sfc( in_file )
   !
   USE sfc_data, only: lon, lat, u10, v10, slp
   !
   ! --- define variables
   character(len=20), intent(in)  :: in_file
   !
   integer :: kx, ky, i, j, k
   character(len=20) :: temp
   real :: beg_lon, beg_lat,end_lon, end_lat, resolution
   !
   ! --- read information
   open(1,file=trim(in_file), status='old', action='read')
   read(1,*)
   read(1,*) temp, temp, kx, temp, ky
   read(1,*) temp, temp, temp, beg_lon, end_lon
   read(1,*) temp, temp, temp, beg_lat, end_lat
   read(1,*) temp, temp, resolution
   do k = 1, 4
     read(1,*)
   enddo
   !
   ! --- determine dimensions based on information read above
   allocate( lon(kx,ky), lat(kx,ky), u10(kx,ky), v10(kx,ky), slp(kx,ky) )
   !
   ! --- give values to position
   do i = 1, kx
   do j = 1, ky
     lon(i,j) = beg_lon + (i-1)*resolution
     lat(i,j) = beg_lat + (j-1)*resolution
   enddo
   enddo
   !
   ! --- read u10, v10 and slp
   read(1,*)
   read(1,'(15f7.2)') u10
   read(1,*)
   read(1,'(15f7.2)') v10
   read(1,*)
   read(1,'(15f7.2)') slp
   !
   ! --- close file yyyymmdd_hh.sfc
   close(1)
   !
 END SUBROUTINE read_sfc

