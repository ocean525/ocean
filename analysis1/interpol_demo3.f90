 MODULE sfc_data
   !
   ! --- define prepared data
   real, allocatable, dimension(:)   :: lon, lat
   real, allocatable, dimension(:,:) :: u10, v10, slp
   !   
   ! --- define output data
   real, parameter   :: lon_beg = 117.0 , lon_end = 124.0, &
                        lat_beg = 36.0  , lat_end = 41.0, &
                        resolution = 0.05
   real, allocatable, dimension(:)   :: slon, slat
   real, allocatable, dimension(:,:) :: su10, sv10, sslp
   !
   !
 END MODULE sfc_data


 PROGRAM read_interpol_sfc
   !
   USE sfc_data
   !
   implicit none
   !
   character(len=20)  :: in_file_name
   integer            :: kx, ky, i, j, ii, jj
   !
   ! --- method 1: four_point_average
   real                  :: four_point_average
   real                  :: delt_x, delt_y
   integer,dimension(4)  :: bg_grid_x, bg_grid_y
   real,dimension(4)     :: bg
   real,dimension(4)     :: bg_x, bg_y
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
   ! === Part 2: interpolate 
   !
   ! --- 2.1 determine dimension of slon, slat, su10, sv10, sslp   
   kx = nint( (lon_end - lon_beg)/resolution )
   ky = nint( (lat_end - lat_beg)/resolution )
   ! --- check
   write(*,*) ' kx, ky = ', kx, ky 
   !
   allocate( slon(kx), slat(ky) )
   allocate( su10(kx, ky), sv10(kx,ky), sslp(kx,ky) )
   !
   ! --- 2.2 determine slon and slat
   do i = 1, kx
     slon(i) = lon_beg + (i-1)*resolution
   enddo
   do j = 1, ky
     slat(j) = lat_beg + (j-1)*resolution
   enddo
   ! --- 2.3 interpolation: Method 1, four_point_average
   ! FUNCTION four_point_average(xobs, yobs, delt_x, del_y, bg, bg_x, bg_y) result(a)
   !
   !             (x4,y4)      (x3,y3)
   !              ---------------
   !              |             |
   !              |             |
   !              |      *      |
   !              | (xobs,yobs) |
   !              |             |
   !              ---------------
   !             (x1,y1)       (x2,y2)
   !
   LOOP_interpol: &
   do i = 1, kx
   do j = 1, ky
     ! --- at first, look for which four points around a given location
     ! --- x direction
     do ii = 1, size(lon) - 1
       if( slon(i) >= lon(ii) .and. slon(i) < lon(ii+1) ) then
         bg_grid_x(1) = ii 
         bg_grid_x(2) = ii + 1 
         bg_grid_x(3) = ii + 1
         bg_grid_x(4) = ii 
       endif
     enddo
     ! --- y direction
     do jj = 1, size(lat) - 1
       if( slat(j) >= lat(jj) .and. slat(j) < lat(jj+1) ) then
         bg_grid_y(1) = jj 
         bg_grid_y(2) = jj 
         bg_grid_y(3) = jj + 1
         bg_grid_y(4) = jj + 1 
       endif
     enddo
     !
     ! --- determine bg, bg_x, bg_y, delt_x, delt_y
     delt_x = lon(2) - lon(1)
     delt_y = lat(2) - lat(1)
     do ii = 1, 4
       bg_x(ii) = lon( bg_grid_x(ii) ) 
       bg_y(ii) = lat( bg_grid_y(ii) ) 
     enddo
     ! --- do interpolation
     ! --- su10
     do ii = 1, 4
       bg(ii) = u10( bg_grid_x(ii), bg_grid_y(ii) )
     enddo
     su10(i,j) = four_point_average(slon(i), slat(j), delt_x, delt_y, bg, bg_x, bg_y)
     ! --- sv10
     do ii = 1, 4
       bg(ii) = v10( bg_grid_x(ii), bg_grid_y(ii) )
     enddo
     sv10(i,j) = four_point_average(slon(i), slat(j), delt_x, delt_y, bg, bg_x, bg_y)
     ! --- sslp
     do ii = 1, 4
       bg(ii) = slp( bg_grid_x(ii), bg_grid_y(ii) )
     enddo
     sslp(i,j) = four_point_average(slon(i), slat(j), delt_x, delt_y, bg, bg_x, bg_y)
     !
   enddo
   enddo &
   LOOP_interpol
   !
   ! --- 2.4 check using GrADS plotting
   CALL grads_plot
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
   allocate( lon(kx), lat(ky) )
   allocate( u10(kx,ky), v10(kx,ky), slp(kx,ky) )
   !
   ! --- give values to position
   do i = 1, kx
     lon(i) = beg_lon + (i-1)*resolution
   enddo
   do j = 1, ky
     lat(j) = beg_lat + (j-1)*resolution
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


 FUNCTION four_point_average(xobs, yobs, delt_x, delt_y, bg, bg_x, bg_y) result(a)
   !
   ! --- This is a weighted mean in both the x and y directions. 
   ! ---                                                                    
   ! --- Creatde by GAO Shanhong, 26 July 2002, Qingdao. 
   ! ---                                          
   !                                          
   implicit none
   !
   real,intent(in)               ::  xobs, yobs, delt_x, delt_y
   real,dimension(4),intent(in)  ::  bg, bg_x, bg_y
   real                          ::  a, dxob, dyob
   !
   !  Calculate values of first guess at the observation location:
   !  note that i is in west-east (or x) direction, and j in south-north
   !  (or y) direction, linear interpolation.  
   !
   !
   !                    (x4,y4)      (x3,y3)
   !               -        ---------------
   !        1-dyob |        |             |
   !               |        |             |
   !               -        |      *      |
   !          dyob |        | (xobs,yobs) |
   !               |        |             |
   !               -        ---------------
   !                    (x1,y1)       (x2,y2)
   !
   !                        |------|------|
   !                          dxob  1-dxob   
   !
   ! --- normalizing
   dxob  = ( xobs - bg_x(1) ) / delt_x 
   dyob  = ( yobs - bg_y(1) ) / delt_y
   !   
   a     = ( 1.-dxob ) * ( ( 1.-dyob ) * bg(1) +  dyob * bg(4) )   &
           +    dxob *   ( ( 1.-dyob ) * bg(2) +  dyob * bg(3) )    
   !
   ! --- check
   !
   !write(*,'(5f12.2)') bg(1), bg(2), bg(3), bg(4), a, delt_x, delt_y
   !write(*,*) bg_x
   !write(*,*) bg_y
   !write(*,*) xobs, yobs
   !pause
   !
   ! --- end of check
   !
   !
 END FUNCTION four_point_average


 SUBROUTINE grads_plot
   !
   USE sfc_data, only: slon, slat, su10, sv10, sslp, &
                       lon_beg, lat_beg, resolution

   !
   implicit none
   !
   integer :: kx, ky
   !
   ! --- write out sfc.dat
   kx = size(slon)
   ky = size(slat)
   open(1, file='sfc.dat', form='unformatted', recl=kx*ky, action='write')
     write(1,rec=1) su10 
     write(1,rec=2) sv10 
     write(1,rec=3) sslp 
   close(1)
   !
   ! --- write out sfc.ctl
   open(2, file='sfc.ctl', action='write')
   write(2,'(a)') 'dset sfc.dat'
   write(2,'(a,f8.1)') 'undef -999.0'
   write(2,'(a)') 'title checkof interpolation'
   write(2,'(a,i5,a,f8.1,f8.3)') 'xdef ', kx, ' linear ', lon_beg, resolution
   write(2,'(a,i5,a,f8.1,f8.3)') 'ydef ', ky, ' linear ', lat_beg, resolution
   write(2,'(a)') 'zdef  1 linear 1 1'
   write(2,'(a)') 'tdef  1 linear 12Z17APR2005 12hr'
   write(2,'(a)') 'vars 3'
   write(2,'(a)') 'u   0 99 Surface U'
   write(2,'(a)') 'v   0 99 Surface V'
   write(2,'(a)') 'slp 0 99 Surface slp'
   write(2,'(a)') 'endvars'
   close(2)
   !
   !
 END SUBROUTINE grads_plot
