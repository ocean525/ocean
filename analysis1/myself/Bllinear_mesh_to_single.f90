module var
!define var
    implicit none
    real,allocatable :: lon(:), lat(:)
    real,allocatable :: u10(:,:), W(:,:)
    real :: res, bres, lons, lats, lone, late, blon, blat, bu10
    integer :: i, j, kx, ky
    character :: temp
end module var

program interpol_my
    use var
    implicit none
    integer :: ia,ib,ic,id
    real :: b(4),x(4),y(4)
    real :: ra,rb,rc,rd
    real :: Bllinear_my
    real :: dxo, dyo, a
 
! read lon,u10... from file
    open(1,file='20050429_08.sfc')
    read(1,*)
    read(1,*) temp,temp,kx,temp,ky
    read(1,*) temp,temp,temp,lons,lone
    read(1,*) temp,temp,temp,lats,late
    read(1,*) temp,temp,res
    do i=1,4
        read(1,*)
    enddo

! allocate u10 and so on
    allocate(u10(kx,ky),W(kx,ky))
    allocate(lon(kx),lat(ky))

! give value to lon(:),lat(:)
    do i=1,kx
    lon(i)=lons+(i-1)*res
    end do
    do i=1,ky
    lat(i)=lats+(i-1)*res
    enddo

    !write(*,*) lon,lat

    read(1,*)
    read(1,'(15f7.2)') u10
    
    close(1)
!    write(*,*) 'kx=',kx,'ky=',ky,'res=',res,'lons=',lons&
!    ,'lone=',lone,'lats=',lats,'late=',late,'lon=',lon,'lat=',lat

   ! write(*,*) 'u10=',u10,'size(u10)=',size(u10,1)

!read blon,blat
    write(*,*) 'give a lon between',lons,lone,'and a lat between',lats,late
    read(*,*) blon,blat
    
    write(*,*) 'blon=',blon,'blat=',blat
!set W

!find bu10
    i=1
    do while(blon>=lon(i))
    i=i+1
    ia=i-1
    enddo
   
    i=1
    do while(blat>=lat(i))
    i=i+1
    ib=i-1
    enddo
  
   ! write(*,*) 'ia,ib=',ia,ib 
    x(1:4)=[lon(ia),lon(ia+1),lon(ia+1),lon(ia)]
    y(1:4)=[lat(ib),lat(ib),lat(ib+1),lat(ib+1)] 
    b(1:4)=[u10(ia,ib),u10(ia+1,ib),u10(ia+1,ib+1),u10(ia,ib+1)]
    bu10=Bllinear_my(blon,blat,b,x,y)

    write(*,*) 'x=',x,'y=',y,'b=',b,'blon=',blon,'blat=',blat
   
    write(*,*) 'bu10=',bu10

 
end program interpol_my


