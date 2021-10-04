module var
!define var
    implicit none
    real,allocatable :: lon(:), lat(:), blon(:), blat(:) 
    real,allocatable :: u10(:,:), W(:,:,:), bu10(:,:), lonn(:,:),&
    latt(:,:),blonn(:,:),blatt(:,:)
    real :: res, bresx, bresy, lons, lats, lone, late, blons, blone, blats, blate
    integer :: i, j, kx, ky, nx, ny
    character :: temp
end module var

program interpol_my
    use var
    implicit none
    integer :: ia,ib,ic,id
    real :: b(4),x(4),y(4)
    real :: ra,rb,rc,rd
    real :: Bllinear_my
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
    allocate(u10(kx,ky))
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
    write(*,*) 'give  blons,blone,blats,blate,nx,ny'
    read(*,*) blons,blone,blats,blate,nx,ny
!    blons=120
!    blone=121
!    blats=35
!    blate=36
!    nx=2
!    ny=2
    allocate(blon(nx),blat(ny))
    bresx=(blone-blons)/nx
    bresy=(blate-blats)/ny
    do i=1,nx
    blon(i)=blons+bresx*(i-1)
    enddo
    do i=1,ny
    blat(i)=blats+bresy*(i-1)
    enddo
    write(*,*) 'blon=',blon,'blat=',blat
!set W
    allocate(lonn(ky,kx),latt(ky,kx),blonn(ny,nx),blatt(ny,nx))
    do i=1,ky
    lonn(i,:)=lon(:)
    enddo
    do i=1,kx
    latt(:,i)=lat(:)
    enddo
    do i=1,ny
    blonn(i,:)=blon(:)
    enddo
    do i=1,nx
    blatt(:,i)=blat(:)
    enddo

    allocate(bu10(ny,nx),W(ny,nx,2))
   
    do i=1,ny
    do j=1,nx
    W(i,j,1)=(blonn(i,j)-lons)/res
    W(i,j,2)=(blatt(i,j)-lats)/res
    enddo
    enddo

    !write(*,*) W(1,1,:)



!find bu10
    do i=1,ny
    do j=1,nx
    ia=W(i,j,1)
    ib=W(i,j,2) 

   ! write(*,*) 'ia,ib=',ia,ib 
    x(1:4)=[lon(ia),lon(ia+1),lon(ia+1),lon(ia)]
    y(1:4)=[lat(ib),lat(ib),lat(ib+1),lat(ib+1)] 
    b(1:4)=[u10(ia,ib),u10(ia+1,ib),u10(ia+1,ib+1),u10(ia,ib+1)]
    bu10(i,j)=Bllinear_my(blonn(i,j),blatt(i,j),b,x,y)

    write(*,*) '------four-point------'
    write(*,*) 'x=',x,'y=',y,'b=',b
    write(*,*) '------mid-point------'
    write(*,*) blonn(i,j),blatt(i,j),bu10(i,j)
    write(*,*) '------',i,j,'------'
    enddo
    enddo
 
end program interpol_my


