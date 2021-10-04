module var
!define var
    implicit none
    real,allocatable :: lon(:), lat(:), blon(:), blat(:)&
    ,lon1(:),lat1(:),dis(:),u101(:),W1(:,:)
    real,allocatable :: u10(:,:), bu10(:,:), lonn(:,:),&
    latt(:,:),blonn(:,:),blatt(:,:),dis1(:,:),W(:,:,:)
    real :: res, bresx, bresy, lons, lats, lone, late, blons, blone, blats, blate
    integer :: i, j, k, kx, ky, nx, ny
    character :: temp
end module var

program interpol_my
    use var
    implicit none
    integer :: ia,ib,ic,id
    real :: b(4),x(4),y(4)
    real :: ra,rb,rc,rd
    real :: weighted_my
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
!    read(*,*) blons,blone,blats,blate,nx,ny
    blons=120
    blone=121
    blats=35
    blate=36
    nx=2
    ny=2
    allocate(blon(nx),blat(ny))
    bresx=(blone-blons)/nx
    bresy=(blate-blats)/ny
    do i=1,nx
    blon(i)=blons+bresx*(i-1)
    enddo
    do i=1,ny
    blat(i)=blats+bresy*(i-1)
    enddo
  !  write(*,*) 'blon=',blon,'blat=',blat
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

  !  write(*,*) lonn


    allocate(bu10(ny,nx),W(ny,nx,2))
    ib=((2/res)*2+1)**2
    allocate(lon1(ib),lat1(ib),dis(ib),u101(ib),W1(ib,4),dis1(ib,4)) 
    write(*,*) kx*ky,size(lon1)
  
    do i=1,ny
    do j=1,nx
    W(i,j,1)=(blonn(i,j)-lons)/res
    W(i,j,2)=(blatt(i,j)-lats)/res
    enddo
    enddo


    lon1=reshape(lonn(1:41,(W(i,j,1)-2/res):(W(i,j,1)+2/res)),(/41*41/))
    lat1=reshape(latt((W(i,j,2)-2/res):(W(i,j,2)+2/res),1:41),(/41*41/))
    u101=reshape(u10((W(i,j,2)-2/res):(W(i,j,2)+2/res),(W(i,j,1)-2/res):(W(i,j,1)+2/res)),(/41*41/))

    write(*,*) 'reshape succeed',ny,nx,kx*ky
!find bu10
    
    do i=1,ny
    do j=1,nx
    ib=ib+1
    do k=1,ib
    call cal_length(blonn(i,j),blatt(i,j),lon1(k),lat1(k),dis(k))
    write(*,*) k
    enddo
    dis1(:,ib)=dis(:)
    enddo
    enddo
  !  write(*,*) 'dis=',dis
    
  !  do i=1,ny
  !  do j=1,nx
!    bu10(i,j)=weighted_my(u101,dis,W1,ib)
  !  enddo
  !  enddo

    do k=1,4
    do i=1,ib
    if (dis1(i,k)<4) then
    W1(i,k)=(16-dis1(i,k)**2)/(16+dis1(i,k)**2)
    write(*,*) 'i=',i
    else
    W1(i,k)=0
    endif
    enddo
    enddo 
!    write(*,*) 'final wrong'

    do i=1,ny
    do j=1,nx
    rc=rc+1
    bu10(i,j)=sum(W1(:,rc)*u101(:))/sum(W1(:,rc))
    enddo
    enddo

    write(*,*) sum(W1(:,1)*u101(:)),sum(W1(:,1))


    write(*,*) '------ori------'
    write(*,*) '120,35=',u10(30,50)
    write(*,*) '------own------'
    write(*,*) blonn(1,1),blatt(1,1),bu10(1,1)
 
end program interpol_my


Subroutine cal_length(x1,y1,x2,y2,dis) 
  implicit none
  real :: x1,y1,x2,y2,dis
  dis=sqrt((abs(x2-x1))**2+(abs(y2-y1))**2)
  return
end subroutine cal_length
                       

