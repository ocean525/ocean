module var
	implicit none
    real,allocatable :: lon(:), lat(:), blon(:), blat(:)
    real,allocatable :: u10(:,:), bu10(:,:), W(:,:)
    real :: res, bres, lons, lats, lone, late
    integer :: i, j, kx, ky
    character :: temp
end module var

program interpol_my
	use var
    implicit none

! read u10 from file
    open(1,file='20050429_08.sfc')
    read(1,*)
    read(1,*) temp,temp,kx,temp,ky
    read(1,*) temp,temp,temp,lons,lone
    read(1,*) temp,temp,temp,lats,late
    read(1,*) temp,temp,res
    do i=1,4
        read(1,*)
    enddo

    allocate(u10(kx,ky),W(kx,ky))
    allocate(lon(int((lone-lons)/res)),lat(int((late-lats)/res)))

! lon(:),lat(:)
!	do i=1,kx
!	lon(i)=lons+(i-1)*res
!	end do
!	do i=1,ky
!	lat(i)=lats+(i-1)*res
!	enddo

!	write(*,*) int((lone-lons)/res),lon(kx),lat(ky)

    read(1,*)
    read(1,'(15f7.2)') u10
    
    close(1)
    write(*,*) 'kx=',kx,'ky=',ky,'res=',res,'lons=',lons&
    ,'lone=',lone,'lats=',lats,'late=',late

    write(*,*) 'u10=',u10,'size(u10)=',size(u10,1)

end program interpol_my