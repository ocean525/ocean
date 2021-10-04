    program main
    implicit none
    real,allocatable :: dis(:,:),W(:,:),W1(:)
    
    integer :: i,j,k,ia,ib,ic
    real :: ra,rb(9),rc
    real :: lon(4),lat(4),a(4)
    real :: lonb(3,3),latb(3,3),b(3,3)
    
    data lon/121.5,122.5,122.5,121.5/
    data lat/31.5,31.5,32.5,32.5/
    data lonb/120,122,124,120,122,124,120,122,124/
    data latb/30,30,30,32,32,32,34,34,34/

    data b/2,4,6,4,6,8,6,8,10/
! 1.6    
write(*,*) 'start'
    allocate(dis(9,4),W(9,4),W1(9))
    ia=0
    do i=1,3
    do j=1,3
    ia=ia+1
    do k=1,4
    dis(ia,k)=sqrt((lonb(i,j)-lon(k))**2+(latb(i,j)-lat(k))**2)
    enddo
    enddo
    enddo


    do i=1,9
    do k=1,4
    W(i,k)=(1.6**2-dis(i,k)**2)/(1.6**2+dis(i,k)**2)
    enddo
    enddo

write(*,*) '1'


    rb=reshape(b,(/9/))
    do k=1,4
    a(k)=sum(W(:,k)*rb)/sum(W(:,k))
    enddo
    write(*,*) sum(W(:,1)*rb),sum(W(:,1))

    write(*,*) a

    end
    
