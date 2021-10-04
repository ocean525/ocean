    program weighted
    implicit none
    real :: xo,yo,a
    real,allocatable :: b(:),x(:),y(:),dis(:),W(:)
    real :: weighted_my
    integer :: i,ia
    data xo,yo/120,35/
    ia=4
    allocate(b(4),x(4),y(4),W(4),dis(4)) 
    b(1:4)=[2,4,4,8]
    x(1:4)=[119,122,122,119]
    y(1:4)=[34,34,36,36]
    do i=1,4
    call cal_length(xo,yo,x(i),y(i),dis(i)) 
    enddo

    a=weighted_my(b,dis,ia)
    end program weighted 

    subroutine cal_length(x1,y1,x2,y2,dis1)
    implicit none
    real :: x1,y1,x2,y2,dis1
    dis1=sqrt((abs(x2-x1))**2+(abs(y2-y1))**2)
    return
    end subroutine cal_length
 
  
    function weighted_my(b,dis,ia) result(aa)
    implicit none
    real :: aa
    real :: b(ia),dis(ia),W(ia)
    integer :: i,ia

    do i=1,ia
    if (dis(i)<3) then
    W(i)=(9-dis(i)**2)/(9+dis(i)**2)
    else
    W(i)=0
    endif
    enddo

    aa=sum(W(:)*b(:))/sum(W(:))

    end function weighted_my


