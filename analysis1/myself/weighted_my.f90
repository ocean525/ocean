    subroutine cal_length(x1,y1,x2,y2,dis)
    implicit none
    real :: x1,y1,x2,y2,dis
    dis=sqrt((abs(x2-x1))**2+(abs(y2-y1))**2)
    return
    end subroutine cal_length
 
  
    function weighted_my(b,dis,W,ia) result(aa)
    implicit none
    real :: aa
    real :: b(:),dis(:),W(:)
    integer :: i,ia
    write(*,*) 'dis=',dis
    
    do i=1,ia
    if (dis(i)<2) then
    W(i)=(4-dis(i)**2)/(4+dis(i)**2)
    else
    W(i)=0
    endif
    enddo

    aa=sum(W(:)*b(:))/sum(W(:))

    end function weighted_my


