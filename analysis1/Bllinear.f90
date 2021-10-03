function Bllinear_my(xo,yo,b,x,y) result(a)
    implicit none
    real :: xo, yo
    real :: b(4),x(4),y(4)
    real :: dxo, dyo, a
    dxo=(xo-x(1))/(x(2)-x(1))
    dyo=(yo-y(1))/(y(4)-y(1))
    a=dxo*(dyo*b(3)+(1-dyo)*b(2))+&
	(1-dxo)*(dyo*b(4)+(1-dyo)*b(1))
	return
end function Bllinear_my

program main_Bllinear
    implicit none
    real :: xo, yo
    real :: bg(4),x(4),y(4)
    real :: a
	real :: Bllinear_my

    xo=3
    yo=3
    data bg /2,4,8,4/
    data x /2,4,4,2/
    data y /2,2,4,4/

!    xo=2.5
!    yo=2.5
!    data bg /2,4,8,4/
!    data x /2,3,3,2/
!    data y /2,2,3,3/

    a=Bllinear_my(xo,yo,bg,x,y)
    write(*,*) 'a=',a

end program main_Bllinear