program main_Bllinear
    implicit none
    real :: xo, yo
    real :: bg(4),x(4),y(4)
    real :: a
	real :: Bllinear_my

    xo=120.5
    yo=35.0
    data bg /2.2,4.3,8.4,4.5/
    data x /120.4,120.5,120.5,120.4/
    data y /34.9,34.9,35.0,35.0/

!    xo=2.5
!    yo=2.5
!    data bg /2,4,8,4/
!    data x /2,3,3,2/
!    data y /2,2,3,3/

    a=Bllinear_my(xo,yo,bg,x,y)
    write(*,*) 'a=',a

end program main_Bllinear
