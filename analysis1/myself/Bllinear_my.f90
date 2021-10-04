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


