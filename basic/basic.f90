! 注释
! 1 定义整数，实数，字符串，数组，矩阵
integer :: a
integer, allocatable:: h(:,:)
real,parameter  :: earth_w=7.292e-5
real, save      :: lon,lat,res,a
real, allocatable:: dx(:)
real, allocatable:: u(:,:,:)
integer, dimension (10) :: a
real, dimension (10, 10) :: a,b,c
character(len=20) :: c
logical switch


u=0. !初始化

read(1,'(a7,i5)') str5,nx
allocate(dx(ny),f(ny),ddx(ny))


! 2 字符串拼接，切割，矩阵拼接，切割
c=a//b !字符串拼接
!a=2007,aa='2007'
write(bb,"(I4)") a     !...a -> bb  整型转字符
read(aa,'(I4)') b       !...aa -> b 字符转整型

! 3 特别类型
open(1,file='inp_change.dat')
read(1,'(a7,f5.2)') str1,lon

print*,str1,lon
! 4 循环
    do k=1,i_nt
    do j=ny,1,-1
	write(1,'(<nx>f5.2)'), z(:,j,k)! 输出水位，潮流
	write(2,'(<nx>f5.2)'), u(:,j,k)
	write(3,'(<nx>f5.2)'), v(:,j,k)
	end do
	end do
! 5 判断
    .AND. 与

    .OR.  或
    
    .NOT. 非
 == >= <= /= > <

if(.not.dptype)then
    open(1, file='inp_depth.dat', action='read')
    do j=ny,1,-1
    read(1,*), h(:,j)
    end do
    close(1)
 else
    h=dp
end if

! 6 函数
module var
    !
end module var

use var

subroutine Fourier_z
    !
end subroutine Fourier_z 

call Fourier_z