 FUNCTION four_point_average(xobs, yobs, bg, x, y) result(a)
   !
   ! --- This is a weighted mean in both the x and y directions. 
   ! ---                                                                    
   ! --- Creatde by GAO Shanhong, 26 July 2002, Qingdao. 
   ! ---                                          
   !                                          
   implicit none
   !
   real,intent(in)               ::  xobs, yobs
   real,dimension(4),intent(in)  ::  bg, x, y
   real                          ::  a, dxob, dyob
   !
   !  Calculate values of first guess at the observation location:
   !  note that i is in west-east (or x) direction, and j in south-north
   !  (or y) direction, linear interpolation.  
   !
   !
   !                    (x4,y4)      (x3,y3)
   !               -        ---------------
   !        1-dyob |        |             |
   !               |        |             |
   !               -        |      *      |
   !          dyob |        | (xobs,yobs) |
   !               |        |             |
   !               -        ---------------
   !                    (x1,y1)       (x2,y2)
   !
   !                        |------|------|
   !                          dxob  1-dxob   
   !
   !
   dxob  = xobs - x(1) 
   dyob  = yobs - y(1)
   a     = ( 1.-dxob ) * ( ( 1.-dyob ) * bg(1) +  dyob * bg(4) )   &
           +    dxob *   ( ( 1.-dyob ) * bg(2) +  dyob * bg(3) )    
   !
   ! --- check
   !
   !write(*,'(5f12.2)') bg(1), bg(2), bg(3), bg(4), a
   !
   ! --- end of check
   !
   !
 END FUNCTION four_point_average 




  ! --- Please change the following codes according to your needs!
  total_weight = 0.
  LOOP: &
  do m = m1, m2
  do n = n1, n2
     dx = ?
     dy = ?
     dis = sqrt(dx*dx + dy*dy)
     if( dis < R ) then
         weight = (R*R-dis*dis) / (R*R+dis*dis)
     else
         weight = 0.
     endif     
     a = a + VAR(m,n)*weight
     total_weight = total_weight + weight
  enddo
  enddo LOOP
  !
  if( total_weight == 0. ) then
      a = 999.90  ! --- no obervations are available
  else
      a = a/total_weight
  endif


