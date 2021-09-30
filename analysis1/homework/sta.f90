  PROGRAM grads
    !
    implicit none
    ! 
    ! --- define Data Type for Surface Measurements
    type sfc
        integer  :: id
        real     :: lon
        real     :: lat
        real     :: elevation
        real     :: slp
        real     :: temp
        real     :: td
        real     :: dire
        real     :: spd
        real     :: deltp
        real     :: rain
        real     :: wx
        real     :: vis
        real     :: cld
    end type sfc
    !
    type upr
        integer  :: id
        real     :: lon
        real     :: lat
        real,dimension(3) :: hgt
        real,dimension(3) :: temp
        real,dimension(3) :: td
        real,dimension(3) :: dire
        real,dimension(3) :: spd
    end type upr
    !
    character(len=50)  ::  filename
    character(len=8)   ::  staname
    character(len=13)  ::  date_time
    character(len=16)  ::  outpng1, outpng2
    character(len=14)  ::  outpnga, outpngb, outpngc, outpngd, outpnge
    character(len=14)  ::  outpnga1, outpnga2, outpnga3 
    character(len=14)  ::  outpngb1, outpngb2, outpngb3 
    character(len=2)   ::  yy, mm, dd, hh
    real               ::  tmp, us, vs, tim, wea
    integer            ::  num, stnum, stnum1, i, j, k, nlev, nflag
    !integer,parameter  ::  stanum=28
    !integer,dimension(stanum)            :: sta
    type(sfc),allocatable,dimension(:)   :: sfc_obs
    real,parameter     :: p1=9999., p2=-999.
    real,parameter     :: x1=114.,x2=126., y1=34., y2=42.
    !
    ! --- temperature contour plotting
    integer         :: kx, ky
    real,parameter  :: deltx = .5, &
                       lon_beg = 78., lon_end = 135., &
                       lat_beg = 15., lat_end = 52.
    real, allocatable, dimension(:,:) :: gts
    !
    ! --- upper air plotting
    integer ::  nargum, iargc
    integer ::  upr_n1, upr_n2, upr_lev
    real    ::  eleva
    type(upr),allocatable,dimension(:)  :: upr_obs
    !
    !
    nargum = iargc()
    !
    ! ==================== Part I =============================
    ! --- read surface measurements
    call GETARG(1,filename)
    open(1,file=trim(filename),status='old',action='read')
    read(1,*)
    read(1,*) yy, mm, dd, hh, num
    allocate( sfc_obs(num) )
    do i = 1, num
       read(1,*) sfc_obs(i)%id, sfc_obs(i)%lon, sfc_obs(i)%lat, sfc_obs(i)%elevation, &
                 tmp, sfc_obs(i)%cld, sfc_obs(i)%dire, sfc_obs(i)%spd, sfc_obs(i)%slp, &
                 sfc_obs(i)%deltp, tmp, tmp, sfc_obs(i)%rain, tmp, tmp, tmp, &
                 sfc_obs(i)%td, sfc_obs(i)%vis, sfc_obs(i)%wx, sfc_obs(i)%temp 
       ! --- convert default values
       if( sfc_obs(i)%slp  == p1 ) sfc_obs(i)%slp  = p2
       if( sfc_obs(i)%temp == p1 ) sfc_obs(i)%temp = p2
       if( sfc_obs(i)%td   == p1 ) sfc_obs(i)%td   = p2
       if( sfc_obs(i)%dire == p1 ) sfc_obs(i)%dire = p2
       if( sfc_obs(i)%spd  == p1 ) sfc_obs(i)%spd  = p2
       if( sfc_obs(i)%deltp== p1 ) sfc_obs(i)%deltp= p2
       if( sfc_obs(i)%rain == p1 ) sfc_obs(i)%rain = p2
       if( sfc_obs(i)%rain == 0. ) sfc_obs(i)%rain = p2
       if( sfc_obs(i)%wx   == p1 ) sfc_obs(i)%wx   = p2
       if( sfc_obs(i)%vis  == p1 ) sfc_obs(i)%vis  = p2
       if( sfc_obs(i)%cld  == p1 ) sfc_obs(i)%cld  = p2
       !
       ! --- set max. and min. values
       if( sfc_obs(i)%rain >= 500. ) sfc_obs(i)%rain = p2
       if( sfc_obs(i)%temp == 60.  ) sfc_obs(i)%temp = p2
       !
    enddo
    close(1)
    date_time(1:13) = '20'//yy//'-'//mm//'-'//dd//' '//hh
    outpng1(1:16) = '20'//yy//mm//dd//hh//'a.png'
    outpng2(1:16) = '20'//yy//mm//dd//hh//'b.png'
    outpnga(1:14) = hh//'a.png'
    outpngb(1:14) = hh//'b.png'
    outpngc(1:14) = hh//'c.png'
    outpngd(1:14) = hh//'d.png'
    outpnge(1:14) = hh//'e.png'
    !
    ! --- pick out some stations
    !open(2,file='station.dat',status='old',action='read')
    !do j = 1, stanum
    !   read(2,*) sta(j)
    !enddo
    !close(2)
    !
    ! --- for Temperature contour plotting
    kx = nint((lon_end - lon_beg )/deltx) + 1 
    ky = nint((lat_end - lat_beg )/deltx) + 1 
    allocate( gts(kx, ky) )
    gts(:,:) = 0.
    open(3,file='gts.dat',form='binary',action='write')
      write(3) ((gts(i,j),i=1,kx),j=1,ky)  
    close(3)
    deallocate( gts )
    !
    open(3,file='gts.ctl',action='write')
    write(3,'(a)') 'dset ^gts.dat'
    write(3,'(a)') 'title grid-ts'
    write(3,'(a)') 'undef -999.0'
    write(3,'(a,i3,a)') 'xdef ', kx, ' linear 78.0 1.0'
    write(3,'(a,i3,a)') 'ydef ', ky, ' linear 15.0 1.0'
    write(3,'(a)') 'zdef  1 linear 1 1'
    write(3,'(a)') 'tdef  1 linear 12z18jan1992 12hr'
    write(3,'(a)') 'vars 1'
    write(3,'(a)') 'gts 0 99 grid-ts'
    write(3,'(a)') 'endvars'
    close(3)
    !
    open(3,file='sta.dat',form='binary',action='write')
    tim= 0.
    nlev = 1
    nflag = 1
    stnum = 0
    stnum1 = 0
    do i = 1, num
      k = i
      staname = ''
      write(staname(1:5),'(i5.5)') sfc_obs(i)%id
      ! --- calculate how many stations in the given small domain
      if( sfc_obs(i)%lon >= x1 .and.  sfc_obs(i)%lon <= x2 .and. &
        sfc_obs(i)%lat >= y1 .and.  sfc_obs(i)%lat <= y2 ) then
        stnum = stnum + 1
      endif
      ! --- calculate how many stations in the given large domain
      if( sfc_obs(i)%lon >= lon_beg .and.  sfc_obs(i)%lon <= lon_end .and. &
        sfc_obs(i)%lat >= lat_beg .and.  sfc_obs(i)%lat <= lat_end ) then
        stnum1 = stnum1 + 1
      endif
      ! --- delete the station 47035, which is too closer to 54497
      if( sfc_obs(i)%id == 47035 ) cycle
      ! --- judge cloud
      if( sfc_obs(i)%cld == 0. ) then
        sfc_obs(i)%cld = 20.
      else if( sfc_obs(i)%cld >=1. .and. sfc_obs(i)%cld <=3. ) then   
        sfc_obs(i)%cld = 21.
      else if( sfc_obs(i)%cld >=4. .and. sfc_obs(i)%cld <=5. ) then   
        sfc_obs(i)%cld = 22.
      else if( sfc_obs(i)%cld >=6. .and. sfc_obs(i)%cld <=9. ) then   
        sfc_obs(i)%cld = 23.
      else if( sfc_obs(i)%cld ==10. ) then   
        sfc_obs(i)%cld = 24.
      else if( sfc_obs(i)%cld == p2 ) then   
        sfc_obs(i)%cld = 25.
      endif
      ! --- judge winds
      if( sfc_obs(i)%spd /= p2 .and. sfc_obs(i)%dire /= p2 ) then
        us = sfc_obs(i)%spd*cos(270.-sfc_obs(i)%dire) 
        vs = sfc_obs(i)%spd*sin(270.-sfc_obs(i)%dire) 
      else
        us = p2 ; vs =p2
      endif
      ! --- judge weather phenomena
      if( sfc_obs(i)%wx /= p2 ) then
        wea = sfc_obs(i)%wx
        sfc_obs(i)%wx = 0.
        if( wea == 19 ) sfc_obs(i)%wx = 1
        if( wea == 17 ) sfc_obs(i)%wx = 2 
        if( wea == 91 .or. wea == 92 ) sfc_obs(i)%wx = 3
        if( wea == 95 .or. wea == 97 ) sfc_obs(i)%wx = 4
        if( wea == 98 ) sfc_obs(i)%wx = 5
        if( wea == 93 .or. wea == 94 .or. wea == 96 ) sfc_obs(i)%wx = 6
        if( wea == 97 ) sfc_obs(i)%wx = 7
        if( wea == 95 ) sfc_obs(i)%wx = 8
        if( wea == 96 ) sfc_obs(i)%wx = 9
        if( wea == 98 ) sfc_obs(i)%wx = 10
        if( wea == 99 ) sfc_obs(i)%wx = 11
        if( wea == 67 ) sfc_obs(i)%wx = 12
        if( wea == 66 ) sfc_obs(i)%wx = 13
        if( wea == 81 .or. wea == 82 ) sfc_obs(i)%wx = 14
        if( wea == 80 ) sfc_obs(i)%wx = 15 
        if( wea == 21 .or. wea == 60 .or. wea == 61 ) sfc_obs(i)%wx = 16
        if( wea == 62 .or. wea == 63 ) sfc_obs(i)%wx = 17
        if( wea == 64 .or. wea == 65 ) sfc_obs(i)%wx = 18
        if( wea == 85 ) sfc_obs(i)%wx = 19
        if( wea == 86 ) sfc_obs(i)%wx = 20
        if( wea == 70 .or. wea == 71 ) sfc_obs(i)%wx = 21
        if( wea == 72 .or. wea == 73 ) sfc_obs(i)%wx = 22
        if( wea == 74 .or. wea == 75 ) sfc_obs(i)%wx = 23
        if( wea == 23 ) sfc_obs(i)%wx = 24
        if( wea == 79 ) sfc_obs(i)%wx = 25
        if( wea == 57 ) sfc_obs(i)%wx = 26
        if( wea == 56 ) sfc_obs(i)%wx = 27
        if( wea == 50 .or. wea == 51 ) sfc_obs(i)%wx = 28
        if( wea == 52 .or. wea == 53 ) sfc_obs(i)%wx = 29
        if( wea == 54 .or. wea == 55 ) sfc_obs(i)%wx = 30
        if( wea == 89 ) sfc_obs(i)%wx = 31
        if( wea == 90 ) sfc_obs(i)%wx = 32
        if( wea == 76 ) sfc_obs(i)%wx = 33
        if( wea >= 10 .and. wea <= 12 ) sfc_obs(i)%wx = 34
        if( wea >= 36 .and. wea <= 39 ) sfc_obs(i)%wx = 35
        if( wea >= 30 .and. wea <= 35 ) sfc_obs(i)%wx = 36
        if( wea == 23 .and. wea == 26 ) sfc_obs(i)%wx = 37
        if( wea == 13 ) sfc_obs(i)%wx = 38
        if( wea >= 4 .and. wea <= 9 ) sfc_obs(i)%wx = 39
      endif
      !
      write(3) staname(1:8), sfc_obs(i)%lat, sfc_obs(i)%lon, tim, nlev, nflag
      write(3) us, vs, sfc_obs(i)%temp,  sfc_obs(i)%td,  sfc_obs(i)%slp, &
               sfc_obs(i)%deltp,  sfc_obs(i)%rain, sfc_obs(i)%vis, &
               sfc_obs(i)%wx, sfc_obs(i)%cld
      ! --- check
      !write(4,*) staname, sfc_obs(i)%lat, sfc_obs(i)%lon, tim, nlev, nflag
      !write(4,*) us, vs, sfc_obs(i)%temp,  sfc_obs(i)%td,  sfc_obs(i)%slp, &
      !           sfc_obs(i)%vis, sfc_obs(i)%wx, sfc_obs(i)%cld
    enddo
    nlev = 0
    write(3) staname(1:8), sfc_obs(k)%lat, sfc_obs(k)%lon, tim, nlev, nflag
    close(3)
    !
    ! --- generate sta.gs  
    open(5,file='sta1.gs',action='write')
    write(5,'(a)') "'open sta.ctl'"
    write(5,'(a)') "'set font 2'"
    write(5,'(a)') "'set grads off'"
    write(5,'(a)') "'set mpdset cnworld cnriver'"
    write(5,'(a)') "'set map 4'"
    write(5,'(a)') "'set parea 0.2 11.0 0.25 8.45'"
    write(5,'(a)') "'set lon 114.0 126.0'"
    write(5,'(a)') "'set lat 34.0 42.0'"
    write(5,'(a)') "'set xlint 2'"
    write(5,'(a)') "'set ylint 2'"
    write(5,'(a)') " 'set gxout model'"
    write(5,'(a)') " 'set digsiz 0.10'"
    write(5,'(a)') " 'set wxcols 2 2 2 2 2 2'"
    write(5,'(a)') " 'd us;vs;ts;td;slp;delt;cld;wx;vis'"
    write(5,'(a)') " 'set line 5'"
    write(5,'(a)') " 'draw recf 6.45 0.26 10.70 1.9'"
    write(5,'(a)') " 'set string 2 '"
    write(5,'(a)') "'set strsiz 0.12'"
    write(5,'(a)') " 'draw string 6.6 1.7  Surface Measurements'"
    write(5,'(a)') " 'draw string 6.6 1.45  Time: "//date_time//":00LST'"
    write(5,'(a)') " 'set string 1 '"
    write(5,'(a)') " 'set strsiz 0.10'"
    write(5,'(a)') " 'draw string 6.50 1.15  Data from http://www.weather.org.uk'"
    write(5,'(a)') " '!date +%H:%M > tmp_grads_gdate'"
    write(5,'(a)') " result = read (tmp_grads_gdate)"
    write(5,'(a)') " gdate = sublin(result,2)"
    write(5,'(a)') " '!rm -f tmp_grads_gdate'"
    write(5,'(a)') " 'draw string 6.5 0.90 Generated at 'gdate' LST'"
    write(5,'(a)') " 'draw string 6.5 0.56  By Dr.Gao Shanhong using GrADS'"
    write(5,'(a)') " 'draw string 6.5 0.35  Coastal Meteorology Group,OUC'"
    write(5,'(a)') " 'set line 1'"
    write(5,'(a)') " 'draw mark 3 10.2 0.9 0.10'"
    write(5,'(a)') " 'draw line 10.2 0.9 10.5  0.6'"
    write(5,'(a)') " 'draw line 10.5 0.6 10.4  0.35'"
    write(5,'(a)') " 'set strsiz 0.08'"
    write(5,'(a)') " 'draw string 10.3 1.0 SLP'"
    write(5,'(a)') " 'draw string 10.48 0.815 P'"
    write(5,'(a)') " 'draw line 10.35 0.81 10.40 0.90'"
    write(5,'(a)') " 'draw line 10.35 0.81 10.45 0.81'"
    write(5,'(a)') " 'draw line 10.45 0.81 10.40 0.90'"
    write(5,'(a)') " 'draw string 10.0 1.0 T'"
    write(5,'(a)') " 'draw string 10.0 0.65 Td'"
    write(5,'(a)') " 'draw string 9.65 0.82 Vis'"
    write(5,'(a)') " 'q gxinfo'"
    write(5,'(a)') " line3 = sublin(result,3)"
    write(5,'(a)') " line4 = sublin(result,4)"
    write(5,'(a)') " x1 = subwrd(line3,4)"
    write(5,'(a)') " x2 = subwrd(line3,6)"
    write(5,'(a)') " y1 = subwrd(line4,4)"
    write(5,'(a)') " y2 = subwrd(line4,6)"
    write(5,'(a)') " 'set line 1 1 6'"
    write(5,'(a)') " 'draw rec 'x1' 'y1' 'x2' 'y2"
    write(5,'(a)') " 'set string 2'"
    write(5,'(a)') " 'draw string 9.90 0.82 WP'"
    if( stnum >= 1 ) then
      write(5,'(a)') "'printim "//trim(outpnga)//" x900 y675 white'"
    endif
    write(5,'(a)') "quit"
    close(5) 
    !
    !
    open(5,file='sta2.gs',action='write')
    write(5,'(a)') "'open sta.ctl'"
    write(5,'(a)') "'set font 2'"
    write(5,'(a)') "'set rgb 20 157 225 255'"
    write(5,'(a)') "'set grads off'"
    write(5,'(a)') "'set mpdset mres cnriver'"
    write(5,'(a)') "'set map 4'"
    write(5,'(a)') "'set parea 0.11 10.98 0.3 8.48'"
    write(5,'(a)') "'set lon 78.0 135.0'"
    write(5,'(a)') "'set lat 15.0 52.0'"
    write(5,'(a)') "'set xlint 10'"
    write(5,'(a)') "'set ylint 10'"
    write(5,'(a)') "'set xlopts 1 4 0.08'"
    write(5,'(a)') "'set ylopts 1 4 0.08'"
    write(5,'(a)') "'set strsiz 0.12'"
    write(5,'(a)') " 'set gxout model'"
    write(5,'(a)') " 'set digsiz 0.06'"
    write(5,'(a)') " 'set ccolor 2'"
    write(5,'(a)') " 'd us;vs;0;0;0;0;cld;wx;0'"
    write(5,'(a)') " 'basemap O 20 15 M'"
    write(5,'(a)') " 'set ccolor 1'"
    write(5,'(a)') " 'd us;vs;ts;0;0;0;cld;0;0'"
    write(5,'(a)') " 'set line 5'"
    write(5,'(a)') " 'draw recf 3.45 7.00 7.0 8.48'"
    write(5,'(a)') " 'set string 2 '"
    write(5,'(a)') " 'draw string 3.6 8.30  Surface Measurements'"
    write(5,'(a)') " 'draw string 3.6 8.10  Time: "//date_time//":00LST'"
    write(5,'(a)') " 'set string 1 '"
    write(5,'(a)') " 'set strsiz 0.085'"
    write(5,'(a)') " 'draw string 3.50 7.80  Data from http://www.weather.org.uk'"
    write(5,'(a)') " '!date +%H:%M > tmp_grads_gdate'"
    write(5,'(a)') " result = read (tmp_grads_gdate)"
    write(5,'(a)') " gdate = sublin(result,2)"
    write(5,'(a)') " '!rm -f tmp_grads_gdate'"
    write(5,'(a)') " 'draw string 3.50 7.60 Generated at 'gdate' LST'"
    write(5,'(a)') " 'set strsiz 0.10'"
    write(5,'(a)') " 'draw string 3.5 7.30  By Dr.Gao Shanhong using GrADS'"
    write(5,'(a)') " 'draw string 3.5 7.10  Coastal Meteorology Group,OUC'"
    write(5,'(a)') " 'set line 1 1 1'"
    write(5,'(a)') " 'draw mark 3 6.6 7.6 0.10'"
    write(5,'(a)') " 'draw line   6.6 7.6 6.9 7.3'"
    write(5,'(a)') " 'draw line   6.9 7.3 6.8 7.05'"
    write(5,'(a)') " 'set strsiz 0.09'"
    write(5,'(a)') " 'draw string 6.4 7.7 T'"
    write(5,'(a)') " 'q gxinfo'"
    write(5,'(a)') " line3 = sublin(result,3)"
    write(5,'(a)') " line4 = sublin(result,4)"
    write(5,'(a)') " x1 = subwrd(line3,4)"
    write(5,'(a)') " x2 = subwrd(line3,6)"
    write(5,'(a)') " y1 = subwrd(line4,4)"
    write(5,'(a)') " y2 = subwrd(line4,6)"
    write(5,'(a)') " 'set line 1 1 6'"
    write(5,'(a)') " 'draw rec 'x1' 'y1' 'x2' 'y2"
    write(5,'(a)') " 'set string 2'"
    write(5,'(a)') " 'draw string 6.30 7.52 WP'"
    if( stnum1 >= 1 ) then
      write(5,'(a)') "'printim "//trim(outpngb)//" x1010 y730 white'"
    endif
    write(5,'(a)') "quit"
    close(5) 
    !
    open(5,file='sta3.gs',action='write')
    write(5,'(a)') "'open sta.ctl'"
    write(5,'(a)') "'set font 2'"
    write(5,'(a)') "'set rgb 20 157 225 255'"
    write(5,'(a)') "'set grads off'"
    write(5,'(a)') "'set mpdset cnworld cnriver'"
    write(5,'(a)') "'set map 4'"
    write(5,'(a)') "'set parea 0.11 10.98 0.3 8.48'"
    write(5,'(a)') "'set lon 78.0 135.0'"
    write(5,'(a)') "'set lat 15.0 52.0'"
    write(5,'(a)') "'set xlint 10'"
    write(5,'(a)') "'set ylint 10'"
    write(5,'(a)') "'set xlopts 1 4 0.08'"
    write(5,'(a)') "'set ylopts 1 4 0.08'"
    write(5,'(a)') "'set strsiz 0.12'"
    write(5,'(a)') " 'set digsiz 0.06'"
    write(5,'(a)') " 'd rain'"
    write(5,'(a)') " 'basemap O 20 4 M'"
    write(5,'(a)') " 'set line 5'"
    write(5,'(a)') " 'draw recf 3.45 7.00 6.80 8.48'"
    write(5,'(a)') " 'set string 2 '"
    write(5,'(a)') " 'draw string 3.6 8.30  Past-6hr Precipitation(mm)'"
    write(5,'(a)') " 'draw string 3.6 8.10  Time: "//date_time//":00LST'"
    write(5,'(a)') " 'set string 1 '"
    write(5,'(a)') " 'set strsiz 0.085'"
    write(5,'(a)') " 'draw string 3.50 7.80  Data from http://www.weather.org.uk'"
    write(5,'(a)') " '!date +%Y-%m-%d_%H:%M > tmp_grads_gdate'"
    write(5,'(a)') " result = read (tmp_grads_gdate)"
    write(5,'(a)') " gdate = sublin(result,2)"
    write(5,'(a)') " '!rm -f tmp_grads_gdate'"
    write(5,'(a)') " 'draw string 3.50 7.60 Generated at 'gdate' LST'"
    write(5,'(a)') " 'set strsiz 0.10'"
    write(5,'(a)') " 'draw string 3.5 7.30  By Dr.Gao Shanhong using GrADS'"
    write(5,'(a)') " 'draw string 3.5 7.10  Coastal Meteorology Group,OUC'"
    write(5,'(a)') " 'q gxinfo'"
    write(5,'(a)') " line3 = sublin(result,3)"
    write(5,'(a)') " line4 = sublin(result,4)"
    write(5,'(a)') " x1 = subwrd(line3,4)"
    write(5,'(a)') " x2 = subwrd(line3,6)"
    write(5,'(a)') " y1 = subwrd(line4,4)"
    write(5,'(a)') " y2 = subwrd(line4,6)"
    write(5,'(a)') " 'set line 1 1 6'"
    write(5,'(a)') " 'draw rec 'x1' 'y1' 'x2' 'y2"
    write(5,'(a)') " 'set string 2'"
    if( stnum1 >= 1 ) then
      write(5,'(a)') "'printim "//trim(outpngc)//" x1010 y730 white'"
    endif
    write(5,'(a)') "quit"
    close(5) 
    !
    open(5,file='sta4.gs',action='write')
    write(5,'(a)') "'open gts.ctl'"
    write(5,'(a)') "'open sta.ctl'"
    write(5,'(a)') "'set font 2'"
    write(5,'(a)') "'set rgb 20 157 225 255'"
    write(5,'(a)') "'set grads off'"
    write(5,'(a)') "'set mpdset mres cnriver'"
    write(5,'(a)') "'set map 4'"
    write(5,'(a)') "'set parea 0.11 10.98 0.3 8.48'"
    write(5,'(a)') "'set lon 78.0 135.0'"
    write(5,'(a)') "'set lat 15.0 52.0'"
    write(5,'(a)') "'set xlint 10'"
    write(5,'(a)') "'set ylint 10'"
    write(5,'(a)') "'set xlopts 1 4 0.08'"
    write(5,'(a)') "'set ylopts 1 4 0.08'"
    write(5,'(a)') "'define tc=oacres(gts,ts.2)'"
    write(5,'(a)') " 'set cint 1'"
    write(5,'(a)') " 'set gxout shaded'"
    write(5,'(a)') " 'd tc'"
    write(5,'(a)') " 'basemap O 20 4 M'"
    write(5,'(a)') " 'set line 0'"
    write(5,'(a)') " 'draw recf 3.60 7.0 6.90 8.48'"
    write(5,'(a)') " 'set string 2 '"
    write(5,'(a)') "'set strsiz 0.12'"
    write(5,'(a)') " 'draw string 3.70 8.30  Surface Temperature'"
    write(5,'(a)') " 'draw string 3.70 8.10  Time: "//date_time//":00LST'"
    write(5,'(a)') " 'set string 1 '"
    write(5,'(a)') " 'set strsiz 0.085'"
    write(5,'(a)') " 'draw string 3.70 7.80  Data from http://www.weather.org.uk'"
    write(5,'(a)') " '!date +%Y-%m-%d_%H:%M > tmp_grads_gdate'"
    write(5,'(a)') " result = read (tmp_grads_gdate)"
    write(5,'(a)') " gdate = sublin(result,2)"
    write(5,'(a)') " '!rm -f tmp_grads_gdate'"
    write(5,'(a)') " 'draw string 3.70 7.60 Generated at 'gdate' LST'"
    write(5,'(a)') " 'set strsiz 0.10'"
    write(5,'(a)') " 'draw string 3.70 7.30  By Dr.Gao Shanhong using GrADS'"
    write(5,'(a)') " 'draw string 3.70 7.10  Coastal Meteorology Group,OUC'"
    write(5,'(a)') " 'q gxinfo'"
    write(5,'(a)') " line3 = sublin(result,3)"
    write(5,'(a)') " line4 = sublin(result,4)"
    write(5,'(a)') " x1 = subwrd(line3,4)"
    write(5,'(a)') " x2 = subwrd(line3,6)"
    write(5,'(a)') " y1 = subwrd(line4,4)"
    write(5,'(a)') " y2 = subwrd(line4,6)"
    write(5,'(a)') " 'cbarc.gs 10.7 1.8 0'"
    write(5,'(a)') " 'set strsiz 0.12'"
    write(5,'(a)') " 'set line 1 1 1'"
    write(5,'(a)') " 'draw string 9.80 2.00 C'"
    write(5,'(a)') " 'draw mark 2 9.78 2.03 0.05'"
    write(5,'(a)') " 'set line 1 1 6'"
    write(5,'(a)') " 'draw rec 'x1' 'y1' 'x2' 'y2"
    write(5,'(a)') " 'set string 2'"
    if( stnum1 >= 1 ) then
      write(5,'(a)') "'printim "//trim(outpngd)//" x1010 y730 white'"
    endif
    write(5,'(a)') "quit"
    close(5) 
    !
    open(5,file='sta5.gs',action='write')
    write(5,'(a)') "'open gts.ctl'"
    write(5,'(a)') "'open sta.ctl'"
    write(5,'(a)') "'set font 2'"
    write(5,'(a)') "'set rgb 20 157 225 255'"
    write(5,'(a)') "'set grads off'"
    write(5,'(a)') "'set mpdset mres cnriver'"
    write(5,'(a)') "'set map 4'"
    write(5,'(a)') "'set parea 0.11 10.98 0.3 8.48'"
    write(5,'(a)') "'set lon 78.0 135.0'"
    write(5,'(a)') "'set lat 15.0 52.0'"
    write(5,'(a)') "'set xlint 10'"
    write(5,'(a)') "'set ylint 10'"
    write(5,'(a)') "'set xlopts 1 4 0.08'"
    write(5,'(a)') "'set ylopts 1 4 0.08'"
    write(5,'(a)') "'define tc=oacres(gts,rain.2)'"
    write(5,'(a)') " 'set cmin 1.0'"
    write(5,'(a)') " 'set gxout shaded'"
    write(5,'(a)') " 'd tc'"
    write(5,'(a)') " 'basemap O 20 4 M'"
    write(5,'(a)') " 'set line 20'"
    write(5,'(a)') " 'draw recf 3.60 7.0 6.90 8.48'"
    write(5,'(a)') " 'set string 2 '"
    write(5,'(a)') " 'set strsiz 0.12'"
    write(5,'(a)') " 'draw string 3.70 8.30  Past-6hr Precipitation'"
    write(5,'(a)') " 'draw string 3.70 8.10  Time: "//date_time//":00LST'"
    write(5,'(a)') " 'set string 1 '"
    write(5,'(a)') " 'set strsiz 0.085'"
    write(5,'(a)') " 'draw string 3.70 7.80  Data from http://www.weather.org.uk'"
    write(5,'(a)') " '!date +%Y-%m-%d_%H:%M > tmp_grads_gdate'"
    write(5,'(a)') " result = read (tmp_grads_gdate)"
    write(5,'(a)') " gdate = sublin(result,2)"
    write(5,'(a)') " '!rm -f tmp_grads_gdate'"
    write(5,'(a)') " 'draw string 3.70 7.60 Generated at 'gdate' LST'"
    write(5,'(a)') " 'set strsiz 0.10'"
    write(5,'(a)') " 'draw string 3.70 7.30  By Dr.Gao Shanhong using GrADS'"
    write(5,'(a)') " 'draw string 3.70 7.10  Coastal Meteorology Group,OUC'"
    write(5,'(a)') " 'q gxinfo'"
    write(5,'(a)') " line3 = sublin(result,3)"
    write(5,'(a)') " line4 = sublin(result,4)"
    write(5,'(a)') " x1 = subwrd(line3,4)"
    write(5,'(a)') " x2 = subwrd(line3,6)"
    write(5,'(a)') " y1 = subwrd(line4,4)"
    write(5,'(a)') " y2 = subwrd(line4,6)"
    write(5,'(a)') " 'cbar.gs'"
    write(5,'(a)') " 'set strsiz 0.12'"
    write(5,'(a)') " 'draw string 10.2 3.2 mm'"
    write(5,'(a)') " 'set line 1 1 6'"
    write(5,'(a)') " 'draw rec 'x1' 'y1' 'x2' 'y2"
    write(5,'(a)') " 'set string 2'"
    if( stnum1 >= 1 ) then
      write(5,'(a)') "'printim "//trim(outpnge)//" x1010 y730 white'"
    endif
    write(5,'(a)') "quit"
    close(5) 
    ! 
    ! ==================== Part II =============================
    ! --- read upper-air soundings
    if( nargum == 1 ) stop 
    if( nargum == 2 ) then
      call GETARG(2,filename)
      open(1,file=trim(filename),status='old',action='read')
      read(1,*)
      read(1,*) yy, mm, dd, hh, num
      allocate( upr_obs(num) )
      !
      stnum1 = 0
      do i = 1, num
        upr_obs(i)%hgt(:)  = p2 
        upr_obs(i)%temp(:) = p2 
        upr_obs(i)%td(:)   = p2 
        upr_obs(i)%dire(:) = p2 
        upr_obs(i)%spd(:)  = p2 
        !
        read(1,*) upr_obs(i)%id, upr_obs(i)%lon, upr_obs(i)%lat, eleva, upr_n1, upr_n2 
        ! --- calculate how many stations in the given large domain
        if( upr_obs(i)%lon >= lon_beg .and.  upr_obs(i)%lon <= lon_end .and. &
            upr_obs(i)%lat >= lat_beg .and.  upr_obs(i)%lat <= lat_end ) then
          stnum1 = stnum1 + 1
        endif
        !
        do j = 1, upr_n1+upr_n2
          read(1,*) upr_lev
          if( upr_lev == 850 ) then
            backspace(1)
            read(1,*) upr_lev, upr_obs(i)%hgt(1), upr_obs(i)%temp(1), upr_obs(i)%td(1), &
                      upr_obs(i)%dire(1), upr_obs(i)%spd(1)
          endif
          if( upr_lev == 700 ) then
            backspace(1)
            read(1,*) upr_lev, upr_obs(i)%hgt(2), upr_obs(i)%temp(2), upr_obs(i)%td(2), &
                      upr_obs(i)%dire(2), upr_obs(i)%spd(2)
          endif
          if( upr_lev == 500 ) then
            backspace(1)
            read(1,*) upr_lev, upr_obs(i)%hgt(3), upr_obs(i)%temp(3), upr_obs(i)%td(3), &
                      upr_obs(i)%dire(3), upr_obs(i)%spd(3)
          endif
          ! --- process default values ( == p1)
          where( upr_obs(i)%hgt(:) == p1 )  
            upr_obs(i)%hgt(:) = p2
          end where 
          where( upr_obs(i)%temp(:) == p1 )
            upr_obs(i)%temp(:) = p2
          end where
          where( upr_obs(i)%td(:) == p1 )
            upr_obs(i)%td(:) = p2
          end where
          where( upr_obs(i)%dire(:) == p1 )
            upr_obs(i)%dire(:) = p2
          end where
          where( upr_obs(i)%spd(:) == p1 )
            upr_obs(i)%spd(:) = p2
          end where
          !
        enddo
      enddo
      close(1)
    endif
    ! --- write out upr_oba(:) for plotting
    do j = 1, 3
      if( j == 1 ) open(3,file='upr1.dat',form='binary',action='write')
      if( j == 2 ) open(3,file='upr2.dat',form='binary',action='write')
      if( j == 3 ) open(3,file='upr3.dat',form='binary',action='write')
      tim= 0.
      nlev = 1
      nflag = 1
      do i = 1, num
        k = i
        staname = ''
        ! --- calculate us, vc from dire and spd	
        if( upr_obs(i)%spd(j) /= p2 .and. upr_obs(i)%dire(j) /= p2 ) then
          us = upr_obs(i)%spd(j)*cos(270.-upr_obs(i)%dire(j)) 
          vs = upr_obs(i)%spd(j)*sin(270.-upr_obs(i)%dire(j)) 
        else
          us = p2 ; vs =p2
        endif
        !	
        write(staname(1:5),'(i5.5)') upr_obs(i)%id
        write(3) staname(1:8), upr_obs(i)%lat, upr_obs(i)%lon, tim, nlev, nflag
        write(3) us, vs, upr_obs(i)%temp(j),  upr_obs(i)%td(j),  upr_obs(i)%hgt(j)
      enddo
      nlev = 0
      write(3) staname(1:8), upr_obs(k)%lat, upr_obs(k)%lon, tim, nlev, nflag
      close(3)
    enddo
    !
    ! --- write out upr?.ctl    
    do i = 1, 3
      if( i == 1 ) open(3,file='upr1.ctl',action='write')
      if( i == 2 ) open(3,file='upr2.ctl',action='write')
      if( i == 3 ) open(3,file='upr3.ctl',action='write')
      write(3,'(a,i1,a)') 'DSET ^upr',i,'.dat'
      write(3,'(a)') 'DTYPE station'
      write(3,'(a)') 'STNMAP ^sta.map'
      write(3,'(a)') 'UNDEF -999.0'
      write(3,'(a)') 'TITLE  Station Data Test'
      write(3,'(a)') 'TDEF   1 linear 12z18jan1992 12hr'
      write(3,'(a)') 'vars 5'
      write(3,'(a)') 'us    0  99  U'
      write(3,'(a)') 'vs    0  99  V'
      write(3,'(a)') 'ts    0  99  T'
      write(3,'(a)') 'td    0  99  Td'
      write(3,'(a)') 'hgt   0  99  Height'
      write(3,'(a)') 'endvars'
      close(3)
    enddo    
    !
    ! --- write out upr??.gs (scattered-plotting)
    outpnga1(1:14) = hh//'upra1.png'
    outpnga2(1:14) = hh//'upra2.png'
    outpnga3(1:14) = hh//'upra3.png'
    outpngb1(1:14) = hh//'uprb1.png'
    outpngb2(1:14) = hh//'uprb2.png'
    outpngb3(1:14) = hh//'uprb3.png'
    do i = 1, 3
      if( i == 1 ) open(5,file='upra1.gs',action='write')
      if( i == 2 ) open(5,file='upra2.gs',action='write')
      if( i == 3 ) open(5,file='upra3.gs',action='write')
      !
      write(5,'(a,i1,a)') "'open upr",i,".ctl'"
      write(5,'(a)') "'set font 2'"
      write(5,'(a)') "'set rgb 20 157 225 255'"
      write(5,'(a)') "'set rgb 21 196 196 255'"
      write(5,'(a)') "'set grads off'"
      write(5,'(a)') "'set mpdset mres cnriver'"
      write(5,'(a)') "'set map 4'"
      write(5,'(a)') "'set parea 0.11 10.98 0.3 8.48'"
      write(5,'(a)') "'set lon 78.0 135.0'"
      write(5,'(a)') "'set lat 15.0 52.0'"
      write(5,'(a)') "'set xlint 10'"
      write(5,'(a)') "'set ylint 10'"
      write(5,'(a)') "'set xlopts 1 4 0.08'"
      write(5,'(a)') "'set ylopts 1 4 0.08'"
      write(5,'(a)') "'set gxout model'"
      write(5,'(a)') "'set digsiz 0.06'"
      write(5,'(a)') "'d us;vs;ts;0.0;hgt'"
      write(5,'(a)') "'basemap O 20 4 M'"
      write(5,'(a)') "'set ccolor 1'"
      write(5,'(a)') "'d us;vs;ts;0.0;hgt'"
      write(5,'(a)') "'set line 21'"
      write(5,'(a)') "'draw recf 0.285 8.2 3.3 8.48'"
      write(5,'(a)') "'set line 1'"
      write(5,'(a)') "'draw rec 0.285 8.2 3.3 8.48'"
      write(5,'(a)') "'set string 1'"
      if( i == 1 ) write(5,'(a)') " 'draw string 0.4 8.30  850hPa "//date_time//":00LST'"
      if( i == 2 ) write(5,'(a)') " 'draw string 0.4 8.30  700hPa "//date_time//":00LST'"
      if( i == 3 ) write(5,'(a)') " 'draw string 0.4 8.30  500hPa "//date_time//":00LST'"
      write(5,'(a)') " 'set strsiz 0.085'"
      write(5,'(a)') " '!date +%H:%M > tmp_grads_gdate'"
      write(5,'(a)') " result = read (tmp_grads_gdate)"
      write(5,'(a)') " gdate = sublin(result,2)"
      write(5,'(a)') " '!rm -f tmp_grads_gdate'"
      write(5,'(a)') " 'draw string 0.40 8.10 Generated at 'gdate' LST'"
      write(5,'(a)') "'set string 1'"
      write(5,'(a)') " 'q gxinfo'"
      write(5,'(a)') " line3 = sublin(result,3)"
      write(5,'(a)') " line4 = sublin(result,4)"
      write(5,'(a)') " x1 = subwrd(line3,4)"
      write(5,'(a)') " x2 = subwrd(line3,6)"
      write(5,'(a)') " y1 = subwrd(line4,4)"
      write(5,'(a)') " y2 = subwrd(line4,6)"
      write(5,'(a)') " 'set strsiz 0.12'"
      write(5,'(a)') " 'set line 1 1 6'"
      write(5,'(a)') " 'draw rec 'x1' 'y1' 'x2' 'y2"
      write(5,'(a)') " 'set string 2'"
      if( stnum1 >= 1 ) then
        if( i == 1 ) write(5,'(a)') "'printim "//trim(outpnga1)//" x1010 y730 white'"
        if( i == 2 ) write(5,'(a)') "'printim "//trim(outpnga2)//" x1010 y730 white'"
        if( i == 3 ) write(5,'(a)') "'printim "//trim(outpnga3)//" x1010 y730 white'"
      endif
      write(5,'(a)') "quit"
      !
      close(5)
    enddo
    !
    do i = 1, 3
      if( i == 1 ) open(5,file='uprb1.gs',action='write')
      if( i == 2 ) open(5,file='uprb2.gs',action='write')
      if( i == 3 ) open(5,file='uprb3.gs',action='write')
      !
      write(5,'(a)') "'open gts.ctl'"
      write(5,'(a,i1,a)') "'open upr",i,".ctl'"
      write(5,'(a)') "'set font 2'"
      write(5,'(a)') "'set rgb 20 157 225 255'"
      write(5,'(a)') "'set rgb 21 196 196 255'"
      write(5,'(a)') "'set grads off'"
      write(5,'(a)') "'set mpdset mres cnriver'"
      write(5,'(a)') "'set map 4'"
      write(5,'(a)') "'set parea 0.11 10.98 0.3 8.48'"
      write(5,'(a)') "'set lon 78.0 135.0'"
      write(5,'(a)') "'set lat 15.0 52.0'"
      write(5,'(a)') "'set xlint 10'"
      write(5,'(a)') "'set ylint 10'"
      write(5,'(a)') "'set xlopts 1 4 0.08'"
      write(5,'(a)') "'set ylopts 1 4 0.08'"
      write(5,'(a)') "'define tc=oacres(gts,ts.2)'"
      write(5,'(a)') "'set gxout contour'"
      write(5,'(a)') "'set ccolor 2'"
      write(5,'(a)') "'set cstyle 1'"
      write(5,'(a)') "'set cint 3'"
      write(5,'(a)') "'set clopts 2 1 0.07'"
      write(5,'(a)') "'d tc'"
      write(5,'(a)') "'basemap O 20 15 M'"
      write(5,'(a)') "'set cint 3'"
      write(5,'(a)') "'set ccolor 2'"
      write(5,'(a)') "'set cstyle 1'"
      write(5,'(a)') "'set cthick 6'"
      write(5,'(a)') "'set clopts 2 1 0.07'"
      write(5,'(a)') "'d tc'"
      write(5,'(a)') "'define hg=oacres(gts,hgt.2)'"
      write(5,'(a)') "'set cint 4'"
      write(5,'(a)') "'set ccolor 4'"
      write(5,'(a)') "'set cstyle 1'"
      write(5,'(a)') "'set clopts 4 1 0.07'"
      write(5,'(a)') "'set cthick 6'"
      write(5,'(a)') "'d hg'"
      write(5,'(a)') "'set line 21'"
      write(5,'(a)') "'draw recf 0.285 8.2 3.30 8.48'"
      write(5,'(a)') "'set line 1'"
      write(5,'(a)') "'draw rec  0.285 8.2 3.30 8.48'"
      write(5,'(a)') "'set string 1 '"
      if( i == 1 ) write(5,'(a)') " 'draw string 0.4 8.3  850hPa "//date_time//":00LST'"
      if( i == 2 ) write(5,'(a)') " 'draw string 0.4 8.3  700hPa "//date_time//":00LST'"
      if( i == 3 ) write(5,'(a)') " 'draw string 0.4 8.3  500hPa "//date_time//":00LST'"
      write(5,'(a)') " 'set strsiz 0.085'"
      write(5,'(a)') " '!date +%H:%M > tmp_grads_gdate'"
      write(5,'(a)') " result = read (tmp_grads_gdate)"
      write(5,'(a)') " gdate = sublin(result,2)"
      write(5,'(a)') " '!rm -f tmp_grads_gdate'"
      write(5,'(a)') " 'draw string 0.40 8.10 Generated at 'gdate' LST'"
      write(5,'(a)') "'set string 1 '"
      write(5,'(a)') " 'q gxinfo'"
      write(5,'(a)') " line3 = sublin(result,3)"
      write(5,'(a)') " line4 = sublin(result,4)"
      write(5,'(a)') " x1 = subwrd(line3,4)"
      write(5,'(a)') " x2 = subwrd(line3,6)"
      write(5,'(a)') " y1 = subwrd(line4,4)"
      write(5,'(a)') " y2 = subwrd(line4,6)"
      write(5,'(a)') " 'set strsiz 0.12'"
      write(5,'(a)') " 'set line 1 1 1'"
      write(5,'(a)') " 'set line 1 1 6'"
      write(5,'(a)') " 'draw rec 'x1' 'y1' 'x2' 'y2"
      write(5,'(a)') " 'set string 2'"
      if( stnum1 >= 1 ) then
        if( i == 1 ) write(5,'(a)') "'printim "//trim(outpngb1)//" x1010 y730 white'"
        if( i == 2 ) write(5,'(a)') "'printim "//trim(outpngb2)//" x1010 y730 white'"
        if( i == 3 ) write(5,'(a)') "'printim "//trim(outpngb3)//" x1010 y730 white'"
      endif
      write(5,'(a)') "quit"
      !
      close(5)
    enddo
    !
    !
  END PROGRAM grads
