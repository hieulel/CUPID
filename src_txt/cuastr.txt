c $VERSION "@(#)cuastr.f	7.1 08/16/95"
      subroutine date
c************************date*******************************************
c if icumdy=0, then subroutine evaluates icumdy
c if month and jday are zero, subroutine evaluates these
c if all are non-zero sub uses icumdy and month and day are evaluated.
      parameter(mh=98)
      dimension noday(12)
      common/time/month,jday,iyear,icumdy,timloc(mh)
      noday( 1)=1
      noday( 2)=32
      noday( 3)=60
      noday( 4)=91
      noday( 5)=121
      noday( 6)=152
      noday( 7)=182
      noday( 8)=213
      noday( 9)=244
      noday(10)=274
      noday(11)=305
      noday(12)=335
      write(*,*) "date subroutine - cuastr.f"
      if(iyear)100,100,5
 5    if(ifix(2.5*(iyear-1900))-10*ifix((iyear-1900)/4.))10,10,18
 10   do15i=3,12
 15   noday(i)=noday(i)+1
 18   if(icumdy)100,20,60
 20   if(month)100, 60,25
 25   if(jday)100, 60,30
 30   if(month-12)35,35,100
 35   if(jday-31)40,40,100
 40   icumdy=noday(month)+jday-1
      go to 200
 60   if(icumdy-366)65,65,100
 100  write(6,110) month,jday,iyear,icumdy
 110  format(1x,4i6,'  trouble with date')
      stop
 65   m0=13
 70   m0=m0-1
      if(m0.eq.0) go to 100
      if(noday(m0)-icumdy)80,80,70
 80   month=m0
      jday=icumdy-noday(m0)+1
 200  continue
      return
      end
      subroutine declin
c***********************declin******************************************
c  calc declin. of sun in rad and eq. of time in fractions of hours
c  jan 1,1977 at 1 second after midnight is day 28124.0
      parameter(mh=98)
      common/astron/eqtm,decl,sindec,cosdec,decmax,sinlat,coslat,
     1tanlat,dlong
      common/time/month,jday,iyear,icumdy,timloc(mh)
	  write (*,*) "declin subroutine called - cuastr.f"
      pid180=3.1415926537/180.
      kday=(iyear-1977)*365+icumdy+28123
      xm=(-1.+.9856*kday)*pid180
      delnu=2.*.01674*sin(xm)+1.25*.01674*.01674*sin(2.*xm)
      slong=(-79.8280+.9856479*kday)*pid180+delnu
      decl=asin(decmax*sin(slong))
      sindec=sin(decl)
      cosdec=cos(decl)
      eqtm=9.4564*sin(2.*slong)/cosdec-4.*delnu/pid180
      eqtm=eqtm/60.
      return
      end
      subroutine zenith(timsun,ihrstr,ihrend,strt,end,nohrs,sunazm)
c************************zenith*****************************************
      parameter(mh=98)
      dimension hrang(mh)
     &,timsun(mh),sunazm(mh)
      common/astron/eqtm,decl,sindec,cosdec,decmax,sinlat,coslat,
     1tanlat,dlong
      common /rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),hfday
     &,ratiod,ration,ratio(mh)
      common/time/month,jday,iyear,icumdy,timloc(mh)
c   if local times are all zero then hour angles will be used from 1200
c   -hfday to 1200 + hfday. all other hours will have zenang=pi/2.
  5   format(1x,i3,1x,f7.4)
      pid2=3.1415926537/2.
      pi=3.1415926537
      write (*,*) "zenith subroutine called - cuastr.f"
      hfday=12./pi*acos(-(sinlat*sindec)/(coslat*cosdec))
      do10i=1,nohrs
      if(timloc(i))10,10,100
 10   continue
      do20i=1,nohrs
      hrang(i)=(i-12)*pid2/6.
 20   timloc(i)=i -eqtm-dlong
      go to 500
 100  do150i=1,nohrs
      timsun(i)=timloc(i)+eqtm+dlong
 150  hrang(i)=(timsun(i)-12.)*pid2/6
 500  ihfday=hfday
      strt=12.-hfday
      end=12.+hfday
      ihrstr=strt+1
      ihrend=end
      do600i=1,nohrs
      if(timsun(i)-strt)520,550,550
 520  zenang(i)=pid2
      go to 600
 550  if(timsun(i)-end)570,570,560
 560  zenang(i)=pid2
      go to 600
 570  continue
c     write(6,*)i,i
      zenang(i)=acos(sinlat*sindec+coslat*cosdec*cos(hrang(i)))
      sunazm(i)=asin(cosdec*sin(hrang(i))/sin(zenang(i)))
c      sunazm(i)=asin(cosdec*sin(hrang(i))/sin(zenang(i)))+pi
c pi in above stm was added by Chen, 10/31/89.
c  calc crit zenith angle when sun azimuth is 90 deg
      crtzen=acos(sindec/sinlat)
      if(zenang(i).gt.crtzen)sunazm(i)=(pi-abs(sunazm(i)))*sunazm(i)
     1/abs(sunazm(i))
      sunazm(i) = sunazm(i) + pi
 600  continue
c  check zenith angles near sunrise and sunset so not = 90 deg.
      if(pid2-zenang(ihrstr)-.001)650,650,700
 650  ihrstr=ihrstr+1
 700  if(pid2-zenang(ihrend)-.001)750,750,800
 750  ihrend=ihrend-1
 800  continue
      do900i=1,nohrs
 900  coszen(i)=cos(zenang(i))
      return
      end
