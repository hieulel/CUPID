c $VERSION "08/16/95 @(#)cunr.f	7.1"
      subroutine newrap(ijk,jbctop,jbcbot,vdeltz,vcond,vcap,vpot
     &,vsourc,vdeld,vpotck,ihr,ihrm1,iloop,vdt,icheck,iday)
c
      parameter(mh=98)
      implicit real*8 (a-h,o-z)
      real*4 vdeltz,vcond,vcap,vpot,vsourc,vdeld,vpotck,vdt
c
c   revised 02-23-87 from newrap fortran
      dimension cond(50),cap(50),pot(mh,50),source(50),potchk(50)
     &,icheck(50),b(50),c(50),d(50),ef(50),deltz(50)
     &,vdeltz(50),vcond(50),vcap(50),vpot(mh,50),vsourc(50)
     &,vpotck(50),flux(50),stor(50)
c
c     jbctop    subscript of upper b.c. of potential
c     jbcbot    subscript of lower b.c. of potential
c     deltz     delta-z array used in storage change term from last
c                 time step.
c     cond      conductance array (divided by delta z)
c                 heat-akh, water vapor-ake, liq soil water-akw
c     cond2     conductivity array not div by delta z
c                 cpy heat-akcpy, soil heat-aksoil
c                 cpy vapor-.64*akcpy, soil water-aksol
c     cap       capacitance array mult by delta mid-z/delta time
c                 cpy heat-cp  , soil heat-cp
c                 cpy vapor-ce , soil liq - cw
c     cap2      capacitance array not mult by delta mid z/dt
c                 cpy heat-cpcpy, soil heat-cpsoil
c                 cpy vapor-770 (new value 727=mw/(r*t)=1800/(8.314*298)
c                 soil liq-wt/(pn*bx)   derivative of moist rel curve
c     pot       potential (v.p. or temp.)
c     source    source array, source has - sign if it is being added
c               to the layer (et,q,...)
c     deld      2*e-5 typical convergence criteria for v.p.
c     potchk    array of limiting values of potential (sat. v.p. and
c               air entry pot for soil-pe)
c     iloop     no. of iterations in newton-raphson loop
c     ihr       hour number from hourly loop
c     ihrm1     ihr-1
c
c  set single precision vbls from calling seq to dbl prec in prog
      do4jz=jbctop,jbcbot
      deltz(jz)=0.0d0
      cond(jz)=0.0d0
      cap(jz)=0.0d0
      source(jz)=0.0d0
      potchk(jz)=0.0d0
      pot(ihr,jz)=0.0d0
4     pot(ihrm1,jz)=0.0d0
      dt=0.0d0
      deld=0.0d0
      do5jz=jbctop,jbcbot
      deltz(jz)=vdeltz(jz)
      cond(jz)=vcond(jz)
      cap(jz)=vcap(jz)
      source(jz)=vsourc(jz)
      potchk(jz)=vpotck(jz)
      pot(ihr,jz)=vpot(ihr,jz)
5     pot(ihrm1,jz)=vpot(ihrm1,jz)
      dt=vdt
      deld=vdeld
c
      istop=0
      iloop=0
      jstart=jbctop+1
      jend=jbcbot-1
      jendm1=jend-1
 10   sd=0.0d0
      do 100 j=jstart,jend
        c(j)=-cond(j)
        b(j)=cond(j)+cond(j-1)+cap(j)
      dum1=-cond(j-1)*pot(ihr,j-1)
      dum2=(cond(j-1)+cond(j))*pot(ihr,j)
      dum3=-cond(j)*pot(ihr,j+1)
      dum4=cap(j)*(pot(ihr,j)-pot(ihrm1,j))
c  if dum4 is estimated from pot(ihr-1)-pot(ihr-2) on first inter in
c    new-raph routine convergence will be at least 1 iter faster.
      d(j)=dum1+dum2+dum3+dum4-source(j)
        sd=sd+dabs(d(j))
c     if(ihr.ge.12)write(25,90)ihr,j,d(j),dum1,dum2,dum3,dum4,
c    &source(j),pot(ihr,j)
 90   format(i2,1x,i2,7(1x,e9.3))
 100  continue
c     if(ihr.eq.7) write(6,121)iloop,sd,pot(ihr,1)
 121  format(i3,e11.4,f6.2)
      if(sd.lt.deld) istop=1
      do 200 j=jstart,jendm1
        c(j)=c(j)/b(j)
        d(j)=d(j)/b(j)
        b(j+1)=b(j+1)+cond(j)*c(j)
 200    d(j+1)=d(j+1)+cond(j)*d(j)
      ef(jend)=d(jend)/b(jend)
      pot(ihr,jend)=pot(ihr,jend)-ef(jend)
      do 300 j=jstart,jendm1
        jx=jendm1+2-j
        ef(jx)=d(jx)-c(jx)*ef(jx+1)
 300    pot(ihr,jx)=pot(ihr,jx)-ef(jx)
      isum=0
      do 400 j=jbctop,jbcbot
        icheck(j)=0
c       if(pot(ihr,j).gt.potchk(j)) pot(ihr,j)=potchk(j)
        if(pot(ihr,j).gt.potchk(j)) icheck(j)=1
 400    isum=isum+icheck(j)
c     if(isum.gt.0) istop=0
c     write(25,450)iloop
 450  format(' iloop= ',i3)
      if(istop.eq.1) goto 600
      iloop=iloop+1
      if(iloop.lt.50) go to 10
      jsdmax=0
      sdmax=1.e-20
      do461j=jstart,jend
      if(d(j).gt.sdmax)jsdmax=j
      if(d(j).gt.sdmax)sdmax=d(j)
 461  continue
      write(6,500)ihr,iloop,pot(ihr,1),sdmax,jsdmax
 500  format('ihr=',2i3,' new-rap iter ',' pot bc=',f7.2,'  dmax=',e11.4
     &,'  j=',i3)
 600  continue
c  calc flux balance at nodes
c     do650jz=jbctop,jend
c     flux(jz)=cond(jz)*(pot(ihr,jz+1)-pot(ihr,jz))
c     stor(jz)=cap(jz)*(pot(ihr,jz)-pot(ihrm1,jz))
c650  continue
c     do660jz=jbctop,jend
c     bal=flux(jz-1)-flux(jz)+stor(jz)-source(jz)
c     if(ihr.eq.11)write(15,640)ijk,ihr,jz,bal,flux(jz),flux(jz+1)
c    &,stor(jz),source(jz)
c640  format(' newrap',i1,' ihr',2i3,' bal',e11.4,4e11.4)
c660  continue
c
c  set dbl precision vbls from prog to single prec in calling seq
      do700jz=jbctop,jbcbot
      vdeltz(jz)=deltz(jz)
      vcond(jz)=cond(jz)
      vcap(jz)=cap(jz)
      vsourc(jz)=source(jz)
      vpotck(jz)=potchk(jz)
      vpot(ihr,jz)=pot(ihr,jz)
 700  vpot(ihrm1,jz)=pot(ihrm1,jz)
      vdt=dt
      vdeld=deld
      return
      end
c------------------------------------------------------------------------------
c                                                                     NRAPT
      subroutine nrapt(ijk,jbctop,jbcbot,vdeltz,vcond,vcap,vpot
     &,vsourc,vdeld,vpotck,ihr,ihrm1,iloop,vdt,icheck,iday
     &,vtght,jtot,nlabcy,jzcpy,vwater,vtlfav)
c
      parameter(mh=98)
      implicit real*8 (a-h,o-z)
      real*4 vdeltz,vcond,vcap,vpot,vsourc,vdeld,vpotck,vdt,
     &vtght,vwater,vtlfav
c
c   revised 02-23-87 from newrap fortran
      dimension cond(50),cap(50),pot(mh,50),source(50),potchk(50)
     &,icheck(50),b(50),c(50),d(50),ef(50),deltz(50)
     &,vdeltz(50),vcond(50),vcap(50),vpot(mh,50),vsourc(50)
     &,vpotck(50),flux(50),stor(50)
     &,vtght(20),vwater(20),tgheat(50),waterg(50),vtlfav(20)
     &,tlfavg(50)
c
c     jbctop    subscript of upper b.c. of potential
c     jbcbot    subscript of lower b.c. of potential
c     deltz     delta-z array used in storage change term from last
c                 time step.
c     cond      conductance array (divided by delta z)
c                 heat-akh, water vapor-ake, liq soil water-akw
c     cond2     conductivity array not div by delta z
c                 cpy heat-akcpy, soil heat-aksoil
c                 cpy vapor-.64*akcpy, soil water-aksol
c     cap       capacitance array mult by delta mid-z/delta time
c                 cpy heat-cp  , soil heat-cp
c                 cpy vapor-ce , soil liq - cw
c     cap2      capacitance array not mult by delta mid z/dt
c                 cpy heat-cpcpy, soil heat-cpsoil
c                 cpy vapor-770 (new value 727=mw/(r*t)=1800/(8.314*298)
c                 soil liq-wt/(pn*bx)   derivative of moist rel curve
c     pot       potential (v.p. or temp.)
c     source    source array, source has - sign if it is being added
c               to the layer (et,q,...)
c     deld      2*e-5 typical convergence criteria for v.p.
c     potchk    array of limiting values of potential (sat. v.p. and
c               air entry pot for soil-pe)
c     iloop     no. of iterations in newton-raphson loop
c     ihr       hour number from hourly loop
c     ihrm1     ihr-1
c
c     vtght     j subscripted variable
c     tgheat    (jz)
c     vwater    j subscripted variable
c     waterg    (jz)
c     vtlfav    j subscripted variable
c     tlfavg    (jz)
c
c  set single precision vbls from calling seq to dbl prec in prog
      do4jz=jbctop,jbcbot
      deltz(jz)=0.0d0
      cond(jz)=0.0d0
      cap(jz)=0.0d0
      source(jz)=0.0d0
      potchk(jz)=0.0d0
      tgheat(jz)=0.0d0
      waterg(jz)=0.0d0
      pot(ihr,jz)=0.0d0
4     pot(ihrm1,jz)=0.0d0
      dt=0.0d0
      deld=0.0d0
      do5jz=jbctop,jbcbot
      deltz(jz)=vdeltz(jz)
      cond(jz)=vcond(jz)
      cap(jz)=vcap(jz)
      source(jz)=vsourc(jz)
      potchk(jz)=vpotck(jz)
c     -----------------
c     | Need to get vght and vwater from j subscripts
c     | to jz subscripts LMM JMN 94/11/8
      if (jzcpy-nlabcy.ne.(jtot-1)) then
	  write(*,*)'cunr.f:NRAPT:zcpy-nlabcy.ne.jtot'
	  stop
      endif
      if (jz.gt.nlabcy.and.jz.le.jzcpy) then
	  tgheat(jz)=vtght(jtot-(jz-(nlabcy+1)))
	  waterg(jz)=vwater(jtot-(jz-(nlabcy+1)))
	  tlfavg(jz)=vtlfav(jtot-(jz-(nlabcy+1)))
      endif
      pot(ihr,jz)=vpot(ihr,jz)
5     pot(ihrm1,jz)=vpot(ihrm1,jz)
      dt=vdt
      deld=vdeld
c
      istop=0
      iloop=0
      jstart=jbctop+1
      jend=jbcbot-1
      jendm1=jend-1
 10   sd=0.0d0
      do 100 j=jstart,jend
        c(j)=-cond(j)
        b(j)=cond(j)+cond(j-1)+cap(j)
c       -------
c       | b calculated differently in canopy LMM 94/11/8
	if (j.gt.nlabcy.and.j.le.jzcpy) then
	   b(j)=b(j)+tgheat(j)
	endif
      dum1=-cond(j-1)*pot(ihr,j-1)
      dum2=(cond(j-1)+cond(j))*pot(ihr,j)
      dum3=-cond(j)*pot(ihr,j+1)
      dum4=cap(j)*(pot(ihr,j)-pot(ihrm1,j))
c  if dum4 is estimated from pot(ihr-1)-pot(ihr-2) on first inter in
c    new-raph routine convergence will be at least 1 iter faster.
      d(j)=dum1+dum2+dum3+dum4-source(j)
        sd=sd+dabs(d(j))
c     if(ihr.ge.12)write(25,90)ihr,j,d(j),dum1,dum2,dum3,dum4,
c    &source(j),pot(ihr,j)
 90   format(i2,1x,i2,7(1x,e9.3))
 100  continue
c     if(ihr.eq.7) write(6,121)iloop,sd,pot(ihr,1)
 121  format(i3,e11.4,f6.2)
      if(sd.lt.deld) istop=1
      do 200 j=jstart,jendm1
        c(j)=c(j)/b(j)
        d(j)=d(j)/b(j)
        b(j+1)=b(j+1)+cond(j)*c(j)
 200    d(j+1)=d(j+1)+cond(j)*d(j)
      ef(jend)=d(jend)/b(jend)
      pot(ihr,jend)=pot(ihr,jend)-ef(jend)
      do 300 j=jstart,jendm1
        jx=jendm1+2-j
        ef(jx)=d(jx)-c(jx)*ef(jx+1)
 300    pot(ihr,jx)=pot(ihr,jx)-ef(jx)
      isum=0
      do 400 j=jbctop,jbcbot
        icheck(j)=0
c       if(pot(ihr,j).gt.potchk(j)) pot(ihr,j)=potchk(j)
        if(pot(ihr,j).gt.potchk(j)) icheck(j)=1
 400    isum=isum+icheck(j)
      do 410 j=nlabcy+1,jzcpy
	source(j)=tgheat(j)*(tlfavg(j)-pot(ihr,j))+waterg(j)
 410  continue
c     if(isum.gt.0) istop=0
c     write(25,450)iloop
 450  format(' iloop= ',i3)
      if(istop.eq.1) goto 600
      iloop=iloop+1
      if(iloop.lt.50) go to 10
      jsdmax=0
      sdmax=1.e-20
      do461j=jstart,jend
      if(d(j).gt.sdmax)jsdmax=j
      if(d(j).gt.sdmax)sdmax=d(j)
 461  continue
      write(6,500)ihr,iloop,pot(ihr,1),sdmax,jsdmax
 500  format('ihr=',2i3,' new-rap iter ',' pot bc=',f7.2,'  dmax=',e11.4
     &,'  j=',i3)
 600  continue
c  calc flux balance at nodes
c     do650jz=jbctop,jend
c     flux(jz)=cond(jz)*(pot(ihr,jz+1)-pot(ihr,jz))
c     stor(jz)=cap(jz)*(pot(ihr,jz)-pot(ihrm1,jz))
c650  continue
c     do660jz=jbctop,jend
c     bal=flux(jz-1)-flux(jz)+stor(jz)-source(jz)
c     if(ihr.eq.11)write(15,640)ijk,ihr,jz,bal,flux(jz),flux(jz+1)
c    &,stor(jz),source(jz)
c640  format(' newrap',i1,' ihr',2i3,' bal',e11.4,4e11.4)
c660  continue
c
c  set dbl precision vbls from prog to single prec in calling seq
      do700jz=jbctop,jbcbot
      vdeltz(jz)=deltz(jz)
      vcond(jz)=cond(jz)
      vcap(jz)=cap(jz)
      vsourc(jz)=source(jz)
      vpotck(jz)=potchk(jz)
      vpot(ihr,jz)=pot(ihr,jz)
 700  vpot(ihrm1,jz)=pot(ihrm1,jz)
      vdt=dt
      vdeld=deld
      return
      end
c---------------------------------------------------------------------------
c								    NRAPE
c
      subroutine nrape(ijk,jbctop,jbcbot,vdeltz,vcond,vcap,vpot
     &,vsourc,vdeld,vpotck,ihr,ihrm1,iloop,vdt,icheck,iday,
     &jtot,nlabcy,jzcpy,vestn,vgvap1,vgvap3)
c
      parameter(mh=98)
      implicit real*8 (a-h,o-z)
      real*4 vdeltz,vcond,vcap,vpot,vsourc,vdeld,vpotck,vdt
     &		,vgvap1,vestn,vgvap3
c
c   revised 02-23-87 from newrap fortran
      dimension cond(50),cap(50),pot(mh,50),source(50),potchk(50)
     &,icheck(50),b(50),c(50),d(50),ef(50),deltz(50)
     &,vdeltz(50),vcond(50),vcap(50),vpot(mh,50),vsourc(50)
     &,vpotck(50),flux(50),stor(50)
     &,vestn(50),vgvap1(20),vgvap3(50)
     &,estnew(50),tgvap1(50),tgvap3(50)
c
c     jbctop    subscript of upper b.c. of potential
c     jbcbot    subscript of lower b.c. of potential
c     deltz     delta-z array used in storage change term from last
c                 time step.
c     cond      conductance array (divided by delta z)
c                 heat-akh, water vapor-ake, liq soil water-akw
c     cond2     conductivity array not div by delta z
c                 cpy heat-akcpy, soil heat-aksoil
c                 cpy vapor-.64*akcpy, soil water-aksol
c     cap       capacitance array mult by delta mid-z/delta time
c                 cpy heat-cp  , soil heat-cp
c                 cpy vapor-ce , soil liq - cw
c     cap2      capacitance array not mult by delta mid z/dt
c                 cpy heat-cpcpy, soil heat-cpsoil
c                 cpy vapor-770 (new value 727=mw/(r*t)=1800/(8.314*298)
c                 soil liq-wt/(pn*bx)   derivative of moist rel curve
c     pot       potential (v.p. or temp.)
c     source    source array, source has - sign if it is being added
c               to the layer (et,q,...)
c     deld      2*e-5 typical convergence criteria for v.p.
c     potchk    array of limiting values of potential (sat. v.p. and
c               air entry pot for soil-pe)
c     iloop     no. of iterations in newton-raphson loop
c     ihr       hour number from hourly loop
c     ihrm1     ihr-1
c
c  set single precision vbls from calling seq to dbl prec in prog
      do4jz=jbctop,jbcbot
      deltz(jz)=0.0d0
      cond(jz)=0.0d0
      cap(jz)=0.0d0
      source(jz)=0.0d0
      potchk(jz)=0.0d0
      pot(ihr,jz)=0.0d0
4     pot(ihrm1,jz)=0.0d0
      dt=0.0d0
      deld=0.0d0
      do5jz=jbctop,jbcbot
	  deltz(jz)=vdeltz(jz)
	  cond(jz)=vcond(jz)
	  cap(jz)=vcap(jz)
	  source(jz)=vsourc(jz)
	  potchk(jz)=vpotck(jz)
	  pot(ihr,jz)=vpot(ihr,jz)
	  pot(ihrm1,jz)=vpot(ihrm1,jz)
	  if (jz.gt.nlabcy.and.jz.le.jzcpy) then
	      tgvap1(jz)=vgvap1(jtot-(jz-(nlabcy+1)))
	  endif
	  tgvap3(jz)=vgvap3(jz)
	  estnew(jz)=vestn(jz)
5     continue
      dt=vdt
      deld=vdeld
c
      istop=0
      iloop=0
      jstart=jbctop+1
      jend=jbcbot-1
      jendm1=jend-1
 10   sd=0.0d0
      do 100 j=jstart,jend
        c(j)=-cond(j)
        b(j)=cond(j)+cond(j-1)+cap(j)
c       -----------------
c       | Include the efect of the change in water flux
c	| per change in vapor pressure deficit. b(j) is the
c	| slope of the equation
	if (j.gt.nlabcy.and.j.le.jzcpy) then
	    b(j)=b(j)+tgvap1(j)
	endif
      dum1=-cond(j-1)*pot(ihr,j-1)
      dum2=(cond(j-1)+cond(j))*pot(ihr,j)
      dum3=-cond(j)*pot(ihr,j+1)
      dum4=cap(j)*(pot(ihr,j)-pot(ihrm1,j))
c  if dum4 is estimated from pot(ihr-1)-pot(ihr-2) on first inter in
c    new-raph routine convergence will be at least 1 iter faster.
      d(j)=dum1+dum2+dum3+dum4-source(j)
        sd=sd+dabs(d(j))
c     if(ihr.ge.12)write(25,90)ihr,j,d(j),dum1,dum2,dum3,dum4,
c    &source(j),pot(ihr,j)
 90   format(i2,1x,i2,7(1x,e9.3))
 100  continue
c     if(ihr.eq.7) write(6,121)iloop,sd,pot(ihr,1)
 121  format(i3,e11.4,f6.2)
      if(sd.lt.deld) istop=1
      do 200 j=jstart,jendm1
        c(j)=c(j)/b(j)
        d(j)=d(j)/b(j)
        b(j+1)=b(j+1)+cond(j)*c(j)
 200    d(j+1)=d(j+1)+cond(j)*d(j)
      ef(jend)=d(jend)/b(jend)
      pot(ihr,jend)=pot(ihr,jend)-ef(jend)
      do 300 j=jstart,jendm1
        jx=jendm1+2-j
        ef(jx)=d(jx)-c(jx)*ef(jx+1)
 300    pot(ihr,jx)=pot(ihr,jx)-ef(jx)
      isum=0
      do 400 j=jbctop,jbcbot
        icheck(j)=0
c       if(pot(ihr,j).gt.potchk(j)) pot(ihr,j)=potchk(j)
        if(pot(ihr,j).gt.potchk(j)) icheck(j)=1
 400    isum=isum+icheck(j)
      do 410,j=nlabcy+1,jzcpy
	  source(j)=tgvap1(j)*(estnew(j)-pot(ihr,j))+tgvap3(j)
 410  continue
c     if(isum.gt.0) istop=0
c     write(25,450)iloop
 450  format(' iloop= ',i3)
      if(istop.eq.1) goto 600
      iloop=iloop+1
      if(iloop.lt.50) go to 10
      jsdmax=0
      sdmax=1.e-20
      do461j=jstart,jend
      if(d(j).gt.sdmax)jsdmax=j
      if(d(j).gt.sdmax)sdmax=d(j)
 461  continue
      write(6,500)ihr,iloop,pot(ihr,1),sdmax,jsdmax
 500  format('ihr=',2i3,' new-rap iter ',' pot bc=',f7.2,'  dmax=',e11.4
     &,'  j=',i3)
 600  continue
c  calc flux balance at nodes
c     do650jz=jbctop,jend
c     flux(jz)=cond(jz)*(pot(ihr,jz+1)-pot(ihr,jz))
c     stor(jz)=cap(jz)*(pot(ihr,jz)-pot(ihrm1,jz))
c650  continue
c     do660jz=jbctop,jend
c     bal=flux(jz-1)-flux(jz)+stor(jz)-source(jz)
c     if(ihr.eq.11)write(15,640)ijk,ihr,jz,bal,flux(jz),flux(jz+1)
c    &,stor(jz),source(jz)
c640  format(' newrap',i1,' ihr',2i3,' bal',e11.4,4e11.4)
c660  continue
c
c  set dbl precision vbls from prog to single prec in calling seq
      do700jz=jbctop,jbcbot
      vdeltz(jz)=deltz(jz)
      vcond(jz)=cond(jz)
      vcap(jz)=cap(jz)
      vsourc(jz)=source(jz)
      vpotck(jz)=potchk(jz)
      vpot(ihr,jz)=pot(ihr,jz)
 700  vpot(ihrm1,jz)=pot(ihrm1,jz)
      vdt=dt
      vdeld=deld
      return
      end
