c $VERSION "08/16/95 @(#)cuet.f	7.1"
c	--------------------
c	| The penmon equation does NOT take into consideration
c	| slope and aspect. LMM 93/8/31
c
c	--------------------
c	| This routine does NOT currently take the dead and
c	| live vegetation ratios into account.  MCA 5/5/95
c
      subroutine penmon(ihr,icumdy,etpm)
c  subroutine to calc hourly et from penman-monteith eq
      parameter(mh=98)
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
      common/wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,
     &am,zcrit
      common/rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),
     &hfday,ratiod,ration,ratio(mh)
      common/rad4/d(3,20),u(3,20),bmflx(3,20),rnet(50),rndiv(50),tsfc
     &,rnlam(3,20)
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
c pid2,kmax in /misc1/ were added by Chen, 9/4/89.
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/misc6/xint(mh),nxintz,xintz(90),nalpha,ispher
      common/resis2/radn,anstom,rcut20,rsmin,trsopt,trsmin,trsmax,rsm
     &,rsexp,psi1,psidum
      common/cpy2/hsoil,hcpys,evtot,etotw(20),contot(20),scond(10,20)
     &,ihrwet(20),ecpys,cphstr,cpestr,wcpys,evsmic
c
 10   continue
c
c
c  soil heat flux?
      g=0.
      hbar=(h+refhtw)*.5
      ustr=.4*wind(ihr)/(alog((refhtw-disp)/z0))
      rmom=alog((refhtw-disp)/z0)/(.4*ustr)
c  correct for interfacial or excess resistance for heat according 
c    to Verma in Black(1989) or Brusaert book(1984) kB-1=ln(zo/zh)
c    =2.5 (brusaert p. 104)
      rb=6./ustr
      rheat=rmom + rb
 20   continue
      alam2=597.-.57*temair(ihr)
      esat=6.108*10**(7.5*temair(ihr)/(237.3+temair(ihr)))
      s=esat*alam2/(.1103*(temair(ihr)+273.)**2)
c  calc stom resistance
      if(radtop(1,ihr).lt.0.1)rstot=rcut20/totlai
      if(radtop(1,ihr).lt.0.1)go to 80
      c1=.07*fbeam1(1,ihr)*radtop(1,ihr)*(1.1-.1*totlai)*exp(-coszen
     &(ihr))
      sshad=(1.-fbeam1(1,ihr))*radtop(1,ihr)*exp(-.5*(totlai**.7))+c1
      ssun=sshad+fbeam1(1,ihr)*radtop(1,ihr)*xint(ihr)/coszen(ihr)
c     rssun=rsmin*(1.+(anstom-1.)*radn/ssun)
c  calc. stom resis for Konza grasses-scaling paper may 1991
c  non-rectangular hyperbola same as photosynthesis
      gmax=280.
      bg=.4
      zg=.4
      ww=bg*ssun*4.6+gmax
      gs=(ww-(ww**2-4.*zg*gmax*bg*ssun*4.6)**.5)/(2.*zg)
c  put resis in units of s m-1 from cond of mmol m-2 s-1
      rssun=42.*1000./gs
      rssun=rssun*rcut20/(rssun+rcut20)
c     rsshad=rsmin*(1.+(anstom-1.)*radn/sshad)
      ww=bg*sshad*4.6+gmax
      gs=(ww-(ww**2-4.*zg*gmax*bg*sshad*4.6)**.5)/(2.*zg)
      rsshad=42.*1000./gs
      rsshad=rsshad*rcut20/(rsshad+rcut20)
 30   continue
      sunlai=-xint(ihr)*totlai/coszen(ihr)
      if(sunlai.gt.-20)then
	  sunlai=(1.-exp(sunlai))*coszen(ihr)/xint(ihr)
      else
	  sunlai=1.e-10
      endif
      shalai=totlai-sunlai
      rstot=1./(sunlai/rssun+shalai/rsshad)
c     write(6,*)c1,sshad,ssun,rssun,rsshad,sunlai,shalai,rstot
c
c  cycle to here for diabatic corrections
c
c  canopies usually can hold 0.5 mm or so ,thus we can use this as
c    threshold for setting rstot to 0.
 80   if(precip(ihr).gt.0.5)rstot=0.
      ietpm2=0
      if(rstot.gt.9999.)rstot=9999.
 90   icycle=0
 100  continue
c  for amphi leaves constant in gamstr eq is 1.0, for hypo is 2.0
      gamstr=.66*1.0*(1.+rstot/rheat)
      num=1200.*(esat-vpair(ihr))/rheat
c  use net radiation divergence to get transpiration. if we do this
c    then do not subtract soil heat flux.
      rnetdv=rnet(jtot)-rnet(1)
c  calc rnetdv using exp(-.4 LAI) for net rad like done in sub scaleh
      rnetdv=rnet(jtot)*(1.-exp(-0.4*totlai))
      if(ietpm2.eq.1)etpm=(s*(rnet(jtot)-g)+num)/(s+gamstr)
      if(ietpm2.eq.1)etpm=(s*(rnetdv      )+num)/(s+gamstr)
      if(ietpm2.eq.0)etpm2=(s*(rnet(jtot)-hsoil)+num)/(s+gamstr)
      if(ietpm2.eq.0)etpm2=(s*(rnet(jtot)-rnet(1))+num)/(s+gamstr)
      if(ietpm2.eq.1)c=rnet(jtot)-g-etpm
      if(ietpm2.eq.1)c=rnetdv      -etpm
      if(ietpm2.eq.0)c=rnet(jtot)-hsoil-etpm2
      if(ietpm2.eq.0)c=rnet(jtot)-rnet(1)-etpm2
      if(ietpm2.eq.0)sens2=c
      if(icycle.ge.2)go to 500
      zdl2=(hbar-disp)/(-1200.*(temair(ihr)+273.)*ustr**3/(.4*9.8*c))
      if(zdl2.gt.1.)zdl2=1.
      if(zdl2.lt.-1.)zdl2=-1.
      if(zdl2.ge.0.)psi2=-5.*zdl2
      if(zdl2.lt.0.)psi2=1.88+(1./(-.533+.790*zdl2))
c  recalculate rmom
      ustr=.4*wind(ihr)/(alog((refhtw-disp)/z0-psi2))
      rmom=(alog((refhtw-disp)/z0)-psi2)/(.4*ustr)
      if(zdl2.ge.0.)rheat=rmom
      if(zdl2.lt.0.)rheat=rmom/(1.-16.*zdl2)**.25
      icycle=icycle+1
      go to 100
c
 500  tcrop=temair(ihr)+c*rheat/1200.
c  redo pen-mon eqs using cupid soil heat flux
      if(ietpm2.eq.1)go to 550
      ietpm2=1
      go to 90
 550  continue
c
c                                              more file write 7
c
      if(iwrite(7,15).eq.1)write(21,600)icumdy,ihr
 600  format(' 1715',i3,i2,4x,/,14x,' etpm2  rnetdv etpm   rstot  rheat
     & sens    ustr  tcrop  zdl2  ')
      if(iwrite(7,15).eq.1)write(21,700)icumdy,ihr,etpm2,rnetdv,etpm,
     &rstot,rheat,c,ustr,tcrop,zdl2
 700  format(' 2715',i3,i2,4x,9f7.2)
      return
c     debug trace
c     at 10
c     trace on
      end
      subroutine simpet(ihr,icumdy,rncpyd,taird,vpaird,winddy,solard
     &,etpmd,iday,tairmx,tairmn)
c  subroutine to calc daily et from simple equations
c
c  tdaym(i) is not WORKING *******************************************
c
      dimension tdaym(3)
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
c pid2,kmax in /misc1/ were added by Chen, 9/4/89.
c
c
c  penman equation
c
      alam=597.-.57*taird
      es=6.108*10**(7.5*taird/(237.3+taird))
      s=es*alam/(.110*(taird+273)**2)
c  convert wind to km/day
      u=winddy*24.*3600./1000.
      f=.27*(1.+.93*u/100.)
c  convert net rad to mm/day
      rn=rncpyd*24.*3600./(alam*4.18e3)
      sdspg=s/(s+.66)
      etpend=sdspg*rn+(1.-sdspg)*f*(es-vpaird)
c
c  priestly-taylor equation
c
      etptd=sdspg*1.3*rn
c
c  jensen-haise equation
c
c  convert solar rad to ly/day
      swdn=solard*100./4.18
      tdegf=taird*1.8+32.
      etjhd=(.014*tdegf-.37)*swdn*.01724
c
c  blainey-criddle equation
c
      etbcd=.33*(.46*taird+8)
c
c
c  nebraska modified penman pot et calculation.  see paul fishbachs
c    red notebook on irrigation scheduling sec. d page 10 for details.
c  iplnt is planting cumulative day number
      iplnt=136
      tmx=tairmx*1.8+32.
      tmn=tairmn*1.8+32.
      tday=(tmx+tmn)*.5
      if(iday.eq.1)g=0.
      if(iday.eq.2)g=5.*(tday-tdaym(3))
      if(iday.eq.3)g=5.*(tday-.5*(tdaym(2)+tdaym(3)))
      if(iday.ge.4)g=5.*(tday-(tdaym(1)+tdaym(2)+tdaym(3))/3.)
      if(iday.eq.1)tdaym(3)=tday
      if(iday.eq.2)tdaym(2)=tday
      if(iday.eq.3)tdaym(1)=tday
      if(iday.ge.4)tdaym(3)=tdaym(2)
      if(iday.ge.4)tdaym(2)=tdaym(1)
      if(iday.ge.4)tdaym(1)=tday
      vpmax=-.6959+.2946*tmx-.0052*tmx**2+89.e-6*tmx**3
      vpmin=-.6959+.2946*tmn-.0052*tmn**2+89.e-6*tmn**3
      vpsat=.5*(vpmax+vpmin)
      xlog=alog10(vpaird/6.108)
      tdew=237.3*xlog/(7.5+xlog)
      tdew=tdew*1.8+32.
      vpday=-.6959+.2946*tdew-.0052*tdew**2+89.e-6*tdew**3
      ta=tairmx+273.
      tb=tairmn+273.
      rbo=(.37-.044*sqrt(vpday))*11.71e-8*(ta**4+tb**4)/2.
c  day reference is march 1
      rso=760.*exp(-(((icumdy-59)-107.)/157.)**2)
      rn2=.77*(solard/.0418)-(.9*(solard/.0418)/rso+.1)*rbo
      c2=.959-.0125*tday+4.534e-5*tday**2
      c1=1.-c2
      w=winddy*3.28*3600.*24./5280.
      etpneb=6.73e-4*(c1*(rn2-g)+15.36*c2*(1.1+.017*w)*(vpsat-vpday))
c     write(6,*)tday,vpday,vpsat,rbo,rso,rn2,w,c1,c2
c  crop coefficient calculation
c    this coeff is referenced to irrigated alfalfa.
      if(icumdy.lt.(70.+iplnt))rb=(icumdy-iplnt)/70.
      if(icumdy.lt.(70.+iplnt))akcorn=-1.583*rb**3+2.756*rb**2
     &-.4276*rb+.213
      if(icumdy.ge.(70.+iplnt))ra=icumdy-(iplnt+70.)
      if(icumdy.ge.(70.+iplnt))akcorn=257e-8*ra**3-4688e-7*ra**2
     &+.01195*ra+.915
c  convert pot et to mm/day
      etpneb=akcorn*etpneb*25.4
c     --------
c     !  I zeroed etpneb and g because it was given Fermanich saw stars.
c     ! So did Murty.    LMM 94/4/8
      etpneb = 0.0
      g=0.0
c                                              more file write
c
      if(iwrite(2,9).eq.1)write(21,100)icumdy
 100  format(' 1209',i3,6x,/,14x,' etpend etptd  etjhd  etbcd  kmpday'
     &,' rnetmm lypday etpmd  etpneb')
      if(iwrite(2,9).eq.1)write(21,200)icumdy,etpend,etptd,etjhd,etbcd,
     &u,rn,swdn,etpmd,etpneb
 200  format(' 2209',i3,6x,9f7.2)
c  convert rn2 and g to mm/day from ly/day
      rn2=rn2/58.5
      g=g/58.5
      if(iwrite(2,12).eq.1)write(21,300)icumdy
 300  format(' 1212',i3,6x,/,14x,'  rn2     g2   crcoef')
      if(iwrite(2,12).eq.1)write(21,400)icumdy,rn2,g,akcorn
 400  format(' 2212',i3,6x,9f7.2)
      return
      end
