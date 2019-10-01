c $VERSION "08/16/95 @(#)cuintc.f	7.1"
      subroutine interc(ihr)
c  subroutine to calc precip or irrig intercepted by foliage
c    this intercepted rain may be much greater than that stored on
c    leaves at this point. after calling drip then pint(j) is that
c    stored on foliage
      parameter(mh=98)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/inter1/wtp(20),frwet(20),frwtmx,pint(20),pilast(20)
     1,pint1(20),twater
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
      do100j=1,jtot
      jj=jtot+1-j
      if(precip(ihr).lt.0.001.and.pilast(jj).le.0.001)go to 175
      frwet(jj)=frwtmx
c  pint1 is quantity of water in mm=kg/m2 intercepted on first
c    interaction of rain with a leaf.
      pint1(jj)=wtp(jj)*precip(ihr)
      pint(jj)=pint1(jj)+pilast(jj)
      sum3=sum3+pint(jj)
      go to 100
 175  pint(jj)=0.
      pint1(jj)=0.
      frwet(jj)=0.
 100  continue
      return
      end
      subroutine drips(ihr,lbredo,irrchk,tprecd)
      parameter(mh=98)
      dimension wtp0(20),irrchk(mh),tprecd(mh)
      common/leaf2/evap(10,20),gevap(10,20),heat(10,20),gheat(10,20)
     1,alam ,tlfavg(20),tgheat(20),tgvap1(20),tgvap2(20)
      common/water1/iprecp,tprecp,pn(mh,50),wcond(50),wstor(50),
     & wpond(mh)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/inter2/evint(20),evimm(20),pintmx,frstem,drip(20),stem
      common/inter1/wtp(20),frwet(20),frwtmx,pint(20),pilast(20)
     1,pint1(20),twater
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
c  subroutine to calculate amount of intercepted precip that runs
c    down stem,drips and stays on leaves.
c  calc weighting factors for each layer for precip intercpt from zenith
c  and call it wtp0. only used for drip within canopy.
      cl=df
      do695j=2,jtot
      jj=jtot+2-j
      wtp0(jj)=exp(-.5*(cl-df))-exp(-.5*cl)
      cl=cl+df
 695  continue
      sum=0.
      stem=0.
      lbredo=0
      do800j=2,jtot
      jj=jtot+2-j
      evimm(jj)=evint(jj)*dt/(alam*4.18e3)
      if(abs(evimm(jj)-pint(jj)).lt.0.001)go to 540
      if(evimm(jj)-pint(jj))600,500,500
 500  continue
      frwet(jj)=pint(jj)*frwtmx/evimm(jj)
      drip(jj)=0.
      pint(jj)=0.
      lbredo=1
      go to 800
 540  drip(jj)=0.
      frwet(jj)=0.
      pint(jj)=0.
      go to 800
c
 600  continue
c  mult pintmx times 2.*df because both sides of leaf are wetted.
      if(pint(jj)-evimm(jj)-pintmx*2.*df)520,650,650
 520  drip(jj)=0.
      pint(jj)=pint(jj)-evimm(jj)
      if(pint(jj).lt.0.)pint(jj)=0.
      go to 800
c
 650  continue
      drip(jj)=(1.-frstem)*(pint(jj)-evimm(jj)-pintmx*2.*df)
      stem=stem+frstem*(pint(jj)-evimm(jj)-pintmx*2.*df)
      pint(jj)=pintmx*2.*df
      jlay=jj-1
      cl=df
      do690j2=1,jlay
      jjj=jlay+1-j2
      if(jjj.gt.1)pint(jjj)=pint(jjj)+drip(jj)*wtp0(jtot+1-j2)
      if(jjj.eq.1)pint(1)=pint(1)+drip(jj)*(exp(-.5*(cl-df)))
      cl=cl+df
 690  continue
 800  continue
c if irrchk(ihr)=2 then irrigation with drop tubes below the cpy
      if(irrchk(ihr).eq.2)goto700
c  tprecp in kg m-2 s-1 = mm/s as b.c. for soil water infiltration
      tprecp=(pint(1)+stem)/dt
      goto710
c tprecd is same value as precip, but can't use precip because the
c canopy gets wetted. see near beginning of daily loop in main prog
 700  tprecp=tprecd(ihr)/dt
 710  continue
      if(precip(ihr).lt.0.0)tprecp=-precip(ihr)/dt
      if(ihr.eq.16)then
      endif
      return
      end
      subroutine preang(ihr,drpang)
c     calculate incident angle of drops on canopy from wind speed and
c     droplet terminal velocity
      parameter(mh=98)
      common /wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     &,zcrit
c     drop size in mm
      drpdia=1.
c     term velocity m sec-1  empirical fit from smithsonian met tables
c     page 396
      vterm=-0.334+(5.444+(-1.299+(0.168-0.00986*drpdia)*drpdia)*drpdia)
     &*drpdia
      drpang=atan(wind(ihr)/vterm)
      return
      end
      subroutine cpywt(anginc,weight)
c     subroutine to calculate weighting factor as fnc of depth in canopy
c     for precip
c     anginc is the angle from the vertical
      dimension weight(20)
      parameter(mh=98)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      cl=df
      sump=0.
      do695j=2,jtot
      jj=jtot+2-j
      weight(jj)=exp(-.5*(cl-df)/cos(anginc))-exp(-.5*cl/cos(anginc))
      sump=sump+weight(jj)
      cl=cl+df
695   continue
      weight(1)=1.-sump
      return
      end
