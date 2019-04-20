c $VERSION "08/16/95 @(#)cudpe.f	7.1"
       subroutine dpe(precip,ihr,tirrig,nlabcy,dt,ettot,qtot)
c precip :  precip(ihr)
c z1     :  -z(jz)
c z2     :  -z(jz+1)
c zmax   :  effective maximum height of droplets above sprinkler
c htdrop :  effective maximum height of droplets above ground
c aprate :  application rate (mm/sec)
c
      parameter(mh=98)
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),uprof(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
      common/spkler/ctwa,dtwa,etwa,ftwa,gtwa,cetsum,detsum,eetsum,fetsum
     &,getsum,hetsum,cqsum,dqsum,eqsum,fqsum,gqsum,cetfix,detfix,eetfix,
     &fetfix,cqfix,dqfix,eqfix,fqfix,zmax,htspk,abspk,bspk,spkprs
     &,bdyang,tdropi
c ******** calculate average layer conditions and tirrig **************
      htdrop=htspk+zmax
      aprate=precip/dt
      ettot=0.
      qtot=0.
      rhsum=0.
      tsum=0.
      zsum=0.
      do100jz=1,nlabcy
      rh=en(ihr,jz)/esat(jz)*100.
      if(-z(jz).gt.htdrop)z2=-z(jz)-z(jz+1)-htdrop
      if(-z(jz+1).gt.htdrop)z2=-z(jz)
      if(-z(jz).le.htdrop)z2=-z(jz+1)
      rhsum=rhsum+(-z(jz)-z2)*rh
      tsum=tsum+(-z(jz)-z2)*tn(ihr,jz)
 100  zsum=zsum+(-z(jz)-z2)
      rhsum=rhsum/zsum
      tsum=tsum/zsum
      tirrig=regf(tsum,rhsum,ctwa,dtwa,etwa,ftwa,gtwa,1.)
c *********** calculate layer summaries from input parameter : ********
       do500jz=1,nlabcy
       rh=en(ihr,jz)/esat(jz)*100.
       etsum=regf2(tn(ihr,jz),rh,cetsum,detsum,eetsum,fetsum,getsum
     & ,hetsum,aprate)
       qsum=regf(tn(ihr,jz),rh,cqsum,dqsum,eqsum,fqsum,gqsum,aprate)
       etfix=regf3(tn(ihr,jz),rh,cetfix,detfix,eetfix,fetfix,aprate)
       qfix=regf3(tn(ihr,jz),rh,cqfix,dqfix,eqfix,fqfix,aprate)
       etmax=2.*(etsum-etfix*(bspk+.5))/(abspk+2.)
       qmax=2.*(qsum-qfix*(bspk+.5))/(abspk+2.)
c
c ******** calculate layer sources *********************************
       et(jz)=0.
       q(jz)=0.
       z1=-z(jz)
       z2=-z(jz+1)
       if(z2.ge.htdrop)goto200
       zsub=z1+.1
       do300n=1,200
       zsub=zsub-.1
      if(zsub.lt.z2)goto200
      if(zsub.gt.htdrop)then
      etlay=0.
      qlay=0.
      endif
      if(zsub.ge.(htspk+.09).and.zsub.le.htdrop)then
      etlay=etmax*(1.-(zsub-htspk-.1)*10./abspk)
      qlay=qmax*(1.-(zsub-htspk-.1)*10./abspk)
c     write(15,*)zsub,etlay,qlay
      endif
      if(zsub.lt.(htspk+.09))then
      etlay=(etmax+etfix)/2.
      qlay=(qmax+qfix)/2.
c     write(15,*)zsub,etlay,qlay
      endif
      if(zsub.le.(htspk-.1))then
      etlay=etfix
      qlay=qfix
c     write(15,*)zsub,etlay,qlay
      endif
      et(jz)=et(jz)+etlay
      q(jz)=q(jz)+qlay
 300  continue
 200  continue
      ettot=ettot+et(jz)
      qtot=qtot+q(jz)
 500  continue
       return
       end
       function regf(t,rh,c,d,e,f,g,fac)
       regf=(c+d*t+(e+f*t+g*t*t)*rh)*fac
       return
       end
       function regf2(t,rh,c,d,e,f,g,h,fac)
       regf2=(c+d*t+e*t*t+(f+g*t+h*t*t)*rh)*fac
       return
       end
       function regf3(t,rh,c,d,e,f,fac)
       regf3=(c+d*t+(e+f*t)*rh)*fac
       return
       end
      subroutine drywt(tcpy,ihr)
c  subprogram to calc plant increment of dry matter for a given
c    photosynthetic rate considering maintenance resp and resp from
c    biosynthesis. for ritchie chapter 1988.
c
      parameter(mh=98)
      common/drywt1/fdwstm,fdwlf,fdwsh,fdwrot,fdwgr,dwtot,dwlfpa
      common/drywt2/cm25s,cm25l,cm25r,cm25g
      common/drywt3/rmstem,rmleaf,rmroot,rmgr,rmtot,rgtot,psmrm,dminc
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
c pid2,kmax in /misc1/ were added by Chen, 9/4/89.
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),uprof(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
c akroot(50) added to /root1/ by MCA - 6/12/95     
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1,rootup(50),cpytr,psisum,akroot(50)
      common /time/month,jday,iyear,icumdy,timloc(mh)
      common/photo5/cscpy,pscpyl,pscpyg,cicpy,rdcpy,qemax
c
c  ***maintenance*****
c  part    frac dry wt    main     maintenance coeff at 25c
c                         resp    g gluc/gdw/d  gco2/gdw/d  gc/gc/d
c  stem     fdwstm      rmstem       .015          .022      .0134
c  leaf     fdwlf       rmleaf       .030          .044      .0268
c  sheath   fdwsh       rmsh     main resp offset by photosynthesis
c  root     fdwrot      rmroot       .030          .044      .0268
c  grain    fdwgr       rmgr         .015          .022      .0134
c  main coeff from mcree 1988 crop sci. 28:111-113,28:114-120.
c  except root. higher root values from amthor(1984),plant,cell
c    environ. 7:561-569.  andre et al.(1978) physiol. plant. 44:197-
c    204. he had value of 25 ml/hr/plant in nut. sol. from 60-100
c    days after sowing and this converts to .082 mgco2/m2/s for 6
c    plants/m2. using .044 gco2/gdw/d with 90 gdw/m2 in roots gives
c    .092 mgc02/m2/s.
c
c  revised values of maintenance coeff after boote and loomis reviewed
c    boote manuscript.
c  part    frac dry wt    main     maintenance coeff at 25c
c                         resp    g gluc/gdw/d  gco2/gdw/d  gc/gc/d
c  stem     fdwstm      rmstem       .002          .0029     .0018
c  leaf     fdwlf       rmleaf       .020          .0294     .0178
c  sheath   fdwsh       rmsh     main resp offset by photosynthesis
c  root     fdwrot      rmroot       .007          .0103     .0062
c  grain    fdwgr       rmgr         .015          .022      .0134
c
c  dwtot    total plant dry wt per m2.
c  pltpm2   no. of plants per m2
c  dwlfpa   dry wt of leaf per leaf area(77 g/m2=7.7mg/cm2)
c
      fdwstm=.45
      fdwlf=.30
      fdwsh=.15
      fdwrot=.10
      fdwgr=.00
c  900 gdw/m2 is 150 gdw/plant just before tasseling
      dwtot=900.
      dwlfpa=77.
c main coeff for stem,leaf,root and grain at 25 c
      cm25s=.022
c     cm25s=.0029
      cm25l=.044 
c     cm25l=.0294
      cm25r=.044 
c     cm25r=.0103
      cm25g=.022
c seconds per day
      spday=24.*3600.
c  get mean root zone temp weighted by root length den profile which
c    is assumed similar to root dry wt profile.
      jzsfc1=jzsfc+1
      sumt=0.
      do50jz=jzsfc1,jzbot
 50   sumt=sumt+froot(jz)*tn(ihr,jz)
      trootm=sumt
c
c  calc temp adjustment factor for roots assuming q10=2.
      tfacr=exp(.0693*(trootm-25.))
c  get temp of plant tops
      ttop=tcpy
      tfact=exp(.0693*(ttop-25.))
c  calc main resp in micromol/m2 ground area/s
      rmstem=1000.*cm25s*tfact*fdwstm*dwtot/(spday*.044)
      rmleaf=1000.*cm25l*tfact*fdwlf *dwtot/(spday*.044)
      rmroot=1000.*cm25r*tfacr*fdwrot*dwtot/(spday*.044)
      rmgr  =1000.*cm25g*tfact*fdwgr *dwtot/(spday*.044)
c
      rmtot=rmstem+rmleaf+rmroot+rmgr
c  calc photosyn -main resp in units of micromol/m2/s. calc gross
c    photosyn by adding leaf dark resp. from sub photks (rdcpy)
c     psmrm= pscpyg +rdcpy - rmtot
c  alternatively if we want to use rdcpy instead of rmleaf then
c    just dont subtract rmleaf
      psmrm=pscpyg -rmtot + rmleaf
c******growth*****************************
c  get dry matter increment accounting for biosynthesis costs
c    vertregt and penning devries(1987),j.theor.biol.128:109-119
c    for corn 1.42 g glucose/gdw or 2.088 gco2/gdw of which .3 gco2/gdw
c    is given off as growth respiration.
c  dry matter increment in mg dw/m2/s
      dminc=psmrm*.044*.68/1.49
c  calc growth resp. if no loading costs then coeff is .301, if include
c    loading and unloading then coeff is .411; however if .411 is used
c    than coeff for dry mat inc is 1.49 instead of 1.42 above
      rgtot=.411*dminc/.044
c
      if(iwrite(7,19).eq.1)write(21,200)icumdy,ihr
 200  format(' 1719',i3,i2,4x,/14x,' rmstem rmleaf rmroot rmgr   rmtot '
     &,' rgtot  psmrm  dminc  trootm')
      if(iwrite(7,19).eq.1)write(21,210)icumdy,ihr,rmstem,rmleaf
     &,rmroot,rmgr,rmtot,rgtot,psmrm,dminc,trootm
 210  format(' 2719',i3,i2,4x,9f7.2)
      return
      end
