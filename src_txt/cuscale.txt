c $VERSION "08/16/95 @(#)cuscale.f	7.1"
      subroutine scaleh(icumdy,ihr,apar,pari,pmax,znr,pce,pcea
     &,rdcpyd,frlive)
c  scale leaf to canopy using hourly quantities
c
c	----------------------------------
c	|  This routine does not yet take into account live and
c	|  dead vegetation fractions.  MCA 5/5/95
c
      parameter (mh=98)
      save
c
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
      common /rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),hfday
     &            ,ratiod,ration,ratio(mh)
      common /rad4/d(3,20),u(3,20),bmflx(3,20),rnet(50),rndiv(50),tsfc
     &            ,rnlam(3,20)
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &            ,clai(20),distls(10,mh),jdead
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/misc6/xint(mh),nxintz,xintz(90),nalpha,ispher
      common /wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     &             ,zcrit
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
      common/photo1/t0,xk1,xk2,xk3,xk4,xk5,xk6,ro,vm,xj2,delha
     &             ,delhl,delhh,thalfl,thalfh,cidca,gsin
      common/photo2/ox,cair,rastom,cucond,d1,d2,facj,ic3c4,rxcham,rsfac
     &             ,znon,conmin
c conmin in above stm was added, Chen, 10/8/89.
      common/photo3/csleaf(10,20),psleaf(10,20),cileaf(10,20),rgas
     &             ,rdrk(10,20)
      common/photo5/cscpy,pscpyl,pscpyg,cicpy,rdcpy,qemax
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
      common/scale1/ pss1a(mh),pss1ad,pss1b(mh),pss1bd,pss2a(mh),
     &pss2b(mh),pss2ad,pss2bd,pss3a(mh),pss3b(mh),pss3ad,pss3bd
     &,pss4(mh),pss4d,evlf(mh),evlfd,rnlfd,qlfd
c  if itest=1 then diagnostic write invoked
      itest=0
      if(itest.eq.1)write(22,*)' subr. scaleh ',' ihr= ',ihr
c
c  need to put in an if statement so photosyn. done only during day
c
c  define parameters of non-rectangular psyn. light curve
c  light curve to simulate big blue stem
      pmax=45.
c  non rectangular hyperbola coefficient - znr
      znr=.7
c  method 1-scale using max. leaf photochemical eff. and apar
c
c  pce=photo. chem. eff. for upper sunlit leaf = 0.062 for c4 grass at
c    light comp. pt. in mol co2 (mol q)-1.
      pce=0.062
c  pcea = ave. pce between light comp. pt. and full sun (.062+.0065)
c    calc. slope of ps vs light curve at full sun prependicular to
c    beam plus diffuse.   Get max. par.
      parmax=radtop(1,ihr)
      if(coszen(ihr).gt.0.01)then
        parmax=radtop(1,ihr)*(1.-fbeam1(1,ihr))+fbeam1(1,ihr)*radtop
     &  (1,ihr)/coszen(ihr)
c  try using max par as on hor above cpy
      parmax=radtop(1,ihr)
      endif
      parmax=parmax*4.6
      dpdi=sqrt((pce*parmax+pmax)**2-4.*znr*pmax*pce*parmax)
      dpdi=(2.*pce*(pce*parmax+pmax)-4.*znr*pmax*pce)/dpdi
      dpdi=(pce-dpdi/2.)/(2.*znr)
      pcea=(dpdi+pce)/2.
      if(itest.eq.1)write(22,*)' parmax=',parmax,' pce=',pce,
     &' pcea=',pcea,' dpdi=',dpdi
c  pss stands for photosynthesis scaled and pss1 is for method 1
c  calc. dark resp. of leaf using air temp. and resp. eq.
      rdlf=ro*exp((temair(ihr)-t0)*xk5/((t0+273.)*8.314*
     &(temair(ihr)+273.)))
c  use same dark resp for cpy as cupid used because we want to
c    compare photosynthesis; otherwise dark resp difference with
c    depth into cpy is a factor.  This only caused 15% error 
c    over day (.16 vs .14) so not a big factor
c     rdlf=rdcpy/totlai
c  apar unit is w m-2 so mult. by 4.6 to get umol m-2 s-1.
      pss1a(ihr)=pce*apar*4.6 - rdlf*totlai
      pss1b(ihr)=pcea*apar*4.6 - rdlf*totlai
c  sum over the whole day
      if(ihr.eq.1) then
        pss1ad=0.
        pss1bd=0.
        pss2ad=0.
        pss2bd=0.
        pss3ad=0.
        pss3bd=0.
        pss4d=0.
        evlfd=0.
        rnlfd=0.
        qlfd=0.
	rdcpyd=0.
      endif
      pss1ad=pss1ad + pss1a(ihr)*dt*1.e-6
      pss1bd=pss1bd + pss1b(ihr)*dt*1.e-6
      rdcpyd=rdcpyd + rdlf * dt * 1.e-6
c
c  method 2 - use average leaf inc. flux, mult by leaf absor., use
c             photosyn. from light curve and mult. by lai
c  ave. inc. flux density
      aveinc=(radtop(1,ihr)+d(1,1)+bmflx(1,1))*4.6/2.
      avabs1=aveinc*aleaf(1,jtot)/totlai
c  use geometric average light flux density
      avabs2=sqrt(radtop(1,ihr)*(d(1,1)+bmflx(1,1)))*4.6*
     &aleaf(1,jtot)
c     avabs2=avabs2/totlai
      if(itest.eq.1)write(22,*)' avabs1=',avabs1,' avabs2=',avabs2
c  using geometric mean gives much too low a values to be useful
c  calc. phtotsynthesis
      ww=pce*avabs1+pmax
      pss2a(ihr)=(ww - sqrt(ww**2-4.*znr*pmax*pce*avabs1))/(2.*znr)
      pss2a(ihr)=(pss2a(ihr)-rdlf)*totlai
c     ww=pce*avabs2+pmax
c     pss2b(ihr)=(ww - sqrt(ww**2-4.*znr*pmax*pce*avabs2))/(2.*znr)
c     pss2b(ihr)=(pss2b(ihr)-rdlf)*totlai
c
c  use analytical integral of rectangular hyperbola light curve over
c    lai.
      pmaxr=65.
      extcof=0.53
      fact=fbeam1(1,ihr)*extcof + (1.-fbeam1(1,ihr))
      if(radtop(1,ihr).gt.0.01) then
        term1=log((1./pmaxr)+1./(pce*aleaf(1,jtot)*
     &  radtop(1,ihr)*4.6))
        term2=log((1./pmaxr)+exp(extcof*totlai)/(pce*radtop(1,ihr)*
     &  4.6*aleaf(1,jtot)))
        pss2b(ihr)=fact  *(totlai*pmaxr+pmaxr*(term1-term2)/extcof)
        pss2b(ihr)=pss2b(ihr)-rdlf*totlai
      else
	pss2b(ihr)=-rdlf*totlai
      endif
c  sum over day
      pss2ad=pss2ad+pss2a(ihr)*dt*1.e-6
      pss2bd=pss2bd+pss2b(ihr)*dt*1.e-6
c
c  method 3 - consider canopy as unshaded horizontal leaves, use light-ps
c             curve with radiation on horizontal above cpy and assume
c             a large horizontal leaf.  Alternatively use geometric
c             mean flux density.
c  calc rad. abs. by hor. leaf
      radabs=radtop(1,ihr)*4.6*aleaf(1,jtot)
      ww=pce*radabs+pmax
      pss3lf=(ww - sqrt(ww**2-4.*znr*pmax*pce*radabs))/(2.*znr)-rdlf
c  use fraction of par intercepted to convert leaf area to ground area
c    Assume canopy same as large horizontal leaf
      if((pss3lf+rdlf).le.0.1) then
	pss3a(ihr)=-rdlf
      else
        pss3a(ihr)=(pss3lf+rdlf)*pari/radtop(1,ihr) - rdlf*totlai
        pss3a(ihr)=pss3lf
      endif
c  alternatively use geometric mean par and mult by lai
      ww=pce*avabs2+pmax
      pss3bl=(ww - sqrt(ww**2-4.*znr*pmax*pce*avabs2))/(2.*znr)-rdlf
      pss3b(ihr)=pss3bl*totlai
c  sum over hourly to get daily
      pss3ad=pss3ad+pss3a(ihr)*dt*1.e-6
      pss3bd=pss3bd+pss3b(ihr)*dt*1.e-6
c  
c  method 4 - stratify canopy into sunlit and shaded leaves and add
c             contributions separately.  This is for photosynthesis.
c             In sub. penmon this is done for stomatal conductance.
c
      pss4(ihr)=-rdlf*totlai
      if(radtop(1,ihr).lt.0.1)go to 80
      c1=.07*fbeam1(1,ihr)*radtop(1,ihr)*(1.1-.1*totlai)*exp(-coszen
     &(ihr))
      sshad=(1.-fbeam1(1,ihr))*radtop(1,ihr)*exp(-.5*(totlai**.7))+c1
      ssun=sshad+fbeam1(1,ihr)*radtop(1,ihr)*xint(ihr)/coszen(ihr)
      sshad=sshad*4.6*aleaf(1,jtot)
      ssun=ssun*4.6*aleaf(1,jtot)
      sunlai=-xint(ihr)*totlai/coszen(ihr)
      if(sunlai.gt.-20)then
        sunlai=(1.-exp(sunlai))*coszen(ihr)/xint(ihr)
      else
        sunlai=1.e-10
      endif
      shalai=totlai-sunlai
      ww=pce*ssun+pmax
      psun=(ww - sqrt(ww**2-4.*znr*pmax*pce*ssun))/(2.*znr)-rdlf
      ww=pce*sshad+pmax
      pshad=(ww - sqrt(ww**2-4.*znr*pmax*pce*sshad))/(2.*znr)-rdlf
      pss4(ihr)=sunlai*psun+shalai*pshad
 80   continue
c  sum over the day
      pss4d=pss4d+pss4(ihr)*dt*1.e-6
c
      if(itest.eq.1)write(22,*)' sshad=',sshad,' ssun=',ssun,
     &' sunlai=',sunlai,' shalai=',shalai,' pss4=',pss4(ihr)
c
c
c  method 5 - consider canopy to be made of many appropriate sized hor.
c             leaves all exposed to full sun.  perhaps use sunlit lai
c             to convert to ground area?  look at energy and water exchange.
c  use gaylon campbell log profile equation (ln((z+zo-d)/zo))
      disp2=disp+z0
      ustar=0.4*wind(ihr)/(log((refhtw+z0-disp2)/z0))
c  wind speed at top of cpy
      uh=ustar*log((h+z0-disp2)/z0)/0.4
c  calc. ve. at disp. height in cpy. au is coeff in vel. prof. in-cpy.
      au=2.
      udisp=uh*exp(au*(disp2/h-1))
c  use the same equation as in subr. profl2
      udisp=uh/(1.+am*(1.-disp2/h))**2
c  this eqn. for udisp also occurs later with diabatic corrections
      if(itest.eq.1) write(22,*)' wind= ',wind(ihr),
     &' ustar= ',ustar,' uh= ',uh,' udisp= ',udisp
c
c  solve leaf energy balance
c
c  net radiation of leaf at top of cpy assuming leaf at air temp.
      rnetab=aleaf(1,jtot)*radtop(1,ihr)+aleaf(2,jtot)*radtop(2,ihr)
     &+aleaf(3,jtot)*
     &radtop(3,ihr)-aleaf(3,jtot)*sigma*(temair(ihr)+273.)**4
c  
c  alternatively use rnlam(i,jtot) from cupid to get netradiation
c    and must divide by LAI to compare with rnetlf above.
c
      rntlf2=rnlam(1,jtot)+rnlam(2,jtot)+aleaf(3,jtot)*
     &radtop(3,ihr)-aleaf(3,jtot)*sigma*(temair(ihr)+273.)**4
c
c  net rad. on average leaf in cpy assuming net rad. ext. coeff is
c    0.4.  using lai/2 is the same as geometric mean
      rnetav=rntlf2*exp(-0.4*totlai/2.)
c  get net radiation that would be on a leaf at the disp. ht, thus
c   we need to find the lai at the disp. ht.
      jdisp=0
      do60jz=nlabcy+1,jzcpy
      j=nlabcy+1+jtot-jz
      if(-1.*zmid(jz).gt.disp2)go to 60
      jzdisp=jz
      jdisp=j
      go to 61
 60   continue
 61   continue
c  net radiation at disp ht
c	if (jdisp.le.0) then 
c	   write(*,*)'nlabcy = ',nlabcy,' jtot = ',jtot,
c     &	  		   ' jzsfc = ',jzsfc
c	   write(*,*)'jdisp = ',jdisp,' zmid(nlab+1) ',
c     &		   zmid(nlabcy+1),' zmid(jzsfc) ',zmid(jzsfc)
c	endif
	if (jdisp.le.0) then
c
c	    ----------
c	    | Fell through loop.  Layers probably = 1.  Set it
c	    | to "Top" layer.  LMM 94/4/14
	    jdisp = jtot
	endif
      rnetlf=rntlf2*exp(-.4*clai(jdisp))
c  net radiation on average leaf with net rad. below cpy subtracted
      rnetlf=(rntlf2-rntlf2*exp(-.4*totlai))/totlai
c
      if(itest.eq.1)write(22,*)' rnetlf= ',rnetlf,' radtop(3,ihr)= ',
     &radtop(3,ihr),' radtop(1,ihr)= ',radtop(1,ihr),' radtop(2,ihr)= ',
     &radtop(2,ihr),' aleaf(1,2,3) ',aleaf(1,jtot),aleaf(2,jtot),
     &aleaf(3,jtot),' rntlf2=',rntlf2,' clai(jdisp)=',clai(jdisp)
     &,' jzdisp= ',jzdisp,' jdisp= ',jdisp
c  define latent heat (xl=J mol-1), heat cap (cmol=J mol-1 C-1)
c    air den (rhomol=mol m-3), atmos. press (press=Pa)
      xl=44000.
      cmol=29.1
      rhomol=41.4
      press=1.e5
c
c  initialize iteration
c
      tlf=temair(ihr)
      tdisp=temair(ihr)
      cdisp=cair
      edisp=vpair(ihr)
      psim=0.
      psih=0.
c     cidca=0.3
      loop=0
 200  continue
      loop=loop+1
      if(itest.eq.1)write(22,*)' loop= ',loop
c  recalculate leaf resp using leaf temp.
c     pss3lf=pss3lf+rdlf
c     rdlf=ro*exp((tlf-t0)*xk5/((t0+273.)*8.314*
c    &(tlf+273.)))
c     pss3lf=pss3lf-rdlf
c bound. layer resis in m2 s mol-1 for both sides of leaf
      blrlf=.5*180.*8.314*(tdisp+273.)*sqrt(sizelf/udisp)/press
c  using pss3lf, solve for stom. resis. assuming ci=.3ca
      rslf2=(cdisp-cidca*cdisp)/(1.6*pss3lf) -1.3*(rxcham/42.)/1.6
      gslf2=1./rslf2
c
c  alternative approach to calculating stom resis of leaf
c  calc. stom resis for Konza grasses-scaling paper may 1991
c  non-rectangular hyperbola same as photosynthesis
      gmax=280.
      bg=.4
      zg=.4
c  get abs. par on leaf area basis
      aparlf=apar/totlai
      ww=bg*aparlf*4.6+gmax
      gslf=(ww-(ww**2-4.*zg*gmax*bg*aparlf*4.6)**.5)/(2.*zg)
      gslf=gslf/1000.
      if(gslf.lt.0.02)gslf=0.02
      rslf=1./gslf
      if(pss3lf.le.0.)then
        clf=cdisp-pss3lf*(1.6*rslf+1.3*blrlf)
      else
        clf=cidca*cdisp
      endif
      if(itest.eq.1)write(22,*)' gslf= ',gslf,' blrlf= ',blrlf
     &,' rslf2=',rslf2,' rslf=',rslf,' aparlf= ',aparlf
c
c  get conditions at leaf surface
      elf=6.108*10**(7.5*tlf/(237.3+tlf))
      esdisp=6.108*10**(7.5*tdisp/(237.3+tdisp))
      esfc=edisp+2.*blrlf/(2.*blrlf+rslf)*(elf-edisp)
      csfc=cdisp+1.3*2.*blrlf/(1.3*2.*blrlf+1.6*rslf)*(clf-cdisp)
      rhsfc=esfc/elf
c  solve for temp. diff. between leaf and air at disp. ht.
      tbar=(tlf+tdisp)*.5
      esat=6.108*10**(7.5*tbar/(237.3+tbar))
      stair=esat*585./(.1103*(tbar+273.)**2)
      xnum=rnetlf-xl*100.*(esdisp-edisp)/(press*(blrlf+rslf))
      den=xl*100.*stair/((blrlf+rslf)*press)+cmol/blrlf+4.*sigma*
     &aleaf(3,jtot)*(tbar+273.)**3
      deltlf=xnum/den
c  calc. fluxes
      qlf=cmol*deltlf/blrlf
      evlf(ihr)=100.*(esdisp-edisp+stair*deltlf)*xl/((blrlf+rslf)*press)
      rnlf=rnetlf-deltlf*4.*aleaf(3,jtot)*sigma*(tbar+273.)**3
c  To get canopy values of fluxes, mult. the fluxes from the 
c    average leaf by totlai. to get fluxes of average leaf adjust
c    fluxes on leaf at disp. ht. by ave. net rad.
c     adj=(rnetab-deltlf*4.*aleaf(3,jtot)*sigma*(tbar+273.)**3)/rnlf
      adj=totlai
      evlf(ihr)=evlf(ihr)*adj
      qlf=qlf*adj
      rnlf=rnlf*adj
c
c  alternatively use net radiation from cupid
c
      rnlf2=rntlf2-deltlf*4.*aleaf(3,jtot)*sigma*(tlf+273.)**3
c
      excess=rnlf-qlf-evlf(ihr)
      if(abs(excess).gt.5) write(6,*)' leaf bal. excess = ',excess
c
      if(itest.eq.1)write(22,*)' deltlf= ',deltlf,' rnlf= ',rnlf,
     &' qlf= ',qlf,' evlf= ',evlf(ihr),' pss3lf= ',pss3lf,' rnlf2='
     &,rnlf2,' adj= ',adj,' rnetab=',rnetab
c  solve for air temp. at top of canopy.
      xlog2=log((refhtw+z0-disp2)/z0) - psih
      xlog2=xlog2 - (log((h+z0-disp2)/z0) - psih)
      den =rhomol*cmol*0.40*ustar
      tdisp1=temair(ihr)+qlf*xlog2/den
      tlf=tdisp1+deltlf
c  solve for vapor pressure at displacement height
      edisp1=vpair(ihr)+8.314*((tdisp1+temair(ihr))*0.5+273.)*
     &(evlf(ihr)/xl)*xlog2/(0.4 * ustar*100.)
c  solve for co2 conc. at disp. ht.
      cdisp1=cair-8.314*((tdisp1+temair(ihr))*0.5+273.)*pss3lf*xlog2/
     &(0.4 * ustar*press)
      if(itest.eq.1)write(22,*)' xlog2= ',xlog2,' tdisp1= ',tdisp1,
     &' edisp1= ',edisp1,' cdisp1= ',cdisp1
      if(itest.eq.1)write(22,*)' xlog2= ',xlog2,' tdisp= ',tdisp,
     &' edisp= ',edisp,' cdisp= ',cdisp
c  diabatic corrections from Brusaert(1982)
      obukov=-ustar**3*1200.*(tbar+273.)/(0.4*9.8*qlf)
      zdl=((h+refhtw)*0.5+z0-disp2)/obukov
      zdlmx1=1.
      zdlmx2=1.
      loopm=100
      if(zdl.lt.0.) then
        if(zdl.lt.-zdlmx1.and.loop.lt.loopm) zdl=-zdlmx1
        if(zdl.lt.-zdlmx2.and.loop.ge.loopm) zdl=-zdlmx2
        xdiab=(1.-16.*zdl)**.25
        dum=log((1.+xdiab**2)/2.)
        psim=2.*log((1.+xdiab)/2.)+dum-2.*atan(xdiab)+3.141593/2.
        psih=2.*dum
      else
        if(zdl.ge.zdlmx1.and.loop.lt.loopm) zdl=zdlmx1
        if(zdl.ge.zdlmx2.and.loop.ge.loopm) zdl=zdlmx2
	if(zdl.ge.0.5.and.loop.lt.20)zdl=0.5
	if(zdl.ge.0.1.and.loop.ge.20)zdl=0.1
        psim=-5.*zdl
        psih=psim
      endif
c  do diabatic corections on velocity and set up iteration  must be
c    careful with uh and not let diabatic corrections change it 
c    excessively.
      ustar=0.4*wind(ihr)/(log((refhtw+z0-disp2)/z0)-psim)
      uh1=ustar*(log((h+z0-disp2)/z0)-psim)/0.4
      if(uh1.ge.wind(ihr).or.uh1.le.0.)uh1=0.3*wind(ihr)
      uh=(uh+uh1)*0.5
      udisp=uh*exp(au*(disp2/h - 1.))
c  use same in-canopy prof. for wind as subr. profl2
      udisp=uh/(1.+am*(1.-disp2/h))**2
      if(itest.eq.1)write(22,*)' ustar= ',ustar,' uh= ',uh,' udisp= ',
     &udisp,' psim= ',psim,' psih= ',psih,' zdl= ',zdl
c  check if iteration is necessary
      if(loop.ge.200)then
        write(6,*)' no conv. on scaling sub-loop= ',loop,' ihr= ',ihr
        write(6,*)tdisp,tdisp1,edisp,edisp1,cdisp,cdisp1
	go to 400 
      endif
      iredo=0
      fd=.1
      if(abs(tdisp1-tdisp).le.0.2) then
        tdisp=tdisp+fd *(tdisp1-tdisp)
        go to 280
      else
        tdisp=tdisp+ fd*(tdisp1-tdisp)
        iredo=1
      endif
 280  if(abs(edisp1-edisp).le.0.2) then
        edisp=edisp+fd *(edisp1-edisp)
        go to 290
      else
        edisp=edisp+ fd*(edisp1-edisp)
        iredo=1
      endif
 290  if(abs(cdisp1-cdisp).le.1.0) then
        cdisp=cdisp+ fd*(cdisp1-cdisp)
        go to 300
      else
        cdisp=cdisp+ fd*(cdisp1-cdisp)
        iredo=1
      endif
 300  continue
       if(iredo.eq.1)go to 200
       tdisp=tdisp1
       edisp=edisp1
       cdisp=cdisp1
 400  continue
c
      qloop=loop
      if(iwrite(7,23).eq.1)write(21,1000)icumdy,ihr
 1000 format(' 1723',i3,i2,4x,/14x,' pss1a  pss1b   pss2a  pss2b  evlf '
     1,'  qlf    rnlf  loop   rnlf2 ')
      if(iwrite(7,23).eq.1)write(21,1010)icumdy,ihr,pss1a(ihr),pss1b(ihr
     1),pss2a(ihr),pss2b(ihr),evlf(ihr),qlf,rnlf,qloop,rnlf2
 1010 format(' 2723',i3,i2,4x,9f7.2)
      if(iwrite(7,24).eq.1)write(21,1040)icumdy,ihr
 1040 format(' 1724',i3,i2,4x,/14x,' pss3a  rdlf    pss4   zdl    psih '
     &,'  pce    pcea   blrlf  rslf ')
      if(iwrite(7,24).eq.1)write(21,1050)icumdy,ihr,pss3a(ihr),rdlf
     1,pss4(ihr),zdl,psih,pce*100.,pcea*100.,blrlf,rslf
 1050 format(' 2724',i3,i2,4x,9f7.2)
      if(iwrite(7,25).eq.1)write(21,1020)icumdy,ihr
 1020 format(' 1725',i3,i2,4x,/14x,' temair  tdisp  tlf2  vpair  edisp '
     1,'  elf    cair   cdisp   clf  ')
      if(iwrite(7,25).eq.1)write(21,1030)icumdy,ihr,temair(ihr),tdisp,
     1tlf,vpair(ihr),edisp,elf,cair,cdisp,clf
 1030 format(' 2725',i3,i2,4x,9f7.2)
c
c  sum for daily values
      rnlfd=rnlfd+rnlf*dt*1.e-6
      evlfd=evlfd+evlf(ihr)*dt*1.e-6
      qlfd =qlfd +qlf*dt*1.e-6
      return
      end
c***********************************************************************
      subroutine scaled(icumdy,taird,rdcpyd,pmax,znr,pce,pcea,apard,pard
     &,parid,psyngd,rncpyd,hcpyd,ecpyd,hsoild,rnsold,hcpysd,wcpysd,
     &nohrs)
c  scaling from leaf to canopy using daily quantities
c
c	----------------------------------
c	|  This routine does not yet take into account live and
c	|  dead vegetation fractions.  MCA 5/5/95
c
      parameter (mh=98)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
      common /rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),hfday
     &            ,ratiod,ration,ratio(mh)
      common /rad4/d(3,20),u(3,20),bmflx(3,20),rnet(50),rndiv(50),tsfc
     &            ,rnlam(3,20)
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &            ,clai(20),distls(10,mh),jdead
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common /wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     &             ,zcrit
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
      common/photo1/t0,xk1,xk2,xk3,xk4,xk5,xk6,ro,vm,xj2,delha
     &             ,delhl,delhh,thalfl,thalfh,cidca,gsin
      common/photo2/ox,cair,rastom,cucond,d1,d2,facj,ic3c4,rxcham,rsfac
     &             ,znon,conmin
c conmin in above stm was added, Chen, 10/8/89.
      common/photo3/csleaf(10,20),psleaf(10,20),cileaf(10,20),rgas
     &             ,rdrk(10,20)
      common/photo5/cscpy,pscpyl,pscpyg,cicpy,rdcpy,qemax
      common/scale1/ pss1a(mh),pss1ad,pss1b(mh),pss1bd,pss2a(mh),
     &pss2b(mh),pss2ad,pss2bd,pss3a(mh),pss3b(mh),pss3ad,pss3bd
     &,pss4(mh),pss4d,evlf(mh),evlfd,rnlfd,qlfd
c
c  method 1 - scale using max leaf photochem. eff. and daily apar
c    evaluate eq. for dark resp. using daily mean temp. (mol m-2 day-1)
      rdday=ro*exp((taird-t0)*xk5/((t0+273.)*8.314*(taird+273.)))*
     &1.e-6*24.*3600.
c  compare rdday to sum of hourly canopy dark respiration values
      rdday2=rdcpyd
      pss1cd=pce*apard*4.6-rdday*totlai
c  pcea has last value from hourly calc,at hr=24 pcea=pce from low lite
      pss1dd=pcea*apard*4.6-rdday*totlai
c
c  method 2 - scale using ave. inc. leaf flux * leaf abs. * ps.vs.light
c             curve for leaf * lai
c
c
c  method 3 - consider cpy as made up of horizontal leaves all exposed
c             to full sun. use rad. inc. above cpy on horizontal and
c             absorptivity of leaf and mult. by frac. par intercepted.
c
c  calc. rad. abs. by hor. leaf (pard=mj m-2 d-1), 2*hfday is the
c    number of hours from sunrise to sunset.  put radiation into
c    umol q m-2 s-1.
c     rad1=pard*4.6*aleaf(1,jtot)*1.e6/(2.*hfday*3600.)
c     ww=pce*rad1+pmax
c     pss3ad=(ww-sqrt(ww*ww-4.*znr*pmax*pce*rad1))/(2.*znr)
c     pss3ad=pss3ad*3600.*2.*hfday/1.e6 - rdday
c     pss3ad=pss3ad*parid/pard
c  compare to daily sum of hourly quantities pss3ad from sub scaleh
c
c
c  method 4 - canopy stratified into sunlit and shaded leaves
c             and total if weighted sum of each
c  write output
      if(iwrite(2,15).eq.1)write(21,1000)icumdy
 1000 format(' 1215',i3,6x,/14x,'  rdday rdday2 pss1ad pss1bd pss1cd '
     1,'pss1dd pss2ad pss3ad pss3bd')
      if(iwrite(2,15).eq.1)write(21,1010)icumdy,rdday,rdday2,pss1ad,
     1pss1bd,pss1cd,pss1dd,pss2ad,pss3ad,pss3bd
 1010 format(' 2215',i3,6x,9f7.2)
      if(iwrite(2,16).eq.1)write(21,1020)icumdy
 1020 format(' 1216',i3,6x,/14x,' psyngd pss4d  pss2bd')
      if(iwrite(2,16).eq.1)write(21,1030)icumdy,psyngd/44.,pss4d,pss2bd
 1030 format(' 2216',i3,6x,9f7.2)
      if(iwrite(2,17).eq.1)write(21,1040)icumdy
 1040 format(' 1217',i3,6x,/14x,'  rnlfd  evlfd  qlfd ')
      if(iwrite(2,17).eq.1)write(21,1050)icumdy,rnlfd
     1,evlfd,qlfd
 1050 format(' 2217',i3,6x,9f7.2)
c
c  output daily values from cupid and convert from average w m-2 over
c    24 hours to mj m-2 day-1.
      fk=1.e-6*(nohrs*dt)
      if(iwrite(2,18).eq.1)write(21,1060)icumdy
 1060 format(' 1218',i3,6x,/14x,' rncpyd hcpyd  ecpyd  hsoild rnsold'
     1,' hcpysd wcpysd')
      if(iwrite(2,18).eq.1)write(21,1070)icumdy,rncpyd*fk,hcpyd*fk,
     1ecpyd*fk,hsoild*fk,rnsold*fk,hcpysd*fk,wcpysd*fk
 1070 format(' 2218',i3,6x,9f7.2)
      return
      end
