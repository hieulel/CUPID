c $VERSION "08/16/95 @(#)cuphot.f	7.1"
      subroutine photks(ihr,notemp,tfix,frdead)
c larry - i took dvpdsf out
      parameter (mh=98)
c  subprog to calc p.s. and stom cond with farquhar eqs for c3 or c4
c
c
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/resis2/radn,anstom,rcut20,rsmin,trsopt,trsmin,trsmax,rsm
     1,rsexp,psi1,psi2
      common/photo1/t0,xk1,xk2,xk3,xk4,xk5,xk6,ro,vm,xj2,delha
     &             ,delhl,delhh,thalfl,thalfh,cidca,gsin
      common/photo2/ox,cair,rastom,cucond,d1,bkv,facj,ic3c4,rxcham,rsfac
     &             ,znon,conmin
      common/photo3/csleaf(10,20),psleaf(10,20),cileaf(10,20),rgas
     &,rdrk(10,20),hsleaf(10,20),csfclf(10,20)
      common/photo4/cslay(20),rslay(20),cilay(20),fehist(20)
     &             ,rdlay(20),ecompl(20),pslay(20)
      common/photo5/cscpy,pscpyl,pscpyg,cicpy,rdcpy,qemax
      common /rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20)
     &            ,templf(10,20),tsoil(mh)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &            ,nlabcy,nlbcpy
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),uprof(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
c akroot(50) added to /root1/ by MCA - 6/12/95     
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1           ,rootup(50),cpytr,psisum,akroot(50)
c
c  add save command or answers are wrong on hp even though ok on IBM
      save
c     write(6,*) 'enter photks'
      vpdmax=.001
      vpdmin=1000.
c     write(1,*)psitop
c     do113j=2,jtot
c     write(1,5)en(ihr,nlabcy+jtot+1),rhleaf(j)
c 5   format(1x,5f7.2)
c     write(1,6)(dstrad(1,i,j),i=1,6)
c     write(1,6)(templf(i,j),i=1,6)
c     write(1,6)(frarea(i,j),i=1,6)
c 6   format(1x,6f7.2)
c113  continue
c
c  calc water pot effect on p.s.
      if(psitop.ge.psi1)pa=1.
      if(psitop.lt.psi1.and.psitop.ge.psi2)pa=(psitop-psi2)/(psi1-psi2)
      if(psitop.lt.psi2)pa=0.
c     write (16,*)'psitop=',psitop,'psi1=',psi1,'psi2=',psi2,'pa=',pa
c
      do 500 j=2,jtot
c  calc light history factor to reduce vm,jm and rd
      fe=exp(-.23*clai(j))
      rx=rhleaf(j)
c
c  calc vapor pres for each cpy layer, in mbar. (sun's note)  
      vp=en(ihr,jtot+nlabcy+1-j)  
c
      do490i=1,itotp1
c  convert incident rad in w/m2 to absorbed micro mol q/m2/s
      e=dstrad(1,i,j)*4.6*aleaf(1,j)
c calc vpd for each leaf class
      tl=templf(i,j)
      tlr=templf(i,j)
c
c  to remove temp effect on photosyn and stom notemp=1 else notemp=0
c    tfix is fixed leaf temp that is used
      if(notemp.eq.1)tl=tfix
c
      vsat=6.108*10**(7.5*tl/(237.3+tl))
c
      vpd=vsat-vp
      if(vpd.gt.vpdmax)vpdmax=vpd
      if(vpd.lt.vpdmin)vpdmin=vpd
c
      tlk=tl+273.
c
c  cmo is used to convert cond in mol/m2/s to m/s
      cmo=tlk/(273.*44.5)
      a1=(tl-t0)/((t0+273.)*rgas*tlk)
c carboxylation coeff
      xkc=xk1*exp(a1*xk2)
c oxygenation coeff
      xko=xk3*exp(a1*xk4)
c  dark respiration if notemp=1 then resp must use actual templf
      r3=ro*exp((tlr-t0)/((t0+273.)*rgas*(tlr+273.))*xk5)
c  rate of carboxylation
      v1=vm*exp(a1*xk6)
c temp dep of max rate of electron xfer
      term1=xj2*exp(delha*(1./(t0+273)-1./tlk)/rgas)
      term2=exp(delhl*(1./thalfl-1./tlk)/rgas)
      term3=exp(delhh*(1./thalfh-1./tlk)/rgas)
      xjm=term1/(1.+term2+term3)
c include water stress factor and reduced light
      xjmax=xjm*pa*(.6*fe+.4)
c
c  rectangular hyperbola
c     xj=xjmax*e/(e+facj*xjmax)
c  non-rect hyper from farquhar and wond(1984)
      ww=e/facj+xjmax
      xj=ww-sqrt(ww*ww-4.*znon*xjmax*e/facj)
      xj=xj/(2.*znon)
c
c co2 comp pt  gamma star if gsin from sub inplnt.lt.o then c3 else c4
c equation for gamma star from farquhar and von caemmerer, 1982. E.P.P.
c     if(gsin.lt.0)gs=.5*.21*ox*xkc/xko
c  equation from brooks and farquhar, 1985.planta 165:397-406 for c3
c  using this eq means that the factor .21 is changed.
      if(gsin.lt.0)gs=42.7+1.68*(tl-t0)+.0012*(tl-t0)**2
      if(gsin.ge.0)gs=gsin
c
c  calc knee on ps vs ci curve
      v2=xj/(4.5*v1)
      co=(xkk*v2-2.3333*gs)/(1.-v2)
c  internal co2 conc
      ci=cidca*cair
      if(ci.lt.0.)ci=0.
c  adjust for kok effect where rdn=rday/rnight=0.5
      rdn=1.0
      if(e.lt.20.)then
       rday=(rdn-1.)*r3*(e/20.)+r3
      else
       rday=rdn*r3
      endif
c  calc co2 comp pt    gamma
      xkk=xkc*(1.+ox/xko)
      if(gsin.lt.0.)g=(gs+xkk+rday/v1)/(1.-rday/v1)
      if(gsin.ge.0.)g=gs
c  photosynthesis without blr or vpd effects
      p1=v1*(ci-gs)/(ci+xkk) -rday
      pdum=(ci-gs)/(4.5*ci+10.5*gs)
      p2=xj*pdum -rday
      if(p1.le.p2)p=p1
      if(p1.gt.p2)p=p2
      if(p.lt.1.e-5)p=1.e-5
c  maximum quantum efficiency
      qemax=pdum/facj
c  stom cond to water-blr used here is value for chamber used to
c    meas p.s. rate- units mol/m2/s
      rstom=(cair-ci)/(1.6*p)-rxcham*cmo*rsfac*1.3/1.6
      conds=1./rstom
      if(conds.lt.0)then
        conds=cucond
      else
        conds=cucond+conds
      endif
c  adjust dark resp and carboxyl vel by light history
      vc=v1*(.6*fe+.4)
      rd=r3*(.6*fe+.4)
c  recalc day respiration
      if(e.lt.20.)then
       rday=(rdn-1.)*rd*(e/20.)+rd
      else
       rday=rdn * rd
      endif
c  calc co2 compensation point
      if(gsin.lt.0.)g=(gs+xkk+rday/vc)/(1.-rday/vc)
      if(gsin.ge.0.)g=gs
c  calc vpd effect on stomata
c     if(vpd.le.d1)goto 100
c     if(vpd.gt.d1.and.vpd.lt.d2)conds=(conds-conmin)*(d2-vpd)/(d2-d1)
c    &+conmin
c     if(vpd.gt.d2)conds=conmin
c conmin is above 2 stm replaced the original cucond. Chen, 10/8/89.
c100  continue
c
      conds0=conds
      rbl=rx*cmo*rsfac
      iter=1
c iteration for stomatal conductance begins at 200.
200   continue
c above 4 stm were added by Chen, 05/14/90.
c  solve for ci using conds above and p.s. parameters by setting p from
c    farquhar's eqs equal to p calcd from ci,ca and ks.
      rstom=1./conds
      clfwat=1./(rstom+rbl)
      clfco2=1./(rstom*1.6+1.3*rbl)
      a2=clfco2
      a3=a2*cair
      a4=a3+rday-a2*xkk-vc
      a5=(a3+rday)*xkk+vc*gs
      a6=sqrt(a4*a4+4.*a2*a5)
      a7=(-a4-a6)/(-2.*a2)
      ab=2.3333*gs
      ac=xj/4.5
      v3=ac/vc
      co=(xkk*v3-ab)/(1.-v3)
      a8=a3+rday-a2*ab-ac
      a9=(a3+rday)*ab+ac*gs
      aa=sqrt(a8*a8+4.*a2*a9)
      ad=(-a8-aa)/(-2.*a2)
      if(ad.gt.a7)then
        ci=ad
      else
        ci=a7
      endif
      p1=vc*(ci-gs)/(ci+xkk)-rday
      pdum=(ci-gs)/(4.5*ci+10.5*gs)
      p2=xj*pdum-rday
      if(p1.le.p2)p=p1
      if(p1.gt.p2)p=p2
c  calc light comp pt. this is approximate to extent that the transition
c    between day and night resp is at or below the light comp pt.
      xjcomp=rday/pdum
      ecomp=facj*xjcomp/(1.-xjcomp/xjmax)
c  actual quantum effic.
      qe=pdum/facj
c
      if(e.lt.20.) goto 300
      conds1=conds
      vpds=vpd*rstom/(rstom+rbl)
      if(dvpds.gt.d1) conds=conds0/(1.+bkv*(dvpds-d1))
      if(conds.lt.cucond) conds=cucond
	aux=vpd/vsat
c	write(6,*)'dvpd,dvpds,rbl,rstom=',aux,rbl,rstom
c      write(6,*)'iter,ci,conds1,conds=',iter,ci,conds1,conds
      if(abs(conds1-conds).lt.0.005) goto 300
      iter=iter+1
      conds=0.5*conds1+(1.-0.5)*conds
      goto 200
300   continue
c
c  output vbls from subr
c
c  csleaf is leaf cond with stom and b.l.
c  rsleaf is stomatal resistance without b.l.
c  rsnovp is stomatal resistance without vapor stress
c  rdrk is leaf dark respiration
c
c These parameters now depend on whether leaf is live or dead - all
c dead leaves are found in layers 2->jdead, with jdead+1 being 
c partially dead.  The stomatal resistance of dead leaves is assumed
c to be 10,000 - MCA 4/26/95
c
c     Dead leaves:
      if (j.le.jdead) then
        csleaf(i,j)=1./(10000.+rbl)
        rsleaf(i,j)=10000.
        rsnovp(i,j)=10000.
        psleaf(i,j)=0.
        cileaf(i,j)=cair
        rdrk(i,j)=0.
c     Partially dead layer - use weighted avg conductance:        
      else if (j.eq.jdead+1) then
        rsavg = 1./((1-frdead)/rstom + frdead/10000.) 
        csleaf(i,j)=1./(rsavg+rbl)
        rsleaf(i,j)=1./(frdead/10000. + cmo*(1-frdead)*conds)
        rsnovp(i,j)=1./(frdead/10000. + cmo*(1-frdead)*conds0)
        psleaf(i,j)=(1-frdead)*p
        cileaf(i,j)=(1-frdead)*ci + frdead*cair
        rdrk(i,j)=(1-frdead)*rd
c     Live leaves:        
      else
        csleaf(i,j)=clfwat
        rsleaf(i,j)=1./(cmo*conds)
        rsnovp(i,j)=1./(cmo*conds0)
        psleaf(i,j)=p
        cileaf(i,j)=ci
        rdrk(i,j)=rd
      endif
c     write(11,491)ihr,j,i,csleaf(i,j),rsleaf(i,j),psleaf(i,j)
c    &,cileaf(i,j),dstrad(1,i,j)
c491  format(1x,3i3,4(1x,f12.2),1x,f5.2)      
 490  continue
      fehist(j)=fe
      ecompl(j)=xjcomp
 500  continue
c
      return
      end
c
c--------------------------------------------------------------------------
c
      subroutine c4phot(ihr,notemp,tfix,frdead)
      parameter (mh=98)
c This is based on cuphtc4.f and linked with Larry machine.
c  cuphtc4.f is used for c4 plant such as corn and glass. the program is
c  to combine the part in the program of 'cuphota.f' with the
c subroutine 'cuphot.f' in the cupid6. The 'cuphota.f' use the new set
c of formula to calculate the photosynthesis rate instead of the the
c formula to calculate the rate in 'cuphot.f'. However, the parameters 
c of vmax and rkjc25 is adjusted to fit the averaged data of big bluce
c stem, indian glass and switch glass in 1987, so vmax=30.15 and rkjc25
c =0.208.
c In this program the Newton-raphson method is completely applied to
c all terms in ci equations to get new guess of ci. 
c  subprog to calc p.s. and stom cond with farquhar eqs for c3 or c4
c
c
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/resis2/radn,anstom,rcut20,rsmin,trsopt,trsmin,trsmax,rsm
     1,rsexp,psi1,psi2
c  change vm to vmm in following common to avoid the interaction with
c  the vm in this subroutine.
      common/photo1/t0,xk1,xk2,xk3,xk4,xk5,xk6,ro,vmm,xj2,delha
     &             ,delhl,delhh,thalfl,thalfh,cidca,gsin
      common/photo2/ox,cair,rastom,cucond,d1,bkv,facj,ic3c4,rxcham,rsfac
     &             ,znon,conmin
      common/photo3/csleaf(10,20),psleaf(10,20),cileaf(10,20),rgas
     &,rdrk(10,20),hsleaf(10,20),csfclf(10,20)
      common/photo4/cslay(20),rslay(20),cilay(20),fehist(20)
     &             ,rdlay(20),ecompl(20),pslay(20)
      common/photo5/cscpy,pscpyl,pscpyg,cicpy,rdcpy,qemax
      common /rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20)
     &            ,templf(10,20),tsoil(mh)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &            ,nlabcy,nlbcpy
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),uprof(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
c akroot(50) added to /root1/ by MCA - 6/12/95     
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1           ,rootup(50),cpytr,psisum,akroot(50)
      common /photc34/pair,rds,qtrd,vmax,qtvm,coef,
     &coef1,tb,rkjc25,qtpc,sms,bc4,alpha,c4tht,c4tht2,beta,beta2,
     &rcut,condc,tt
c
c*  all following parameters are used for c4 photosynthesis only.
c pair: atomospheric presure, rds and qtrd: two parameters in dark
c respiration eq., vmax and qtvm: two parameters in Rubisco rate e2.,
c krjc25 and qtpc: two parameters in rkjc eq.,
c rkjc: parameter in the eq. of
c co2 limited flux, wc., alpha: the rate coeeficient in light limited
c flux, bc4: proportional constant in eq of canopy resistance  
c theta (c4tht)  and beta: two adjustable parameters in the equation of finding
c minimum, ax, of vm and we, and minimum, px, of ax and wc..
c cio: initial guess of intercellular co2 concentration
c                 unit explanation:
c pair: mbar. ci: micro mol co2 /mol air. rds and vmax:
c micro mol co2/m2/s. 
c rkjc25:mol air/m2/s. b34; mol h2o/m2/s. alpha: micro mol co2 /micro
c mol photons. rcut: cuticular resistance (m2*s/mol). condc=1./rcut=bc4 
c
c* finish the parameters input between C* and c*
c set maximum iteration number for finding resistance 	
c     write (6,*) 'enter in c4phot subroutine'
      miterrs=50
      miterci=50
c        
      vpdmax=.001
      vpdmin=1000.
      rcut=1./bc4
c     starting the iterative calculation for each layer. 
      do 500 j=2,jtot
      cio=cair*cidca
      ci=cio             
      fe=exp(-.23*clai(j))
c     rx boundary layer resistance in s/m (sun's note)       
      rx=rhleaf(j)
c     write (6,*) 'rhleaf(j)',rhleaf(j),'rx',rx
c
c     starting calculation for different leaf angle classes
      do490i=1,itotp1
      iterci=0
c  convert incident rad in w/m2 to absorbed quantum flux density
c  in micro mol q/m2/s (sun's note)
      e=dstrad(1,i,j)*4.6*aleaf(1,j)
c calc vpd for each leaf class
      tl=templf(i,j)
      tlr=templf(i,j)
c
c  to remove temp effect on photosyn and stom notemp=1 else notemp=0
c    tfix is fixed leaf temp that is used
      if(notemp.eq.1)tl=tfix
c  saturated vapor presure in mbar. This is according to the original
c    ideas of Ball and Berry.
      vsat=6.108*10**(7.5*tl/(237.3+tl))
c  calculate sat. vp of leaf using air temp because using leaf temp
c    causes too strong a dependence of stom cond on rhsfc. When I used
c    this with kon153-8.in the effect was minimal on energy bal. and
c    temperatures.
c     tairlf=tn(ihr,jtot+nlabcy+1-j)
c     tairlf=tair(j)
c     vsat=6.108*10**(7.5*tairlf/(237.3+tairlf))
c  calc vapor pres for each cpy layer. wtvp is a weighting factor
c    that reduces the effect of dry air on stomatal closure.
c     wtvp=0.7
c     vp=wtvp*en(ihr,jtot+nlabcy+1-j) + (1.-wtvp)*vsat
c  set vp equal to value at the appropriate layer in cpy.
      vp=en(ihr,jtot+nlabcy+1-j)
c
      vpd=vsat-vp
c  vsatr and vpr are nondimensional unit.
      vsatr= vsat/pair
      vpr= vp/pair 
      vpdr=vpd/pair
      if(vpd.gt.vpdmax)vpdmax=vpd
      if(vpd.lt.vpdmin)vpdmin=vpd
c  tlk: absolute temperature (k). qt: dimensionless.
      tlk=tl+273.
      qt=(tl-25.0)/10.
      qtr=(tlr-25.0)/10.
c
c  cmo: convert cond in mol/m2/s to m/s or resistance in s/m to s*m2/mol
c  rbl: m2*s/mol.  rbl1: m2*s/mol.
      cmo=tlk/(273.*44.5)
      rbl=cmo*rx*rsfac
      condb=1./rbl
      rbl1=1.3*rbl
c  dark respiration,rd(micro mol co2/m2/s), if notemp=1 then resp must
c  use actual templf
      if (ic3c4.eq.1) then
c  for c4 corn  (note: is above criterion right for identifying corn or
c  grass?) ??????????
      rd=(0.6*fe+0.4)*rds*qtrd**qtr/(1.+exp(1.3*(tl-55.))) 
c above rd has been changed to new following equation: (Sun, 8/6,91)
      else
c  for c4 grass
      rd=(0.6*fe+0.4)*rds*qtrd**qtr 
      if(tlr.gt.45) rd=rd*exp(-0.0073*(tlr-45.)*(tlr-57.5)) 
      end if
c  calculation of third Rubisco limited flux, vm: micro co2/m2/s.
      vm=(0.6*fe+0.4)*vmax*qtvm**qt/(1.+exp(coef*(tl-tt))
     $+exp(-coef1*(tl-tb)))  	 
c  calc water pot effect on p.s.
      if(psitop.ge.psi1)pa=1.
      if(psitop.lt.psi1.and.psitop.ge.psi2)pa=(psitop-psi2)/(psi1-psi2)
      if(psitop.lt.psi2)pa=0.
c      pa1=1.
      pa1=pa
      if (pa.eq.0.0) pa1=0.000001
c  Photosynthetic and dark respiration rates are now attenuated for
c  water stress AFTER convergence.  I.e., find a solution assuming
c  no water stress, then attenuate photosyn and rdrk linearly for
c  psi2 < psi < psi1.  MCA 8/16/95
c      vm=vm*pa
c      rd=rd*pa
c     write (16,*)'psitop=',psitop,'psi1=',psi1,'psi2=',psi2,'pa=',pa
c
c     vm=vmax*qtvm**qt/(1.+exp(0.4*(tl-41.5)))
c calculation of co2 limited flux, wc: micro mol of co2/m2/s 
      rkjc=rkjc25*qtpc**qt
301     wc=rkjc*ci
c calculation of light limited flux, we: micro mol of co2/m2/s   
      we=alpha*e    
c
c  solve quadratic for minimum of vm and we, ax: micro mol of co2/m2/s.
      c1=vm+we
      c2=we*vm
      ax=(c1-sqrt(c1*c1-4.*c4tht*c2))/c4tht2 
c  solve quadratic for minimum of ax and wc, px: micro mol of co2/m2/s.
      c1=ax+wc
      c2=ax*wc
      px=(c1-sqrt(c1*c1-4.*beta*c2))/beta2
c  find photosynthesis rate pn: micro mol of co2/m2/s
      pn=px-rd
c     write (16,*) 'pn=',pn
c      write (16,20) rd,vm,rkjc,wc,we,ax,px,pn,e 
 20    format(1x,'rd=',f8.4,1x,'vm=',f8.4,1x,'rkjc=',f8.4,1x,
     $'wc=',f8.4,1x,'we=',f8.4,1x,'ax=',f8.4,1x,'px=',
     $f8.4,1x,'pn=',f8.4,1x,'e=',f11.4)
c  find the canopy resistance rstom (mol H2O /m2/s) by using iteration
c  method together with solving a derived quadratic equation.  
c  set initial iterrs=0 and rstom = 0
      iterrs=0
      rstom=0. 
c  finding the rstom at prefixed internal ci and leaf temperature tl by
c  using iteration method together with solving a quadratic equation.
c  ca and cs : concentration of co2 in air and just above leaf surface
c  (micro mol co2/ mol air) , ci: internal co2 concentration (same unit
c  as ca or cs)
 101  eleaf=vpdr/(rbl+rstom)
      eleaf1=0.5*eleaf
      cs=(cair/(rbl1-eleaf1)-pn)*(rbl1+eleaf1)
c     write(6,*) 'rbl',rbl,'rbl1',rbl1,'pn',pn,
c    &'eleaf1',eleaf1,'cair',cair
c  following formula is derived from equation (4) and (5) for rstom at
c  time step (n+1), based on cs calculated by using rstom in time step n.
c  (please refer to Sun's note). a1,a2,a3,aa,b1,c1 are intermediate
c  parameters.
      a1=1.-0.5*(vsatr+vpr) 
      a2=1.-vpr/vsatr
      a3=sms*pn
      aa=cs
      b1=(cs*condb-a3-bc4*cs)
c     if(iday.eq.8.and.ihr.ge.10) then 
c     write(6,*)'vsatr',vsatr,'vpr',vpr,'sms',sms,'pn',pn,'a3=',a3,
c    &'a1=',a1,'a2=',a2,'condb',condb,'cs',cs,'bc4',bc4
c     end if
      c1=a3*(condb-a1*a2*condb)+bc4*cs*condb
      conds=0.5*(-b1+sqrt(b1*b1+4.*aa*c1))/aa
c  The following line was altered by MCA because it introduced instability
c  into the convergence 7/12/95
c      if (conds.le.0.) conds=condc
      if (conds.lt.bc4)conds=bc4
c      write (11,10) aa,b1,c1,a1,a2,cs,condb,bc4,conds
c 10    format (1x,'aa=',e10.3,'b1=',e10.3,'c1=',e10.3,'a1=',e10.3,
c    $'a2=',e10.3,'cs=',e10.3,'condb=',e10.3,'bc4=',
c    $e10.3,'conds=',e10.3) 
      rsnew=1./conds
      rserr=abs(rsnew-rstom)
      rsdelt=rserr/(0.5*(abs(rsnew)+abs(rstom))) 
      if(rsdelt.le.0.001) goto 201
      rstom=rsnew
c     write (6,*) 'rs=',rstom
      iterrs=iterrs+1
c     write (6,*) 'iterrs',iterrs
      iterrs1=iterrs
      if (iterrs.gt.miterrs) go to 401
      go to 101
201   rhs=(1.-a1*a2*condb/(conds+condb))
      rsnew=1./conds 
      rstom=rsnew
      iterrs=0
      gh2ot=1./(rbl+rstom)
      rco2t=1.3*rbl+1.6*rstom
      gco2t=1./rco2t
      eleaf=vpdr*gh2ot
      eleaf1=0.5*eleaf
      cinew=((gco2t-eleaf1)*cair-pn)/(gco2t+eleaf1)
      cierr=abs(cinew-ci)  
      cidelt=cierr/(0.5*(abs(cinew)+abs(ci)))
      if (cidelt.le.0.001) go to 400
      iterci=iterci+1
c     write (6,*) 'iterci=',iterci
      if (iterci.gt.miterci) go to 501
      dpx=rkjc*(px-ax)/(2.*beta*px-ax-wc)
      gh2ot=1./(rbl+rstom)
      dci=-dpx/(gco2t+eleaf1)
c   Sun add following parts into dci to completely consider newton
c   raphson method. the parts added is between C*S to C*E
c*S
      if (conds.le.bc4) then       
      dgs=0.
      de=0.
      go to 305
      else
      end if
      condt=conds+condb
      condt1=1./(1.3*conds+1.6*condb)
      fact1=1./(gco2t+eleaf1)
      dcidgs=(cair*eleaf+pn)*1.6*condb*condb*fact1*fact1*condt1*condt1
      dcide = (pn-2.*cair*gco2t)*(vsatr-vpr)*condb*condb*0.5*fact1*fact1
     &/(condt*condt)
      cost=sms*rhs*pn/cs
      drhsdgs=condb/(condt*condt)*a1*a2                      
      co1=(1.3*rbl-eleaf1)     
      co2=(1.3*rbl+eleaf1)     
      dcsdci=-co2*dpx
      dedgs=vpdr*condb*condb/(condt*condt)
      dcsdgs=dedgs*(co2*0.5*cair/(co1*co1)+0.5*(cair/co1-pn))
      dgsdci=cost*(dpx/pn-dcsdci/cs)/(1.0-cost*(drhsdgs/rhs-dcsdgs/cs))
      dgs=dcidgs*dgsdci
      de=dcide*dgsdci
c*E   (Sun: following ci equation is changed by me)
c     ci=ci-(cinew-ci)/(dci-1.)
 305  ci=ci-(cinew-ci)/(dci+dgs+de-1.)
      go to 301
c
c output vbls from subr
c csleaf is leaf cond with stom and b.l. : mol H2O/m2/s
400   continue      
      vpds=vpd*rstom/(rstom+rbl)
      csleaf(i,j)=gh2ot
c
c Output variables from subroutine:
c
c  rsleaf is stomatal resistance without b.l.: (s/m) 
c  rsnovp is stomatal resistance without vapor stress
c  psleaf is photosynthesis rate micro mol co2/m2/s
c  cileaf is internal co2 concentration micro mol co2/mol 
c  leaf dark respiration: micro mol co2/m2/s
c  csleaf is leaf conductance with stom and b.l
c
c These parameters now depend on whether leaf is live or dead - all
c dead leaves are found in layers 2->jdead, with jdead+1 being 
c partially dead.  The stomatal resistance of dead leaves is assumed
c to be 10,000 - MCA 4/26/95
c
c Rdrk, psleaf, and csleaf are attenuated here by a factor pa1 to
c account for the effects of water stress.  pa1 varies linearly between
c small and 1 for psi2 < psi < psi1. MCA 8/16/95
c
c   Dead leaves:
      if (j.le.jdead) then
        rsleaf(i,j)=10000.
c       rsnovp(i,j)=10000.
c       if (pn.le.-6.) pn=-6.
        psleaf(i,j)=0.
c       if (psleaf(i,j).le.-6.) psleaf(i,j)=-6.
        cileaf(i,j)=cair
        rdrk(i,j)=0.
        csleaf(i,j)=1./(10000.+rbl)
c   Partially dead layer - used weighted avg conductance:
      else if (j.eq.jdead+1) then
        rsleaf(i,j)=1./(frdead/10000. + cmo*(1-frdead)*pa1*conds)
c       rsnovp(i,j)=1./(frdead/10000. + cmo*(1-frdead)*conds0)
c       if (pn.le.-6.) pn=-6.
        psleaf(i,j)=pa1*(1-frdead)*(pn)
c       if (psleaf(i,j).le.-6.) psleaf(i,j)=-6.
        cileaf(i,j)=(1-frdead)*ci + frdead*cair
        rdrk(i,j)=pa1*(1-frdead)*rd
        rsavg = 1./((1-frdead)/(rstom/pa1) + frdead/10000.) 
        csleaf(i,j)=1./(rsavg+rbl)
c   Live leaves:
      else
        rsleaf(i,j)=1./(pa1*cmo*conds)
c       rsnovp(i,j)=1./(cmo*conds0)
c       if (pn.le.-6.) pn=-6.
        psleaf(i,j)=pa1*pn
c       if (psleaf(i,j).le.-6.) psleaf(i,j)=-6.
        cileaf(i,j)=ci
        rdrk(i,j)=pa1*rd
c        csleaf(i,j)=gh2ot 
        csleaf(i,j)=1./(rbl+rstom/pa1)
      endif
c              
c     if(ihr.eq.13)then
c     write(6,491)ihr,j,i,csleaf(i,j),rsleaf(i,j),psleaf(i,j)
c    &,cileaf(i,j),dstrad(1,i,j),rhs*100.,cs,bc4,conds
c491  format(1x,3i3,4(1x,f12.2),1x,f5.2,2f7.2,f8.5)
c     endif
c  save rel hum and CO2 of leaf sfc (rhs) in indexed vbl.
      hsleaf(i,j)=rhs
      csfclf(i,j)=cs
 490  continue
      fehist(j)=fe
c     ecompl(j)=xjcomp
 500  continue
      
      go to 600
c
 401  write (6,*) 'stop  in c4phot iteration  for rs >',iterrs1
      stop
 501  write (6,*) 'stop  in c4phot iteration  for ci >',iterci
      stop
 600  return
      end
c
c----------------------------------------------------------------------
c
c ************************************************************************
c *This Subroutine is based on the cuphotc3.f and will be used to link with
c *cupid6 in murdorc machine.
c *To introduce the new photosynthesis rate formula of C-3 plant instead
c *of subroutine calculating the rate in the cupid6.
c also, Newton-raphson is applied to all terms in ci equation.  
c **********************************************************************
      subroutine c3phot (ihr,notemp,tfix,frdead)
      parameter (mh=98)
c  subprog to calc p.s. and stom cond with farquhar eqs for c3 or c4
c
c
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/resis2/radn,anstom,rcut20,rsmin,trsopt,trsmin,trsmax,rsm
     &,rsexp,psi1,psi2
      common/photo1/t0,xk1,xk2,xk3,xk4,xk5,xk6,ro,vmm,xj2,delha
     &             ,delhl,delhh,thalfl,thalfh,cidca,gsin
      common/photo2/ox,cair,rastom,cucond,d1,bkv,facj,ic3c4,rxcham,rsfac
     &             ,znon,conmin
      common/photo3/csleaf(10,20),psleaf(10,20),cileaf(10,20),rgas
     &,rdrk(10,20),hsleaf(10,20),csfclf(10,20)
      common/photo4/cslay(20),rslay(20),cilay(20),fehist(20)
     &             ,rdlay(20),ecompl(20),pslay(20)
      common/photo5/cscpy,pscpyl,pscpyg,cicpy,rdcpy,qemax
      common /rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20)
     &            ,templf(10,20),tsoil(mh)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &            ,nlabcy,nlbcpy
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),uprof(50),q(50),et(50),
     &en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
c akroot(50) added to /root1/ by MCA - 6/12/95     
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     &           ,rootup(50),cpytr,psisum,akroot(50)
      common /priout/rhs,vpd,cs,ci,pn,conds,eleaf,iterci
      common /photc34/pair,rds,qtrd,vmax,qtvm,coef,
     &coef1,tb,rkjc25,qtpc,sms,bc4,alpha,c4tht,c4tht2,beta,beta2,
     &rcut,condc,tt  
      common /photc3/vmvif,coefvm,tvm,paab,qtwz,coefwz,twz,sp25,qtsp,o2,
     &rkc25,qtkc,rko25,qtko,bc3,thtc3,thtc32 
c
c **  all following parameters are used for c3 photosynthesis only. **
c pair: atmospheric presure, (1013. mb)
c rds and qtrd: two parameters in dark respiration eq. rds:(u mol co2 m-2
c sec.-1).  qtrd: nodimensional.
c vmax and qtvm: two parameters in Rubisco rate e2.. Vmax: (u mol co2
c  m-2 s-1)i. qtvm: nodimensional.
c vmvif: factor of the effect from root water stress on Vmax
C Tvm: the reference temperature in vm formula. (degree C)
c paab: factor to correct the alpha in light limited flux
c formula.
c qtwz: the parameter at 25 degree c in wz formula.
c coefwz: the parameter in wz formula (1/(degree c))
c twz: the reference temperature in wz formula (degree c) 
c o2: in g formula (20000.0 pa)
c sp25: sp value in 25 degree c. (=2600.)
c rkc25: the parameter in 25 degree 25. in rk formula.
c qtkc:  the parameter in 25 degree 25 in rk formula.
c rko25: the parameter in 25 degree 25. in rk formula.(30000.pa)
c qtko:  the parameter in 25 degree 25 in rk formula.
c sms : propotional constant in Eq. of canopy resistance. (mol-H2o/mol-air)
c bc4 :                     detto                         (mol-H2o m-2 s-1)
c alpha: the rate coefficient in light limited flux, u mol co2/umol-photons
c thtc3 and beta: two adjustable parameters in the equation of finding
c minimum, ax, of vm and we, and minimum, px, of ax and wc.. both
c nondimensional
c cio: initial guess of intercellular co2 concentration
c rcut: cuticular resistance (m2*s/mol). condc=1./rcut 
c
c p finish the parameters input between C p and c p
c set maximum iteration number for finding resistance 	
c ************** program start    **********************
c     write (6,*) 'enter in c3phot subroutine'
      miterci=150
c        
      vpdmax=.001
      vpdmin=1000.
c     starting the iterative calculation for each layer. 
      vmax=vmax*vmvif
      rds=0.015*vmax
      do 500 j=2,jtot
      cio=cair*cidca
      ci=cio             
      fe=exp(-.23*clai(j))
c     rx boundary layer resistance in s/m (sun's note)       
      rx=rhleaf(j)
c  calc vapor pres for each cpy layer, in mbar. (sun's note)  
      vp=en(ihr,jtot+nlabcy+1-j)  
c
c     starting calculation for different leaf class
      do 490 i=1,itotp1
      iterci=0
c  convert incident rad in w/m2 to absorbed absorbed quatum flux density
c  in micro mol q/m2/s (sun's note)
      e = dstrad(1,i,j)*4.6*aleaf(1,j)
c calc vpd for each leaf class
      tl=templf(i,j)
      tlr=templf(i,j)
c
c  to remove temp effect on photosyn and stom notemp=1 else notemp=0
c    tfix is fixed leaf temp that is used
      if(notemp.eq.1)tl=tfix
c  saturated vapor presure in mbar. (sun's note)	
c  vsatr and vpr are nondimensional unit.
      vsat=6.108*10.**(7.5*tl/(237.3+tl))
c
c  calculate sat. vp of leaf using air temp because using leaf temp
c    causes too strong a dependence of stom cond on rhsfc. When I used
c    this with kon153-8.in the effect was minimal on energy bal. and
c    temperatures.
      tairlf=tn(ihr,jtot+nlabcy+1-j)
      tairlf=tair(j)
c     write(6,*)' ihr=',ihr,' jtot= ',jtot,' nlabcy= ',nlabcy,
c    &' j= ',j,' tn= ',tn(ihr,jtot+nlabcy+1-j)
      vsat=6.108*10**(7.5*tairlf/(237.3+tairlf))
c  calc vapor pres for each cpy layer. wtvp is a weighting factor
c    that reduces theeffect of dry air on stomatal closure.
c     wtvp=0.7
c     vp=wtvp*en(ihr,jtot+nlabcy+1-j) + (1.-wtvp)*vsat
c  set vp equal to value at the appropriate layer in cpy.
c
      vpd=vsat-vp
c     write(6,*)' vpd= ',vpd,'vp= ',vp,' tairlf= ',tairlf
      vsatr= vsat/pair
      vpr= vp/pair 
      vpdr=vpd/pair
      if(vpd.gt.vpdmax)vpdmax=vpd
      if(vpd.lt.vpdmin)vpdmin=vpd
c  tlk: absolute temperature (k). qt: dimensionless.
      tlk=tl+273.
      qt=(tl-25.0)/10.
      qtr =(tlr-25.0)/10.
c
c  cmo: convert cond in mol/m2/s to m/s or resistance in s/m to s*m2/mol
c  rbl: m2*s/mol.  rbl1: m2*s/mol.
      cmo=tlk/(273.*44.5)
      rbl=cmo*rx*rsfac
      condb=1./rbl
      rbl1=1.3*rbl
c  dark respiration,rd(micro mol co2/m2/s), if notemp=1 then resp must 
c  use actual templf
      rd=(0.9*fe+0.1)*rds*qtrd**qtr/(1.+exp(1.3*(tlr-55.))) 
c  calculation of third Rubisco limited flux, vm: micro co2/m2/s.
      vm=(0.9*fe+0.1)*vmax*qtvm**qt/(1.+exp(coefvm*(tl-tvm)))
c
c     --------------
c     | Added water stress to C3 from C4 LMM 10/10/94
c     ! calc water pot effect on p.s.
      if(psitop.ge.psi1)pa=1.
      if(psitop.lt.psi1.and.psitop.ge.psi2)pa=(psitop-psi2)/(psi1-psi2)
      if(psitop.lt.psi2)pa=0.
      pa1=pa
      if (pa.eq.0.0) pa1=0.000001
c
c  Photosynthetic and dark respiration rates are now attenuated for
c  water stress AFTER convergence.  I.e., find a solution assuming
c  no water stress, then attenuate photosyn and rdrk linearly for
c  psi2 < psi < psi1.  MCA 8/16/95
c
c      vm=vm*pa
c      rd=rd*pa
c
c sun change above vm formula to following vm in Feb,7 1992 for soybean.
c     vm=vmax*qtvm**qt/(1.+exp(coef*(tl-tt))+exp(coef1*(tl-tb)))
c     vm=(0.9*fe+0.1)*vm
      g=0.5*o2/(sp25*qtsp**qt)
      g=g/pair/100.*1000000.    
      rko=rko25*qtko**qt
      rkc=rkc25*qtkc**qt  
      rk=rkc*(1.+o2/rko)/pair/100.*1000000.   
      icount =1 
c start to find ci by iteration method
 301  wc=vm*(ci-g)/(ci+rk)
c     | next stmnt added 94/11/1 LMM JMN
      if (wc .lt. 0.0) wc = 0.0
c calculation of sink or co2 limited flux, wc: micro mol of co2/m2/s 
      wz=0.5*vmax*qtwz**qt/(1.+exp(coefwz*(twz-tl)))
c calculation of light limited flux, we: micro mol of co2/m2/s   
      alpha1=alpha*paab*(ci-g)/(ci+2.*g)
      we=alpha1*e    
c
c  solve quadratic for minimum of wc and we, ax: micro mol of co2/m2/s.
      c1=wc+we
      c2=we*wc
      ax=(c1-sqrt(c1*c1-4.*thtc3*c2))/thtc32 
c  solve quadratic for minimum of ax and wz, px: micro mol of co2/m2/s.
      c1=ax+wz
      c2=ax*wz
      px=(c1-sqrt(c1*c1-4.*beta*c2))/beta2
c  find photosynthesis rate pn: micro mol of co2/m2/s
      pn=px-rd
c     write (25,20) rd,vm,wc,we,ax,px,pn,e 
c20    format(1x,'rd=',f8.4,1x,'vm=',f8.4,1x,
c    &'wc=',f8.4,1x,'we=',f8.4,1x,'ax=',f8.4,1x,'px=',
c    &f8.4,1x,'pn=',f8.4,1x,'e=',f11.4)
c  find the canopy resistance rstom (mol H2O /m2/s) by solving
c  a quadratic equation ars*rsnew**2+brs*rsnew+crs=0 to get rsnew.
c  where
      cs=(cair-1.3*rbl*pn)
      a1=sms*pn/cs
      b1=eair(j)/rbl/vsat
      ars=a1*b1+bc3/rbl
      brs=a1+bc3-1./rbl
      crs=-1.0
      if (ars.eq.0.0) then
          if (brs .eq.0.0) then
              rsnew = 0.0
          else
              rsnew =crs/brs
          endif
      else
          rsnew=(-brs+sqrt(brs*brs-4.*ars*crs))/(2.*ars)
      endif
      conds=1./rsnew
      rstom=rsnew
      if (conds.le.bc3) conds=bc3
      rsnew=1./conds 
      rstom=rsnew
c finish to find ci by iteration way.
      gh2ot=1./(rbl+rstom)
      rco2t=1.3*rbl+1.6*rstom
      gco2t=1./rco2t
      eleaf=vpdr*gh2ot
      eleaf1=0.5*eleaf
      if (kp.eq.1) go to 401
      cinew=(gco2t*cair-pn)/gco2t
      cierr=abs(cinew-ci)  
      cidelt=cierr/(0.5*(abs(cinew)+abs(ci)))
      if (cidelt.le.0.001) go to 400
      iterci=iterci+1
      if (iterci.gt.miterci) go to 501
c  using newton-roughson way to find new guess of ci
      aa=2.0*thtc3*ax-(we+wc)
      dw=((ax-we)/aa)*vm*(rk+g)/((ci+rk)*(ci+rk)) 
      dq=e*((ax-wc)/aa)*alpha*paab*3.0*g/((ci+2.0*g)*(ci+2.0*g))
      da=dw+dq
      dfcdpn=-1./gco2t
      dpndci=(px-wz)/(2.*beta*px-ax-wz)*da
      dfcdrs=-1.6*pn  
      drsdpn=-sms*cair*rstom*(rstom*b1+1.)/(cs*cs*(ars*2.*rstom+brs))
      gh2ot=1./(rbl+rstom)
      dci1=dpndci*dfcdpn
      dci2=dfcdrs*drsdpn*dpndci
      if (conds.eq.bc3) dci2=0.0
      dci=dci1+dci2
      ci=ci-(cinew-ci)/(dci-1.)
      if (ci.le.1.0) ci=1.0
      go to 301
c
400   if (ci.le.g) then
      ci=g
      kp=1
      go to 301
      else
      end if
401   kp=0
      rhs= (1.+eair(j)/vsat*rstom/rbl)/(1.+rstom/rbl)      
      vpds=vpd*rstom/(rstom+rbl)
      dvpds=vpds/vsat
      csleaf(i,j)=gh2ot
c
c Output variables from subroutine:
c
c  rsleaf is stomatal resistance without b.l.: (s/m) 
c  rsnovp is stomatal resistance without vapor stress
c  psleaf is photosynthesis rate micro mol co2/m2/s
c  cileaf is internal co2 concentration micro mol co2/mol 
c  leaf dark respiration: micro mol co2/m2/s
c  csleaf is leaf conductance with stom and b.l
c
c These parameters now depend on whether leaf is live or dead - all
c dead leaves are found in layers 2->jdead, with jdead+1 being 
c partially dead.  The stomatal resistance of dead leaves is assumed
c to be 10,000 - MCA 4/26/95
c
c Rdrk, psleaf, and csleaf are attenuated here by a factor pa1 to
c account for the effects of water stress.  pa1 varies linearly between
c small and 1 for psi2 < psi < psi1.  MCA 8/16/95
c
c   Dead leaves:
      if (j.le.jdead) then
        rsleaf(i,j)=10000.
c       rsnovp(i,j)=10000.
        psleaf(i,j)=0.
        cileaf(i,j)=cair
        rdrk(i,j)=0.
        csleaf(i,j)=1./(10000.+rbl)
c   Partially dead layer - use weighted avg conductance:
      else if (j.eq.jdead+1) then
        rsleaf(i,j)=1./(frdead/10000. + cmo*(1-frdead)*pa1*conds)
c       rsnovp(i,j)=1./(frdead/10000. + cmo*(1-frdead)*conds0)
        psleaf(i,j)=pa1*(1-frdead)*pn
        cileaf(i,j)=(1-frdead)*ci + frdead*cair
        rdrk(i,j)=pa1*(1-frdead)*rd
        rsavg = 1./((1-frdead)/(rstom/pa1) + frdead/10000.) 
        csleaf(i,j)=1./(rsavg+rbl)
c   Live leaves:
      else
        rsleaf(i,j)=1./(pa1*cmo*conds)
c       rsnovp(i,j)=1./(cmo*conds0)
        psleaf(i,j)=pa1*pn
        cileaf(i,j)=ci
        rdrk(i,j)=pa1*rd
c        csleaf(i,j)=gh2ot 
        csleaf(i,j)=1./(rbl+rstom/pa1)
      endif
c                    
c  save rel hum and CO2 of leaf sfc (rhs) in indexed vbl.
      hsleaf(i,j)=rhs
      csfclf(i,j)=cs
 490  continue
      fehist(j)=fe
 500  continue
      go to 600
c
 501  write (6,*)  '**Iteration time in c3 photosyn.rate subprog. >',iterci
      stop
 600  return
      end
