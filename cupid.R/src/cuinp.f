c $VERSION "08/16/95 @(#)cuinp.f	7.1"
c--------------------------------------------------------------- INFIX
      subroutine infix(xlat,xlong,stdlng,ihrpnt,nodays,ispecl,nn1pnt
     &,kpnt,lpnt,ispn,mday,slope,aspect,hrzang)
      parameter(mh=98,daymax=367)
      dimension ipnt(99),jpnt(99),kpnt(99),lpnt(99),ihrpnt(mh),ispn(mh)
     &,mmday(100),mday(100)
      common/astron/eqtm,decl,sindec,cosdec,decmax,sinlat,coslat,
     1tanlat,dlong
      common /misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
c pid2,kmax in above statement were added by Chen, 9/4/89.
c  zero iwrite option so default is printing nothing to disk
      do5l=1,9
      do5m=1,99
 5    iwrite(l,m)=0
c
c
c          input and calculated fixed data
c
c
cread....read....read....read....read....read....read....read....read
c  iwrite=1 means output for l(col.3),m(col.4&5) in out code
      read(20,*)nowrit,(ipnt(l),jpnt(l),l=1,nowrit)
      do10l=1,nowrit
 10   iwrite(ipnt(l),jpnt(l))=1
c  special hours for longer profile outputs read in here if desired
      read(20,*)ispecl
      do17i=1,mh
 17   ihrpnt(i)=0
      read(20,*)nn1pnt,(kpnt(l),lpnt(l),l=1,nn1pnt)
      read(20,*)nn2pnt,(ispn(l),l=1,nn2pnt)
      if(ispecl.eq.0)go to 25
      do22l=1,nn2pnt
      do20i=1,mh
      if(i.eq.ispn(l))then
      ihrpnt(i)=1
      goto 22
      endif
 20   continue
 22   continue
c  read in days when profile output wanted, if mdayin=1 then profiles
c    done for all days. iday is vbl used here
 25   read(20,*)mdayin,(mmday(ll),ll=1,mdayin)
      do23iday=1,100
      if(mdayin.eq.1)then
        mday(iday)=1
      else
        mday(iday)=0
      endif
 23   continue
      do31ll=1,mdayin
      do30iday=1,100
      if(iday.eq.mmday(ll)) mday(iday)=1
 30   continue
 31   continue
 75   continue
      read(20,*) xlat,xlong,stdlng
      dlong=(stdlng-xlong)/15.
      sinlat=sin(xlat*pid180)
      coslat=cos(xlat*pid180)
      tanlat=sinlat/coslat
c
c	----------------
c	| Add slope aspect and slope of the 
c	| horizon.  These values are entered
c	| as degrees but internally we use
c	| radians.  LMM 30/9/93
c
	read(20,*)slope,aspect,hrzang
	slope = slope*pid180
	aspect = aspect*pid180
	hrzang = hrzang*pid180
	return
	end
c
c--------------------------------------------------------------- INPLNT
      subroutine inplnt(ibidir,ipp,nvzenp,vwzenp,itassl,jmax,jmin,dfmin,
     &jmaxp1,plspc,ilaiup,zdhcr,theta,pltpm2,clump,notemp,tfix,factir
     &,amfull,iunif,z0dh,dispdh)
c ipp,nvzenp,vwzenp in above stm were added by Chen, 06/02/90.
	  integer daymax
	  parameter(mh=98,daymax=367)

      dimension ldayin(30),xtlai(30),xht(30),xarot(30),xzldh(30)
     &,xzmdh(30),tsolbc(30),wsolbc(30),ndayin(daymax),theta(10)
     &,vwzenp(10), xfrlv(30)
c vwzenp(10) in above stm was added by Chen, 06/03/90.
c
c*****  cupradazm insert begin 1: dimension and common *****************
c  Chen, 8/30/89.
      dimension sa(10),ca(10),gr(10)
      common/parabola/avisr,bvisr,cvisr,anirr,bnirr,cnirr
     &               ,avist,bvist,cvist,anirt,bnirt,cnirt
      common/rad6/rlfdif(3,20),tlfdif(3,20),rlfdir(3,20,mh),
     &tlfdir(3,20,mh)
      common/rad8/rlfhem(3,9,20),tlfhem(3,9,20)
      common/indax2/xmeu,xneu,gamrat,frdeg(91),iangot(9),kpntsp
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
      common/misc6/xint(mh),nxintz,xintz(90),nalpha,ispher
c xintz, extin. coef. at different zenith averaged over azimuth, and 
c nxtintz, dimension of xintz, Chen, 05/25/89.
      common/deg/sunazm(mh),nozenv,viewzn(10),noazmv,viewaz(50)
     &,xintv(10,50)
c above stm was added by Chen, 9/18/89.
c*****  cupradazm insert end   1: dimension and common *****************
c
      common/daily/tlai(daymax),ht(daymax),arotin(daymax)
     &,zldh1(daymax),zmdh1(daymax),tsbc(daymax),wsbc(daymax)
     &,frlive(daymax)
c     --------
c     | added frlive LMM 94/9/6
      common/misc5/deltz(50),zbcpy(10),zbc(10),zabc(20)
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
c     ---------
c     | added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
      common /misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt,clai(20)
     &,distls(10,mh),jdead
c pid2, kmax in /misc1/ were added by Chen, 8/30/89.
c distls(10,mh) in /misc2/ was added by Chen, 8/30/89.
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common /wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     &,zcrit
      common/resis2/radn,anstom,rcut20,rsmin,trsopt,trsmin,trsmax,rsm
     1,rsexp,psi1,psi2
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
      common/soil1/zsoil(25),wti(50),ndsoil,aksoil(50),cpsoil(50),tsn(50
     1)
c akroot(50) added to /root1/ by MCA - 6/12/95     
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1,rootup(50),cpytr,psisum,akroot(50)
      common/inter1/wtp(20),frwet(20),frwtmx,pint(20),pilast(20)
     1,pint1(20),twater
      common/inter2/evint(20),evimm(20),pintmx,frstem,drip(20),stem
      common/photo1/t0,xk1,xk2,xk3,xk4,xk5,xk6,ro,vm,xj2,delha
     &             ,delhl,delhh,thalfl,thalfh,cidca,gsin
      common/photo2/ox,cair,rastom,cucond,d1,bkv,facj,ic3c4,rxcham,rsfac
     &             ,znon,conmin
c conmin is above stm was added, Chen, 10/8/89.
c---- DrSun/lMurdock new common for photc4 and photc3file
c
      common /photc34/pair,rds,qtrd,vmax,qtvm,coef,
     &coef1,tb,rkjc25,qtpc,sms,bc4,alpha,c4tht,c4tht2,beta,beta2,
     &rcut,condc,tt
      common /photc3/vmvif,coefvm,tvm,paab,qtwz,coefwz,twz,sp25,qtsp,o2,
     &rkc25,qtkc,rko25,qtko,bc3,thtc3,thtc32 
c
c     common/dpevp1/dpdmin,nsmpl,dianoz,spkprs,ispkty,inoint
c     common/dpevp2/dtint,dtnew,iprtin,dtheta,htspk,tdropi
c     common/dpevp3/evapsm(50),aprtot(50),irrchk(50,mh),iprock,twb,irrga
c     common/dpevp4/etchk2(5),qchk2(5),tirrig,drpang,drpdeg,spkev,totevp
c  ibidir is flag for bidir refl., itassl is flag for tassels in top lay
      read(20,*)ibidir,itassl
      read(20,*) ipp
      read(20,*) nvzenp
      read(20,*) (vwzenp(i),i=1,nvzenp)
c above 3 stm were added by Chen, 06/03/90.
      read(20,*)nozenv,(viewzn(i),i=1,nozenv)
      read(20,*)noazmv,(viewaz(i),i=1,noazmv)
c above two stms were added by Chen, 9/18/89.
c   jmax is max. no. of canopy layers of leaves
c  clump is nilson clumping factor- about .85 for corn from guelph
      read(20,*) itot,jmax,jmin,dfmin,clump,kmax
c kmax in above statement was added by Chen, 8/30/89.
      jmaxp1=jmax+1
      itotp1=itot+1
c  factir is reduction factor for emissivity of sky in atmos window
      read(20,*) emis,emisol, factir
c
c     ------------
c     | rleaf, tleaf used to be entered directly
c     | here, now they are calculated based on the
c     | fraction of the lai which has scenesed (sp?)
c     | Since this depends on the fraction live variable
c     | (frlive), which can change daily, rleaf, tleaf
c     | and aleaf are calculated daily in the daily loop
c     | of cusub.f    LMM 94/9/6
c
      read(20,*) (rsoil(m),m=1,kmax),(rllive(m),m=1,kmax),
     1(tllive(m),m=1,kmax)
      read(20,*) (rldead(m),m=1,kmax),(tldead(m),m=1,kmax)
c original 3 in above stm was changed to kmax, Chen, 8/30/89.
c
c
c*****  cupradazm insert begin 2: leaf reflectance/transmittance *******
c  Chen, 8/30/89.
c
c  input coefficients of parabola describing hemispherical refl and tran
c    as a func of source inc angle(a*x**2 + b*x + c = refl) for vis and
c    nir. other wavebands interpolated from these. c=normal inc value.
      read(20,*)avisr,bvisr,cvisr,anirr,bnirr,cnirr
      read(20,*)avist,bvist,cvist,anirt,bnirt,cnirt
c      
c*****  cupradazm insert end   2: leaf reflectance/transmittance *******
c
c********************canopy structure and root distribution
c
c   calc area fraction of leaves in each leaf angle class for
c   spherical leaf angle distrib.
      delthe=pi/(2.*itot)
      theta(1)=delthe*.5
      ct(1)=cos(theta(1))
      fr2=cos(delthe)
      fr(1)=1.-fr2
      fr1=fr2
      do 100 i=2,itot
      im1=i-1
      theta(i)=theta(im1)+delthe
      ct(i)=cos(theta(i))
      fr2=cos(i*delthe)
      fr(i)=fr1-fr2
      fr1=fr2
 100  continue
c
c** cupradazm insert begin 3: leaf incl./orient. function, extinction **
c Chen, 8/30/89.
      nxintz=9
      read(20,*)ispher,nalpha,(gr(l),l=1,nalpha)
      read(20,*)imunu,xmeu,xneu
      if(nalpha.ne.itot)write(6,123)itot,nalpha
 123  format(' itot= ',i3,' nalpha= ',i3)
      if(ispher.ne.0)go to 180
      if(imunu.eq.1)go to 185
      if(imunu.eq.2)angmn=xmeu
c      if(imunu.eq.2)ang2m=xneu
c above was commented and following two added by Chen, 7/6/89.
	if(imunu.eq.2)angsd=xneu
 	if(imunu.eq.2)ang2m=angmn*angmn+angsd*angsd
      if(imunu.eq.2)go to 176
      sum=0.
      do160i=1,nalpha
 160  sum=sum+gr(i)
      do170i=1,nalpha
 170  fr(i)=gr(i)/sum
      itot=nalpha
      goto172
c  set nalpha=itot so leaf angle classes are consistent
c 180  xint=.5
c above 1 was commented and following 2 added by Chen, 7/9/89. 
 180    do 171 ixintz=1,nxintz
 171	xintz(ixintz)=.5
      nalpha=itot
c
c  calc 1 st and 2 nd moments to fit beta dist to fr(i) for input dist
c    or spherical.
 172  sum1=0.
      sum2=0.
      do175i=1,nalpha
      alpha= 90./(2.*itot) + (i-1)*90./itot
      sum1=sum1+fr(i)*alpha
      sum2=sum2+fr(i)*alpha**2
 175  continue
      angmn=sum1
      ang2m=sum2
      angsd=sqrt(ang2m-angmn**2)
 176  xneu=(1.-ang2m/(90.*angmn))/(ang2m/angmn**2 -1.)
      xmeu=xneu*(90./angmn - 1.)
 185  continue
c  set nalpha=itot so leaf angle classes are consistent
      nalpha=itot
      read(20,*) imunua,xmeuaz,xneuaz,beta0,nbeta
	if (ispher.eq.0 .and. imunua.ne.0) goto 194
	  xmeuaz=1.
	  xneuaz=1.
	  beta0=0.
	  angamn=180.
	  angasd=103.92
	  goto 196
 194	if (imunua.ne.2) goto 196
	  angamn=xmeuaz
	  angasd=xneuaz
	  anga2m=angamn*angamn+angasd*angasd
	  xneuaz=(1.-anga2m/(360.*angamn))/(anga2m/angamn**2-1.)
	  xmeuaz=xneuaz*(360./angamn-1.)
 196	continue
c above 15 statements were added by Chen, 07/03/89.
      call lad(xmeu,xneu,fr,itot,gamrat,frdeg)
	if (ispher.eq.1) goto 198
	  if (imunu.ne.1) goto 197
	    angmn=0.
	    ang2m=0.
	    do 186 i=1,nalpha
	    alpha=(90./nalpha)*(i-.5)
	    angmn=angmn+alpha*fr(i)
 186	    ang2m=ang2m+alpha*alpha*fr(i)
	    angmn=angmn/nalpha
	    ang2m=ang2m/nalpha
	    angsd=sqrt(ang2m-angmn*angmn)
 197	  if (imunua.ne.1) goto 198
	    angamn=0.
	    anga2m=0.
	    do 187 i=1,nbeta
	    beta=(360./nbeta)*(i-.5)
	    angamn=angamn+beta*fraz(i)
 187	    anga2m=anga2m+beta*beta*fraz(i)
	    angamn=angamn/nbeta
	    anga2m=anga2m/nbeta
	    angasd=sqrt(anga2m-angamn*angamn)
 198	continue
c above 22 statements were added by Chen, 7/3/89.
c	write(6,*)'xmeuaz,xneuaz,angamn,angasd',xmeuaz,xneuaz,angamn,
c     1             angasd
c following 17 statements calculates extinction coefficient at nxintz
c zeniths averaged over azimuth, xintz(iz). Chen, 05/25/89.
	if (ispher.eq.1) goto 290
	wz=pi/2/nxintz
	wbeta=2*pi/nbeta
	do 211 iz=1,nxintz
	thetas=wz*(iz-.5)
	if (abs(xmeuaz-1.+xneuaz-1.).gt.0.0001) goto 263
	call simpsn(thetas,0.,fr,nalpha,xintz(iz))
	goto 211
 263	suma=0.
	do 220 iphi=1,nbeta
	phi=wbeta*(iphi-.5)
	call simpsn(thetas,phi,fr,nalpha,xxint)
 220	suma=suma+xxint
	xintz(iz)=suma/nbeta
 211    continue
 290	continue
c	write(6,*)'xintz(i)=',(xintz(i),i=1,nxintz)
c calculation of extinction coef. in viewing directions, xintv(.,.)
c follow. Chen, 9/19/89.
      do 270 ithetv=1,nozenv
      do 280 iphiv=1,noazmv
      xintv(ithetv,iphiv)=0.5
      if(ispher.eq.1) goto 280
      thetv=viewzn(ithetv)*pid180
      phiv=viewaz(iphiv)*pid180
      call simpsn(thetv,phiv,fr,nalpha,xintv(ithetv,iphiv))
 280  continue
 270  continue
c** cupradazm insert begin 3: leaf incl./orient. function, extinction **
c
c row and plant spacings
      read(20,*)rowspc,plspc,dmax
c  canopy str. (lai,leaf angles,leaf density,plant crown dimensions)
c    can be updated hourly, daily or fixed.  update variables ilaiup,
c    iladup, ildup, icrwup, icrhup
c    i...up  =1   update each unit of time(eg. hourly) by calculation
c            =2   update daily by calculation
c            =3   read values in
c
      read(20,*)ilaiup,pltpm2
      if(ilaiup.ne.3) go to 105
c     read(20,*)nodays,(tlai(l),l=1,nodays)
c     read(20,*)nodays,(ht(l),l=1,nodays)
c  input exponent in root distribution  eq in m-1
c     read(20,*)nodays,(arotin(l),l=1,nodays)
c
c********************wind
c
c  reference height for wind above ground and wind profile coeff
 105  read(20,*) refhtw,zdhcr,amfull,z0dh,dispdh
c
c********************air temperature
c
c  input number and heights of layers above canopy and above soil sfc.
      read(20,*)nlabcy,(z(l),l=1,nlabcy)
      refhtt=z(1)
      refhte=refhtt
      do 33 i=1,nlabcy
  33  z(i)=-z(i)
      read(20,*)nlbcpy,(zbcpy(l),l=1,nlbcpy)
c
c********************plant
c
c  this is where stom resis, photosynthesis, resp. etc. parameters
c    are read in if they are to remain fixed.
c  read in rs vs light
777   read(20,*)rcut20,rsmin,anstom,radn
c  read rs vs temp
778   read(20,*)trsopt,trsmax,trsmin,rsexp,rsm
c  read rs vs leaf psi
780   read(20,*)psi1,psi2,cidcai
c  cidcai  if < 0 then use cidca in code else set cidca=cidcai
c  convert potentials from bars to j/kg
      psi1=psi1*100.
      psi2=psi2*100.
c
c  additions to subr inplant for leaf p.s. by farquahr eqs.
      read(20,*)ic3c4,cair,rastom,cucond,d1,bkv,conmin,rxcham
c conmin in above stm was added. Chen, 10/8/89.
c
c  rxcham blr of one side of leaf in chamber used to meas p.s. (s/m)
c  rastom ratio of stom resis on top and bot of leaf.
c  rsfac  factor to adjust blr by for stom cond or resis ratio(li-6200)
      rsfac=(rastom**2+1.)/(rastom+1.)**2
c  cucond cuticular cond to water (mol/m2/s)
c  d1    vpd for start of stom closure (mbar)
c  d2    vpd for end of stom closure (mbar)
c  bkv   parameter for vpd response of stomata
c
c  ic3c4 flag 0=c3 farquhar   1=c4 corn  2=big blue stem  3=soybean
c	     10=c3 farquhar  11=c4 corn 12=big blue stem 13=soybean
c             4=c3 cranberry 14=c3 cranberry
c  Adding 10 flags cu main to call the old photks routine instead
c  of the new photc3 and photc4
c
c Note: search for ic3c4 in this file and in cumain to see the changes 
c       made by Dr. Sun and larry
c
      if  (  ((ic3c4.lt.0).or.(ic3c4.gt.14))
     +  .or. ((ic3c4.gt.4).and.(ic3c4.lt.10))  ) then
          write(6,'(''trouble in sub inplant ic3c4= '',i5)')ic3c4
          stop
      endif
c
c  notemp=1 if no temp effect in photosyn and stom
      read(20,*)notemp,tfix
      if(notemp.eq.1) write(6,81)tfix
 81   format(' notemp=1 so no temp effect in photosyn  tfix= ',f6.2)
c
c  the coefficients for the farquhar eqns are always gone thru first
c    and then other veg types are adjustments from that
c
c  coefficients from farquhar 1980 paper
c
c  t0    ref temp for respand k's (c)
      t0=25.
c  rgas  gas constant (j/mol/k)
      rgas=8.314
c  xk1   mult coeff for kc (microbars)
      xk1=460.
c  xk2   exp coefffor kc (j/mol)
      xk2=59356.
c  xk3   mult coeff for ko (millibars)
      xk3=330.
c  xk4   exp coeff for ko (j/mol)
      xk4=35948.
c  xk5   exp coeff for rdark (j/mol)                         c3 or c4
      xk5=66400.
c  xk6   exp coeff for vm (j/mol)
      xk6=58520.
c  ro    dark resp at t0 (micromol/m2/s)                     c3 or c4
      ro=1.1
c  vm    max carbox vel at t0 (micromol/m2/s)                 c3 or c4
      vm=100.
c  ox    oxygen conc. (millibars)
      ox=210.
c  cair  co2 conc of air (microbars)
c     cair=330.
c  xj2   max rate electron transfer (micro eq/m2/s)        c3 or c4
      xj2=250.
c  next 5 coeff for eq. 4 of schoolfield, sharp, magnuson(1981).
c  delha exp coeff for jm vs temp in numerator (j/mol)
      delha=1000.
c  delhl exp coeff jm vs temp, low temp side (j/mol)
      delhl=-120000.
c  delhh exp coeff jm vs temp, high temp side (j/mol)
      delhh=300000.
c  thalfl temp of half response onlow side (k)
      thalfl=294.
c  thalfh temp of half response on high side (k)
      thalfh=313.
c  pa    water pot factor to reduce p.s. and stom cond.
c  facj  factor in j vs light eq.                            c3 or c4
      facj=2.1
c  non-rect hyper coeff from Farquhar and Wong, 1984
      znon=0.7
c  cidca is ci/ca
      cidca=.72
c  gsin is gs (gamma star) input here. if it is neg (-1.) then gs is
c    calc. in sub photks. if gsin ge o. then gs takes on value given
c    here. if gs.lt.0 then c3    if gs.ge.0 then c4
c
      gsin=-1.
c---------------------------------------------------------------------
c DrSun/lMurdock - new c4 routine has a new common photc4.  The
c values below are default values.  They are actually for konza
c grasslands (big blue stem).  
c
c **  all following parameters are used for c4 photosynthes only. **
c pair: atomospheric presure, (1013. mb)
c rds and qtrd: two parametes in dark respiration eq. rds:(u mol co2 m-2
c sec.-1).  qtrd: nodimensional.
c vmax and qtvm: two parameters in Rubisco rate e2.. Vmax: (u mol co2
c  m-2 s-1)i. qtvm: nodimensional.
c krjc25 and qtpc: two parameters in rkjc eq.  rkjc25: (mol air m-2 s-1) 
c qtpc: nondimensionnal.
c rkjc: parametr in the eq. of co2 limited flux, wc. (mol air m-2 s-1)
c sms : propotional constant in Eq. of canopy resistance. (mol-H2o/mol-air)
c bc4 :                     detto                         (mol-H2o m-2 s-1)
c alpha: the rate coeeficient in light limited flux, u mol co2/umol-photons
c theta and beta: two adjustable parameters in the equation of finding
c minimum, ax, of vm and we, and minimum, px, of ax and wc.. both
c nondimensional
c cio: initial guess of intercelluer co2 concentration
c rcut: culticular resistance (m2*s/mol). condc=1./rcut 
c
      pair = 1013.0
      rds = 1.55
      qtrd = 1.8
      vmax = 30.15
      qtvm = 1.8
      coef = 0.16
      coef1 = 0.28
      tb = 14.
      rkjc25 = 0.208
      qtpc = 2.0
      sms = 3.7
      bc4 = 0.04
      alpha = 0.062
      c4tht = 0.81
      c4tht2 = 1.62
      beta = 0.92
      beta2 = 1.84
      rcut = 50.
      condc = 0.02
c
c farquhar coefficients c4
      if((ic3c4.eq.0).or.(ic3c4.eq.10))goto 190
c corn coefficients
      if((ic3c4.eq.1).or.(ic3c4.eq.11))goto 140
c big blue stem
      if((ic3c4.eq.2).or.(ic3c4.eq.12))goto 145
c soybean
      if((ic3c4.eq.3).or.(ic3c4.eq.13))goto 150
c cranberry
      if((ic3c4.eq.4).or.(ic3c4.eq.14))goto 150
c
c  corn
c
 140  continue
c if icorn=1 than xj2=850 instead of normal 700 and cidca=.4 instead
c    of the normal .37. xj2=700 is big using absorbed light, pmax=55
c    micromol m-2 s-1.
      icorn=0
c 
c next set of values added by DrSun/lMurdock for photc4
      pair = 1013.0
      rds = 1.55
      qtrd = 1.8
c     vmax = 35.0
c     qtvm = 2.4
c     coef = 0.4
c     coef1 = 10.0
c     tb = -50.0
      vmax = 56.25681
c  change vmax so pmax=50 at 1550 PAR
      vmax=70.
      qtvm = 0.98323
      coef = 0.70959
      coef1 = 0.25010
      tt=44.92311 
      tb = 20.41421
      rkjc25 = 0.6
      qtpc = 2.0
      sms = 3.7
      bc4 = 0.03
      alpha = 0.062
      c4tht = 0.81
      c4tht2 = 1.62
      beta = 0.99
c  change beta so less charp knee at A vs Ci curve
      beta=0.9
      beta2 = 2.*beta
      rcut = 50.
      condc = 0.02
c
c  this value of xk5 gives q10=1.84 at 25c (q10=exp(.0609))
      xk5=45000.
      ro=1.4
      vm=400.
      xj2=700.
c  reduce xj2 to 600 for more reasonable max p.s.
      xj2=600.
      if(icorn.eq.1)xj2=850
      delha=-4180.
      delhl=-167200.
      delhh=700000.
      thalfl=298.
      thalfh=317.
c  this facj=3.3 was derived from corn refl and trans using betty
c    walter-shea's thesis and quan eff=.062 q/mol from pearcy paper
c    this value of 3.3 requires using absorbed par.
      facj=3.3
      znon=.1
      cidca=.65
      if(icorn.eq.1)cidca=.4
      gsin=5
      goto 190
c
c  big blue stem konza prairie
c
 145  continue
c 
c next set of values added by DrSun/lMurdock for photc4
      pair = 1013.0
      rds = 1.55
      qtrd = 1.8
c     vmax = 30.15
c     qtvm = 1.8
c     coef = 0.16
c     coef1 = 0.28
c     tb = 14.
      vmax =41.87  
c this value was used for aug of 1987, after water stress period MCA 5/19/95
c      vmax = 16.75 
      qtvm = 0.8990
      coef = 0.63206
      coef1 = 0.20884
      tt=50.63505
      tb = 22.06127
      rkjc25 = 0.208
      qtpc = 2.0
      sms = 3.7
      bc4 = 0.04
      alpha = 0.062
      c4tht = 0.81
      c4tht2 = 1.62
      beta = 0.92
      beta2 = 1.84
      rcut = 50.
      condc = 0.02
c
      xk5=45000.
      ro=1.55
      vm=290.
      xj2=265.
      vm=311.
      xj2=349.
c  vm=290 xj2=265 was from Polley data but not at 25c. Better
c    values are vm=223 xj2=293 averaged over 20 leaves.  the
c    old values(290,265) were used in Briggs chapter and are too
c    low.
c  Average values for site 16 which is .41 BB, .26 SG .33 IG 
c    leads to vm=311 and xjmax=349.
c 
c  big blue  vm=223 xj2=293
c  switch gr vm=388 xj2=321
c  indian gr vm=361 xj2=416
c
      delha=-8000.
      delhl=-147200.
      delhh=436000.
      thalfl=297.
      thalfh=323.5
c  facj=3.3 yields QE=.055 at t=40c: too low so use 3.0(5/91)
      facj=3.3
      facj=3.0
      znon=0.1
      cidca=.65
      gsin=2.
      goto 190
c
c  soybean
c
 150  continue
      vmax =65.27418
      qtvm = 0.5413
      coef = 0.62397
      coef1 = 0.17359
      tt=42.91797
      tb = 25.39932
      xj2=430.
      vm=170.
      ro=1.7
      delha=1000.
      delhl=-120000.
      thalfl=294.
      thalfh=315.
      delhh=300000.
c
c  define coefficients for Berry  Photosynthesis model.  Based on
c ***   files in sunc/cupid6/psleaf, particularily prd3a.f********************
      pair = 1013
      if (ic3c4.eq.3) then
c         ------
c         | Soybean
          vmax = 100.
      else if (ic3c4.eq.4) then
c         -----
c         | Cranberry
	  vmax = 30.
      endif
      qtvm = 2.4
      tb = 0.0
      coef1 = 0.0
      sms = 9.0
      bc3 = 0.01
      alpha = 0.08
      thtc3 = .95
      thtc32 = 1.90
      beta = 0.98
      beta2 = 1.96
      vmvif = 1.0
      coefvm = 0.26
      tvm = 40.0
      paab = 1.0
      qtwz = 2.4
      coefwz = 0.3
      twz = 12.0
      sp25 = 2600.0
      qtsp = 0.57
      o2 = 20000.0
      rkc25 = 30.0
      qtkc = 2.1
      rko25 = 30000.0
      qtko = 1.2
      qtrd = 2.0
c****** Berry Photosynthesis model coefficients have been defined above******
c
      goto 190
c
 190  continue
      if(cidcai.gt.0.0001)cidca=cidcai
c
c  input root resis in m4 kg-1 s-1 (corn=3.e6) assume .6 of plant
c    resistance is in root
      read(20,*)rroot
c  input cpy rain interception parameters
c    frstem  frac total water intercepted by leaf that goes down stem
c    frwtmx  max frac leaf sfc wetted by rain where transp=0
c    pintmx  max ave effective depth of rain on leaf before it will
c            drip.(in mm) pint(j) is water intercepted by leaf
      read(20,*)frstem,frwtmx,pintmx
c
c ***************** read in daily plant data *********************
c
c clai, ht, etc is read in here by day and interpolated for all days.
c
c
      read(20,*)nodyin
      if(nodyin.gt.30)write(6,200)nodyin
 200  format(' nodyin = ',i3,'  greater than 30')
      if(nodyin.gt.30)stop
c
      do210i=1,nodyin
c     --------
c     | added xfrlv  for Green LAI  LMM 94/9/6
      read(20,*)ldayin(i),xtlai(i),xfrlv(i),xht(i),xarot(i),xzldh(i)
     &,xzmdh(i),tsolbc(i),wsolbc(i)
c     |  DO NOT REMOVE THE FOLLOWING WRITE STATEMENT!
c     |  It prevents a Math error on HP for some reason... MCA 5/25/95
      write(6,*)' '
c     --------
c     | Set lai profile to uniform if xzmdh < 0
      iunif=0
      if (xzmdh(i).lt.0.) iunif=1
c     --------
c     | Check Input ranges on Fraction Live.
      if (xfrlv(i).gt. 1.0 ) then
          write(*,*)"Line ",i," of day inputs has a frac live > 1.0"
          stop
      endif
      if (xfrlv(i) .lt. 0.01) then
          write(*,*)"Line ",i," of day inputs has a frac live < 0.01"
          stop
      endif
c  input height is height of plant but height needed for model is 
c    height perpendicular to slope.
      xht(i)=xht(i)*cos(slope)
 210  continue
c
      nstr=1
      do250 i=2,nodyin
      nxday=ldayin(i)-ldayin(i-1)
      nend=nstr+nxday
      do240 i2=nstr,nend
      if(i2.gt.daymax)write(6,235)i2
 235  format(' no. of days gt daymax, noday= ',i4)
      if(i2.gt.daymax)stop
      f2=float((i2-nstr))/float(nxday)
      tlai(i2)=xtlai(i-1)+ (xtlai(i)-xtlai(i-1))*f2
      frlive(i2)=xfrlv(i-1)+ (xfrlv(i)-xfrlv(i-1))*f2
      ht(i2)=xht(i-1)+ (xht(i)-xht(i-1))*f2
      arotin(i2)=xarot(i-1)+ (xarot(i)-xarot(i-1))*f2
      zldh1(i2)=xzldh(i-1)+ (xzldh(i)-xzldh(i-1))*f2
      zmdh1(i2)=xzmdh(i-1)+ (xzmdh(i)-xzmdh(i-1))*f2
      tsbc(i2)=tsolbc(i-1)+ (tsolbc(i)-tsolbc(i-1))*f2
      wsbc(i2)=wsolbc(i-1)+ (wsolbc(i)-wsolbc(i-1))*f2
      if(ht(i2).ge.-z(1))then
      write(*,*)'all layer heights for avove the canopy are too small.'
      stop
      endif
 240  continue
      nstr=nend
 250  continue
c
      do260 i=1,nend
      ndayin(i)=ldayin(1)+i-1
c     write(21,261)i,ndayin(i),tlai(i),ht(i),arotin(i),zldh1(i),zmdh1(i)
c    &,tsbc(i),wsbc(i)
c261  format(i3,i4,7f8.3)
 260  continue

c *********************************************************************
c check if all height input data in correct order. give error messages
c if not:
      do30jj=1,nlabcy-1
      if(z(jj+1)-z(jj).le.0)then
      write(*,*)'error in height input for layers above canopy:'
      write(*,93)(jz,z(jz),jz=1,nlabcy)
  93  format(1x,i3,f9.3)
      stop
      endif
  30  continue
      do40jj=1,nlbcpy-1
      if(zbcpy(jj)-zbcpy(jj+1).le.0.)then
      write(*,*)'error in height input for layers below canopy:'
      write(*,44)(jz,zbcpy(jz),jz=1,nlbcpy)
  44  format(1x,i3,f9.3)
      stop
      endif
  40  continue
      do50jj=1,ndsoil-1
      if(zsoil(jj+1)-zsoil(jj).le.0)then
      write(*,*)'error in height input for soil layers:'
      write(*,56)(jz,zsoil(jz),jz=1,ndsoil)
  56  format(1x,i3,f9.3)
      stop
      endif
  50  continue
c **********************************************************************
      return
      end
c-------------------------------------------------------------- INSOIL
      subroutine insoil(refdpt)
      common/soil1/zsoil(25),wti(50),ndsoil,aksoil(50),cpsoil(50),tsn(50
     1)
c     --------------
c     | residue variables added LMM 94/9/6
c     | layer subscripts were added to the *fc variables - MCA 5/24/95
      common/soil3/clodsz,sandfc(25),siltfc(25),clayfc(25),qrtzfc(25),
     & iresdu
c     --------------
c     | rock variables added to soil4 LMM 94/9/8
c     | layer subscripts were added by MCA 5/24/95
      common/soil4/pe(25),bx(25),bd(25),aks(25),an(25),ws(25),asoil(25),
     &  bsoil(25),csoil(25),dsoil(25),esoil
     &             ,idoroc,irocly,akrock,cprock,layid(25)
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/water2/sw,deld,drhsfc,rhslop,drain,drain5,filt,etmm,drgrav
      common /misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
c pid2,kmax in above statement were added by Chen, 9/4/89.
c     | The following dimension statement was added by MCA 5/24/95
      dimension zlsoil(10),dbd(10),dsndfc(10),dsltfc(10),dclyfc(10),
     &		dqtzfc(10),dpe(10),dbx(10),daks(10)
c
c  input no of soil depths and ref depth
c  clodsz is soil clod size for calc of soil transfer coeff under
c  stable conditions
c  iresdu added by LMM 94/9/6.  if 1 then residue on surface
c                                  0 no residue.
c  z0soil added by LMM 94/10/3  roughness length in meters
c                               0.003 is typical for clear ground
c                               0.01  is typical for ground with residue.
c
c  input soil texture data. if not available for pe,bx and aks, enter
c  0 and program generates estimates.
      read(20,*)clodsz,ndsoil,refdpt,iresdu,z0soil
c  These are all layered quantities now - MCA 5/24/95
c      read(20,*)bd,sandfc,siltfc,clayfc,qrtzfc,dpe,dbx,daks
      read(20,*)(zsoil(jz),jz=1,ndsoil)
c  These next two soil layer composition read-in statements were added
c  by MCA on 5/24/95
      read(20,*)nlsoil
      do 220 jl=1,nlsoil
        read(20,*)zlsoil(jl),dbd(jl),dsndfc(jl),dsltfc(jl),
     &       dclyfc(jl),dqtzfc(jl),dpe(jl),dbx(jl),daks(jl)
 220  continue
c
c  Fill in soil layers 1 to ndsoil with appropriate layered soil parameters
      if (zlsoil(nlsoil).ne.zsoil(ndsoil)) then
	write(*,*) 'Setting zlsoil(nlsoil) = zsoil(ndsoil)'
	zlsoil(nlsoil) = zsoil(ndsoil)
      end if
      jz1=1
      layno=1
      do 260 jl=1,nlsoil
	if ((jl.ne.nlsoil).and.(zlsoil(jl).ge.zlsoil(jl+1))) then
	  write(*,*) 'Problem with soil layer interface depths.'
	  stop
        end if
	do while (zsoil(jz1).lt.zlsoil(jl))
	  layid(jz1)=layno
	  bd(jz1) = dbd(jl)
	  sandfc(jz1) = dsndfc(jl)
	  siltfc(jz1) = dsltfc(jl)
	  clayfc(jz1) = dclyfc(jl)
	  qrtzfc(jz1) = dqtzfc(jl)
	  pe(jz1) = dpe(jl)
	  bx(jz1) = dbx(jl)
	  aks(jz1) = daks(jl)
	  jz1 = jz1+1
	  if (jz1.gt.ndsoil) go to 265
        end do
        layno=layno+1
 260  continue
 265  continue
      layid(ndsoil)=layno-1
      bd(ndsoil) = dbd(nlsoil)
      sandfc(ndsoil) = dsndfc(nlsoil)
      siltfc(ndsoil) = dsltfc(nlsoil)
      clayfc(ndsoil) = dclyfc(nlsoil)
      qrtzfc(ndsoil) = dqtzfc(nlsoil)
      pe(ndsoil) = dpe(nlsoil)
      bx(ndsoil) = dbx(nlsoil)
      aks(ndsoil) = daks(nlsoil) 
c
c     ----------
c     | Rock layer variables added LMM 94/9/8
c     | idoroc - 1 if rock layer exists - 0 no rocs
c     | irocly - the ndsoil layer where rock starts.
c     | akrock - rock thermal cond.
c     | cprock - heat capacity of rock
      read(20,*)idoroc,irocly,akrock,cprock
      cprock = cprock*1000000
      if (idoroc.eq.1 .and. irocly.gt.ndsoil) then
          write(*,*) 'IROCLY > NDSOIL !!!!'
          stop
      endif
c     -----------
c     | z0soil no longer set. Rather
c     | it is in the input file and read in
c     | above. LMM 94/10/3
c Sun: z0soil is 0.01 originally, then corrected to be 0.005 m
c     z0soil=0.005
c     z0soil=0.01
c Sun: following z0soil is  for Tom's plate
c     z0soil=0.0002
c     z0soil=0.00004
c
c  input lower limit of sum of abs values of d(jz) in profl2 for
c    iteration over soil water profile.
c  deld is tolerance on sum d(jz) on new-raph soil eqs--1.e-5 to 1.e-7
c    should be at least 1.e-5 to get storage change to match fluxes.
c  drhsfc is tolerance on matching sfc rel hum from cpy vapor eqs
c    with rel hum from soil eqs-determines how close ecpys and wcpys
c    are. 0.001 is small and 0.01 usually acceptable.
c  rhslop is change of rhsfc below value from cpy vapor eqs to get
c    slope of rhsfc vs soil evap for new-raph interation in soil eqs.
      read(20,*)deld,drhsfc,rhslop
c
c I made this into a loop over soil layers - MCA 5/24/95
c here is where alan's file got inserted:
c
c     compute geometric diameter and geometric standard deviation
c     write(*,200)
c     write(*,210) sandfc,siltfc,clayfc,qrtzfc,clodsz,bd,iflgks
c200  format(' sandfc,siltfc,clayfc,qrtzfc,clodsz,bd,iflgks ')
c210  format(1x,6(f7.3,1x),i2)
      do 250 jz1=1,ndsoil
        adum=sandfc(jz1)*alog(1.025)+siltfc(jz1)*alog(.026)+
     &	  clayfc(jz1)*alog(.001)
        bdum=(sandfc(jz1)*(alog(1.025))**2+siltfc(jz1)*(alog(.026))**2+
     &	  clayfc(jz1)*(alog(.001))**2-adum*adum)**.5 
        dgeom=exp(adum)
        sgeom=exp(bdum)
c     compute coefficients for soil thermal conductivity
        psolid=bd(jz1)/2.65
        ws(jz1)=1.-psolid
        asoil(jz1)=(.57+1.73*qrtzfc(jz1)+.93*(psolid-qrtzfc(jz1)))/
     &(1.-.74*qrtzfc(jz1)-.49*(psolid-qrtzfc(jz1)))-2.8*psolid*ws(jz1)
        bsoil(jz1)=1.06*bd(jz1)
        csoil(jz1)=1.+2.6*clayfc(jz1)**(-.5)
        dsoil(jz1)=.03+.1*bd(jz1)*bd(jz1)
        esoil=2.
c     compute air entry potential, pe=j/kg
        pes=-.5*dgeom**(-.5)
        if (abs(bx(jz1)).le.1.e-9) bx(jz1)=-2.*pes+.2*sgeom
        if (abs(pe(jz1)).le.1.e-9) pe(jz1)=pes*(bd(jz1)/1.3)**
     &	  (.67*bx(jz1))
c     compute sat. hyd. conductivity (kg s m-3)
        if (abs(aks(jz1)).le.1.e-9) aks(jz1)=.004*(1.3/bd(jz1))**
     &	  (1.3*bx(jz1))*exp(-6.9*clayfc(jz1)-3.7*siltfc(jz1))
c     aks=4.25e-4 is measured field value for sharpsburg soil at rf unl
c     aks=2.05e-3 is value used for sat. cond vf sandy loam, phd86
c     bx=4.35 and pe=-8.64 are for rf sharpsburg soil
c     pe=-2.5 is for vf sandy loam, phd86 , bx was computed with theory
c     bd for rf soil was 1.3
c     vf sandy loam phd86 :sand=.6,silt=.3,clay=.1,qrtz=.63
c     pe=-2.5
        an(jz1)=2.+3./bx(jz1)
 250  continue
c
      if(iwrite(1,8).eq.1)then
      write(21,61)
 61   format(' 1108',9x,/,14x,
     &'  zsoil    pwp  wt3.0  wt0.3 wt0.1  wt.05 ','  wt.01')
      do350jz1=1,ndsoil
      if(-1500.0.lt.pe(jz1))then
        pwp=100.*ws(jz1)*(pe(jz1)/(-1500.))**(1./bx(jz1))
      else
        pwp=100.*ws(jz1)
      endif
c     pwp=0.15
      if(-1.0.lt.pe(jz1))then 
        fc5=100.*ws(jz1)*(pe(jz1)/(- 1.))**(1./bx(jz1))
      else
        fc5=100.*ws(jz1)
      endif
      if(-5.0.lt.pe(jz1))then
        fc4=100.*ws(jz1)*(pe(jz1)/(- 5.))**(1./bx(jz1))
      else
        fc4=100.*ws(jz1)
      endif
      if(-10.0.lt.pe(jz1))then
        fc3=100.*ws(jz1)*(pe(jz1)/(-10.))**(1./bx(jz1))
      else
        fc3=100.*ws(jz1)
      endif
      if(-30.0.lt.pe(jz1))then
        fc2=100.*ws(jz1)*(pe(jz1)/(-30.))**(1./bx(jz1))
      else
        fc2=100.*ws(jz1)
      endif
      if(-300.0.lt.pe(jz1))then
        fc1=100.*ws(jz1)*(pe(jz1)/(-300.))**(1./bx(jz1))
      else
        fc1=100.*ws(jz1)
      endif
      write(21,62)jz1,zsoil(jz1),pwp,fc1,fc2,
     & fc3,fc4,fc5
 62   format(' 2108',5x,i2,2x,9f7.2)
 350  continue
      endif
c
      if(iwrite(1,11).eq.1)then
      write(21,63)
 63   format(' 1111',9x,/,14x,'   pe     bx  ',' aksmmh   ws  ')
      do360jz1=1,ndsoil
c  calc aks in mm/hr=kg s m-3 * 9.8m s-2 *3600 s hr-1 
      aksmmh=aks(jz1)*9.8*3600. 
      if(iwrite(1,11).eq.1)write(21,64)jz1,pe(jz1),bx(jz1),aksmmh,
     &  100.*ws(jz1)
 64   format(' 2111',5x,i2,2x,9f7.2)
c
c     write(*,300) pe,pes,bx,an,ws,aks,pwp,fc
c     write(*,310) dgeom,sgeom,adum,bdum
c     write(*,320) asoil,bsoil,csoil,dsoil
c300  format(' pe,pes,bx,an,ws,aks ',5(f7.3,1x),f8.6,2(1x,f4.2))
c310  format(' dgeom,sgeom,adum,bdum ',4(f7.3,1x))
c320  format(' asoil,bsoil,csoil,dsoil ',4(f8.3,1x))
 360  continue
      endif
c     stop
      return
      end
c--------------------------------------------------------------- INSPKL
      subroutine inspkl
c input parameter for droplet evaporation calculation
      common/spkler/ctwa,dtwa,etwa,ftwa,gtwa,cetsum,detsum,eetsum,fetsum
     &,getsum,hetsum,cqsum,dqsum,eqsum,fqsum,gqsum,cetfix,detfix,eetfix
     &,fetfix,cqfix,dqfix,eqfix,fqfix,zmax,htspk,abspk,bspk,spkprs
     &,bdyang,tdropi
      read(20,*)ctwa,dtwa,etwa,ftwa,gtwa
      read(20,*)cetsum,detsum,eetsum,fetsum,getsum,hetsum
      read(20,*)cqsum,dqsum,eqsum,fqsum,gqsum
      read(20,*)cetfix,detfix,eetfix,fetfix
      read(20,*)cqfix,dqfix,eqfix,fqfix
      read(20,*)zmax,htspk,abspk,bspk,spkprs,bdyang,tdropi
      return
      end
      subroutine initc
      parameter(mh=98)
      common/soil1/zsoil(25),wti(50),ndsoil,aksoil(50),cpsoil(50),tsn(50
     1)
c  input initial water content profile
      read(20,*)(wti(jz),jz=1,ndsoil)
      return
      end
