c $VERSION "08/16/95 @(#)culfbal.f	7.1"
      subroutine stoma(ihr,itassl)
      parameter(mh=98,daymax=367)
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/resis2/radn,anstom,rcut20,rsmin,trsopt,trsmin,trsmax,rsm
     1,rsexp,psi1,psi2
      common /rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20)
     &,templf(10,20),tsoil(mh)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
c akroot(50) added to /root1/ by MCA - 6/12/95
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1,rootup(50),cpytr,psisum,akroot(50)
      common/mitdam/xmdcm2(20),xmdmin,xmdmax,rsdam
c  cuticular resistance may be a func of temp
      rcut=rcut20
      do1000i=1,itotp1
      do1000j=2,jtot
c   calc rs vs radiation
      if(dstrad(1,i,j).gt.0.01)go to 100
      rsleaf(i,j)=rcut
      go to 1000
 100  znum=(anstom-1.)*radn/dstrad(1,i,j)
      rsleaf(i,j)=rsmin*(znum+1.)
c     write(6,*)rsleaf(i,j),znum,rcut
c  calc temp effect g(templf)
      if(templf(i,j)-trsopt)400,400,500
 400  g=1.+(((trsopt-templf(i,j))/(trsopt-trsmin))**rsexp)*rsm
      go to 600
 500  g=1.+(((templf(i,j)-trsopt)/(trsmax-trsopt))**rsexp)*rsm
 600  continue
c  calc water pot effect hpsi
      if(psitop.ge.psi1)hpsi=1.
      if(psitop.lt.psi1)hpsi=(psitop-psi2)/(psi1-psi2)
      if(psitop.lt.psi2)hpsi=0.
      if(hpsi.lt.0.001)hpsi=0.001
      rsleaf(i,j)=rsleaf(i,j)*g/hpsi
c     write(6,*)rsleaf(i,j),rcut,znum,g,hpsi
c  calc final resis as parallel comb of rcut and rs
      rsleaf(i,j)=rcut*rsleaf(i,j)/(rcut+rsleaf(i,j))
c  for tassels rsleaf(i,jtot)=rcut
      if(itassl.ne.0)rsleaf(i,jtot)=9900.
c   routine to adjust the stomatal resistance due to mite
c   damage of leaf tissue
c     fdam=(xmdcm2(j)-xmdmin)/(xmdmax-xmdmin)
c     if(fdam.lt.0.0)fdam=0.0
c     if(fdam.gt.1.0)fdam=1.0
c     rsleaf(i,j)=(1.0-fdam)*rsleaf(i,j)+fdam*rsdam
c   note- assume that when max feeding damage occurs stom resis=1000.
 1000 continue
      return
      end
c------------------------------------------------------------------------------
c								LFEBAL
      subroutine lfebal(ihr,iday)
	  integer daymax
      parameter(mh=98,daymax=367)
      dimension jcond(24,10,20)
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/photo2/ox,cair,rastom,cucond,d1,d2,facj,ic3c4,rxcham,rsfac
     &             ,znon,conmin
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/leaf2/evap(10,20),gevap(10,20),heat(10,20),gheat(10,20)
     &,alam ,tlfavg(20),tgheat(20),tgvap1(20),tgvap2(20)
      common/leaf3/hwater(10,20),ghwatr(10,20)
      common /rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20)
     &,templf(10,20),tsoil(mh)
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
c pid2,kmax in /misc1/ were added by Chen, 9/4/89.
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
      common/cpy2/hsoil,hcpys,evtot,etotw(20),contot(20),scond(10,20)
     1,ihrwet(20),ecpys,cphstr,cpestr,wcpys,evsmic
      common/inter2/evint(20),evimm(20),pintmx,frstem,drip(20),stem
      common/inter1/wtp(20),frwet(20),frwtmx,pint(20),pilast(20)
     1,pint1(20),twater
c     ----------
c     | added for frlive LMM 94/9/7
      common/daily/tlai(daymax),ht(daymax),arotin(daymax)
     &,zldh1(daymax),zmdh1(daymax),tsbc(daymax),wsbc(daymax)
     &,frlive(daymax)
      common/iterat/noiter,iter2,iter3,iterw,loope,loopt,loopw
c=======================================================================
c						     DA BIG DIAGNOSTIC
c from the period when we were adding feed back
c between lfbal and newton raphson 94/11/23
c
c      if (iday.eq.2 .and. ihr.eq.15) then
c
c1111    format(F7.2,9(',',F6.2))
c1112    format(I1,F6.2,9(',',F6.2))
c	write(23,*)
c     &'-----------------------------------',
c     &'-----------------------------------'
c        write(23,*)
c     &'  iday   ihr   iter2  noiter'
c        write(23,1111)
c     &  iday,  ihr,  iter2, noiter
c	write(23,*)
c     &'-----------------------------------',
c     &'-----------------------------------'
c	write(23,*)
c     &'   dt     df   sigma   rsfac   jtot'
c	write(23,1111)
c     & dt ,  df   ,sigma , rsfac , jtot
c	write(23,*)' '
c	write(23,*)
c     &'cont1  cont2  cont4  cont6  cont8  ',
c     &'cont10 cont12 cont14 cont16 cont17'
c	write(23,1111)
c     &contot(1 ),contot(2 ),contot(4 ),contot(6 ),contot(8 ),
c     &contot(10), contot(12),contot(14),contot(16),contot(17)
c	write(23,*)' '
c	write(23,*)
c     &'frwet1 frwet2 frwet4 frwet6 frwet8 ',
c     &'frwet10frwet12frwet14frwet16frwet17'
c	write(23,1111)
c     &frwet(1 ),frwet(2 ),frwet(4 ),frwet(6 ),frwet(8 ),
c     &frwet(10), frwet(12),frwet(14),frwet(16),frwet(17)
c	write(23,*)' '
c	write(23,*)
c     &'frwet1 frwet2 frwet4 frwet6 frwet8 ',
c     &'frwet10frwet12frwet14frwet16frwet17'
c	write(23,1111)
c     &frwet(1 ),frwet(2 ),frwet(4 ),frwet(6 ),frwet(8 ),
c     &frwet(10), frwet(12),frwet(14),frwet(16),frwet(17)
c	write(23,*)' '
c	write(23,*)
c     &'pint1  pint2  pint4  pint6  pint8  ',
c     &'pint10 pint12 pint14 pint16 pint17'
c	write(23,1111)
c     &pint1(1 ),pint1(2 ),pint1(4 ),pint1(6 ),pint1(8 ),
c     &pint1(10), pint1(12),pint1(14),pint1(16),pint1(17)
c	write(23,*)' '
c	write(23,*)
c     &'eair1  eair2  eair4  eair6  eair8  ',
c     &'eair10 eair12 eair14 eair16 eair17'
c	write(23,1111)
c     &eair(1 ),eair(2 ),eair(4 ),eair(6 ),eair(8 ),
c     &eair(10), eair(12),eair(14),eair(16),eair(17)
c	write(23,*)' '
c	write(23,*)
c     &'tair1  tair2  tair4  tair6  tair8  ',
c     &'tair10 tair12 tair14 tair16 tair17'
c	write(23,1111)
c     &tair(1 ),tair(2 ),tair(4 ),tair(6 ),tair(8 ),
c     &tair(10), tair(12),tair(14),tair(16),tair(17)
c	write(23,*)' '
c	write(23,*)
c     &'rhlf1  rhlf2  rhlf4  rhlf6  rhlf8  ',
c     &'rhlf10 rhlf12 rhlf14 rhlf16 rhlf17'
c	write(23,1111)
c     &rhleaf(1 ),rhleaf(2 ),rhleaf(4 ),rhleaf(6 ),rhleaf(8),
c     &rhleaf(10), rhleaf(12),rhleaf(14),rhleaf(16),rhleaf(17)
c	write(23,*)' '
c	write(23,*)'RSLEAF'
c	write(23,*)
c     &'Layer1      2      4      6      8 ',
c     &'    10     12     14     16     17'
c        do i=1,10
c	write(23,1112)
c     &i,rsleaf(i,1 ),rsleaf(i,2 ),rsleaf(i,4 ),rsleaf(i,6 ),rsleaf(i,8),
c     &rsleaf(i,10), rsleaf(i,12),rsleaf(i,14),rsleaf(i,16),rsleaf(i,17)
c        enddo
c	write(23,*)' '
c	write(23,*)'FRAREA'
c	write(23,*)
c     &'Layer1      2      4      6      8 ',
c     &'    10     12     14     16     17'
c        do i=1,10
c	write(23,1112)
c     &i,frarea(i,1 ),frarea(i,2 ),frarea(i,4 ),frarea(i,6 ),frarea(i,8),
c     &frarea(i,10), frarea(i,12),frarea(i,14),frarea(i,16),frarea(i,17)
c        enddo
c	write(23,*)' '
c	write(23,*)'TEMPLF'
c	write(23,*)
c     &'Layer1      2      4      6      8 ',
c     &'    10     12     14     16     17'
c        do i=1,10
c	write(23,1112)
c     &i,templf(i,1 ),templf(i,2 ),templf(i,4 ),templf(i,6 ),templf(i,8),
c     &templf(i,10), templf(i,12),templf(i,14),templf(i,16),templf(i,17)
c        enddo
c	write(23,*)' '
c	write(23,*)'DSTNET'
c	write(23,*)
c     &'Layer1      2      4      6      8 ',
c     &'    10     12     14     16     17'
c        do i=1,10
c	write(23,1112)
c     &i,dstnet(i,1 ),dstnet(i,2 ),dstnet(i,4 ),dstnet(i,6 ),dstnet(i,8),
c     &dstnet(i,10), dstnet(i,12),dstnet(i,14),dstnet(i,16),dstnet(i,17)
c        enddo
c	write(23,*)' '
c	write(23,*)'DSTRAD(3,.,.)'
c	write(23,*)
c     &'Layer1      2      4      6      8 ',
c     &'    10     12     14     16     17'
c        do i=1,10
c	write(23,1112)
c     &i,dstrad(3,i,1 ),dstrad(3,i,2 ),dstrad(3,i,4 ),dstrad(3,i,6 )
c     &,dstrad(3,i,8),
c     &dstrad(3,i,10), dstrad(3,i,12),dstrad(3,i,14),dstrad(3,i,16)
c     &,dstrad(3,i,17)
c        enddo
c	write(23,*)' '
cc      1 3456 2 3456 3 3456 4 3456 5 34567
cc      6 3456 7 3456 8 3456 9 3456 0 34567
cc
c      endif
cc
cc=======================================================================
c  calc leaf energy bal for each angle class and layer
      do1100j=2,jtot
c  tair and eair are at top of layers so ave adjacent values to get
c    layer average
c      if(j.gt.2)tmean=(tair(j)+tair(j-1))*.5
c      if(j.eq.2)tmean=tair(j)
c      if(j.gt.2)emean=(eair(j)+eair(j-1))*.5
c      if(j.eq.2)emean=eair(j)
      emean=eair(j)
      tmean=tair(j)
      sumbal=0.
      sumtlf=0.0
      sgheat=0.0
      sgvap1=0.0
      sgvap2=0.0
      do1000i=1,itotp1
      iflgrs=1
      irept=0
      icond=0
      isate=0
 100  temp=(templf(i,j)+tmean)*0.5
c  sat vp calc
       estemp=6.108*10**(7.5*temp/(temp+237.3))
      estair=6.108*10**(7.5*tmean/(tmean+237.3))
c  latent heat
      alam=597.-0.57*temp
c  slope of sat vp curve
      s=estemp*alam/(0.1103*(temp+273.)**2)
      if(contot(j).ge.0.)go to 400
 300  isate=1
      iflgrs=0
 400  continue
c  rsleaf is for both sides of leaf  rhleaf is for one side of leaf
c  for amphistom leaves rsfac=.5  see surb inplnt
c  for hypostom leaves rsfac=1
      if(isate.eq.1)rsfac2=.5
      if(isate.eq.0)rsfac2=rsfac
c      
c     ------------------
c     | Leaf balance Needs a resistance that
c     | reflects both the live and dead parts of
c     | the leaf.  The dead parts have an rsleaf of
c     | 0.0 thus the equation just scales back rsleaf
c     | the fraction live. LMM 94/9/6
c      if ((         abs(rsleaf(i,j)).lt. 0.0001)
c     &		.or.(frlive(iday).lt. 0.0001)) then
c          rslftot = 0.0
c      else
c          rslftot = 1./((1./rsleaf(i,j))*frlive(iday))
c      endif
c
c  Now RSLEAF scaling for dead veg fraction is done in the photosynthesis
c  model, so rsleaf is correct at this point - MCA 4/26/95 (added next statement)
      rslftot = rsleaf(i,j)
c
      r1=iflgrs*rslftot+rhleaf(j)*rsfac2
c  assume frwet applies to both sides of leaf
      hh=frwet(j)/(rhleaf(j)*0.5)+(1.-frwet(j))/r1
c     approx correction for heat taken from leaf by cold irrig water
c     twater calculated in profl2
      if(ihr.eq.1.and.iday.eq.1) twater=tair(j)
      if(pint1(j).lt..0001) twater=tair(j)
      znum=1825.*hh*(estair-emean)+1.134e-7*emis*(tmean+273.)**4
     &+pint1(j)*4187.6*(tair(j)-twater)/(dt*df)
      den=1825.*s*hh+2400./rhleaf(j)+4.536e-7*emis*(temp+273.)**3
     &+pint1(j)*4187.6/(dt*df)
      delt(i,j)=(dstnet(i,j)+2.*dstrad(3,i,j)-znum)/den
      templf(i,j)=tmean+delt(i,j)
      estlf=6.108*10**(7.5*templf(i,j)/(templf(i,j)+237.3))
      if(isate.eq.1)go to 700
      if(emean-estlf)580,580,550
 550  if(icond.eq.1)go to 700
      icond=1
      iflgrs=0
      go to 100
 580  if(icond-1)700,595,595
 595  if(irept.eq.1)go to 700
      iflgrs=1
      irept=1
      go to 100
 700  continue
      relh(j)=emean/estair
c     write(6,*)estemp,alam,znum,den,templf(i,j),r,s
      evap(i,j)=1825.*hh*((estair-emean)+s*delt(i,j))
      gvap1=1825.*hh*(estair-emean)*df*frarea(i,j)/(alam*4.18e-3)
      gvap2=1825.*hh*s*delt(i,j)*df*frarea(i,j)/(alam*4.18e-3)
      tran(i,j)=(1.-frwet(j))*1825./r1*(estair-emean+s*delt(i,j))
      gevap(i,j)=evap(i,j)*df*frarea(i,j)
      heat(i,j)=2400./rhleaf(j)*delt(i,j)
      gheat(i,j)=heat(i,j)*df*frarea(i,j)
c  calc heat sink from cold irrig water
      hwater(i,j)=pint1(j)*4187.6*(templf(i,j)-twater)/(dt*df)
      ghwatr(i,j)=hwater(i,j)*df*frarea(i,j)
c
c     write(6,*)r,znum,den,delt(i,j),tair(j),dstrad(3,i,j),dstnet(i,j)
      dstnet(i,j)=dstnet(i,j)+2.*dstrad(3,i,j)
      dstrad(3,i,j)=emis*sigma*(templf(i,j)+273.)**4
      dstnet(i,j)=dstnet(i,j)-2.*dstrad(3,i,j)
c     write(6,*)temp,delt(i,j),r,s,evap(i,j),heat(i,j),frarea(i,j),df
c stms to check leaf energy bal, should be good to .01wm-2.
      bal=dstnet(i,j)*df*frarea(i,j)-gevap(i,j)-gheat(i,j)-ghwatr(i,j)
      bal2=dstnet(i,j)-evap(i,j)-heat(i,j)-hwater(i,j)
      sumbal=sumbal+bal
      sgheat=sgheat+gheat(i,j)
      sgvap1=sgvap1+gvap1
      sgvap2=sgvap2+gvap2
      sumtlf=sumtlf+templf(i,j)*frarea(i,j)
 1000 continue
c    | end of i loop (started above 100)
c    -------------------
      tlfavg(j)=sumtlf
      if ( abs(tlfavg(j)-tmean).lt.0.01) then
	  tgheat(j) = 0.0
	  tgvap2(j) = 0.0
      else
          tgheat(j)=sgheat/(tlfavg(j)-tmean)
	  tgvap2(j)=sgvap2/(tlfavg(j)-tmean)
      endif
      if ( abs(estair-emean).lt.0.01) then
	  tgvap1(j) = 0.0
      else
	  tgvap1(j)=sgvap1/(estair-emean)
      endif
 1100 continue
c    | end of j loop (started above 100)
c    -------------------
      do2000j=2,jtot
      sum=0.
      do1500i=1,itotp1
      sum=sum+(evap(i,j)-tran(i,j))*frarea(i,j)
 1500 continue
      evint(j)=sum*df
 2000 continue
c     if(ihr.eq.21)write(26,405)ihr,isate,iflgrs,irept,icond,eair(jtot),
c    1estlf,tair(jtot)
 405  format(1x,5i3,2(1x,e10.4))
c     if(ihr.eq.19)write(6,*)icond,isate,iflgrs
      return
      end
c----------------------------------------------------------------------
c                                                         SUMFLX
      subroutine sumflx(ihr)
      parameter(mh=98)
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/leaf2/evap(10,20),gevap(10,20),heat(10,20),gheat(10,20)
     &,alam ,tlfavg(20),tgheat(20),tgvap1(20),tgvap2(20)
      common/leaf3/hwater(10,20),ghwatr(10,20)
      common/cpy1/etotwt,htot,rplnt,evapg(20),heatg(20),eave(20)
     &,have(20),waterg(20)
      common/cpy2/hsoil,hcpys,evtot,etotw(20),contot(20),scond(10,20)
     1,ihrwet(20),ecpys,cphstr,cpestr,wcpys,evsmic
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
c  calc leaf water pot from total evap,soil potent and plant res a
      etotwt=0.
      htot=0.
      tranw=0.
      rplnt=30.
      do600j=2,jtot
      evapg(j)=0.
      heatg(j)=0.
      waterg(j)=0.
      etotw(j)=0.
      do500i=1,itotp1
c  calc tot flux den per unit of leaf area
      etotwt=etotwt+gevap(i,j)
      htot=htot+gheat(i,j)
      heatg(j)=heatg(j)+gheat(i,j)
      waterg(j)=waterg(j)+ghwatr(i,j)
      evapg(j)=evapg(j)+gevap(i,j)
      etotw(j)=etotw(j)+gevap(i,j)/(alam*4.18e-03)
 500  continue
 600  if(contot(j).ge.0.)tranw=tranw+etotw(j)
      evtot=etotwt
      etotwt=etotwt/(alam*4.18e-3)
c     psilf=psisol(ihr)-tranw/rplnt
      psilf=-.1
c     write(6,*)etotwt,psilf,htot,a
      return
      end
