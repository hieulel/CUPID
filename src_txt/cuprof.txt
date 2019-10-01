c $VERSION "10/03/95 @(#)cuprof.f	7.2"
c---------
c Uncomment next line to allow the creation of 
c the probe files for plplot output.
c#define PROBE
c
      subroutine profl2(ihr,iday,iprofl,itassl,nohrs,irrchk,tirrig,
     &ettot,qtot,tiretq,taerom,taeroh,sauer,tanner,frdead)
      parameter(mh=98)
      dimension ts(50),c(50),b(50),d(50),t(50),tssave(50),e(50),ake(50)
     1,akh1(50),et1(50),q1(50),p(50),wtlast(50),ce(50),wgrav(50),dif(50)
     2,pf(50),v(50),akv(50),rhsfc(3),evsfc(3)
     3,ichkt(50),ichke(50),tlfbar(50),irrchk(mh)
     4,tgvap3(50),estnew(50)
c above 2 variables added for nrape() LMM 950105
c
c taerom,taeroh for aerodynamic temp. in above stm was added by chen,
c 03/07/90.
      common/rad4/d1(3,20),u1(3,20),bmflx(3,20),rnet(50),rndiv(50),tsfc
     1,rnlam(3,20)
      common/rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20),
     1templf(10,20),tsoil(mh)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/misc5/deltz(50),zbcpy(10),zbc(10),zabc(20)
      common/wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     1,zcrit
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),u(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
     2,akcpynut(50),akcpyold(50)
      common/prof3/ustara,ustars,phima,phims,akh(50),cp(50)
      common/prof4/psima,psims,zdla,zdls,zdlast
      common/prof5/ecap(50),qcap(50)
      common/cpy1/etotwt,htot,rplnt,evapg(20),heatg(20),eave(20)
     &,have(20),waterg(20)
      common/cpy2/hsoil,hcpys,evtot,etotw(20),contot(20),scond(10,20)
     1,ihrwet(20),ecpys,cphstr,cpestr,wcpys,evsmic
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/soil1/zsoil(25),wti(50),ndsoil,aksoil(50),cpsoil(50),tsn(50
     1)
      common/soil2/aksol(50),akw(50),cw(50),wt(mh,50),esave(3,mh),
     & wnu(mh,50),wnl(mh,50)      
c     --------------
c     | residue variables added LMM 94/9/6
c     | layer subscripts were add to the ****fc vars - MCA 5/24/95
c     | layer subscripts were add to the ****fc vars - MCA 5/24/95
      common/soil3/clodsz,sandfc(25),siltfc(25),clayfc(25),qrtzfc(25),
     & iresdu
c     --------------
c     | rock variables added to soil4 LMM 94/9/8
c     | layer subscripts were added by MCA 5/24/95
      common/soil4/pe(25),bx(25),bd(25),aks(25),an(25),ws(25),asoil(25),
     & bsoil(25),csoil(25),dsoil(25),esoil,idoroc,irocly,akrock,cprock,
     & layid(25)
      common/leaf2/evap(10,20),gevap(10,20),heat(10,20),gheat(10,20)
     &,alam ,tlfavg(20),tgheat(20),tgvap1(20),tgvap2(20)
      common/water1/iprecp,tprecp,pn(mh,50),wcond(50),wstor(50),
     & wpond(mh)
      common/water2/sw,deld,drhsfc,rhslop,drain,drain5,filt,etmm,drgrav
      common/water3/swlast
      common/time/month,jday,iyear,icumdy,timloc(mh)
      common/iterat/noiter,iter2,iter3,iterw,loope,loopt,loopw
c akroot(50) added to /root1/ by MCA - 6/12/95      
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1,rootup(50),cpytr,psisum,akroot(50)
c     common/dpevp3/evapsm(50),aprtot(50),irrchk(50,mh),iprock,twb,irrga
c     common/dpevp4/etchk2(5),qchk2(5),tirrig,drpang,drpdeg,spkev,totevp
      common/spkler/ctwa,dtwa,etwa,ftwa,gtwa,cetsum,detsum,eetsum,fetsum
     &,getsum,hetsum,cqsum,dqsum,eqsum,fqsum,gqsum,cetfix,detfix,eetfix,
     &fetfix,cqfix,dqfix,eqfix,fqfix,zmax,htspk,abspk,bspk,spkprs
     &,bdyang,tdropi
      common/inter1/wtp(20),frwet(20),frwtmx,pint(20),pilast(20)
     1,pint1(20),twater
      common /teststab/unutr  
c save vbls because of akh1,et1,q1
      save
c	--------------------
c	| Probe for tn and en
      ihrm1=ihr-1
      if(ihr.eq.1)ihrm1=mh
c#ifdef PROBE
c      call pget6(tn(ihrm1, jzcpy),tn(ihr,jzcpy),en(ihrm1,jzcpy),
c     &        en(ihr,jzcpy),iter2,1,3)
c#endif
c     routine to compute twb at top boundary using tdb & vpact
c     use tanner's a-value (constant for a given set of conditions)
      aterm=.00059
      patm=972.
      vpact=vpair(ihr)
      twb1=temair(ihr)
14    dum2=(17.2693882*twb1/(twb1+237.3))
      fofx=twb1+6.1078/(aterm*patm)*exp(dum2)-(vpact/(aterm*patm)+temair
     &(ihr))
      dum3=17.2693882*(2.*twb1+237.3)/((twb1+237.3)**2)
      fpofx=1.+(6.1078/(aterm*patm))*dum3*exp(dum2)
      twb9=twb1-fofx/fpofx
      deltck=twb9-twb1
      if(abs(deltck).lt..0005) goto 15
      twb1=twb9
c     write(8,21) twb1,tn(ihr,1),en(ihr,1)
21    format(' twb1,tn,en ',3(f12.6,1x))
      goto 14
15    twb=twb9
      iprock=0
      ihrm1=ihr-1
      if(ihr.eq.1)ihrm1=mh
      f=1.
      g=1.-f
c
      if(iday.eq.1.and.ihr.eq.1)goto46
c  if first call of profl2 use wt(,) from last hour
      if(iter2.gt.1)go to 30
      jzsfc1=jzsfc+1
      do20jz=jzsfc1,jzbot
 20   wt(ihr,jz)=wt(ihrm1,jz)
 30   continue
c
 46   continue
c ************* from alan's program : **************************
c  calc thermal properties of soil
c  thermal cond and cap from campbell
c  idepth=1 is from sfc to 1 cm.
      do260idepth=1,ndsoil
      idep=idepth+jzsfc
      aksoil(idepth)=asoil(idepth)+bsoil(idepth)*wt(ihr,idep)-
     &(asoil(idepth)-dsoil(idepth))*exp(-(csoil(idepth)*
     &wt(ihr,idep))**esoil)
      cpsoil(idepth)=2.4e6*bd(idepth)/2.65+4.18e6*wt(ihr,idep)
c     ----------
c     | If there is a rock layer, change thermal properties
c     | accordingly. LMM 94/9/8
      if (idoroc.eq.1) then
          if (idepth .ge. irocly) then
              aksoil(idepth) = akrock
              cpsoil(idepth) = cprock
          endif
      endif
c
 260  continue
c     ----------
c     | Cupid uses a homogeneous mineral soil model which
c     | is particularly wrong at the surface.  As a first move
c     | to a layered soil properties model we modify the top layers
c     | to have properties closer to humus. LMM 95/2/26
c     | Fig 8.3 and Fig 8.2 in Campbell new book is a source for
c     | the factors used below. LMM 95/3/29
c      aksoil(1)=aksoil(1)/4.0
c      cpsoil(1)=cpsoil(1)/2.0
c      aksoil(2)=aksoil(2)/4.0
c      cpsoil(2)=cpsoil(2)/2.0
c      aksoil(3)=aksoil(3)/4.0
c      cpsoil(3)=cpsoil(3)/2.0
c ********************************************************************
c  calclate soil thermal properties.
c     ----------
c     | I moved 255 loop into 260 loop :^) LMM 94/9/8
ccccc do255idepth=1,ndsoil
ccccc idep=idepth+jzsfc
c255  cpsoil(idepth)=2.4e6*bd/2.65+4.18e6*wt(ihr,idep)
      ndsm1=ndsoil-1
      do350idepth=2,ndsm1
      cp(idepth)=cpsoil(idepth)*(zsoil(idepth+1)-zsoil(idepth-1))/(2.*dt
     1)
      akh(idepth)=aksoil(idepth)/(zsoil(idepth+1)-zsoil(idepth))
 350  continue
      cp(1)=cpsoil(1)*(zsoil(2)-zsoil(1))/(dt*2.0)
      cp(ndsoil)=cpsoil(ndsoil)*(zsoil(ndsoil)-zsoil(ndsoil-1))/dt
      akh(ndsoil)=akh(ndsoil-1)
c  average cp of adjacent layers
      ndsm1=ndsoil-1
      akh(1)=aksoil(1)/(zsoil(2)-zsoil(1))
c  with first hour on first day generate initial soil temp profile
c    by using temair(24) as soil sfc b.c. and run 4 days.
      if(iday.eq.1.and.ihr.eq.1)go to 400
      go to 1000
 400  continue
      do470idepth=1,ndsoil
      tsn(idepth)=temsol(ihr)
 470  ts(idepth)=temsol(ihr)
c  lower b.c. and upper b.c. not included in matrix to be solved.
      ndsm1=ndsoil-1
      do700k=1,4
      do700i=1,nohrs
      tsn(1)=temair(i)
      do500idepth=2,ndsm1
      c(idepth)=-akh(idepth)*f
      b(idepth)=f*(akh(idepth)+akh(idepth-1))+cp(idepth)
c     write(6,*)idepth,b(idepth)
      d(idepth)=g*akh(idepth-1)*ts(idepth-1)+(cp(idepth)-g*(akh(idepth)
     1+akh(idepth-1)))*ts(idepth)+g*akh(idepth)*ts(idepth+1)
 500  continue
      d(2)=d(2)+akh(1)*temair(i)*f
      d(ndsm1)=d(ndsm1)+akh(ndsm1)*f*temsol(ihr)
      do550idepth=3,ndsm1
      c(idepth-1)=c(idepth-1)/b(idepth-1)
      d(idepth-1)=d(idepth-1)/b(idepth-1)
      b(idepth)=b(idepth)+akh(idepth-1)*f*c(idepth-1)
 550  d(idepth)=d(idepth)+akh(idepth-1)*f*d(idepth-1)
      tsn(ndsm1)=d(ndsm1)/b(ndsm1)
      ndsm2=ndsoil-2
      do600idepth=2,ndsm2
      ix=ndsm2+2-idepth
 600  tsn(ix)=d(ix)-c(ix)*tsn(ix+1)
c     write(6,1005)i,(tsn(l),l=1,ndsoil)
 1005 format(1x,i2,15f5.1)
      do650idepth=1,ndsm1
 650  ts(idepth)=tsn(idepth)
 700  continue
c  initialize soil temps to tn(mh,jz) since soil init cond is set here
c    this will go after call hite2 when soil init cond setup above
c    goes into initc subroutine.
      do710idepth=1,ndsoil
      jz=jzsfc+1+ndsoil-idepth
      idep=ndsoil-idepth+1
      tn(mh,jz)=ts(idep)
      tn(1,jz)=ts(idep)
 710  continue
 1000 continue
 1010 format(1x,15f5.2)
 1020 format(1x,15f5.1)
c  calc conductances for all canopy and air layers
c  calc ustara above cpy using previous hours stability. if
c    iday and ihr eq. 1 than start neutral.
      if(ihr.eq.1.and.iday.eq.1)psima=0.
      ilog=1
c     xlog=(refhtw-disp)/z0-psima
      xlog=(refhtw-disp)/z0
      if(xlog.le.0)write(6,1111)ilog,xlog
 1111 format(' trouble with alog in profl2',i3,1x,e12.6)
c     ustarb=.4*wind(ihr)/alog(xlog)
      ustarb=.4*wind(ihr)/(alog(xlog)-psima)
c  sum sensible heat up for all layers and assume soil same as
c    previous hour unless first hr of first day. sum  done
c    in subroutine stress.
c  calc stability corredted ustar above cpy.
c  use sen heat flux calc for above the cpyfrom profile eq's
c    for stability calculation. on first hr of first day use htot
c    without hcpys because of decoupling between soil and atmos.
c    on first time thru each hour it will use last hour value to strt
      qabove=qcond(2)
      zdlamx=.4
c  limit zdlamn to -.5 so get reasonable wind speeds at low vel.
      zdlamn=-.5
c  if no. of iterations in profl2 too large,set stability to neutral
c      if(iter2.gt.40)zdlamx=zdlast
c      if(iter2.gt.40)zdlamn=zdlast
c      if(iter2.eq.40)write(*,1023)iday,ihr
c 1023 format(' diabatic profile iteration stopped iday=',i2,' hour=',i2)
      if(iter2.gt.50)zdlamx=0.
      if(iter2.gt.50)zdlamn=0.
      if(iter2.eq.50)write(6,1023)iday,ihr
 1023 format(' profile set neutral on day=',i2,' hour=',i2)
      if(ihr.eq.1.and.iday.eq.1)qabove=htot
      zdla=-(refhtw-disp)*qabove*.00326/((temair(ihr)+273.)*ustarb**3)
      if(zdla.gt.zdlamx)zdla=zdlamx
      if(zdla.lt.zdlamn)zdla=zdlamn
      if(zdla.lt.0.)psima=1.88+(1./(-.533+.790*zdla))
      if(zdla.ge.0.)psima=-5.*zdla
c  recalc stability corrected ustar
      ilog=2
c     xlog=(refhtw-disp)/z0-psima
      xlog=(refhtw-disp)/z0
      if(xlog.le.0.)write(6,1111)ilog,xlog
c     ustara=.4*wind(ihr)/alog(xlog)
      ustara=.4*wind(ihr)/(alog(xlog)-psima)
      zdla=zdla*ustarb**3/ustara**3
      if(zdla.gt.zdlamx)zdla=zdlamx
      if(zdla.lt.zdlamn)zdla=zdlamn
      if(zdla.lt.0.)psima=1.88+(1./(-.533+.790*zdla))
      if(zdla.ge.0.)psima=-5.*zdla
      zdlast=zdla
c  calc wind above cpy
      do1030jz=1,nlabcy
      ilog=3
c     xlog=(abs(z(jz))-disp)/z0-psima
      xlog=(abs(z(jz))-disp)/z0
      if(xlog.le.0.)write(6,1111)ilog,xlog
c1030 u(jz)=ustara*alog(xlog)/.4
 1030 u(jz)=ustara*(alog(xlog)-psima)/.4
c  calc wind at cpy top
      ilog=4
c     xlog=(h-disp)/z0-psima
      xlog=(h-disp)/z0
      if(xlog.le.0.)write(6,1111)ilog,xlog
c     uh=ustara*alog(xlog)/.4
      uh=ustara*(alog(xlog)-psima)/.4
c  calc wind profile in cpy from h to zcrit using z0,disp and am
c    from daily update.
      jzflag=0
      jzcrit=0
      if(ihr.eq.1.and.iday.eq.1)psims=0.
      if(ihr.eq.1.and.iday.eq.1)zdls=0.
      do1100jz=nlabcy+1,jzsfc
      if(jzflag.ne.0)go to 1055
      if(abs(zmid(jz))-zcrit)1050,1050,1060
 1050 jzflag=1
      if(jzcrit.eq.0)jzcrit=jz-1
 1055 ilog=5
c     xlog=abs(zmid(jz))/z0soil-psims
      xlog=abs(zmid(jz))/z0soil
      if(xlog.le.0.)write(6,1112)ihr,ilog,xlog
 1112 format(' trouble with alog in profl2',2i3,1x,e12.6)
c  for short canopies set xlog=1 so u=0 at bottom of cpy
      if(xlog.le.0.)xlog=1.0001
      ilog=6
c     ylog=abs(zmid(jzcrit))/z0soil-psims
      ylog=abs(zmid(jzcrit))/z0soil
      if(ylog.le.0.)write(6,1111)ilog,ylog
c     write(6,*)jz,jzcrit,z(jz),z(jzcrit),z0soil,psims,zmid(jz)
c     u(jz)=u(jzcrit)*alog(xlog)/alog(ylog)
      u(jz)=u(jzcrit)*(alog(xlog)-psims)/(alog(ylog)-psims)
      if(u(jz).le.0.)u(jz)=0.001
      go to 1100
 1060 u(jz)=uh/(1.+am*(1.-abs(zmid(jz))/h))**2
 1100 continue
c Sun moves 'call cpyexc1' from below 1950 statement to here
c 11/25/91
c Sun add from c **** to  c *****
c Sun change the akcpy(jz) from jz=nlabcy+1 to jz=jzsfc to following 
c calculation by using call cpyexc1(iday,ihr,uh) 11/25,91
c ****
c      call cpyexc1 (iday,ihr,uh)
c     --------
c     | use new neutral only routine 94/11/1 LMM
      call cpyexc(iday,ihr,uh)
      do 1101 jz=jzcrit+1,jzsfc
      ilog=5
c     xlog=abs(zmid(jz))/z0soil-psims
      xlog=abs(zmid(jz))/z0soil
      if(xlog.le.0.)write(6,1113)ihr,ilog,xlog
 1113 format(' trouble with alog in profl2',2i3,1x,e12.6)
c  for short canopies set xlog=1 so u=0 at bottom of cpy
      if(xlog.le.0.)xlog=1.0001
      ilog=6
c     ylog=abs(zmid(jzcrit))/z0soil-psims
      ylog=abs(zmid(jzcrit))/z0soil
      if(ylog.le.0.)write(6,1111)ilog,ylog
c     write(6,*)jz,jzcrit,z(jz),z(jzcrit),z0soil,psims,zmid(jz)
c     u(jz)=u(jzcrit)*alog(xlog)/alog(ylog)
      u(jz)=u(jzcrit)*(alog(xlog)-psims)/(alog(ylog)-psims)
      if(u(jz).le.0.)u(jz)=0.001
 1101 continue
      unutr=u(jzcrit)*(alog(xlog))/(alog(ylog))
c ***** 
c  calc stability corrected ustar just above soil sfc
      zdlsmx=0.
      zdlsmn=-.5
c  if no. of iterations in profl2 too large,set stability to neutral
      if(iter2.gt.50)zdlsmx=0.
      if(iter2.gt.50)zdlsmn=0.
      if(ihr.eq.1.and.iday.eq.1)psims=0.
      if(ihr.eq.1.and.iday.eq.1)hcpys=0.
      if(ihr.eq.1.and.iday.eq.1)tn(ihr,jzcrit)=temair(ihr)
      ilog=7
c     xlog=abs(zmid(jzcrit))/z0soil-psims
      xlog=abs(zmid(jzcrit))/z0soil
      if(xlog.le.0.)write(6,1111)ilog,xlog
c     ustars=.4*u(jzcrit)/alog(xlog)
      ustars=.4*u(jzcrit)/(alog(xlog)-psims)
c Sun adds ustarsn calculatio at 12/16/91, it can be canceleed later.
      ustarsn=.4*u(jzcrit)/alog(xlog)
      if(ihr.eq.1.and.iday.eq.1)tn(ihr,jzcrit)=temair(ihr)
      zdls=-abs(zmid(jzcrit))*hcpys*.00326/((tn(ihr,jzcrit)+273.)*ustars
     1**3)
      if(zdls.gt.zdlsmx)zdls=zdlsmx
      if(zdls.lt.zdlsmn)zdls=zdlsmn
      if(zdls.lt.0)psims=1.88+(1./(-.533+.790*zdls))
      if(zdls.ge.0)psims=-5.*zdls
      ilog=8
c     xlog=abs(zmid(jzcrit))/z0soil-psims
      xlog=abs(zmid(jzcrit))/z0soil
      if(xlog.le.0.)write(6,1111)ilog,xlog
c     ustars=.4*u(jzcrit)/alog(xlog)
      ustars=.4*u(jzcrit)/(alog(xlog)-psims)
c Sun adds ustarsn calculatio at 12/16/91, it can be canceleed later.
      ustarsn=.4*u(jzcrit)/alog(xlog)
 1140 zdls=-abs(zmid(jzcrit))*hcpys*.00326/((tn(ihr,jzcrit)+273.)*ustars
     1**3)
      if(zdls.gt.zdlsmx)zdls=zdlsmx
      if(zdls.lt.zdlsmn)zdls=zdlsmn
      if(zdls.lt.0)psims=1.88+(1./(-.533+.790*zdls))
      if(zdls.ge.0)psims=-5.*zdls
c  update boundry layer resis for next iteration later. this
c    is same as rbound subroutine
c Assume dead leaves are of size sizelf/2 - these are all found in
c layers up to layer jdead.  jdead+1 has weighted avg leaf size.
c This size adjustment added by MCA 4/25/95  
      do1200j=2,jtot
        jz=nlabcy+1+jtot-j
        if(u(jz).le..01) rhleaf(j)=500.
        if(u(jz).gt..01) then
          if (j.le.jdead) then
            size = sizelf/2.
          else if (j.eq.jdead+1) then
            size = frdead*sizelf/2. + (1-frdead)*sizelf
          else
            size = sizelf
          endif        
          rhleaf(j)=180.*sqrt(size/u(jz))
        endif
1200  continue
c  for tassels divide top rh by 3 assuming tassel 1 cm dia
      if(itassl.ne.0)rhleaf(jtot)=rhleaf(jtot)/3.
c  redefine subscripts on soil vbls calculated earlier in prog.
      do1250idepth=1,ndsoil
      idep=ndsoil-idepth+1
c  redefine from larger subscripts to smaller so dont over write
      jz=jzsfc+ndsoil+1-idepth
      aksoil(jz)=aksoil(idep)
      cpsoil(jz)=cpsoil(idep)
      akh(jz)=akh(idep)
      cp(jz)=cp(idep)
      tsn(jz)=tsn(idep)
 1250 continue
c  calc conductances and phi's above canopy
      do1500jz=1,nlabcy
      if(zdla-0.)1300,1400,1400
c -0. in above stm was added by chen, 01/23/90 for SUN.
 1300 phima=1./(sqrt(sqrt(1.-15.*zdla)))
      akma=.4*ustara*(abs(zmid(jz))-disp)/phima
      akcpy(jz)=1200.*akma/phima
      go to 1500
 1400 phima=1.+4.7*zdla
      akma=.4*ustara*(abs(zmid(jz))-disp)/phima
      akcpy(jz)=1200.*akma
 1500 continue
c  calc conductances and phi's at soil sfc
      go to 1701
c------------------------------------------------------------------------
c NOTE: none of the code from here to 1701 is reachable.  I.E. this never
c       gets used.    LMM 94/9/6
c
      do1700jz=jzcrit,jzsfc
      if(zdls-0.)1600,1650,1650
c -0. in above stm was added by chen, 01/23/90 for SUN.
 1600 phims=1./sqrt(sqrt(1.-15.*zdls))
      akms=.4*ustars*abs(zmid(jz))/phims
c Sun adds ustarsn calculatio at 12/16/91, it can be canceleed later.
      akmsn=.4*ustarsn*abs(zmid(jz))
      akcpy(jz)=1200.*akms/phims
c Sun adds ustarsn calculatio at 12/16/91, it can be canceleed later.
      akcpynut(jz)=1200.*akmsn 
c     if ((iday.eq.11).and.(ihr.eq.26)) then
c     write (16,8000) jz,zdls,phims,zmid(jz),akms,akcpy(jz)        
c     else
c     end if
      go to 1700
 1650 phims=1.+4.7*zdls
      akms=.4*ustars*abs(zmid(jz))/phims
      akmsn=.4*ustarsn*abs(zmid(jz))
c Sun adds ustarsn calculatio at 12/16/91, it can be canceleed later.
      akcpynut(jz)=1200.*akmsn 
      akmsr=akms
c temp. fix for soil sfc turb.
c  clodsz is soil clod size that is read in
c Sun change akmst=1./(180.*sqrt(clodsz/u(jzcrit))) to 
c            akmst=(z(jzsfc+1)-z(jzsfc))/(180.*sqrt(clodsz/u(jzcrit)))
      akmst=(z(jzsfc+1)-z(jzsfc))/(180.*sqrt(clodsz/u(jzcrit)))
      if(akmst.gt.akms)akms=akmst
      akcpy(jz)=1200.*akms
c     if ((iday.eq.11).and.(ihr.eq.26)) then
c     write(16,8001) jz,zdls,phims,zmid(jz),akms,akcpy(jz),akmsr,akmst    
c     else
c     end if
8000  format ('ust',1x,i3,5(1x,e9.2))
8001  format ('st ',1x,i3,7(1x,e9.2))
 1700 continue
 1701 continue
c  calc cond from top(h) to critical level(z(jcrit))
c  Sun commentted following statement with cs on 10/9,91
cs    jcrtm1=jzcrit-1
cs    do1800jz=nlabcy+1,jcrtm1
cs    akcpy(jz)=akcpy(nlabcy)-(akcpy(nlabcy)-akcpy(jzcrit))
cs   1*(h-abs(zmid(jz)))/(h-zcrit)
cs    if ((iday.eq.11).and.(ihr.eq.26)) write (16,*)
cs   &'1800  jz=',jz,'akcpy(jz)=',akcpy(jz)
c1800 continue
c  Sun commentted above  statement with cs on 10/9,91
c  because diabatic wind profile above soil underestimates transfer
c    coeff at soil sfc,interpolate transfer coeff from cpy top
c    to soil sfc using akcpy(5) and akmst.
c Sun added the following statement for akms.(:(m*m/s)), u(jzcrit): m/s
c following fpmula is obtained by Tom soer. 11/1/91
c Sun recovered all statement with c25 and change name akcpy to akcpyold
c in order to find what is difference for akcpy from Goudrian's theory
c and john's method, that is the statements with c25. 12/18/91
c25   akms=(4.0+0.1*(u(jzcrit)*100.))/1000.
c25   akms=akms/5.
c26     akms=(4.0+0.1*(u(jzcrit)*100.))/1000.
c	if (iday.eq.1.and.ihr.eq.1) write (9,*) 'ujzcrit',u(jzcrit),'zcrit'
c    &,abs(zmid(jzcrit))
c 26    akms=akms/5.
c26d        akms=(4.0+0.1*(u(jzsfc)*100.))/1000.
c26d	akms=akms*abs(z(jzsfc))
c Sun coment above two statement again with c25 11/25/91
c Sun also comment following statement with c25
c 25  do1850jz=nlabcy+1,jzsfc
c 25   akcpy(jz)=akcpy(nlabcy)-(akcpy(nlabcy)-1200.*akms)
c 25   1*(h-abs(zmid(jz)))/(h-abs(zmid(jzsfc)))
c 26  do  1850 jz=nlabcy+1,jzsfc
c26d      do  1850 jz=nlabcy+1,jzsfc
c 26  akcpy(jz)=akcpy(nlabcy)-(akcpy(nlabcy)-1200.*akms)
c 26 1*(h-abs(zmid(jz)))/(h-abs(zmid(jzsfc)))
c  sun change above two statement to following two statements
c26d       akcpy(jz)=akcpy(nlabcy)-(akcpy(nlabcy)-1200.*akms)
c26d    1*(h-abs(zmid(jz)))/(h-abs(zmid(jzsfc)))
c     if ((iday.eq.11).and.(ihr.eq.26)) write (16,*)
c    &'1850  jz=',jz,'akcpy(jz)=',akcpy(jz),'iday=',iday
c     if ((iday.eq.2).and.(ihr.eq.26)) write (16,*)'akms=',akms,
c     if ((ihr.eq.26).and.(jz.eq.jzsfc)) write (16,*)'akms=',akms,
c    &'1850  jz=',jz,'akcpy(jz)=',akcpy(jz),'iday=',iday
c    &'akcpy(nlabcy)',akcpy(nlabcy),'zmid(jz)',zmid(jz),'h=',h,
c    &'1850  jz=',jz,'akcpy(jz)=',akcpy(jz),'iday=',iday,'jzfsc=',jzsfc
c25  1850 continue
c26  1850  continue
c26d 1850  continue
c  get heat cap of cpy + air assuming both have same change in
c    temperature in a given hour--reasonable.
      do1900jz=1,jzsfc
 1900 cpcpy(jz)=1200.
      jzcpm1=jzcpy-1
      do1920jz=nlabcy+1,jzcpy
 1920 cpcpy(jz)=cpcpy(jz)+836.*df/(z(jz+1)-z(jz))
c  calc transfer coeff for free convection if unstable. from goudriaan
c    p 114. coeff was 7.3 from goudriaan but seems high so used 2.5
c
c Sun change free=0.5*qcond(jz)**.333 to free=2.5*qcond(jz)**.333,
c 10/9,91
      do1950 jz=2,jzsfc
c Sun add following one statement if ( .....) go to 1950
      if (jz.lt.jzcrit.or.jz.gt.nlabcy+1) go to 1950
      free=0.
      if(ihr.eq.1.and.iday.eq.1)qcond(jz)=0.
      if(qcond(jz).gt.1.)free=0.5*qcond(jz)**.333
c     if ((iday.eq.11).and.(ihr.eq.26)) write (16,*)
c     if (jz.gt.nlabcy) write (9,*)
c    &'1950  jz=',jz,'akcpy(jz)=',akcpy(jz),'iday=',iday,
c    &'free=',free,'qcond=',qcond(jz)
      if(free.gt.akcpy(jz)) akcpy(jz)=free
 1950 continue
c  solve finite element equations for cpy + soil
c  get cpy k's and cp's for equations-already have them for soil
c
c  Include free convection at soil sfc
      if(qcond(jzsfc).gt.1.)free=0.5*qcond(jzsfc)**.333
      if(free.gt.akcpy(jzsfc)) akcpy(jzsfc)=free
c  sfc exch coeff from tomSauers measurements in corn in mm/sec
c     ----------
c     | Added Residue code, residue is a new parameter in the input file.
c     | Added for kevin F and Murty projects.  LMM 94/9/6
c
c      sauer=(4.0+0.12*2*(u(jzcrit)*100.))
      if (iresdu.eq.1) then
c       -----------------
c       | This is a simplified empirical fit to rough surface data
        sauer=(4.0+0.12*(u(jzcrit)*100.))
      else
c	-----------------
c       | This is a simplified empirical fit to smooth plate data
        sauer=(4.0+0.07*(u(jzcrit)*100.)) 
      endif     
      if (iresdu.eq.0) then
          akcpy(jzsfc) = sauer*1.2*(z(jzsfc+1)-z(jzsfc))
	  tanner = 0.0
      else
c
c         ----------
c         | If there is residue modify
c         | two layers near the surface
          akcpy(jzsfc-1) = sauer*1.2*(z(jzsfc)-z(jzsfc-1))
c
c         ----------
c         | sfc exch coeff for corn residue from Tanner1986 in mm/sec
          tanner=(2.29+0.02977*(u(jzcrit)*100.))
          akcpy(jzsfc) = tanner*1.2*(z(jzsfc+1)-z(jzsfc))
c
      endif
c
c     --------------
c     | set the minimum akcpy to sauer value 94/11/1 LMM
      do 1998 jz=nlabcy+1,jzsfc-1
1998	  if (akcpy(jz).lt.sauer) akcpy(jz)=sauer
	      
c
      do2000jz=2,jzsfc
      akh(jz)=akcpy(jz)/(z(jz+1)-z(jz))
      if(ihr.eq.1.and.iday.eq.1)akh1(jz)=akh(jz)
      akh(jz)=(akh(jz)+akh1(jz))*.5
      ake(jz)=.64*akh(jz)
      akh1(jz)=akh(jz)
 2000 cp(jz)=cpcpy(jz)*(z(jz+1)-z(jz-1))/(2.*dt)
      akh(1)=akcpy(1)/(z(2)-z(1))
      if(ihr.eq.1.and.iday.eq.1)akh1(1)=akh(1)
      akh(1)=(akh(1)+akh1(1))*.5
      ake(1)=.64*akh(1)
      akh1(1)=akh(1)
      cp(1)=1200.*(z(2)-z(1))/dt
c  get heat and water source distribution
c **************** call dpe ****************************************
      tirrig=twb
      do2017jz=1,nlabcy
      et(jz)=0.
2017  q(jz)=0.
      if(irrchk(ihr).eq.1) then
      call dpe(precip(ihr),ihr,tirrig,nlabcy,dt,ettot,qtot)
c  tirrig is calc from empirical eqs in sub dpe and result not too
c    good for gradiants. thus calc tirrig by setting qtot+ettot=dpstor
c    and solving for tirrig. on testing on iday=215 ihr=14 on allens
c    rog farm input, tirrig was too high by 1 deg c when sprinkler was
c    set at 2.6 m (or top of cpy) when empirical eqs were derived
c    for sprinkler ht 2.6 m above cpy. **however** the tirrig temps
c    computed from below are systematically lower than values from
c    allens dpevap subr and ener bal closure worse so use susans value.
       if(precip(ihr).lt.0.0001)then
        tiretq=0.
       else
        tiretq=-(ettot+qtot)*1000.*dt/(precip(ihr)*4.1876e6)+tdropi
       endif
      endif
c *******************************************************************
c     add heat source or sink from rain being different temp from leaves
c     train=temp of rain
c     tirrig=temp of irrigation water at canopy top
      do 2030 j=2,jtot
      tsum=0.
      do 2032 i=1,itotp1
2032  tsum=tsum+templf(i,j)*frarea(i,j)
      tlfbar(j)=tsum
      if(irrchk(ihr).eq.1) goto 2035
      train=twb
      qwater(j)=pint1(j)*4187.6*(train-tlfbar(j))/dt
      twater=train
      goto 2038
2035  qwater(j)=pint1(j)*4187.6*(tirrig-tlfbar(j))/dt
      twater=tirrig
2038  continue
2030  continue
c     write(21,16)ihr,iday,irrchk(ihr),twb,tn(ihr,1),en(ihr,1),u(1)
c     write(21,2042)
2042  format(' j   tlfbar(j)   tirrig     wtp(j)    drpang  qwater(j)')
c
16    format(' ihr,iday,irrchk,twb,tn,en,u',3(i4,1x),4(f7.2,1x))
      do2020jz=nlabcy+1,jzcpy
      j=jtot+(nlabcy+1)-jz
      et(jz)=etotw(j)
c  remove et1 and q1 which represent et and q from last hour here cause
c    causes leaf ener bal source to differ from source used in newrap.
c    changed 06-06-88.
c     if(ihr.eq.1.and.iday.eq.1)et1(jz)=et(jz)
c     et(jz)=(et(jz)+et1(jz))*.5
c     et1(jz)=et(jz)
c     et(jz)=0.
c  etotw is in mg m-2 s-1
      q(jz)=heatg(j)+waterg(j)
c     if(ihr.eq.1.and.iday.eq.1)q1(jz)=q(jz)
c     q(jz)=(q(jz)+q1(jz))*.5
c     q1(jz)=q(jz)
c     write(6,*)jz,q(jz)
c     q(jz)=10.
 2020 continue
c  set q(jz) and et(jz) from jzcpy+1 to jzsfc to 0.0
      do 2021jz=jzcpy+1,jzsfc
      q(jz)=0.
 2021 et(jz)=0.
c
c     =========================================================================
c     | From here to 6000 we iterate to solve the temperature profile,
c     | water vapor profile and the liquid water profile(soil).  These three
c     | are all interrelated and so at the end of these calculations (in soilw)
c     | we check to see that surface rel hum calculated from the atmosphere 
c     | down ~= surface rel hum calculated from the soil up.  If not then it 
c     | recycles to here.  This largest loop uses the iterw counter.
c     |
c     -----------------------------------------------------------------------
c     | The first inside loop is from here to 2870 and uses iter3 as its 
c     | iteration counter.  This loop first calculates the temperature profile 
c     | using the Newton Raphson solver newrap().  With this temperature 
c     | profile it then calculates the water vapor using the newrap() solver.
c     | This loop exits when the canopy evapo-transpiration is ~= the previous
c     | value.
c     |
c     | The second inside loop is a Newton Raphson solver for soil water
c     | content.  This is performed in subroutine soilw.
c     |
      if(ihr.eq.1.and.iday.eq.1)ecpys=0.
c  recycle to here to get evap from soil sfc to converge. initialize
c    vbls to be used to average soil evap.
c#ifdef PROBE
cc      ---------
cc      | diagnostics for lfbal-cuprof feed back work 94/11/23
c       if (iter2.eq.1) then
c 	   call pend(7)
c	   call pbeg7(iday,ihr,1)
c	   do jz=1,jzsfc
c	       call pget7(akcpy(jz))
c	   enddo
c	
c       endif
c#endif
c-----
      iter3=1
      iterw=0
      ecpys1=ecpys
 2025 continue
c#ifdef PROBE
cc       --------
cc       | diagnostics for lfbal-cuprof feed back work 94/11/23
cc	--------------------
cc	| Probe for tn and en
c        call pget6(tn(ihrm1, jzcpy),tn(ihr,jzcpy),en(ihrm1,jzcpy),
c     &        en(ihr,jzcpy),iter2,iter3,iterw)
c#endif
      if(iter2.gt.1)goto2026
      if(ihr.eq.1.and.iday.eq.1)goto2026
      ihrm1=ihr-1
      if(ihr.eq.1)ihrm1=mh
      if(ihr.eq.1.and.iday.eq.1)ihrm1=ihr
      tn(ihr,jzsfc+1)=tn(ihrm1,jzsfc+1)
 2026 continue
c     ---------
c     | If there is residue, some of the energy is absorbed by the
c     | residue instead of the soil surface.  1/4 was chosen as a 
c     | reasoned guess by JMN.  LMM 94/11/23
       if(iresdu.eq.0) then
           q(jzsfc+1)=rnet(1)-ecpys-pint1(1)*4187.6*(tn(ihr,jzsfc+1)-
     &  		twater)/dt
       else
           q(jzsfc)=rnet(1)/4.
           q(jzsfc+1)=3.*rnet(1)/4.-ecpys-pint1(1)*4187.6*(tn(ihr,jzsfc
     &  		+1)-twater)/dt
       endif
c  zero all sources in soil so when layers change source still 0.0
      do 2028 jz=jzsfc+1,jzbot
      et(jz)=0.
 2028 if(jz.ge.jzsfc+2)q(jz)=0.
c
c  the heat source at the sfc is put into the top soil layer.
c    the heat cap of that top layer is the ave of the bottom cpy
c    layer and the top soil layer. the hsoil is sum of conducted
c    plus storred in the top soil layer.
c  setup up finite element eq's.
c  test of everything but k's and cp's
c     do2027jz=1,jzbot
c     akh(jz)=10.
c     cp(jz)=100.
c2027 continue
c  set up bound condit for temp and vp except for vp at soil sfc.
      en(ihr,1)=vpair(ihr)
      tn(ihr,1)=temair(ihr)
      tn(ihr,jzbot)=temsol(ihr)
      pn(ihr,jzbot)=pe(ndsoil)*(watsol(ihr)/ws(ndsoil))**(-bx(ndsoil))
c  pn can now exceed pe: MCA - 6/13/95
c      if(pn(ihr,jzbot).ge.pe(ndsoil))pn(ihr,jzbot)=pe(ndsoil)
      wt(ihr,jzbot)=watsol(ihr)
c  b.c. for infiltration and evap are set later.
      if(ihr.eq.1.and.iday.eq.1)go to 2050
c  set t(jz) to profil from last hour
      ihrm1=ihr-1
      if(ihr.eq.1)ihrm1=mh
      do2150jz=1,jzbot
      e(jz)=en(ihrm1,jz)
 2150 t(jz)=tn(ihrm1,jz)
      do2170jz=jzsfc,jzbm1
      p(jz)=pn(ihrm1,jz)
c  only reset pn(,) on first call of profl2, on later calls use value
c    of pn(,) from previous call instead previous hour
 2170 if(iter2.eq.1.and.iterw.eq.0)pn(ihr,jz)=p(jz)
      do2175jz=jzsfc1,jzbm1
      wt(ihr,jz)=wt(ihrm1,jz)
 2175 wtlast(jz)=wt(ihrm1,jz)
      go to 2250
 2050 do2100jz=1,jzsfc
      e(jz)=vpair(ihr)
      en(ihr,jz)=e(jz)
      tn(ihr,jz)=temair(ihr)
 2100 t(jz)=temair(ihr)
c  define en(ihr,jzsfc) for pn(,) b.c. on soil on first pass
      en(ihr,jzsfc)=vpair(ihr)
      jzbm1=jzbot-1
      jzsfc1=jzsfc+1
c  wti(jz) defined in main program on first pass.
      do2200jz=jzsfc1,jzbot
      jz1 = jz-jzsfc
c     write(6,*)ihr,jz,wt(ihr,jzbot),p(jz),wti(jz)
      p(jz)=pe(jz1)*(wti(jz)/ws(jz1))**(-bx(jz1))
c  p can now exceed pe - MCA 6/13/95
c      if(p(jz).gt.pe(jz1))p(jz)=pe(jz1)
      if(iter2.eq.1.and.iterw.eq.0)pn(ihr,jz)=p(jz)
      wtlast(jz)=wti(jz)
      wt(ihr,jz)=wti(jz)
      wt(ihrm1,jz)=wti(jz)
      e(jz)=0.
      en(ihr,jz)=0.
      tn(ihr,jz)=tsn(jz)
 2200 t(jz)=tsn(jz)
c  calc initial stored water
      jzsfc2=jzsfc1+1
      swlast=wti(jzsfc1)*(z(jzsfc2)-z(jzsfc1))/2.
      do2210jz=jzsfc2,jzbm1
 2210 swlast=swlast+wti(jz)*(z(jz+1)-z(jz-1))/2.
      swlast=swlast*1000.
 2250 continue
c  newton-raphson equations for temp
c  begin solution of temperature profiles for cpy and soil together
c    using newton-raphson described in campbells book(1985)
c  deldt is sum of absolute values of flux errors at all nodes in wm-2
      deldt=.001
      do2540jz=1,jzbot
 2540 tcheck(jz)=100.
c
c=======================================================================
c                                                    DA BIG DIAGNOSTIC
c used for lfbal-cuprof feedback work LMM 94/11/23
c
c      if (iday.eq.2 .and. ihr.eq.15) then
c
c1121    format(F7.2,6(',',F6.2),',',F8.2)
c1122    format(I2,8(',',F6.2))
c1123    format(I3,I3,I3,I3)
c	write(23,1123)iday,ihr,iter2,iter3
c        write(23,*)
c     &'-----------------------------------',
c     &'----------------------------PROFL2-'
c        write(23,*)
c     &'  iday   ihr   iter2  iterw  iter3',
c     &'  deldt  jzsfc      dt '
c        write(23,1121)
c     &  iday,  ihr,  iter2, iterw, iter3,deldt,jzsfc,dt
c        write(23,*)
c     &'-----------------------------------',
c     &'-----------------------------------'
c	write(23,*)'before'
c	write(23,*)
c     &'     tn     tn-1  akh     cp   deltz     q   tcheck ichkt'
c	do jz=1,jzbot
c	write(23,1122)
c     &jz,tn(ihr,jz),tn(ihrm1,jz),akh(jz),cp(jz),deltz(jz),q(jz),
c     &tcheck(jz),ichkt(jz)
c	enddo
c      121 34567 2 34567 3 34567 4 34567 5 34567 6 34567 7 34567
c      1 3456 2 3456 3 3456 4 3456 5 34567
c      6 3456 7 3456 8 3456 9 3456 0 34567
c
c      endif
c
c=======================================================================
c
      loopt=0
c      call newrap(0,1,jzbot,deltz,akh,cp,tn,q,deldt,tcheck,ihr,ihrm1
c     &,loopt,dt,ichkt,iday)
c     ----------
c     | new rap changed to accomodate source dependence on tleaf-tn
c     | LMM JMN 94/11/8
      call nrapt(0,1,jzbot,deltz,akh,cp,tn,q,deldt,tcheck,ihr,ihrm1
     &,loopt,dt,ichkt,iday,tgheat,jtot,nlabcy,jzcpy,waterg,tlfavg)
c     ----------
c     | diagnostic output used for lfbal-cuprof feed back work 
c     | LMM 94/11/23
c      if (iday.eq.2 .and. ihr.eq.15) then
c	write(23,*)'after'
c	write(23,*)
c     &'    tn     tn-1  akh     cp   deltz     q   tcheck ichkt'
c	do jz=1,jzbot
c	write(23,1122)
c     &jz,tn(ihr,jz),tn(ihrm1,jz),akh(jz),cp(jz),deltz(jz),q(jz),
c     &tcheck(jz),ichkt(jz)
c	enddo
c      121 34567 2 34567 3 34567 4 34567 5 34567 6 34567 7 34567
c	endif
c
c
c     if(iday.eq.2)write(6,*)(tn(ihr,l),l=1,jzbot),eref,t
c  calc vp at soil surface, depends on soil sfc temp so not fixed
      tref=tn(ihr,jzsfc+1)
      eref=6.108d0*10**(7.5d0*tref/(237.3d0+tref))
c  calc evap from soil sfc from 2 sfc rel hum values to get slope of
c    rhsfc() vs sfc evap to use in newton-raphson iteration over soil
c    water.
      rhsfc(2)=exp(pn(ihr,jzsfc+1)*.018/(8.314*(tref+273.)))
      rhsfc(1)=rhsfc(2)-rhslop
      if(rhsfc(1).lt.0.01)rhsfc(1)=.01
c
c     --------------------------------
c     | This loop is done twice because
c     | it is calculating evap of surface = f(humidty of the surface)
      do3620irh=1,2
      en(ihr,jzsfc+1)=eref*rhsfc(irh)
      if(ihr.eq.1.and.iday.eq.1)e(jzsfc+1)=en(ihr,jzsfc+1)
c  newton-raphson equations for vapor pressure
c  conversion for correct conversion for eddy diffusivity for
c    water vapor is 770/1200=.64 if vapor flux in mg m-2 s-1 and
c    heat in w m-2.
c    etotw in mg m-2 s-2     akh-j m-2 s-1 k-1  =(m+2 s-1)*1200
c    specific cap for water=770.   ake-mg m-2 s-1 mb-1=(m+2 s-1)*770
c
c  calc water capacity of the air
      do3460jz=2,jzsfc
      ce(jz)=770.*(z(jz+1)-z(jz-1))/(2.*dt)
 3460 continue
c temporary fix for ce (2-20-87)
      ce(1)=ce(2)
      do3470jz=1,jzsfc1
c     if (iday.eq.8.and.ihr.eq.10) write (6,*) tn(ihr,jz),' iday',iday,
c    &' ihr',ihr,' jz',jz
 3470 esat(jz)=6.108*10**(7.5*tn(ihr,jz)/(237.3+tn(ihr,jz)))
c
cbVP
      do3475,jz=nlabcy+1,jzcpy
	  j=jtot-(jz-(nlabcy+1))
c         ----------
c 	  | calculate new sat. vp of air using new
c	  | air temp from nrapt()
	  estnew(jz)=6.108*10**(7.5*tn(ihr,jz)/(tn(ihr,jz)+237.3))
	  tgvap3(jz)=tgvap2(j)*(tlfavg(j)-tn(ihr,jz))
 3475 continue
ceVP
      loope=0
c      call newrap(1,1,jzsfc1,deltz,ake,ce,en,et,deld,esat,ihr,ihrm1,loop
c     &,dt,ichke,iday)
      call nrape(1,1,jzsfc1,deltz,ake,ce,en,et,deld,esat,ihr,ihrm1,loop
     &,dt,ichke,iday,jtot,nlabcy,jzcpy,estnew,tgvap1,tgvap3)

      do 3618 jdum=1,jzsfc
      if(en(ihr,jdum).lt.0.0) then
        write(6,*)'en(ihr,j) < 0 on early iteration--PROBABLY ',   
     &'BECAUSE WIND TOO LOW '
        go to 3619
      endif
 3618 continue
 3619 continue
c  calc soil sfc evap in mg m-2 s-1
      evsoil=ake(jzsfc)*(en(ihr,jzsfc+1)-en(ihr,jzsfc))
      evsfc(irh)=evsoil
 3620 continue
      rhev=(evsfc(1)-evsfc(2))/(rhsfc(1)-rhsfc(2))
      rhsoil=rhsfc(2)
c     write(17,*)rhev,(evsfc(l),l=1,2),(rhsfc(l),l=1,2)
c  second time thru irh loop gives evsoil as actual soil evap
c  calc soil sfc evap in w m-2
      ecpys=alam*4.18e-3*evsoil
c  check whether soil evap is stable.
      if(abs(ecpys-ecpys1)-2.)2870,2870,2810
 2810 continue
      iter3=iter3+1
      if(iter3.gt.50)write(6,2815)ihr,iter3,ecpys,ecpys1
 2815 format(' ihr=',i3,' iter3=',i3,2e12.5)
      if(iter3.gt.50)go to 2870
      ecpys=(ecpys+ecpys1)*.5
      ecpys1=ecpys
c     write(19,*)ihr,ecpys,eref,tref,en(ihr,jzsfc),pn(ihr,jzsfc)
c    1,en(ihr,jzsfc+1),pn(ihr,jzsfc+1)
c    3,ake(jzsfc),akw(jzsfc+1)
c     write(17,*)(en(ihr,jjj),jjj=1,17),(tn(ihr,kkk),kkk=1,17)
c    1,(pn(ihr,lll),lll=14,20)
      go to 2025
 2870 continue
c
c     | END OF INSIDE LOOP STARING AT 2025 (temp and water vapor profiles)
c     ---------------------------------------------------------------------
c  calc all cpy water fluxes
      do2849jz=1,jzbot
 2849 econd(jz)=0.
      do2850jz=1,jzsfc
      econd(jz)=alam*4.18e-3*ake(jz)*(en(ihr,jz+1)-en(ihr,jz))
 2850 continue
c  convert v.p. back to absolute value instead of dif from eref
c    and save 3 soil values of e() ad differences from eref
      esave(1,ihr)=en(ihr,jzsfc+1)
      esave(2,ihr)=en(ihr,jzsfc+2)
      esave(3,ihr)=en(ihr,jzsfc+3)
c     do2860jz=1,jzsfc
c     en(ihr,jz)=en(ihr,jz)+eref
c2860 e(jz)=e(jz)+eref
c  set tn(jx) equal to tair(j) and en(jx) to eair(j)
      do2700jz=nlabcy+1,jzcpy
      j=jtot+nlabcy+1-jz
c  note tn,en and tair,eair are all at top of layers. in energy
c    budget subroutine and net rad calc should use averages of these
c    between adjacent layers.
      tair(j)=tn(ihr,jz)
      eair(j)=en(ihr,jz)
 2700 continue
c  value of ijhrm1 set previously at hsoil calculation
      ijhrm1=ihr-1
      if(ihr.eq.1)ijhrm1=mh
      if(ihr.eq.1.and.iday.eq.1)ijhrm1=1
      hsoil=akh(jzsfc+1)*f*(tn(ihr,jzsfc+1)-tn(ihr,jzsfc+2))
     1+cp(jzsfc+1)*(tn(ihr,jzsfc+1)-tn(ijhrm1,jzsfc+1))
      hcpys=-akh(jzsfc)*f*(tn(ihr,jzsfc)-tn(ihr,jzsfc+1))
c     if(ihr.eq.20)write(25,2910)ihr,en(ihr,jzsfc+1),hcpys,ecpys,hsoil
c    1,tn(ihr,jzsfc+1),rnet(1)
c     if(ihr.eq.20)write(25,2910)ihr,templf(itotp1,2),tsfc,ecpys1
c     if(ihr.eq.20)write(25,2910)iter2,zdla,zdls,ustara,ustars,akcpy(5),
c    1akcpy(jzcrit)
 2910 format(' ihr=',i3,6(1x,e10.4))
c
c  finite element eqs for soil water
c
      call soilw(ihr,ihrm1,iday,eref,tref,evsoil,rhsoil,rhev,rhsfc,
     &           irecyc,wgrav)
      if (irecyc.eq.1) go to 2025
 6000 continue
c
c     | END OF MAIN LOOP THAT STARTS AT 2025 (temp, vapor, and soil water profs)
c     =========================================================================
c
      cphstr=0.
      cpestr=0.
      jzbm1=jzbot-1
      do2900jz=1,jzbm1
      qcond(jz)=akh(jz)*(tn(ihr,jz+1)-tn(ihr,jz))
      qcap(jz)=cp(jz)*(tn(ihr,jz)-tn(ijhrm1,jz))
      if(jz.gt.jzsfc)wcond(jz)=akw(jz)*(pn(ihr,jz+1)-pn(ihr,jz))-wgrav
     &(jz)
      if(jz.gt.jzsfc)go to 2880
c  water vapor fluxes in cpy were calc'd eralier
      ecap(jz)=ce(jz)*(en(ihr,jz)-en(ijhrm1,jz))
      cphstr=cphstr+qcap(jz)
      cpestr=cpestr+ecap(jz)
      go to 2900
 2880 continue
      wcond(jzbot)=wcond(jzbm1)
      qcond(jzbot)=qcond(jzbm1)
c     if(ihr.eq.20)write(25,2890)jz,econd(jz),et(jz),en(ihr,jz),akh(jz)
c    1,tn(ihr,jz),q(jz),qcond(jz)
c     if(ihr.eq.20)write(25,2890)jz,u(jz),rhleaf(jz),cphstr,cpestr
c    1,templf(itotp1,jz)
 2890 format(i3,e10.4,6(1x,e10.4))
c     if(ihr.eq.21)write(25,2890)jz,u(jz),akcpy(jz)
 2900 continue
	tsoil(ihr)=tn(ihr,jzsfc+1)
c above stm was added by chen, 01/19/90.
c     write(19,5100)ihr,wcpys,ecpys,delstr,en(ihr,jzsfc+1),en(ihr,jzsfc
c    1),tn(ihr,jzsfc+1),tn(ihr,jzsfc),pn(ihr,jzsfc+1),pn(ihr,jzsfc)
 5100 format(1x,i2,6e12.5,/,3e12.5)
c     write(22,*)tn
c chen, 03/07/90.
c ** beginning of calculation of aerodynamic canopy temperature ********
c taerom: temperature calculated based on roughness length z0.
c taeroh: temperature calculated based on roughness length z0h.
	jz=1
	z0h=z0/(2.72*2.72)
	if(qcond(jz).lt.0.1) then 
	    zdl=0.0e-6
        else
c	    bowen=qcond(jz)/econd(jz)
	    tstara=qcond(jz)/1200./ustara
	    obukhv=-ustara*ustara*(tn(ihr,jz)+273)
     1         /.4/9.8/tstara
c       | Got rid of minor boyancy correction 94/11/2 LMM (JMN)
c     1         /.4/9.8/tstara/(1+.07/bowen)
	    zdl=abs(z(jz))/obukhv
	endif
c	if(zdl.gt..4) zdl=.4
c	if(zdl.lt.-.5) zdl=-.5
	psih=-5*zdl
	if(zdl.lt.0.)psih=exp(.598+.390*alog(-zdl)-.09*(alog(-zdl))**2)
	xlogm=alog((abs(z(jz))-disp)/z0)-psih
	taerom=tn(ihr,jz)+(tstara/.4)*xlogm
	if (taerom.lt.-30..or.taerom.gt.70) taerom=999.99
	xlogh=alog((abs(z(jz))-disp)/z0h)-psih
	taeroh=tn(ihr,jz)+(tstara/.4)*xlogh
	if (taeroh.lt.-30..or.taeroh.gt.70) taeroh=999.99
c	write(6,2931)jz,z0,z0h,disp,z(jz),tn(ihr,jz),qcond(jz),econd(jz),
c     1  bowen,ustara,tstara,obukhv,zdl,psih,taerom,taeroh
c2931	format(/'jz     z0    z0h   disp      z     tn  qcond  econd'
c     1  ,'  bowen ustara tstara obukhv    zdl   psih taerod'/i2,2f7.3,
c     1  f7.2,f7.2,f7.2,2f7.1,f7.1,2f7.3,f7.1,f7.2,f7.3,f7.2)
c ** end of calculation of aerodynamic canopy temperature taerod *******
c  print out soil sfc exchange coeff in mm/sec
c      write(16,*)'sauer',sauer,' u(jzcrit) ',u(jzcrit),' akcpy(jzsfc) '
c     &,akcpy(jzsfc)/1.2,' ihr ',ihr,' day ',icumdy,' z(jzsfc) ',
c     &z(jzsfc),'wind(ihr) ',wind(ihr),' free ',free/1.2
      return
c     debug trace
c     at 1000
c     trace on
      end
c
c     -----------------------------------------------------------------
c     |						             CPYEXC1
c     | This routine is derived from "Crop
c     | Micrometeorology: a simulation study" by
c     | J Gourdiaan p 108-114.
c
c     | This routine can cause stability to flip-flop from time-
c	to time-step and this causes the wind profile to change
c	dramatically so leaf blr can go from 100 to 500 on successive
c	iterations and the iter2 iteration loop will not converge.
c	This is caused by the if statement on xi when canopy is 
c	stable ri>0.
c	For some reason xi can be negative when ri>0 and this
c	should not be possible but it can happen with the eqn.
c	This seems a problem with the Goudriaan model. 11/1/94
      subroutine cpyexc1 (iday,ihr,uh)         
      parameter(mh=98)
      dimension phihc(50),phimc(50),unut(50)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     1,zcrit
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),u(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
     2,akcpynut(50),akcpyold(50)
      common/prof4/psima,psims,zdla,zdls,zdlast
      itercpy=50
      iter=0
c     write (9,51)
c 51  format (1x,'total lai   ','h',11x,'sizelf',6x,'uh',10x,'ihr',3x,
c    &'iday',2x,'nlabcy',1x,'jzsfc')
c     write (9,52) totlai,h,sizelf,uh,ihr,iday,nlabcy,jzsfc
c 52  format (1x,4(e11.4,1x),4(i5,1x))
c     write (9,53)
c 53  format (1x,'jz=',1x,'z',11x,'u',11x,'tn',10x,'qcond')
c     do 55 jz=nlabcy+1,jzsfc
c     write (9,54) jz,z(jz),u(jz),tn(ihr,jz),qcond(jz)
c 54  format (1x,i4,4(1x,e11.4))
c 55  continue
      xld=totlai/h
      xlm=sqrt(4.*sizelf/(3.14*xld))   
      aenut=sqrt(0.2*totlai*h/(2.*xlm*0.5))
      ae1=aenut
      uaver=uh*(1.-exp(-ae1))/ae1
      ntop=nlabcy+1
      nbot=jzsfc+1
      avegrdt=(tn(ihr,ntop)-tn(ihr,nbot))/abs(z(ntop)-z(nbot))
      taver=0.
      do 5 jz=nlabcy+1,jzsfc
      taver=taver+tn(ihr,jz)*abs(z(jz)-z(jz+1))/h
  5   continue
      taver=taver+273.1
      ri1=9.8*avegrdt*xlm*xlm/taver
c     write(9,*)
c    &'xld,xlm,aenut'
c    &,xld,xlm,aenut	
      if (ri1.gt.0) then
2         uaver=uh*(1.-exp(-ae1))/ae1
          ri=ri1/uaver/uaver
          xi=(0.74*(1.+8.926*ri)**0.5+2.*4.7*ri-0.74)/
     &			  (2.*4.7*(1.-4.7*ri))
          if ((xi.gt.1).or.(xi.le.0.)) xi=1.
          avephimc=1.+4.7*xi
          ae=aenut*avephimc**0.5
          if (abs((ae-ae1)/ae).le.0.1) go to 10
          ae1=ae
          iter=iter+1
          if (iter.gt.itercpy) go to 100
          go to 2
      else
3         uaver=uh*(1.-exp(-ae1))/ae1
          ri=ri1/uaver/uaver
          xi=ri
          if(xi.le.-1.) xi=-1.
          avephimc=(1.-15.*xi)**(-0.25)
          ae=aenut*avephimc**0.5
          if (abs((ae-ae1)/ae).le.0.1) go to 10
          ae1=0.5*(ae1+ae) 
          iter=iter+1
          if (iter.gt.itercpy) go to 100
          go to 3
      end if
10    do 15 jz=nlabcy+1,jzsfc
      u(jz)=uh*exp(-ae1*(1.+z(jz)/h))
      unut(jz)=uh*exp(-aenut*(1.+z(jz)/h))
15    continue
c     write (19,*)'iday,ihr',iday,ihr,' uh=',uh,' u=',
c    &(u(jz),jz=jzcrit,jzsfc),jzcrit,' * ',jzsfc
c     write (19,*)'iday,ihr',iday,ihr,'uh=',uh,'unut=',
c    &(unut(jz),jz=jzcrit,jzsfc),jzcrit,' * ',jzsfc
      do 11 jz=nlabcy+1,jzsfc
      cr1=9.8*xlm*xlm*(tn(ihr,jz)-tn(ihr,jz+1))/(
     &(tn(ihr,jz)+273.2)*(z(jz+1)-z(jz)))
      cr2=1./(u(jz)*u(jz))
      ri=cr1*cr2
c     if (ihr.eq.15) write (9,*)'***ihr*** ',ihr,'jz=',jz
c     if (ihr.eq.15) write (9,*)
c    &'cr1,cr2,ri,tn(jz),tn(jz+1)',cr1,cr2,ri,tn(ihr,jz),tn(ihr,jz+1)
      if((tn(ihr,jz)-tn(ihr,jz+1)).gt.0.) then
          co1=.74*(1.+8.926*ri)**.5+2.*4.7*ri-.74
          co2=2.*4.7*(1.-4.7*ri)
          xi=co1/co2
          if ((xi.gt.1).or.(xi.le.0.)) xi=1.
c         write (9,*) 'ri and xi in stable',ri
          phimc(jz)=1.+4.7*xi
          phihc(jz)=1.+4.7*xi
c         if (ihr.eq.15) write (9,*)
c    &        'st xi=',xi,'phihc=',phihc(jz),'phimc=',phimc(jz)
      else 
          xi=ri
          if(xi.le.-1.) xi=-1.
          phimc(jz)=(1.-15.*xi)**(-0.25)
          phihc(jz)=(1.-15.*xi)**(-0.5)
      end if
11    continue
      do 25 jz=nlabcy+1,jzsfc
      akcpy(jz)=xlm*0.5*u(jz)/phihc(jz)  
      akcpynut(jz)=xlm*0.5*unut(jz)  
c     write(9,*)'25 ihr=',ihr,'jz=',jz,'u=',u(jz),'phihc=',phihc(jz),
c    &'akcpy=',akcpy(jz),'xlm=',xlm
25    continue
      do 30 jz=nlabcy+1,jzsfc
      free=0.
      if(ihr.eq.1.and.iday.eq.1)qcond(jz)=0.
      if(qcond(jz).gt.1.)free=(0.5*xlm*xlm*3.0/0.74)**(2./3.)
     &*(9.8/((tn(ihr,jz)+273.2)*1200))**1./3*qcond(jz)**1./3.
      if (free.gt.akcpy(jz)) akcpy(jz)=free
      akcpy(jz)=akcpy(jz)*1200.
      akcpynut(jz)=akcpynut(jz)*1200.  
      if (iday.eq.5.and.ihr.eq.24) 
     &write(19,*)'jz=',jz,'u=',u(jz),'phihc=',phihc(jz),
     1'akcpy=',akcpy(jz),'free=',free
  30  continue
      go to 110
c     write (6,*) 'leave subroutine cpyexc'
 100  write (6,*)'stop in Subr. cpyexc because iteration time exceeds'
      stop
 110  return
      end
c **************************************************************
c **************************************************************
c     This subroutine is attached to the end of original
c file 'cuprof.f'.  This subroutine is for neutral conditions only.
c This routine is called to avoid problems with sub cpyexc1 related to
c stability. 94/11/1              
      subroutine cpyexc (iday,ihr,uh)         
      parameter(mh=98)
      dimension phihc(50),phimc(50)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     1,zcrit
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),u(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
     2,akcpynut(50),akcpyold(50)
c
      xld=totlai/h
      xlm=sqrt(4.*sizelf/(3.14*xld))   
      aenut=sqrt(0.2*totlai*h/(2.*xlm*0.5))
      ae1=aenut
  1   do 5 jz=nlabcy+1,jzsfc
      u(jz)=uh*exp(-ae1*(1.+z(jz)/h))
  5   continue
      do 25 jz=nlabcy+1,jzsfc
      akcpy(jz)=xlm*0.5*u(jz)  
      akcpy(jz)=akcpy(jz)*1200.
c     write(9,*)'25 ihr=',ihr,'jz=',jz,'u=',u(jz),'phihc=',phihc(jz),
c    &'akcpy=',akcpy(jz),'xlm=',xlm
  25  continue
      return
      end
c  ************************************************************
