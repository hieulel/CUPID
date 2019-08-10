c $VERSION "08/16/95 @(#)cusub.f	7.1"
c-------------------
c Uncomment this define to receive
c probe files for use in plplot.
c#define PROBE
c
c  program cupid which is main program for whole canopy model
c
c  cupid1    photosynthesis model with light and temp for potential yiel
c  cupid2    energy balance model with profiles of tair using
c               simplified fit in canopy, plus reflect of canopy from
c              various zenith view angles for vis,nir and thermal.
c  cupid3    bidirectional cpy reflectance
c-----------------------------------------------------------------------
c						      SUBROUTINE CUPIDG2
cBGb
	subroutine cupidg2(invrgl,igl,ihrgl,isrcgl,xx,
     1  tcpyap,ts,psn,kcumdy,isw1)
c
        integer isw1,kcumdy,invrgl,igl,ihrgl,isrcgl
        real*8 xx(30),ts(75),psn(75),eairgl,windgl,psisgl,tairgl
        integer nfilerun,daymax
        common /input/ nfilerun
cBGe
c              various zenith view angles for vis,nir and thermalo
	parameter(mh=98,daymax=367,MAXPRB=10)
c       ----------
c       | used for tcl/tk/plplot "probe" graphing 
        integer         pbfile(MAXPRB)
        integer         pbcnt(MAXPRB)
        integer         pbfrst(MAXPRB)
        common/probe/pbfile,pbcnt,pbfrst
c
cxxxxxxxxxxxxxx cuprad insert begin 1 xxxxxxxxxx
      dimension idayx(12),ihrx(12,24),nhrs(12)
cBG
	  dimension yenang(98),yunazm(98)
cxxxxxxxxxxxxxx cuprad insert end 1 xxxxxxxxxxxx
      dimension ipnt(99),jpnt(99),kpnt(99),lpnt(99),ihrpnt(mh)
     &,ispn(mh),theta(10),temlf1(10,20),radsol(mh)
     &,wnd(20),tlfave(20),tcpyir(9,50),tcpyap(9,50),emiscp(9,50)
     &,timsun(mh),tair1(20),eair1(20),rsave(20),qdiv(50),ediv(50)
     &,irang(9),rsum3(3),radlay(3,9),radlf(3,9)
     &,smtair(20),smeair(20),tairx(100,20),eairx(100,20)
     &,iwetam(20),iwetpm(20),vpdin(mh)
     &,ichkt(50),ichke(50),ickout(50),tprecd(mh),irrchk(mh)
     &,evapsm(daymax),aprtot(daymax),mday(daymax),relhum(mh)
     &,dvpdsf(10,50),freqr(20),akh2(50),akh3(50),distz(50),akh2nut(50)
      	dimension path(mh),radtsv(3,mh),fbm1sv(3,mh),rhocpy(3)
	real	slope
	real	aspect
	real	hrzang
c	---------
c	| The following added to take into account undulating terrain
c	|
c	| path    - beam path through canopy
c	| slope   - inclination of slope from horizontal
c	| hrzang  - inclination of horizon from horizontal
c	| aspect  - azimuth of normal to slope 0=North
c	|
c	|  LMM 25/8/93
c
c Sun added the aray of akh2(50) on 10/30/91
c original radabv(3), tbeam(3), wtir(20,9), sunazm(mh) were removed from
c dimension stm by Chen, 8/31/89.
c tcpyir(5) was changed to tcpyir(9,50), and tcpyap(9,50), emiscp(9,50)
c were added in dimension stm. Chen, 05/02/90.
c
c solar(150),vpave(150),wrun(150),potsol(150),temmax(150),temmin(150),
c prcpt(150),soilt(150),jcumdy(150),sinalp(9),cosalp(9),cotsmb(36),
c nthetv(9),cthetv(9),sthetv(9),bidir(4,9,36),xdata(12,mh)
c in dimension stm were removed, c since they were never used. 
c chen, 02/20/90.
c
      character*22 filein,filout,file15,file16,file19
      character*80 title1,title2
c
      logical*4 extat,optat,nmtat
      character*22 filnam
c
      common/parabola/avisr,bvisr,cvisr,anirr,bnirr,cnirr
     &               ,avist,bvist,cvist,anirt,bnirt,cnirt
      common/balan/dsdum(3,20),dstng(20),dstng2(20)
      common/daily/tlai(daymax),ht(daymax),arotin(daymax),
     &	zldh1(daymax),zmdh1(daymax),tsbc(daymax),wsbc(daymax)
     &,frlive(daymax)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
      common /rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20)
     &,templf(10,20),tsoil(mh)
      common /rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),hfday
     &,ratiod,ration,ratio(mh)
      common /rad4/d(3,20),u(3,20),bmflx(3,20),rnet(50),rndiv(50),tsfc
     1,rnlam(3,20)
      common /rad5/ sourup(3,20,mh),sourdn(3,20,mh)
c     common /misc1/pi,pid180,sigma,iwrite(9,99)
c     common /misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt,clai(20)
c above two stm were commented and following added by Chen, 8/31/89.
c ******* cupradazm insert begin 1 : common ***************************
      common/rad6/rlfdif(3,20),tlfdif(3,20),rlfdir(3,20,mh),
     &tlfdir(3,20,mh)
      common/rad7/radabv(3),fbeam(3),fspec(3),wtir(20,10,50)
      common/rad8/rlfhem(3,9,20),tlfhem(3,9,20)
      common/indax2/xmeu,xneu,gamrat,frdeg(91),iangot(9),kpntsp
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
      common/misc6/xint(mh),nxintz,xintz(90),nalpha,ispher
      common/deg/sunazm(mh),nozenv,viewzn(10),noazmv,viewaz(50)
     &,xintv(10,50)
c above stm was added by Chen, 9/18/89.
c ******* cupradazm insert end   1 : common ***************************
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/misc5/deltz(50),zbcpy(10),zbc(10),zabc(20)
      common /time/month,jday,iyear,icumdy,timloc(mh)
      common/astron/eqtm,decl,sindec,cosdec,decmax,sinlat,coslat,
     1tanlat,dlong
      common /wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     &,zcrit
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/resis2/radn,anstom,rcut20,rsmin,trsopt,trsmin,trsmax,rsm
     1,rsexp,psi1,psi2
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/leaf2/evap(10,20),gevap(10,20),heat(10,20),gheat(10,20)
     &,alam ,tlfavg(20),tgheat(20),tgvap1(20),tgvap2(20)
      common/leaf3/hwater(10,20),ghwatr(10,20)
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),uprof(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
     2,akcpynut(50),akcpyold(50)
      common/prof3/ustara,ustars,phima,phims,akh(50),cp(50)
      common/prof4/psima,psims,zdla,zdls,zdlast
      common/prof5/ecap(50),qcap(50)
      common/cpy1/etotwt,htot,rplnt,evapg(20),heatg(20),eave(20)
     &,have(20),waterg(20)
      common/cpy2/hsoil,hcpys,evtot,etotw(20),contot(20),scond(10,20)
     1,ihrwet(20),ecpys,cphstr,cpestr,wcpys,evsmic
      common/soil1/zsoil(25),wti(50),ndsoil,aksoil(50),cpsoil(50),tsn(50
     1)
      common/soil2/aksol(50),akw(50),cw(50),wt(mh,50),esave(3,mh),
     & wnu(mh,50),wnl(mh,50)      
c     --------------
c     | residue variable added LMM 94/9/6
c     | layer subscripts were add to the ****fc vars - MCA 5/24/95
      common/soil3/clodsz,sandfc(25),siltfc(25),clayfc(25),qrtzfc(25),
     & iresdu
c     --------------
c     | rock variables added to soil4 LMM 94/9/8
c     | layer subscripts were added by MCA 5/24/95
      common/soil4/pe(25),bx(25),bd(25),aks(25),an(25),ws(25),asoil(25),
     & bsoil(25),csoil(25),dsoil(25),esoil,idoroc,irocly,akrock,cprock,
     & layid(25)
      common/water1/iprecp,tprecp,pn(mh,50),wcond(50),wstor(50),
     & wpond(mh)
c	----------------
c	| drain5 added for Kevin Fermanich and Bill Bland
c	| LMM 94/3/25
      common/water2/sw,deld,drhsfc,rhslop,drain,drain5,filt,etmm,drgrav
      common/water3/swlast
      common/photo1/t0,xk1,xk2,xk3,xk4,xk5,xk6,ro,vm,xj2,delha
     &             ,delhl,delhh,thalfl,thalfh,cidca,gsin
      common/photo2/ox,cair,rastom,cucond,d1,bkv,facj,ic3c4,rxcham,rsfac
     &             ,znon,conmin
c conmin in above stm was added, Chen, 10/8/89.
      common/photo3/csleaf(10,20),psleaf(10,20),cileaf(10,20),rgas
     &,rdrk(10,20),hsleaf(10,20),csfclf(10,20)
      common/photo4/cslay(20),rslay(20),cilay(20),fehist(20)
     &             ,rdlay(20),ecompl(20),pslay(20)
      common/photo5/cscpy,pscpyl,pscpyg,cicpy,rdcpy,qemax
c
c---- DrSun/lMurdock new common for photc4 and photc3file
c
      common /photc34/pair,rds,qtrd,vmax,qtvm,coef,
     &coef1,tb,rkjc25,qtpc,sms,bc4,alpha,c4tht,c4tht2,beta,beta2,
     &rcut,condc
      common /photc3/vmvif,coefvm,tvm,paab,qtwz,coefwz,twz,sp25,qtsp,o2,
     &rkc25,qtkc,rko25,qtko,bc3,thtc3,thtc32 
c
      common/iterat/noiter,iter2,iter3,iterw,loope,loopt,loopw
      common/inter2/evint(20),evimm(20),pintmx,frstem,drip(20),stem
c akroot(50) added to /root1/ by MCA - 6/12/95      
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1,rootup(50),cpytr,psisum,akroot(50)
c     common/dpevp1/dpdmin,nsmpl,dianoz,spkprs,ispkty,inoint
c     common/dpevp2/dtint,dtnew,iprtin,dtheta,htspk,tdropi
c     common/dpevp3/evapsm(50),aprtot(50),irrchk(50,mh),iprock,twb,irrga
c     common/dpevp4/etchk2(5),qchk2(5),tirrig,drpang,drpdeg,spkev,totevp
      common/inter1/wtp(20),frwet(20),frwtmx,pint(20),pilast(20)
     1,pint1(20),twater
      common/spkler/ctwa,dtwa,etwa,ftwa,gtwa,cetsum,detsum,eetsum,fetsum
     &,getsum,hetsum,cqsum,dqsum,eqsum,fqsum,gqsum,cetfix,detfix,eetfix,
     &fetfix,cqfix,dqfix,eqfix,fqfix,zmax,htspk,abspk,bspk,spkprs
     &,bdyang,tdropi
      common/drywt1/fdwstm,fdwlf,fdwsh,fdwrot,fdwgr,dwtot,dwlfpa
      common/drywt2/cm25s,cm25l,cm25r,cm25g
      common/drywt3/rmstem,rmleaf,rmroot,rmgr,rmtot,rgtot,psmrm,dminc
      common/teststab/unutr
       dimension vwzenp(10)
c above stm was added by Chen, 06/03/90.
c when idiagn=1 message will be displayed on screen telling which 
c subroutine is calling. this is for debugging. chen, 01/23/90.
c-------------
cBGs
c above stm was added by Chen, 06/03/90.
c ********* cupidg insert begin 1 : (chen, 01/18/90) **********************
	dimension sauwtz(mh,50),sautnz(mh,50)
        common/prof6/akh1(50),wtlast(50)
c	-------------------
c	| dran5d added for Kevin Fermanich and Bill Bland
c	| LMM 94/3/25
        common/daily/dtime,tcpyd,tsfcsd,hsoild,hcpysd,rnsold,wcpysd
     1	,rncpyd,hcpyd,ecpyd,conddy,swday,delwd,draind,dran5d,filtd
     1  ,etmmd,parid,solrid,apard,asolrd,psyngd,preevd,trmmd,tairmx
     1	,tairmn,rhmax,rhmin,vpaird,taird,winddy,solard,pard,precpd
     1	,etpmd,rmstmd,rmlfd,rmrotd,rmgrd,rmtotd,rgtotd,psmrmd,dmincd
        common/savegl/savet(20),savee(20),savetl(10,20),savepi(20)
     1  ,savect(20),saveak(50),savewt(50),saveqc(50),savept,savep1
     1  ,iday,nohrs,ihrstr,ihrend,strt,end
      common/tempir/tlfmax,tlfmin,tircpy,tirhot,taphot,wtirsl,emihot
      integer cycls,cycno
      common/cycle/cycls,cycno
      common/invers/rhsoil
        save
	idiagn=0
        write(6,*)'invrgl,igl,ihrgl,isrcgl,isw1=',invrgl,ihrgl
     1 			 ,isrcgl,isw1
        write(6,*)'xx=',(xx(i),i=1,7)
        if(isw1.eq.1)goto 6666
        tairgl=xx(1)
        eairgl=xx(2)
        windgl=xx(3)
        psisgl=xx(4)*1000.0
c*****wtsfc
        wtsfgl=xx(5)
c Sun added the following statement instead of original one with C now
c 4/14/92
        tnsfgl=xx(6)
c       qcond(2)=xx(6)
        econd(2)=xx(7)
 6666   continue
       write(6,*)'xx(6)=',xx(6),ihrgl,'xx(1) ',xx(1)
        if (invrgl.eq.0) goto 19
c invrgl=0, normal simulation without inversion.
c invrgl=1, inversion.
        if (igl-1) 11,12,13
c igl=0, before inversion, hourly loop begins from 1 and ends at ihrgl-1.
c igl=1, inversion, hourly loop begins from ihrgl and ends at ihrgl.
c igl=2, after inversion, hourly loop begins from ihrgl (not ihrgl+1, since
c        same saved variables can be used) and ends at nohrs.
11      ihrb=1
        ihre=ihrgl-1
        goto 19
12      ihrb=ihrgl
        ihre=ihrgl
        do 21 j=1,jtot
        tair(j)=savet(j)
        eair(j)=savee(j)
        pilast(j)=savepi(j)
        contot(j)=savect(j)
        do 21 i=1,itotp1
        templf(i,j)=savetl(i,j)
21      continue
        do 22 jz=1,50
        akh1(jz)=saveak(jz)
        wtlast(jz)=savewt(jz)
        qcond(jz)=saveqc(jz)
22      continue
        psitop=savept
        psitp1=savep1
        tsfc=savets
        tsfc1=savts1
        psima=savpsi
        zdlast=savzdl
        ecpys=savecp
        swlast=savswl
        if(isrcgl.eq.1)then
         temair(ihrgl)=tairgl
         vpair(ihrgl)=eairgl
         wind(ihrgl)=windgl
         psisum=psisgl
         wt(ihrgl-1,jzsfc+1)=wtsfgl
c Sun added the following statement 4/14/92
         tn(ihrgl-1,jzsfc+1)=tnsfgl
c
c Remarks on changing psisum (chen, 02/22/90):
c Since once psisum is changed, the profile of soil water contents
c has to be changed accordingly. It cannot be done exactly, but an
c approximation can be achieved: calculate uniform water content
c coresponding to changed psisum, wtpsig, and a multiple factor,
c fctrwt, as a ratio of wtpsig and wtpsis (which is calculated at
c each hour in subroutine rootex, and is the uniform water content
c corresponding to original psisum). The original water content
c profile is then  modified by multiplying the factor fctrwt.
c
C
c  The below line was commented out by MCA 6/9/95
c        if(psisum.gt.pe(1)) psisum=pe(1)
         if(psisum.le.pe(1))then
            wtpsig=ws(1)*(psisum/pe(1))**(-1./bx(1))
         else
            wtpsig=ws(1)
         endif
         fctrwt=wtpsig/wtpsis
         do 23 jz=1,50
         wtlast(jz)=wtlast(jz)*fctrwt
23       continue
c        do 24 jz=jzsfc+1,jzsfc+4
c        wtlast(jz)=wtsfc
c24      continue
        endif
        goto 25
13      ihrb=ihrgl
        ihre=nohrs
        do 32 j=1,jtot
        tair(j)=savet(j)
        eair(j)=savee(j)
        pilast(j)=savepi(j)
        contot(j)=savect(j)
        do 32 i=1,itotp1
        templf(i,j)=savetl(i,j)
32      continue
        do 33 jz=1,50
        akh1(jz)=saveak(jz)
        wtlast(jz)=savewt(jz)
        qcond(jz)=saveqc(jz)
        wt(ihrgl-1,jz)=sauwtz(ihrgl-1,jz)
c Sun added the following statement  4/14/92
        tn(ihrgl-1,jz)=sautnz(ihrgl-1,jz)
33      continue
        psitop=savept
        psitp1=savep1
        tsfc=savets
        tsfc1=savts1
        psima=savpsi
        zdlast=savzdl
        ecpys=savecp
        swlast=savswl
        temair(ihrgl)=tairgl
        vpair(ihrgl)=eairgl
        wind(ihrgl)=windgl
         wt(ihrgl-1,15)=wtsfgl
c Sun added the following statement 4/14/94
         tn(ihrgl-1,jzsfc+1)=tnsfgl
c
c  wtlast of jzsfc+1 to jzsfc+4 is already saved above in wtlast
c
        goto 25
19      continue
c ********* cupidg insert end   1 : (chen, 01/18/90) **********************
cBGe
c-------------
c#ifdef PROBE
cc     ----------
cc     | probe diagnostic tools LMM 94/11/23
c      call pinit(1,31)
c      call pinit(2,32)
c      call pinit(3,33)
c      call pinit(4,34)
c      call pinit(5,35)
c      call pinit(6,36)
c      call pinit(7,37)
c#endif
c
c  set up files so multiple runs can be done from background
c
      open(unit=18,file='c6.filist',status='unknown')
c     -------
c     | DA BIG DIAGNOSTICS IN LEAF BALANCE AND CUPROF from the preiod of 
c     | time when we where adding the feed back between lfbal & newrap 94/11/23
c      open(unit=23,file='c6.lfbal',status='unknown')
c     inquire(unit=23,exist=extat,opened=optat,named=nmtat,name=filnam)
c     write(6,*)' exist ', extat ,' opened ',optat,' named ',nmtat
c    &,' name=',filnam
c  read in number of runs
      read(18,*) noruns
      write(6,*)'noruns= ',noruns
      do9900nrun=1,noruns
c-----------
cBGb
c	-----------
c	| reread the lines until you get to the file 
c	| you want for this run. 
	do 9990 nofile=1,nfilerun
	  read(18,*)filein,filout
c Above was changed for Murty output
c          read(18,*)filein,filout,filohr
 9990	continue
cBGe
c-------------
c
c
c  get input file name and output file name
c     write(6,*)' input file name '
c     read(5,7)filein
c7    format(a22)
c     write(6,*)' output file name '
c     read(5,7)filout
c open files to be used with cupid5c
c     open(unit=9,file='kvsu005.911224b',status='unknown')
c     open(unit=10,file='kvsu005.911219a',status='unknown')
c      open(unit=15,file='c6bg.out',status='unknown')
c     open(unit=16,file='coef1.test',status='unknown')
c      open(unit=19,file='file19',status='unknown')
      open(unit=20,file=filein,status='unknown')
      open(unit=21,file=filout,status='unknown')
c  Below was added for Murty output
c      open(unit=30,file=filohr,status='unknown')      
c     open(unit=36,file='coef.check',status='unknown')
c     write (16,8000)
c8000 format (4x,'jz ',3x,'zdls',5x,'phims',5x,'zmid',6x,'akms',5x,
c    &'akcpy',6x,'akms1',6x,'akmst')
c      write(15,*)'noruns= ',noruns
c     write(16,*)'noruns= ',noruns
c     write(19,*)'noruns= ',noruns
c     write(23,*)'noruns= ',noruns
c
c  title for front of output file
      read(20,8)title1
      read(20,8)title2
 8    format(a80)
      write(6,*)' input file name..........',filein
      write(6,*)' output file name.........',filout    
      write(6,*)' title from input file named ',filein  
      write(21,8)title1
      write(21,8)title2
      write(6,8)title1
      write(6,8)title2
c Added for Murty output
c      write(30,8)title1
c      write(30,8)title2        
c     write(30,*)' input file name..........',filein
c     write(30,*)' output file name.........',filout  
c     write(30,*)' 402164 401164 709164 709164 718164 716164 ',
c    &           '701164 704164 704164 704164'
c     write(30,*)' 0      0    100      0      0      0      0',
c    &           '      0      0      0'
c     write(30,*)
c    &'cumhr timloc ta4 0 ta4 45 CO2sol pscpyg  hsoil hcpy ecpy rncpy'
c     write(30,*)'960    960    960    960    960    960',
c    &            '    960    960    960    960 '          
c
c
c     write(6,*)' note boote maintenance coeff used in cudpe.f'
      write(6,*)' note ritchie maintenance coeff used in cudpe.f'
      write(6,*)' sizelf=dmax in sub wndfac (cuht.f)'
c
c                      fixed constants
c
c
      pi=4.*atan(1.)
      pid2=pi/2.
c above stm was added by Chen, 9/18/89.
      pid180=pi/180.
      sigma=5.67e-08
      rgas=8.314
      decmax=sin(23.44*pid180)
c
c
cxxxxxxxxxxxxxxx cupradsp insert 2 begin xxxxxxxxx
c     call inbdr(nodayx)
c     write(6,*)' nodayx = ',nodayx
c     call inbdr2(idayx,ihrx,nodayx,nhrs)
cxxxxxxxxxxxxxxxx cupradsp insert end 2 xxxxxxxxxxx
c
	if(idiagn.eq.1)write(6,*)'before calling infix'
c above stm was added by chen, 01/23/90.
      call infix(xlat,xlong,stdlng,ihrpnt,nodays,ispecl,nn1pnt,kpnt
     &,lpnt,ispn,mday,slope,aspect,hrzang)
c	------
c	| added slope,aspect,hrzang to above call LMM 25/8/93
	if(idiagn.eq.1)write(6,*)'before calling insoil'
      call insoil(refdpt)
	if(idiagn.eq.1)write(6,*)'before calling inspkl'
      call inspkl
	if(idiagn.eq.1)write(6,*)'before calling inplnt'
      call inplnt(ibidir,ipp,nvzenp,vwzenp,itassl,jmax,jmin,dfmin,
     &jmaxp1,plspc,ilaiup,zdhcr,theta,pltpm2,clump,notemp,tfix,factir
     &,amfull,iunif,z0dh,dispdh)
c     write(9,9098) (zbcpy(jk), jk=1,nlbcpy)
c     write (10,9098) (zbcpy(jk), jk=1,nlbcpy)
 9098 format (2x,'"zbcpy (m)"',7(1x,f7.3))
c     write(9,*) 'z0soil= ',z0soil,' nlabcy',nlabcy,' jzsfc',jzsfc
c     write(9,*) 'coresponding to output: kon182a.911224b'
c     write (10,*) 'z0soil= ',z0soil
c     write (10,*) 'coresponding to output: kon182a.911223 '
c     write (10,9095)
c9095 format('"d"',' "h"','"u10"','"k10"','"u65"','"k65"','"u03"',
c    &' "k03"','" pis"','"zdl"',
c    &' "Hs" ',' " Es "','  "DT"',' "ksn"','  "un"')
c ipp,nvzenp,vwzenp in above stm were added by Chen, 06/03/90.
	if(idiagn.eq.1)write(6,*)'before calling initc'
      call initc
	if(idiagn.eq.1)write(6,*)'before calling initc'
	if(idiagn.eq.1)write(6,*)'after calling initc'
c above stm was added by chen, 01/23/90.
c
c  zero leaf condensation variables
      	do790j=1,jmax
	    contot(j)=0.
	    ihrwet(j)=0
		do790i=1,itotp1
 790  		    scond(i,j)=0.
c
c
c
c                set up environmental data arrays
c
c
c  routine to set up environmental data arrays for input to cupid
c    if data is hourly it is just read in.
c
	intsky=0
	read(20,*) nodays,ratiod,ration
c
c  set hpsi=1. initially only because program uses hpsi from previous
c    hour.
	hpsi=1.
	psitp1=psi1
	psitop=0.
c
c                                                           file write 1
	qitot=itot
	qjmax=jmax
	qjmin=jmin
      if(iwrite(1,1).eq.1)write(21,30)
 30   format(' 1101', 9x,/14x,' qitot  qjmax  qjmin  dfmin  xlat   xlong
     1  stdlng  emis  emisol')
      if(iwrite(1,1).eq.1)write(21,31)qitot,qjmax,qjmin,dfmin,xlat,xlong
     1,stdlng,emis,emisol
 31   format(' 2101',9x,9f7.2)
c
      if(iwrite(1,12).eq.1)write(21,34)
 34   format(' 1112', 9x,/14x,' slope  aspect')
      if(iwrite(1,12).eq.1)write(21,35)slope/pid180,aspect/pid180
 35   format(' 2112',9x,9f7.2)
c
      if(iwrite(1,2).eq.1)write(21,40)
 40   format(' 1102', 9x,/14x,' rsoil1 rsoil2 rsoil3 rlive1 rlive2 rlive
     13 tlive1 tlive2 tlive3')
      if(iwrite(1,2).eq.1)write(21,41)(rsoil(m),m=1,3),(rllive(m),m=1,3)
     1,(tllive(m),m=1,3)
 41   format(' 2102',9x,9f7.2)
      if(iwrite(1,13).eq.1)write(21,44)
 44   format(' 1113', 9x,/14x,' rdead1 rdead2 rdead3'
     1' tdead1 tdead2 tdead3')
      if(iwrite(1,13).eq.1)write(21,45)(rldead(m),m=1,3),
     1(tldead(m),m=1,3)
 45   format(' 2113',9x,9f7.2)
      qnoday=nodays
      qlaiup=ilaiup
      if(iwrite(1,3).eq.1)write(21,50)
 50   format(' 1103', 9x,/14x,' qlaiup qnoday refhtt refht  refhtw refdp
     1t rroot  notemp  tfix ')
      rrout=rroot*1.e-6
      qnotmp=notemp
      if(iwrite(1,3).eq.1)write(21,51)qlaiup,qnoday
     1,refhtt,refhte,refhtw,refdpt,rrout,qnotmp,tfix
 51   format(' 2103',9x,9f7.2)
      if(iwrite(1,4).eq.1)write(21,90)
 90   format(' 1104', 9x,/14x,'   fr1    fr2    fr3    fr4    fr5 '
     1,'   fr6    fr7    fr8    fr9 ')
      if(iwrite(1,4).eq.1)write(21,91)(fr(i),i=1,itot)
 91   format(' 2104',9x,9f7.2)
      if(iwrite(1,4).eq.1)write(21,96)
 96   format(' 1104', 9x,/14x,' fraz1  fraz2  fraz3  fraz4  fraz5 '
     1,' fraz6  fraz7  fraz8  fraz9 ')
      if(iwrite(1,4).eq.1)write(21,91)(fraz(i),i=1,nbeta)
      if(iwrite(1,5).eq.1)write(21,92)
 92   format(' 1105', 9x,/14x,' rowspc plspc  z0soil ndsoil',
     1' deld   drhsfc frstem frwtmx pintmx')
      qndsol=ndsoil
      if(iwrite(1,5).eq.1)write(21,93)rowspc,plspc,z0soil,qndsol
     1,deld,drhsfc,frstem,frwtmx,pintmx
 93   format(' 2105',9x,9f7.2)
      qic3c4=ic3c4
      if(iwrite(1,6).eq.1)write(21,88)
 88   format(' 1106',9x,/,14x,' qic3c4  t0     xk1   qxk2    xk3  ',
     &' qxk4   qxk5   qxk6    ro   ')
      qxk2=xk2/1000.
      qxk4=xk4/1000.
      qxk5=xk5/1000.
      qxk6=xk6/1000.
      if(iwrite(1,6).eq.1)write(21,89)qic3c4,t0,xk1,qxk2,xk3,qxk4,      
     &qxk5,qxk6,ro
 89   format(' 2106',9x,9f7.2)
      if(iwrite(1,7).eq.1)write(21,81)
 81   format(' 1107',9x,/,14x,'  vm     xj2   delha  delhl  delhh ',
     &' thalfl thalfh cidca  gsin ')
      qdelha=delha/1000.
      qdelhl=delhl/1000.
      qdelhh=delhh/1000.
      if(iwrite(1,7).eq.1)write(21,82)vm,xj2,qdelha,qdelhl,qdelhh,
     &thalfl,thalfh,cidca,gsin
 82   format(' 2107',9x,9f7.2)
      if(iwrite(1,10).eq.1)write(21,83)
 83   format(' 1110',9x,/,14x,'  ox     cair  rastom cucond rxcham',
     &' facj    znon    d1    bkv ')
      qcucon=cucond*1000.
      if(iwrite(1,10).eq.1)write(21,84)ox,cair,rastom,cucond,rxcham,
     &facj,znon,d1,bkv
 84   format(' 2110',9x,9f7.2)
      if(iwrite(1,9).eq.1)write(21,61)
 61   format(' 1109',9x,/,14x,' htspk  htdrop spkprs bdyang tdropi')
      if(iwrite(1,9).eq.1)write(21,62)htspk,htdrop,spkprs,bdyang,tdropi
 62   format(' 2109',9x,9f7.2)
c
c
c  daily loop recycles here                   ****daily loop****
c======================================================================
c                                                              DAY LOOP
c======================================================================
      ifirst=0
      ihr=1
 99   do9000iday=1,nodays
c
      write(6,102)iday
c      write(15,102)iday
 102  format(' iday=',i3)
      evapsm(iday)=0.
      aprtot(iday)=0.
c  convert tlai(iday) and ht(iday) to totlai and h so labled commons
c    easier in subroutines
c
      ipot=0
      totlai=tlai(iday)
      totlf=totlai/pltpm2
      h=ht(iday)
      aroot=arotin(iday)
      zldh=zldh1(iday)
      zmdh=zmdh1(iday)
c  calc critical height where soil prof is referenced to.
c
      zcrit=zdhcr*h
      z0=h*z0dh
      if(z0.lt.z0soil)z0=z0soil
      disp=h*dispdh
c     | above 3 added 94/11/23
c
c  Calculate the current number of layers in the canopy
c
      if(ilaiup.eq.1)go to 105
      df=dfmin
      jtot=totlai/df
      if(jtot.lt.jmin) jtot=jmin
      if(jtot.gt.jmax) jtot=jmax
      df=totlai/jtot
c  add soil layer (j=1)      
      jtot=jtot+1
 105  continue  
c
c  Find JDEAD - the highest layer in the canopy fully composed of
c  dead vegetation - and fraction dead veg in layer JDEAD+1.
c
      totded = (1-frlive(iday))*(jtot-1)
      ndead  = int(totded)
      frdead = totded - ndead 
      jdead = ndead + 1
c  Finite precision sometimes causes trouble here 
      if (abs (1-frdead).le.0.0001) then
        frdead = 0.
        ndead = ndead+1
        jdead = jdead+1
      endif    
c      
c
c     ------------------------------------------------------------------
c     | Calculate rleaf, tleaf and aleaf.  Then
c     | calculate (r|t)lfhem and (r|t)lfdif.  This used to
c     | be done in cuinp.f but is now here because
c     | of the addition of live and dead leaf reflectance
c     | and transmitances and the daily changing variable
c     | fraction live (frlive).   LMM 94/9/6
c
      do 118, k=1,kmax
c  		These layers are completely dead      
        do 113,j=2,jdead
          rleaf(k,j) = rldead(k)
          tleaf(k,j) = tldead(k)
          aleaf(k,j) = 1-(rleaf(k,j)+tleaf(k,j))
 113    continue
c 		This layer is partially dead
        rleaf(k,jdead+1) = rllive(k)*(1-frdead)+rldead(k)*frdead
        tleaf(k,jdead+1) = tllive(k)*(1-frdead)+tldead(k)*frdead
        aleaf(k,jdead+1) = 1-(rleaf(k,jdead+1)+tleaf(k,jdead+1)) 
c		These layers are completely live
        do 118,j=jdead+2,jtot
          rleaf(k,j) = rllive(k)
          tleaf(k,j) = tllive(k)
          aleaf(k,j) = 1-(rleaf(k,j)+tleaf(k,j))
 118  continue         
c
c         --------------
c         | avisr, bvisr, cvisr etc are entered in cuinp
c         | and are the coefficients of the parabola describing
c         | refl and trans as a func of source inc angle(a*x**2 + b*x
c         | +c = refl) for vis and nir.  Other wavb\e bands are
c         | interpolated from these. c=normal inc value.  This code is
c         | from the "cupradazm insert 2" placed in cuinp.f by Chen
c         | on 89/8/30.  Reorganized here LMM 94/9/6.
c	  | 
c	  | The redundant portion of code was deleted from cuinp.f
c	  | for clarity's sake by MCA 4/19/95.
c
      do 97, j=2,jtot
 	  do 95,k=1,kmax
          do 95,i=1,itot
            angl=90./itot*(i-1)+90./(2.*itot)
            ar=anirr-(cnirr-rleaf(k,j)*100.)*(anirr-avisr)/(cnirr-cvisr)
            br=bnirr-(cnirr-rleaf(k,j)*100.)*(bnirr-bvisr)/(cnirr-cvisr)
            cr=rleaf(k,j)*100.
            rlfhem(k,i,j)=(ar*angl*angl+br*angl+cr)/100.
c             ---------
c             | rlfhem is used with angle betw leaf normal
c             | and sun from light dist.
            at=anirt-(cnirt-tleaf(k,j)*100.)*(anirt-avist)/(cnirt-cvist)
            bt=bnirt-(cnirt-tleaf(k,j)*100.)*(bnirt-bvist)/(cnirt-cvist)
            ctr=tleaf(k,j)*100.
            tlfhem(k,i,j)=(at*angl*angl+bt*angl+ctr)/100.
            if(rlfhem(k,i,j)+tlfhem(k,i,j).gt.1.) then
                tlfhem(k,i,j)=1.-rlfhem(k,i,j)
            endif
c
 95   continue
c
c	-------------------------------------------------------------
c	| The variable zalpha below used to be called alpha.  This
c	| caused the photosynthesis parameter alpha (in common
c       | block photc34) to be changed when the next lines were executed.  
c       | MCA 5/11/95
c	|
      do 97, k =1,kmax
          sum2=0.
          sum3=0.
          sum4=0.
          do 98, iangl=1,itot
              zalpha=pid2*(iangl-1)/(itot) + pi/(4.*itot)
              sa = sin(zalpha)
              ca = cos(zalpha)
              sum2=sum2+sa*ca
              sum3=sum3+rlfhem(k,iangl,j)*sa*ca
              sum4=sum4+tlfhem(k,iangl,j)*sa*ca
 98       continue
          rlfdif(k,j)=sum3/sum2
          tlfdif(k,j)=sum4/sum2
 97   continue
c
c     ------------------------------------------------------------------
c     | Cupid reads a days worth of hourly data at a time.
c     | presumably there are three options controled by indata.
c     | At present the first case always occurs.  this option reads
c     | in data until it finds a line with a day number different
c     | from the current one.
c
      if(indata-2)114,150,170
c
c     -------------------------
c     |  read in hourly data
c
c
 114  continue
c  neg values of rad mean they are not avail.
 101  read(20,*) iyear,icumdy,timloc(ihr),wind(ihr),radtop(1,ihr),
     1radtop(2,ihr),radtop(3,ihr),fbeam1(1,ihr),
     2fbeam1(2,ihr),temair(ihr),vpair(ihr),
     3 precip(ihr),irrchk(ihr)
c
c     ----------------------
c     | NOTE : if you create calculated variables in this
c     | section remmeber that you need to copy the nohrs+1
c     | to subscript 1 near line 9000!  LMM 95/2/10
c****************************************
c  check to be sure wind speed not zero
      if(wind(ihr).lt.0.2)write(6,*)'WIND SET TO 0.2 for IHR= ',ihr
      if(wind(ihr).lt.0.2)wind(ihr)=0.2
c
      if(iday.eq.1.and.ihr.eq.1) then 
c        write(6,*)'enter deltat, deltae and factv'
c	read(6,*) deltat,deltae,factv
      endif
c
c  Piers Sellers canopy chamber had trans=0.77 to PAR so mult radtop
c    by this for comparison with chamber.
c     radtop(2,ihr)=radtop(2,ihr)*.77
      if(ihr.eq.1)then
c     write(6,*)' RADTOP(2,ihr) mult by .77 for piers chamber'
      endif
c
	deltat=0.
	deltae=0.
	factv=1.
	facte= 1.0
c  calc factv using neutral log profile to adjust wind meas at
c    5.4m to 2 m because temp and vp meas at 2 m. For FIFE PAMS
        if(ic3c4.eq.2)then
      factv=(log((refhtw-.70*h)/(.03*h)))/(log((5.4-.70*h)/(.03*h)))
c
        else
        factv = 1.0
        endif
c
      temair(ihr)=temair(ihr)+deltat
      vpair(ihr)=(vpair(ihr)+deltae)*facte 
      wind(ihr)=wind(ihr)*factv
c
      if(ihr.eq.1)then
      write(6,*)' deltat= ',deltat,' deltae= ',deltae,' facte= '
     &,facte,' factv= ',factv
      endif
c****************************************
c     if irrchk(ihr)=0 there is no precip
c     if irrchk(ihr)=1 precip is above canopy sprinkler
c     if irrchk(ihr)=2 precip is below canopy
c     if irrchk(ihr)=3 precip is rain
c     if irrchk(ihr)=2 then irrigation with drop tubes
c     below the canopy
      if(irrchk(ihr).ne.2) goto 58
      tprecd(ihr)=precip(ihr)
      precip(ihr)=0.
      goto 59
58    tprecd(ihr)=0.
59    continue
c  when vpair(ihr) is input check that it is not gt vpsat(temair)
      vpsat=6.108*10**(7.5*temair(ihr)/(237.3+temair(ihr)))
      vpdin(ihr)=vpsat-vpair(ihr)
      if(vpair(ihr).gt.vpsat)vpair(ihr)=vpsat
      relhum(ihr)=vpair(ihr)/vpsat
c set boundary conditions to daily input values:
      temsol(ihr)=tsbc(iday)
      watsol(ihr)=wsbc(iday)
      if(ifirst.ne.0)go to 103
      icumrf=icumdy
      iyearf=iyear
      ifirst=1
      ihr=ihr+1
      goto101
 103  if(icumdy.ne.icumrf)go to 94
      ihr=ihr+1
      go to 101
c
c     | End of read in hourly data loop
c     ---------------------------------
c
  94  nohrs=ihr-1

      idydum=icumdy
      icumdy=icumrf
      icumrf=idydum
      iyedum=iyear
      iyear=iyearf
      iyearf=iyedum
c initialize nhrlst
      if(iday.eq.1)nhrlst=nohrs
c the array locations (nohrs+1)contain the first values of the next day.
c
c
c  precip(ihr) should be in mm = kg m-2 in one hour.
c
c  if no sky thermal set flag to calc it
      intsky=0
      if(radtop(3,1).lt.0.) intsky=1
	if(idiagn.eq.1)write(6,*)'before calling date'
c above stm was added by chen, 01/23/90.
      call date
      call declin
      call zenith(timsun,ihrstr,ihrend,strt,end,nohrs,sunazm)
	if(idiagn.eq.1)write(6,*)'after calling zenith'
c     -------------
c     | Set Radiation prior to sunrise and after sunset to 0
c     | for NIR and VIS
c     | 94/9/19 LMM
c
      do i=1,nohrs
          if (timloc(i).lt.strt .or. timloc(i).gt.end) then
              do k=1,2
                  if (nint(radtop(k,i)) .ne. -1) then
                      radtop(k,i) = 0.0
                  endif
              enddo
          endif
      enddo
c
c------------------------
c	Not quite sure why this 
c	is part of the inversion
c		LMM
c	
cBGb
	if (invrgl.eq.1 ) then
      	    kcumdy=icumdy
	    zenang(ihrgl)=ts(1)*pid180
c 	    Sun changed the following statement to next one (5/10/92)
c	    sunazm(ihrgl)=psn(1)   * this is angle unit instead of radius
c	    unit
	    sunazm(ihrgl)=psn(1)*pid180
	endif
cBGe
c------------------------
c above stm was added by chen, 01/23/90.
c ******* cupradazm insert begin 2 : calling dstlit *******************
c Chen, 8/31/89.
	do 112 ihr=1,nohrs
	if (abs(sunazm(ihr)).lt..00001) goto 112
	if(idiagn.eq.1)write(6,*)'before calling dstlit'
c above stm was added by chen, 01/23/90.
	call dstlit(zenang(ihr),sunazm(ihr),kmax,rlfdir,tlfdir,ihr)
 112	continue
c ******* cupradazm insert end   2 : calling dstlit *******************
c
c  various combinations of input rad are possible: irad=1, nir  beam
c     nir  total,par beam, par total.  irad=2, solar total,
c    par total.  irad=3, solar beam, solar total.  irad=4, solar total.
c   irad=5, use calculated potential above the canopy.
      if(radtop(1,1).lt.0..and.fbeam1(2,1).lt.0.) go to 115
      if(radtop(1,1).lt.0.and.radtop(2,1).lt.0)go to 127
      if(radtop(1,1).lt.0..and. fbeam1(2,1).ge.0.) go to 120
      if(radtop(1,1).ge.0..and.fbeam1(2,1).lt.0.) go to 125
      irad=1
      go to 180
 115  irad=4
c  ipot=0 ratio(i) calc in radin4 hourly from solar rad read in
c  ipot=1 ratio(i) set to ratiod in day and ration in nite.
      ipot=0
      go to 131
 120  irad=3
      go to 131
 125  irad=2
      go to 131
 127  irad=5
      ipot=1
 131  continue
      if(irad-3)133,138,143
 133  continue
      go to 180
 138  continue
      go to 180
 143  do142i=1,nohrs
c	  -------
c	  | radsol requires the radiation which does NOT 
c	  | take into account the slope.  I.e. we
c	  | want horizontal radiation rather than 
c	  | radiation parrallel to the canopy. We reach 
c	  | this point with radtop(1,i) = -1 and rad(2,i)
c	  | equal to the total solar or else = -1 in which
c	  | case it will be set to some percentage of potential
c	  | solar for that day on that part of the globe.
 142      radsol(i)=radtop(2,i)
c
c  ratio(i) i= 1 to nohrs calc'd in sub radin4.
	if(idiagn.eq.1)write(6,*)'before calling radin4'
c above stm was added by chen, 01/23/90.
      call radin4(radsol,ihrstr,ihrend,nohrs,ipot)
c     write(6,*)ratiod,ration,ratio
      go to 180
c  read in all of the daily data and convert to hourly one day at a time
 150  continue
 170  continue
 180  continue
c  set fbeam to 0.95 to get light response curve of leaves to photosyn.
      ifbpsn=0
      if(ifbpsn.eq.1)then
	write(6,*)' fbeam set to .95 because ifbpsn=1 at stm 180 in main'
	write(6,*)' ***************CAUTION*****************************'
	write(6,*)' ***************CAUTION*****************************'
	write(6,*)' ***************CAUTION*****************************'
        do 181 ihrdum=1,nohrs
	fbeam1(1,ihrdum)=0.95
 181    continue
      endif
c  eval zenith angles if hourly data was input
      if(intsky.eq.1)call skyir(nohrs)
c
c
c	-------
c	| call calcpath to determine the path lengths for this
c	| day.  uses RAD3 and DEG commons
c
	call calcpath(nohrs,slope,aspect,path)
c
c	-------
c	| Change radtop and fbeam1 so that they reflect the radiation
c	| incident on the canopy rather than the radiation measured
c	| for that location.  It is assumed that the measurements were
c	| not made with the slope aspect and horizon angle taken into
c	| acccount.
c
	do 182, ihr=1,nohrs
	    do 183,ispec=1,3
c		---------
c		| Save original values for print out
		radtsv(ispec,ihr) = radtop(ispec,ihr)
		fbm1sv(ispec,ihr) = fbeam1(ispec,ihr)
183	    continue
c
	    if (path(ihr).lt.-.99) then
c		--------
c		| Canopy is shaded by hill, remove direct beam
c		| for par and nir radiation.  Thermal unaffected (?)
		do 184,ispec=1,2
		    radtop(ispec,ihr)=radtop(ispec,ihr)
     &				      *(1-fbeam1(ispec,ihr))
		    fbeam1(ispec,ihr) = 0.0
184		continue
	    endif
c
c	    --------
c	    | Setup rhocpy, if it is the first day then
c	    | use a default.  For other days use data from
c	    | the last hour of the previous day.
	    if (iday .eq. 1) then
		rhocpy(1) = .05
		rhocpy(2) = .2
		rhocpy(3) = .02
	    else
                if (radtop(1,ihr) .lt. 0.01 ) then
                    rhocpy(1) = 0.05
                else
                    rhocpy(1) = u(1,jtot)/radtop(1,ihr)
                endif
                if (radtop(2,ihr) .lt. 0.01 ) then
                    rhocpy(2) = 0.2
                else
                    rhocpy(2) = u(2,jtot)/radtop(2,ihr)
                endif
                if (radtop(3,ihr) .lt. 0.01 ) then
                    rhocpy(3) = 0.02
                else
                    rhocpy(3) = u(3,jtot)/radtop(3,ihr)
                endif
	    endif
c
c	    ---------
c	    | Now recalc radtop for the hemisphere that 
c	    | is visible taking into account slope, hrzang and 
c	    | slope.
c
	    fbeam1(3,ihr)=0.0
	    do 185,ispec=1,3
		if (radtop(ispec,ihr).gt.0.0) then
	            direct = radtop(ispec,ihr)*fbeam1(ispec,ihr)
c
c COMMENT THIS THING LARRY
c
		    difuse = radtop(ispec,ihr)*(1.-fbeam1(ispec,ihr))
		    cosslp = cos(slope+hrzang)
		    difnew = difuse*(1-(1-cosslp)/2)
     &			   + ( difuse*(1-(1-cosslp)/2) + direct)
     &			   * ((1-cosslp)/2)*rhocpy(ispec)
c
c  special case for thermal when ispec=3
		    if(ispec.eq.3)difnew=difuse*(1.-(1.-cosslp)/2.)
     &			   + ( difuse*(1-(1-cosslp)/2))
     &			   * ((1-cosslp)/2)*sigma*temair(ihr)**4
c
		    cosdlt = coszen(ihr)*cos(slope)
     &			    +sin(zenang(ihr))*sin(slope)
     &			    *cos(sunazm(ihr)-aspect)
		    dirnew = cosdlt*direct/coszen(ihr)
		    radtmp = difnew+dirnew
c		
c		    -------
c		    | Debugging check for sanity.
c		    if ( (radtmp/radtop(ispec,ihr).gt.1.5) 
c     &			 .or. (radtmp/radtop(ispec,ihr).lt.0.75)) then
c			write(*,*)'Radtop and Radtmp are very different'
c			write(*,*)'CUMAIN in 185 loop.'
c			stop
c		    endif
c		    
c		    --------
c		    | Set Radtop and fbeam1 to new values based on
c		    | modified diffuse value.
		    if (radtmp.gt.0.0) then
			fbeam1(ispec,ihr)
     &			    =dirnew/radtmp
			radtop(ispec,ihr) = radtmp
c		    write(15,*)'iday ihr dirnew difnew',iday,ihr,dirnew
c     &              , difnew,radtmp,fbeam1(ispec,ihr)
		    endif
		endif
185	    continue
c
182	continue
c
c               updating plant data variables
c
c
c    lai was read in under input and calc fixed data
c  here plant vbls and other vbls depending on plant vbls are
c    updated if that should be once/day. if plant data read in
c    then it is assumed to be daily.
c
      if(ilaiup.eq.1)go to 800
c
c  Here we used to calculate the number of layers in the canopy
c  This has been moved to the beginning of the daily loop to
c  facilitate computation of the maximum dead leaf layer, MCA 4/20/95.
c
c      call layerp(nohrs,clump)
c above statement was commented and changed to follows, Chen, 8/31/89.
c
      call difint(nohrs,clump,sunazm,path)
c	-------
c	| added path to above call. LMM 25/8/93
c
	if(idiagn.eq.1)write(6,*)'before calling height'
c above stm was added by chen, 01/23/90.
      call height(iunif)
      call wndfac(amfull,zdhcr)
      call hite2(iday)
	if(idiagn.eq.1)write(6,*)'after calling hite2'
c above stm was added by chen, 01/23/90.
c  initialize temp and vp values hour index 1 and mh, mh is used for
c    the last hour of the previous day.
      if(iday.gt.1)goto 725
      jzsfc1=jzsfc+1
      do720jz=1,jzsfc1
      tn(1,jz)=temair(1)
      tn(mh,jz)=temair(1)
      en(1,jz)=vpair(1)
      en(mh,jz)=vpair(1)
 720  continue
 725  continue
c
c
c  calc frac of root system extracting water at each node
      froot(jzsfc+1)=0.
      resrot(jzsfc+1)=0.
      jzsfc2=jzsfc+2
      jzbm1=jzbot-1
      rootsm=0.
      do700jz=jzsfc2,jzbm1
      froot(jz)=exp(-aroot*zmid(jz-1))-exp(-aroot*zmid(jz))
      resrot(jz)=rroot/froot(jz)
c     ----------
c     | If there is a rock layer, change root restance so
c     | that roots can't get water from rock.  LMM 94/9/8
      if (idoroc.eq.1) then
          if (jz .ge. jzsfc+irocly) then
              resrot(jz)=resrot(jz)*1000
          endif
      endif
 700  rootsm=rootsm+1./resrot(jz)
      froot(jzbot)=froot(jzbm1)
      rootup(jzbot)=rootup(jzbm1)
c  check if jtot is the same on this day as previous day. if not
c    adjust index for soil water content, wt(,), so storage will be
c    cal'd correctly when jtot changes
      if(iday.eq.1)jtlast=jtot
      if(iday.eq.1)nalast=nlabcy
      if(iday.eq.1)nblast=nlbcpy
c     if(jtot.eq.jtlast)go to 410
c  I added the below statement because going through here on the
c  first day caused tn(1,jzsfc+1) to be set to 0, which makes
c  trouble later on (the soil is at 0 C).  Why is the above statement
c  commented out?  MCA - 5/31/95
      if (iday.eq.1)go to 410
      jzsfc1=jzsfc+1
      do405jz=jzsfc1,jzbot
      if(jz.eq.jzsfc1)en(mh,jz)=en(nhrlst,jz-jtot+jtlast-nlabcy+
     &nalast-nlbcpy+nblast)
      pn(mh,jz)=pn(nhrlst,jz-jtot+jtlast-nlabcy+nalast-nlbcpy+nblast)
      tn(mh,jz)=tn(nhrlst,jz-jtot+jtlast-nlabcy+nalast-nlbcpy+nblast)
      wnu(mh,jz)=wnu(nhrlst,jz-jtot+jtlast-nlabcy+nalast-nlbcpy+nblast)
      wnl(mh,jz)=wnl(nhrlst,jz-jtot+jtlast-nlabcy+nalast-nlbcpy+nblast)      
 405  wt(mh,jz)=wt(nhrlst,jz-jtot+jtlast-nlabcy+nalast-nlbcpy+nblast)
c     do407jz=jzsfc1,jzbot
c     if(en(nohrs,jz).eq.jzsfc1) en(nohrs,jz)=en(mh,jz)
c     pn(nohrs,jz)=pn(mh,jz)
c     tn(nohrs,jz)=tn(mh,jz)
c407  wt(nohrs,jz)=wt(mh,jz)
      jtlast=jtot
      nalast=nlabcy
      nblast=nlbcpy
 410  continue
c                                                           file write 2
      qdecl=decl/pid180
      qeqtm=eqtm*60.
c
      qyear=iyear
      qcumdy=icumdy
      qmonth=month
      qjday=jday
      if(iwrite(2,1).eq.1)write(21,106)icumdy
 106  format(' 1201',i3,6x,/14x,' qyear   qjday qmonth   h    totlai',
     1' qdecl   qeqtm aroot ')
      if(iwrite(2,1).eq.1)write(21,107)icumdy,qyear,qjday,qmonth,h,totla
     1i,qdecl,qeqtm,aroot
 107  format(' 2201',i3,6x,9f7.2)
      qiday=iday
      if(iwrite(2,2).eq.1)write(21,108)icumdy
 108  format(' 1202',i3,6x,/14x,' hfday   strt   end   qiday  factir')
      if(iwrite(2,2).eq.1)write(21,109)icumdy,hfday,strt,end,qiday
     &,factir
 109  format(' 2202',i3,6x,9f7.2)
      if(iwrite(2,3).eq.1)write(21,110)icumdy
 110  format(' 1203',i3,6x,/14x,' sizelf  z0    disp     am   zldh   zmd
     1h','     jtot ndsoil')
      qjtot=jtot
      qndsol=ndsoil
      if(iwrite(2,3).eq.1)write(21,111)icumdy,sizelf,z0,disp,am,zldh
     1,zmdh,qjtot,qndsol
 111  format(' 2203',i3,6x,9f7.2)
      qjzcpy=jzcpy
      qjzsfc=jzsfc
      qjzbot=jzbot
      qnlaya=nlabcy
      qnlayb=nlbcpy
      if(iwrite(2,4).eq.1)write(21,116)icumdy
 116  format(' 1204',i3,6x,/14x,' qjzcpy qjzsfc qjzbot nlabcy nlbcpy'
     &,'  psi1   psi2 ')
      psi1ot=psi1/100.
      psi2ot=psi2/100.
      if(iwrite(2,4).eq.1)write(21,117)icumdy,qjzcpy,qjzsfc,qjzbot,
     &qnlaya,qnlayb,psi1ot,psi2ot
 117  format(' 2204',i3,6x,9f7.2)
c  zero daily vbls for hours of leaf wetness in morning and evening
      do104j=1,jtot
      iwetam(j)=0
 104  iwetpm(j)=0
      dtime=0.
      tcpyd=0.
      tsfcsd=0.
      hsoild=0.
      hcpysd=0.
      rnsold=0.
      wcpysd=0.
      rncpyd=0.
      hcpyd=0.
      ecpyd=0.
      conddy=0.
      swday=0.
      delwd=0.
      draind=0.
      dran5d=0.
      filtd=0.
      etmmd=0.
      parid=0.
      solrid=0.
      apard=0.
      asolrd=0.
      psyngd=0.
      preevd=0.
      trmmd=0.
      tairmx=-99.
      tairmn=99.
      rhmax=-99.
      rhmin=99.
      vpaird=0.
      taird=0.
      winddy=0.
      solard=0.
      pard=0.
      precpd=0.
      etpmd=0.
c  zero vbls for daily sum of dry wt and resp.
      rmstmd=0.
      rmlfd=0.
      rmrotd=0.
      rmgrd=0.
      rmtotd=0.
      rgtotd=0.
      psmrmd=0.
      dminc=0.
 800  continue
cxxxxxxxxxxxxxxxx cupradsp insert begin 3 xxxxxxxxx
c      do100iday2=1,nodayx
c     idocr=0
c     if(iday.eq.idayx(iday2))then
c       idaysv=iday2
c       idocr=1
c       call bdrday(iday2,nhrs)
c       write(6,*)' after bdrday ',iday,iday2,idayx(iday2)
c       goto 25
c     endif
c 100 continue
cxxxxxxxxxxxxxxxxxxx cupradsp insert end 3 xxxxxxxxxx
c
c  start loop over layers of the canopy and do whole model one
c  hour at a time.
c
c
c-----------------------------
c	| Inversion can choose the
c	| start and ending hours
cBGb 
c ********* cupidg insert begin 2 : (Chen, 01/18/90) **********************
        if(invrgl.eq.0)then
        ihrb=1
        ihre=nohrs
        endif
c ********* cupidg insert end   2 : (Chen, 01/18/90) **********************
c======================================================================
c						HOURLY LOOP
c
c                                              *****hourly loop******
   25 do 7000 ihr=ihrb,ihre
cBGe
c-----------------------------
      write(6,*)' ihr=',ihr
c#ifdef PROBE
cc       --------
cc       | "probe" diagnostic routines
c	call pend(1)
c	call pbeg1(iday,ihr)
c	call pend(2)
c	call pbeg2(iday,ihr)
c	call pend(3)
c	call pbeg3(iday,ihr)
c	call pend(4)
c	call pbeg4(iday,ihr)
c	call pend(5)
c	call pbeg5(iday,ihr)
c	call pend(6)
c	call pbeg6(iday,ihr)
c#endif
cxxxxxxxxxxxxxxxxxxx cupradsp insert begin 4 xxxxxxxx
c     if(idocr.eq.1)then
c       do55ihr2=1,nhrs(idaysv)
c       if(ihr.ne.ihrx(idaysv,ihr2))goto 55
c       write(6,*)' before cuprad ',ihr,idaysv,ihr2,ihrx(idaysv,ihr2)
c       call cuprad(idaysv,ihr2)
c       write(6,*)' after cuprad ',ihr,idaysv,ihr2,ihrx(idaysv,ihr2)
c  55   continue
c     endif
cxxxxxxxxxxxxxxxxxxx cupradsp insert end 4 xxxxxxxxxx
c  zero rnlam for radiat
      do740 k=1,3
      do739 j=1,jtot
  739 rnlam(k,j)=0.
  740 continue
c  calculate cumulative hour as jday + ihr/24 for plotting multiple
c    days on hp.
      cumhr=float(icumdy)+timloc(ihr)/24.
c  set iwrite for disk output to 1 if hour is to have longer output
      if(ispecl.eq.0)go to 770
      if(ihrpnt(ihr).eq.1.and.mday(iday).eq.1)go to 750
      do760l=1,nn1pnt
 760  iwrite(kpnt(l),lpnt(l))=0
      go to 770
 750  do775l=1,nn1pnt
 775  iwrite(kpnt(l),lpnt(l))=1
 770  continue
      ihrm1=ihr-1
      if(ihr.eq.1)ihrm1=mh
      if(ihr.eq.1.and.iday.eq.1)timloc(ihrm1)=0.
      dt=(timloc(ihr)-timloc(ihrm1))*3600.
      if(ihr.eq.1)dt=(timloc(ihr)+24.-timloc(ihrm1))*3600.
      if(ihr.eq.1.and.iday.eq.1)dt=3600.
 801  format(' ',i2)
      ioutpt=0
c                                                        file write 3
c
      qzen=zenang(ihr)/pid180
      qsunaz=sunazm(ihr)/pid180
      if(iwrite(4,1).eq.1)write(21,710)icumdy,ihr
 710  format(' 1401',i3,i2,4x,/14x,' timloc timsun zenang sunazm radtp1'
     1,' radtp2 radtp3 fbeam1 fbeam2')
      if(iwrite(4,1).eq.1)write(21,711)icumdy,ihr,timloc(ihr),timsun(ihr
     1),qzen,qsunaz,radtop(1,ihr),radtop(2,ihr),radtop(3,ihr)
     2,fbeam1(1,ihr),fbeam1(2,ihr)
 711  format(' 2401',i3,i2,4x,9f7.2)
c
c  angle of sun beam to perpendicular to slope is zenslp
      zenslp=180.*acos(1./path(ihr))/3.14159
c
      if(iwrite(4,4).eq.1)write(21,722)icumdy,ihr
 722  format(' 1404',i3,i2,4x,/14x,' zenslp radab1'
     1,' radab2 radab3 fbeam1 fbeam2')
      if(iwrite(4,4).eq.1)write(21,721)icumdy,ihr,zenslp
     1,radtsv(1,ihr),radtsv(2,ihr),radtsv(3,ihr)
     2,fbm1sv(1,ihr),fbm1sv(2,ihr)
 721  format(' 2404',i3,i2,4x,9f7.2)
c
      if(iwrite(4,2).eq.1)write(21,712)icumdy,ihr
 712  format(' 1402',i3,i2,4x,/14x,' temair vpair  wind   precip  temsol
     1 watsol solar ',' ratio ',' cumhr')
      solar1=radtop(1,ihr)+radtop(2,ihr)
      if(iwrite(4,2).eq.1)write(21,713)icumdy,ihr,temair(ihr),vpair(ihr)
     1,wind(ihr),precip(ihr),temsol(ihr),watsol(ihr),solar1,ratio(ihr)
     2,cumhr
 713  format(' 2402',i3,i2,4x,9f7.2)
      if(iwrite(4,3).eq.1)write(21,715)icumdy,ihr
 715  format(' 1403',i3,i2,4x,/14x,' vpdin  relhum irrchk factv  path ')
      qrrchk=irrchk(ihr)
      qpath=path(ihr)
      if (qpath.ge.999.)qpath=999.
      if (qpath.lt.-1) then
        write(*,*)'The path is very negative!!!!'
        stop
      endif
      if(iwrite(4,3).eq.1)write(21,716)icumdy,ihr,vpdin(ihr),relhum(ihr)
     &,qrrchk,factv,qpath
 716  format(' 2403',i3,i2,4x,9f7.2)
c
c  initialize hourly loop
c
c  check if solar to be calc. kstrt=1(all wave),kstrt=3(thermal only)
c  ihrstr is first daylight hour
      if(timsun(ihr)-strt)810,820,820
 810  kstrt=3
      go to 900
 820  if(timsun(ihr)-end)840,840,830
 830  kstrt=3
      go to 900
 840  kstrt=1
 900  do950k=1,3
          radabv(k)=radtop(k,ihr)
 950      fbeam(k)=fbeam1(k,ihr)
      coszn2=coszen(ihr)
c	-----------
c	| With the additon of slope and aspect we also need
c	| the angle between the sun and the slope.  Used in 
c	| radiate to get the beam flux.
	cosdlt = coszen(ihr)*cos(slope)
     &		+sin(zenang(ihr))*sin(slope)*cos(sunazm(ihr)-aspect)
c
c  setup initial conditions for new hour by keeping profiles of the
c    same shape.  that is tair profile has same shape referenced
c    to temair(ihr) and leaf temp departure from air temp is same.
      iihrm1=ihr-1
c  ihr=mh corresponds to last hour of previous day.
      if(ihr.eq.1)iihrm1=mh
      if(ihr.eq.1.and.iday.eq.1)iihrm1=ihr
      if(ihr.eq.1.and.iday.eq.1)go to 951
      go to 953
 951  do952j=2,jtot
      tair(j)=temair(ihr)
 952  eair(j)=vpair(ihr)
      ecpys=0.
      wpond(mh)=0.0
c  set water content vbl used in profl2 to initial water content
c    profile so wt(ihr,idepth) defined on first pass.
c    wnu and wnl are water contents at the upper and lower surface 
c    of the element.
      do956idepth=1,ndsoil
      idep=ndsoil-idepth+1
      jz=jzsfc+ndsoil+1-idepth
      wti(jz)=wti(idep)
      wnu(ihr,jz)=wti(jz)
      wnl(ihr,jz)=wti(jz)
      wnu(mh,jz)=-999.99
      wnl(mh,jz)=-999.99
 956  wt(ihr,jz)=wti(jz)
 953  do955j=1,jtot
      do954i=1,itotp1
      templf(i,j)=templf(i,j)-temair(iihrm1)+temair(ihr)
 954  temlf1(i,j)=templf(i,j)
      tair(j)=tair(j)-temair(iihrm1)+temair(ihr)
      tair1(j)=tair(j)
      eair(j)=eair(j)-vpair(iihrm1)+vpair(ihr)
      eair1(j)=eair(j)
 955  continue
      tbar=0.
      do975j=2,jtot
 975  tbar=tbar+tair(j)
      tbar=tbar/(jtot-1)
      tbar1=tbar
      iprofl=0
c  set en(ihr,jz) and tn(ihr,jz) to value from ihrm1
      do980jz=2,jzbm1
 980  tn(ihr,jz)=tn(ihrm1,jz)
      do982jz=2,jzsfc1
 982  en(ihr,jz)=en(ihrm1,jz)
c chen added "if (idiagn" statements for diagnostic output
c
      if(idiagn.eq.1)write(6,*)'before calling radiat'
      call radiat(kstrt,coszn2,radabv,fbeam,ihr,ioutpt,
     1            zenang(ihr),iprofl,sunazm(ihr),clump,path,cosdlt)
c                 zenang(ihr),sunazm(ihr) above added by Chen, 9/1/89.
c		-------
c		| path, cosdlt added 93/9/31 LMM
c
      if(idiagn.eq.1)write(6,*)'before calling rbound' 
      call rbound(ihr,wnd,frdead)
c
      psilf=0.
      psilf1=psilf
c
      if(idiagn.eq.1)write(6,*)'before calling photks'
      if (ic3c4.ge.10) then
          call photks(ihr,notemp,tfix,frdead)
      else if ((ic3c4.eq.1).or.(ic3c4.eq.2)) then
          call c4phot(ihr,notemp,tfix,frdead)
      else 
          call c3phot(ihr,notemp,tfix,frdead)
      endif
c
c     calculate angle of precip from zenith from droplet term velocity
c     and upper boundary wind speed
      call preang(ihr,drpang)
      call cpywt(drpang,wtp)
1025  continue
c  get canopy interception of precip if there is precip
c    precip is in mm.
      call interc(ihr)
      call lfebal(ihr,iday)
      call drips(ihr,lbredo,irrchk,tprecd)
	if(idiagn.eq.1)write(6,*)'after calling drips' 
c above stm was added by chen, 01/23/90.
c  if leaves are drying call lfebal again to include in e.b.
      if(lbredo.eq.1)call lfebal(ihr,iday)
      iter2=1
      do1042j=2,jtot
      tairx(1,j)=tair(j)
      eairx(1,j)=eair(j)
      smeair(j)=eair(j)
 1042 smtair(j)=tair(j)
      tsfc=tair(2)
      ecpys2=ecpys
c
c  recycle to here for convergence in hourly loop
c
 1050 continue
c  hpsi set to 1 before daily loop. set iterh=0 each time start leaf
c    energy bal. again so it is iter over stress only.
      iterh=0
 1055 noiter=0
 1060 continue
c     write(16,*) 'psitop,psitp1,iterh,0',psitop,psitp1,iterh,psi1,psi2
      do1100i=1,itotp1
      do1100j=2,jtot
c  tried weighting .5 and .5 but convergence is only half as fast.
      templf(i,j)=.9*templf(i,j)+.1*temlf1(i,j)
 1100 temlf1(i,j)=templf(i,j)
      kstrt=3
      noiter=noiter+1
	if(idiagn.eq.1)write(6,*)'before calling radiat'
c above stm was added by chen, 01/23/90.
      call radiat(kstrt,coszn2,radabv,fbeam,ihr,ioutpt,
     1zenang(ihr),iprofl,sunazm(ihr),clump,path,cosdlt)
c		-------
c		| path,cosdlt added 93/9/31 LMM
c
c     write(16,*) 'psitop,psitp1,iterh,ra',psitop,psitp1,iterh,psi1,psi2
c zenang(ihr),sunazm(ihr) in above stat. were added by Chen, 9/1/89.
c
      if (ic3c4.ge.10) then
          call photks(ihr,notemp,tfix,frdead)
      else if ((ic3c4.eq.1).or.(ic3c4.eq.2)) then
          call c4phot(ihr,notemp,tfix,frdead)
      else 
          call c3phot(ihr,notemp,tfix,frdead)
      endif
c     write(16,*) 'psitop,psitp1,iterh,pho',psitop,psitp1,iterh,psi1,psi2
c
      call interc(ihr)
c     write(16,*) 'psitop,psitp1,iterh,int',psitop,psitp1,iterh,psi1,psi2
      call lfebal(ihr,iday)
c     write(16,*) 'psitop,psitp1,iterh,lfe',psitop,psitp1,iterh,psi1,psi2
      call drips(ihr,lbredo,irrchk,tprecd)
c     write(16,*) 'psitop,psitp1,iterh,dri',psitop,psitp1,iterh,psi1,psi2
	if(idiagn.eq.1)write(6,*)'after calling drips'
c above stm was added by chen, 01/23/90.
      if(lbredo.eq.1)call lfebal(ihr,iday)
      igo=0
      tlfsum=0.
      do1200i=1,itotp1
      do1200j=2,jtot
      if(abs(templf(i,j)-temlf1(i,j)).gt.0.2)igo=1
      tlfsum=tlfsum+abs(templf(i,j)-temlf1(i,j))
 1200 continue
      dtlf=tlfsum/(itotp1*(jtot-1))
      if(noiter.eq.50)write(6,1218)dtlf,ihr,noiter
 1218 format(' mean abs tleaf-tair= ',f6.3,' ihr=',i3,' noiter=',i3)
      if(noiter.gt.50)go to 1219
      if(igo.eq.1)go to 1060
	if(idiagn.eq.1)write(6,*)'before calling rootex'
c above stm was added by chen, 01/23/90.
c------------------------
c     | added parameters for inversion
c     | LMM added invrgl to call for test in rootex Nov/22/93
cBGb
 1219 call rootex(ihr,iday,iter2,isrcgl,psisgl,wtpsis,invrgl,psi2)
cBGe
c------------------------
      if (psitop.le.psi2) psitop = psi2
      if (psitop.gt.0.0) psitop = 0.0
c     write(16,*) 'psitop,psitp1,iterh,roo',psitop,psitp1,iterh,psi1,psi2
 1275 format(3i3,7e10.3)
      call sumflx(ihr)
c     write(16,*) 'psitop,psitp1,iterh,str',psitop,psitp1,iterh,psi1,psi2
	if(idiagn.eq.1)write(6,*)'after calling sumflx'
c above stm was added by chen, 01/23/90.
      psidif=psitop-psitp1
      if(iterh.gt.50)write(6,1265)ihr,iterh,psitop/100.,cpytr
     &,psidif/100.
 1265 format(' ihr=',i3,' iterh=',i3,' psitop=',f6.2,' cpytr=',f8.2
     &,' psidif=',f6.2)
c     if(iterh.gt.50)iterh=0
      if(iterh.gt.50)go to 1300
c  check if stress is limiting
c     if (ihr.eq.2) stop
c     write(16,*) 'psitop,psitp1,iterh,1',psitop,psitp1,iterh,psi1,psi2
      if(psitp1.lt.psi1.and.iterh.eq.0)go to 1255
c     write (16,*) '11111111'
      if(psitop.ge.psi1.and.iterh.eq.0)go to 1300
c     write (16,*) '2222222'
 1255 psimn=(psi1+psi2)/2.
      if(iterh.gt.0)go to 1270
c  psitp1 is previous psitop.   psisv1 is psitot before calc of cpytr
c    from previous iteration. it goes with trsav1 for first coord. of
c    slope estm of cpytr vs psitop relation.
      trsav1=cpytr
      psisv1=psitp1
c     write(16,*) 'psitop,psitp1,2',psitop,psitp1
      if(psitop.ne.psitp1)psitop=(psitop+psitp1)/2.
      if(psitop.eq.psi2)psitop=psitop+10.
c     if(psitop.lt.psi2)psitop=psimn-sqrt((psi2-psitop)/(-psitop)) 
c    &*(psi1-psi2)
      if(psitop.lt.psi2) psitop=psi2 
      if(psitop.gt.0.0) psitop=0.0 
      psitp1=psitop
      iterh=iterh+1
      go to 1055
 1270 if(abs(psitop-psitp1).lt.10.)go to 1300          
c1270 apsi=abs(psitop) 
c     if(abs(psitp1).gt.apsi)apsi=abs(psitp1)	 
c     fraction=(abs(psitop-psitp1))/apsi
c     if(fraction.le.0.01)go to 1300
c     write (16,*) 'ihr,iterh,trsav1,cpytr,psisv1,psitp1,psitop',
c    &ihr,iterh,trsav1,cpytr,psisv1,psitp1,psitop
c     ----------
c     ! it seems strange that this should happen but
c     | psisv1 has equaled psitp1 in the past.
      if (psisv1.eq.psitp1) then
          write(*,*) 'psisv1 = psitp1 ! psitop changed by a bar'
          write(*,*) 'psisv1= ',psisv1,' psitp1= ',psitp1, 'psitop ='
     &                  ,psitop
          psitop = psitop-100
          if (psitop.lt.psi2) psitop=psitop+200
      else
          amx=(trsav1-cpytr)/(psisv1-psitp1)
          b=trsav1-amx*psisv1
          xroot=0.66667*rroot+1./rootsm
          psitop=(-b*xroot+psisum)/(1.+amx*xroot)
c
c psisum in above stm was originally psisum/rootsm, chen, 02/19/90.
          if(iterh.gt.10)psitop=(psitop+psitp1)/2.
c         if(psitop.lt.psi2)psitop=psimn-sqrt((psi2-psitop)/(-psitop))
c    &                                  *(psi1-psi2)/2.
      endif
c
      if (psitop.gt.0.0) psitop=0.0
      if(psitop.lt.psi2) psitop=psi2
c
c     ----------
c     | some times psitop = psitp1  this is a problem. correct by
c     | moving psitop over a bar and try again
      if (psitop.eq.psitp1) then
          psitop = psitop -100
          if (psitop.lt.psi2) psitop = psitop+200
          write(*,*) 'psitp1  was= psitop ! psitop changed by a bar'
          write(*,*) 'psitop= ',psitop,' psitp1= ',psitp1
      endif
c
      trsav1=cpytr
      psisv1=psitp1
      psitp1=psitop
      iterh=iterh+1
c     if (psisv1.eq.psitp1) go to 1300
c     write (16,*) 'iterh,psitop,psitp1,3',iterh,psitop,psitp1,ihr
      go to 1055
 1300 continue
      wdum=wind(ihr)
c *******************************************************************
	if(idiagn.eq.1)write(6,*)'before calling profl2'
c above stm was added by chen, 01/23/90.
      call profl2(ihr,iday,iprofl,itassl,nohrs,irrchk,tirrig
     &,ettot,qtot,tiretq,taerom,taeroh,sauer,tanner,frdead)
c      write (16,*) 'prf akcpy=',akcpy(jzsfc),'jzsfc=',jzsfc,'iday=',iday
c taerom,taeroh for aerodynamic temp. in above stm was added by
c chen, 03/07/90.
	if(idiagn.eq.1)write(6,*)'after calling profl2'
c above stm was added by chen, 01/23/90.
c *******************************************************************
      do1330j=2,jtot
c     write(19,1329)ihr,iter2,j,tair(j),tair1(j),eair(j),eair1(j)
 1329 format(1x,i3,1x,i3,1x,i2,4f6.2)
 1330 continue
      iprofl=1
c  itok is flag that indicates that tair met convergence criteria
c    on last iteration(or eair) even though eair(or tair) didnot,
c    so same tair will be used on next iteration.
      itok=0
      tadfmx=0.
      eadfmx=0.
      dtsum=0.
      desum=0.
      itdif=0
      iedif=0
      do1350j=2,jtot
          tairdf=abs(tair1(j)-tair(j))
          eairdf=abs(eair1(j)-eair(j))
          dtsum=dtsum+tairdf
          desum=desum+eairdf
          if(tairdf.gt.tadfmx)jdfmxt=j
          if(tairdf.gt.tadfmx)tadfmx=tairdf
          if(eairdf.gt.eadfmx)jdfmxe=j
          if(eairdf.gt.eadfmx)eadfmx=eairdf
          if(abs(tair1(j)-tair(j)).gt.0.02) then
	      if (iter2.gt.40) then
		  if (abs(tair1(j)-tair(j)).gt.0.2) then
		     itdif=1
		  endif
	      else 
	          itdif=1
	      endif
	      if (iter2.eq.49) write(*,*)'layer ',j,
     &				' Tair1(j)-tair(j) ',tair1(j)-tair(j)
	  endif
          if(abs(eair1(j)-eair(j)).gt.0.02) then
              if (iter2.gt.40) then
                  if (abs(eair1(j)-eair(j)).gt.0.2) then
                     iedif=1
                  endif
              else
                  iedif=1
              endif
	      if (iter2.eq.49) write(*,*)'layer ',j,
     &				' Eair1(j)-eair(j) ',eair1(j)-eair(j)
	  endif
 1350 continue
c#ifdef PROBE
cc     -----------
cc     | "probe" diagnostic routines
cc     ---------------------
cc     | Get akh after the loop
c      call pend(7)
c      call pbeg7(iday,ihr,iter2)
c      do jz=1,jzsfc
c	  call pget7(akcpy(jz))
c      enddo
c      call pget1(tair1(10),tair(10),tair1(jtot),tair(jtot))
c      call pget2(eair1(10),eair(10),eair1(jtot),eair(jtot))
c      call pget3(psi1,psi2,psitop,psitp1)
c      call pget4(tn(ihr,jzsfc+1),tn(ihrm1,jzsfc+1),
c     &	en(ihr,jzsfc+1),en(ihrm1,jzsfc+1))
c      call pget5(templf(5,2),temlf1(5,2),templf(5,jtot),temlf1(5,jtot))
c#endif
c      if (iter2.gt.40) then
c	  write(6,*) 'iter2=',iter2,' tadfmx=',tadfmx,' jdfmxt=',jdfmxt,
c     &		    ' eadfmx=',eadfmx,' jdfmxe=',jdfmxe
c      endif
      if(itdif.eq.1)go to 1360
      itok=1
      if(iedif.eq.1)go to 1360
      ieok=1
c  set up radiat for output
 1357 ioutpt=1
      kstrt=1
c
c  call photosynthesis subroutine
c
c  the if (idiagn statements were added bu chen 01/23/90
c
      if(idiagn.eq.1)write(6,*)'before calling photks'
      if (ic3c4.ge.10) then
          call photks(ihr,notemp,tfix,frdead)
      else if ((ic3c4.eq.1).or.(ic3c4.eq.2)) then
          call c4phot(ihr,notemp,tfix,frdead)
      else 
          call c3phot(ihr,notemp,tfix,frdead)
      endif
      if(idiagn.eq.1)write(6,*)'after calling photks'
c
      if(idiagn.eq.1)write(6,*)'before calling radiat'
      call radiat(kstrt,coszn2,radabv,fbeam,ihr,ioutpt,
     1          zenang(ihr),iprofl,sunazm(ihr),clump,path,cosdlt)
c                zenang(ihr),sunazm(ihr) above added by Chen, 9/1/89.
c		-------
c		| path, cosdlt added 93/9/31 LMM
c
c
      if(idiagn.eq.1)write(6,*)'after calling radiat'
c
c--------------------->
      go to 1400
c--------------------->
c
c  average previous estimates to get new estm to help convergence
 1360 continue
      dtsum=dtsum/(jtot-1)
      desum=desum/(jtot-1)
      iter2=iter2+1
c  nnsum is the number of consecutive tair and eair values that are
c    averaged together to hasten convergence. during condensation
c    nnsum needs to be fairly large especially at the onset of cond.
c TEMPORARY SHOULD BE 3 not 12 Note: i successive iterations junmp from 
c     | cond to no cond then nnsum changeing from 12 to 3 is believed to
c     | cause trouble.  LMM JMN 94/11/8
      nnsum=6
      do1362j=2,jtot
      if(contot(j).ge.0.)go to 1362
      nnsum=6
 1362 continue
      if(ihr.eq.1.and.iday.eq.1)nnsum=6
c
      if(iter2.ge.70)then
      write(6,*)' day ihr iter jdfmxt tadfmx dtsum jdfmxe eadfmx'
     &,' desum nnsum'
      write(6,1365)iday,ihr,iter2,jdfmxt,tadfmx,dtsum,jdfmxe,eadfmx
     &,desum,nnsum
 1365 format(1x,i3,1x,i3,1x,i4,2x,i3,2x,f6.2,1x,f6.2,1x,i4,1x,f6.2,1x
     &,f6.2,2x,i3)
      goto 1357
      endif
      do1364j=2,jtot
      tairx(iter2,j)=tair(j)
      eairx(iter2,j)=eair(j)
      if(iter2.le.nnsum)go to 1364
      smtair(j)=smtair(j)-tairx(iter2-nnsum,j)
      smeair(j)=smeair(j)-eairx(iter2-nnsum,j)
 1364 continue
      den=nnsum
      if(iter2.le.nnsum)den=iter2
      do1370j=2,jtot
      smtair(j)=smtair(j)+tair(j)
      tdum=smtair(j)/den
c      if(itdif.eq.1)tair(j)=(tdum+tair1(j))*.5
      if (iter2.gt.nnsum.and.abs(tair1(j)-tdum).gt.2.0) then
	  if (tdum.lt.tair1(j) ) then
	      tdum = tair1(j)-2.0
	  else
	      tdum = tair1(j)+2.0
	  endif
      endif
      tair(j)=tdum
      tair1(j)=tair(j)
      smeair(j)=smeair(j)+eair(j)
      edum=smeair(j)/den
c  susan took this out cause doint this to both tair and eair caused
c    cycling and could stop convergence.
c     if(iedif.eq.1)eair(j)=(edum+eair1(j))*.5
c  so lets just set edum=eair
      if (iter2.gt.nnsum.and.abs(eair1(j)-edum).gt.2.0) then
	  if (edum.lt.eair1(j) ) then
	      edum = eair1(j)-2.0
	  else
	      edum = eair1(j)+2.0
	  endif
      endif
      eair(j)=edum
      eair1(j)=eair(j)
 1370 tbar1=tbar
c  average soil sfc temp from last 2 iterations to get new estm.
c  consecutive eddy diffusivities are averaged in profl2
      if(iter2.eq.2)tsfc1=tsfc
      tsfc=(tsfc1+tn(ihr,jzsfc+1))*.5
      tsfc1=tn(ihr,jzsfc+1)
c  ave consecutive ecpys values to help convergence
      if(ihr.eq.1.and.iday.eq.1)ecpys2=ecpys
      ecpys=(ecpys+ecpys2)*.5
      ecpys2=ecpys
c     write (16,*) 'from go to 1050'
      go to 1050
 1400 continue
c#ifdef PROBE
cc     ---------------------
cc     | Get akh after the loop
c      call pend(7)
c      call pbeg7(iday,ihr,2)
c      do jz=1,jzsfc
c	  call pget7(akcpy(jz))
c      enddo
c#endif
c      if (iday.eq.2.and.ihr.eq.19) goto 9900
c  contot(j) is accumulated condensate on leaf in
c    layer j and is zeroed in fixed data part of program.
c  ihrwet(j) is accumulated hours of wetness and iwetam and iwetpm
c    are zeroed before hourly loop and daily values. include precip
c    intercepted also by checking pint(j)
      concpy=0.
      do1225j=2,jtot
      do1220i=1,itotp1
      contot(j)=contot(j)+gevap(i,j)*dt/(alam*4.18)
c     | dt was hard coded as 3.6/1000 fixed 94/11/23
c  accum cond in grams/m**2
      if(contot(j).gt.0.)contot(j)=0.
 1220 continue
      if(contot(j).lt.0.0.or.pint(j).gt.0.)ihrwet(j)=ihrwet(j)+1
      if(timloc(ihr)-12.)1240,1240,1245
 1240 if(contot(j).lt.0.0.or.pint(j).gt.0)iwetam(j)=iwetam(j)+1
      go to 1250
 1245 if(contot(j).lt.0.0.or.pint(j).gt.0.)iwetpm(j)=iwetpm(j)+1
 1250 continue
      concpy=concpy+contot(j)
 1225 continue
      do1510j=2,jtot
      zht=zdh(j)*h
 1500 format(1x,i2,1x,f5.3,f6.2,f7.1)
 1510 continue
c  apparent vis nir and thermal view of canopy from above.
      noang=5
      do1512irt=1,9
 1512 irang(irt)=0.
c  set up view angles
c      do2010irangl=1,noang
c      tcpyir(irangl)=0.
c      do1950k=1,2
c 1950 radlay(k,irangl)=0.
c      dang=90./noang
c      irang(irangl)=(irangl-1)*dang+dang/2.
c      anglir=irang(irangl)*pid180
cc  calc weighting factors for each layer
c      cl=df
c      wtsum=0.
c      do2000j=2,jtot
c      jj=jtot+2-j
c      wtir(jj,irangl)=exp(-.5*(cl-df)/cos(anglir))-exp(-.5*cl/cos(anglir
c     &))
c      wtsum=wtir(jj,irangl)+wtsum
c 2000 cl=cl+df
c 2010 wtir(1,irangl)=1.-wtsum
c above 16 stms were commented by Chen, 9/18/89.
c
c ******* cupradazm insert begin 3 : calculating wtir *****************
c Chen, 9/18/89.
c weighting factor wtir depends also on view azimuth, therefore do loop
c for iphi was added, and modiffications were made bellow.
c following 3 statements were adde by Chen, 05/24/89.
c
	wazmv=2.*pi/noazmv
	do 2010 iphi=1,noazmv
	    phi=wazmv*(iphi-.5)
	    do2010irangl=1,nozenv
		anglir=viewzn(irangl)*pid180
		irang(irangl)=viewzn(irangl)
c	        -------------
c		| Add pathv - View path length which takes into account
c		| 	      slope and aspect.  93/9/9
c		| These are the same equations as calcpath.f but for
c		| view angle instead of sun angle.
c		| 94/2/10 use Shinsuke's solution
c
		if (anglir .lt. pid2*.998) then
c		    
		    if (phi .eq. 0.0) then
			write(*,*)'CUMAIN do loop 2010 phi = 0.0'
			stop
		    endif
c
		    pathv = cos(anglir)*cos(slope)+sin(anglir)*sin(slope)
     &				*cos(aspect-phi) 
		    if (pathv.eq.0.) pathv=1.0e-11
		    pathv=1./pathv
		    if (pathv.le.0.0.or.pathv.gt.1.0e10) then
			pathv = -1
		    endif
		else
c		    ------------
c		    | View below horizon
		    pathv = -1.0
		endif
c 		-------------
c		| calc weighting factors for each layer
		cl=df
		wtsum=0.
		x=xintv(irangl,iphi)
		do2000j=2,jtot
		    jj=jtot+2-j
		    if (pathv.lt.-.99) then
			wtir(jj,irangl,iphi) = 0.0
		    else
			wtir(jj,irangl,iphi)=exp(-clump*x*(cl-df)*
     &				pathv) -exp(-clump*x*cl*pathv)
		    endif
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
		    wtsum=wtir(jj,irangl,iphi)+wtsum
 2000		    cl=cl+df
                if (pathv.lt.-.99) then
                    wtir(1,irangl,iphi) = 0.0
                else
                    wtir(1,irangl,iphi)=1.-wtsum
                endif
 2010           continue
c
c	do 2006 irangl=1,nozenv
c2006	write(6,2007) (xintv(irangl,iphi),iphi=1,noazmv)
c2007	format(10f7.4)
c	do 2008 iphi=1,noazmv
c	do 2008 j=2,jtot
c2008	write(6,2009) iphi,j,(wtir(j,irangl,iphi),irangl=1,nozenv)
c2009	format(i2,i3,7f8.4)
c ******* cupradazm insert end   3 : calculating wtir *****************
      tsum2=0.
      rsum2=0.
      htot=0.
      evtot=0.
c  calc canopy and layer averages for evap,rs,etc with signatures
      do2200j=2,jtot
      tsum1=0.
      rsum1=0.
      hsum1=0.
      esum1=0.
      evapg(j)=0.
      heatg(j)=0.
      do2111k=1,2
      rsum3(k)=0.
 2111 continue
      do2150i=1,itotp1
c  add up conductances to get cpy stom resistance
      rsum1=rsum1+frarea(i,j)/rsleaf(i,j)
      hsum1=hsum1+heat(i,j)*frarea(i,j)
      esum1=esum1+evap(i,j)*frarea(i,j)
      evapg(j)=evapg(j)+gevap(i,j)
      heatg(j)=heatg(j)+gheat(i,j)
      do2130k=1,2
c  add up leaf brightness for vis and nir
      rsum3(k)=rsum3(k)+frarea(i,j)*dstrad(k,i,j)*((rleaf(k,j)+
     &tleaf(k,j))/2.)
 2130 continue
c  add up leaf temperatures
 2150 tsum1=tsum1+templf(i,j)*frarea(i,j)
      tlfave(j)=tsum1
      have(j)=hsum1
      rsave(j)=1./rsum1
      eave(j)=esum1
c  calc vis nir and thermal signatures for canopy
c      do2170irangl=1,noang
c      do2160k=1,2
c 2160 radlay(k,irangl)=radlay(k,irangl)+wtir(j,irangl)*rsum3(k)
c 2170 tcpyir(irangl)=tcpyir(irangl)+wtir(j,irangl)*tlfave(j)
c above 5 stms were commented by Chen, 9/18/89.
      htot=htot+heatg(j)
      evtot=evtot+evapg(j)
      tsum2=tsum2+tlfave(j)
      rsum2=rsum2+rsum1
 2200 continue
      etotwt=evtot/(alam*4.18e-3)
c  dont add soil sfc fluxes to canopy fluxes cause not done in stabality
c     htot=htot+hcpys
c     evtot=evtot+ecpys
      tcpy=tsum2/(jtot-1)
c  calc stom resis of cpy based on ground area
      rscpy=(jtot-1)/(totlai*rsum2)
c  include soil in downward view from above canopy
c      do2240irangl=1,noang
c      do2220k=1,2
c 2220 radlf(k,irangl)=radlay(k,irangl)+wtir(1,irangl)*u(k,1)
c 2240 tcpyir(irangl)=tcpyir(irangl)+wtir(1,irangl)*tsfc
c above 4 stms were commented by Chen, 9/18/89.
c  calc water storage in soil in mm=kg m-2
      delh20=sw-swlast
c                                                  label file write 6
c
c **********************************************************
c layer and canopy summaries from photosynthesis model:
      sum1=0.
      sum2=0.
      sum3=0.
      sum4=0.
      sum5=0.
      sum6=0.
      do1256j=2,jtot
      sumcs=0.
      sumps=0.
      sumci=0.
      sumrd=0.
      do2165i=1,itotp1
c all frarea in a layer add up to 1:
c  csleaf has stomatal and b.l. resistances together
      sumcs=sumcs+csleaf(i,j)*frarea(i,j)
      sumps=sumps+psleaf(i,j)*frarea(i,j)
      sumci=sumci+cileaf(i,j)*frarea(i,j)
      sumrd=sumrd+rdrk(i,j)*frarea(i,j)
 2165 continue
c averages on ground area basis is *df
      cslay(j)=sumcs
      sum1=sum1+cslay(j)
      rslay(j)=40./cslay(j)
      pslay(j)=sumps
      sum2=sum2+pslay(j)
      cilay(j)=sumci
      sum3=sum3+cilay(j)
      sum4=sum4+fehist(j)
      rdlay(j)=sumrd
      sum5=sum5+rdlay(j)
      sum6=sum6+ecompl(j)
c
c
 1256 continue
c
      cscpy=sum1/(jtot-1)
      pscpyl=sum2/(jtot-1)
c     ----------------------
c     | total lai and df include both live and dead lai.
c     | ps, rd and rs are ona  green leaf basis.  As a result
c     | we multiply df by the fraction live lai to get the
c     | living df. LMM 94/9/6
c     ------------------------------------------
c     | This is now taken care of in the photosynthesis routine, so
c     | I have taken the frlive out of pscpyg and rdcpy below
c     | MCA 4/26/95
c     ------------------------------------------      
c      pscpyg=sum2*df*frlive(iday)
      pscpyg=sum2*df
      cicpy=sum3/(jtot-1)
c      rdcpy=sum5*df*frlive(iday)
      rdcpy=sum5*df
c ********************************************************************
c  calc dry wt increment and maintenance and growth respiration
	if(idiagn.eq.1)write(6,*)'before calling drywt'
c above stm was added by chen, 01/23/90.
      call drywt(tcpy,ihr)
c
c  z(jz) and zmid(jz) are neg up pos down in profl2 subprog.
c    reverse them for output only-reverse back later.
      do4005jz=1,jzbot
      z(jz)=-z(jz)
 4005 zmid(jz)=-zmid(jz)
c
c
      if(iwrite(5,4).eq.1)write(21,4010)icumdy,ihr
 4010 format(' 1504',i3,i2,4x,/14x,' height  zdh   contot  etotw vpd
     1',' qcond  tlfave econd pslay ')
      if(iwrite(5,5).eq.1)write(21,4012)icumdy,ihr
 4012 format(' 1505',i3,i2,4x,/14x,' rndiv  pint   tran   evapg  heatg '
     1,' tair     u    eair    rh   ')
c
      jtotm1=jtot-1
      do4020jz=1,nlabcy
c  get sat vp for rh output
      esout=6.108*10**(7.5*tn(ihr,jz)/(237.3+tn(ihr,jz)))
      relout=en(ihr,jz)/esout
      if(jz.eq.1)rhair=relout
      vpd=(1.-relout)*esout
      relout=100.*relout
      if(iwrite(5,4).eq.1)write(21,4018)icumdy,ihr,jz,z(jz),vpd,qcond(jz
     1),econd(jz)
 4018 format(' 2504',i3,i2,2x,i2,  f7.2,3(7x),2f7.2,7x,2f7.2)
      if(iwrite(5,5).eq.1)write(21,4019)icumdy,ihr,jz,et(jz),q(jz)
     1,tn(ihr,jz),uprof(jz),en(ihr,jz),relout
 4019 format(' 2505',i3,i2,2x,i2,3(7x),6f7.2)
 4020 continue
c  jzcpy=jtot-1+nlabcy
      do4015jz=nlabcy+1,jzcpy
      esout=6.108*10**(7.5*tn(ihr,jz)/(237.3+tn(ihr,jz)))
c  subscript for canopy layers only.
      jout=jzcpy-jz+2
      relout=100.*en(ihr,jz)/esout
      vpd=(1.-relout/100.)*esout
      if(iwrite(5,4).eq.1)write(21,4011)icumdy,ihr,jz,z(jz),zdh(jout)
     1,contot(jout),etotw(jout),vpd,qcond(jz),tlfave(jout),econd(jz),
     2pslay(jout)
 4011 format(' 2504',i3,i2,2x,i2,  9f7.2)
      trout=0.
      do4080i=1,itotp1
 4080 trout=trout+tran(i,jout)*frarea(i,jout)
      trout=trout*df
      if(iwrite(5,5).eq.1)write(21,4013)icumdy,ihr,jz,rndiv(jout),
     1pint(jout),trout,evapg(jout),heatg(jout),tair(jout),uprof(jz),
     2en(ihr,jz),relout
 4013 format(' 2505',i3,i2,2x,i2,  9f7.2)
 4015 continue
      do4030jz=jzcpy1,jzsfc
c  get sat vp for rh output
      esout=6.108*10**(7.5*tn(ihr,jz)/(237.3+tn(ihr,jz)))
      relout=en(ihr,jz)/esout
      vpd=(1.-relout)*esout
      relout=100.*relout
      if(iwrite(5,4).eq.1)write(21,4018)icumdy,ihr,jz,z(jz),vpd,qcond(jz
     1),econd(jz)
      if(iwrite(5,5).eq.1)write(21,4021)icumdy,ihr,jz,tn(ihr,jz),uprof(j
     1z),en(ihr,jz),relout
 4021 format(' 2505',i3,i2,2x,i2,5(7x),4f7.2)
 4030 continue
      jzsfc1=jzsfc+1
      do4040jz=jzsfc1,jzbot
      idepth=jz-jzsfc1+1
c  output wcond(jz) the liq water flux in soil as mg m-2 s-1
      watcon=wcond(jz)*1.e6
      if(watcon.lt.-999.99)watcon=-999.99
      if(watcon.gt.9999.99)watcon=9999.99
      pnbar=pn(ihr,jz)/100.
      if(pnbar.lt.-999.99)pnbar=-999.99
      wt100=wt(ihr,jz)*100.
      if(iwrite(5,4).eq.1)write(21,4032)icumdy,ihr,jz,z(jz),qcond(jz)
     1,watcon
 4032 format(' 2504',i3,i2,2x,i2,  f7.2,4(7x),f7.2,7x,2f7.2)
      proot=froot(jz)*100.
      rupout=rootup(jz)*dt
      if(iwrite(5,5).eq.1)write(21,4033)icumdy,ihr,jz,z(jz),proot,rupout
     1,tn(ihr,jz),wstor(jz),pnbar,wt100
 4033 format(' 2505',i3,i2,2x,i2,  f7.2,2f7.2,2(7x),4f7.2)
 4040 continue
c  MCA water printout
c      do jz=jzsfc1,jzbot
c        write(6,*)jz,wt(ihr,jz)
c        write(6,*)wt(ihr,jz),pn(ihr,jz)
c      end do      
c  output layer properties
c  output eddy diff in m**2/s and atmos conductances in m/s by
c    dividing by 1200. soil cond in j m-1 s-1 k-1.
c  output heat cap of cpy+air is kj m-3 k-1 and j m-2 s-1 k-1.
c    soil in mj m-3 k-1 and j m-2 s-1 k-1.
      if(iwrite(5,6).eq.1)write(21,4048)icumdy,ihr
 4048 format(' 1506',i3,i2,4x,/14x,' zmid   akcpso cpcpso  akh     cp  '
     1,' rsave  rhleaf ',' akw    cw   ')
      do4050jz=1,nlabcy
      ak1=akcpy(jz)/1200.
      ak1 = akcpy(jz)/1.2
      ak2=akh(jz)/1200.
      cp1=cpcpy(jz)/1000.
      if(iwrite(5,6).eq.1)write(21,4052)icumdy,ihr,jz,zmid(jz),ak1,cp1,
     1ak2,cp(jz)
 4052 format(' 2506',i3,i2,2x,i2,  9f7.2)
 4050 continue
      do4060jz=nlabcy+1,jzcpy
      ak1=akcpy(jz)/1.200
      ak2=akh(jz)/1200.
      cp1=cpcpy(jz)/1000.
      jout=jzcpy-jz+2
      if (rsave(jout).gt.9999.99) then
        rsaout = 9999.99
      else
        rsaout = rsave(jout)
      endif
      if(iwrite(5,6).eq.1)write(21,4052)icumdy,ihr,jz,zmid(jz),ak1,cp1,
     1ak2,cp(jz),rsaout,rhleaf(jout)
 4060 continue
      jscpy1=jzcpy+1
      do4065jz=jzcpy1,jzsfc
      ak1=akcpy(jz)/1.200
      ak2=akh(jz)/1200.
      cp1=cpcpy(jz)/1000.
      if(jz.ne.jzsfc.and.iwrite(5,6).eq.1)write(21,4052)icumdy,ihr,jz,
     1zmid(jz),ak1,cp1,ak2,cp(jz)
      if(jz.eq.jzsfc)akwsfc=akw(jzsfc)*1.e6
      if(jz.eq.jzsfc.and.iwrite(5,6).eq.1)write(21,4053)icumdy,ihr,jz
     1,zmid(jz),ak1,cp1,ak2,cp(jz),akwsfc
 4053 format(' 2506',i3,i2,2x,i2,  5f7.2,14x,f7.2)
 4065 continue
      jzsfc1=jzsfc+1
      do4070jz=jzsfc1,jzbot
      cpout=cpsoil(jz)*1.e-6
c  output akw and cw in mg s m-4 units.
      cwout=cw(jz)*1.e6
      akout=akw(jz)*1.e6
      if(akout.gt.9999.99)akout=9999.99
      if(iwrite(5,6).eq.1)write(21,4062)icumdy,ihr,jz,zmid(jz),aksoil(jz
     1),cpout,akh(jz),cp(jz),akout,cwout
 4062 format(' 2506',i3,i2,2x,i2,  5f7.2,2(7x),2f7.2)
 4070 continue
c
c *********************************************************8
      if(iwrite(5,7).eq.1)write(21,4085)icumdy,ihr
 4085 format(' 1507',i3,i2,4x,/,14x,'   clai  cslay  rslay  pslay  cilay
     &',' fehist  rdlay ecompl ')
      do4071 j=2,jtot
      jj=jtot-j+2
      if (rslay(jj).gt.9999.99) then
        rslout = 9999.99
      else
        rslout = rslay(jj)
      endif
      if(iwrite(5,7).eq.1)write(21,4090)icumdy,ihr,jj,clai(jj),
     &cslay(jj),rslout,pslay(jj),cilay(jj),fehist(jj),rdlay(jj)
     &,ecompl(jj)
 4090 format(' 2507',i3,2i2,2x,8f7.2)
 4071 continue
      if(iwrite(8,3).eq.1)write(21,4095)icumdy,ihr
 4095 format(' 1803',i3,i2,4x,/,14x,' csleaf rsleaf psleaf cileaf rdrk'
     &,'   rhleaf abspar csfclf hsleaf')
      do333ii=1,itotp1
      do222 j=2,jtot
      jj=jtot-j+2
c  calc absorbed par in umol m-2 s-1
      abspar=dstrad(1,ii,jj)*4.6*aleaf(1,jj)
c  output csleaf in mmol m-2 s-1
      cslfot=csleaf(ii,jj)*1000.
      if (rsleaf(ii,jj) .gt.9999.99) then
        rsout = 9999.99
      else
        rsout = rsleaf(ii,jj)
      endif
      if(iwrite(8,3).eq.1)write(21,4105)icumdy,ihr,ii,jj,cslfot,
     &rsout,psleaf(ii,jj),cileaf(ii,jj),rdrk(ii,jj)
     &,rhleaf(jj),abspar,csfclf(ii,jj),hsleaf(ii,jj)*100.
 4105 format(' 2803',i3,3i2,6f7.2,f7.1,2f7.2)
  222 continue
  333 continue
c *******************************************************************
c  reverse sign on z(jz) and zmid(jz) back to neg up and pos down.
c
      do4075jz=1,jzbot
      z(jz)=-z(jz)
 4075 zmid(jz)=-zmid(jz)
c sun add following statements from c Dec/10 to c Dec/10 at 12/10/91
c Dec/10
      do 4077 jz=nlabcy+1,jzsfc
      distz(jz)=abs(z(jz)-z(jz+1))
      akh2(jz)=akcpy(jz)/distz(jz)/1.2
c     akh3(jz)=akcpyold(jz)/distz(jz)/1.2
      akh2nut(jz)=akcpynut(jz)/distz(jz)/1.2
 4077 continue
      difT=tn(ihr,jzsfc)-tn(ihr,jzsfc+1)
c     ustrsn=0.4*uprof(jzcrit)/(alog(abs(zmid(jzcrit)/z0soil)))
c     do 9091 jz=jzcrit,jzsfc
c     akh2nut(jz)=ustrsn*abs(zmid(jz))*0.4*1.35*1000.
c    &/abs(zmid(jz)-zmid(jz+1))
c9091 continue
c     unutr=ustrsn/0.4*(alog(abs(z(jzsfc)/z0soil)))
c     if (iday.eq.1.and.ihr.eq.1) write(9,*) 'z0soil=',z0soil, 
c    &'nlabcy=',nlabcy,' jzsfc=',jzsfc,' jzcrit=',jzcrit
c     if (iday.eq.1.and.ihr.eq.1)  
c    &write(9,*)'idy ihr u-2 akh2 u-1   akh2 uzfc  akh2 phims zdls hcpys
c    &ecpys  dift akh2nut un'
c    &'z(jzsfc-2 to jzsfc)',(z(jz),jz=jzsfc-2,jzsfc)
c     write(9,9096)iday,ihr,(uprof(jz),akh2(jz),jz=jzsfc-2,jzsfc)
c    &,phims,zdls,hcpys,ecpys,difT,akh2nut(jzsfc),unutr
c     write(9,9096)iday,ihr,(akh2(jz),jz=jzsfc-9,jzsfc)
c     write(9,9097)iday,ihr,(akh3(jz),jz=jzsfc-9,jzsfc)
c9096  format('akh2',i2,1x,i2,10(1x,f5.1))
c9097  format('akh3',i2,1x,i2,10(1x,f5.1))
 9096 format(i2,1x,i2,3(1x,f4.3,1x,f5.1),1x,f4.1,1x,f4.2,
     &1x,f6.2,1x,f6.2,1x,f5.1,1x,f5.1,1x,f5.3)
c     write (6,*)'iday',iday,' ihr',ihr,
c    &'distz      ',(distz(jz),jz=jzcrit,jzsfc) 
c     write (9,*)'iday',iday,' ihr',ihr,
c    &' akh2(mm/s)',(akh2(jz),jz=jzcrit,jzsfc) 
c     write (9,*)'iday',iday,' ihr',ihr,
c    &' akh2nut(mm/s)',(akh2nut(jz),jz=jzcrit,jzsfc) 
c Dec/10
c                                                    file write 7
c  
      if(iwrite(7,1).eq.1)write(21,4100)icumdy,ihr
 4100 format(' 1701',i3,i2,4x,/14x,' tsfcso  psilf   tcpy etotwt '
     1,' evtot  htot   hsoil cphstr  hpsi ')
      if(iwrite(7,1).eq.1)write(21,4110)icumdy,ihr,tn(ihr,jzsfc+1),psilf
     1,tcpy,etotwt,evtot,htot,hsoil,cphstr,hpsi
 4110 format(' 2701',i3,i2,4x,9f7.2,2f7.2)
c taerom,taeroh in above 2 stms were added by chen, 03/09/90.
      if(iwrite(7,22).eq.1)write(21,4111)icumdy,ihr
 4111 format(' 1722',i3,i2,4x,/14x,' tareom taeroh sauer  tanner')              
      if(iwrite(7,22).eq.1)write(21,4112)icumdy,ihr,taerom,taeroh,sauer
     &,tanner
 4112 format(' 2722',i3,i2,4x,9f7.2,2f7.2)
      if(iwrite(7,2).eq.1)write(21,4120)icumdy,ihr
c      write (16,*) '4120 akcpy=',akcpy(jzsfc),'jzsfc=',jzsfc,
c     &'iday=',iday
 4120 format(' 1702',i3,i2,4x,/14x,' ustara ustars  phima  phims ',
     1'akcpy1 akcpys hcpys  ecpys  rnsoil')
      if(iwrite(7,2).eq.1)write(21,4121)icumdy,ihr,ustara,ustars,phima,
c    1phims,akcpy(1),akcpy(jzcrit),hcpys,ecpys,rnet(1)
c sun change above statement to following one. 10/9,91.
     1phims,akcpy(1),akcpy(jzsfc),hcpys,ecpys,rnet(1)
      qjzcrt=jzcrit
      if(iwrite(7,3).eq.1)write(21,4122)icumdy,ihr
 4122 format(' 1703',i3,i2,4x,/14x,'qjzcrt  zcrit  psima  psims  '
     1,'zdla   zdls   cpestr drgrav wcpys')
      if(iwrite(7,3).eq.1)write(21,4123)icumdy,ihr,qjzcrt,zcrit,psima,
     1psims,zdla,zdls,cpestr,drgrav,wcpys
 4123 format(' 2703',i3,i2,4x,9f7.2)
 4121 format(' 2702',i3,i2,4x,9f7.2)
      if(iwrite(7,4).eq.1)write(21,4125)icumdy,ihr
c
c etmic=etmm in microns
c trmic=trmm in microns
c pevmic=preev in microns
       etmic=etmm*1000.
 4125 format(' 1704',i3,i2,4x,/14x,' rncpy  hcpy   ecpy   concpy',
     1' mmh20  delh20 drain   filt  etmic ')
      conout=-concpy
      if(iwrite(7,4).eq.1)write(21,4126)icumdy,ihr,rnet(jtot),qcond(2),
     1econd(2),conout,sw,delh20,drain,filt,etmic
 4126 format(' 2704',i3,i2,4x,9f7.2)
c
      if(iwrite(7,26).eq.1)write(21,4151)icumdy,ihr
c
 4151 format(' 1726',i3,i2,4x,/14x,' drain5')
      if(iwrite(7,26).eq.1)write(21,4152)icumdy,ihr,drain5
 4152 format(' 2726',i3,i2,4x,9f7.2)
c
 802  format(' 1727',i3,i2,4x,/14x,' albedo visrfl nirrfl alb2  '
     & ,' visr2  nirr2 ')
 803  format(' 2727',i3,i2,4x,9f7.2)
      if(iwrite(7,27).eq.1) then
	  if (radtop(1,ihr).gt.0.1.and.radtop(2,ihr).gt.0.1) then
	      visrfl = (u(1,jtot)/radtop(1,ihr))*100
	      nirrfl = (u(2,jtot)/radtop(2,ihr))*100
	      albedo = ((u(1,jtot)+u(2,jtot))/
     &			(radtop(1,ihr)+radtop(2,ihr)))*100
          else
	      visrfl = 0.0
	      nirrfl = 0.0
	      albedo = 0.0
	  endif
	  if (radabv(1).gt.0.1.and.radabv(2).gt.0.1) then
	      visr2 =(u(1,jtot)/radabv(1))*100
	      nirr2 =(u(2,jtot)/radabv(2))*100
	      alb2  =((u(1,jtot)+u(2,jtot))/(radabv(1)+radabv(2)))*100
	  else 
	      visr2  = 0.0
	      nirr2  = 0.0
	      alb2   = 0.0
	  endif
	  write(21,802)icumdy,ihr
c The 803 format is now replaced in the line below.
c Considered it again when you run it
	  write(21,*)icumdy,ihr,albedo,visrfl,nirrfl,alb2,visr2,nirr2
      endif
c  output intercepted rain
      preint=0.
      preev=0.
      do4180j=2,jtot
      preev=preev+evimm(j)
      pevmic=preev*1000.
 4180 preint=preint+pint(j)
      trmm=cpytr*dt
      trmic=trmm*1000.
      evsmic=ecpys*dt/2450.
      dpevp=ettot*dt/2450.
      if(iwrite(7,5).eq.1)write(21,4127)icumdy,ihr
 4127 format(' 1705',i3,i2,4x,/14x,' tprecp preint pevmic stem   trmic'
     1,'  evsmic  dpevp  pscpyl ')
      tprout=tprecp*dt
      if(iwrite(7,5).eq.1)write(21,4128)icumdy,ihr,tprout,preint,pevmic
     1,stem,trmic,evsmic,dpevp,pscpyl
 4128 format(' 2705',i3,i2,4x,9f7.2)
c*****************************************************************
c  output water budget values in mm/hr
c  include storage loss for change in temp. of droplets in flight-wm-2
      dpstor=precip(ihr)*(4.1876e6)*(tdropi-tirrig)/(1000.*dt)
      dpvmmh=dpevp*3.6/dt
      pevmmh=pevmic*3.6/dt
      trmmh=trmic*3.6/dt
      evsmmh=evsmic*3.6/dt
      etmmh=etmic*3.6/dt
c  divergence of evap. so tr. and evap from cpy only
      ecpydv=econd(2)-ecpys
      if(iwrite(7,20).eq.1)write(21,4142)icumdy,ihr
 4142 format(' 1720',i3,i2,4x,/14x,' dpvmmh pevmmh trmmh  evsmmh etmmh'
     1,'  ecpydv')
      if(iwrite(7,20).eq.1)write(21,4143)icumdy,ihr,dpvmmh,pevmmh,trmmh
     1,evsmmh,etmmh,ecpydv
 4143 format(' 2720',i3,i2,4x,9f7.2)
c*****************************************************************
c  output intercepted par and solar for canopy
      apar=rnlam(1,jtot)-rnlam(1,1)
      asolr=(rnlam(1,jtot)+rnlam(2,jtot))-(rnlam(1,1)+rnlam(2,1))
      pari=d(1,jtot)+bmflx(1,jtot)-(d(1,1)+bmflx(1,1))
      solri=pari+d(2,jtot)+bmflx(2,jtot)-(d(2,1)+bmflx(2,1))
c  calc light-use efficiencies for solar and par, absorbed and incpted
      if(radabv(1).gt.10.) then
        eluap=pscpyg*44./apar
        eluas=pscpyg*44./asolr
        eluip=pscpyg*44./pari
        eluis=pscpyg*44./solri
      else
        eluap=0.
        eluas=0.
        eluip=0.
        eluis=0.
      endif
      if(iwrite(7,16).eq.1)write(21,4177)icumdy,ihr
 4177 format(' 1716',i3,i2,4x,/14x,' pari   solri  pscpyg'
     1,' apar   asolr  eluap  eluas  eluip  eluis ')
      if(iwrite(7,16).eq.1)write(21,4178)icumdy,ihr,pari,solri,
     1pscpyg,apar,asolr,eluap,eluas,eluip,eluis
 4178 format(' 2716',i3,i2,4x,9f7.2)
c
c     --------
c     | Added iday and frlive LMM 94/9/8
      call soilco2(iday,ihr,co2sol,frlive)
c
      if(iwrite(7,18).eq.1)write(21,4179)icumdy,ihr
 4179 format(' 1718',i3,i2,4x,/14x,' rdcpy  wtsfc  CO2sol')
      if(iwrite(7,18).eq.1)write(21,4181)icumdy,ihr,rdcpy
     &,wt(ihr,jzsfc+1)*100.,co2sol
 4181 format(' 2718',i3,i2,4x,9f7.2)
c
c*****************************************************************
c
c  calc aerodynamic resis to heat and water vapor from fluxes using
c    eqs in monteiths book.
      rhcpy=10000
      if(qcond(2).gt.1.e-5)rhcpy=abs(1200.*(tcpy-temair(ihr))/qcond(2))
      escpy=6.108*10**(7.5*tcpy/(237.3+tcpy))
      rvcpy=10000.
      if(econd(2).gt.1.e-5)rvcpy=abs((1200./.66)*(escpy-vpair(ihr))/
     &econd(2))
      if(rhcpy.gt.9999.)rhcpy=9999.
      if(rscpy.gt.9999.)rscpy=9999.
      if(rvcpy.gt.9999.)rvcpy=9999.
c
      if(iwrite(7,13).eq.1)write(21,4136)icumdy,ihr
 4136 format(' 1713',i3,i2,4x,/14x,' psixy  psitop psisum deld   wsmm  '
     &,' rscpy  rvcpy  rhcpy ')
      pxyout=psixy/100.
      ptpout=psitop/100.
      psmout=psisum/100.
c psisum in above stm was originally psisum/rootsm, chen, 02/19/90.
      deldot=deld*1.e6
      wsmm=wcpys*dt/(alam*4.18e3)
      if(iwrite(7,13).eq.1)write(21,4137)icumdy,ihr,pxyout,ptpout,psmout
     &,deldot,wsmm,rscpy,rvcpy,rhcpy
 4137 format(' 2713',i3,i2,4x,9f7.2)
      qnoitr=noiter
      qiter2=iter2
      qiter3=iter3
      qiterw=iterw
      qloope=loope
      qloopt=loopt
      qloopw=loopw
      qiterh=iterh
      if(iwrite(7,14).eq.1)write(21,4124)icumdy,ihr
 4124 format(' 1714',i3,i2,4x,/14x,' qnoitr qiter2 qiter3 qiterw',
     1' qloope qiterh qloopt qloopw')
      if(iwrite(7,14).eq.1)write(21,4129)icumdy,ihr,qnoitr,qiter2,qiter3
     1,qiterw,qloope,qiterh,qloopt,qloopw
 4129 format(' 2714',i3,i2,4x,9f7.2)
c     if(iwrite(7,10).eq.1)write(21,4130)icumdy,ihr,(irang(l),l=1,noang)
c4130 format(' 1710',i3,i2,4x,/14x,9(' tir',i3))
c      if(iwrite(7,10).eq.1)write(21,4131)icumdy,ihr,(tcpyir(l),l=1,noang
c     1)
c above 2 stm were commented by Chen, 05/02/90.
 4131 format(' 2710',i3,i2,4x,9f7.2)
      if(iwrite(7,11).eq.1)write(21,4132)icumdy,ihr,(irang(l),l=1,noang)
 4132 format(' 1711',i3,i2,4x,/14x,9(' vis',i3))
      if(iwrite(7,11).eq.1)write(21,4133)icumdy,ihr,(radlf(1,l),l=1,noan
     1g)
 4133 format(' 2711',i3,i2,4x,9f7.2)
      if(iwrite(7,12).eq.1)write(21,4134)icumdy,ihr,(irang(l),l=1,noang)
 4134 format(' 1712',i3,i2,4x,/14x,9(' nir',i3))
      if(iwrite(7,12).eq.1)write(21,4135)icumdy,ihr,(radlf(2,l),l=1,noan
     1g)
 4135 format(' 2712',i3,i2,4x,9f7.2)
      if(iwrite(7,17).eq.1) write(21,4138) icumdy,ihr
4138  format(' 1717',i3,i2,4x,/14x,' tirrig ettot  qtot   dpstor'
     &,' tiretq')
      if(iwrite(7,17).eq.1) write(21,4139) icumdy,ihr,tirrig,ettot,
     &qtot,dpstor,tiretq
4139  format(' 2717',i3,i2,4x,9f7.2)
 4140 continue
c  call scaling subroutine define non-rect hyper. parameters
      pmax=45.
      znr=.7
      pce=.062
      pcea=.040
c
      call scaleh(icumdy,ihr,apar,pari,pmax,znr,pce,pcea,rdcpyd
     &,frlive(iday))
c
c     ibidir=0
c bidirectional thermal part was removed and a new soubroutine bdrtm
c created by Chen, 9/18/89.
c	write(6,*)'tair,tcpy,tsfc=',temair(ihr),tcpy,tn(ihr,jzsfc+1)
	if(idiagn.eq.1)write(6,*)'before calling bdrtm'
c above stm was added by chen, 01/23/90.
      if(ibidir.eq.1) then
c	if(.NOT.((fbeam1(2,ihr).lt. 0.01).or.(coszen(ihr).lt.0.01))) then
	     call bdrtm(ihr,factir,tn(ihr,jzsfc+1),ipp,nvzenp,
     1			vwzenp, tcpyir,tcpyap,emiscp,path(ihr))
c	endif
      endif
c*****************************************************************
c  Output for Murty's special file
c     write(30,4182)cumhr,timloc(ihr),tcpyap(1,1),tcpyap(4,1), 
c    & co2sol,pscpyg,hsoil,qcond(2),econd(2),rnet(jtot)
c4182 format(10(f7.2,2x))
c*****************************************************************     
c
c  calc dist of light over all leaves in cpy 
c
      do 5400irada=1,20
 5400 freqr(irada)=0.
c
      do5600 j=2,jtot
      do5500 i=1,itotp1
      irada=4.6*dstrad(1,i,j)*aleaf(1,j)/100.
      if(irada.gt.20)irada=20
      if(irada.lt.1)irada=1
      freqr(irada)=freqr(irada)+frarea(i,j)/(jtot-1)
 5500 continue
 5600 continue
c
c      if(ihr.eq.10)write(6,*)freqr
c
      do5700j=2,jtot
 5700 pilast(j)=pint(j)
      pilast(1)=0.
      swlast=sw
c  save last hour of the day(nohrs) in subscript mh to use
c    for getting initial profiles of tair and templf for hr 1
c    of next day.
      if(ihr.ne.nohrs)go to 6900
      temair(mh)=temair(nohrs)
      vpair(mh)=vpair(nohrs)
      temsol(mh)=temsol(nohrs)
      wind(mh)=wind(nohrs)
      watsol(mh)=watsol(nohrs)
      tsoil(mh)=tsoil(nohrs)
      precip(mh)=precip(nohrs)
      irrchk(mh)=irrchk(nohrs)
      timloc(mh)=timloc(nohrs)
      wpond(mh)=wpond(nohrs)
      do6899jz=1,50
      tn(mh,jz)=tn(nohrs,jz)
      en(mh,jz)=en(nohrs,jz)
      pn(mh,jz)=pn(nohrs,jz)
      wt(mh,jz)=wt(nohrs,jz)
      wnu(mh,jz)=wnu(nohrs,jz)
      wnl(mh,jz)=wnl(nohrs,jz)
 6899 continue
      esave(1,mh)=esave(1,nohrs)
      esave(2,mh)=esave(2,nohrs)
      esave(3,mh)=esave(3,nohrs)
 6900 continue
c-------------------
cBGb
c ********* cupidg insert begin 3 : (Chen, 01/18/90) **********************
        if(igl.eq.1)goto 6905
c ********* cupidg insert end   3 : (Chen, 01/18/90) **********************
cBGe
c-------------------
      dtime=dtime+dt
      tcpyd=tcpyd+tcpy*dt
      tsfcsd=tsfcsd+tn(ihr,jzsfc+1)*dt
      hsoild=hsoild+hsoil*dt
      hcpysd=hcpysd+hcpys*dt
      rnsold=rnsold+rnet(1)*dt
      wcpysd=wcpysd+wcpys
      rncpyd=rncpyd+rnet(jtot)*dt
      hcpyd=hcpyd+qcond(2)*dt
      ecpyd=ecpyd+econd(2)*dt
      if(abs(concpy).gt.conddy)conddy=-concpy
      swday=swday+sw*dt
      delwd=delwd+delh20
      draind=draind+drain
      dran5d=dran5d+drain5
      filtd=filtd+filt
      etmmd=etmmd+etmm
      parid=parid+pari*dt
      solrid=solrid+solri*dt
      apard=apard+apar*dt
      asolrd=asolrd+asolr*dt
      psyngd=psyngd+pscpyg*dt
      preevd=preevd+preev
      trmmd=trmmd+trmm
      vpaird=vpaird+vpair(ihr)*dt
      taird=taird+temair(ihr)*dt
      if(temair(ihr).gt.tairmx)tairmx=temair(ihr)
      if(temair(ihr).lt.tairmn)tairmn=temair(ihr)
      if(rhair.gt.rhmax)rhmax=rhair
      if(rhair.lt.rhmin)rhmin=rhair
      winddy=winddy+wind(ihr)*dt
      solard=solard+solar1*dt
      pard=pard+radtop(1,ihr)*dt
      precpd=precpd+precip(ihr)
c  sum vbls for dry wt and respiration
      rmstmd=rmstmd+rmstem*dt
      rmlfd=rmlfd+rmleaf*dt
      rmrotd=rmrotd+rmroot*dt
      rmgrd=rmgrd+rmgr*dt
      rmtotd=rmtotd+rmtot*dt
      rgtotd=rgtotd+rgtot*dt
      psmrmd=psmrmd+psmrm*dt
      dmincd=dmincd+dminc*dt
c-------------------
cBGb
6905	continue
cBGe
c-------------------
c  iwrite(7,15) is in subroutine penmon
	if(idiagn.eq.1)write(6,*)'before calling penmon'
c above stm was added by chen, 01/23/90.
c	-----------
c	| pemmon call does not take into account slope and 
c	| aspect 93/8/31 LMM
      call penmon(ihr,icumdy,etpm)
c-------------------
cBGb
c ********* cupidg insert begin 4 : (Chen, 01/18/90) **********************
        if(igl.eq.1)goto 6906
c ********* cupidg insert end   4 : (Chen, 01/18/90) **********************
      etpmd=etpmd+etpm
6906    continue
cBGe
c-------------------
c
c check if newrap temperature and v.p. solutions exceed maximum
c permissable value potchk:
c
      jj=0
      do2005jz=1,jzbot
      if(ichkt(jz).eq.1)then
      jj=jj+1
      ickout(jj)=jz
      endif
 2005 continue
      if(jj.ne.0)write(*,2205)'newrap temp gt tempchk for layers ',
     &(ickout(ij),ij=1,jj)
 2205 format(1x,a,/,26i3)
      jj=0
      do2015jz=1,jzsfc1
      if(ichke(jz).eq.1)then
      jj=jj+1
      ickout(jj)=jz
      endif
 2015 continue
      if(jj.ne.0)write(*,2205)'newrap v.p. gt vpchk for layers ',
     &(ickout(ij),ij=1,jj)
c
c                                            end hourly loop
c
c ********* energy balance check layer by layer **********************
c     skip the first hour, because qcap and ecap not available.
      if(iday.eq.1.and.ihr.eq.1)goto7000
      ibalck=1 
      qdiv(1)=0.
      ediv(1)=0.
c     write(19,*)' iday ihr jz j rndivJ qdivJZ edivJZ qJZ',
c    &' etJZ qcapJZ ecapJZ qcondJZ econdJZ enbal '
      do7007jz=2,jzbm1
      j=jtot-jz+nlabcy+1
      if(j.eq.1)then
         rndum=rndiv(j)
         dstdum=dstng(j)
      endif
      if(j.le.1)then
         j=1
         rndiv(j)=0.
         dstng(j)=0.
      endif
      if(jz.eq.(jtot-1)+nlabcy+(nlbcpy+1)+1)then
         rndiv(j)=rndum
         dstng(j)=dstdum
      endif
      qdiv(jz)=qcond(jz-1)-qcond(jz)
c  ediv in not appropriate in soil
      ediv(jz)=0.
      if(jz.le.jzsfc1)ediv(jz)=econd(jz-1)-econd(jz)
c  rndiv set to heat released from droplets with irrig above canopy
c  to make enbal=0
      if(j.gt.jtot)then
         enbal=et(jz)+q(jz)-qdiv(jz)-ediv(jz)-qcap(jz)
         enbal2=0.
         goto 7015
      endif
      if (iresdu.eq.1) then 
          if (jz.eq.jzsfc) then
              enbal=rndiv(j)-qdiv(jz)-ediv(jz)-qcap(jz)+rnet(1)/4.
          else if (jz.eq.jzsfc+1) then 
	      enbal=rndiv(j)-qdiv(jz)-ediv(jz)-qcap(jz)-rnet(1)/4.
          else
	      enbal=rndiv(j)-qdiv(jz)-ediv(jz)-qcap(jz)
          endif
      else
	  enbal=rndiv(j)-qdiv(jz)-ediv(jz)-qcap(jz)
      endif
      if(jz.eq.jzsfc+1)enbal=enbal-pint1(1)*4187.6*(tn(ihr,jzsfc+1)
     &-twater)/dt
c     if(jz.eq.jzsfc+1)write(19,*)pint1(1),twater,tn(ihr,jzsfc+1)
      enbal2=dstng(j)-qdiv(jz)-ediv(jz)-qcap(jz)
 7015 continue
      if(ibalck.eq.0)go to 7007
c     write(19,7008)iday,ihr,jz,j,rndiv(j),qdiv(jz),ediv(jz),q(jz),
c    &2.45*et(jz),qcap(jz),ecap(jz),qcond(jz),econd(jz),enbal
 7008 format(4i2,10f7.2)
      if(abs(enbal).gt.5.0)then
      write(6,7006)' hour',ihr,', layer',jz,
     &'imbalance is ',enbal,' w/m2',enbal2
c      write(15,7006)' hour',ihr,', layer',jz,
c     &'imbalance is ',enbal,' w/m2',enbal2
      endif
 7006 format(1x,a,i3,a,i3,1x,a,f6.2,a,f6.1)
 7007 continue
c *********** energy balance check over canopy : *******************
c  include storage loss for change in temp. of droplets in flight
c    dpstor calc'd earlier near stm 4128.
c
 7777 format(
     &'v    |CPYb | rncpy  hcpy    ecpy   hsoil cphstr dpstor|SOILb|',
     &' rnsoil hcpys  ecpys |DIFFb| evtot | Edif| htot | Hdif | Hdif2' )
c	write(15,7777)
c
c
 7778 format('V',i2,i2,17F7.2)
      enbal=rnet(jtot)+dpstor-qcond(2)-econd(2)-hsoil-cphstr
	balsol=rnet(1)-ecpys-qcond(jzsfc)-hsoil
	baldif=(rnet(jtot)-rnet(1))-(qcond(2)-qcond(jzsfc))-evtot
	edif=(econd(2)-ecpys)-evtot 
	htot2=0.0
	do jz=nlabcy+1,jzcpy
	    htot2=htot2+q(jz)-waterg(jtot-(jz-(nlabcy+1)))
	enddo
	hdif=(qcond(2)-qcond(jzsfc))-htot
	hdif2=(qcond(2)-qcond(jzsfc))-htot2
c	write(15,7778)iday,ihr,enbal,rnet(jtot),qcond(2),econd(2),
c     &	hsoil,cphstr,dpstor,balsol,rnet(1),qcond(jzsfc),ecpys,
c     &  baldif,evtot,edif,htot,hdif,hdif2
      if(abs(enbal).gt.15.)then
      write(6,7010)' iday=',iday,'canopy imbalance is'
     %,enbal,'hour',ihr
	write(6,*)
c     12341 345672 345673 345674 345675 
c      write(15,7010)' iday=',iday,' canopy imbalance is '
c     %,enbal,'hour',ihr
c     write(6,*)rnet(jtot),qcond(2),econd(2),hsoil,cphstr
      ibalck=1
      endif
 7010 format(1x,a,i4,1x,a,1x,f7.2,1x,a,1x,i3)
c
c  set ibalck=0 if we do not want profile output of imbalances, comment
c    the next line if program is to run as written.
      ibalck=1
c
       if(irrchk(ihr).eq.1)then
       etsum=0.
       qsum=0.
       do7004jz=1,nlabcy
       rh=en(ihr,jz)/esat(jz)*100.
c      write(15,7003)ihr,jz,tn(ihr,jz),rh,et(jz),q(jz)
       etsum=etsum+et(jz)
 7004  qsum=qsum+q(jz)
c      write(16,7005)ihr,twater,etsum,qsum
       endif
 7003  format(1x,2i3,2x,4f6.2)
 7005  format(1x,i3,3f7.2)
c
c
 7000 continue
c-------------------
cBGb
c ********* cupidg insert begin 5 : save   (Chen, 01/18/90) **************
        if(invrgl.eq.0) goto 7030
        if(igl.ne.0) goto 7029
        do 7021 j=1,jtot
        savet(j)=tair(j)
        savee(j)=eair(j)
        savepi(j)=pilast(j)
        savect(j)=contot(j)
        do 7021 i=1,itotp1
        savetl(i,j)=templf(i,j)
7021    continue
        do 7022 jz=1,50
        saveak(jz)=akh1(jz)
        savewt(jz)=wtlast(jz)
        saveqc(jz)=qcond(jz)
        sauwtz(ihrgl-1,jz)=wt(ihrgl-1,jz)
c Sun added following statement in 4/14/92
        sautnz(ihrgl-1,jz)=tn(ihrgl-1,jz)
7022    continue
        savept=psitop
        savet1=psitp1
        savets=tsfc
        savts1=tsfc1
        savpsi=psima
        savzdl=zdlast
        savecp=ecpys
        savswl=swlast
7029    continue
        write(6,*)'ihre,nohrs=',ihre,nohrs
        if(ihre.ne.nohrs) goto 9901
7030    continue
c ********* cupidg insert end   5 : save   (Chen, 01/18/90) **************
cBGe
c-------------------------
c test if day sums up all right:
      if(abs(24.-dtime/3600.).gt.0.1)write(6,7001)iday,dtime/3600.
 7001 format(' iday ',i3,' does not add to 24 hours. dtime is ',f5.2)
      tcpyd=tcpyd/dtime
      tsfcsd=tsfcsd/dtime
      hsoild=hsoild/dtime
      hcpysd=hcpysd/dtime
      rnsold=rnsold/dtime
      wsmmd=wcpysd*3600./(alam*4.18e3)
      wcpysd=wcpysd*3600./dtime
      rncpyd=rncpyd/dtime
      hcpyd=hcpyd/dtime
      ecpyd=ecpyd/dtime
      conddy=conddy*1.e-3
      swday=swday/dtime
      vpaird=vpaird/dtime
      taird=taird/dtime
      winddy=winddy/dtime
      parid=parid*1.e-6
      solrid=solrid*1.e-6
      apard=apard*1.e-6
      asolrd=asolrd*1.e-6
      psyngd=psyngd*.044*1.e-3
      pard=pard*1.e-6
      solard=solard*1.e-6
c  daily resp in g c02/m2/d from umol/m2/d
      rmstmd=rmstmd*44.e-6
      rmlfd=rmlfd*44.e-6
      rmrotd=rmrotd*44.e-6
      rmgrd=rmgrd*44.e-6
      rmtotd=rmtotd*44.e-6
      rgtotd=rgtotd*44.e-6
      psmrmd=psmrmd*44.e-6
c  daily dry matter inc in gdw/m2/d from mgdw/m2/d
      dmincd=dmincd*1.e-3
c  daily light-use efficiencies
      eluapd=psyngd/apard
      eluasd=psyngd/asolrd
      eluipd=psyngd/parid
      eluisd=psyngd/solrid
c
      etpmd=etpmd*dt/(alam*4.18e3)
c                                               label     file write 8
      if(iwrite(2,5).eq.1)write(21,7100)icumdy
 7100 format(' 1205',i3,6x,/,14x,' taird  tairmx tairmn vpaird winddy',
     1' solard  pard  precpd')
      if(iwrite(2,5).eq.1)write(21,7200)icumdy,taird,tairmx,tairmn,
     1vpaird,winddy,solard,pard,precpd
 7200 format(' 2205',i3,6x,9f7.2)
      if(iwrite(2,6).eq.1)write(21,7300)icumdy
 7300 format(' 1206',i3,6x,/,14x,' tcpyd  tsfcsd rhmax  rhmin ')
      if(iwrite(2,6).eq.1)write(21,7400)icumdy,tcpyd,tsfcsd,rhmax,rhmin
 7400 format(' 2206',i3,6x,9f7.2)
      if(iwrite(2,7).eq.1)write(21,7500)icumdy
 7500 format(' 1207',i3,6x,/,14x,' rncpyd  hcpyd  ecpyd hsoild rnsold'
     1,' hcpysd wcpysd wsmmd  conddy')
      if(iwrite(2,7).eq.1)write(21,7600)icumdy,rncpyd,hcpyd,ecpyd,
     1hsoild,rnsold,hcpysd,wcpysd,wsmmd,conddy
 7600 format(' 2207',i3,6x,9f7.2)
      if(iwrite(2,8).eq.1)write(21,7700)icumdy
 7700 format(' 1208',i3,6x,/,14x,' swday  delwd  draind filtd  etmmd '
     1,' preevd trmmd  parid  solrid')
      if(iwrite(2,8).eq.1)write(21,7800)icumdy,swday,delwd,draind,
     1filtd,etmmd,preevd,trmmd,parid,solrid
 7800 format(' 2208',i3,6x,9f7.2)
c	--------------
c	| print dran5d (sum of drain5) for Fermanich/Bland 
c	| 94/3/25 LMM
      if(iwrite(2,20).eq.1)write(21,7702)icumdy
 7702 format(' 1220',i3,6x,/,14x,' dran5d')
      if(iwrite(2,20).eq.1)write(21,7802)icumdy, dran5d
 7802 format(' 2220',i3,6x,9f7.2)
c******************************************************************
      if(iwrite(2,13).eq.1)write(21,7710)icumdy
 7710 format(' 1213',i3,6x,/,14x,' parid  solrid apard  asolrd psyngd'
     &,' elupad elusad elupid elusid')
      if(iwrite(2,13).eq.1)write(21,7808)icumdy,parid,solrid,apard,
     &asolrd,psyngd,eluapd,eluasd,eluipd,eluisd
 7808 format(' 2213',i3,6x,9f7.2)
c******************************************************************
c
c  iwrite(2,9) and iwrite(2,12) are in subroutine simpet
c
	if(idiagn.eq.1)write(6,*)'before calling simpet'
c above stm was added by chen, 01/23/90.
c	 write (*,*) "CALL SIMPET (start)"
      call simpet(ihr,icumdy,rncpyd,taird,vpaird,winddy,solard,etpmd
     &,iday,tairmx,tairmn)
c
      if(iwrite(2,14).eq.1)write(21,7812)icumdy
 7812 format(' 1214',i3,6x,/14x,' rmstmd rmlfd  rmrotd rmgrd  rmtotd',
     &' rgtotd psmrmd dmincd')
      if(iwrite(2,14).eq.1)write(21,7814)icumdy,rmstmd,rmlfd,rmrotd,
     &rmgrd,rmtotd,rgtotd,psmrmd,dmincd
 7814 format(' 2214',i3,6x,9f7.2)
      if(iwrite(2,10).eq.1)write(21,7810)icumdy
 7810 format(' 1210',i3,6x,/,14x,' qhrwet qwetam qwetpm')
      do7900j=2,jtot
      jj=jtot+2-j
      qhrwet=ihrwet(jj)
      qwetam=iwetam(jj)
      qwetpm=iwetpm(jj)
      if(iwrite(2,10).eq.1)write(21,7820)icumdy,jj,qhrwet,qwetam,qwetpm
 7820 format(' 2210',i3,4x,i2,9f7.2)
 7900 continue
c  output soil temp and water prof for last hour of day
      if(iwrite(2,11).eq.1)write(21,7940)icumdy
 7940 format(' 1211',i3,6x,/,14x,
     &            '  depth tsol24 wsol24 psol24 psifrc')
      jzsfc1=jzsfc+1
      do7950jz=jzsfc1,jzbot
      if(jz.eq.jzsfc1.or.jz.eq.jzbot)then
        psifrc=0
      else
        psifrc=1./(rootsm*resrot(jz))
      endif
      psol24=pn(nohrs,jz)/100.0
      if(psol24.lt.-999.99)psol24=-999.99
 7950 if(iwrite(2,11).eq.1)write(21,7960)icumdy,jz,z(jz),tn(nohrs,jz)
     &,wt(nohrs,jz),psol24,psifrc
 7960 format(' 2211',i3,4x,i2,9f7.2)
c  call subroutine to scale on a daily basis
c
c	write (*,*) "CALL SCALED (start)"
      call scaled(icumdy,taird,rdcpyd,pmax,znr,pce,pcea,apard,pard
     &,parid,psyngd,rncpyd,hcpyd,ecpyd,hsoild,rnsold,hcpysd,wcpysd,
     &nohrs)
c
c the first values of the next day ,which are stored in subscript
c (nohrs+1), are written into subscript(1) for the next day.
c ihr is set to 2 before return to the next iday.
      timloc(1)=timloc(nohrs+1)
      wind(1)=wind(nohrs+1)
      radtop(1,1)=radtop(1,nohrs+1)
      radtop(2,1)=radtop(2,nohrs+1)
      radtop(3,1)=radtop(3,nohrs+1)
      fbeam1(1,1)=fbeam1(1,nohrs+1)
      fbeam1(2,1)=fbeam1(2,nohrs+1)
      temair(1)=temair(nohrs+1)
      vpair(1)=vpair(nohrs+1)
      precip(1)=precip(nohrs+1)
      irrchk(1)=irrchk(nohrs+1)
c
c     --------------
c     | Calculated fields calculated in
c     | the hourly readin section need
c     | to be here as well!  LMM 95/2/10
      tprecd(1)=tprecd(nohrs+1)
      temsol(1)=temsol(nohrs+1)
      watsol(1)=watsol(nohrs+1)
      relhum(1)=relhum(nohrs+1)
c  save nohrs in nhrlst in case nohrs changes on next day
      nhrlst=nohrs
      ihr=2
 9000 continue
c======================================================================
c                                                          END DAY LOOP
c======================================================================
      do9200j=2,jtot
 9200 continue
c  
c  end of multiple run loop
c
      close (unit=9)
c     close (unit=10)
      close (unit=15)
      close (unit=16)
      close (unit=20)
      close (unit=21)
      close (unit=19)
      close (unit=36)
c     close (unit=30)
c#ifdef PROBE
cc------------------------
cc DA BIG ouput for lfbal-newrap feedback work 9/11/23 LMM
c      close (unit=23)
c	call pend(1)
c      close (unit=31)
c	call pend(2)
c      close (unit=32)
c	call pend(3)
c      close (unit=33)
c	call pend(4)
c      close (unit=34)
c	call pend(5)
c      close (unit=35)
c	call pend(6)
c      close (unit=36)
c	call pend(7)
c      close (unit=37)
c#endif
c-------------------------
cBGb
      if(nrun.eq.noruns)goto 9901
 9900 continue
 9901 continue
	if (invrgl.eq.1) then
	    write(6,*)'invrgl,igl,ihrgl, isrgl,isw1 = '
	    write(6,*)invrgl,igl,ihrgl, isrgl,isw1
	    write(6,*)'temair(ihrgl),vpair(ihrgl),wind(ihrgl),psisum='
	    write(6,*)temair(ihrgl),vpair(ihrgl),wind(ihrgl),psisum
	endif
      write(6,*)'hsoil,hcpy,ecpy='
      write(6,8001)hsoil,qcond(2),econd(2)
 8001 format(1x,4f10.2)
      write(6,*)'tcpyir(4,1)=',tcpyir(4,1)
c  ;print out special variables if at end of cycles
      if(invrgl.eq.1)then
c     if(cycno.eq.cycls-1)call pntvbl(tsfcso,tcpy,taerom,taeroh,
c    &solar1,pari,ihrgl,wtpsig)
c     ihrm1=ihr-1
c     write (2,*) 'call pntvbl at ihr=',ihrm1
	  write (*,*) "CALL PNTVBL (start)"
      call pntvbl(ihrm1,tsfcso,tcpy,taerom,taeroh,
     &solar1,pari,ihrgl,wtpsig)
      endif
      if(isw1.ge.2)return
      if (invrgl.eq.1) then
	  
          xx(1) =temair(ihrgl)
          xx(2) =vpair(ihrgl)
          xx(3) =wind(ihrgl)
          xx(4) =psisum/1000.
          xx(5)=wt(ihrgl-1,jzsfc+1)
          xx(6)=tn(ihrgl-1,jzsfc+1)
          write(6,*)'ihrgl,tn ', ihrgl,tn(ihrgl,jzsfc+1)
c Sun added the above statement to replace following one 4/14/92
c         xx(6)=qcond(2)
          xx(7)=econd(2)
      endif
c
c
      return
c     debug trace
c     at 800
c     trace on
      end

c
c-----------------------------------------------------------------------
c						       SUBROUTINE pntvbl
c  	Used in inversion
c
      subroutine pntvbl(ihr,tsfcso,tcpy,taerom,taeroh,solar1,pari,ihrgl
     &,wtpsig)
c  subroutineto print out special variables from cupid on the
c    second to last cycle.  common/cycle/ is added to main
c    program, subr. refl and cupidg2 subroutine. Mar. 4, 1992
      parameter(mh=98)
      common /time/month,jday,iyear,icumdy,timloc(mh)
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
c akroot(50) added to /root1/ by MCA - 6/12/95      
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1,rootup(50),cpytr,psisum,akroot(50)
      common /wind1/fwind(20),wind(mh),sizelf,dmax,refhtw,z0,disp,am
     &,zcrit
      common /rad4/d(3,20),u(3,20),bmflx(3,20),rnet(50),rndiv(50),tsfc
     1,rnlam(3,20)
c     --------------
c     | rock variables added to soil4 LMM 94/9/8
c     | layer subscripts were added by MCA 5/24/95
      common/soil4/pe(25),bx(25),bd(25),aks(25),an(25),ws(25),asoil(25),
     & bsoil(25),csoil(25),dsoil(25),esoil,idoroc,irocly,akrock,cprock,
     & layid(25)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/cpy2/hsoil,hcpys,evtot,etotw(20),contot(20),scond(10,20)
     1,ihrwet(20),ecpys,cphstr,cpestr,wcpys,evsmic
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),uprof(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
     2,akcpynut(50),akcpyold(50)
      common/photo5/cscpy,pscpyl,pscpyg,cicpy,rdcpy,qemax
      common/tempir/tlfmax,tlfmin,tircpy,tirhot,taphot,wtirsl,emihot
      common/soil2/aksol(50),akw(50),cw(50),wt(mh,50),esave(3,mh),
     & wnu(mh,50),wnl(mh,50)      
      common/invers/rhsoil
      common/jz1/jzsfc2
c
c
      l=ihrgl
      wtavr=0.0000003
      psiavr=0.0
      do400jz=jzsfc2,jzbm1
      jz1=jz-jzsfc
      wtavr=wtavr+wt(l,jz)*abs(z(jz-1)-z(jz))
      psiavr=(pe(jz1)*(wt(l,jz)/ws(jz1))**(-bx(jz1)))*
     &   abs(z(jz-1)-z(jz))
 400  continue
      wtavr=wtavr/abs(z(jzsfc2)-z(jzbm1))
      psiavr=psiavr/abs(z(jzsfc2)-z(jzbm1))
      write(2,5) ihr
  5   format (2x,' ihr=',i5)
 50   write(2,*)
      write(2,*)' temair vpair  psisum psitop  wind  solar   pari',
     &'  wtpsig  wtsfc tnsfc  rhsoil'
      write(2,100)temair(l),vpair(l),psisum/100.,psitop/100.,wind(l)
     &,solar1,pari,wtpsig*100.,wt(ihrgl,jzsfc+1)*100.
     &,tn(ihrgl,jzsfc+1),rhsoil
      write(2,*)
 100  format(11f7.2)
      write(2,*)' tsfcso  tcpy  tlfmax tlfmin tircpy tirhot',
     &' taphot taerom taeroh wtirsl emihot'
      write(2,100)tn(l,jzsfc+1),tcpy,tlfmax,tlfmin,tircpy,tirhot,taphot
     &,taerom,taeroh,wtirsl,emihot
      write(2,*)
      write(2,*)' rncpy   ecpy   hcpy  hsoil  rnsoil ecpys hcpys',
     &'   pscpyg timloc wtavr psiavr'
      write(2,100)rnet(jtot),econd(2),qcond(2),hsoil,rnet(1)
     &,ecpys,hcpys,pscpyg,timloc(ihr),wtavr,psiavr/100.
      write(2,*)
c
c
      return
      end
cBGe
c------------------ 


