c $VERSION "08/16/95 @(#)culad.f	7.1"
c     subroutine difint
c above statement was commented and following added by Chen, 8/31/89.
      subroutine difint(nohrs,clump,sunazm,path)
c **********************************************************************
c
c	---------
c	| Added path, path length through the canopy.  
c	| this was added becuase now we take into account
c	| slope and aspect.  LMM 30/9/93
c
      parameter(mh=98)
      dimension sunazm(mh),path(mh)
c above two statements were added by Chen, 8/31/89.
      dimension sdnsub(9,20),supsub(9,20),rsubl(9,20),tsubl(9,20),
     &adum(20)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20),
     &aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3),
     &rldead(3),tldead(3)
      common /rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),hfday
     &,ratiod,ration,ratio(mh)
      common /rad4/d(3,20),u(3,20),bmflx(3,20),rnet(50),rndiv(50),tsfc
     1,rnlam(3,20)
      common /rad5/ sourup(3,20,mh),sourdn(3,20,mh)
      common/rad6/rlfdif(3,20),tlfdif(3,20),rlfdir(3,20,mh),
     &tlfdir(3,20,mh)
      common/indax2/xmeu,xneu,gamrat,frdeg(91),iangot(9),kpntsp
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
      common/misc6/xint(mh),nxintz,xintz(90),nalpha,ispher
c xintz, extin. coef. at different zenith averaged over azimuth, and 
c nxtintz, dimension of xintz, Chen, 05/25/89.
c
c  calculate diffuse interception for layer.  calc layer trans and refl
c    for thick layers using layer equations and dividing df layer into
c    10 sub-layers and solving for diffuse radiation.
c    should use good precision in diffuse integral so errors dont
c    compound. thus use 90 intervals because 9 intervals accurate
c    to about .005 and this error becomes .03 in layer non-interception
c    factor because of 10th power from 10 sublayers.
      laysub=10
      laysp1=laysub+1
c      da=pi/180.
c      a=pi/360.
c above two stms were commented and following one added
c by Chen, 05/24/89.
      wz=pi/2/nxintz
      x=0.
c      do 500 ii=1,90
c above stm was commented by Chen, 05/24/89.
      do 500 ii=1,nxintz
      a=wz*(ii-.5)
      ca=cos(a)
      sa=sin(a)
      dfdca=df/ca
c      if(ispher.eq.0)call simpsn(a,fr,nalpha,xint)
c      x=x+ca*sa*exp(-xint*dfdca)
c      a=a+da
c above three were commented and following one added by Chen, 05/24/89.
      x=x+ca*sa*exp(-clump*xintz(ii)*dfdca)
c parameter clump in above statement was added by Chen, 9/4/89.
 500  continue
c      expdif=2.*x*da
c above one was commented and following one added by Chen, 05/24/89.
      expdif=2.*x*wz
c      
c  calc diffuse non-interception for a sublayer from non-inter
c  for a df layer because integral takes care of ang dist thru
c  the whole df layer and sublayers assume isotropy of inc rad.
      difsub=expdif**.1
      do 601 k=1,kmax
      do 601 j=2,jtot
        rlayr(k,j)=(1.-expdif)*rlfdif(k,j)
        tlayr(k,j)=(1.-expdif)*tlfdif(k,j)+expdif
        rsubl(k,j)=(1.-difsub)*rlfdif(k,j)
        tsubl(k,j)=(1.-difsub)*tlfdif(k,j)+difsub
c original rlfdif(k,nohrs) & tlfdif(k,nohrs) were changed to rlfdif(k)
c and tlfdif(k) in above four statements by Chen, 8/31/89.
c
c j subscripts added to rlayr,tlayr,rsubl,tsubl,rlfdif,tlfdif to
c allow for dead veg to accumulate in lower layers - MCA 4/21/95
c
c       write(26,*)rlayr(k),tlayr(k),rsubl(k),tsubl(k)
c	write(6,*)'k,rleaf,rlfdif,rlayr=',k,rleaf(k),rlfdif(k),rlayr(k)
c	write(6,*)'k,tleaf,tlfdif,tlayr=',k,tleaf(k),tlfdif(k),tlayr(k)
 601  continue
c 
c  Solve sublayer equations
c  Here, jl is index for main canopy layers - MCA 4/21/95
      do 620 jl=2,jtot
      do 620 k=1,kmax
        d(k,laysp1)=1.
        adum(1)=0.
        tlay2=tsubl(k,jl)*tsubl(k,jl)
        do 605 j=2,laysp1
          jm1=j-1
          adum(j)=adum(jm1)*tlay2/(1.-adum(jm1)*rsubl(k,jl)) + 
     &            rsubl(k,jl)
 605    continue
        do 610 j=2,laysp1
          jj=laysp1-j+1
          jjp1=jj+1
          d(k,jj)=d(k,jjp1)*tsubl(k,jl)/(1.-adum(jj)*rsubl(k,jl))
          u(k,jjp1)=adum(jjp1)*d(k,jjp1)
c         write(26,*)jjp1,adum(jjp1),d(k,jjp1),u(k,jjp1)
 610    continue
c       write(19,*)itry,rlayr(k,jl),u(k,11)
 615    rlayr(k,jl)=u(k,laysp1)
        tlayr(k,jl)=d(k,1)
c       write(26,*)itry,k,j,rlayr(k),tlayr(k),rsubl(k),tsubl(k),difsub
c    1  ,expdif
 620  continue
c 
c  diffuse should be evaluated at layer midpoints
c  calc thick layer sources from laysub sublayers. since source dist
c  depends on sun angle, this effective source is dep on hour and
c  wavelength.
c
c  zero out all effective sources.
      do 627 ihr=1,nohrs
      do 627 j=1,jtot
      do 627 k=1,kmax
        sourup(k,j,ihr)=0.
 627    sourdn(k,j,ihr)=0.
c  Subscript j added to sourup and sourdn to allow varying R and T
c  properties with layer - MCA 4/21/95
c
      do 690 ihr=1,nohrs
        if (coszen(ihr).lt..01) go to 690
        if (path(ihr).lt..9) go to 690
c6789  if(ispher.eq.0)call simpsn(zenang(ihr),fr,nalpha,xint)
c above stm was commented and following two added by Chen, 05/24/89.
c	xint(ihr)=.5
c 	if(ispher.eq.0)call simpsn(zenang(ihr),sunazm(ihr),fr,
c     1 nalpha,xint(ihr)) 
c	----------
c	| added path to dirsub equation.  This takes into account
c	| slope and aspect.
c	| 	LMM 30/9/93 
        dirsub=exp(-clump*xint(ihr)*df*path(ihr)/laysub)
c parameter clump in above statement was added by Chen, 9/4/89.
c path replaces 1/coszen in above to take into account slope and aspect
c	LMM 30/9/93
c
        do 691 k=1,kmax
        do 691 jl=2,jtot
c original 690 for k loop was changed to 691 since the stm 
c if... goto 690 caused trouble. Chen, 9/7/89.
          if(fbeam1(k,ihr).lt.0.01)go to 690
          sdnsub(k,laysp1)=(1.-dirsub)*tlfdir(k,jl,ihr)
          supsub(k,laysp1)=(1.-dirsub)*rlfdir(k,jl,ihr)
          do 630 j=2,laysub
            jj=laysp1-j+1
            jjp1=jj+1
            sdnsub(k,jj)=sdnsub(k,jjp1)*dirsub
            supsub(k,jj)=supsub(k,jjp1)*dirsub
c           write(26,*)jj,sdnsub(k,jj),supsub(k,jj),dirsub
 630      continue
c  zero all u(k,j) and d(k,j).  d(k,laysp1)=u(k,1)=0. are b.c.
          iter=0
          do 640 j=1,laysp1
            d(k,j)=0.
 640        u(k,j)=0.
 645      iter=iter+1
          irept=0
          do 650 j=2,laysp1
            jj=laysp1-j+1
            jjp1=jj+1
            down=tsubl(k,jl)*d(k,jjp1)+u(k,jj)*rsubl(k,jl)+
     &           sdnsub(k,jjp1)
            if(abs(down-d(k,jj))-.0001)646,646,644
 644        irept=1
 646        d(k,jj)=down
            up=tsubl(k,jl)*u(k,jj)+d(k,jjp1)*rsubl(k,jl)+
     &           supsub(k,jjp1)
            if(abs(up-u(k,jjp1))-.0001)649,648,648
 648        irept=1
 649        u(k,jjp1)=up
 650      continue
          if(irept.ne.0)go to 645
c  calc source terms to be used in radiat, these must be multiplied
c  tbeam above each layer to get the sdn(j) and sup(j) needed there.
          sourup(k,jl,ihr)=u(k,laysp1)
          sourdn(k,jl,ihr)=d(k,1)
c         write(26,*)k,ihr,sourup(k,jl,ihr),sourdn(k,jl,ihr)
 691    continue
c above stm was added by Chen, 9/7/89.
 690  continue
      return
      end
c
c      subroutine dstlit(thets,phis,itot,kmax,rlfdir,
c     &rlfdif,tlfdir,tlfdif,distls,ihr)
c above statement was commented and following added by Chen, 8/31/89.
      subroutine dstlit(thets,phis,kmax,rlfdir,tlfdir,ihr)
c  subroutine to calculate the distribution of leaf-normal to sun angles
c    for a given direction of the sun or given hour. distls(iangle) is
c    the variable where iangle has the same class intervals as fr(ialpha
c      dimension fr(10),distls(10),rlfdir(9,24)
c above was commented and following added by Chen, 5/24/89.
c     dimension rlfdir(9,24)
c    &,rlfdif(9,24),tlfdir(9,24),tlfdif(9,24),ca(9),sa(9)
c above statement was commented and following two added, Chen, 8/31/89.
      parameter(mh=98)
      dimension rlfdir(3,20,mh),tlfdir(3,20,mh),ca(9),sa(9)
      common/rad8/rlfhem(3,9,20),tlfhem(3,9,20)
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
      common/misc2/itot,itotp1,jtot,fr(10),cost(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c original ct(10) was changed to cost(10) by Chen, 7/5/89.
      common/misc6/xint(mh),nxintz,xintz(90),nalpha,ispher
c xintz, extin. coef. at different zenith averaged over azimuth, and 
c nxtintz, dimension of xintz, Chen, 05/25/89.
c      write(6,*)'ihr,thets,phis in dstlit=',ihr,thets,phis
      pi=3.1415926537
      pid180=pi/180.
      pid2=pi/2.
      do40ialpha=1,itot
 40   distls(ialpha,ihr)=0.
c original distls(ialpha) in above statement was changed to 
c distls(ialpha,ihr) by Chen, 8/30/89.
	wbeta=2.*pi/nbeta
      do100ialpha=1,itot
      alpha=pid2*(ialpha-1)/(itot) + pi/(4.*itot)
      sa(ialpha)=sin(alpha)
      ct=cos(thets)
      st=sin(thets)
      ca(ialpha)=cos(alpha)
c      fradd=fr(ialpha)/180
c	do100ibeta=1,20
c      beta=(1.+(ibeta-1)*2.)*pid180 - pi (5/18/89, chen)
c above three were commented and following three added by Chen,5/24/89
	do 100 ibeta=1,nbeta
	beta=wbeta*(ibeta-.5)
	fradd=fr(ialpha)*fraz(ibeta)
      fs=ct*ca(ialpha) + st*sa(ialpha)*cos(beta-phis)
      angl=acos(abs(fs))
      iangl=angl*itot/pid2 + 1.
c	write(6,*)'ialpha,ibeta,angl,iangl=',ialpha,ibeta,angl,iangl
c     write(2,50)alpha,beta,thets,phis,iangl,angl,(distls(m),m=1,itot)
 50   format(4f4.1,i3,f5.2,9f5.3)
      if(iangl.lt.1.or.iangl.gt.itot)go to 80
      go to 95
 80   write(6,90)iangl
 90   format(' trouble in sub dstlit iangl= ',i6)
      stop
 95   distls(iangl,ihr)=distls(iangl,ihr) + fradd
c original distls(iangl) in above statement was changed to 
c distls(iangl,ihr) by Chen, 8/30/89.
100   continue
	sum=0.
	do 105 iangle=1,itot
	  sum=sum+cost(iangle)*distls(iangle,ihr)
105	continue
	xint(ihr)=sum
c      write(2,51)thets/pid180,phis/pid180,(distls(m,ihr),m=1,itot)
c original distls(m) in above statement was changed to 
c distls(m,ihr) by Chen, 8/30/89.
 51   format(2(f7.2,1x),9(f5.3,1x))
c  compute a mean weighted sunlit leaf reflectance
      do400 j=2,jtot
      do400 k=1,kmax
      sum1=0.
      sum=0.
c     sum2=0.
c     sum3=0.
c     sum4=0.
c above three statements were commented by Chen, 8/31/89.
      do300iangl=1,itot
      angl=pid2*(iangl-1)/itot + pid2/(2.*itot)
      sum=sum+distls(iangl,ihr)*rlfhem(k,iangl,j)
      sum1=sum1+distls(iangl,ihr)*tlfhem(k,iangl,j)
c original distls(iangl) in above two statements was changed to 
c distls(iangl,ihr) by Chen, 8/30/89.
c  calc sums to get weighted leaf refl and trans for diffuse inc rad.
c     sum2=sum2+sa(iangl)*ca(iangl)
c     sum3=sum3+rlfhem(k,iangl,j)*sa(iangl)*ca(iangl)
c     sum4=sum4+tlfhem(k,iangl,j)*sa(iangl)*ca(iangl)
c above three statements were commented by Chen, 8/31/89.
 300  continue
      rlfdir(k,j,ihr)=sum
      tlfdir(k,j,ihr)=sum1
c     rlfdif(k,ihr)=sum3/sum2
c     tlfdif(k,ihr)=sum4/sum2
c above two statements were commented by Chen, 8/31/89, since
c rlfdif(k) & tlfdif(k) were calculated in subroutine inplnt.
 400  continue
      return
      end
c      
      subroutine gamma(x,gam)
c this subroutine calculates the gamma function using stirling's
c approximation.
      real x,y,z,pi,gam
      integer i,j
      g(y) = sqrt(2.*pi/y)*
     1exp(y*alog(y)+(1-1/(30*y*y))/(12*y)-y)
     2/((y-2)*(y-1))
      pi=4.*atan(1.)
      if (x.lt.0.0) goto 10
      if (abs(x).lt.1.e-10) goto 40
      xp2=x+2.0
      gam = g(xp2)
      goto 99
  10  z=x
      j=-1
      y=x
  20  j=j+1
      y=y+1.0
      if(y.lt.0.0) goto 20
      yp2=y+2.0
      gam=g(yp2)
      jp1=j+1
      do 30 i=1,jp1
  30  gam=gam/(x+i-1)
      goto 50
  40  gam=1.0
  50  continue
  99  return
      end
c
c      subroutine kernal(cosdel,theta,alpha)
c  subroutine to calculate cosine delta where theta is solar zenith
c    angle and alpha is inclination angle of leaf.
c      pid2=3.1415926537/2.
c      if(theta+alpha-pid2)10,10,50
c10    cosdel=cos(alpha)*cos(theta)
c      go to 100
c 50   beta0=acos(1./(tan(alpha)*tan(theta)))
c      cosdel=((pid2-beta0)*cos(alpha)*cos(theta)+sin(alpha)*sin(theta)*s
c     1in(beta0))/pid2
c 100  continue
c      return
c      end
c
c above subroutine kernal were commented and following one added by
c Chen, 05/24/89.
c
      subroutine kernal(cosdel,theta,phi,alpha)
c  subroutine to calculate cosine delta where
c  theta=zenith angle of sun or viewer
c  phi =azimuth angle of sun or viewer, south = 0., east = -90.
c  alpha=leaf elevation angle from horizontal
c  beta =azimuthal direction leaf normal points
c  fr =fraction of leaf area proj either in dir of sun or viewer.
c  fraz is the fraction of leaf area as a func of azimuth
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
      pi=4.*atan(1.)
      wbeta=2.*pi/nbeta
      cosdel=0.
      do1000ibeta=1,nbeta
      beta=(ibeta-.5)*wbeta
      fs=cos(theta)*cos(alpha)+sin(theta)*sin(alpha)*cos(phi-beta)
      cosdel=cosdel+abs(fs)*fraz(ibeta)
 1000 continue
      return
      end
      subroutine lad(meu,neu,fr,itot,gamrat,frdeg)
c this subroutine calculates the leaf angle distribution
c                                                     4/20/84
c including distribution both in zenith and in azimuth, Chen, 05/24/89.
c leaf angle distributions for various canopy types:
c   canopy type       f(theta)         theta     meu     neu
c   planophile   2(1+cos(2*theta))/pi  26.76   2.770   1.172
c   erectophile  2(1-cos(2*theta))/pi  63.24   1.172   2.770
c   plagiophile  2(1-cos(4*theta))/pi  45.00   3.326   3.326
c   extremophile 2(1+cos(4*theta))/pi  45.00   0.433   0.433
c   uniform      2/pi                  45.00   1.000   1.000
c   spherical    sin(theta)            57.30   1.101   1.930
c      source: naren goel & don strebel (1984)  agron j. 76:800-802
c
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
c above common is added by Chen, 05/23/89, and beta0 is in degrees.
      real meu,neu,meuneu,gam1,gam2,gam12,gamrat,lang,fr(10),
     1frdum(91),frdeg(91),frcls(9)
c
c  beta-distribution
c
c      bdistr(gamrat,tld90,neu,meu)=gamrat*(tld90**(neu-1.))*
c     &       ((1.-tld90)**(meu-1.))/(360.*90.)
c above was commented by Chen, 7/5/89.
      pi=4.*atan(1.)
      pid180=pi/180.
      meuneu=meu+neu
c the next statement is used to avoid overflows in the calculation
c of gamma functions for large values of meu and neu.
c      if(meu.gt.35..or.neu.gt.35..or.meuneu.gt.35.) goto 1
c      call gamma(meu,gam1)
c      call gamma(neu,gam2)
c      call gamma(meuneu,gam12)
c      goto 4
c 1    gamrat=((meu*neu)/(2.*pi*meuneu))**0.5
c      gamrat=gamrat*((meuneu/meu)**meu)*(meuneu/neu)**neu
c      goto 5
c 4    gamrat=gam12/(gam1*gam2)
c above 9 lines were commented by Chen, 7/5/89.
      sum = 0.
      dang=90./itot
 5    do 10 i = 1,itot
         lang=(dang*(i-1)+dang/2)/90.
c      fr(i)=(gamrat*(lang**(neu-1))*((1-lang)**(meu-1))/90.)*dang
c above was commented and following added by Chen, 7/5/89.
	fr(i)=lang**(neu-1)*(1-lang)**(meu-1)
c      write(6,*)'in lad ',i,fr(i),gamrat,itot,lang,neu,meu,dang
      sum = sum + fr(i)
 10   continue
      do 20 i = 1,itot
  20     fr(i)=fr(i)/sum
c ****** 7/7/86 revision  ******
c **  removed seciont that calculates frspec - it's in a separate
c **  subroutine called "fractn"
c ************************************
c  calculates leaf angle distribution for specular contribution at
c 10 degree increments
c
c     sum = 0
c     do 50 i = 1,9
c        lang=(10.*(i-1)+5)/90.
c        frdum(i) = gamrat*(lang**(neu-1))*((1-lang)**(meu-1))/9.
c     sum=sum+frdum(i)
c 50  continue
c     do 70 i=1,9
c        frdum(i) = frdum(i)/sum
c        do 70 j = 1,36
c  frspec is fraction of leaf area in single azimuth and zenith angle
c    class that contributes to specular reflection.
c           frspec(i,j)=frdum(i)/36.
c 70  continue
c     write(6,*)(frspec(i,1),i=1,9)
c
c  calculate for every 10 deg view zenith angle class
      do 75 i=1,91
         frdum(i)=0.
  75  continue
c for deg classes 1 through 91
      do 200 ideg=1,90
         deg=float(ideg)
         tld90=(deg-0.5)/90.
c         frdum(ideg)=bdistr(gamrat,tld90,neu,meu)
c above was commented and following added by Chen, 7/5/89.
	 frdum(ideg)=tld90**(neu-1)*(1-tld90)**(meu-1)
  200 continue
c
c  normalize fraction of leaves
c
      sum=0.
      do 250 i=1,91
         sum=sum+frdum(i)
 250  continue
      do 275 i=1,91
         frdeg(i)=frdum(i)/sum
 275  continue
c  put into 9 classes for screen display
      do 300 i=1,9
         frcls(i)=0.
 300  continue
      do 305 i=2,10
 305     frcls(1)=frcls(1)+frdeg(i)
      do 310 i=2,9
         j=i-1
         frtemp=0.
         do 320 k=1,10
            kk=(k+j*10)
            frtemp=frtemp+frdeg(kk)
 320     continue
         frcls(i)=frtemp
 310  continue
c
c following 12 statments for calculating leaf azimuthal angle distrb.
c were added by Chen, 05/23/89.
c	write(6,*)'starting calculation of azimuthal angle distribution'
      jbeta0=int(beta0/360.*nbeta)
      sum=0.
      do 410 ibeta=1,nbeta
      x=(ibeta-.5)/nbeta
      aux=x**(xneuaz-1)*(1-x)**(xmeuaz-1)
      ii=ibeta+jbeta0
      if (ii.gt.nbeta) ii=ii-nbeta
      fraz(ii)=aux
      sum=sum+aux
 410  continue
      do 420 ii=1,nbeta
      fraz(ii)=fraz(ii)/sum
 420  continue
c	write(6,*) (fraz(i),i=1,nbeta)
c following 13 statements for calculating leaf azimuthal angle distrb.
c in each degree class were added by Chen, 07/11/89.
      jbeta0=int(beta0)
      sum=0.
      do 411 ibeta=1,360
      x=(ibeta-.5)/360
      aux=x**(xneuaz-1)*(1-x)**(xmeuaz-1)
      ii=ibeta+jbeta0
      if (ii.gt.360) ii=ii-nbeta
      fradeg(ii)=aux
      sum=sum+aux
 411  continue
      do 421 ii=1,360
      fradeg(ii)=fradeg(ii)/sum
 421  continue
c
      return
      end
c**************************simpsn************************************
      subroutine simpsn(theta,phi,gr,nalpha,xint)
c phi in above and following common were adde by Chen, 05/24/89.
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
      dimension gr(10)
c  xint  extinct coeff-nel is in first part of array and nthet is
c          in last part of array and tot size of array is nelpth.
c  nalpha  no of leaf angle classes
c
c   program to calc integrl of ga*cosd (simpson rule)
c     write(11,7)
 7    format(' ','*extinction data')
c
c     nthet=num. of sun angles
c     dt= delta theta
c     nalpha=num. of leaf angle classes
c     da= delta alpha
c
      pi=4.0*atan(1.0e0)
      pid180=pi/180.0
      pid2=pi/2.0
c     nalpha must be odd
      xint=0.
      st=sin(theta)
      ct=cos(theta)
      sum=0.
      do 1001 k=1,nalpha
      alpha=(k-.5)*pid2/nalpha
c      call kernal(cosdel,theta,alpha)
c  above was commented and following added by Chen, 7/9/89. 
      call kernal(cosdel,theta,phi,alpha)
      sum=sum+cosdel*gr(k)
c      write(2,*)'ialpha,fr,cosdel,xint=',k,gr(k),cosdel,sum
 1001 continue
      xint=sum*pid2/nalpha
c  for area fractions weighted by leaf angle distrib,gr(alpha),
c    should not mult by dalpha because integral over gr(alpha)dalpha
c    in denominator will cancel dalpha.
      xint=sum
 1000 continue
c     write(11,1003)
 1003 format(' ','theta and xint')
c     write(11,500) theta,xint
  500 format(' ',f10.3,2(5x,f16.12))
      return
      end
