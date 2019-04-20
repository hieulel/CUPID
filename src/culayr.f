c $VERSION "08/16/95 @(#)culayr.f	7.1"
c
      subroutine layerp(nohrs,clump)
c
      parameter(mh=98)
       dimension rsubl(3),tsubl(3),adum(20),sdnsub(3,20),supsub(3,20)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3),tleaf(3),aleaf(3)
     &,expdif,rlayr(3),tlayr(3),rllive(3),tllive(3),rldead(3),tldead
      common /rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),hfday
     &,ratiod,ration,ratio(mh)
      common /rad4/d(3,20),u(3,20),bmflx(3,20),rnet(50),rndiv(50),tsfc
     1,rnlam(3,20)
      common /rad5/ sourup(3,mh),sourdn(3,mh)
      common /misc1/pi,pid180,sigma,iwrite(9,99)
      common /misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt,clai(20)
c  check if jtot is different from last hour. if it is, adjust index
c    over soil water cont so storage is cal'd correctly. we can ignore
c    pecularities for cpy profiles because storage is negl.
c    this must be done after jz subscript over height is defined
c    following call of hite2
c
c  calculate diffuse interception for layer.  calc layer trans and refl
c    for thick layers using layer equations and dividing df layer into
c    10 sub-layers and solving for diffuse radiation.
c    should use good precision in diffuse integral so errors dont
c    compound. thus use 90 intervals because 9 intervals accurate
c    to about .005 and this error becomes .03 in layer non-interception
c    factor because of 10th power from 10 sublayers.
	  write (*,*) "Layerp subroutine is called - culayr.f"
      laysub=10
      laysp1=laysub+1
      da=pi/180.
      a=pi/360.
      x=0.
      do 500 ii=1,90
      ca=cos(a)
      sa=sin(a)
      dfdca=df/ca
      x=x+ca*sa*exp(-clump*0.5*dfdca)
      a=a+da
 500  continue
      expdif=2.*x*da
c
c  calc diffuse non-interception for a sublayer from non-inter
c    for a df layer because integral takes care of ang dist thru
c    the whole df layer and sublayers assume isotropy of inc rad.
      difsub=expdif**.1
      do601k=1,3
      rlayr(k)=(1.-expdif)*rleaf(k)
      tlayr(k)=(1.-expdif)*tleaf(k)+expdif
      rsubl(k)=(1.-difsub)*rleaf(k)
      tsubl(k)=(1.-difsub)*tleaf(k)+difsub
 601  continue
c  solve sublayer equations
      do620k=1,3
 603  continue
      d(k,laysp1)=1.
      adum(1)=0.
      tlay2=tsubl(k)*tsubl(k)
      do605j=2,laysp1
      jm1=j-1
 605  adum(j)=adum(jm1)*tlay2/(1.-adum(jm1)*rsubl(k)) + rsubl(k)
      do610j=2,laysp1
      jj=laysp1-j+1
      jjp1=jj+1
      d(k,jj)=d(k,jjp1)*tsubl(k)/(1.-adum(jjp1)*rsubl(k))
      u(k,jjp1)=adum(jjp1)*d(k,jjp1)
 610  continue
 615  rlayr(k)=u(k,laysp1)
      tlayr(k)=d(k,1)
 620  continue
 625  continue
c  diffuse should be evaluated at layer midpoints
c  calc thick layer sources from laysub sublayers. since source dist
c    depends on sun angle, this effective source is dep on hour and
c    wavelength.
c  zero out all effective sources.
      do627ihr=1,nohrs
      do627k=1,3
      sourup(k,ihr)=0.
 627  sourdn(k,ihr)=0.
      do691ihr=1,nohrs
      if(coszen(ihr).lt.0.01)go to 691
      do690k=1,2
      if(fbeam1(k,ihr).lt.0.01)go to 690
      dirsub=exp(-.5*df/(coszen(ihr)*laysub))
      sdnsub(k,laysp1)=(1.-dirsub)*tleaf(k)
      supsub(k,laysp1)=(1.-dirsub)*rleaf(k)
      do630j=2,laysub
      jj=laysp1-j+1
      jjp1=jj+1
      sdnsub(k,jj)=sdnsub(k,jjp1)*dirsub
      supsub(k,jj)=supsub(k,jjp1)*dirsub
 630  continue
c  zero all u(k,j) and d(k,j).  d(k,laysp1)=u(k,1)=0. are b.c.
      iter=0
      do640j=1,laysp1
      d(k,j)=0.
 640  u(k,j)=0.
 645  iter=iter+1
      irept=0
      do650j=2,laysp1
      jj=laysp1-j+1
      jjp1=jj+1
      down=tsubl(k)*d(k,jjp1)+u(k,jj)*rsubl(k)+sdnsub(k,jjp1)
      if(abs(down-d(k,jj))-.0001)646,646,644
 644  irept=1
 646  d(k,jj)=down
      up=tsubl(k)*u(k,jj)+d(k,jjp1)*rsubl(k)+supsub(k,jjp1)
      if(abs(up-u(k,jjp1))-.0001)649,648,648
 648  irept=1
 649  u(k,jjp1)=up
 650  continue
      if(irept.ne.0)go to 645
c  calc source terms to be used in radiat, these must be multiplied
c    tbeam above each layer to get the sdn(j) and sup(j) needed there.
      sourup(k,ihr)=u(k,laysp1)
      sourdn(k,ihr)=d(k,1)
 690  continue
 691  continue
      return
      end
