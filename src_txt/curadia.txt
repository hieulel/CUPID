c $VERSION "08/16/95 @(#)curadia.f	7.1"
      subroutine radiat(kstrt,coszen,radabv,fbeam,ihr,ioutpt,thetas,
     1iprofl,phis,clump,path,cosdlt)
c thetas and phis in above stm were added by Chen, 9/7/89.
c****************************radiat*************************************
      parameter(mh=98)
      dimension u(3,20), d(3,20), radabv(3), sup(20), sdn(20), adum(20)
     &,fbeam(3)
     &,theta(10)
     &,tbeam(20),beam(3)
     4,dave(3,20),shade(3,20),smsup3(20)
     5,path(mh)
     6,tlayer(20),rlayer(20)
c smsup(20) in dimension stm was added by Chen, 9/30/89.
c tlayer(20) and rlayer(20) in dim stm added by MCA, 4/2195.
c	-------
c	| path added - path length of sunbeam through canopy
c	| LMM 93/9/31
      common/balan/dsdum(3,20),dstng(20),dstng2(20)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20)
     &,aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3)
     &,rldead(3),tldead(3)
      common /rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20)
     &,templf(10,20),tsoil(mh)
      common/rad4/d,u,bmflx(3,20),rnet(50),rndiv(50),tsfc,rnlam(3,20)
      common /rad5/ sourup(3,20,mh),sourdn(3,20,mh)
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/leaf2/evap(10,20),gevap(10,20),heat(10,20),gheat(10,20)
     &,alam ,tlfavg(20),tgheat(20),tgvap1(20),tgvap2(20)
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
c pid2,kmax in /misc1/ were added by Chen, 9/4/89.
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/misc6/xint(mh),nxintz,xintz(90),nalpha,ispher
c xintz, extin. coef. at different zenith averaged over azimuth, and 
c nxtintz, dimension of xintz, Chen, 05/25/89.
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
c /misc6/ and /indax3/ were added by Chen, 9/7/89.
      common /time/month,jday,iyear,icumdy,timloc(mh)
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),u1(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
      common/photo3/csleaf(10,20),psleaf(10,20),cileaf(10,20),rgas
     &,rdrk(10,20),hsleaf(10,20),csfclf(10,20)
c  loop over wavelength, 1=vis; 2=nearir; 3=thermal
      do  5900 k=kstrt, kmax
      iter = 0
c in above stm original 3 was replaced by kmax.
      do 540 j=2,jtot
        tlayer(j)=tlayr(k,j)
        rlayer(j)=rlayr(k,j)
 540  continue 
        tlayer(1)=0.
        rlayer(1)=rsoil(k) 
c  separate solar and thermal
 545  if(k-3) 550, 4000,4000
 550  continue
c  if fbeam or coszen lt .01, don't calculate beam factors
c	
	if (coszen.lt.0.01) then
c	    ----------
c	    | Night Time 
c
	    do630j=1,jtot
		adum(j)=0.
		d(k,j)=0.
		u(k,j)=0.
		do625i=1,itotp1
		    dstrad(k,i,j)=0.
 625  		    frarea(i,j)=0.
		frarea(itotp1,j)=1.
 630  	    continue
   	    expdir=0.
      	    do 631, j=1,jtot
      	      	tbeam(j)=0.
      		bmflx(k,j)=0.
      		sup(j)=0.
      		sdn(j)=0.
 631  	    continue
c	    ------------------>>>>
	    GOTO 5000	
c	    ------------------>>>>
	endif
 	if ((fbeam(k).lt..01) .or. (path(ihr).lt.-.9)) then
c	    ----------
c	    | No direct due to cloud cover or 
c	    | slope interception, zero out varaibles
c
   	    expdir=0.
      	    do590j=1,jtot
      	      	tbeam(j)=0.
      		bmflx(k,j)=0.
      		sup(j)=0.
      		sdn(j)=0.
 590  	    continue
	else
c	    ----------
c	    |  calculate direct interception for a layer
 565  	    expdir=exp(-clump*xint(ihr)*df*path(ihr))
c	    |  xint(ihr) in above stm replaced original 0.5. Chen, 9/7/89.
c	    |  calculate all beam sources
c	    |  added path LMM 93/8/31
c
	    tbeam(jtot)=fbeam(k)
	    sdn(1)=0.
	    do 800 j=2,jtot
		jj=jtot-j+1
		jjp1=jj+1
		tbeam(jj)=1.e-20
		if(tbeam(jjp1).gt.1.e-20)tbeam(jj)=tbeam(jjp1)*expdir
		sup(jjp1)=sourup(k,jjp1,ihr)*tbeam(jjp1)
		sdn(jjp1)=sourdn(k,jjp1,ihr)*tbeam(jjp1)
c 	        xup=(tbeam(jjp1)-tbeam(jj))*rleaf(k)
c	        xdn=(tbeam(jjp1)-tbeam(jj))*tleaf(k)
c		write(19,*)jjp1,sup(jjp1),sdn(jjp1),xup,xdn
 800  	   continue
	endif
c
 	sup(1)=tbeam(1)*rsoil(k)
c   calc up and down fluxes with diffuse only equations and add
c    source terms to down terms as they are computed.
	aux1=0.
	do 603 j=1,jtot
	aux2=tbeam(j)
	x=sdn(j)+sup(j)
	aux=(aux2-aux1)*(1.-aleaf(k,j))
	if(j.eq.1) aux=(aux2-aux1)*rsoil(k)
	aux1=aux2
603	continue
      d(k,jtot)=1.-fbeam(k)
      adum(1)=rsoil(k)
      do 600 j=2,jtot
        tlay2=tlayer(j)*tlayer(j)      
        jm1=j-1
 600    adum(j)=adum(jm1)*tlay2/(1.-adum(jm1)*rlayer(j))+rlayer(j)
      do 700 j=2,jtot
        jj=jtot-j+1
        jjp1=jj+1
        d(k,jj)=d(k,jjp1)*tlayer(jjp1)/(1.-adum(jj)*rlayer(jjp1))
     &           +sdn(jjp1)
 700    u(k,jjp1)=adum(jjp1)*d(k,jjp1)+sup(jjp1)
      u(k,1)=rsoil(k)*d(k,1)+sup(1)
c      
c  calculate total diffuse up and down considering beam sources.
 1045 iter=0
 900  irept=0
      iter=iter+1
      do 1000 j=2,jtot
        jj=jtot-j+1
        jjp1=jj+1
        down=tlayer(jjp1)*d(k,jjp1)+u(k,jj)*rlayer(jjp1)+sdn(jjp1)
        if(abs(down-d(k,jj))-.0001)1000, 1000, 950
 950    irept=1
 1000   d(k,jj)=down
      u(k,1)=(d(k,1)+tbeam(1))*rsoil(k)
      do 1200 jj=2,jtot
        jjm1=jj-1
        up=rlayer(jj)*d(k,jj)+u(k,jjm1)*tlayer(jj)+sup(jj)
        if(abs(up-u(k,jj))-.0001) 1100,1100,1050
 1050   irept=1
 1100   u(k,jj)=up
 1200 continue
c 
c  calculate the diffuse and direct rad absorbed by layer j
	do 1203 j=2,jtot
	a=d(k,j)-u(k,j)+tbeam(j)-(d(k,j-1)-u(k,j-1)+tbeam(j-1))
	adif=(d(k,j)+u(k,j-1))*(1-expdif)*aleaf(k,j)
	adir=(tbeam(j)-tbeam(j-1))*aleaf(k,j)
	ad=adif+adir
1203	continue
 1260 if(irept) 900, 2000, 900
c  calculate fraction of leaf area in class i that is associated with
c  light incident in class i.
c  frarea(i,j) sums to 1. over leaf angl classes including shaded
 2000 jtotm1=jtot-1
      do 3000 j=2,jtot
      jm1=j-1
      if(tbeam(jtot)-.0001)2050,2100,2100
 2050 sunlit=0.
      go to 2200
 2100 if (path(ihr).gt.-.9 ) then
	  sunlit=(tbeam(j)-tbeam(jm1))/(tbeam(jtot)*df)/
     &			(xint(ihr)*path(ihr))
      else
	  sunlit = 0.0
      endif
c in above stm original 2.* was replaced by /xint(ihr), Chen, 9/7/89.
c	--------
c	| path added LMM 93/9/31
c
 2200 do 2500 i=1,itot
       frarea(i,j)=sunlit*distls(i,ihr)
c in above stm original fr(i) was replaced by distls(i,ihr), Chen.
 2500 continue
      frarea(itotp1,j)=1-sunlit
 3000 continue
c      beam(k)= fbeam(k)*radabv(k)/coszen
c
c	--------
c	| since fbeam and radabv are modified by slope,
c	| need to use cosdlt which is the angle between the
c	| sun and the slope to get the beam intensity. LMM
	beam(k)= fbeam(k)*radabv(k)/cosdlt
      d(k,1)=d(k,1)*radabv(k)
      u(k,1)=u(k,1)*radabv(k)
      bmflx(k,1)=tbeam(1)*radabv(k)
      do2600j=2,jtot
      jm1=j-1
c  convert to flux densities
      u(k,j)=u(k,j)*radabv(k)
      d(k,j)=d(k,j)*radabv(k)
 2600 bmflx(k,j)=tbeam(j)*radabv(k)
      do3100j=2,jtot
      jm1=j-1
      dsdum(k,j)=0.
c  next line calculates diffuse rad above layer
c      dstrad(k,itotp1,j)=(u(k,jm1)+d(k,j))*(1.-expdif)/df
c above stm was commented because it did not include the multiple
c scattering within the layer j. the absorbed diffuse radiation by
c layer j is (u(k,jm1)+d(k,j))*(1-tlayer-rlayer), and the absorbed
c direct radiation is (bmflx(k,j)-bmflx(jm1))*aleaf(k,j). due to the
c multiple scattering within the layer, the total absorption caused
c by direct radiation is (bmflx(k,j)-bmflx(k,jm1))-s(k,j). therefore
c (bmflx(k,j)-bmflx(k,jm1)-s(k,j)-(bmflx(k,j)-bmflx(k,jm1))*aleaf(k)
c =(bmflx(k,j)-bmflx(k,jm1))*(1-aleaf(k))-s(k,j) is the additional
c contribution to absorption, which can be included for simplicity
c in diffuse absorption as follows: (Chen, 10/3/89)
	adif1=(u(k,jm1)+d(k,j))*(1.-tlayer(j)-rlayer(j))
	adif2=(bmflx(k,j)-bmflx(k,jm1))*(1.-aleaf(k,j))
     1        -(sdn(j)+sup(j))*radabv(k)
	dstrad(k,itotp1,j)=(adif1+adif2)/df/aleaf(k,j)
c  next line calculated average diffuse rad at midpoint of layer.
c     dstrad(k,itotp1,j)=(d(k,j)+d(k,jm1)+u(k,jm1)+u(k,j))*.5
c    &*(1.-expdif)/df
      dsdum(k,j)=dsdum(k,j)+dstrad(k,itotp1,j)*frarea(itotp1,j)*df
     &*aleaf(k,j)
	proj=0.
      do 2700 i=1,itot
      dstrad(k,i,j)=dstrad(k,itotp1,j)+ beam(k)*ct(i)
 2700 dsdum(k,j)=dsdum(k,j)+dstrad(k,i,j)*frarea(i,j)*df*aleaf(k,j)
      rnldiv=d(k,j)-u(k,j)+bmflx(k,j)-(d(k,jm1)-u(k,jm1)+bmflx(k,jm1))
c     if(ihr.gt.5)write(15,2701)ihr,j,dsdum(k,j),rnldiv
 2701 format(' solar ',2i3,2f7.2)
 3100 continue
      go to 5000
 4000 continue
c  treat thermal wavelengths.
c  compute thermal source terms
c  check to see if solar has been executed(day or night)
      sdn(1)=0.
      do4009 j=1,jtot
	  adum(j)=0.
 4009     bmflx(3,j)=0.
      if(kstrt-3)4020,4010,4010
c  check if night or recycling of leaf energy balance
 4010 if(coszen-.01)4012,4012,4020
 4012 do4015j=2,jtot
      sdn(j)=emis*sigma*(templf(itotp1,j)+273.)**4
      do4014 i=1,itotp1
      frarea(i,j)=0.
      dstrad(3,i,j)=sdn(j)
      dstrad(1,i,j)=0.
 4014 dstrad(2,i,j)=0.
      frarea(itotp1,j)=1.
      d(1,j)=0.0
      d(2,j)=0.0
      tbeam(j)=0.0
      bmflx(1,j)=0.0
      bmflx(2,j)=0.0
      sdn(j)=sdn(j)*(1.-expdif)
 4015 sup(j)=sdn(j)
      d(1,1)=0.0
      d(2,1)=0.0
      tbeam(1)=0.0
      bmflx(1,1)=0.0
      bmflx(2,1)=0.0
      expdir=0.
      go to 4200
 4020 do 4100 j=2,jtot
      source=0.
      do 4050 i=1,itotp1
c      if(iday.eq.2)write(*,*)emis,sigma,templf(i,j)
      dstrad(3,i,j)=emis*sigma*(templf(i,j)+273.)**4
      source=source+frarea(i,j)*dstrad(3,i,j)
 4050 continue
      sdn(j)=source*(1.-expdif)
 4100 sup(j)=source*(1.-expdif)
c  thermal boundary conditions
 4200 continue
c  tsfc is soil sfc temp for each iteration.  its calc'd in main
c    program after profl2 is called.
      esoil =emisol*sigma*(tn(ihr,jzsfc+1)+273.)**4
      d(k,jtot)=radabv(3)
c  compute thermal layer properties
c     tlayer=expdif
c     rlayer=(1.-expdif)*(1.-emis)
c  assume thermal reflectance =0 and call u+d
      do 4500 j=2,jtot
      jm1=j-1
      jj=jtot-j+1
      jjp1=jj+1
 4500 d(k,jj)=tlayer(jjp1)*d(k,jjp1)+sdn(jjp1)
      sup(1)=d(k,1)*(1.-emisol)
      u(k,1)=esoil+sup(1)
      do4600j=2,jtot
      jm1=j-1
 4600 u(k,j)=tlayer(j)*u(k,jm1)+sup(j)
c  iterate 2 times with refl=1-emis
 4580 do 4900 jjj=1,2
      do 4800 j=2,jtot
      jm1=j-1
      jj=jtot-j+1
      jjp1=jj+1
 4800 d(k,jj)=tlayer(jjp1)*d(k,jjp1)+u(k,jj)*rlayer(jjp1)
     &         +sdn(jjp1)
      sup(1)=d(k,1)*(1.-emisol)
      u(k,1)=esoil+sup(1)
      do4810j=2,jtot
      jm1=j-1
 4810 u(k,j)=rlayer(j)*d(k,j)+u(k,jm1)*tlayer(j)+sup(j)
 4900 continue
c
c  sum up thermal contribution to dstnet to be sure it matches u and d
c    thermal. dstnet is calc'd below.
      do4910j=2,jtot
      dsdum(k,j)=0.
      jm1=j-1
      do4905i=1,itotp1
      xxx=(1.-expdif)/df
c     dumean=(d(k,j)+d(k,jm1)+u(k,jm1)+u(k,j))*.5
c     dsdum(k,j)=dsdum(k,j)+(dumean*emis-2.*dstrad(k,i,j))*xxx
c    &*df*frarea(i,j)
c above 3 lines were commented, and following added, Chen 10/3/89.
      dsdum(k,j)=dsdum(k,j)-2.*dstrad(k,i,j)*xxx*df*frarea(i,j)
 4905 continue
      dsdum(k,j)=dsdum(k,j)+(d(k,j)+u(k,jm1))*
     &                             (1.-tlayer(j)-rlayer(j))
c above stm was added by Chen, 10/3/89.
      rnldiv=d(k,j)-u(k,j)-(d(k,jm1)-u(k,jm1))
c     write(15,4915)ihr,j,dsdum(k,j),rnldiv,sdn(j),sup(j)
 4915 format(' thermal ',2i3,4f8.2)
 4910 continue
c
c                                                  label file write 4
c
 5000 continue
      if(ioutpt.eq.0)go to 5030
      if(k-2)5005,5006,5007
 5005 if(iwrite(5,1).eq.1)write(21,5010)k,icumdy,ihr
 5010 format(' 150',i1,i3,i2,4x,/14x,'  iter  expdir expdif  clump')
      go to 5009
 5006 if(iwrite(5,2).eq.1)write(21,5011)k,icumdy,ihr
 5011 format(' 150',i1,i3,i2,4x,/14x,'  iter  expdir expdif  clump')
       go to 5009
 5007 if(iwrite(5,3).eq.1)write(21,5012)k,icumdy,ihr
 5012 format(' 150',i1,i3,i2,4x,/14x,'  iter  expdir expdif  clump')
 5009 continue
      qiter=iter
      if(iwrite(5,1).eq.1.and.k.eq.1)go to 5014
      if(iwrite(5,2).eq.1.and.k.eq.2)go to 5014
      if(iwrite(5,3).eq.1.and.k.eq.3)go to 5014
      go to 5024
 5014 write(21,5015)k,icumdy,ihr,qiter,expdir,expdif,clump
 5015 format(' 250',i1,i3,i2,4x,9f7.2)
      write(21,5025)k,icumdy,ihr
 5025 format(' 150',i1,i3,i2,4x,/14x,' clai   rlayer tlayer',
     1'   d      u    tbeam  bmflx  rn123   rnet ')
 5024 continue
c                                                       file write 4
c
 5030 do5040j=1,jtot
      jout=jtot-j+1
      clai(jout)=(j-1)*df
      if(k.eq.1.or.k.eq.2)rnlam(k,jout)=d(k,jout)-u(k,jout)
     1 +bmflx(k,jout)
      if(k.eq.3)rnlam(k,jout)=d(k,jout)-u(k,jout)
c +smsup3(jout) in above stm was added by Chen, 9/30/89.
      rnet(jout)=rnlam(1,jout)+rnlam(2,jout)+rnlam(3,jout)
      if(iwrite(5,1).eq.1.and.k.eq.1)go to 5038
      if(iwrite(5,2).eq.1.and.k.eq.2)go to 5038
      if(iwrite(5,3).eq.1.and.k.eq.3)go to 5038
      go to 5040
 5038 if(ioutpt.eq.1)write(21,5026)k,icumdy,ihr,jout,clai(jout)
     1,rlayer(jout),tlayer(jout),d(k,jout),u(k,jout),tbeam(jout)
     2,bmflx(k,jout),rnlam(k,
     3jout),rnet(jout)
 5026 format(' 250',i1,i3,i2,2x,i2,  9f7.2)
 5040 continue
c     if(ihr.ge.20)write(25,5069)ihr,ihr,rnet(1),d(3,1),u(3,1)
 5069 format(1x,2i3,3e10.4)
      do5970j=2,jtot
      jm1=j-1
c	write(6,*)'j,dsdum1,2,3=',j,dsdum(1,j),dsdum(2,j),dsdum(3,j)
c	write(6,*)'j,absor1,2,3=',j,rnlam(1,j)-rnlam(1,jm1),rnlam(2,j)
c     1  -rnlam(2,jm1),rnlam(3,j)-rnlam(3,jm1)
 5970 rndiv(j)=rnet(j)-rnet(jm1)
      rndiv(1)=rnet(1)
 5900 continue
 5200 do6500j=2,jtot
      jm1=j-1
      dstng(j)=0.
      do 6000i=1,itotp1
c  calc thermal dif at layer midpoint
      dstnet(i,j)=dstrad(1,i,j)*aleaf(1,j)+dstrad(2,i,j)*aleaf(2,j)+
c     &((d(3,j)+d(3,jm1)+u(3,jm1)+u(3,j))*.5*emis
c     &-2.*dstrad(3,i,j))*(1.-expdif)/df
c above two line were replaced by the following two, Chen, 10/3/89.
     & (d(3,j)+u(3,jm1))*(1.-tlayr(3,j)-rlayr(3,j))/df
     & -2.*dstrad(3,i,j)*(1.-expdif)/df
      dstng(j)=dstng(j)+dstnet(i,j)*df*frarea(i,j)
 6000 continue
      dstng2(j)=dsdum(1,j)+dsdum(2,j)+dsdum(3,j)
c     write(16,6002)ihr,j,rndiv(j),dstng(j),dstng2(j)
c	write(6,*)'j,rndiv,dstng,dstng2=',j,rndiv(j),dstng(j),dstng2(j)
 6002 format(' total ',2i3,3f8.2)
 6500 continue
c                                                  label file write 5
c
      if(ioutpt.eq.0)go to 6550
      if(iwrite(8,1).eq.1)write(21,5050)icumdy,ihr
 5050 format(' 1801',i3,i2,4x,/14x,' dstnet dstvis dstnir dstthr prarea
     1  ps1 ')
      if(iwrite(8,2).eq.1)write(21,5052)icumdy,ihr
 5052 format(' 1802',i3,i2,4x,/14x,' templf  delt   evap   heat   gevap'
     1' gheat rsleaf rsnovp ')
c                                                       file write 5
c
      jtotm1=jtot-1
      do5100iout=1,itotp1
      do5100j=1,jtotm1
      jout=jtot-j+1
      prarea=frarea(iout,jout)*100.
      if(iwrite(8,1).eq.1)write(21,5051)icumdy,ihr,iout,jout,dstnet(iout
     1,jout),dstrad(1,iout,jout),dstrad(2,iout,jout),dstrad(3,iout,jout)
     2,prarea,psleaf(iout,jout)
 5051 format(' 2801',i3,i2,i2,i2,  9f7.2)
      if(iwrite(8,2).eq.1)write(21,5053)icumdy,ihr,iout,jout,templf(iout
     1,jout),delt(iout,jout),evap(iout,jout),heat(iout,jout),
     2gevap(iout,jout),gheat(iout,jout),rsleaf(iout,jout),
     3rsnovp(iout,jout)
 5053 format(' 2802',i3,i2,i2,i2,  6f7.2,2f7.1)
 5100 continue
 5150 continue
c  calc average flux density downward for shaded leaves. this
c    is for simplified model using only sunlit and shaded leaves.
c    must ave all d(k,j) above lai of interest
      do5190k=1,2
      do5190j=1,jtot
 5190 dave(k,j)=0.
      do5350k=1,2
      jtotm1=jtot-1
      do5350j=1,jtotm1
      jout=jtot-j+1
c     shade(k,jout)=d(k,jtot)*exp(-.5*clai(jout)**.7)+radabv(k)*fbeam(k)
c    1*.1*(1.-.1*clai(jout))*exp(-coszen)
c above stm was commented and following three were added by Chen, 9/7/89.
	if(coszen.lt.01)goto 5355
c above stm was added by chen, 01/23/90.
      x=0.5
      if(ispher.ne.1) x=xintz(int(thetas/(pi/2.)*90)+1)
      shade(k,jout)=d(k,jtot)*exp(-clump*x*clai(jout)**.7)
     1   +radabv(k)*fbeam(k)*.1*(1.-.1*clai(jout))*exp(-coszen)
 5355	continue
c above stm was added by chen, 01/23/90.
      do5300l=1,j
      lout=jtot-l+1
 5300 dave(k,jout)=dave(k,jout)+(d(k,lout)+d(k,lout-1))/2.
      dave(k,jout)=dave(k,jout)/j
 5350 continue
c     write(27,5373)ihr
 5373 format(1x,' ihr=',i3)
      do5375j=1,jtotm1
      jout=jtot-j+1
c     write(27,5376)clai(jout-1),dave(1,jout),shade(1,jout),dave(2,jout)
c    1,shade(2,jout)
 5376 format(1x,f5.2,4f6.1)
 5375 continue
 6550 continue
 7700 continue
      return
      end
