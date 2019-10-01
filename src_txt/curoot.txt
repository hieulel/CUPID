c $VERSION "08/16/95 @(#)curoot.f	7.1"
      subroutine rootex(ihr,iday,iter2,isrcgl,psisgl,wtpsis,
     &			invrgl,psi2)
c isrcgl,psisgl,wtpsis in above stm were added for invertion
c problem, cupidg. chen, 02/20/90.
c	----
c	| added psi2 to calling sequence to set lower limit
c	| for soil layers that can effect the plant potential
c	| JMN&LMM 94/10/21
c  subroutine to calculate root extraction from the soil
      parameter(mh=98)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/leaf1/delt(10,20),psilf,tran(10,20)
      common/leaf2/evap(10,20),gevap(10,20),heat(10,20),gheat(10,20)
     &,alam ,tlfavg(20),tgheat(20),tgvap1(20),tgvap2(20)
c akroot(50) added to /root1/ by MCA - 6/12/95
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1,rootup(50),cpytr,psisum,akroot(50)
      common/rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20),
     1templf(10,20),tsoil(mh)
      common/water1/iprecp,tprecp,pn(mh,50),wcond(50),wstor(50),
     & wpond(mh)
      common/soil2/aksol(50),akw(50),cw(50),wt(mh,50),esave(3,mh),
     & wnu(mh,50),wnl(mh,50)      
c     --------------
c     | rock variables added to soil4 LMM 94/9/8
c     | layer subscripts were added by MCA 5/24/95
      common/soil4/pe(25),bx(25),bd(25),aks(25),an(25),ws(25),asoil(25),
     & bsoil(25),csoil(25),dsoil(25),esoil,idoroc,irocly,akrock,cprock,
     & layid(25)
      common/invers/rhsoil
      common/jz1/jzsfc2
c above common stm was added for invertion, cupidg. chen, 02/20/90.
      cpytr=0.
      do10j=2,jtot
      do10i=1,itotp1
      cpytr=cpytr+tran(i,j)*frarea(i,j)
 10   continue
c  convert w m-2 to kg m-2 s-1
      cpytr=cpytr*df/(alam*4.18e3)
      jzsfc2=jzsfc+2
      jzbm1=jzbot-1
      if(iter2.gt.1)goto300
      if(ihr.eq.1.and.iday.eq.1)goto300
      ihrm1=ihr-1
      if(ihr.eq.1)ihrm1=mh
      do100jz=jzsfc2,jzbm1
 100  pn(ihr,jz)=pn(ihrm1,jz)
 300  continue
      psisum=0.
      wtavr=0.
      do200jz=jzsfc2,jzbm1
      if(pn(ihr,jz).ge.psi2) then
	  psisum=psisum+pn(ihr,jz)/resrot(jz)
      else 
	  psisum=psisum+(psi2+100.)/resrot(jz)
      endif
c  Sun add the following statement to the above one on 5/31/92	
c	------
c	| LMM added test for invrgl nov 22 93.
c
c     --------
c     | the following probably does nothing now that the else clause
c     | was added above.  Left "in case" we delete else stmnt. LMM 94/10/20
      if (invrgl.eq.1) then
	  if(pn(ihr,jz).lt.psi2)psisum=psisum+(psi2+10.)/resrot(jz)
      endif
c     write(19,*)ihr,jz,psisum,resrot(jz),rootsm,froot(jz),pn(ihr,jz)
 200  continue
	psisum=psisum/rootsm
c  The below line was altered by MCA 6/9/95
	if(psisum.gt.pe(1).and.ihr.eq.1.and.iday.eq.1)psisum=pe(1)
c *************** cupidg insert begin 1 chen, 02/16/90 **************
	if(isrcgl.eq.1) psisum=psisgl
	if(psisum.le.pe(1))then
	   wtpsis=ws(1)*(psisum/pe(1))**(-1./bx(1))
	else
	   wtpsis=ws(1)
	endif
         if(isrcgl.eq.1)write(6,*)' psitop=',psitop
c wtpsis is the volumetric water content corresponding to a uniform  
c water potential psisum, it is used only for calculating the factor 
c which will be multiplied by wt(j) to adjust artificially the water
c content profile when psisum is set arbitrally.
c *************** cupidg insert end   1 chen, 02/16/90 **************
c  rootsm is sum of reciprocals of root resis from main prog
c     if(cpytr.ge.0)psixy=(-cpytr+psisum)/rootsm
c     if(cpytr.lt.0.)psixy=psisum/rootsm
      if(cpytr.ge.0)psixy=-cpytr/rootsm+psisum
      if(cpytr.lt.0.)psixy=psisum
c  assume .6 of plant resis is in root so rleaf=2*rroot/3
      psitop=psixy-cpytr*rroot*0.6667
      if(cpytr.lt.0.)psitop=psixy
      do400jz=jzsfc2,jzbm1
      rootup(jz)=(pn(ihr,jz)-psixy)/resrot(jz)
      akroot(jz)=1/resrot(jz)
c  Sun change -1500 to -2500 in the following statement on 4/22/92	
      if(pn(ihr,jz).lt.psi2) then
        rootup(jz)=0.
        akroot(jz)=0.
      endif
 400  continue
      rootup(jzsfc+1)=0.
      akroot(jzsfc+1)=0.
      rootup(jzbot)=rootup(jzbm1)
      akroot(jzbot)=akroot(jzbm1)
900   return
      end
c
c-----------------------------------------------------------------------
c						      SUBROUTINE soilco2
c
      Subroutine soilco2(iday,ihr,co2sol,frlive)
c
c  calculate soil surface CO2 flux using eqs from FIFE in JGR
c    97:18,845-18,853.  1992.  p.18,850.
c  average water content between sfc and 10 cm depth
c  temp depth = .1 m and water averaged from 0 - .1 m
	  integer daymax
      parameter(mh=98,daymax=367)
c     ----------
c     | Added fraction live to multiply with totlai
c     | in equation to determine the co2sol.
c     | LMM 94/9/8
      dimension frlive(daymax)
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/soil2/aksol(50),akw(50),cw(50),wt(mh,50),esave(3,mh),
     & wnu(mh,50),wnl(mh,50)      
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),uprof(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
     2,akcpynut(50),akcpyold(50)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
      zco2=0.1
      if(z(jzsfc+2).gt.zco2)then
        write(6,*)'Top soil layer too thick stop in subr. soilco2'
        stop
      endif
      dzco2=(z(jzsfc+2)-z(jzsfc+1))/2.
      wttot=wt(ihr,jzsfc+1)*dzco2
      dzco2s=dzco2
      do 100 jz=jzsfc+2,jzbot
      if(z(jz).lt.zco2) then
        dzco2=(z(jz+1)-z(jz-1))/2.
        wttot=wttot+wt(ihr,jz)*dzco2
        dzco2s=dzco2s+dzco2
        jzco2=jz
      endif
 100  continue
c
      wtave=100.*wttot/dzco2s
      dz=(zco2-z(jzco2))/(z(jzco2+1)-z(jzco2))
      ts=tn(ihr,jzco2)+(tn(ihr,jzco2+1)-tn(ihr,jzco2))*dz
      co2sol=(.135+0.054*totlai*frlive(iday))*wtave*exp(.069*(ts-25.))
c     write(16,*)' ihr= ',ihr
c     do1000jz=jzsfc+1,jzsfc+8
c     write(16,*)jz,z(jz),wt(ihr,jz),tn(ihr,jz)
c1000 continue
c     write(16,*)'wtave= ',wtave,' ts= ',ts,' co2sol= ',co2sol
      return
      end
c
