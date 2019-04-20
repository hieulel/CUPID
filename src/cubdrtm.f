c $VERSION "08/16/95 @(#)cubdrtm.f	7.1"
      subroutine bdrtm(ihr,factir,tsfcbd,ipp,nvzenp,vwzenp,  
     1tcpyir,tcpyap,emiscp,pathhr)
      dimension  cthetv(9),sthetv(9),sinalp(9),cosalp(9),
     1   term1(9,20),term2(9,20),term3(9,20),term4(9,20),term5(9),
     2   term6(9),term7(9,9),term8(9,9),cotsmb(50),copvmb(50,50),
     3   nthetv(9),relazm(50),relang(50),
     4   tcpyir(9,50),tcpyap(9,50),emiscp(9,50),
     5   sup(20),sdn(20),d(3,20),u(3,20),rtdifd(20),rtdifu(20),
     6   tlayer(20),rlayer(20) 
      parameter(mh=98)
c     ---------
c     | rad1 cmn added rlleaf, tlleaf, rldead and tldead LMM 94/9/6
      common /rad1/emis,emisol,rsoil(3),rleaf(3,20),tleaf(3,20),
     &aleaf(3,20),expdif,rlayr(3,20),tlayr(3,20),rllive(3),tllive(3),
     &rldead(3),tldead(3)
      common /rad2/dstrad(3,10,20),dstnet(10,20),frarea(10,20)
     &,templf(10,20),tsoil(mh)
      common /rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),hfday
     &,ratiod,ration,ratio(mh)
      common/rad6/rlfdif(3,20),tlfdif(3,20),rlfdir(3,20,mh),
     &tlfdir(3,20,mh)
      common/rad7/radabv(3),fbeam(3),fspec(3),wtir(20,10,50)
      common/indax3/xmeuaz,xneuaz,beta0,nbeta,fraz(50),fradeg(360)
c above common was added by Chen, 05/18/89.
      common/misc1/pi,pid180,pid2,sigma,iwrite(9,99),kmax
c pid2,kmax in /misc1/ were added by Chen, 9/4/89.
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/misc6/xint(mh),nxintz,xintz(90),nalpha,ispher
c xintz, extin. coef. at different zenith averaged over azimuth, and 
c nxtintz, dimension of xintz, Chen, 05/25/89.
      common/time/month,jday,iyear,icumdy,timloc(24)
      common/deg/sunazm(mh),nozenv,viewzn(10),noazmv,viewaz(50)
c 
      dimension vwzenp(nvzenp)
c ipp: =1, bdr were calc only in prinnciple plane.   
c nvzenp: number of view zenith measured
c vwzenp(i),i=1,nvzenp: view zenith angle measured (degree)
      write (*,*) "BDRTM subroutine - cubdrtm.f"
      if(ipp.ne.1) goto 100
      nozenv=nvzenp
      noazmv=2
c
c  To get a single view angle like nadir, set ipp=1 and nozenv=1
       if(nozenv.eq.1)noazmv=1
c
      do 110 i=1,nvzenp
110   viewzn(i)=vwzenp(i)
      viewaz(1)=sunazm(ihr)/pid180
      viewaz(2)=viewaz(1)+180.
      if(viewaz(2).gt.360.) viewaz(2)=viewaz(2)-360.
100   continue
c
	kt=1
        k=3
c  kt is for the wavelength we are interested in for the ir thermometer
c    that has low sky radiation in the atmos window but k is from
c    subr radiat and the thermal properties of the leaves and soil
c    remain the same as for the broad band.
c 
c  Thermal routine extracted from subr radiat 10-6-89
c
c
c  factir is the factor that the thermal sky flux calc'd from subr
c    skyir is multiplied by to get sky thermal in band of interest.
c
c  compute thermal layer properties
	  write (*,*) "thermal layer properties"
      do 4000 j=2,jtot
        tlayer(j)=tlayr(k,j)
        rlayer(j)=rlayr(k,j)
 4000 continue
c  compute thermal layer properties
c
c  treat thermal wavelengths.
c  compute thermal source terms
c  check to see if solar has been executed(day or night)
      sdn(1)=0.
 4020 do 4140 j=2,jtot
      source=0.
      do 4050 i=1,itotp1
      dstrad(3,i,j)=emis*sigma*(templf(i,j)+273.)**4
      source=source+frarea(i,j)*dstrad(3,i,j)
 4050 continue
      sdn(j)=source*(1.-expdif)
 4140 sup(j)=source*(1.-expdif)
c  thermal boundary conditions
c  tsfc is soil sfc temp for each iteration.  its calc'd in main
c    program after profl2 is called.
      esoil =emisol*sigma*(tsfcbd+273.)**4
      d(kt,jtot)=radabv(3)*factir
c  assume thermal reflectance =0 and call u+d
      do 4560 j=2,jtot
      jm1=j-1
      jj=jtot-j+1
      jjp1=jj+1
 4560 d(kt,jj)=tlayer(jjp1)*d(kt,jjp1)+sdn(jjp1)
      sup(1)=d(kt,1)*(1.-emisol)
      u(kt,1)=esoil+sup(1)
      do4670j=2,jtot
      jm1=j-1
 4670 u(kt,j)=tlayer(j)*u(kt,jm1)+sup(j)
c  iterate 2 times with refl=1-emis
 4580 do 4910 jjj=1,2
      do 4820 j=2,jtot
      jm1=j-1
      jj=jtot-j+1
      jjp1=jj+1
 4820 d(kt,jj)=tlayer(jjp1)*d(kt,jjp1)+u(kt,jj)*rlayer(jjp1)
     &          +sdn(jjp1)
      sup(1)=d(kt,1)*(1.-emisol)
      u(kt,1)=esoil+sup(1)
      do4810j=2,jtot
      jm1=j-1
 4810 u(kt,j)=rlayer(j)*d(kt,j)+u(kt,jm1)*tlayer(j)+sup(j)
 4910 continue
c  Bidirectional thermal  subroutine put together by chen
c
c  thets=zenith angle of sun
c  phis =azimuth angle of sun south = 0., east = -90.
c  thetv=zenith angle of view
c  phiv =azimuth angle of view
c  alpha=leaf elevation angle from horizontal
c  beta =azimuthal direction leaf normal points
c  calc trig functions of angles first to save time
c
c In this subroutine 4 different canopy temeratures are calculated:
c t1: average vegetation kinetic temerature.
c t2(ithetv,iphiv): average directional kinetic temperature.
c t4: hemispherical infrared temperature.
c t5(ithetv,iphiv): apparent directional infrared temperature.
c t1 and t4 are calculated by setting factir=.99, t2 and t5 is 
c calculated by setting factir=.2 in input file. chen, 03/06/90.
c
c t4: assuming canopy emmisivity ecpy=.99. chen,03/06/90.
	if(abs(factir-.99).lt..001) then
	ecpy=.99
	t4=((u(kt,jtot)-(1.-ecpy)*d(kt,jtot))/ecpy/sigma)**.25-273.
	endif
c above 4 stm were added by chen, 03/06/90.
      thets=zenang(ihr)
      phis =sunazm(ihr)
      cthets=cos(thets)
      sthets=sin(thets)
      do4165iangv=1,nozenv
      angv=viewzn(iangv)*pid180
      cthetv(iangv)=cos(angv)
      sthetv(iangv)=sin(angv)
 4165 continue
      do4150ialpha=1,itot
      alpha=(90./(2.*itot)+(ialpha-1)*90./itot)*pid180
      sinalp(ialpha)=sin(alpha)
      cosalp(ialpha)=cos(alpha)
c  calc the 4 terms of the shad eq in beta loop to save time
      cap1=1.+cosalp(ialpha)
      cam1=1.-cosalp(ialpha)
      do4110j=2,jtot
c  this jm1 was not in betty's prog but it came in calling seq and
c    was set to 10 from difint call seq.
      jm1=j-1
      d5=.5*d(kt,j)
      u5=.5*u(kt,jm1)
      term1(ialpha,j)=cap1*d5
      term2(ialpha,j)=cam1*d5
      term3(ialpha,j)=cam1*u5
      term4(ialpha,j)=cap1*u5
 4110 continue
c  calc some of the terms of fs and fv
      term5(ialpha)=cthets*cosalp(ialpha)
      term6(ialpha)=sthets*sinalp(ialpha)
      do4120 ithetv=1,nozenv
        term7(ialpha,ithetv)=cthetv(ithetv)*cosalp(ialpha)
        term8(ialpha,ithetv)=sthetv(ithetv)*sinalp(ialpha)
 4120 continue
 4150 continue
      wbeta=2.*pi/nbeta
      do 4160 ibeta=1,nbeta
	beta=wbeta*(ibeta-.5)
        cotsmb(ibeta)=cos(phis-beta)
c  calc cos(phiv-beta) just once here to save time
        do4160iphiv=1,noazmv
          phiv=viewaz(iphiv)*pid180
          copvmb(ibeta,iphiv)=cos(phiv-beta)
 4160 continue
      do4500ithetv=1,nozenv
         thetv=viewzn(ithetv)*pid180
      do4400iphiv=1,noazmv
      phiv=viewaz(iphiv)*pid180
c	sumslt=0.
c	do 4402 j=2,jtot
c	sumslt=sumslt+(1.-frarea(itotp1,j))*wtir(j,ithetv,iphiv)
c4402	continue
c	write(6,*)'ithetv,iphiv,sumsunlit=',ithetv,iphiv,sumslt
      esoil=emisol*sigma*(tsfcbd+273.)**4  
      sumfv2=0.
      srdlf2=0.
      tir2=0.
      tir42=0.
      tavr2=0.
c above stm was added for calculating t1 by chen, 03/06/90.
      do4300ialph=1,itot
      sumfv=0.
      sradlf=0.
      tir=0.
      tir4=0.
      tavr=0.
c above stm was added for calculating t1 by chen, 03/06/90.
      do 4200 ibeta=1,nbeta
        fv=term7(ialph,ithetv)+term8(ialph,ithetv)*copvmb(ibeta,iphiv)
c	----------------
c	| commented 94/4/19 LMM JN
c        if(abs(sunazm(ihr)).lt..01) goto 4203
        fs=term5(ialph)+term6(ialph)*cotsmb(ibeta)
        angslf=acos(abs(fs))
        iangle=int(angslf/(pi/2)*itot)+1
        if (iangle.gt.itotp1.or.iangle.lt.1) then
	    write(*,*)'iangle is <1 or > itot in sub bdrtm, iangle = ',
     &			    iangle
	    stop
        endif
	if (iangle.gt.itot) iangle=itot
 4203 continue
      do 4205 j=2,jtot
        rtdifd(j)=rlfdif(k,j)
        rtdifu(j)=tlfdif(k,j)
        if(fv.lt.0.)rtdifd(j)=tlfdif(k,j)
        if(fv.lt.0.)rtdifu(j)=rlfdif(k,j)
 4205 continue      
      fv=abs(fv)
      srdlf1=0.
      tir1=0.
      tir41=0.
      tavr1=0.
c above stm was added for calculating t1 by chen, 03/06/90.
      do4100j=2,jtot
      shad=term1(ialph,j)*rtdifd(j)+term2(ialph,j)*rtdifu(j)
     &+term3(ialph,j)*rtdifd(j)+term4(ialph,j)*rtdifu(j)
      sunlit=1.-frarea(itotp1,j)
c     sunlit=1.
      if(abs(sunazm(ihr)).lt..01) sunlit=0.
      tlfsun=templf(iangle,j)
c     tlfsun=templf(iangle,jtot)
      tlfshd=templf(itotp1,j)
c     tlfshd=templf(itotp1,jtot)
      eleaf=emis*sigma*(sunlit*(tlfsun+273.)**4+
     1      (1-sunlit)*(tlfshd+273.)**4)
      radlf2=shad+eleaf
      tlf2=sunlit*tlfsun+(1-sunlit)*tlfshd
      srdlf1=srdlf1+wtir(j,ithetv,iphiv)*radlf2
      tir1=tir1+wtir(j,ithetv,iphiv)*tlf2
      tir41=tir41+wtir(j,ithetv,iphiv)*(tlf2+273.)**4
      tavr1=tavr1+tlf2
c above stm was added for calculating t1 by chen, 03/06/90.
 4100 continue
      sumfv=sumfv+fv*fraz(ibeta)
      sradlf=sradlf+srdlf1*fv*fraz(ibeta)
      tir=tir+tir1*fv*fraz(ibeta)
      tir4=tir4+tir41*fv*fraz(ibeta)
	tavr=tavr+tavr1*fv*fraz(ibeta)
c above stm was added for calculating t1 by chen, 03/06/90.
c     write(22,4290)phiv,beta,fs,fv
 4290 format(1x,4f8.5)
 4200 continue
      sumfv2=sumfv2+sumfv*fr(ialph)
      srdlf2=srdlf2+sradlf*fr(ialph)
      tir2=tir2+tir*fr(ialph)
      tir42=tir42+tir4*fr(ialph)
	tavr2=tavr2+tavr*fr(ialph)
c above stm was added for calculating t1 by chen, 03/06/90.
 4300 continue
      srdlf2=srdlf2/sumfv2
      tir2=tir2/sumfv2
      tir42=tir42/sumfv2
	tavr2=tavr2/sumfv2
c above stm was added for calculating t1 by chen, 03/06/90.
      thmcp=srdlf2+wtir(1,ithetv,iphiv)*(d(kt,1)*rsoil(k)+esoil)
      tcpyir(ithetv,iphiv)=tir2+wtir(1,ithetv,iphiv)*tsfcbd
c when factir=.2, tcpyir is t2.
      tcpir4=tir42+wtir(1,ithetv,iphiv)*(tsfcbd+273.)**4
      if (tcpir4 .eq. 0.0) then
c          ---------
c          | tcpir4 .eq. o.o when wtir(... is 0.0, which happens when
c          | the sun can't see the slope
           emiscp(ithetv,iphiv) = 0.0
      else
c         emiscp(ithetv,iphiv)=thmcp/(tcpyir(ithetv,iphiv)+273.)**4/sigma
          emiscp(ithetv,iphiv)=thmcp/tcpir4/sigma
      endif
c
c      if (ihr.ge.24.or.ihr.le.26) write(15,*)'ihr=',ihr,'
c     1tsfc=',tsfc,'tsfcbd=',tsfcbd
	t1=tavr2/(jtot-1)
c above stm was added for calculating t1 by chen, 03/06/90.
c tsoil(ihr) in above two stms was originally tsfc, chen, 01/19/90.
c tsfcbd added by jmn 5/15/92 which is tn(ihr,jzsfc+1) from call seq.
c       if(ihr.eq.10)write(15,*)'ihr',ihr,' emiscp',emiscp(ithetv,iphiv),
c     1' thmcp',thmcp,' tcpir4',tcpir4,' sigma',sigma
c       if(ihr.eq.24)write(15,*)'ihr',ihr,' emiscp',emiscp(ithetv,iphiv),
c     1'thmcp=',thmcp,'tcpir4=',tcpir4,'sigma=',sigma
c       if(ihr.eq.25)write(15,*)'ihr',ihr,' emiscp',emiscp(ithetv,iphiv),
c     1'thmcp=',thmcp,'tcpir4=',tcpir4,'sigma=',sigma
c       if(ihr.eq.26)write(15,*)'ihr',ihr,' emiscp',emiscp(ithetv,iphiv),
c     1'thmcp=',thmcp,'tcpir4=',tcpir4,'sigma=',sigma
      if(thmcp.le.1.0e-6) then 
c         ---------
c         | slope not visible to view wtir(1.. set to 0.0
          tcpyap(ithetv,iphiv)=0.
      else
          tcpyap(ithetv,iphiv)=(thmcp/sigma)**(.25)-273.
      endif
c when factir=.2, tcpyap is t5.
c	write(6,*)'ithetv,iphiv,tcpyir,emiscp=',ithetv,iphiv,
c     1             tcpyir(ithetv,iphiv),emiscp(ithetv,iphiv)
 4400 continue
 4500 continue
 4550 continue
      do4980ithetv=1,nozenv
 4980 nthetv(ithetv)=viewzn(ithetv)
c                                                  more file write 7
	if(abs(factir-.99).lt..001) then
      if(iwrite(7,7).eq.1) write(21,4981)icumdy,ihr
 4981 format(' 1707',i3,i2,4x/14x,'      t1      t4')
      if(iwrite(7,7).eq.1) write(21,4982)icumdy,ihr,t1,t4
 4982 format(' 2707',i3,i2,4x,2f8.3)
	goto 4830
	endif
	if(abs(factir-.99).ge..001) then
      if(iwrite(7,7).eq.1) write(21,4984)icumdy,ihr,
     1(nthetv(l),l=1,nozenv)
 4984 format(' 1707',i3,i2,4x/14x,'viewaz ',6(' tb4',i3),' (t2)')
      do4800iphiv=1,noazmv
      if(iwrite(7,7).eq.1) write(21,4600)icumdy,ihr,iphiv,viewaz(iphiv),
     1(tcpyir(ithetv,iphiv),ithetv=1,nozenv)
 4600 format(' 2707',i3,i2,i2,2x,10f7.2)
 4800 continue
      if(iwrite(7,8).eq.1) write(21,4985)icumdy,ihr,
     1(nthetv(l),l=1,nozenv)
 4985 format(' 1708',i3,i2,4x,/14x,'viewaz ',6(' em4',i3),' (emiscp)')
      do4900iphiv=1,noazmv
      if(iwrite(7,8).eq.1) write(21,4601)icumdy,ihr,iphiv,viewaz(iphiv),
     1(emiscp(ithetv,iphiv),ithetv=1,nozenv)
 4601 format(' 2708',i3,i2,i2,2x,f7.2,10f7.3)
 4900 continue
      if(iwrite(7,9).eq.1) write(21,4986)icumdy,ihr,
     1(nthetv(l),l=1,nozenv)
 4986 format(' 1709',i3,i2,4x,/14x,'viewaz ',6(' ta4',i3),' (t5)')
      do4806iphiv=1,noazmv
      if(iwrite(7,9).eq.1) write(21,4606)icumdy,ihr,iphiv,viewaz(iphiv),
     1(tcpyap(ithetv,iphiv),ithetv=1,nozenv)
 4606 format(' 2709',i3,i2,i2,2x,10f7.2)
 4806 continue
	goto 4830
	endif
      write(6,4831) factir
4831  format(/'factir has to be either .99 for whole band or .2 for',
     1 ' narow band. now factir=',f5.2)
4830	continue
c ******* hot spot section begin, Chen, 9/25/89 ************************
c  if fbeam1(2,ihr) is small, skip hopspot calc.
c
c     --------------
c     | Skip hot spot if sun can't see slope
      if(cthets.lt..01.or.fbeam1(2,ihr).lt.0.01.or.pathhr.lt.-.99) 
     &          goto 5530
c
      esoil=emisol*sigma*(tsfcbd+273.)**4  
      sumfv2=0.
      srdlf2=0.
      tir2=0.
      do4330ialph=1,itot
      sumfv=0.
      sradlf=0.
      tir=0.
      do 4230 ibeta=1,nbeta
      fs=term5(ialph)+term6(ialph)*cotsmb(ibeta)
	angslf=acos(abs(fs))
	iangle=int(angslf/(pi/2)*itot)+1
	if (iangle.gt.itot) iangle=itot
      fv=abs(fs)
      sumfv1=0.
      srdlf1=0.
      tir1=0.
      do4130j=2,jtot
      shad=term1(ialph,j)*rtdifd(j)+term2(ialph,j)*rtdifu(j)
     &+term3(ialph,j)*rtdifd(j)+term4(ialph,j)*rtdifu(j)
      sunlit=1.-frarea(itotp1,j)
      tlfsun=templf(iangle,j)
      eleaf=emis*sigma*(tlfsun+273.)**4
      radlf2=shad+eleaf
      tlf2=tlfsun
c note: shaded leaves are not considered in calculation of eleaf,radlf2,
c       and tlf2 under hot spot condition.
      sumfv1=sumfv1+sunlit
      srdlf1=srdlf1+radlf2*sunlit
      tir1=tir1+tlf2*sunlit
 4130 continue
      sumfv=sumfv+sumfv1*fv*fraz(ibeta)
      sradlf=sradlf+srdlf1*fv*fraz(ibeta)
      tir=tir+tir1*fv*fraz(ibeta)
 4230 continue
      sumfv2=sumfv2+sumfv*fr(ialph)
      srdlf2=srdlf2+sradlf*fr(ialph)
      tir2=tir2+tir*fr(ialph)
 4330 continue
      srdcpy=srdlf2/sumfv2
      tircpy=tir2/sumfv2
      sumwtr=sumfv2*df*pathhr
c srdcpy: average radiance of canopy.
c tircpy: average temperature of canopy for sunlit leaves.
c sumwtr: fraction of projection of canopy onto view direction.
      wtirsl=1.-sumwtr
      thmcp=sumwtr*srdcpy+wtirsl*(d(kt,1)*rsoil(k)+esoil)
      tirhot=sumwtr*tircpy+wtirsl*tsfcbd
      emihot=thmcp/(tirhot+273.)**4/sigma
      taphot=(thmcp/sigma)**(.25)-273.
c check if tircpy is within the range of maximum and minimum leaf temp.
	tlfmax=-100.
	tlfmin=100.
	do 5101 j=2,jtot
c	j=jtot
	do 5101 iangle=1,itot+1
c	do 5101 iangle=1,itot
	if (templf(iangle,j).gt.tlfmax) tlfmax=templf(iangle,j)
	if (templf(iangle,j).lt.tlfmin) tlfmin=templf(iangle,j)
5101	continue
      if(iwrite(7,21).eq.1) write(21,5103) icumdy,ihr
 5103 format(' 1721',i3,i2,4x,/14x,' wtirsl   tsfc tlfmax tlfmin',
     1' tircpy tirhot taphot emihot')
      if(iwrite(7,21).eq.1) write(21,5105) icumdy,ihr,
     1wtirsl,tsfcbd,tlfmax,tlfmin,tircpy,tirhot,taphot,emihot
 5105 format(' 2721',i3,i2,4x,f7.3,6f7.2,f7.3)
c ****** hot spot section end   ***********************************
 5530   continue
 9000 return
      end
