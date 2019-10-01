c $VERSION "08/16/95 @(#)cuinfil.f	7.1"
c
      subroutine soilw(ihr,ihrm1,iday,eref,tref,evsoil,rhsoil,rhev,
     &                 rhsfc,irecyc,wgrav)
c
c-----------------------------------------------------------------------
c  Computes the soil water profile for a given timestep.  Based on
c  code provided by G. Campbell.
c
c  Returns:
c    wt(ihr,jz)  	Volumetric water content of soil layer
c    cw(jz)		Capillary water capacity of soil divided      
c                       by a layer depth increment (kgsm-4)
c    akw(jz)            Capillary conductivity of a layer in soil 
c                       divided by layer depth increment (kgsm-4)
c    pn(ihr,jz)         Soil layer water potential
c    drain              Drainage out bottom of root zone  
c                       (kgm-2s-1=mms-1)
c
c-----------------------------------------------------------------------
c
      parameter(mh=98)
      parameter(wd=1000.)
      parameter(gr=9.8)
      parameter(wm=0.018)
      parameter(r=8.31)
      parameter(dv=2.4e-5)
      parameter(vd=0.017)
      parameter(ha=0.5)
      dimension a(50),b(50),c(50),f(50),fv(50),fl(50),dp(50),source(50)
      dimension h(50),cpl(50),cpu(50),akl(50),aku(50),v(50)
      dimension phil(50),phiu(50),akbar(50),akv(50),dkdpl(50)
      dimension dkdpu(50),wtlast(50),dz(50),wgrav(50),rhsfc(3)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/prof2/tn(mh,50),akcpy(50),cpcpy(50),u(50),q(50),et(50),
     1en(mh,50),qcond(50),econd(50),tcheck(50),esat(50),qwater(50)
     2,akcpynut(50),akcpyold(50)
      common/prof3/ustara,ustars,phima,phims,akh(50),cp(50)
      common/cpy2/hsoil,hcpys,evtot,etotw(20),contot(20),scond(10,20)
     1,ihrwet(20),ecpys,cphstr,cpestr,wcpys,evsmic
      common/soil1/zsoil(25),wti(50),ndsoil,aksoil(50),cpsoil(50),tsn(50
     1)
      common/soil2/aksol(50),akw(50),cw(50),wt(mh,50),esave(3,mh),
     & wnu(mh,50),wnl(mh,50)
      common/soil4/pe(25),bx(25),bd(25),aks(25),an(25),ws(25),asoil(25),
     & bsoil(25),csoil(25),dsoil(25),esoil,idoroc,irocly,akrock,cprock,
     & layid(25)
      common/leaf2/evap(10,20),gevap(10,20),heat(10,20),gheat(10,20)
     &,alam ,tlfavg(20),tgheat(20),tgvap1(20),tgvap2(20)
      common/water1/iprecp,tprecp,pn(mh,50),wcond(50),wstor(50),
     & wpond(mh)
      common/water2/sw,deld,drhsfc,rhslop,drain,drain5,filt,etmm,drgrav
      common/water3/swlast
      common/iterat/noiter,iter2,iter3,iterw,loope,loopt,loopw     
      common/root1/froot(50),resrot(50),rootsm,psixy,psitop,rroot
     1,rootup(50),cpytr,psisum,akroot(50)       
c
c      write(6,*)rhsfc(2),rhev
c
	  write (*,*) "soilw subroutine is called - cuinfil.f"
      jzsfc1=jzsfc+1
      jzsfc2=jzsfc+2
      jzbm1=jzbot-1
      jzbm2=jzbot-2
      deldi=deld
      ideld=0
c
c  Compute water source at each soil node: 
c  source(jzsfc+1) = transmitted precip + ponded water - soil sfc evap
c  up to a maximal infiltration rate of 5*gr*aks (excess is ponded).
c  Root uptake is a sink at all other levels. 
c  
c      source(jzsfc1)=tprecp+wpond(ihrm1)*dt-
c     &                    (evsoil+rhev*(rhsoil-rhsfc(2)))*1.e-6
      source(jzsfc1)=tprecp+wpond(ihrm1)/dt
      sourmx=5.*gr*aks(1)                          
      if(source(jzsfc1).gt.sourmx)then
        wpond(ihr)=(source(jzsfc1)-sourmx)*dt
        source(jzsfc1)=sourmx
        write(6,5000)wpond(ihr)
 5000   format(' Ponded ',f7.3,' mm water this timestep.') 
      else
        wpond(ihr)=0.0
      endif 
      do100jz=jzsfc2,jzbm1
        source(jz)=-rootup(jz)
 100  continue  
c
c  Compute hydraulic properties at upper boundary (jzsfc=lower boundary
c  for canopy). Surface evaporation is included in the source term at
c  level jzsfc1.
c
c    pn(ihr,jzsfc) always occurs as a factor with ak(jzsfc) which
c    is always 0; therefore, the exact value of pn(ihr,jzsfc) is
c    unimportant.  Choose a reasonable value.
c
      xlog=en(ihr,jzsfc)/eref
      if(xlog.le.0.)write(6,5100)en(ihr,jzsfc),eref
 5100 format(' Trouble with p(jzsfc) in subroutine soilw',2e12.6)
      if(xlog.le.0.)xlog=0.01
      pn(ihr,jzsfc)=r*(tn(ihr,jzsfc1)+273.)*alog(xlog)/wm
c      
      akl(jzsfc)=0.0
      aku(jzsfc)=0.0
      akv(jzsfc)=0.0
      fl(jzsfc)=0.0
      fv(jzsfc)=0.0
      dkdpu(jzsfc)=0.0
      dkdpl(jzsfc)=0.0
      cpl(jzsfc)=0.0
      cpu(jzsfc)=0.0
      wnl(ihr,jzsfc)=0.0
      wnu(ihr,jzsfc)=0.0
c
      do150jz=jzsfc,jzbm1
        v(jz)=(z(jz+1)-z(jz-1))*1000./2.      
 150    dz(jz)=z(jz+1)-z(jz)
      v(jzsfc+1)=(z(jzsfc+2))*1000./2.
c
c  Newton-Raphson loop begins here. Loopw is the NR iteration counter.
c
      loopw=0
 200  continue
      se=0.
c
c  Compute layer hydraulic properties
c
      write(*,*) "compute hydraulic properties"
      do300jz=jzsfc1,jzbm1
        jz1=jz-jzsfc
        call hydro(pn(ihr,jz+1),pe(jz1),ws(jz1),an(jz1),bx(jz1),
     &        aks(jz1),wnl(ihr,jz),akl(jz),phil(jz),dwdp)
        if(pn(ihr,jz).lt.pe(jz1))then
          h(jz)=exp(wm*pn(ihr,jz)/(r*(tn(ihr,jz)+273.)))
        else
          h(jz)=1.0
        endif
        cpl(jz)=wd*dwdp/(2.*dt)
        if((jz.ne.jzsfc1).and.(layid(jz1).eq.layid(jz1-1)))then
          aku(jz)=akl(jz-1)
          phiu(jz)=phil(jz-1)
          cpu(jz)=cpl(jz-1)
          wnu(ihr,jz)=wnl(ihr,jz-1)
        else
          call hydro(pn(ihr,jz),pe(jz1),ws(jz1),an(jz1),bx(jz1),
     &          aks(jz1),wnu(ihr,jz),aku(jz),phiu(jz),dwdp)
          cpu(jz)=wd*dwdp/(2.*dt)        
        endif 
        if(wnu(ihrm1,jz).eq.-999.99)then
c     the below initialization put bad kinks into the water profile
c     at layer interfaces.
c          wnu(ihrm1,jz)=wnu(ihr,jz)
c          wnl(ihrm1,jz)=wnl(ihr,jz)
          wnu(ihrm1,jz)=wt(ihr,jz)
          wnl(ihrm1,jz)=wt(ihr,jz)
        endif
        if(loopw.eq.0)then
          wnu(ihr,jz)=wnu(ihrm1,jz)
          wnl(ihr,jz)=wnl(ihrm1,jz)
        endif
        if(abs(pn(ihr,jz)-pn(ihr,jz+1)).lt.1.0)then
          akbar(jz)=aku(jz) 
        else
          akbar(jz)=(phiu(jz)-phil(jz))/(pn(ihr,jz)-pn(ihr,jz+1))
        endif    
 300  continue
      if(pn(ihr,jzbot).lt.pe(jzbot-jzsfc))then
        h(jzbot)=exp(wm*pn(ihr,jzbot)/(r*(tn(ihr,jzbot)+273.)))
      else
        h(jzbot)=1.0
      endif 
c
c  Add evaporation sink to surface layer (based on this loop's rh).
c
c      source(jzsfc1)=source(jzsfc1)-
c     &                 (evsoil+rhev*(rhsoil-rhsfc(2)))*1.e-6
c
c  Compute Jacobian
c
      do400jz=jzsfc1,jzbm1
        jz1=jz-jzsfc
        if(abs(pn(ihr,jz)-pn(ihr,jz+1)).lt.1e-10)then
          fl(jz)=-gr*aku(jz)
          fv(jz)=0.
          akv(jz)=0.
          if(pn(ihr,jz).ge.pe(jz1))then
            dkdpu(jz)=0.
            dkdpl(jz)=0.
          else
            dkdpu(jz)=-an(jz1)*aku(jz)/pn(ihr,jz)
            dkdpl(jz)=dkdpu(jz)
          endif
        else
          fl(jz)=(phil(jz)-phiu(jz))*(1./dz(jz)
     &                  +gr/(pn(ihr,jz)-pn(ihr,jz+1)))
          akv(jz)=0.66*dv*vd*(ws(jz1)-(wnu(ihrm1,jz)+wnu(ihr,jz)+
     &           wnl(ihrm1,jz)+wnl(ihr,jz))/4.)*(h(jz+1)-h(jz))/
     &           ((pn(ihr,jz+1)-pn(ihr,jz))*dz(jz)) 
          fv(jz)=akv(jz)*(pn(ihr,jz+1)-pn(ihr,jz))
          if(pn(ihr,jz).lt.pe(jz1))then
            dkdpu(jz)=(akbar(jz)-aku(jz))/(pn(ihr,jz+1)-pn(ihr,jz))
          else
            dkdpu(jz)=0.
          endif
          if(pn(ihr,jz+1).lt.pe(jz1))then
            dkdpl(jz)=(akl(jz)-akbar(jz))/(pn(ihr,jz+1)-pn(ihr,jz))
          else
            dkdpl(jz)=0.
          endif
        endif
        a(jz)=-aku(jz-1)/dz(jz-1)-gr*dkdpu(jz-1)-akv(jz-1)
        c(jz)=-akl(jz)/dz(jz)-gr*dkdpl(jz)-akv(jz)
        b(jz)=akl(jz-1)/dz(jz-1)+aku(jz)/dz(jz)+akv(jz-1)+akv(jz)
     &        +cpu(jz)*dz(jz)+cpl(jz-1)*dz(jz-1)+gr*(dkdpl(jz-1)
     &        +dkdpu(jz))+akroot(jz)         
        f(jz)=wd*((wnu(ihr,jz)-wnu(ihrm1,jz))*dz(jz)
     &        +(wnl(ihr,jz-1)-wnl(ihrm1,jz-1))*dz(jz-1))/(2.*dt)
     &        +fl(jz-1)-fl(jz)+fv(jz-1)-fv(jz)
     &        -source(jz)
        if(jz.eq.jzsfc1)f(jzsfc1)=f(jzsfc1)+
     &                 (evsoil+rhev*(rhsoil-rhsfc(2)))*1.e-6
        dum1=fl(jz-1)+fv(jz-1)
        dum2=-fl(jz)-fv(jz)
        dum3=wd*((wnu(ihr,jz)-wnu(ihrm1,jz))*dz(jz)
     &        +(wnl(ihr,jz-1)-wnl(ihrm1,jz-1))*dz(jz-1))/(2.*dt)
        dum4=-source(jz)
c        write(6,3794)ihr,jz,dum1,dum2,dum3,dum4
 3794   format(2i3,4e12.5)        
        se=se+abs(f(jz))
 400  continue
c
c  Include slope of evap vs sfc rel hum in b(jzsfc1)
c
      b(jzsfc1)=b(jzsfc1)+rhev*(.018*rhsoil/
     &                       (8.314*(tref+273.)))*1.e-6  
c
c  Use the Thomas Algorithm to solve the system
c
      write (*,*) "Thomas algorithm - cuinfil.f"
      do500jz=jzsfc1,jzbm2
        c(jz)=c(jz)/b(jz)
        f(jz)=f(jz)/b(jz)
        b(jz+1)=b(jz+1)-a(jz+1)*c(jz)
        f(jz+1)=f(jz+1)-a(jz+1)*f(jz)
 500  continue
c
c  Increment potentials (limit to increment taken from Ross 19??)
c
      write (*,*) "increment potential limit - cuinfil.f"
      dp(jzbm1)=f(jzbm1)/b(jzbm1)
      pn(ihr,jzbm1)=pn(ihr,jzbm1)-dp(jzbm1)       
      do600jz=jzbm2,jzsfc1,-1
        dp(jz)=f(jz)-c(jz)*dp(jz+1)
c    Without this if statement, p can get locked at 0...
        if(abs(pn(ihr,jz)).gt.2.)then
          dplim=0.8*abs(pn(ihr,jz))
        else
          dplim=2.0
        endif
        abv=abs(dp(jz))
        if(abv.gt.dplim) dp(jz)=dplim*dp(jz)/abv
        pn(ihr,jz)=pn(ihr,jz)-dp(jz)       
 600  continue
      rhsoil=exp(pn(ihr,jzsfc1)*.018/(8.314*(tref+273.))) 
c
c  Recycle Newton-Raphson loop if flux-balance not satisfied (50 iter max)
c
      write (*,*) "recycle newton-raphson - cuninfil.f"
      if(se.lt.deld) go to 650
      loopw=loopw+1
      if(loopw.ge.50) then
        write(6,5200)ihr,loopw
 5200   format(' ihr = ',i3,': ',i3,
     &           ' iterations on Newton-Raphson soil water') 
        go to 650
      endif     
      go to 200
c
c  Find average water values at each node: water contents are measured
c  AT the node, therefore average AROUND node.  Conductivities apply
c  to the element as a whole, therefore average WITHIN element.
c   
 650  continue
      do670jz=jzsfc1,jzbm1
        jz1=jz-jzsfc
        if(pn(ihr,jz).lt.pe(jz1))then
          wnu(ihr,jz)=ws(jz1)*(pe(jz1)/pn(ihr,jz))**(1/bx(jz1))
        else
          wnu(ihr,jz)=ws(jz1)
        endif
        if(pn(ihr,jz+1).lt.pe(jz1))then
          wnl(ihr,jz)=ws(jz1)*(pe(jz1)/pn(ihr,jz+1))**(1/bx(jz1))
        else
          wnl(ihr,jz)=ws(jz1)
        endif
 670  continue
      do700jz=jzsfc2,jzbm1 
        jz1=jz-jzsfc
        wt(ihr,jz)=(wnl(ihr,jz-1)+wnu(ihr,jz))/2.
        if (wt(ihr,jz).gt.ws(jz1))wt(ihr,jz)=ws(jz1)
        cw(jz)=(cpl(jz-1)+cpu(jz))/2.
        ak=(akl(jz)+aku(jz))/2.
        akw(jz)=(akv(jz)+ak)/(z(jz+1)-z(jz))
        wgrav(jz)=gr*ak
 700  continue 
      wt(ihr,jzsfc1)=wnu(ihr,jzsfc1)
      cw(jzsfc1)=cpu(jzsfc1)
      ak=(akl(jzsfc1)+aku(jzsfc1))/2.
      akw(jzsfc1)=(akv(jzsfc1)+ak)/(z(jzsfc2)-z(jzsfc1))
      wgrav(jzsfc1)=gr*ak         
c
c  Determine whether relative humidity calculated from the atmosphere down ~= 
c  the surface relative humidity calculated from the soil up.  If not,
c  recycle through the temperature/water vapor/liquid water profile
c  computations once again (50 iter max) -> flag irecyc=1 sent to profl2.
c
      write (*,*) "determine relative humidity"
      wcpys=alam*4.18e-3*(evsoil+rhev*(rhsoil-rhsfc(2))) 
      iterw=iterw+1
      if(abs(rhsoil-rhsfc(2)).lt.drhsfc) then
        irecyc=0
      else if(iterw.ge.50) then
        irecyc=0
        write(6,5300)ihr,loopw
 5300   format(' ihr = ',i3,': ',i3,
     &           ' iterations on T-E-WT profile computations')         
      else
        irecyc=1
        go to 900
      endif         
c
c  Calculate infiltration and drainage in mm/hr
c
      write (*,*) "calculate infil and drainage"
      filt=source(jzsfc1)*dt
      drain=fl(jzbm1)*dt
      drain5=fl(jzbot-5)*dt 
      drgrav=wgrav(jzbm1)*dt 
      etmm=econd(2)*dt/(alam*4.18e3)
c
c  Verify that change in water storage matches boundary fluxes
c
      sw=0.
      swtemp=0.
      do800jz=jzsfc2,jzbm1
c        wstor(jz)=wt(ihr,jz)*v(jz)
        wtemp=wt(ihr,jz)*v(jz)
        swtemp=swtemp+wtemp
        wstor(jz)=wnl(ihr,jz-1)*(z(jz)-z(jz-1))*1000./2.+
     &              wnu(ihr,jz)*(z(jz+1)-z(jz))*1000./2.
        sw=sw+wstor(jz)
        wtlast(jz)=wt(ihr,jz)
 800  continue
      wtemp=wt(ihr,jzsfc1)*v(jzsfc1)
      swtemp=sw+wtemp
      wstor(jzsfc1)=wnu(ihr,jzsfc1)*(z(jzsfc2)*1000./2.)
      sw=sw+wstor(jzsfc1)
      wtlast(jzsfc1)=wt(ihr,jzsfc1)
      if(sw.ne.swtemp)then
        write (*,*) "Attention 1 ***************"
        write(6,*)'ATTENTION!!!!!!!!!!!'
        write(6,*)sw,swtemp
      endif
      if(abs((sw-swlast)-(drain+filt-cpytr)).ge.0.15
     &                                 .and.ideld.eq.0) then
        deld=5.e-6
        ideld=1
        go to 200
      else
        irecyc=0
        wtlast(jzsfc1)=wt(ihr,jzsfc1)    
      endif
 900  continue
      deld=deldi
c      write(6,*)'End of soilw:'
c      do1000jz=jzsfc1,jzbm1
c        write(6,*)jz,wt(ihr,jz)
c 1000 continue
      return
      end 
c     
c
c
      subroutine hydro(p,pe,ws,an,b,aks,w,ak,phi,dwdp)
c
c---------------------------------------------------------------------
c  Computes hydraulic properties of a soil layer - they depend on
c  whether the layer is saturated or not.
c---------------------------------------------------------------------
c
	  write (*,*) "hydro subroutine is called - cuinfil.f"
      if(p.lt.pe)then
        w=ws*(pe/p)**(1/b)
        dwdp=-w*(1/b)/p
        ak=aks*(pe/p)**an 
        phi=ak*p/(1-an)
      else
        w=ws
        dwdp=1e-5
        ak=aks
        phi=aks*(pe*an/(1-an)+p)
      endif
      return
      end       
