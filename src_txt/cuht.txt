c $VERSION "08/16/95 @(#)cuht.f	7.1"
      subroutine wndfac(amfull,zdhcr)
      parameter(mh=98)
      common/wind1/fwind(20),wind(mh),sizelf,dmax,refht,z0,disp,am,zcrit
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
c  calc factors to mult wind above canopy by to get wind in
c    canopy. uses neutral profile to save time;good near sfc.
c
c--------------
c z0 and disp now entered in input file. 94/11/22
c  calc z0,disp and m adjusting for height to row spacing factor.
c     hdw=(h/rowspc)**2
c     if(hdw.gt.10)hdw=10.
c     z0=0.13*h*(1.-exp(-hdw))
c  roughness for Verma Konza site of prairie grass
c     z0=0.03*h*(1.-exp(-hdw))
c  amfull is wind profile coeff(used in profl2) for full cover
c     am=amfull*(1.-exp(-3.*hdw))
c     if(z0-z0soil)100,100,50
c50   disp=.63*h*(1.-exp(-2.*hdw))
c  displacement height for Verma Konza site
c     disp=.71*h*(1.-exp(-2.*hdw))
c     go to 120
c100  z0=z0soil
c     disp=0.
c--------------
 120  fwindh=alog((h-disp)/z0)/alog((refht-disp)/z0)
c  use soil roughness if height in canopy less than 20cm or .2m
      jflag=0
      do200j=2,jtot
      jj=jtot+2-j
      if(jflag.ne.0)go to 155
      if(zdh(jj)-zdhcr)150,150,160
 150  jflag=1
      jcrit=jj+1
 155  fwind(jj)=fwind(jcrit)*(alog(zdh(jj)*h/z0soil)/
     1alog(zdh(jcrit)*h/z0soil))
      go to 200
 160  fwind(jj)=fwindh/(1.+am*(1.-zdh(jj)))**2
 200  continue
c  compute size of leaf for corn in meters.
      sizelf=totlai*dmax/(totlai+13.*dmax)
c  for canopies other than corn set dmax=sizelf
      sizelf=dmax
      return
      end
c----------------------------------------------------------------------
c							HEIGHT
c
      subroutine height(iunif)
      parameter(mh=98)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
c  zdh is z/h or dimensionless height
c  compute height for any lai given canopy height h.
      tlaid2=totlai/2.
      cumlai=totlai
      zdh(1)=0.
      do100 j=2,jtot
      if (iunif.eq.0) then
          if(cumlai-tlaid2)50,50,75
 50       zdh(j)=1.-sqrt((cumlai/totlai)*(1.-zmdh)*(1.-zldh))
          go to 90
 75       zdh(j)=zldh+sqrt((1.-zldh)*(zmdh-zldh)*(1.-cumlai/totlai))
 90       cumlai=cumlai-df
      else
	  zdh(j)=zldh+(j-2)*(1-zldh)/(jtot-1)
      endif
 100  continue
      return
      end
c----------------------------------------------------------------------
c							RBOUND
c
      subroutine rbound(ihr,wnd,frdead)
      parameter(mh=98)
      dimension wnd(20)
      common/wind1/fwind(20),wind(mh),sizelf,dmax,htref,z0,disp,am,zcrit
      common/resis1/rhleaf(20),rsleaf(10,20),rsnovp(10,20),hpsi
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
c  calc leaf boundry layer resis versus height in canopy
c  forced convection only. free approx. by rh=500 sec/m
c
c Assume dead leaves are of size sizelf/2 - these are all found in
c layers up to layer jdead.  jdead+1 has weighted avg leaf size.
c This size adjustment added by MCA 4/25/95  
      do100j=2,jtot
        wnd(j)=fwind(j)*wind(ihr)
        if(wnd(j)-.01)50,50,75
 50     rhleaf(j)=500.
        go to 100  
 75     if (j.le.jdead) then
          size = sizelf/2.
        else if (j.eq.jdead+1) then
          size = frdead*sizelf/2. + (1-frdead)*sizelf
        else
          size = sizelf
        endif
        rhleaf(j)=180.*sqrt(size/wnd(j))
 100  continue
      return
      end
c      
c----------------------------------------------------------------------
c                            				HITE2
c
      subroutine hite2(iday)
      parameter(mh=98)
      common/misc2/itot,itotp1,jtot,fr(10),ct(10),totlai,df,dt
     &,clai(20),distls(10,mh),jdead
c distls(10,mh) in /misc2/ was added by Chen, 9/4/89.
      common/misc3/h,cover,zdh(20),rowspc,z0soil,zldh,zmdh
      common/misc4/z(50),zmid(50),jzcpy,jzcpy1,jzsfc,jzsfm1,jzbot,jzcrit
     &,jzbm1
      common/misc5/deltz(50),zbcpy(10),zbc(10),zabc(20)
      common/soil1/zsoil(25),wti(50),ndsoil,aksoil(50),cpsoil(50),tsn(50
     1)
      common/prof1/tair(20),eair(20),phim,phih,refhtt,refhte,relh(20)
     &,nlabcy,nlbcpy
c  put save in here for vbls nlabc and nlbc
      save
c
c
c  calc height array to go from lower soil b.c. to upper b.c. above
c    canopy. z(jz) is height in meters and jz subscript-1=refht above
c    cpy and jzbot=refdp in soil. jz increases downward and z(jz) is
c    neg. above soil sfc and pos. below sfc. all heights are for
c    top of layers as in campbell handout.
c  the no. of layers abv. the canopy is nlabcy ,so jz=(nlabcy+1)
c  is the precise index for the canopy top.
c  layers in cpy--loop within cpy goes from (nlabcy+1)to jzcpy.
c  (nlabcy+1) correspond to jtot in lai section of program .jzcpyis
c  lowest layer of leaves in canopy.z(jzcpy) is height of top of lowest
c  layer.
c
c
c ************* layers above canopy are readjusted in this block*******
c input data for layer heights are stored in variable zabc in first pass
       write (*,*) "hite2 subroutine is called - cuht.f"
       if(iday.eq.1)then
       nlabc=nlabcy
       do140i=1,nlabcy
  140  zabc(i)=z(i)
       else
       nlabcy=nlabc
       do145i=1,nlabcy
  145  z(i)=zabc(i)
       endif
 112  if(z(nlabcy).ge.-h) then
      nlabcy=nlabcy-1
      if(nlabcy.eq.1)then
      write(*,*)'warning: layers above canopy are reduced to one.'
      endif
      go to 112
      endif
      jzcpy=jtot-1+nlabcy
      z(nlabcy+1)=-h
c
c **************layers within canopy are set in this block ************

      do200jz=nlabcy+2,jzcpy
      j=jtot+(nlabcy+2)-jz
 200  z(jz)=-zdh(j)*h
      jzcpy1=jzcpy+1
      z(jzcpy+1)=-zdh(2)*h
c
c ************** layers below canopy are readjusted in this block*******
c  nlbcpy is the no. of layers between lowest cpy layer and the soil sfc
c  it is necessary to store original height data here, since number of
c  layers may increase during a simulation:
      if(iday.eq.1)then
      nlbc=nlbcpy
      do160i=1,nlbcpy
 160  zbc(i)=zbcpy(i)
      else
      nlbcpy=nlbc
      do170i=1,nlbcpy
 170  zbcpy(i)=zbc(i)
      endif
      jzcpy2=jzcpy+2
  250 if(z(jzcpy1).ge.-zbcpy(1))then
      do 350 i=1,nlbcpy
  350 zbcpy(i)=zbcpy(i+1)
      nlbcpy=nlbcpy-1
      if(nlbcpy.eq.0)then
      write(*,35)'all layer heights on iday ',iday,' for below the',
     +' canopy are too high.',' program inserts one layer.'
   35 format(1x,a,i2,a,a,/,a)
      z(jzcpy1+1)=z(jzcpy1)/2.
      nlbcpy=1
      goto360
      endif
      go to 250
      endif
      do300i=1,nlbcpy
 300  z(jzcpy1+i)=-zbcpy(i)
c *******************************************************************
c  soil layers, ndsoil and zsoil read in under fixed data.
 360  jzsfc=jzcpy1+nlbcpy
      jzbot=jzsfc+ndsoil
      jzsfc1=jzsfc+1
      do400jz=jzsfc1,jzbot
      jz1=jz-jzsfc
 400  z(jz)=zsoil(jz1)
c
c ************** final check for layer structure *********************
      ierrl=0
      do450jz=1,jzbot-1
      if(z(jz).ge.z(jz+1))then
      ierrl=1
      write(*,44)'layer miscalculated in hite2 for iday=',iday,
     +'check layers jz=',jz,' (',z(jz),'m) and jz=',jz+1,' (',
     + z(jz+1),'m) .'
  44   format(1x,a,1x,i2,/,1x,a,i2,a,f5.2,a,i2,a,f5.2,a)
       endif
  450  continue
       if(ierrl.eq.1)stop
c
c *******************************************************************
c  calc midpoints of all layers. subscript is same for middle of
c    layers as for top.
      jzsfm1=jzsfc-1
      jzbm1=jzbot-1
      do500jz=1,jzbm1
 500  zmid(jz)=(z(jz)+z(jz+1))*.5
      zmid(jzbot)=z(jzbot)
c  note- this defines deltz(jz) as the dif of midpoints between layers
c        and is  used for heat or water storage terms in new-raph.
      do600jz=2,jzbm1
 600  deltz(jz)=(z(jz+1)-z(jz-1))/2.
      deltz(1)=z(2)-z(1)
      deltz(jzbot)=z(jzbot)-z(jzbm1)
      deltz(jzsfc1)=(z(jzsfc+2)-z(jzsfc+1))/2.0
 999  return
      end
