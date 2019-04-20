c $VERSION "@(#)curadin.f	7.1 08/16/95"
      subroutine radin4(averad,ihrstr,ihrend,nohrs,ipot)
c************************visnir*****************************************
c  averad is input as total cal/cm sqd/day
      parameter(mh=98)
      dimension averad(mh)
c   compute potential flux for day,pcpt water=1 cm,alpha=1
      common /rad3/radabv(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh),hfday
     &,ratiod,ration,ratio(mh)
      do10i=1,nohrs
      do10k=1,2
      radabv(k,i)=0.
 10   fbeam1(k,i)=0.
c     write(6,*)ihrstr,ihrend,coszen
      pid2=3.1415926537/2.
      pid180=3.1415926737/180.
c  convert ly/hr to w/sq m.
      do1000i=1,nohrs
c     averad(i)=averad(i)*697.
      if(coszen(i).lt.0.01 )go to 900
c  correct for curvature of atmos in airmas
 50   airmas=(sqrt(coszen(i)**2+.0025)-coszen(i))/.00125
c  correct for refraction(good to 89.5 deg.)
      airmas=airmas-2.8/(90.-zenang(i)/pid180)**2
      potbm1=600.*exp(-.160*airmas)
      potvis=(potbm1+(600.-potbm1)*.4)*coszen(i)
      potdif=(600.-potbm1)*.4*coszen(i)
      u=1/coszen(i)
      axlog=alog10(u)
      a=10**(-1.195+.4459*axlog-.0345*axlog*axlog)
      watabs=1320.*a
      potbm2=720.*exp(-.05*airmas)-watabs
      if(potbm2.lt.0.)potbm2=0.
 60   eval=(720.-potbm2-watabs)*.54*coszen(i)
 90   potnir=eval+potbm2*coszen(i)
      if(ipot-1)100,200,200
c
c
c
c
c  use ration read in for night time value of ratio for thermal rad
c
c
c
 100  if(averad(i).lt.0.01)go to 900
      ratio(i)=averad(i)/(potvis+potnir)
      radabv(1,i)=averad(i)*potvis/(potvis+potnir)
      radabv(2,i)=averad(i)*potnir/(potvis+potnir)
      if(ratio(i)-1.)150,150,110
 110  ratio(i)=1.
 150  continue
      go to 300
 200  ratio(i)=ratiod
      radabv(1,i)=potvis*ratio(i)
      radabv(2,i)=potnir*ratio(i)
 300  fb1=potbm1*coszen(i)/potvis
      fb2=potbm2*coszen(i)/potnir
      ratiox=ratio(i)
      if(ratio(i).gt..9)ratiox=.9
      fbeam1(1,i)=fb1*(1.-((.9-ratiox)/.7)**.6667)
      if(ratio(i).gt.0.88)ratiox=.88
      fbeam1(2,i)=fb2*(1.-((.88-ratiox)/.68)**.6667)
      if(fbeam1(1,i).lt.0.)fbeam1(1,i)=0.
      if(fbeam1(2,i).lt.0.)fbeam1(2,i)=0.
      if(fbeam1(1,i).gt.fb1)fbeam1(1,i)=fb1
      if(fbeam1(2,i).gt.fb2)fbeam1(2,i)=fb2
c  check that fbeam1(1,ihr) cannot be greater than .01 when
c    fbeam1(2,ihr) is less than .01 or vice versa.
      if(fbeam1(1,i).lt..01.and.fbeam1(2,i).gt..01)fbeam1(1,i)=.011
      if(fbeam1(2,i).lt..01.and.fbeam1(1,i).gt..01)fbeam1(2,i)=.011
      go to 1000
 900  ratio(i)=ration
 1000 continue
      return
      end
      subroutine skyir(nohrs)
      parameter(mh=98)
c*************************skyir*****************************************
      common/met1/temair(mh),vpair(mh),precip(mh),temsol(mh),watsol(mh)
      common/rad3/radtop(3,mh),fbeam1(3,mh),coszen(mh),zenang(mh)
     1,hfday,ratiod,ration,ratio(mh)
c  calc thermal rad from sky with brutsaert eq.
c  use ratio to get weighted average of clear sky and 'clouds'.
      do 100 i=1,nohrs
      temp=temair(i)+273.
      sigma=5.67e-08
      esky=1.24*(vpair(i)/temp)**(1./7.)
      radtop(3,i)=sigma*(temp**4)*(esky*ratio(i)+1.-ratio(i))
 100  continue
      return
      end
