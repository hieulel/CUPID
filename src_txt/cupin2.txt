c $VERSION "08/16/95 @(#)cupin2.f	7.1"
*     cupin2                                                         *
*     program cupid inversion                                        *
*     with scale factors for xx(1) and xx(2)                         *
**********************************************************************
*
      common /misc/pi,rd,dg,pid2
      integer nfilerun
      common /input/ nfilerun
      dimension  tcpyap(9,50)
      real*8 rmin,ref(75),xi(30),rmsopt,rmsc(100),xcyc(100,30),ts(75)
      real*8 err,xx(30),xr(30),psn(75),tsun(75),tview(75),refl(75),
     1refs(75,2),vij(75,30),vv(30,30),dv(30),wk(30),cng,bfr,dd,cc,
     2dlim,cf,cx,ub(30),err2,dvv(30),sa,rms,del,drag(30),drag1(30),wgt
     3,yy1(30)
      real*8 pvw(75),xlb(30),date(75),crefl(100,75)
      character*6 name(30)
      character*22 filein1,filout1
      integer free,ifree(30),cycno,cycls,
     1retry,opcycn
      real*8 dif,difp,wg(75),dvp(30),prms,delta(30),gc,temp,rdv
      real tl,pl,snp,sna,vp,va,sunzn
      real*8 upb(30),xlowb(30)
      logical ier
      integer jtime(75),jcode(75),iquit
      common/cycle/cycls,cycno
*
**********************************************************************
*                                                                    *
*     xlb - lower bounds of parameters                               *
*     ub  - upper bounds of parameters                               *
*                                                                    *
*     use scaled upper bounds to reflect scaling of parameters       *
*                                                                    *
**********************************************************************
*
      data xlb/-.001,5.,-.001, -20.,0.02,-600.,-200.,23*-.001/
*
      data ub/50.,40.,10.,-0.01,0.4,1500.,1500.,23*1./
*
**********************************************************************
*                                                                    *
*     assorted factors                                               *
*                                                                    *
**********************************************************************
*
      data wg/75*1./
      data err,dlim,del/1.,1.5,.001/
      data wgt,drag/1.,30*.0/
*     data delta/5*.001,2*.01,23*.001/
c  delta is the step size for taking derivatives usually taken
c    as 1/1000 of magnitude of typical parameter value
      data delta/3.0,1.0,0.50,-0.5,0.005,1.0,1.0,23*.001/
      data name/'temair','vpair ','wind  ','psisum','wtsfc ',
     &'tnsfc ','ecpy  ','blnk  ','blnk  ','blnk  ',
     &'blnk  ','blnk  ','blnk  ','blnk  ','blnk  ',
     &'blnk  ','blnk  ','blnk  ','blnk  ','blnk  ',
     &'blnk  ','blnk  ','blnk  ','blnk  ','blnk  ',
     &'blnk  ','blnk  ','blnk  ','blnk  ','blnk  '/
c
c-------------- SET - ERROR HANDLER -------------------
c  invrgl=0 forward calculation   invrgl=1 inversion
c ***** following four statements are cancelled on 5/8/92 and should
c  recovered later.
        invrgl=0
	if (invrgl.eq.0) then
	    nfilerun = 1
	    igl = 0
	    ihrgl = 0
	    isrcgl = 0
	    isw1 = 0
	    icumdy = 0
	    do 4, i = 1, 30
		yy1(i)=0.0
4	    continue
	    do 2, i = 1, 9
		do 2, j = 1,50
		    tcpyap(i,j) = 0.0
2	    continue
	    do 3, i=1,75
		ts(i)= 0.0
		psn(i)=0.0
3	    continue
            call cupidg2(invrgl,igl,ihrgl,isrcgl,yy1,
     &			  tcpyap,ts,psn,icumdy,isw1)
c
	    goto 500
	endif
*
**********************************************************************

      open(unit=35,file='file.list',status='unknown')
      read(35,*) noruns1
      write(6,*)'**noruns= ',noruns1
*                                                                    *
*     heading                                                        *
      do 9900 nrun=1,noruns1
      nfilerun=nrun
      read(35,*)filein1,filout1
      write (6,*)'input name=',filein1,'output name=',filout1
c

      open(unit=2,file=filout1,status='unknown')
*                                                                    *
**********************************************************************
*
      write(2,1030)
 1030 format('cupid - 02/17/92',/)
*
**********************************************************************
*                                                                    *
*     initialization                                                 *
*                                                                    *
**********************************************************************
*
      iquit=0
      pi=4.*atan(1.)
      rd=pi/180.
      dg=1./rd
      pid2=pi/2.
*
*********************************************************************
*                                                                   *
*     read num paramaters                                     (#)   *
*          simulation of 'observed' refl,                     (0/1) *
*          calculation of real cr                             (0/1) *
*                                                                   *
*********************************************************************
*
      open(unit=5,file=filein1,status='old')
      read(5,22)nnp,isim,ireal
  22  format(/,10i5)
*
*     write(2,56)
  56  format(
     &' num. of param.  sim. of observ. tir  calc. real tir  ')
*     write(2,57)nnp,isim,ireal
 57   format(3(i3,10x))
*
*********************************************************************
*
*     read desired number of cycles for inversion                    *
*                                                                    *
**********************************************************************
*
      read(5,*)cycls
*
**********************************************************************
*                                                                    *
*     read, scale and store initial values of parameters             *
*                                                                    *
**********************************************************************
*
      read(5,1000) (xx(i),i=1,nnp)
 1000 format(7f11.4)
*
      xx(1)=xx(1)/10.
      xx(2)=xx(2)/10.
*
      do 14 i=1,nnp
       xi(i)=xx(i)
   14 continue
*
**********************************************************************
*                                                                    *
*     when generating tir  for graphs, cycls will be set to -1       *
*                                                                    *
**********************************************************************
*
      if (cycls.eq.-1)goto 826
*
**********************************************************************
*                                                                    *
*   read & print drag factors, num. and which parameters vary freely *
*   print scaled upper and lower bounds of parameters                *
*   print delta values used in sbrt deriv                            *
*   print real values of parameters (unscale first)                  *
**********************************************************************
*
      read (5,1000) (drag1(i),i=1,nnp)
      read (5,1002) free
 1002 format(i3)
      read (5,1003) (ifree(i),i=1,free)
 1003 format(25i3)
*
      write(2,1005)free
 1005 format(/,'there are ',i3,' free variables.')
      write(2,700 )(ifree(i),i=1,free)
 700  format('they are # ',25i3)
*
**********************************************************************
*                                                                    *
*     read, scale and store real    values of parameters             *
*                                                                    *
**********************************************************************
*
c 100 read (5,1000) (xx(i),i=1,nnp)
*
*     xr(1)=xx(1)/10.
*     xr(2)=xx(2)/10.
*
c     do 111 i=1,nnp
c     xr(i)=xx(i)
c111   continue
*
**********************************************************************
*                                                                    *
*     read data for measured (obsd) cr values                        *
*                                                                    *
**********************************************************************
*
      call obsct(nobs,jtime,jcode,date,psn,pvw,tsun,tview,ref)
      xlb(1)=ref(1)-15.
       ub(1)=ref(1)+15.
      write(2,723)
 723  format(/,'  variable    drag1     lower bnd.     upper',
     &' bnd.   delta  ')
c     do 725 i=1,nnp
c       write(2,724)name(i),drag1(i),xlb(i),ub(i),delta(i)
c724    format(a7,3x,f10.4,3x,f12.4,5x,2(f12.4,3x))
c725  continue
*
**********************************************************************
*                                                                    *
*     compute refl using real values of parameters                   *
*                                                                    *
**********************************************************************
*
*     wgt=0.
      isw1=1
      retry=1
*
**********************************************************************
*                                                                    *
*     if only real refl desired with no error analysis, free set to 0*
*     skip deriv and compute only refl                               *
*                                                                    *
*     if no calculation of real cr desired, ireal set to 0           *
*                                                                    *
**********************************************************************
*
      if (ireal.eq.0) goto 3965
 3966 if (free.eq.0) goto 207
*
      icycno=0
 208  call deriv(nobs,ifree,free,psn,pvw,tsun,tview,xr,refl,vij,
     &nnp,delta,icycno,isw1,jcode,jtime,iquit)
      goto 211
 207  continue
      call reflt(nobs,psn,pvw,tsun,tview,xr,refl,
     &nnp,icycno,isw1,jcode,jtime,iquit)
 211  continue
*
**********************************************************************
*                                                                    *
*     print and store real(calc.) and meas.(obsd.) refl              *
*                                                                    *
**********************************************************************
*
      do 59 k=1,nnp
      do 60 ik=1,free
      ii=ifree(ik)
      if(k.eq.ii) go to 59
 60   continue
      xi(k)=xr(k)
 59   continue
      write(2,1017)
 1017 format(/,'  obs. angles &  reflectances')
      write(2,1013)
 1013 format(2x,' i   szn   saz',7x,'vzn',5x,'vaz',4x
     1, 2x, 'obs. tir','  calc. tir','  code', 'hour')
*
 3965 continue
*
**********************************************************************
*                                                                    *
*     assign values                                                  *
*     obs.refl is = refs(i,1)                                        *
*     cal.refl is = refs(i,2)                                        *
*     for simulation, assign real refl to obsd. ones                 *
*                                                                    *
**********************************************************************
*
      do 1 i=1,nobs
         if (isim.eq.1) goto 237
 338     continue
         refs(i,1)=ref(i)
         goto 238
 237     refs(i,1)=refl(i)
 238     continue
*
         if (ireal.eq.0) goto 1
         refs(i,2)=refl(i)
*
      write(2,1012) i,tsun(i),psn(i),tview(i),pvw(i),refs(i,1),
     &refs(i,2),jcode(i),jtime(i)
 1012 format(1x,i3,4f8.1,2f10.4,2i6)
*
  1   continue
*
      if (ireal.eq.0) goto 3967
*
**********************************************************************
*                                                                    *
*     if only real refl desired with no error analysis, free set to 0*
*                                                                    *
**********************************************************************
*
      if (free.eq.0) goto 209
*
**********************************************************************
*                                                                    *
*     adjust weight factors wg(jj) *** default value is 1.0          *
*     for error analysis, use real reflectances                      *
*                                                                    *
**********************************************************************
*
*     do 811 jj=1,nobs
*     wg(jj)=1./(refs(jj,2)**2)
*811  continue
*
      do 80 i=1,nnp
      drag(i)=0.
 80   continue
*
 3968 call vmatr(nobs,free,ifree,vv,vij,drag,wg)
*
**********************************************************************
*                                                                    *
*     error analysis                                                 *
*                                                                    *
**********************************************************************
*
      call error(xr,vv,nobs,free,ifree,name,vij,refl,err,wg,nnp)
*
**********************************************************************
*                                                                    *
*     print rms and prms values for real vs obsd. refl               *
*                                                                    *
**********************************************************************
*
 209  continue
*
 987  call rmsr(rms,refl,refs,nobs,1)
      write(2,1001) rms
 1001 format(/,' rms  of real vs obs. reflectances = ',f14.6)
      call prmsr(prms,refl,refs,nobs,1)
      write(2,2001)prms
 2001 format(' prms of real vs obs. reflectances = ',f10.2,' %')
*
 3967 continue
*
**********************************************************************
*                                                                    *
*     if no inversion desired, quit now                              *
*                                                                    *
**********************************************************************
*
      if (cycls. eq. 0.) go to 805
*
**********************************************************************
*                                                                    *
*     adjust weight factors wg(jj)                                   *
*     for inversion, use measured reflectances                       *
*                                                                    *
**********************************************************************
*
*     do 81  jj=1,nobs
*     wg(jj)=1./(refs(jj,1)**2)
*81   continue
*
**********************************************************************
*                                                                    *
*     prepare for inversion                                          *
*     make initial (scaled) parameter values the current ones        *
*                                                                    *
**********************************************************************
*
      do 18 i=1,nnp
      xx(i)=xi(i)
 18   continue
*
**********************************************************************
*                                                                    *
*     unscale and print current parameter values                     *
*                                                                    *
**********************************************************************
*
      xx(1)=10.*xx(1)
      xx(2)=10.*xx(2)
*
      write(2,1015)
 1015 format('  initial values:')
*
      do 16 i=1,nnp
       write(2,1016) name(i),xx(i)
 1016 format(5x,a6,1x,f12.4)
   16 continue
*
*
**********************************************************************
*                                                                    *
*     rescale current parameters                                     *
*                                                                    *
**********************************************************************
*
      xx(1)=xx(1)/10.
      xx(2)=xx(2)/10.
      xlb(1)=xlb(1)/10.
      ub(1)=ub(1)/10.
      xlb(2)=xlb(2)/10.
*
**********************************************************************
*                                                                    *
*     begin inversion                                                *
*                                                                    *
**********************************************************************
*
      write(2,*)' before inversion do 5  cycls= ',cycls
      do 5 cycno=1,cycls
         icycno=cycno
	 write (6,*) '******icycno****', icycno
*
**********************************************************************
*                                                                    *
*     store current (scaled) parameter values                        *
*                                                                    *
**********************************************************************
*
         do 67 i=1,nnp
            xcyc(icycno,i)=xx(i)
  67     continue
*
**********************************************************************
*                                                                    *
*     on last cycle, skip deriv - compute only refl                  *
*                                                                    *
**********************************************************************
*
      if (icycno.eq.cycls) goto 200
*
**********************************************************************
*                                                                    *
*     compute and store refl                                         *
*                                                                    *
**********************************************************************
*
 201  call deriv(nobs,ifree,free,psn,pvw,tsun,tview,xx,refl,vij,
     &nnp,delta,icycno,isw1,jcode,jtime,iquit)
c     write(2,*)'xx,vij=',xx(1),(vij(iw1,1),iw1=1,5)
      goto 202
 200  continue
 847  continue
      call reflt(nobs,psn,pvw,tsun,tview,xx,refl,
     &nnp,icycno,isw1,jcode,jtime,iquit)
*
 202  continue
*
      do 932 jp=1,nobs
         crefl(icycno,jp)=refl(jp)
 932  continue
*
**********************************************************************
*                                                                    *
*     compute, store, & print rms of calc. vs obsd. refl             *
*                                                                    *
**********************************************************************
*
      call rmsr(rms,refl,refs,nobs,1)
      rmsc(icycno)=rms
      write(2,205)rms,icycno
 205  format(45x,'rms=',f12.6,' cycno=',i4,/)
*
**********************************************************************
*                                                                    *
*     if rms very small, quit inversion here                         *
*                                                                    *
**********************************************************************
*
       if (rms .lt. .00001) go to 92
*
**********************************************************************
*                                                                    *
*     on last cycle, quit inversion here                             *
*                                                                    *
**********************************************************************
*
       if (icycno.eq.cycls) goto 92
 204   continue
*
**********************************************************************
*                                                                    *
*      compute change in parameters for next cycle                   *
*                                                                    *
**********************************************************************
*
       do 93 i=1,nnp
        drag(i)=wgt*drag1(i)
   93  continue
*
       call vmatr(nobs,free,ifree,vv,vij,drag,wg)
       do 6 i=1,free
        sa=0.
        do 7 j=1,nobs
         sa=sa+(refs(j,1)-refl(j))*vij(j,i)*wg(j)
    7   continue
        dv(i)=sa
    6  continue
       call gauss(vv,dv,dvp,free,ier)
       if (ier) stop
c      write(2,831)(dvp(j),j=1,free)
 831   format(5e14.4)
       do 123 j=1,free
       dv(j)=dvp(j)
 123   continue
       cng=0.
       do 8 j=1,free
        cng=cng+dv(j)**2
    8  continue
       bfr=free
       cng=dsqrt(cng/bfr)
       dd=1.
       do 9 i=1,free
        cc=dd*dabs(dv(i))
        if (cc .gt. dlim) dd=dd*dlim/cc
    9  continue
       do 90 i=1,nnp
        dvv(i)=0.
   90  continue
       do 10 i=1,free
        j=ifree(i)
        dvv(j)=dd*dv(i)
   10  continue
       cf=1.
c      do 11 k=1,nnp
       do 11 kk=1,free
        k=ifree(kk)
c  this is where check is done on upper and lower bound/71
c
       write(2,*)' original ub(2)=',ub(2)
c  mult xx(1) times 10 (scaling factor) to get correct value
       ub(2)=6.108*10**(7.5*xx(1)*10./(237.3+xx(1)*10.))
       ub(2)=ub(2)/10.
	write(2,*)' xx(1),ub(2)= ',xx(1),ub(2)
	write(6,*)' xx(1),ub(2)= ',xx(1),ub(2)
c  divide xlb by 10 (scaling factor )for index 1 & 2 (temp and vp)
        if ((xx(k)+cf*dvv(k)).gt.xlb(k)) go to 12
        write(2,1007) k
 1007 format('  lower bndry violation for vble ',i3)
        cx=(-xlb(k)-xx(k))/(2.*dvv(k))
        cf=dmin1(cf,cx)
        go to 11
   12   if ((xx(k)+cf*dvv(k)).lt. ub(k)) go to 11
        write(2,1008) k
 1008 format('  upper bndry violation for vble ',i3)
        cx=(ub(k)-xx(k))/(2.*dvv(k))
        cf=dmin1(cf,cx)
   11  continue
   91  dd=dd*cf
       rmsc(cycno)=rms
       write(2,1006) cng,dd
 1006 format('  change=',d15.8,' reduction=',d15.8)
       cng=cng*dd
*
**********************************************************************
*                                                                    *
*     change values of parameters                                    *
*                                                                    *
**********************************************************************
*
       do 13 i=1,nnp
        xx(i)=xx(i)+cf*dvv(i)
   13  continue
*
**********************************************************************
*                                                                    *
*     unscale, print and rescale current values of parameters        *
*                                                                    *
**********************************************************************
*
      xx(1)=10.*xx(1)
      xx(2)=10.*xx(2)
*
       write(2,1099) (xx(i),i=1,nnp)
 1099 format('  xx= ',5f12.4)
*
      xx(1)=xx(1)/10.
      xx(2)=xx(2)/10.
*
**********************************************************************
*                                                                    *
*     naren's tricks of the trade                                    *
*                                                                    *
**********************************************************************
*
       if (rms .gt. .0005) go to 71
       wgt=0.
       retry=0.
 71    if (rms .lt. .00001) go to 92
       if (dd . lt. .0001 ) go to 70
       if (cng .lt. .001) wgt=wgt/5.
       if (cng .gt. 100.) wgt=wgt*5.
      go to 5
 70   if (retry.eq.0) go to 92
      do 72 i=1,nnp
      xx(i)=xi(i)
 72   continue
      wgt=5.*wgt+1.
    5 continue
*
**********************************************************************
*                                                                    *
*     end of inversion                                               *
*                                                                    *
**********************************************************************
*                                                                    *
**********************************************************************
*                                                                    *
*     select optimal cycle by minimum rms value (last one)           *
*                                                                    *
**********************************************************************
*
 92   opcycn=1
      rmsopt=rmsc(1)
      do 133 cycno=1,icycno
      if (rmsc(cycno).le.rmsopt) goto 134
      goto 133
 134   rmsopt=rmsc(cycno)
      opcycn=cycno
 133   continue
*
**********************************************************************
*                                                                    *
*     make optimal parameter values the current ones                 *
*
**********************************************************************
*
      do 19 i=1,nnp
 19   xx(i)=xcyc(opcycn,i)
*
**********************************************************************
*                                                                    *
*     make optimal refl values the current refl                      *
*                                                                    *
**********************************************************************
*
      do 333 iobs=1,nobs
         refl(iobs)=crefl(opcycn,iobs)
 333  continue
*
**********************************************************************
*                                                                    *
*     unscale optimal and real parameter values                      *
*                                                                    *
**********************************************************************
*
      xx(1)=10.*xx(1)
      xx(2)=10.*xx(2)
*
      xr(1)=10.*xr(1)
      xr(2)=10.*xr(2)
*
**********************************************************************
*                                                                    *
*     print optimal vs real parameter values                         *
*                                                                    *
**********************************************************************
*
      write(2,1009)
 1009 format('  vble., calculated value, real value')
       write(2,5999)opcycn
 5999  format(' opcycn  =',i3)
      do 15 i=1,nnp
       write(2,1010) name(i),xx(i),xr(i)
 1010 format(5x,a6,1x,f15.4,1x,f15.4)
   15 continue
*
*
      call rmsr(rms,refl,refs,nobs,2)
      write(2,1011) rms
 1011 format('  rms of computed vs. real reflectances=',f12.6)
*
      rms=rmsopt
      write(2,1018) rms
 1018 format('  rms of computed vs. obsd. reflectances=',f12.6)
*
*     rewind 2
*
**********************************************************************
*                                                                    *
*     distribution of optimal (calc.) vs obsd. refl and errors       *
*                                                                    *
**********************************************************************
*
      write(2,1100)
 1100 format(1x,'obs  szen   saz ','    vzn ','  vaz ',
     &'      obs.tir',
     1' calc.tir',' obs-calc','  % error')
*
      do 95 k=1,nobs
      dif=refs(k,1)-refl(k)
      difp=dif*100./refs(k,1)
      write(2,1101)k,tsun(k),psn(k),tview(k),pvw(k),refs(k,1),
     1refl(k),dif,difp
 1101 format(i3,4f7.1,4f10.4)
 95    continue
*
      call prmsr(prms,refl,refs,nobs,1)
      write(2,96)prms
 96   format(/,'percentage rms of computed vs. obs. = ',f10.2,' %')
*
*804   go to 999
 805   continue
      goto 829
*
**********************************************************************
*                                                                    *
*     call to generate refl for graphs                               *
*                                                                    *
**********************************************************************
*
 826  call genref(nnp,xr,name,iquit)
 829  continue
      iquit=1
      call reflt(nobs,psn,pvw,tsun,tview,xr,refl,
     &nnp,icycno,isw1,jcode,jtime,iquit)
 9900 continue
      close (unit=35)
 500  stop
      end
      subroutine reflt(nobs,psn,pvw,ts,tv,xx,refl,
     &nnp,icycno,isw1,jcode,jtime,iquit)
c     parameter (mh=98)
c     parameter (nobs=5)
      dimension zenang(98),tcpyir(9,50),tcpyap(9,50)
      real*8 psn(75),pvw(75),ts(75),tv(75),xx1(30),refl(75),yy1(30)
     &,xx2(30),yy2(30),xx(30)
      integer jcode(75),nnp,icycno,isw1,nobs,ihrgl,invrgl,igl,isrcgl
      integer iquit,jtime(75)
      integer cycls,cycno
      common/cycle/cycls,cycno
c      dimension (0:100, -50:50) :: tairgl,eairgl,windgl,psisgl,hsoil,ecpy,hcpy
      common/deg/sunazm(98),nozenv,viewzn(10),noazmv,viewaz(50)
c     write(6,*)'iquit=',iquit
      if(iquit.eq.1)goto 2000
      if(isw1.ge.2)go to 102
      invrgl=1
      if(invrgl.ne.0)goto 101
      call cupidg2(invrgl,igl,ihrgl,isrcgl,yy1,
     &tcpyap,ts,psn,icumdy,isw1)
      go to 500
 101  continue
      ihrgl=jtime(1)
      igl=0
      isrcgl=0
      write(2,1001)ihrgl
 1001 format(' hour=', i10)
      call cupidg2(invrgl,igl,ihrgl,isrcgl,yy1,
     &tcpyap,ts,psn,icumdy,isw1)
c     write(2,*)'tair,eair,wind,psis=',yy1(1),yy1(2),yy1(3),yy1(4)
      xx(1)=yy1(1)/10.
      xx(2)=yy1(2)/10.
      xx(3)=yy1(3)
c     yy1(4)=-1500.
      xx(4)=yy1(4)
      xx(5)=yy1(5)
      xx(6)=yy1(6)
      xx(7)=yy1(7)
c     write(2,*)'hsoil,hcpy,ecpy='
c     write(2,1002)xx(5),xx(6),xx(7)
 1002 format(1x,4f10.2)
      k=1
c     do 17 i=1,nozenv
c     do 177 j=1,noazmv
c     ts(k)=zenang(ihrgl)
c     psn(k)=sunazm(ihrgl)
c     jcode(k)=icumdy
c     tv(k)=viewzn(i)
c     pvw(k)=viewaz(j)
c     k=k+1
c177  continue
c17   continue
c     nobs=k-1
c60   continue
c     tv(1)=viewzn(4)
c     tv(2)=viewzn(3)
c     tv(3)=viewzn(2)
c     tv(4)=viewzn(1)
c     tv(5)=viewzn(2)
c     tv(6)=viewzn(3)
c     tv(7)=viewzn(4)
c     pvw(1)=viewaz(1)
c     pvw(2)=viewaz(1)
c     pvw(3)=viewaz(1)
c     pvw(4)=viewaz(1)
c     pvw(5)=viewaz(2)
c     pvw(6)=viewaz(2)
c     pvw(7)=viewaz(2)

      igl=1
      call cupidg2(invrgl,igl,ihrgl,isrcgl,yy1,
     &tcpyap,ts,psn,icumdy,isw1)
c     k=1
c     do 199 i=1,nozenv
c     do 199 j=1,noazmv
c     refl(k)=tcpyir(i,j)
c     k=k+1
 199  continue
c     refl(1)=tcpyir(4,1)
c     refl(2)=tcpyir(3,1)
c     refl(3)=tcpyir(2,1)
c     refl(4)=tcpyir(1,1)
c     refl(5)=tcpyir(2,2)
c     refl(6)=tcpyir(3,2)
c     refl(7)=tcpyir(4,2)
      refl(1)=tcpyap(4,1)
      refl(2)=tcpyap(3,1)
      refl(3)=tcpyap(2,1)
      refl(4)=tcpyap(1,1)
      refl(5)=tcpyap(2,2)
      refl(6)=tcpyap(3,2)
      refl(7)=tcpyap(4,2)
      go to 100
 102  continue
      isrcgl=1
      yy2(1)=xx(1)*10.
      yy2(2)=xx(2)*10.
      yy2(3)=xx(3)
      yy2(4)=xx(4)
      yy2(5)=xx(5)
      yy2(6)=xx(6)
      yy2(7)=xx(7)
      call cupidg2(invrgl,igl,ihrgl,isrcgl,yy2,
     &tcpyap,ts,psn,icumdy,isw1)
c     write(2,*)'hsoil,hcpy,ecpy='
c     write(2,1002)xx(5),xx(6),xx(7)
c     k=1
c     do 1999 i=1,nozenv
c     do 1999 j=1,noazmv
c     refl(k)=tcpyir(i,j)
c     k=k+1
 1999 continue
c     refl(1)=tcpyir(4,1)
c     refl(2)=tcpyir(3,1)
c     refl(3)=tcpyir(2,1)
c     refl(4)=tcpyir(1,1)
c     refl(5)=tcpyir(2,2)
c     refl(6)=tcpyir(3,2)
c     refl(7)=tcpyir(4,2)
      refl(1)=tcpyap(4,1)
      refl(2)=tcpyap(3,1)
      refl(3)=tcpyap(2,1)
      refl(4)=tcpyap(1,1)
      refl(5)=tcpyap(2,2)
      refl(6)=tcpyap(3,2)
      refl(7)=tcpyap(4,2)
 2000 continue
      if(iquit.eq.0)goto 100
      igl=2
      isrcgl=0
c     tairgl=yy1(1)
c     eairgl=yy1(2)
c     windgl=yy1(3)
c     psisgl=yy1(4)
      call cupidg2(invrgl,igl,ihrgl,isrcgl,yy1,
     &tcpyap,ts,psn,icumdy,isw1)
 100  continue
      isw1=isw1+1
 500  continue
      return
      end
