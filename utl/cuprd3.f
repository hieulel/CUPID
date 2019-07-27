c  program to read output from cupid and set it up for hp graphics.
c    designed for cupid4 but may work on earlier versions.
      dimension id1(16,100),var(20),n(20),iday(100),ihr(24),ilay(20)
     1,iang(10),mcode(10),ncode(20),icolm(10),data(9),igood(20)
     2,icount(20),data1(20,1000),idin(20,3),icolm1(20),sum1(20)
     2,id(20,2),nvlay(10),nlay(10),nvang(10),nang(10)
     3,delta(20),datax(9),data1x(20,1000),idsav1(20,1000)
     4,idsav2(20,1000)
      integer*4 title(20)
      integer*2 name(20,5),lab(20,3),label(20,3)
      character*22 filein,filout,compin,comout
      character*78 title1,title2, title3,title4
c
c get input file name and output file name
      Write(6,*)' input file name'
      read(5,30)filein
 30   format(a22)
      write(6,*)' output file name'
      read(5,30)filout
c
      open(unit=24,file=filein,status='unknown')
      open(unit=25,file=filout,status='unknown')
c
c  read title line on input file
      read(24,40)title1
      read(24,40)title2
 40   format(a78)
      write(6,*)' title line of file'
      write(6,40)title1
      write(6,40)title2
      write(6,*)' input file name ........',filein
      write(6,*)' output file name .......',filout
c
c  ask user if he wants all labels from cupid output
c
 99   write(6,100)
 100  format(' print cupid labels?(0,1  stop=-1)')
      read(5,*)ians
      if(ians.eq.0)go to 200
      if(ians.eq.-1)stop
c  read labels for fixed input and forst day only.
      write(6,105)
 105  format(16x,'   1      2      3      4      5      6      7   ',
     1'   8      9   ')
      nlab=1
 108  read(24,110,end=190)(id1(k,nlab),k=1,14)
 110  format(1x,14i1)
      if(id1(1,nlab).ne.1)go to 108
      read(24,115)((lab(m,i),i=1,3),m=1,9)
 115  format(14x,9(1x,3a2))
      if(nlab.eq.1)go to 132
      nlabm1=nlab-1
      do122ilab=1,nlabm1
c  do check on first 4 colms cause days & hrs different
      do119k=1,4
      if(id1(k,nlab).eq.id1(k,ilab))go to 119
      go to 122
 119  continue
      go to 108
 122  continue
      if(id1(4,nlab).eq.1)write(6,130)
 130  format(' ')
 132  write(6,135)(id1(k,nlab),k=1,15),((lab(m,i),i=1,3),m=1,9)
 135  format(1x,15i1,9(1x,3a2))
      nlab=nlab+1
      go to 108
 190  continue
      rewind 24
      read(24,40)title1
      read(24,40)title2
 200  continue
c  ***************************************************file compare
 204  write(6,205)
 205  format(' compare file 24 to 23 (1=yes,0=no)')
      read(5,*)icomp
      if(icomp.lt.0.or.icomp.gt.1)goto 204
      if(icomp.eq.1)then
      write(6,*)' input file name for compare'
      read(5,30)compin
      write(6,*)' output file name for compare'
      read(5,30)comout
      open(unit=23,file=compin,status='unknown')
      open(unit=26,file=comout,status='unknown')
      read(23,40)title3
      read(23,40)title4
      write(6,*)' title on compare input file'
      write(6,40)title3
      write(6,40)title4
      write(6,*)' input compare file name ......',compin
      write(6,*)' output compare file name .....',comout
      endif
c
c  initialize contribution of hours,layers or angles to no. of vbls.
      nvhr=1
      do220i=1,10
      nvlay(i)=0
      nvang(i)=0
 220  continue
c  ino is integer to identify indep. vbl- 1=hour,2=layer,3=angle
      ino=0
c  no is no. of obs to be sent to hp.
      no=0
c  no. of days in cupid output that data is desired for
 305  write(6,300)
 300  format(' input nday,ndayin,day nos')
      ihrday=1
c  if iday(1) le 0 than all days and all hours are desired
c    thus if all days and hours wanted with time as dep vbl nday=1,
c    iday(1)=-1, and nhrs=-1
      read(5,*)nday,ndayin,(iday(k),k=1,ndayin)
      if(iday(1).le.0)ihrday=0
      if(iday(1).le.0)go to 390
c  read in no of days to be searched for and no of day numbers read
c    in (ndayin). if not eq first day no read in is start day and
c    nday no of consecutive days used.
      if(nday.eq.ndayin)go to 350
      do310k=1,nday
 310  iday(k)=iday(1)+k-1
 350  continue
c
c
c
c  preset searches
c
c
c
c
      write(6,330)
 330  format(' preset search- 0=no,1=bidir')
      read(5,*)iset
c  hourly vapor pressure and humidity above canopy
	if(iset.eq.5) goto 355  
c  hourly cpy energy bal, photosynthesis and canopy resistances
	if(iset.eq.6) goto 360
c  hourly cpy energy bal, photosynthesis and canopy resistances
	if(iset.eq.7) goto 370
c  hourly photosynthesis scaling 
	if(iset.eq.8) goto 445
c  hourly transpiration scaling
	if(iset.eq.9) goto 450
c  daily photosynthesis scaling
	if(iset.eq.10) goto 455
c  daily transpiration scaling
	if(iset.eq.11) goto 465
c
      if(iset.lt.0.or.iset.gt.11)go to 305
      if(iset.eq.0)go to 390
c     if(iset.ne.1)go to 390
c
c  bidir preset search                                 iset=1  bidir
c
      ino=2
      write(6,311)
 311  format(' input nvbl & no')
      read(5,*)nvin,noin
      nvbl=nvin
      write(6,315)
 315  format(' ncode=6,7,8,or 9?')
      read(5,*)ncodex
      if(ncodex.lt.6.or.ncodex.gt.9)go to 305
      do320i=1,nvbl
      ncode(i)=ncodex
      mcode(i)=7
      icolm(i)=i
 320  continue
      nhrs=1
      write(6,322)
 322  format(' hr no= ?')
      read(5,*)ihr(1)
      nlay(1)=noin
      do325i=1,nvbl
 325  nang(i)=0
c this line added by larry to get past the High Wind run code.
	goto 390
c
c                                         end bidir preset search
c
c  hourly vapor pressure and humidity above canopy
c
355	nvbl = 5
c  timloc
	mcode(1) = 4
	ncode(1) = 1
	icolm(1) = 1
c  vpair
	mcode(2) = 4
	ncode(2) = 2
	icolm(2) = 2
c  wind
	mcode(3) = 4
	ncode(3) = 2
	icolm(3) = 3
c  vpdin
	mcode(4) = 4
	ncode(4) = 3
	icolm(4) = 1
c  relhum
	mcode(5) = 4
	ncode(5) = 3
	icolm(5) = 2
	ino = 1
        go to 385
c
c  hourly cpy energy bal, photosynthesis and canopy resistances
c
360	nvbl = 7
c  hsoil
	mcode(1) = 7
	ncode(1) = 1
	icolm(1) = 7
c  ecpys
	mcode(2) = 7
	ncode(2) = 2
	icolm(2) = 8
c  rncpy
	mcode(3) = 7
	ncode(3) = 4
	icolm(3) = 1
c  hcpy
	mcode(4) = 7
	ncode(4) = 4
	icolm(4) = 2
c  ecpy
	mcode(5) = 7
	ncode(5) = 4
	icolm(5) = 3
c  pscpyg
	mcode(6) = 7
	ncode(6) = 16
	icolm(6) = 3
c  cumhr
 	mcode(7) = 4
 	ncode(7) = 2 
 	icolm(7) = 9
c  rscpy
c	mcode(7) = 7
c	ncode(7) = 13
c	icolm(7) = 6
c  rhcpy
c	mcode(8) = 7
c	ncode(8) = 13
c	icolm(8) = 8
c
	ino = 1
c
	go to 385
c
c  profiles for hr=13, 23 layers 
c
370	nvbl = 8
c height 
	mcode(1) = 5
	ncode(1) = 4
	icolm(1) = 1
c  vpd
	mcode(2) = 5
	ncode(2) = 4
	icolm(2) = 5
c  tair
	mcode(3) = 5
	ncode(3) = 5
	icolm(3) = 6
c  eair
	mcode(4) = 5
	ncode(4) = 5
	icolm(4) = 8
c  rh
	mcode(5) = 5
	ncode(5) = 5
	icolm(5) = 9
c  zmid
	mcode(6) = 5
	ncode(6) = 6
	icolm(6) = 1
c  rsave
	mcode(7) = 5
	ncode(7) = 6
	icolm(7) = 6
c  rhleaf
	mcode(8) = 5
	ncode(8) = 6
	icolm(8) = 7
c
	ino = 2
c Corn = 23 Big Blue = ?	
c	
380	nhrs = 1
 	ihr(1) = 13	
	do 382 i=1,nvbl
	nlay(i) = 23
382	nang(i) = 0
	goto 390
c
c  hourly photosynthesis scaling 
c
 445   nvbl =10
c cumhr  
	mcode(1) = 4
	ncode(1) = 1
	icolm(1) = 1
c  pari
	mcode(2) = 7
	ncode(2) = 16
	icolm(2) = 1
c  pscpyg
	mcode(3) = 7
	ncode(3) = 16
	icolm(3) = 3
c  rdcpy
	mcode(4) = 7
	ncode(4) = 18
	icolm(4) = 1
c  pss1a
	mcode(5) = 7
	ncode(5) = 23
	icolm(5) = 1
c  pss1b
	mcode(6) = 7
	ncode(6) = 23
	icolm(6) = 2
c  pss2a
	mcode(7) = 7
	ncode(7) = 23
	icolm(7) = 3
c  pss2b 
        mcode(8) = 7
        ncode(8) = 23
        icolm(8) = 4
c  pss3a
        mcode(9) = 7
        ncode(9) = 24
        icolm(9) = 1
c  pss4  
        mcode(10) = 7
        ncode(10) = 24
        icolm(10) = 3
	ino = 1
	go to 385
c
 450   continue
c
c  hourly canopy transpiration scaling
c
       nvbl =10
c cumhr  
	mcode(1) = 4
	ncode(1) = 1
	icolm(1) = 1
c  rncpy
	mcode(2) = 7
	ncode(2) = 4 
	icolm(2) = 1
c  hcpy  
	mcode(3) = 7
	ncode(3) = 4 
	icolm(3) = 2
c  ecpy
	mcode(4) = 7
	ncode(4) = 4  
	icolm(4) = 3
c  ecpydv
	mcode(5) = 7
	ncode(5) = 20 
	icolm(5) = 6
c  rnsoil
	mcode(6) = 7
	ncode(6) = 2 
	icolm(6) = 9
c  etpm 
	mcode(7) = 7
	ncode(7) = 15
	icolm(7) = 3
c  evlf  
        mcode(8) = 7
        ncode(8) = 23
        icolm(8) = 5
c  qlf  
        mcode(9) = 7
        ncode(9) = 23
        icolm(9) = 6
c  rnlf  
        mcode(10) = 7
        ncode(10) = 23
        icolm(10) = 7
	ino = 1
	go to 385
c
 455   continue
c
c  daily  photosynthesis scaling 
c
       nvbl =10
c cumhr  
	mcode(1) = 4
	ncode(1) = 2
	icolm(1) = 9
c  apard
	mcode(2) = 2
	ncode(2) = 13
	icolm(2) = 3
c  rdday 
	mcode(3) = 2
	ncode(3) = 15
	icolm(3) = 1
c  pss1ad
	mcode(4) = 2
	ncode(4) = 15
	icolm(4) = 3
c  pss1bd
	mcode(5) = 2
	ncode(5) = 15
	icolm(5) = 4
c  pss2ad
	mcode(6) = 2
	ncode(6) = 15
	icolm(6) = 7
c  pss3ad
	mcode(7) = 2
	ncode(7) = 15
	icolm(7) = 8
c  pss2bd 
        mcode(8) = 2
        ncode(8) = 16
        icolm(8) = 3
c  psyngd
        mcode(9) = 2
        ncode(9) = 16
        icolm(9) = 1
c  pss4d 
        mcode(10) = 2
        ncode(10) = 16
        icolm(10) = 2
	ino = 1
	go to 388
c
 465   continue
c
c  daily  canopy transpiration scaling
c
       nvbl =10
c rncpyd 
	mcode(1) = 2
	ncode(1) = 18
	icolm(1) = 1
c  hcpyd
	mcode(2) = 2
	ncode(2) = 18
	icolm(2) = 2
c  ecpyd 
	mcode(3) = 2
	ncode(3) = 18
	icolm(3) = 3
c  hsoild
	mcode(4) = 2
	ncode(4) = 18
	icolm(4) = 4
c  rnsold
	mcode(5) = 2
	ncode(5) = 18
	icolm(5) = 5
c  hcpysd
	mcode(6) = 2
	ncode(6) = 18
	icolm(6) = 6
c  wcpysd
	mcode(7) = 2
	ncode(7) = 18
	icolm(7) = 7
c  rnlfd  
        mcode(8) = 2
        ncode(8) = 17
        icolm(8) = 1
c  evlfd 
        mcode(9) = 2
        ncode(9) = 17
        icolm(9) = 2
c  qlfd  
        mcode(10) = 2
        ncode(10) = 17
        icolm(10) = 3
	ino = 1
	go to 388
c
c  this finishes off searches over time
c
385	nhrs = 48
	do 387 i=1,nvbl
	nlay(i) = 0
387	nang(i) = 0
	goto 390
 388    nhrs=1
	do 389 i=1,nvbl
	nlay(i) = 0
389	nang(i) = 0
c
c
c  which vbl will be indep vbl 1=hour,2=layer,3=angle
 390  if(iset.eq.0)write(6,400)
 400  format(' indep vbl(1=hr,2=lay,3=ang)')
      if(iset.eq.0)read(5,*)ino
c  get number of vbls such as leaf temp,air temp, vis rad, etc and their
c    code numbers.  these are not derived vbls from hrs,layers or angles
      if(iset.eq.0)write(6,2100)
 2100 format(' input nvbl')
      if(iset.eq.0)read(5,*)nvbl
      if(iset.eq.0.and.icomp.eq.1)write(6,2150)
c  delta is magnitude of dif that invokes print to file 26 on vbl
 2150 format(' input vbl code-m,nn,col#,delta')
      if(iset.eq.0.and.icomp.eq.0)write(6,2151)
 2151 format(' input vbl code-m,nn,col#')
      do2300i=1,nvbl
 2190 if(iset.eq.0)write(6,2200)i
 2200 format(' vbl ',i1)
      if(iset.eq.0.and.icomp.eq.0)read(5,*)mcode(i),ncode(i),icolm(i)
      if(iset.eq.0.and.icomp.eq.1)read(5,*)mcode(i),ncode(i),icolm(i)
     &,delta(i)
      if(mcode(i).lt.0)go to 2210
      if(mcode(i).gt.9)go to 2210
      if(ncode(i).lt.0)go to 2210
      if(ncode(i).gt.30)go to 2210
      if(icolm(i).lt.0)go to 2210
      if(icolm(i).gt.9)go to 2210
      go to 2300
 2210 if(iset.ne.0)write(6,*)' trbl with mcode,ncode or icolm,stm 2210'
      if(iset.eq.0)go to 2190 
 2300 continue
c  no. of hours per day from cupid output
      if(iset.eq.0)write(6,500)
 500  format(' input nhrs/day')
      if(iset.eq.0)read(5,*)nhrs
c  if nhrs le 0 than all hrs on all days wanted
      if(nhrs.le.0)go to 800
      if(ino.ne.1.and.iset.eq.0)write(6,600)
 600  format(' input hr nos')
      if(ino.ne.1.and.iset.eq.0)read(5,*)(ihr(k),k=1,nhrs)
c  nvhr is vbl that recordes how many hours of data go into total no.
c    of vbls. to hp graphics
      if(ino.ne.1)nvhr=nhrs
      if(ino.eq.1)no=nhrs
      go to 890
 800  if(iset.eq.0)write(6,810)
 810  format(' input no for all days & hrs(nodays+nhrs)')
      if(iset.eq.0)read(5,*)no
c  no. of layers involved in output form cupid output
c  when time is indep vbl, some vbls may be in layers or angle classes
c    and others may not so nlay must be subscripted.
c
c  check whether and vbls require layer or angle specification
 890  write(6,891)
 891  format(' does any vbl require layers or angle classes (1/0)')
      read(5,*)jlay
      if(jlay.ne.1)goto 2010
c
c
      nvbll=nvbl
      if(ino.eq.2)nvbll=1
      if(iset.eq.0)write(6,900)nvbll
 900  format(' input nlay(l),l=1,',i2)
      if(iset.eq.0)read(5,*)(nlay(l),l=1,nvbll)
      iin=0
      do1400l=1,nvbll
      if(nlay(l).lt.0)go to 1200
      if(nlay(l).eq.0)go to 1400
c  assume layer nos are same for all nvbll
      if(ino.ne.2.and.iin.eq.0.and.iset.eq.0)write(6,1000)nlay(l)
 1000 format(' input' ,i2,' layer nos')
      nloop=nlay(l)
      if(ino.ne.2.and.iin.eq.0.and.iset.eq.0)read(5,*)(ilay(k),k=1,nloop
     &)
      iin=1
c  nvlay is vbl that recordes how many layers of data go into total
c    no. of vbls.
      if(ino.ne.2)nvlay(l)=nlay(l)
      if(ino.eq.2)no=nlay(l)
      go to 1400
c  if layer nos. are not to be specified,must know whether max no.
c    of layers is jtot(rad) or qjzbot(turb).
 1200 if(ino.ne.2.and.iin.eq.0.and.iset.eq.0)write(6,1000)nlay(l)
      if(ino.ne.2.and.iin.eq.0.and.iset.eq.0)read(5,*)(nlay(lx),lx=1,nvb
     &ll)
      iin=1
      if(ino.ne.2)nvlay(l)=nlay(l)
      if(ino.eq.2)no=nlay(l)
 1400 continue
c  no. of leaf angle classes involved in output from cupid output file.
      nvbla=nvbl
      if(ino.eq.3)nvbla=1
 1490 if(iset.eq.0)write(6,1500)nvbla
 1500 format(' input nang(l),l=1,',i2,'(-1=all)')
      if(iset.eq.0)read(5,*)(nang(l),l=1,nvbla)
      iin=0
      do2000l=1,nvbla
      if(nang(l).lt.0)go to 1800
      if(nang(l).eq.0)go to 2000
      if(ino.ne.3.and.iin.eq.0.and.iset.eq.0)write(6,1600)nang(l)
 1600 format(' input',i2,'  angl cls nos')
      nloop=nang(l)
      if(ino.ne.3.and.iin.eq.0.and.iset.eq.0)read(5,*)(iang(k),k=1,nloop
     &)
      iin=1
c  nvang recordes no. of angle classes that go into total no. of vbls.
      if(ino.ne.3)nvang(l)=nang(l)
      if(ino.eq.3)no=nang(l)
      go to 2000
 1800 continue
      if(ino.ne.3.and.iin.eq.0.and.iset.eq.0)write(6,1900)nvbla
 1900 format(' input',i2,'  itot+1')
      if(ino.ne.3.and.iin.eq.0.and.iset.eq.0)read(5,*)(nang(lx),lx=1,nvb
     &la)
      iin=1
      if(ino.ne.3)nvang(l)=nang(l)
      if(ino.eq.3)no=nang(l)
 2000 continue
 2010 continue
c  calc total number of vbls going to hp. this is tricky because if
c   nlay is indep vbl than values of variables at selected layers may
c   represent additional variables from point of vies of hp graphics
      nv=0
      do2400i=1,nvbl
      do2400j=1,nvhr
      mloop=nvlay(i)
      if(mloop.eq.0)mloop=1
      do2400k=1,mloop
      nloop=nvang(i)
      if(nloop.eq.0)nloop=1
      do2400l=1,nloop
 2400 nv=nv+1
c
c  must search for dat and get it into a table form with nv columns and
c    no observations per column.
c
c  first get id codes so all derived variables can be searched for in
c    one pass of the data.
      do6000jday=1,nday
c
c    -lmnndddhhiijj--    code number structure
c                         code number is on line before labels and
c                         on the appropriate data line. this is necessar
c                         to avoid a and f format problems.
      do2900i=1,nv
      do2900l=1,3
 2900 idin(i,l)=1
      if(ino-2)3000,4000,5000
c
c  setup code number with hour as indep. vbl.
c  vbl idin(l) is 1 or 0 and is used to modify idin2,idin3 or idin4
c    so code read in does not include indep vbl code number.
c    thus idin2 3 or 4 are compatible with id(i,2).l=1 hr,l=2 ang,l=3
c    layer.
 3000 continue
      ncount=0
      do3500i=1,nvbl
      if(nvlay(i).eq.0.and.nvang(i).eq.0)go to 3400
      if(nvlay(i).ne.0.and.nvang(i).ne.0)go to 3100
      go to 3200
 3100 nloop=nvlay(i)
      nloop1=nvang(i)
      do3150j=1,nloop
      do3150k=1,nloop1
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=iang(k)*100+ilay(j)
      icolm1(ncount)=icolm(i)
      idin(ncount,1)=0
 3150 continue
      go to 3500
 3200 if(nvlay(i).eq.0.and.nvang(i).ne.0)go to 3300
      nloop=nvlay(i)
      do3250j=1,nloop
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=ilay(j)
      icolm1(ncount)=icolm(i)
      idin(ncount,2)=0
      idin(ncount,1)=0
 3250 continue
      go to 3500
 3300 nloop=nvang(i)
      do3350k=1,nloop
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=iang(k)*100
      icolm1(ncount)=icolm(i)
      idin(ncount,3)=0
      idin(ncount,1)=0
 3350 continue
      go to 3500
 3400 ncount=ncount+1
c  ihrday is 0 or 1 and controls whether all hrs and all days wanted
c    if ihrday=0 all hrs and days wanted so remove day form id(ncount,1)
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)*ihrday
      id(ncount,2)=0
      icolm1(ncount)=icolm(i)
      idin(ncount,2)=0
      idin(ncount,3)=0
      idin(ncount,1)=0
 3500 continue
      go to 5520
c  setup code number with layer as indep variable
 4000 continue
      ncount=0
      do4500i=1,nvbl
      if(nvhr.eq.0.and.nvang(i).eq.0)go to 4400
      if(nvhr.ne.0.and.nvang(i).ne.0)go to 4100
      go to 4200
 4100 do4150j=1,nvhr
      nloop=nvang(i)
      do4150k=1,nloop
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=iang(k)*100+ihr(j)*10000
      icolm1(ncount)=icolm(i)
      idin(ncount,3)=0
 4150 continue
      go to 4500
 4200 if(nvhr.eq.0.and.nvang(i).ne.0)go to 4300
      do4250j=1,nvhr
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=ihr(j)*10000
      icolm1(ncount)=icolm(i)
      idin(ncount,2)=0
      idin(ncount,3)=0
 4250 continue
      go to 4500
 4300 nloop=nvang(i)
      do4350k=1,nloop
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=iang(k)*100
      icolm1(ncount)=icolm(i)
      idin(ncount,1)=0
      idin(ncount,3)=0
 4350 continue
      go to 4500
 4400 ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=0
      icolm1(ncount)=icolm(i)
      idin(ncount,1)=0
      idin(ncount,2)=0
      idin(ncount,3)=0
 4500 continue
      go to 5520
c  setup code number with leaf angle class as indep vbl.
 5000 continue
      ncount=0
      do5500i=1,nvbl
      if(nvlay(i).eq.0.and.nvhr.eq.0)go to 5400
      if(nvlay(i).ne.0.and.nvhr.ne.0)go to 5100
      go to 5200
 5100 nloop=nvlay(i)
      do5150j=1,nloop
      do5150k=1,nvhr
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=ilay(j)+ihr(k)*10000
      icolm1(ncount)=icolm(i)
      idin(ncount,2)=0
 5150 continue
      go to 5500
 5200 if(nvlay(i).eq.0.and.nvhr.ne.0)go to 5300
      nloop=nvlay(i)
      do5250j=1,nloop
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=ilay(j)
      icolm1(ncount)=icolm(i)
      idin(ncount,1)=0
      idin(ncount,2)=0
 5250 continue
      go to 5500
 5300 do5350k=1,nvhr
      ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=ihr(k)*10000
      icolm1(ncount)=icolm(i)
      idin(ncount,3)=0
      idin(ncount,2)=0
 5350 continue
      go to 5500
 5400 ncount=ncount+1
      id(ncount,1)=ncode(i)*1000+mcode(i)*100000+iday(jday)
      id(ncount,2)=0
      icolm1(ncount)=icolm(i)
      idin(ncount,1)=0
      idin(ncount,3)=0
      idin(ncount,2)=0
 5500 continue
 5520 continue
c     write(6,*)((idin(i,j),j=1,3),i=1,nv)
      write(6,5530)(id(j,1),j=1,nv)
      write(6,5530)(id(j,2),j=1,nv)
 5530 format(10(1x,i6))
c     write(6,*)nvbl,nhrs,(nang(k),k=1,nvbl),(nlay(k),k=1,nvbl),nv
c    1,ncount,no
c  read label id(first half)
c    if it matches next line is label. dont
c    worry about hour here because labels for all hours are the same.
c
c  zero vbl icount(i) used to count up no. of obs for each vbl.
c     nv=ncount
c
c  ***************************************************file compare
c
      if(nv.ne.ncount)write(6,5480)nv,ncount
 5480 format(' trouble nv=',i3,'ncount=',i3)
      do5501i=1,nv
      igood(i)=0
 5501 if(jday.eq.1)icount(i)=0
      istop=1
c  search file for correct label
 5505 read(24,5510,end=7000,err=5511)ilabl,idin1,idum
 5510 format(1x,i1,i6,i2)
      ilablq=ilabl
      idin1q=idin1
      goto 5513
 5511 write(6,5512)ilablq,idin1q,idum
 5512 format(' last ilabl= ',i3,' last idin1= ',i8,' ihr= ',i3)
      stop
 5513 continue
c  save first half of id for output if files dont compare
      id1sav=idin1
c
      if(icomp.eq.1)read(23,5510,end=7000)ilablx,idin1x
      if(icomp.eq.1.and.idin1.ne.idin1x)write(6,5509)idin1,idin1x
 5509 format(' first file id does not compare ',2i8)
c
      if(ilabl.ne.1)go to 5505
 5515 continue
      read(24,5560,end=7000)((lab(m,k),k=1,3),m=1,9)
 5560 format(14x,9(1x,3a2))
c
      if(icomp.eq.1)read(23,5560,end=7000)((lab(m,k),k=1,3),m=1,9)
c
c     write(6,5560)((lab(m,k),k=1,3),m=1,9)
      do5580i=1,nv
      if(id(i,1).ne.idin1)go to 5580
      do5570k=1,3
 5570 label(i,k)=lab(icolm1(i),k)
 5580 continue
 5590 read(24,5600,end=7000)ilabl,idin1,idin2,idin3,idin4,(data(m),m=1,9
     1)
c
      if(icomp.eq.1)read(23,5600,end=7000)ilabl,idin1x,idin2x,idin3x,
     &idin4x,(datax(m),m=1,9)
c
      do5595i=1,nv
c  ihrday=0 if all hrs and days wanted so remove day from idin1
      if(ihrday.eq.0)idin1=(idin1/1000)*1000
 5595 if(idin1.eq.id(i,1))idin5=idin(i,1)*idin2*10000+idin(i,2)*idin3*10
     10+idin(i,3)*idin4
c  save second half of raw id if files dont compare
      id2sav=idin2*10000+idin3*100+idin4
c
      if(icomp.eq.1.and.idin1.eq.id(i,1))idin5x=idin(i,1)*idin2x*10000
     &+idin(i,2)*idin3x*100+idin(i,3)*idin4
      if(icomp.eq.1.and.idin5.ne.idin5x)write(6,5597)idin1,idin5,idin1x
     &,idin5x
 5597 format(' second half of line id does not compare ',2i8,1x,2i8)
c
c     write(6,*)ilabl,idin1,idin5
 5600 format(1x,i1,i6,3i2,9f7.2)
c  check to see if 2 labels in a row,if so read second label and
c    save it if appropriate.
      if(ilabl.eq.1)go to 5515
c  if next line was data check to see if its data we want.
      do5900i=1,nv
      if(id(i,1).eq.idin1.and.id(i,2).eq.idin5)go to 5700
      go to 5900
 5700 if(icount(i).ge.no*jday)go to 5900
      icount(i)=icount(i)+1
      data1(i,icount(i))=data(icolm1(i))
c
      if(icomp.eq.1)data1x(i,icount(i))=datax(icolm1(i))
      if(icomp.eq.1)idsav1(i,icount(i))=id1sav
      if(icomp.eq.1)idsav2(i,icount(i))=id2sav
c     write(6,5850)i,icount(i),idin1,idin5,data1(i,icount(i))
 5850 format(1x,i2,i3,2i7,f7.2)
 5900 continue
c  read in another set of numerical data if all values have not
c    yet been read in.
      istop=1
      do5950i=1,nv
c     write(6,*)icount(i),no
 5950 if(icount(i).lt.no*jday)istop=0
      if(istop.eq.0)go to 5590
c
 6000 continue
      ieof=0
      go to 7080
 7000 write(6,7050)no
 7050 format(' eof reached no=',i3)
c  use max icount(i) for no
      no=icount(1)
      if(nv.eq.1)go to 7070
      do7060i=2,nv
 7060 if(icount(i).gt.no)no=icount(i)
 7070 continue
      ieof=1
      rewind 24
      read(24,40)title1
      read(24,40)title2
c  *************************************************compare files
 7080 if(icomp.eq.0)goto 501
      ipnt=0
      do8200 i=1,nv
      do8100 j=1,icount(i)
      dif=data1(i,j)-data1x(i,j)
      if(abs(dif).ge.delta(i))  write(26,8010)idsav1(i,j),idsav2(i,j),
     &(label(i,k),k=1,3),data1(i,j),dif
      if(abs(dif).ge.delta(i))ipnt=1
 8010 format(1x,2i6,1x,3a2,2(1x,f7.2))
 8100 continue
 8200 continue
      if(ipnt.eq.1)write(6,8300)
 8300 format(' files 24 and 23 differ more than delta')
      if(ipnt.eq.0)write(6,8400)
 8400 format(' files 24 and 23 differ less than delta')
c
c  labels are stored in array label(i,k) where i is subscript for
c    no. of indep vbls and k is no. of a2 fields reqd for storing
c    6 letter name(3).
c  data are storred in array data1(i,icount) where icount is subscript o
c    over no. of obs.
c  write file 'hpg input' with label on first record and data following
c    so hpg program can handle it.
c      writing to 9845 memory
c   input information for 9845 plot
 501  mode=1
c  print out labels with code nos if desired
      write(6,818)
 818  format(' pnt lab(0,1),data(0,1)')
      read(5,*)ipnt1,ipnt2
      nvout=nv
      ispred=1
      if(ispred.eq.0)then
c
c  file output for fortran or other use
c
      write(25,40)title1
      write(25,40)title2
      write(25,*)' input file name ........',filein
      write(25,*)' output file name .......',filout
      if(nv.gt.10)nvout=10
      write(25,825)(id(j,1),j=1,nvout)
      if(ipnt1.eq.1)write( 6,825)(id(j,1),j=1,nvout)
      write(25,825)(id(j,2),j=1,nvout)
      if(ipnt1.eq.1)write( 6,825)(id(j,2),j=1,nvout)
 825  format(10(1x,i6))
      if(ipnt1.eq.1)write(6,830)((label(m,i),i=1,3),m=1,nvout)
      write(25,830)((label(m,i),i=1,3),m=1,nvout)
 830  format(10(1x,3a2))
      if(ipnt1.eq.1)write(6,825)(icount(j),j=1,nvout)
      write(25,825)(icount(j),j=1,nvout)
      else
c
c  file output for quattro pro spread sheet
c
      write(25,901)title1
      write(25,901)title2
 901  format('"',a78,'"')
      write(25,*)'"input file name ........',filein,'"'
      write(25,*)'"output file name .......',filout,'"'
      if(nv.gt.10)nvout=10
      write(25,925)(id(j,1),j=1,nvout)
      if(ipnt1.eq.1)write( 6,925)(id(j,1),j=1,nvout)
      write(25,925)(id(j,2),j=1,nvout)
      if(ipnt1.eq.1)write( 6,925)(id(j,2),j=1,nvout)
 925  format(10(1x,i6))
      if(ipnt1.eq.1)write(6,930)((label(m,i),i=1,3),m=1,nvout)
      write(25,930)((label(m,i),i=1,3),m=1,nvout)
 930  format(10('"',3a2,'"'))
      if(ipnt1.eq.1)write(6,925)(icount(j),j=1,nvout)
      write(25,925)(icount(j),j=1,nvout)
      endif
      if(ipnt1.eq.1)write(6,840)
 840  format('   1      2      3      4      5      6      7   ',
     1'   8      9     10   ')
      nopnt=no*nday
      do7200jno=1,nopnt
      if(ipnt2.eq.1)write(6,7101)(data1(i,jno),i=1,nvout)
 7101 format(1x,10f7.2)
 7200 write(25,7101)(data1(i,jno),i=1,nvout)
 7100 format(10(1x,f6.2))
 835  if(nv.le.10)go to 845
      write(25,825)(id(j,1),j=11,nv)
      if(ipnt2.eq.1)write(6,825) (id(j,1),j=11,nv)
      write(25,825)(id(j,2),j=11,nv)
      if(ipnt2.eq.1)write(6,825) (id(j,2),j=11,nv)
      if(ipnt2.eq.1)write(6,830)((label(m,i),i=1,3),m=11,nv)
      write(25,830)((label(m,i),i=1,3),m=11,nv)
      if(ipnt2.eq.1)write(6,825)(icount(j),j=1,nvout)
      write(25,825)(icount(j),j=1,nvout)
      if(ipnt2.eq.1)write(6,841)
 841  format('  11     12     13     14     15     16     17   ',
     1'  18     19     20   ')
      if(ieof.eq.1)go to 99
      nopnt=no*nday
      do7400jno=1,nopnt
      if(ipnt2.eq.1)write(6,7101)(data1(i,jno),i=11,nv)
 7400 write(25,7101)(data1(i,jno),i=11,nv)
 845  continue
 850  write(6,820)
 820  format(' no. vbls to hp')
      read(5,*)nvhp
      if(nvhp.eq.0)rewind 24
      read(24,40)title1
      read(24,40)title2
      if(nvhp.eq.0.and.icomp.eq.1)rewind 23
      if(nvhp.eq.0.and.icomp.eq.1)read(23,40)title3
      if(nvhp.eq.0.and.icomp.eq.1)read(23,40)title4
      if(nvhp.eq.0)go to 99
      write(6,860)
 860  format(' col nos. of vbl for plot')
      read(6,*)(n(i),i=1,nvhp)
      write(6,519)
 519  format(' ','enter 80 character title for this data set')
      read(6,525) title
 525  format(20a4)
c  read variable names at top of each column if they are there
      write(6,430)
 430  format(' vbl names',/' name  no. col')
      do460m=1,nvhp
 460  write(6,440)(label(n(m),i),i=1,3),m,n(m)
 440  format(1x,3a2,2i3)
      write(6,470)
 470  format(' ok? (0,1)')
      read(6,*)nok
      if(nok.eq.1)go to 80
c  variable names can be changed. if changed all must be changed.
c  names of variables must be less than or equal to 10 characters.
      write(6,535)
 535  format(' vbl names <= 10 char')
      do 75 i=1 , nvhp
      write(6,538) i,n(i)
 538  format(' ','enter name of vbl ',i2,' in col ',i2)
      read(6,550) (name(i,j),j=1,5)
 550  format(5a2)
 75   continue
c  select hp format
 80   write(6,554)
 554  format(' format to hp(1=f6.1,2=f6.2,3=f7.2)')
      read(5,*)ifmt
c  input starting and ending obs no. for multiple day data. default
c    for zeros is all observations.
      write(6,610)
 610  format(' input nostr noend (2i3,0=all)')
      read(5,620)nostr,noend
      if(noend.gt.no*nday)noend=no*nday
      if(nostr.lt.1)nostr=1
      if(noend.lt.1)noend=no*nday
      nohp=noend-nostr+1
 620  format(2i3)
c  hp buffer clear and memory recording
c     write(6,555)esc,esc
      write(6,555)esc
c555  format(' ',a1,'w',a1,'s')
 555  format(' ',a1,'s')
      if(mode.eq.1) write(6,560) nvhp,nohp,gs
 560  format(' ',2(i3,1x),a1,'t')
      write(6,580) title
 580  format(' ',20a4)
      do 90 i=1,nvhp
      if(nok.eq.1)write(6,590)(label(n(i),j),j=1,3)
 590  format(' ',3a2)
      if(nok.eq.0) write(6,595) (name(i,j),j=1,5)
 595  format(' ',5a2)
 90   continue
c  send data to hp 9845 terminal
      do960m=1,nvhp
 960  sum1(n(m))=0.
      do980j=nostr,noend
      if(ifmt.eq.1)write(6,950)(data1(n(m),j),m=1,nvhp)
 950  format(10(1x,f6.1))
      if(ifmt.eq.2)write(6,951)(data1(n(m),j),m=1,nvhp)
 951  format(10(1x,f6.2))
      if(ifmt.eq.3)write(6,952)(data1(n(m),j),m=1,nvhp)
 952  format(10(1x,f7.2))
c  calc means of data sent to hp
c
      do970m=1,nvhp
 970  sum1(n(m))=sum1(n(m))+data1(n(m),j)
 980  continue
c     do990m=1,nvhp
c990  sum1(n(m))=sum1(n(m))/no
c     write(6,*)(sum1(n(m)),m=1,nvhp)
c  turn off memory recording
      write(6,1111) esc
 1111 format(' ',a1,'t')
c  enter hp graphics routine
      write(6,1230) esc
 1230 format(' ',a1)
      write(6,1240)
 1240 format(' ','x recycle? (0,1,2)  rewind? (0,1)')
      read(6,*)nrecyc,nrew
      if(nrew.eq.1)rewind 24
      if(nrecyc.eq.0)stop
      if(nrecyc.eq.1)go to 501
      if(nrecyc.eq.2)go to 99
      stop
      end
