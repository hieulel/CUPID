C $VERSION "08/16/95 @(#)calcpath.f	7.1"
c
c	---------------------------------------------------------------
c	|						CalcPath
c	| calculate the path length a beam of
c	| light given the slope and aspect of the
c	| ground and the angle of the sun.  This
c	| is returns a unitless ratio that is
c	| multiplied with the height of the canopy
c	| to give you the path length through
c	| the canopy.
c
c	| changed to use Shinsuke's equations 94/2/10
c
	subroutine calcpath(nohrs,slope,aspect,path)
c
	IMPLICIT NONE
	integer    mh
	parameter (mh=98)
c
c	-----
c	| parameters
c
	integer nohrs
	real	slope
	real	aspect
	real	path(mh)
c
c	-----
c	| commons
c
	real	pi
	real	pid180
	real	pid2
	real	sigma
	integer	iwrite(9,99)
	integer kmax
	common/misc1/pi,pid180,pid2,sigma,iwrite,kmax
c
	real 	radtop(3,mh)
	real	fbeam1(3,mh)
	real	coszen(mh)
	real	zenang(mh)
	real	hfday
	real	ratiod
	real	ration
	real	ratio(mh)
	common /rad3/radtop,fbeam1,coszen,zenang
     &		     	,hfday,ratiod,ration,ratio
c
	real	sunazm(mh)
	real	nozenv
	real	viewzn(10)
	real	noazmv
	real	viewaz(50)
	real	xintv(10,50)
	common /deg/sunazm,nozenv,viewzn,noazmv,viewaz,xintv
c
c	------
c	| Local
c
	integer	i
	real	term1
	real	term2
	real	sqs
	real	e
c
c
	do 2000, i = 1, nohrs
c
	    if (zenang(i) .lt. pid2*.998) then
c
		if (sunazm(i).eq.0.0) then
		    write(*,*)'sunazm of i = ',i,' equals 0.0'
		    write(*,*) 'zenang of i = ',zenang(i)
		    stop
		endif
c
      	        path(i)= cos(zenang(i))*cos(slope)+sin(zenang(i))
     &				*sin(slope)*cos(aspect-sunazm(i))
  		if(path(i).eq.0.)path(i)=1.0e-11
		path(i)=1./path(i)
 		if (path(i).le.0.0.or.path(i).gt.1.0e10) then
c		if (path(i).le.0.0) then
		    path(i)=-1.0
		endif
c
	    else
c	 	----------
c		| Sun  below horizon
c
		path(i) = -1.0
	    endif
c
2000	continue
	return
	end
