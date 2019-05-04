# CUPID Subroutine Structure
Written by [Hieu Le](https://github.com/hieulel)

## Introduction
In Cupid, there exists many subroutines.This is a reason why we need to care how our code works. As a result, I wrote this Markdown file to show the detail of each subroutine including
1. Usage
2. Main structure
3. Algorithm (if available)
4. `CALL` function to interconnect with other subroutines
Nonetheless, please follow the Table of Contents (TOC) to access it as a library.

## Table of Contents
1. [`calcpath.f`](#calcpathf)
2. [`cuastr.f`](#cuastrf)
3. [`cubdrtm.f`](#cubdrtmf)
4. [`cudpe.f`](#cudpef)
5. [`cuet.f`](#cuetf)
6. [`cuht.f`](#cuhtf)
7. [`cuinf.f`](#cuinff)
8. [`cuinp.f`](#cuinpf)
9. [`cuintc.f`](#cuintcf)
10. [`culad.f`](#culadf)
11. [`culayr.f`](#culayrf)
12. [`culfbal.f`](#cufbalf)
13. [`cunr.f`](#cunrf)
14. [`cuphot.f`](#cuphotf)
15. [`cupin2.f`](#cupin2f)
16. [`cupmod2.f`](#cupmod2f)
17. [`cuprof.f`](#cuproff)
18. [`curadia.f`](#curadiaf)
19. [`curadin.f`](#curadinf)
20. [`curoot.f`](#curootf)
21. [`cuscale.f`](#cuscalef)
22. [`cusub.f`](#cusubf)

## Subfile
<!-- For each heading, add usage, structure and subroutine called -->
### `calcpath.f`
#### Usage
* To **calculate the path length a beam of light** given the slope and aspect of the ground and the angle of the sun. This returns a unitless ratio that is multiplied with the height of the canopy to give you the path length through the canopy
#### Main Structure
* Input: 
* Ouput: path length through the canopy
* **Algorithm**:
	```fortran
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
	```
#### Other subroutine `CALL` function
`CALL = NULL`
* * *

### `cuastr.f`
#### Usage
* Dedicate for suncyle (similar code in Matlab can be found [here](http://mooring.ucsd.edu/software/matlab/doc/toolbox/geo/suncycle.html))
* Return Sunrise, sunset, solar altitude and radiation
* To calculate the calendar date, sun position and declination. This implies the radiation impacts on the leaves.
#### Main Structure
Comprises three subroutines:
1. `date`
* Calendar Date
* Refer to [time structure in cupid](https://soils.wisc.edu/facstaff/wayne/cupid/timsum.html)
* Maximum time step `mh=98` (roughly 15 mins)
* *This statement determines the maximum number of time steps in a day and is presently set to 98*, so that the **maximum number of time steps per day is 96**
2. `declin`
* Calculate decline of sun in rad and equal of time in fractions of hours.
* Refer to [Position of the Sun](https://en.wikipedia.org/wiki/Position_of_the_Sun)
* Refer to [Declination](https://en.wikipedia.org/wiki/Declination)
3. `zenith`
* Sun zenith and `azm angl` (refer to [Zenith](https://en.wikipedia.org/wiki/Zenith))
```fortran
    hfday=12./pi*acos(-(sinlat*sindec)/(coslat*cosdec)) !half day
    .
    timsun(i)=timloc(i)+eqtm+dlong !time of the sun
    hrang(i)=(timsun(i)-12.)*pid2/6
    .
    zenang(i)=acos(sinlat*sindec+coslat*cosdec*cos(hrang(i)))
    sunazm(i)=asin(cosdec*sin(hrang(i))/sin(zenang(i))) !sun azimuth
    .
    crtzen=acos(sindec/sinlat)
    if(zenang(i).gt.crtzen)sunazm(i)=(pi-abs(sunazm(i)))*sunazm(i)
    1/abs(sunazm(i))
    sunazm(i) = sunazm(i) + pi
! sunazm is solar azimuth
```
* Refer to [Solar Azimuth `azm`](https://en.wikipedia.org/wiki/Solar_azimuth_angle)

![Solar azimuth](https://www.pveducation.org/sites/default/files/PVCDROM/Properties-of-Sunlight/Images/AZIMUTH.gif)
#### Other subroutine `CALL` function
`CALL = NULL`
* * *

### `cubdrtm.f`
#### Usage
* Directional thermal IR
#### Main Structure
* Contains only one subroutine, i.e. `cbdrtm.f`
* `factir` is the factor that the thermal sky flux calc'd from subr
* `skyir` is multiplied by to get sky thermal in band of interest.
#### Other subroutine `CALL` function
`CALL = NULL`
* * *

### `cudpe.f`
#### Usage
To calculate the layer summaries
#### Main Structure
Include subroutine
* `dpe`
* `drywt`
#### Other subroutine `CALL` function
`CALL = NULL`
* * *

### `cuet.f`
#### Usage
* Calculate evapotranspiration in CUPID
* Use Penmon-Monteith model (OSM) over other TSM
* Applied twice: to the soil and the canopy.
#### Main Structure
Contains 2 subroutines
1. `penmon`
* Output: `ihr`, `icumdy`, `etpm`
2. `simpet`
* Output: `ihr`,`icumdy`,`rncpyd`,`taird`,`vpaird`,`winddy`,`solard`,`etpmd`,`iday`,`tairmx`,`tairmn`
* Subroutine to calc daily et from simple equations.
#### Other subroutine `CALL` function
* * *

### `cuht.f`
#### Usage
* Calculate factors to mult wind above canopy by to get wind in `windfac`
* Compute height for any LAI given canopy height h `height`
* Calculate leaf boundary layer resistance vs. height in canopy `rboudn`
* Calculate height array to go from lower soil b.c to upper b.c above `hite2`
#### Main Structure
Include subroutines
* `wndfac`
* `height`
* `rbound`
Assume dead leaves are of size `sizelef/2` - can be found in layers up to layer `jdead`
* `hite2`
#### Other subroutine `CALL` function
`CALL = NULL`
* * *

### `cuinf.f`
#### Usage
Compute the soil water profile for a given timestep
#### Main Structure
Contain subroutines:
* `soilw`
* `hydro`
#### Other subroutine `CALL` function
`soilw` call `hydro`
* * *

### `cuinp.f`
#### Usage

#### Main Structure
Include subroutine
* `infix`
* `inplnt`
#### Other subroutine `CALL` function
* * *

### `cuintc.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `culad.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `culayr.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `culfbal.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `cunr.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `cuphot.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `cupin2.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `cupmod2.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `cuprof.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `curadia.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `curadin.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `curoot.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `cuscale.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *

### `cusub.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
* * *
