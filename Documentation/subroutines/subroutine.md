# CUPID Subroutine Structure

## Introduction
In Cupid, there exists many subroutines.This is a reason why we need to care how our code works. As a result, I wrote this Markdown file to show the detail of each subroutine including
1. Usage
2. Main structure
3. Algorithm (if available)
4. `CALL` function to interconnect with other subroutines
Nonetheless, please follow the Table of Contents (TOC) to access it as a library.

## Table of Contents
1. [`calcpath.f`](###`calcpath.f`)
2. [`cuastr.f`](###`cuastr.f`)
3. [`cubdrtm.f`](###`cubdrtm.f`)

## Subfile
<!-- For each heading, add usage, structure and subroutine called -->
### `calcpath.f`
#### Usage
* To calculate the path length a beam of light given the slope and aspect of the ground and the angle of the sun. This returns a unitless ratio that is multiplied with the height of the canopy to give you the path length through the canopy
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

### `cuastr.f`
#### Usage

#### Main Structure
Comprises three subroutines:
1. `date`
* Refer to [time structure in cupid](https://soils.wisc.edu/facstaff/wayne/cupid/timsum.html)
* Maximum time step `mh=98` (roughly 15 mins)
    - This statement determines the maximum number of time steps in a day and is presently set to 98, so that the maximum number of time steps per day is 96
2. `declin`
Calculate decline of sun in rad and equal of time in fractions of hours.
3. `zenith`

#### Other subroutine `CALL` function

### `cubdrtm.f`
#### Usage

#### Main Structure
#### Other subroutine `CALL` function

### `cudpe.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function

### `cuet.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function

### `cuht.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function

### `cuinf.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function

### `cuinp.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function

### `cuintc.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function

### `culad.f`
#### Usage
#### Main Structure
#### Other subroutine `CALL` function
