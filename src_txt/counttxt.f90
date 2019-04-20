program counttxt
! This program was written to count the number of active subroutine
! and the frequency of each subroutine during each run.

implicit none

!****************************STRUCTURE******************************************
! 1. Convert *.f to .txt for the reading purpose
! 2. List number of subroutine belongs to sub-programs
! 3. Read the function start with CALL
! 4. Create the matrix-like to calculate the frequency
!
!
! Method to convert from *.f -> *.txt
! From terminal using: for x in *.f; do mv "$x" "${x%.f}.txt"; done
! This command convert every *.f to *.txt file
!*******************************************************************************

character (len = 80), allocatable :: text (:)

	integer :: i,j,n
	integer :: maxline, nlines

	maxline = 100000 !in the case of too much word or lines

	nlines = 0

	open (1,file = 'calcpath.txt')
!	open (2,file = 'cuastr.txt')
!	open (3,file = 'cubdrtm.txt')
!	open (4,file = 'cudpe.txt')
!	open (5,file = 'cuet.txt')
!	open (6,file = 'cuht.txt')
!	open (7,file = 'cuinfil.txt')
!	open (8,file = 'cuinp.txt')
!	open (9,file = 'cuintc.txt')
!	open (10,file = 'culad.txt')
!	open (11,file = 'culayr.txt')
!	open (12,file = 'culfbal.txt')
!	open (13,file = 'cunr.txt')
!	open (14,file = 'cuphot.txt')
!	open (15,file = 'cupin2.txt')
!	open (16,file = 'cupmod2.txt')
!	open (17,file = 'cuprof.txt')
!	open (18,file = 'curadia.txt')
!	open (19,file = 'curadin.txt')
!	open (20,file = 'curoot.txt')
!	open (21,file = 'cuscale.txt')
! 	open (22,file = 'cusub.txt')

	do
		read (1,*, end = 10)
		nlines = nlines + 1
	end do
	write (*,*) nlines

	do i = 1,80
		read (1,*) text(i)
	end do
	10 close (1)
! 	deallocate(text)
end program counttxt
