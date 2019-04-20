#  Make File for cupid7
#
# Generic flags for gfortran
FLAGS =
#
# Generic flags for Ansi C compiler
CFLAGS = -Iutl
#
# Ansi C Flags for HPUX
#CFLAGS = -Aa -g -Iutl
#
# Flags for HPUX
#FLAGS  = -C -g +FPVZ
#
# OP is the Fortran Optimizer, see man gfortran


OBJ= $(OBJG1) $(OBJ2) $(OBJ3) $(OBJ4)
OBJG1 = o/cupin2.o o/cusub.o o/cupmod2.o o/cuinp.o o/curadia.o o/culad.o o/curoot.o
OBJ2 = o/culfbal.o o/cuprof.o o/cunr.o o/cuphot.o o/cuintc.o
OBJ3 = o/cuet.o o/cudpe.o o/cubdrtm.o o/cuscale.o o/calcpath.o
OBJ4 = o/cuht.o o/curadin.o o/cuastr.o o/cuinfil.o

cupid7: $(OBJ)
	gfortran  -o cupid7 $(FLAGS) $(OBJ)
clean :
	rm o/*.o

cuprd3: utl/cuprd3.f
	gfortran -o cuprd3 $(FLAGS) utl/cuprd3.f
	rm cuprd3.o

cup2rdb : utl/cup2rdb.c utl/awserror.c utl/commautl.c
	cc $(CFLAGS) -o cup2rdb utl/cup2rdb.c utl/awserror.c utl/commautl.c
	rm cup2rdb.o awserror.o commautl.o

cupdiff : utl/cupdiff.c
	cc $(CFLAGS) -o cupdiff utl/cupdiff.c

context : utl/context.c
	cc $(CFLAGS) -o context utl/context.c




o/cupin2.o : src/cupin2.f
	gfortran -c -o o/cupin2.o $(FLAGS) src/cupin2.f
o/cusub.o : src/cusub.f
	gfortran -c -o o/cusub.o $(FLAGS) src/cusub.f
o/cupmod2.o : src/cupmod2.f
	gfortran -c -o o/cupmod2.o $(FLAGS) src/cupmod2.f
o/cumain1.o : src/cumain1.f
	gfortran -c -o o/cumain1.o $(FLAGS) src/cumain1.f
o/calcpath.o : src/calcpath.f
	gfortran -c -o o/calcpath.o $(FLAGS) src/calcpath.f
o/cuinp.o  : src/cuinp.f
	gfortran -c -o o/cuinp.o $(FLAGS) src/cuinp.f
o/cuastr.o : src/cuastr.f
	gfortran -c -o o/cuastr.o $(FLAGS) src/cuastr.f
o/curadia.o : src/curadia.f
	gfortran -c -o o/curadia.o $(FLAGS) src/curadia.f
o/curadin.o : src/curadin.f
	gfortran -c -o o/curadin.o $(FLAGS) src/curadin.f
o/cuht.o   : src/cuht.f
	gfortran -c -o o/cuht.o $(FLAGS) src/cuht.f
o/culfbal.o : src/culfbal.f
	gfortran -c -o o/culfbal.o $(FLAGS) src/culfbal.f
o/cuprof.o : src/cuprof.f
	gfortran -c -o o/cuprof.o $(FLAGS) src/cuprof.f
o/cunr.o   : src/cunr.f
	gfortran -c -o o/cunr.o $(FLAGS) src/cunr.f
o/cuphot.o : src/cuphot.f
	gfortran -c -o o/cuphot.o $(FLAGS) src/cuphot.f
o/curoot.o : src/curoot.f
	gfortran -c -o o/curoot.o $(FLAGS) src/curoot.f
o/cuintc.o : src/cuintc.f
	gfortran -c -o o/cuintc.o $(FLAGS) src/cuintc.f
o/cuet.o   : src/cuet.f
	gfortran -c -o o/cuet.o $(FLAGS) src/cuet.f
o/culad.o : src/culad.f
	gfortran -c -o o/culad.o $(FLAGS) src/culad.f
o/cudpe.o  : src/cudpe.f
	gfortran -c -o o/cudpe.o $(FLAGS) src/cudpe.f
o/cubdrtm.o : src/cubdrtm.f
	gfortran -c -o o/cubdrtm.o $(FLAGS) src/cubdrtm.f
o/cuscale.o : src/cuscale.f
	gfortran -c -o o/cuscale.o $(FLAGS) src/cuscale.f
o/cuinfil.o : src/cuinfil.f
	gfortran -c -o o/cuinfil.o $(FLAGS) src/cuinfil.f


o/ocupin2.o : src/cupin2.f
	gfortran -c -o o/ocupin2.o $(OPTFLAGS) src/cupin2.f
o/ocusub.o : src/cusub.f
	gfortran -c -o o/ocusub.o $(OPTFLAGS) src/cusub.f
o/ocupmod2.o : src/cupmod2.f
	gfortran -c -o o/ocupmod2.o $(OPTFLAGS) src/cupmod2.f
o/ocumain1.o : src/cumain1.f
	gfortran -c -o o/ocumain1.o $(OPTFLAGS) src/cumain1.f
o/ocalcpath.o : src/calcpath.f
	gfortran -c -o o/ocalcpath.o $(OPTFLAGS) src/calcpath.f
o/ocuinp.o  : src/cuinp.f
	gfortran -c -o o/ocuinp.o $(OPTFLAGS) src/cuinp.f
o/ocuastr.o : src/cuastr.f
	gfortran -c -o o/ocuastr.o $(OPTFLAGS) src/cuastr.f
o/ocuradia.o : src/curadia.f
	gfortran -c -o o/ocuradia.o $(OPTFLAGS) src/curadia.f
o/ocuradin.o : src/curadin.f
	gfortran -c -o o/ocuradin.o $(OPTFLAGS) src/curadin.f
o/ocuht.o   : src/cuht.f
	gfortran -c -o o/ocuht.o $(OPTFLAGS) src/cuht.f
o/oculfbal.o : src/culfbal.f
	gfortran -c -o o/oculfbal.o $(OPTFLAGS) src/culfbal.f
o/ocuprof.o : src/cuprof.f
	gfortran -c -o o/ocuprof.o $(OPTFLAGS) src/cuprof.f
o/ocunr.o   : src/cunr.f
	gfortran -c -o o/ocunr.o $(OPTFLAGS) src/cunr.f
o/ocuphot.o : src/cuphot.f
	gfortran -c -o o/ocuphot.o $(OPTFLAGS) src/cuphot.f
o/ocuroot.o : src/curoot.f
	gfortran -c -o o/ocuroot.o $(OPTFLAGS) src/curoot.f
o/ocuintc.o : src/cuintc.f
	gfortran -c -o o/ocuintc.o $(OPTFLAGS) src/cuintc.f
o/ocuet.o   : src/cuet.f
	gfortran -c -o o/ocuet.o $(OPTFLAGS) src/cuet.f
o/oculad.o : src/culad.f
	gfortran -c -o o/oculad.o $(OPTFLAGS) src/culad.f
o/ocudpe.o  : src/cudpe.f
	gfortran -c -o o/ocudpe.o $(OPTFLAGS) src/cudpe.f
o/ocubdrtm.o : src/cubdrtm.f
	gfortran -c -o o/ocubdrtm.o $(OPTFLAGS) src/cubdrtm.f
o/ocuscale.o : src/cuscale.f
	gfortran -c -o o/ocuscale.o $(OPTFLAGS) src/cuscale.f
o/ocuinfil.o : src/cuinfil.f
	gfortran -c -o o/ocuinfil.o $(OPTFLAGS) src/cuinfil.f



o/testcupin2.o : src/edit/cupin2.f
	gfortran -c -o o/testcupin2.o $(FLAGS) src/edit/cupin2.f
o/testcusub.o : src/edit/cusub.f
	gfortran -c -o o/testcusub.o $(FLAGS) src/edit/cusub.f
o/testcupmod2.o : src/edit/cupmod2.f
	gfortran -c -o o/testcupmod2.o $(FLAGS) src/edit/cupmod2.f
o/testcumain1.o : src/edit/cumain1.f
	gfortran -c -o o/testcumain1.o $(FLAGS) src/edit/cumain1.f
o/testcalcpath.o : src/edit/calcpath.f
	gfortran -c -o o/testcalcpath.o $(FLAGS) src/edit/calcpath.f
o/testcuinp.o  : src/edit/cuinp.f
	gfortran -c -o o/testcuinp.o $(FLAGS) src/edit/cuinp.f
o/testcuastr.o : src/edit/cuastr.f
	gfortran -c -o o/testcuastr.o $(FLAGS) src/edit/cuastr.f
o/testcuradia.o : src/edit/curadia.f
	gfortran -c -o o/testcuradia.o $(FLAGS) src/edit/curadia.f
o/testcuradin.o : src/edit/curadin.f
	gfortran -c -o o/testcuradin.o $(FLAGS) src/edit/curadin.f
o/testcuht.o   : src/edit/cuht.f
	gfortran -c -o o/testcuht.o $(FLAGS) src/edit/cuht.f
o/testculfbal.o : src/edit/culfbal.f
	gfortran -c -o o/testculfbal.o $(FLAGS) src/edit/culfbal.f
o/testcuprof.o : src/edit/cuprof.f
	gfortran -c -o o/testcuprof.o $(FLAGS) src/edit/cuprof.f
o/testcunr.o   : src/edit/cunr.f
	gfortran -c -o o/testcunr.o $(FLAGS) src/edit/cunr.f
o/testcuphot.o : src/edit/cuphot.f
	gfortran -c -o o/testcuphot.o $(FLAGS) src/edit/cuphot.f
o/testcuroot.o : src/edit/curoot.f
	gfortran -c -o o/testcuroot.o $(FLAGS) src/edit/curoot.f
o/testcuintc.o : src/edit/cuintc.f
	gfortran -c -o o/testcuintc.o $(FLAGS) src/edit/cuintc.f
o/testcuet.o   : src/edit/cuet.f
	gfortran -c -o o/testcuet.o $(FLAGS) src/edit/cuet.f
o/testculad.o : src/edit/culad.f
	gfortran -c -o o/testculad.o $(FLAGS) src/edit/culad.f
o/testcudpe.o  : src/edit/cudpe.f
	gfortran -c -o o/testcudpe.o $(FLAGS) src/edit/cudpe.f
o/testcubdrtm.o : src/edit/cubdrtm.f
	gfortran -c -o o/testcubdrtm.o $(FLAGS) src/edit/cubdrtm.f
o/testcuscale.o : src/edit/cuscale.f
	gfortran -c -o o/testcuscale.o $(FLAGS) src/edit/cuscale.f
o/testcuinfil.o : src/edit/cuinfil.f
	gfortran -c -o o/testcuinfil.o $(FLAGS) src/edit/cuinfil.f

#o/testprobe.o : probe.f
#	gfortran -c -o o/testprobe.o $(FLAGS) probe.f

main.o  :  main.f
	gfortran -c -o main.o $(FLAGS) main.f

cusoil.o  :  cusoil.f
	gfortran -c -o cusoil.o $(FLAGS) cusoil.f

main2.o  :  main2.f
	gfortran -c -o main2.o $(FLAGS) main2.f

cuinfil.o  :  cuinfil.f
	gfortran -c -o cuinfil.o $(FLAGS) cuinfil.f
