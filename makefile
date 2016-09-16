OUT = connect3.exe
OBJ = prec.f90 globals.f90 boardclass.f90 ai.f90 main.f90
DBG = -Wall -Wextra -Wconversion -pedantic -fbounds-check -ffpe-trap=zero,overflow,underflow -fbacktrace -pg
#PRC = -fdefault-real-8 -fdefault-integer-8
#PRC = -fdefault-integer-8
compile:
	gfortran $(PRC) $(OBJ) -O3 -o $(OUT)
dbg:
	gfortran $(PRC) $(DBG) $(OBJ) -o $(OUT)
clean:
	rm -fv *.out *.mod *.MOD $(OUT)
