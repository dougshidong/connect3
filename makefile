OUT = connect3.exe
OBJ = prec.f90 globals.f90 ctofort.f90 boardclass.f90 ai.f90
OFF = prec.o globals.o ctofort.f90 boardclass.o ai.o
DBG = -Wall -Wextra -Wconversion -pedantic -fbounds-check -ffpe-trap=zero,overflow,underflow -fbacktrace -pg
#PRC = -fdefault-real-8 -fdefault-integer-8
#PRC = -fdefault-integer-8
compile:
	gcc -c tcp.c
	gfortran $(PRC) $(OBJ) -c -O3
	gfortran main.f90 $(OFF) tcp.o -o $(OUT)
dbg:
	gcc -c -g tcp.c
	gfortran $(PRC) $(DBG) $(OBJ) -c
	gfortran main.f90 $(DBG) $(OFF) tcp.o -o $(OUT)
pro:
	gcc -c tcp.c
	gfortran $(PRC) $(OBJ) -p -c -O1
	gfortran main.f90 -p $(OFF) tcp.o -o $(OUT)
clean:
	rm -fv *.out *.mod *.MOD $(OUT)
