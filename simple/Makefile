all: cylinder expvalue eigenproblem

cylinder: cylinder.o
	gfortran -o cylinder cylinder.o

expvalue: expvalue.o
	gfortran -o expvalue expvalue.o

eigenproblem: eigenproblem.o
	gfortran -o eigenproblem eigenproblem.o -llapack

cylinder.o: cylinder.f90
	gfortran -c -o cylinder.o cylinder.f90

expvalue.o: expvalue.f90
	gfortran -c -o expvalue.o expvalue.f90

eigenproblem.o: eigenproblem.f90
	gfortran -c -o eigenproblem.o eigenproblem.f90

clean:
	rm -f *.o
	rm -f cylinder expvalue eigenproblem

.PHONY: all clean

