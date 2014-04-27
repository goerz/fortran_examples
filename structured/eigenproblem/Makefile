FC = gfortran
FFLAGS = -O3
LIBS = -llapack
LDFLAGS = -L/usr/lib

OBJ = constants.o globals.o

# How to make object files from Fortran 77 files.
%.o: %.f
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# How to make object files from Fortran 90 files.
%.o: %.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# How to make object files from Fortran 90 preprocessor files.
%.o: %.F90
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

all: eigenproblem

globals.o: constants.o

eigenproblem: $(OBJ) eigenproblem.o
	$(FC) $(FFLAGS) -o $@ $(OBJ) eigenproblem.o $(LDFLAGS) $(LIBS)

clean:
	rm -f *.o
	rm -f *.mod
	rm -f eigenproblem

.PHONY: all clean

