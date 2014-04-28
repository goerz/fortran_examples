FC = gfortran
FFLAGS = -O3
LIBS = -llapack -leigenlib
LDFLAGS = -L../eigenlib_install -I../eigenlib_install/mod

# The LDFLAGS defined above allow to use the eigenlib liberary, see 
# ../eigenlib/
# 
# This program won't compile unless you run 'make install' in ../eigenlib/.
# The 'make install' for eigenlib will create a folder ../eigenlib_install that
# contains the files libeigenlib.a and in a subfolder mod all the .mod files
# for the modules of eigenlib.
#
# The -L flag above tells the compiler where to find libeigenlib.a (which it is
# looking for because -leigenlib is in LIBS), and the -I flag tells the
# compiler where to look for .mod files

# How to make object files from Fortran 77 files.
%.o: %.f
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# How to make object files from Fortran 90 files.
%.o: %.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# How to make object files from Fortran 90 preprocessor files.
%.o: %.F90
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# Note that technically, the -L flag isn't necessary for the compilation step
# above, but I never bother to separate -I (which nees to be there both for
# compilation and linking), and -L.

all: eigenproblem

eigenproblem: eigenproblem.o
	$(FC) $(FFLAGS) -o $@ eigenproblem.o $(LDFLAGS) $(LIBS)

clean:
	rm -f *.o
	rm -f eigenproblem

.PHONY: all clean

