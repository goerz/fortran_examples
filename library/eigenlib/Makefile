FC = gfortran
FFLAGS = -O3

OBJ = constants.o diag.o eigenlib.o

# How to make object files from Fortran 77 files.
%.o: %.f
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# How to make object files from Fortran 90 files.
%.o: %.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# How to make object files from Fortran 90 preprocessor files.
%.o: %.F90
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

all: libeigenlib.a

libeigenlib.a: $(OBJ)
	ar -rcs $@ $(OBJ)

# If some module depends on other modules, this needs to be declared explicitly

eigenlib.o: constants.o diag.o

diag.o: constants.o


clean:
	rm -f *.a
	rm -f *.o
	rm -f *.mod

#  Put the result of the compilation (.a and .mod files) into some folder, so
#  that we can then use them from a program
install: libeigenlib.a
	mkdir -p ../eigenlib_install/mod
	cp libeigenlib.a ../eigenlib_install/
	cp *.mod ../eigenlib_install/mod/

uninstall:
	rm -rf ../eigenlib_install


.PHONY: all clean install uninstall

