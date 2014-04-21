FC = gfortran
FFLAGS = -O3
LIBS = -llapack
LDFLAGS = -L/usr/lib

# How to make object files from Fortran 77 files.
%.o: %.f
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# How to make object files from Fortran 90 files.
%.o: %.f90
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

# How to make object files from Fortran 90 preprocessor files.
%.o: %.F90
	$(FC) $(FFLAGS) $(LDFLAGS) -c -o $@ $<

all: read_pulse

read_pulse: read_pulse.o
	$(FC) $(FFLAGS) -o $@ read_pulse.o $(LDFLAGS) $(LIBS)

clean:
	rm -f *.o
	rm -f read_pulse
	rm -rf *.out *.out2

.PHONY: all clean

