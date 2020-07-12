.PHONY: clean

.DEFAULT_GOAL := arraypassing

arraypassing: arraypassing.f90
	gfortran -Og -g -Warray-temporaries -fcheck=array-temps -pedantic -fbacktrace -std=f2018 -o arraypassing arraypassing.f90

clean:
	@rm -f arraypassing
	@rm -rf *.dSYM/
