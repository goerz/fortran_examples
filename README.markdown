Fortran Example Programs
========================

This repository contains some Fortran examples for the
[Introduction to Computational Physics][1], Summer 2014, at the University of
Kassel

    .
    ├── simple
    │   ├── cylinder.f90:          Illustrates simple data types, simple program
    │   ├── expvalue.f90:          Illustrates use of arrays
    │   │                          Also: subroutines/functions
    │   └── eigenproblem.f90:      Illustrates user-defined types,
    │                              use of external library routines
    ├── inout
    │   └── read_pulse.f90:        Illustrates how to read/write numerical data
    ├── structured
    │    └── eigenproblem:         Version of simple/eigenproblem, using modules
    │        ├── Makefile
    │        ├── constants.f90
    │        ├── eigenproblem.f90
    │        └── globals.f90
    └── library
        ├── eigenlib:              Library version of structured/eigenproblem
        │   ├── Makefile
        │   ├── constants.f90
        │   ├── diag.f90
        │   └── eigenlib.f90:      Main library module
        └── eigenprog              Example program using eigenlib
            ├── Makefile
            └── eigenproblem.f90

[1]: http://www.uni-kassel.de/fb10/institute/physik/forschungsgruppen/quantendynamik-und-kontrolle/lehre/computational-physics-ss-2014.html
