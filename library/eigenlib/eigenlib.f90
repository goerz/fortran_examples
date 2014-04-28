module eigenlib

  use eigenlib_constants_mod ! import ...
  use eigenlib_diag_mod      ! ... everything

  implicit none

  public ! Make everything public

  ! This module doesn't do anything except to publicly export everything from
  ! constants_mod and diag_mod. As a result, in a program it is sufficient to
  ! say
  !
  !    use eigenlib
  !
  ! as opposed to
  !
  !     use eigenlib_constants_mod
  !     use eigenlib_diag_mod
  !
  ! In a modern Fortran library, it is considered good style to have a single
  ! "main" module for a library like this one.

end module
