module eigenlib_constants_mod

  implicit none

  public :: idp, zero, one, two, czero, cone, ci
  private ! everything not pulic is private

  integer,      parameter :: idp   = kind(1.0d0)
  real(idp),    parameter :: zero  = 0.0d0
  real(idp),    parameter :: one   = 1.0d0
  real(idp),    parameter :: two   = 2.0d0
  complex(idp), parameter :: czero = cmplx(zero,      kind=idp)
  complex(idp), parameter :: cone  = cmplx(one,       kind=idp)
  complex(idp), parameter :: ci    = cmplx(zero, one, kind=idp)

end module
