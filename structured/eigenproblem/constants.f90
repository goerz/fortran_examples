module constants_mod

  implicit none
  integer,   parameter :: idp   = kind(1.0d0)
  real(idp), parameter :: zero  = 0.0d0
  real(idp), parameter :: one   = 1.0d0
  real(idp), parameter :: two   = 2.0d0
  real(idp), parameter :: czero = cmplx(zero, kind=idp)
  real(idp), parameter :: cone  = cmplx(one, kind=idp)

end module 
