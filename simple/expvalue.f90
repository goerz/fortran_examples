program expvalue

! Calculate the expectation value of the pauli-x matrix, for a random vector

  implicit none    ! Require all variables to be explicitly declared

  integer,      parameter :: idp   = kind(1.0d0)
  real(idp),    parameter :: zero  = 0.0d0
  real(idp),    parameter :: one   = 1.0d0
  real(idp),    parameter :: two   = 2.0d0
  complex(idp), parameter :: czero = cmplx(zero,      kind=idp)
  complex(idp), parameter :: cone  = cmplx(one,       kind=idp)
  complex(idp), parameter :: ci    = cmplx(zero, one, kind=idp)

  complex(idp), allocatable :: v(:), v_temp(:)
  complex(idp)              :: pauli_x(2,2)
  real(idp)                 :: result
  integer :: i, j

  ! Initialize Pauli-X matrix
  pauli_x = czero
  pauli_x(1,2) = cone
  pauli_x(2,1) = cone
  pauli_x = pauli_x / sqrt(two)

  ! Initialized vector
  allocate(v(2))
  call rand_vec(v)

  ! Calculate expectation value
  allocate(v_temp(2))
  ! 1. Calculate v_temp = pauli_x.v
  do i = 1, 2
    v_temp(i) = czero
    do j = 1, 2
      v_temp(i) = v_temp(i) + pauli_x(i,j) * v(j)
    end do
  end do
  ! 2. Calculate inner product (v, v_temp)
  result = zero
  do i = 1, 2
    result = result + real(conjg(v(i))*v_temp(i), idp)
  end do

  ! Write result to the screen
  write(*,*) "v = ", v
  write(*,*) "expectation value of pauli_x: ", result
  write(*,*) sum(conjg(v)*v_temp)

  ! Cleanup
  deallocate(v, v_temp)


contains


  ! Calculate a normalized random complex vector
  !
  ! Paramters:
  ! v  Complex vector of arbitrary length. Will be overwritten with random
  !    values and normalized
  subroutine rand_vec(v)

    complex(idp), intent(inout) :: v(:)

    integer :: i, n
    real(idp) :: x, y

    n = size(v)
    do i = 1, n
      call random_number(x)
      call random_number(y)
      v(i) = cmplx(x, y, kind=idp)
    end do
    v(:) = v(:) / norm(v) ! or simply "v = v/norm(v)"

  end subroutine rand_vec


  ! Calculate the norm of a complex vector
  !
  ! Parameters:
  ! v  Complex vector of arbitry length
  function norm(v)

    complex(idp), intent(in) :: v(:)
    real(idp)                :: norm

    integer :: i, n

    n = size(v)
    norm = zero
    do i = 1, n
      norm = norm + conjg(v(i)) * v(i)
    end do

  end function norm


end program expvalue
