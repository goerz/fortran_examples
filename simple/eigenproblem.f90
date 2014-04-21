program eigenproblem

! Solve the eigenproblem for the pauli-x matrix

  implicit none    ! Require all variables to be explicitly declared

  integer,   parameter :: idp   = kind(1.0d0)
  real(idp), parameter :: zero  = 0.0d0
  real(idp), parameter :: one   = 1.0d0
  real(idp), parameter :: two   = 2.0d0
  real(idp), parameter :: czero = cmplx(zero, kind=idp)
  real(idp), parameter :: cone  = cmplx(one, kind=idp)


  ! Eigensystem of a complex Hermitian matrix
  !
  ! Components
  ! eigen_vecs  Matrix that contains n eigenvectors in its columns
  ! eigen_vals  Array of eigenvalues
  ! spec_rad    Spectral radius: max eigenvalue - min eigenvalue
  ! n           Dimension of system
  type eigensys_t
    real(idp), allocatable :: eigen_vecs(:,:)
    real(idp), allocatable :: eigen_vals(:)
    real(idp)              :: spec_rad
    integer                :: n
  end type eigensys_t


  complex(idp)     :: pauli_x(2,2)
  type(eigensys_t) :: eigensystem
  integer          :: i

  ! Initialize Pauli-X matrix
  pauli_x = czero
  pauli_x(1,2) = cone
  pauli_x(2,1) = cone
  pauli_x = pauli_x / sqrt(two)

  call get_eigensystem(pauli_x, eigensystem)

  ! Write result to the screen
  write(*,*) "Eigenvalues: ", eigensystem%eigen_vals
  do i = 1, eigensystem%n
    write(*,'(" Eigenvector ",I1,": ")') i
    write(*,*) eigensystem%eigen_vecs(:,i)
  end do
  write(*,*) "Spectral Radius:", eigensystem%spec_rad

  ! Cleanup
  deallocate(eigensystem%eigen_vecs, eigensystem%eigen_vals)


contains


  ! Calculate the eigensystem for the given matrix
  !
  ! Parameters:
  ! matrix       Complex Hermitian Matrix
  ! eigensystem  On output, eigensystem of the matrix
  subroutine get_eigensystem(matrix, eigensystem)

    complex(idp),     intent(in)    :: matrix(:,:)
    type(eigensys_t), intent(inout) :: eigensystem

    integer :: n
    complex(idp), allocatable :: eigen_vecs(:,:)
    real(idp) :: emin, emax

    n = size(matrix, 1)
    eigensystem%n = n
    allocate(eigen_vecs(n,n)) ! we need a *complex* temporary matrix
    allocate(eigensystem%eigen_vecs(n,n))

    ! diagonalize
    eigen_vecs = matrix
    call diag_hermitian_matrix(eigen_vecs, eigensystem%eigen_vals)
    eigensystem%eigen_vecs = real(eigen_vecs, idp)

    ! spectral radius
    emin = minval(eigensystem%eigen_vals)
    emax = maxval(eigensystem%eigen_vals)
    eigensystem%spec_rad = emax - emin

    ! cleanup
    deallocate(eigen_vecs)

  end subroutine get_eigensystem


  !! @description: Diagonalize the given complex Hermitian matrix via a call to
  !!               the Lapack routine `zheevd`. The calculated eigenvectors are
  !!               saved in the columns of the matrix.
  !! @param: eigen_vecs  Matrix that should be diagonalized, will be replaced
  !!                     by matrix of (complex) eigenvectors
  !! @param: eigen_vals  Array of (real) eigenvalues of the matrix
  subroutine diag_hermitian_matrix(eigen_vecs, eigen_vals)

    complex(idp),              intent(inout) :: eigen_vecs(:,:)
    real(idp),    allocatable, intent(inout) :: eigen_vals(:)

    integer :: nn, lwork, lrwork, liwork, error
    integer ,  allocatable :: iwork(:)
    real(idp), allocatable :: rwork(:)
    complex(idp), allocatable :: work(:)

    nn = size(eigen_vecs(:,1))

    if (allocated(eigen_vals)) deallocate(eigen_vals)
    allocate(eigen_vals(nn))
    allocate (work(1))
    allocate (iwork(1))
    allocate (rwork(1))

    ! Perform workspace query: zheevd only calculates the optimal sizes of the
    ! WORK, RWORK and IWORK arrays, returns these values as the first entries of
    ! the WORK, RWORK and IWORK arrays,
    ! cf. http://www.netlib.org/lapack/complex16/zheevd.f
    lwork = -1; liwork = -1; lrwork = -1 ! indicate workspace query
    call zheevd('v', 'u', nn, eigen_vecs, nn, eigen_vals,                      &
    &           work, lwork, rwork, lrwork, iwork, liwork, error)
    if (error /= 0) then
      write (*,*) "Could not calculate optimal sizes for work arrays!"
      stop
    end if

    ! Now we can re-allocate WORK, IWORK, RWORK with the optimal size, obtained
    ! from the first call to zheevd
    lwork = work(1)
    liwork = iwork(1)
    lrwork = rwork(1)
    deallocate(work, iwork, rwork)
    allocate(work(lwork))
    allocate(iwork(liwork))
    allocate(rwork(lrwork))

    ! The second call to zheevd performs the actual diagonalization
    call zheevd('v', 'u', nn, eigen_vecs, nn, eigen_vals,                      &
    &           work, lwork, rwork, lrwork, iwork, liwork, error)
    if (error /= 0) then
      write(*,*) "An argument of zheevd had an illegal entry!"
      stop
    end if

    deallocate(work, iwork, rwork)

  end subroutine diag_hermitian_matrix


end program eigenproblem
