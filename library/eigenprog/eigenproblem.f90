program eigenproblem

! Solve the eigenproblem for the pauli-x matrix

  use eigenlib ! import everything from eigenlib

  implicit none

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


end program eigenproblem
