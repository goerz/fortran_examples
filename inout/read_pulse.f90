program read_pulse

  implicit none    ! Require all variables to be explicitly declared

  integer, parameter :: idp = kind(1.0d0)
  real(idp) :: t(1199)
  real(idp) :: x(1199)
  real(idp) :: y(1199)
  integer :: n
  real(idp), allocatable :: t2(:), x2(:), y2(:)
  character(len=2) :: comment
  integer :: i, error

  ! read in original pulse
  open(110, file='pulse.dat', action='READ')
  i = 1
  do
    read(110,*,iostat=error) t(i), x(i), y(i)
    if (error == 0) then
      i = i + 1
    end if
    if (error < 0) exit ! end of file
    if (i > size(t)) exit
  end do
  close(110)

  ! write it out
  open(110, file='pulse.out', action='WRITE')
  comment = "# "
  write(110, '(A2,I8," data rows")') comment, size(t)
  write(110, '(A2,A23,2A25)')                                                  &
  &    comment, "time [ns]", "Re(ampl) [MHz]", "Im(ampl) [MHz]"
  do i = 1, size(t)
    write(110, '(3ES25.16)') t(i), x(i), y(i)
  end do
  close(110)

  ! read it in again -- without knowing the size a-priori
  open(110, file='pulse.out', action='READ')
  read(110, *, iostat=error) comment, n
  if (error == 0) then
    allocate(t2(n))
    allocate(x2(n))
    allocate(y2(n))
  else
    write(*,*) "No size information in file"
    stop
  end if
  i = 1
  do
    read(110,*,iostat=error) t2(i), x2(i), y2(i)
    if (error == 0) then
      i = i + 1
    end if
    if (error < 0) exit ! end of file
    if (i > size(t)) exit
  end do
  close(110)

  ! write it out once more
  open(110, file='pulse.out2', action='WRITE')
  comment = "# "
  write(110, '(A2,I8," data rows")') comment, size(t)
  write(110, '(A2,A23,2A25)')                                                  &
  &    comment, "time [ns]", "Re(ampl) [MHz]", "Im(ampl) [MHz]"
  do i = 1, size(t)
    write(110, '(3ES25.16)') t2(i), x2(i), y2(i)
  end do
  close(110)

  ! clean up
  deallocate(t2, x2, y2)

end program read_pulse
