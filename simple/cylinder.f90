program cylinder

! Calculate the surface area of a cylinder.

  implicit none    ! Require all variables to be explicitly declared

  integer,   parameter :: idp = kind(1.0d0)
  real(idp), parameter :: two = 2.0d0
  real(idp), parameter :: pi  = 3.141592653589793d0

  integer            :: ierr
  real(idp)          :: radius, height, area
  logical            :: invalid_input
  character(len=255) :: msg


  interactive_loop: do

    ! Prompt the user for radius and height and read them.
    msg =  'Enter radius and height.'
    write(*,*) trim(msg)
    read(*,*, iostat=ierr) radius, height

    ! If radius and height could not be read from input, then cycle through the
    ! loop.
    invalid_input = (ierr /= 0)
    if (invalid_input) then
      write(*,*) 'Error, invalid input.'
      cycle interactive_loop
    end if

    area = two * pi * (radius*radius + radius*height)

    ! Write the input variables (radius, height) and output (area) to the
    ! screen.
    write(*,*) "radius = ", radius
    write(*,*) "height = ", height
    write(*,*) "area   = ", area

    exit interactive_loop

  end do interactive_loop


end program cylinder
