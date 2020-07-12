program arraypassing

  ! This program demonstrates some of the details for the various ways of
  ! passing arrays.
  !
  ! Fortran has the following nomenclature regarding arrays:
  !
  ! rank:   the number of dimensions
  ! size:   total number of elements (product of the extents), queried by
  !         `size(array)`
  ! shape:  array of extents
  ! extent: number of elements in a particular dimension, queried by
  !         `size(array, dim)`
  ! bounds: The lowest and highest integers for indices in a given dimension,
  !         queried by `lbound(array, dim)` and `ubound(array, dim)`
  !
  ! The five possible specification for a dummy array are:
  !
  !    integer :: a(n)                ! An explicit shape array
  !    integer :: b(:)                ! An assumed shape array
  !    integer, allocatable :: c(:)   ! A deferred shape array
  !    integer :: d(*)                ! An assumed size array
  !    integer :: d(..)               ! An assumed rank array (F2018)
  !
  ! Compile this program as:
  !
  !    gfortran -Og -g -Warray-temporaries -fcheck=array-temps -pedantic -fbacktrace -std=f2018 arraypassing.f90
  !
  ! Or, `-std=f2008` for everything except the assumed-rank testing at the end.


  implicit none
  integer, allocatable :: a(:,:,:)

  call initialize_rank3(a, (/2, 2, 4/))

  write(*, '("")')
  write(*, '("******* Full Array Access *******")')
  write(*, '("")')
  write(*, '("Deferred-shape keeps the (0-based!) bounds from the actual argument")')
  call print_deferred_shape_rank3(a)
  write(*, '("")')
  write(*, '("Assumed-shape keeps only the extents, but not the bounds from the actual argument")')
  call print_assumed_shape_rank3(a)
  write(*, '("")')
  write(*, '("Explicit-shape implicitly reshapes, allowing to impose a new rank without copying")')
  call print_explicit_shape_rank2(a, 4, 4)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  write(*, '("")')
  write(*, '("")')
  write(*, '("******* Explicit-Shape Partial Array Access *******")')

  write(*, '("")')
  write(*,*) "Printing a truncated contiguous subset of a, via explicit-shape passing"
  call print_explicit_shape_rank2(a, 3, 3)

  ! Going out-of-bounds with an explicit-shape: *may* crash at runtime, but
  ! probably just prints garbage -- the compiler won't catch this at run time,
  ! even with bound-checking enabled.
  !call print_explicit_shape_rank2(a, 5, 5)

  write(*, '("")')
  write(*,*) "Printing a contiguous subset of a via explicit-shape"
  ! This should not create an array-temporary!
  call print_explicit_shape_rank2(a(:,:,1), 2, 2)

  write(*, '("")')
  write(*,*) "Printing a non-contiguous subset of a via explicit-shape passing creates an array-temporary"
  call print_explicit_shape_rank2(a(1,:,:), 2, 4)  ! creates array-temporary

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  write(*, '("")')
  write(*, '("")')
  write(*, '("******* Assumed-Shape Partial Array Access *******")')

  ! assumed-shape with a rank mismatch is a compile time error:
  !call print_assumed_shape_rank3(a(1,:,:))

  write(*, '("")')
  write(*,*) "Printing a contiguous subset of a via assumed-shape passing"
  ! This should not create an array-temporary!
  call print_assumed_shape_rank2(a(:,:,1))

  write(*, '("")')
  write(*,*) "Printing a non-contiguous subset of a via assumed-shape passing"
  ! Unlike explicit-shape, this should not create an array temporary
  call print_assumed_shape_rank2(a(1,:,:))


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  write(*, '("")')
  write(*, '("")')
  write(*, '("******* Assumed-Shape Partial Array Access (Contiguous) *******")')

  write(*, '("")')
  write(*,*) "Printing a contiguous subset of a via assumed-shape passing"
  ! This should not create an array-temporary!
  call print_assumed_shape_rank2_contiguous(a(:,:,1))

  write(*, '("")')
  write(*,*) "Printing a non-contiguous subset of a via assumed-shape passing creates an array-temporary"
  ! gfortran doesn't seem to emit a runtime-warning about this
  ! Cf. the assumed-shape passing without the 'contiguous' keyword which avoides
  ! the array-temporary
  call print_assumed_shape_rank2_contiguous(a(1,:,:))  ! creates array-temporary


  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  write(*, '("")')
  write(*, '("")')
  write(*, '("******* Assumed-Rank-Passing *******")')

  ! With assumed-rank passing, we can do almost nothing with the array (not even
  ! print its elements)

  write(*, '("")')
  write(*,*) "Passing the full 3-rank array"
  call print_assumed_rank(a)
  write(*,*) "Passing a contiguous 2-rank sub-array"
  call print_assumed_rank(a(:,:,1))
  write(*,*) "Passing a non-contiguous 2-rank sub-array"
  call print_assumed_rank(a(1,:,:))


contains


  subroutine initialize_rank3(a, a_shape)

    integer, allocatable, intent(inout) :: a(:,:,:)  ! deferred-shape
    integer, intent(in)                 :: a_shape(3)

    ! initialize to zero-based indices
    allocate(a(0:a_shape(1)-1, 0:a_shape(2)-1, 0:a_shape(3)-1))
    call initialize(a, size(a))

  end subroutine initialize_rank3


  subroutine initialize(a, n)

    integer, intent(in)    :: n
    integer, intent(inout) :: a(n)  ! explicit-shape

    integer :: i, val

    val = 1
    do i = 1, n
      a(i) = val
      val = val + 1
    end do

  end subroutine initialize


  subroutine print_explicit_shape_rank2(a, n, m)

    integer, intent(in)    :: n
    integer, intent(in)    :: m
    integer, intent(inout) :: a(n, m)  ! explicit-shape

    integer :: i, j
    integer :: shp(2)

    shp = shape(a)
    write(*,*) "Explicit shape", shp
    do j = 1, m
      do i = 1, n
        write(*, '("a(", I1, ",", I1, ") = ", I4)') i, j, a(i, j)
      end do
    end do

  end subroutine print_explicit_shape_rank2


  subroutine print_assumed_shape_rank2(a)

    integer, intent(in) :: a(:,:)  ! assumed-shape (discard bounds)

    integer :: m, n
    integer :: i, j
    integer :: shp(2)

    shp = shape(a)
    n = size(a, 1)
    m = size(a, 2)

    write(*,*) "Assumed shape", shp, "is_contiguous:", is_contiguous(a)
    do j = 1, m
      do i = 1, n
        write(*, '("a(", I1, ",", I1, ") = ", I4)') i, j, a(i, j)
      end do
    end do

  end subroutine print_assumed_shape_rank2


  subroutine print_assumed_shape_rank2_contiguous(a)
    ! identidal to print_assumed_shape_rank2 except for the 'contiguous'

    integer, contiguous, intent(in) :: a(:,:)  ! assumed-shape (discard bounds)

    integer :: m, n
    integer :: i, j
    integer :: shp(2)

    shp = shape(a)
    n = size(a, 1)
    m = size(a, 2)

    write(*,*) "Assumed shape", shp, "is_contiguous:", is_contiguous(a)
    do j = 1, m
      do i = 1, n
        write(*, '("a(", I1, ",", I1, ") = ", I4)') i, j, a(i, j)
      end do
    end do

  end subroutine print_assumed_shape_rank2_contiguous


  subroutine print_assumed_shape_rank3(a)

    integer, intent(in) :: a(:,:,:)  ! assumed-shape (discard bounds)

    integer :: l, m, n
    integer :: i, j, k
    integer :: shp(3)

    n = size(a, 1)
    m = size(a, 2)
    l = size(a, 3)
    shp = shape(a)

    write(*,*) "Assumed shape", shp
    do k = 1, l
      do j = 1, m
        do i = 1, n
          write(*, '("a(", I1, ",", I1, ",", I1, ") = ", I4)') i, j, k, a(i, j, k)
        end do
      end do
    end do

  end subroutine print_assumed_shape_rank3


  subroutine print_deferred_shape_rank3(a)

    integer, allocatable, intent(in) :: a(:,:,:)  ! deferred-shape (keep bounds)

    integer :: i, j, k
    integer :: shp(3)

    shp = shape(a)

    write(*,*) "Deferred shape", shp
    do k = lbound(a, 3), ubound(a, 3)
      do j = lbound(a, 2), ubound(a, 2)
        do i = lbound(a, 1), ubound(a, 1)
          write(*, '("a(", I1, ",", I1, ",", I1, ") = ", I4)') i, j, k, a(i, j, k)
        end do
      end do
    end do

  end subroutine print_deferred_shape_rank3


  subroutine print_assumed_rank(a)

    integer, intent(in) :: a(..)

    integer :: rnk
    integer, allocatable :: shp(:)

    rnk = rank(a)
    allocate(shp(rnk))
    shp = shape(a)

    write(*,*) "Assumed rank", rnk, "shape", shp

    deallocate(shp)

  end subroutine print_assumed_rank


end program arraypassing
