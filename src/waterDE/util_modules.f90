module utils
contains

!-------------------------------------------------------------------------------
subroutine init()
    use data_const
	use strings
    use data_waterde
    implicit none

    integer(kind=IB), dimension(8) :: time
    logical :: ok
    character (MSGMAX) :: err_msg
    character (MSGMAX) :: date_and_time_string
    character (MSGMAX) :: text_line

    intrinsic date_and_time



! Check whether we need to save a benchmark file
	if (save_every > 0) then

! set benchmark filename (based on basename of data file)
		fbench = trim(basename) // "-DE_benchmark.out"
		inquire (file = trim(fbench), exist = ok)
		if ( ok ) then
			err_msg = "Error: default benchmark file already exists"
			call write_msg(err_msg)
			err_msg = "Rename or delete benchmark file and re-run"
			call write_msg(err_msg)
			stop
		endif
! and create an empty history file by 'touching' it (opening/closing the file
! without writing anything)
		open(isave, file=fbench, access = 'append', status = 'new')
		close(isave)
	endif


! read current date and time
    call date_and_time(values=time)

! write current date and time on screen and on log file
	write (date_and_time_string, fmt=10) time(1:3), time(5:7)
	text_line = date_and_time_string
	call write_msg(text_line)


10  format('Program start: ', I4.4, '-', I2.2,'-', I2.2, ' ', I2.2,':',I2.2,':',I2.2)


end subroutine init
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine exit()
    use data_const
	use strings
    use data_waterde
    implicit none

    integer(kind=IB), dimension(8) :: time
    character (MSGMAX) :: date_and_time_string
    character (MSGMAX) :: text_line

    intrinsic date_and_time


! read current date and time
    call date_and_time(values=time)

! write current date and time on screen and on log file
	write (date_and_time_string, fmt=20) time(1:3), time(5:7)
	text_line = date_and_time_string
	call write_msg(text_line)


20  format('Program end: ', I4.4, '-', I2.2,'-', I2.2, ' ', I2.2,':',I2.2,':',I2.2)


end subroutine exit
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
subroutine init_random(seed)
	use data_const
	implicit none

	integer(kind=IB) :: seed

	integer(kind=IB) :: seed_size
	integer(kind=IB), allocatable :: seed_init(:)


!	initialize with system generated seed
	call random_seed()

!	find out dimension of seed array
	call random_seed(size = seed_size)

	allocate(seed_init(1:seed_size))

!	initialize seed array
	seed_init = seed
	call random_seed(put = seed_init)

	deallocate(seed_init)

end subroutine init_random
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
subroutine lower(str)
    use data_const
    implicit none

	character (STRLEN) :: str			! string to be converted

	integer(kind=IB) :: i
	integer(kind=IB) :: diffchar


! find ASCII position difference between 'A' and 'a'
	diffchar = iachar('a') - iachar('A')

	do i = 1, len_trim(str)
		if ( lge(str(i:i), "A") .AND. lle(str(i:i), "Z") ) then
			str(i:i) = achar(iachar(str(i:i)) + diffchar)
		endif
	enddo


end subroutine lower
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine sort_data(N, x, y)
    use data_const
    implicit none

	integer(kind=IB) :: N
	real(kind=RP) :: x(:)
    real(kind=RP) :: y(:)

	integer(kind=IB) :: i, j
    real(kind=RP) :: xtemp, ytemp


! loop through every element to compare it against every other element
! the first loop increments i, which is the starting point for the comparison
	do i = 1, N
! the second loop decrements j, which is the ending point for the comparison
		do j = N, i+1, -1
			if ( x(j) < x(i) ) then
! swap the array elements here
				xtemp = x(j)
				ytemp = y(j)

				x(j) = x(i)
				y(j) = y(i)

				x(i) = xtemp
				y(i) = ytemp
			end if
		end do
	end do


end subroutine sort_data
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine positive(x)
    use data_const
    implicit none

	real(kind=RP) :: x(:)

	x = abs(x)

end subroutine positive
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine negative(x)
    use data_const
    implicit none

	real(kind=RP) :: x(:)

	x = - abs(x)

end subroutine negative
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine alloc_1d_array(dim, array)
    use data_const
    use data_waterde
    implicit none

    integer(kind=IB) :: dim
    real(kind=RP), allocatable :: array(:)

	integer :: ierr
    character (MSGMAX) :: err_msg


    if (.NOT. allocated(array)) allocate(array(1:dim), stat=ierr)

    if(ierr /= 0) then
		err_msg = "Error: 1d array allocation error"
		call write_msg(err_msg)
        stop
    endif


end subroutine alloc_1d_array
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
subroutine alloc_1d_complex_array(dim, array)
    use data_const
    use data_waterde
    implicit none

    integer(kind=IB) :: dim
    double complex, allocatable :: array(:)

	integer :: ierr
    character (MSGMAX) :: err_msg


    if (.NOT. allocated(array)) allocate(array(1:dim), stat=ierr)

    if(ierr /= 0) then
		err_msg = "Error: complex 1d array allocation error"
		call write_msg(err_msg)
        stop
    endif


end subroutine alloc_1d_complex_array
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
subroutine alloc_2d_array(dim, array)
    use data_const
    use data_waterde
    implicit none

    integer(kind=IB) :: dim
    real(kind=RP), allocatable :: array(:,:)

	integer :: ierr
    character (MSGMAX) :: err_msg


    if (.NOT. allocated(array)) allocate(array(1:dim, 2), stat=ierr)

    if(ierr /= 0) then
		err_msg = "Error: 2d array allocation error"
		call write_msg(err_msg)
        stop
    endif


end subroutine alloc_2d_array
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
subroutine dealloc_1d_array(array)
    use data_const
    use data_waterde
    implicit none

    real(kind=RP), allocatable :: array(:)

	integer :: ierr
    character (MSGMAX) :: err_msg


    if (allocated(array)) deallocate(array, stat=ierr)

    if(ierr /= 0) then
		err_msg = "Error: 1d array deallocation error"
		call write_msg(err_msg)
        stop
    endif


end subroutine dealloc_1d_array
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
subroutine dealloc_1d_complex_array(array)
    use data_const
    use data_waterde
    implicit none

    double complex, allocatable :: array(:)

	integer :: ierr
    character (MSGMAX) :: err_msg


    if (allocated(array)) deallocate(array, stat=ierr)

    if(ierr /= 0) then
		err_msg = "Error: complex 1d array deallocation error"
		call write_msg(err_msg)
        stop
    endif


end subroutine dealloc_1d_complex_array
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine dealloc_2d_array(array)
    use data_const
    use data_waterde
    implicit none

    real(kind=RP), allocatable :: array(:,:)

	integer :: ierr
    character (MSGMAX) :: err_msg


    if (allocated(array)) deallocate(array, stat=ierr)

    if(ierr /= 0) then
		err_msg = "Error: 2d array allocation error"
		call write_msg(err_msg)
        stop
    endif

end subroutine dealloc_2d_array
!-------------------------------------------------------------------------------

!!-------------------------------------------------------------------------------
!function loc(i, j , lda)
!     use data_const
!     use data_waterde
!     implicit none
!
!     integer :: i, j, lda        ! lda = leading dimension of array
!
!     integer :: loc
!
!
!     loc = j*lda + i
!
!
! end function loc
!!-------------------------------------------------------------------------------

!!-------------------------------------------------------------------------------
!subroutine map_2d_to_1d(array2d, array1d)
!    use data_const
!    use data_waterde
!    implicit none
!
!    real(kind=RP), allocatable :: array1d(:,:)
!    real(kind=RP), allocatable :: array2d(:,:,:)
!
!    integer :: n, m
!
!
!    array1d = reshape(array2d, shape(array1d))
!
!
!end subroutine map_2d_to_1d
!!-------------------------------------------------------------------------------

!!-------------------------------------------------------------------------------
!subroutine map_1d_to_2d(array1d, array2d)
!    use data_const
!    use data_waterde
!    implicit none
!
!    real(kind=RP), allocatable :: array1d(:,:)
!    real(kind=RP), allocatable :: array2d(:,:,:)
!
!    integer :: n, m
!
!
!    array2d = reshape(array1d, shape(array2d))
!
!
!end subroutine map_1d_to_2d
!!-------------------------------------------------------------------------------


end module
