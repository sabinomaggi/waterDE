!-------------------------------------------------------------------------------
subroutine find_data_limits(N, varx, vary)
    use data_const
    use data_waterde
    implicit none

    integer(kind=IB) :: N
    real(kind=RP), dimension(1:N) :: varx
    real(kind=RP), dimension(1:N) :: vary

    integer(kind=IB) :: i

! fix and normalize experimental data
! 	do i = 1, N
! 		if (varx(i) == 0.0) then
! 			varx(i) = 1.0E0
! 		endif
! 	enddo
!
! 	varx = log10(varx)

	x_min = varx( int(N/2) )
	x_max = varx( int(N/2) )

	y_min = vary( int(N/2) )
	y_max = vary( int(N/2) )

	do i = 1, N
		x_min = min(x_min, varx(i))
		x_max = max(x_max, varx(i))

		y_min = min(y_min, vary(i))
		y_max = max(y_max, vary(i))
	enddo

! 	varx = varx / x_max
! 	vary = vary / y_max

	x0 = 10**ceiling( log10(x_max) )


end subroutine find_data_limits
!-------------------------------------------------------------------------------

