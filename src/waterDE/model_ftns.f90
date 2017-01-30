!-------------------------------------------------------------------------------
subroutine van_genuchten(N, x, y, npar, par)
    use data_const
    use data_waterde
    use data_DE
    implicit none

	integer(kind=IB) :: N
    integer(kind=IB) :: npar
    real(kind=RP), dimension(1:N) :: x
    real(kind=RP), dimension(1:N) :: y
    real(kind=RP), dimension(1:npar), intent(in) :: par

	integer(kind=IB) :: h

    do h = 1, N
        y(h) = par(1) + (par(2) - par(1)) / &
			   ( 1 + (par(3) * abs(x(h)) )**par(4) )**(1.0 - 1.0/par(4))
    enddo


end subroutine van_genuchten
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine brooks_corey(N, x, y, npar, par)
    use data_const
    use data_waterde
    use data_DE
    implicit none

    integer(kind=IB) :: N
    integer(kind=IB) :: npar
    real(kind=RP), dimension(1:N) :: x
    real(kind=RP), dimension(1:N) :: y
    real(kind=RP), dimension(1:npar), intent(in) :: par

	integer(kind=IB) :: h


    do h = 1, N
		if ( x(h) <= par(3) ) then
			y(h) = par(1) + (par(2) - par(1))
		else
			y(h) = par(1) + (par(2) - par(1)) * ( par(3) / abs(x(h)) )**par(4)
		endif
    enddo


end subroutine brooks_corey
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine rossi_nimmo(N, x, y, npar, par)
    use data_const
    use data_waterde
    use data_DE
    implicit none

	integer(kind=IB) :: N
    integer(kind=IB) :: npar
    real(kind=RP), dimension(1:N) :: x
    real(kind=RP), dimension(1:N) :: y
    real(kind=RP), dimension(1:npar), intent(in) :: par

	integer(kind=IB) :: h
    real(kind=RP)    :: beta_prime, gamma_prime
    real(kind=RP)    :: x_i, x_j


	beta_prime = 0.5 * par(4) * ( 2.0 / ( 2.0 + par(4)) )**( (2 + par(4) ) / par(4) )
	gamma_prime = par(4) * EXP1 * ( par(3) / x0)**par(4)
	x_i = par(3) * ( 2.0 / ( 2.0 + par(4)) )**( -1.0 / par(4) )
	x_j = x0 * exp( -1.0 / par(4) )


	do h = 1, N
		if ( 0 <= x(h) .and. x(h) < x_i ) then
			y(h) = par(1) + (par(2) - par(1)) * ( 1 - beta_prime * ( x(h) / par(3))**2 )
		else if ( x_i <= x(h) .and. x(h) < x_j ) then
			y(h) = par(1) + (par(2) - par(1)) * ( par(3) / x(h) )**par(4)
		else if ( x_j <= x(h) .and. x(h) <= x0 ) then
			y(h) = par(1) + (par(2) - par(1)) * gamma_prime * log( (x0 / x(h)) )

		endif
	enddo


end subroutine rossi_nimmo
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
subroutine ftn_de(params, fitness)
    use data_const
    use data_waterde
    use data_DE
    implicit none

	real(kind=RP), dimension(1:Dim_XC), intent(in) :: params
	real(kind=RP), intent(out) :: fitness

    integer(kind=IB) :: h
    real(kind=RP) :: de, obs
    real(kind=RP) :: sum_obs


! The code below implements DE optimization
! choose model
    select case(trim(model))
    case("Van_Genuchten")
		call van_genuchten(n_dat, x_calc, y_calc, Dim_XC, params)
    case("Brooks-Corey")
		call brooks_corey(n_dat, x_calc, y_calc, Dim_XC, params)
    case("Rossi-Nimmo")
		call rossi_nimmo(n_dat, x_calc, y_calc, Dim_XC, params)
	end select


    sum_obs = 0.0
    do h = 1, n_dat
        sum_obs = sum_obs + abs(y_obs(h)**2)
    enddo

    fitness = 0.0
    do h = 1, n_dat
        de  = y_calc(h)
        obs = y_obs(h)
        fitness = fitness + (de - obs)**2
    enddo
	fitness = fitness / sum_obs

end subroutine ftn_de
!-------------------------------------------------------------------------------
