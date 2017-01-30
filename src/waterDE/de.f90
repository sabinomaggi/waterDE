!-------------------------------------------------------------------------------
subroutine de(N, x, y)
	use data_const
	use strings
	use data_waterde
	use data_DE
	use utils
	implicit none

	integer(kind=IB) :: N
	real(kind=RP) :: x(:)
	real(kind=RP) :: y(:)

	integer(kind=IB) :: i
	character (MSGMAX)  :: header

	external ftn_de


! set main DE parameters
	Dim_XC = n_model_params

	call alloc_1d_array(Dim_XC, XCmin)
	call alloc_1d_array(Dim_XC, XCmax)
	call alloc_1d_array(Dim_XC, bestmem_XC)
	call alloc_1d_array(itermax, bestval_HIST)

	XCmin = par_min(1:n_model_params)
	XCmax = par_max(1:n_model_params)

	bestmem_XC = 0.0_RP

! run DE
	call DE_Fortran90(ftn_de, Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC, &
						CR_XC, strategy, refresh, isave, bestmem_XC, &
						bestval, nfeval, F_CR, method)


! print results
	fsave  = trim(basename) // "-DE_params.out"
	header = CMNT // "DE results"
	call write_de_results(isave, fsave, header)
	call write_de_results(ilog, flog, header)


! compute new fit function from best DE result
	select case(trim(model))
	case("Van_Genuchten")
		call van_genuchten(n_dat, x_calc, y_calc, Dim_XC, bestmem_XC)
	case("Brooks-Corey")
		call brooks_corey(n_dat, x_calc, y_calc, Dim_XC, bestmem_XC)
	case("Rossi-Nimmo")
		call rossi_nimmo(n_dat, x_calc, y_calc, Dim_XC, bestmem_XC)
	end select


! save fitness history
	if (FITH) then
		fitness_history = bestval_HIST
	endif
	
! compute extended plot data
	if (EXTD) then
		call calc_de_extended_table(n_extd, x_calc_extd, y_calc_extd)
	endif


! release memory
	call dealloc_1d_array(XCmin)
	call dealloc_1d_array(XCmax)
	call dealloc_1d_array(bestmem_XC)
	call dealloc_1d_array(bestval_HIST)


end subroutine de
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine calc_de_extended_table(N, x, y)
	use data_const
	use strings
	use data_waterde
	use data_DE
	use utils
	implicit none

	integer :: i
	integer :: N
	real(kind=RP) :: x(*)
	real(kind=RP) :: y(*)

	real(kind=RP) :: logxmax, logdx


	logxmax = ceiling(log10(x_max))
	logdx = logxmax / (N-1)

	do i = 1, N
		x(i) = 10**( (i-1) * logdx )
	enddo


! compute extended plot data from best DE result
	    select case(trim(model))
	    case("Van_Genuchten")
			call van_genuchten(N, x, y, Dim_XC, bestmem_XC)
	    case("Brooks-Corey")
			call brooks_corey(N, x, y, Dim_XC, bestmem_XC)
	    case("Rossi-Nimmo")
			call rossi_nimmo(N, x, y, Dim_XC, bestmem_XC)
		end select


end subroutine calc_de_extended_table
!-------------------------------------------------------------------------------
