!-------------------------------------------------------------------------------
program waterDE
	use data_const
	use strings
	use data_waterde
	use data_DE
	use utils
	implicit none

	integer(kind=IB) :: line_count = 0
	integer(kind=IB) :: line_skip  = 0

	character (MSGMAX) :: header


! set default calculation parameters
	call set_default_parameters()

! read options from commandline
	call read_commandline_options(fdata, fconf_default, fconf_custom, flog)

! read calculation parameters from config file(s)
	call read_config_file(iconf, fconf_default, fconf_custom)

! initialize program
	call init()

! read data file
	call find_data_file_lenght(idata, fdata, line_count, line_skip)
	n_dat = line_count - line_skip


! allocate main data arrays
	call alloc_1d_array(n_dat, x_obs)
	call alloc_1d_array(n_dat, y_obs)

! allocate arrays for initial computation
	call alloc_1d_array(n_dat, x_calc)
	call alloc_1d_array(n_dat, y_calc)


	call read_data_file(idata, fdata, n_dat, line_skip, x_obs, y_obs)
	call sort_data(n_dat, x_obs, y_obs)

	fsave  = trim(basename) // "-data.out"
	header = "Experimental data"
	call write_xy_table(isave, fsave, header, n_dat, x_obs, y_obs)
	call write_xy_table(ilog, flog, header, n_dat, x_obs, y_obs)


	call dealloc_1d_array(x_calc)
	call dealloc_1d_array(y_calc)


!-------------------------------------------------------------------------------
!   Optimimization by Differential Evolution (DE) method
!-------------------------------------------------------------------------------

! allocate arrays for DE
	call alloc_1d_array(n_dat, x_calc)
	call alloc_1d_array(n_dat, y_calc)
	call alloc_1d_array(n_extd, x_calc_extd)
	call alloc_1d_array(n_extd, y_calc_extd)
	call alloc_1d_array(itermax, fitness_history)


! find experimental data limits
	call find_data_limits(n_dat, x_obs, y_obs)


! initialize random number generator
	call init_random(seed)


! initialize arrays for DE

!	randomize x_calc (proportionally to weight given in configuration file)
!!SM: not sure it is even useful, needs further checks!
!!SM: better to leave it out for now
! 	call random_number(x_calc)
! 	x_calc = xweight * (2.0 * x_calc - 1.0)
! 	x_calc = x_obs + x_calc * x_obs

!	randomize y_calc only
	x_calc = x_obs
	call random_number(y_calc)
	y_calc = y_calc * y_max
!	y_calc = 0.0

	x_calc_extd = 0.0
	y_calc_extd = 0.0


! run DE optimimization with these experimental data
	call de(n_dat, x_calc, y_calc)


! save data files from DE optimization
	fsave  = trim(basename) // "-DE.out"
	header = trim(model) // " model fitted to data by Differential Evolution"
	call write_xy_table(isave, fsave, header, n_dat, x_calc, y_calc)
	call write_xy_table(ilog, flog, header, n_dat, x_calc, y_calc)


! save file with fitness history
	if (FITH) then
		fsave  = trim(basename) // "-DE_fitness.out"
		header = "DE fitness history"
		call write_fitness_table(isave, fsave, header, itermax, fitness_history)
		call write_fitness_table(ilog, flog, header, itermax, fitness_history)
	endif

! save file with extended plot data
	if (EXTD) then
		fsave  = trim(basename) // "-DE_extdplot.out"
		header = "Extended plot of the " // trim(model) // &
				 " model fitted to data by Differential Evolution"
		call write_xy_table(isave, fsave, header, n_extd, x_calc_extd, y_calc_extd)
		call write_xy_table(ilog, flog, header, n_extd, x_calc_extd, y_calc_extd)
	endif


! deallocate DE arrays
	call dealloc_1d_array(x_calc)
	call dealloc_1d_array(y_calc)
	call dealloc_1d_array(x_calc_extd)
	call dealloc_1d_array(y_calc_extd)
	call dealloc_1d_array(fitness_history)


!-------------------------------------------------------------------------------
! deallocate all remaining arrays to recover memory before exiting program
	call dealloc_1d_array(x_obs)
	call dealloc_1d_array(y_obs)

	call exit()

end program waterDE
!-------------------------------------------------------------------------------
