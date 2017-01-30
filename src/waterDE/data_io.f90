!-------------------------------------------------------------------------------
subroutine set_default_parameters()
	use data_const
	use data_waterde
	use data_DE
	implicit none


! parameter values (they can be changed only in source code here)
! 	n_max_dat = 10000
! 	n_max_model_params = 20

! general default variable values (in 'namelist &general')
	model = trim(model_list(3))
	optim = optim_list(1)

	nD = 1
	n_extd = 201
	par_min = 0.0
	par_max = 1.0

! de default variable values (in 'namelist &de')
	NP = 50
	itermax = 500
	strategy = 6
	refresh = 100
	method = (/0, 1, 0/)
	save_every = 100

!!	VTR   = -1.0E-4_RP
	VTR   = 1.0E-4_RP
	CR_XC = 0.5_RP

	F_XC = 0.8_RP
	F_CR = 0.8_RP


end subroutine set_default_parameters
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine read_commandline_options(data_file, conf_file_default, conf_file_custom, log_file)
	use data_const
	use strings
	use data_waterde
	use data_DE
	use utils
	use getopt_m
	implicit none

	character (FILEMAX) :: data_file
	character (FILEMAX) :: conf_file_default
	character (FILEMAX) :: conf_file_custom
	character (FILEMAX) :: log_file

	character :: ch
	type(option_s) :: opts(5)

	integer(kind=IB) :: ipos = 0
	integer(kind=IB) :: argc = 0
	character (MSGMAX) :: err_msg
	logical :: ok


! check if at least one (mandatory!) argument has been given on command line
! it should be either the help option or the name of the data file

	argc = command_argument_count()
	if ( argc /= 0) then
		call get_command_argument(argc, data_file)
	else
		err_msg = "Error: no data file specified on command line"
		call write_msg(err_msg)
		err_msg = "Check command line and re-run"
		call write_msg(err_msg)
		stop
	endif

! process other command line options (if present) and set custom config/log filenames
	opts(1) = option_s( "config",	.true.,  'c' )
	opts(2) = option_s( "log",		.true.,  'l' )
	opts(3) = option_s( "extdplot",	.false., 'e' )
	opts(4) = option_s( "fitness",	.false., 'f' )
	opts(5) = option_s( "help",		.false., 'h' )

	do
		select case( getopt( "c:l:efh", opts ))
			case( char(0))
				exit
			case( 'c' )
! assign name of custom configuration file (only if option is set)
				conf_file_custom = optarg
! and check if the custom configuration file exists (and stop if not)
				inquire (file = trim(conf_file_custom), exist = ok)
				if ( .not. ok ) then
					err_msg = "Error: custom configuration file does not exist"
					call write_msg(err_msg)
					err_msg = "Check custom configuration file and re-run"
					call write_msg(err_msg)
					stop
				endif
			case( 'l' )
				log_file = optarg
			case( 'e' )
				EXTD = .true.
			case( 'f' )
				FITH = .true.
			case( 'h' )
				call help_message()
				stop
			case default
! 				err_msg = "Error: wrong option"
! 				call write_msg(err_msg)
				err_msg = "Check command line and re-run"
				call write_msg(err_msg)
 				stop
		end select
	end do


! check if data file exists (and stop if not)
	inquire (file = trim(data_file), exist = ok)
	if ( .not. ok ) then
		err_msg = "Error: data file does not exist"
		call write_msg(err_msg)
		err_msg = "Check data file and re-run"
		call write_msg(err_msg)
		stop
	endif

! find basename of data_file (without extension)
	ipos = index(data_file, ".", .true.)
	if ( ipos /= 0 ) then
		basename = data_file(1:ipos-1)
	else
		basename = data_file
	endif


! set default config filename if not explicitly set on the command line
! and check if it exists (stop if not)
!!	if (conf_file == "") then
	conf_file_default = trim(basename) // ".conf"
	inquire (file = trim(conf_file_default), exist = ok)
	if ( .not. ok ) then
		err_msg = "Error: default configuration file does not exist"
		call write_msg(err_msg)
		err_msg = "Check default configuration file and re-run"
		call write_msg(err_msg)
		stop
	endif
!!	endif

! set log filename if not explicitly set on the command line
	if (log_file  == "") then
		log_file = trim(basename) // ".log"
	endif
! and check if it already exixts (stopping if it does)
	inquire (file = trim(log_file), exist = ok)
	if ( ok ) then
		err_msg = "Error: default log file already exists"
		call write_msg(err_msg)
		err_msg = "Rename or delete previous log file and re-run"
		call write_msg(err_msg)
		stop
	else
! the log file does not exist, yet, so create a new empty log file
! by 'touching' it (opening/closing the file without writing anything)
		open(ilog, file=flog, access = 'append', status = 'new')
		close(ilog)
	endif


end subroutine read_commandline_options
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine read_config_file(input_unit, conf_file_default, conf_file_custom)
	use data_const
	use data_waterde
	use data_DE
	use utils
	implicit none

	integer(kind=IB) :: input_unit
	character (FILEMAX) :: conf_file_default
	character (FILEMAX) :: conf_file_custom

	integer(kind=IB) :: ipos
	logical :: ok
	character (MSGMAX) :: err_msg
	character (STRLEN) :: int_to_string

	integer(kind=IB) :: dimensions = 1
	integer(kind=IB) :: n_parameters
	real(kind=RP) :: weight
	real(kind=RP), dimension(n_max_model_params) :: parameter_min = 0.0
	real(kind=RP), dimension(n_max_model_params) :: parameter_max = 0.0

	integer(kind=IB) :: population
	integer(kind=IB) :: generations
	integer(kind=IB) :: benchmark_save
	real(kind=RP) :: expected_fitness
	real(kind=RP) :: crossover_factor
	real(kind=RP) :: mutation_factor
	real(kind=RP) :: combined_factor


	namelist /general/ model, optim, dimensions, seed, &
					   weight, n_parameters, parameter_min, parameter_max

	namelist /de/ population, generations, strategy, refresh, method, &
					   expected_fitness, &
					   crossover_factor, mutation_factor, combined_factor, &
					   benchmark_save


! read default configuration file
	inquire (file = trim(conf_file_default), exist = ok)
	if ( ok ) then
		open(input_unit, file=trim(conf_file_default))
		read(input_unit, nml=general)
		read(input_unit, nml=de)
		close(input_unit)
	endif

! read custom configuration file
	inquire (file = trim(conf_file_custom), exist = ok)
	if ( ok ) then
		open(input_unit, file=trim(conf_file_custom))
		read(input_unit, nml=general)
		read(input_unit, nml=de)
		close(input_unit)
	endif


!-------------------------------------------------------------------------------
! Check seed value from conf file and:
!	1. if seed = 0, use value from /dev/urandom as random seed
!	2. if seed = 999, use value from system clock
!	3. in all other cases, use value read from conf file
!-------------------------------------------------------------------------------

	if (seed == 0) then
#if defined MACOSX || defined LINUX
		open(irnd, file = '/dev/urandom', access = 'stream', form = 'unformatted')
		read(irnd) seed
		close(irnd)

		seed = abs(seed)
		print *, " urandom seed:", seed
#endif
#if defined WINDOWS
		call system_clock(seed)
		print *," windows clock seed:", seed
#endif
	else if (seed == 999) then
		call system_clock(seed)
		print *," clock seed:", seed
	else
		seed = abs(seed)
		print *, " conf seed:", seed
	endif


!-------------------------------------------------------------------------------
! Fix model options
!-------------------------------------------------------------------------------

! convert option to lowercase
	call lower(model)

! substitute " " and "-" characters in string with an underscore
	ipos = 0
	ipos = scan(trim(model), "- ")
	if (ipos /= 0) then
		model(ipos:ipos) = "_"
	endif

! substitute model short form with equivalent long form
	if (trim(model) == "bc") model = "brooks_corey"
	if (trim(model) == "rn") model = "rossi_nimmo"
	if (trim(model) == "vg") model = "van_genuchten"

! select model
	if (trim(model) == "brooks_corey") then
		model = trim(model_list(1))
	else if (trim(model) == "rossi_nimmo") then
		model = trim(model_list(2))
	else if (trim(model) == "van_genuchten") then
		model = trim(model_list(3))
	else
		err_msg = "Error: wrong model"
		call write_msg(err_msg)
		err_msg = "Check configuration file and re-run"
		call write_msg(err_msg)
		stop
	endif


!-------------------------------------------------------------------------------
! Fix optimization options
!-------------------------------------------------------------------------------

! convert option to lowercase
	call lower(optim)

 	if (trim(optim) == "de" ) then
 		optim = optim_list(1)
 	else if (trim(optim) == "swarm" ) then
 		optim = optim_list(2)
 	else
		err_msg = "Error: wrong optimization"
		call write_msg(err_msg)
		err_msg = "Check configuration file and re-run"
		call write_msg(err_msg)
		stop
 	endif


!-------------------------------------------------------------------------------
! Check given number of model parameters
!-------------------------------------------------------------------------------

	if (n_parameters <= n_max_model_params) then
		n_model_params = n_parameters
	else
		write (int_to_string, '(I12)') n_max_model_params
		err_msg = "Error: too many fit parameters" // &
			  " (> " // trim(adjustl(int_to_string)) // ")"
		call write_msg(err_msg)
		err_msg = "Check configuration file and re-run"
		call write_msg(err_msg)
		stop
	endif


!-------------------------------------------------------------------------------
! Assign remaining local mnemonic configuration variables to global variables
!-------------------------------------------------------------------------------
	nD = dimensions
	xweight = weight
	par_min = parameter_min
	par_max = parameter_max

	NP = population
	itermax = generations
	VTR = expected_fitness
	CR_XC = crossover_factor
	F_XC = mutation_factor
	F_CR = combined_factor
	save_every = benchmark_save


end subroutine read_config_file
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine find_data_file_lenght(input_unit, filename, nlines, nskips)
	use data_const
	use data_waterde
	implicit none

	integer(kind=IB) :: input_unit
	character (FILEMAX) :: filename
	integer(kind=IB) :: nlines
	integer(kind=IB) :: nskips
	integer(kind=IB) :: ndat

	integer(kind=IB) :: stat
	character (BUFMAX) :: buffer = ""
	character (STRLEN) :: int_to_string
	character (MSGMAX) :: err_msg


	open(input_unit, file=trim(filename), status='old')

	do
		read(input_unit, '(A)', iostat=stat) buffer

! exit loop if EOF or errors
		if (stat /= 0) exit

! if line is not empty, increment line counter
		if (buffer /= "") then
			nlines = nlines + 1
		endif

! increment skip line counter if either "#" or "!" is the first character of the line,
! otherwise throw error if "#" (or "!") is not the first character of the line
		if (scan(buffer, "#!") == 1) then
			nskips = nskips + 1
		else if (scan(buffer, "#!") > 1) then
			err_msg = "Error: comments in data file start with a '#' or '!' character"
			call write_msg(err_msg)
			err_msg = "Check data file and re-run"
			call write_msg(err_msg)
		endif

	enddo

	close(input_unit)


	ndat = nlines - nskips
!!!	print *, "ndat: ", ndat
	if (ndat > n_max_dat) then
		write (int_to_string, '(I12)') n_max_dat
		err_msg = "Error: too many data points" // &
				  " (> " // trim(adjustl(int_to_string)) // ")"
		call write_msg(err_msg)
		err_msg = "Check data file and rerun"
		call write_msg(err_msg)
		stop
	endif


end subroutine find_data_file_lenght
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine read_data_file(input_unit, filename, N, nskips, varx, vary)
	use data_const
	use data_waterde
	implicit none

	integer(kind=IB) :: input_unit
	character (FILEMAX) :: filename
	integer(kind=IB) :: N
	integer(kind=IB) :: nskips
	real(kind=RP), dimension(1:N) :: varx
	real(kind=RP), dimension(1:N) :: vary

	integer(kind=IB) :: i
	character (1024) :: buffer = ""


	open(input_unit, file=trim(filename), status='old')
	do i = 1, N + nskips
   		if (i <= nskips) then
	   		read(input_unit, '(A)') buffer
		else
			read(input_unit, *) varx(i - nskips), vary(i - nskips)
		endif
	enddo
	close(input_unit)


end subroutine read_data_file
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine write_msg(text_line)
	use data_const
	use strings
	use data_waterde
	implicit none

	character (MSGMAX) :: text_line


	call write_var(iscreen, text_line)

	if ( flog /= "") then
		open(ilog, file=flog, access = 'append', status = 'old')
		call write_var(ilog, text_line)
		close(ilog)
	endif

end subroutine write_msg
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine write_var(output_unit, text_line)
	use data_const
	use data_waterde
	implicit none

	integer(kind=IB) :: output_unit
	character (MSGMAX) :: text_line


	write(unit=output_unit, fmt='(A)') trim(text_line)


end subroutine write_var
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine skip_line(output_unit)
	use data_const
	implicit none

	integer(kind=IB) :: output_unit


	write(unit=output_unit, fmt='(A)') ""


end subroutine skip_line
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine write_xy_table(output_unit, filename, header, N, varx, vary)
	use data_const
	use strings
	use data_waterde
	implicit none

	integer(kind=IB) :: output_unit
	character (FILEMAX) :: filename
	character (MSGMAX)  :: header
	integer :: N
	real(kind=RP), dimension(1:N) :: varx
	real(kind=RP), dimension(1:N) :: vary

	integer :: i
	character (STRLEN) :: real_to_string_x
	character (STRLEN) :: real_to_string_y
	character (MSGMAX) :: text_line


	if (trim(filename) == flog) then
		open(output_unit, file=trim(filename), access="append", status="old")
	else
		open(output_unit, file=trim(filename), status="new")
	endif

! print header
	text_line = CMNT // header
	call write_var(output_unit, text_line)

! print names of table variables and line separator
	text_line = CMNT // TAB // XVAR // TAB // TAB // YVAR
	call write_var(output_unit, text_line)

	text_line = CMNT // HR
	call write_var(output_unit, text_line)


! print xy table
	do i = 1, N
		write (real_to_string_x, '(ES18.9)') varx(i)
		write (real_to_string_y, '(ES18.9)') vary(i)

		text_line = trim(adjustl(real_to_string_x)) // TAB // &
					trim(adjustl(real_to_string_y))
		call write_var(output_unit, text_line)
	enddo

	call skip_line(output_unit)

	close(output_unit)


end subroutine write_xy_table
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
subroutine write_fitness_table(output_unit, filename, header, N, list)
	use data_const
	use strings
	use data_waterde
	implicit none

	integer(kind=IB) :: output_unit
	character (FILEMAX) :: filename
	character (MSGMAX)  :: header
	integer :: N
	real(kind=RP), dimension(1:N) :: list

	integer :: i
	character (STRLEN) :: int_to_string
	character (STRLEN) :: real_to_string
	character (MSGMAX) :: text_line


	if (trim(filename) == flog) then
		open(output_unit, file=trim(filename), access="append", status="old")
	else
		open(output_unit, file=trim(filename), status="new")
	endif

! print header
	text_line = CMNT // header
	call write_var(output_unit, text_line)

! print names of table variables and line separator
	text_line = CMNT // ITER // TAB // FITN
	call write_var(output_unit, text_line)

	text_line = CMNT // HR
	call write_var(output_unit, text_line)


! print fitness table
	do i = 1, N
		write (int_to_string, '(I9)') i
		write (real_to_string, '(ES18.9)') list(i)

		text_line = trim(adjustl(int_to_string)) // TAB // &
					trim(adjustl(real_to_string))
		call write_var(output_unit, text_line)
	enddo

	call skip_line(output_unit)

	close(output_unit)


end subroutine write_fitness_table
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine write_de_results(output_unit, filename, header)
	use data_const
	use strings
	use data_waterde
	use data_DE
	implicit none

	integer(kind=IB) :: output_unit
	character (FILEMAX) :: filename
	character (MSGMAX)  :: header

	integer(kind=IB) :: i
	character (STRLEN) :: num_to_string
	character (MSGMAX) :: text_line



	if (trim(filename) == flog) then
		open(output_unit, file=trim(filename), access="append", status="old")
	else
		open(output_unit, file=trim(filename), status="new")
	endif

	text_line = header
	call write_var(output_unit, text_line)

	call skip_line(output_unit)

	text_line = "Model: " // model
	call write_var(output_unit, text_line)

	call skip_line(output_unit)

	write (num_to_string, '(I9)') NP
	text_line = "Population size NP:        " // num_to_string
	call write_var(output_unit, text_line)

	write (num_to_string, '(I9)') nfeval
	text_line = "No. of function calls:     " // num_to_string
	call write_var(output_unit, text_line)

	write (num_to_string, '(3I4)') method(1:3)
	text_line = "Method:                     " // trim(num_to_string)
	call write_var(output_unit, text_line)

	write (num_to_string, '(F6.3)') F_XC
	text_line = "Mutation scaling factor F_XC: " // num_to_string
	call write_var(output_unit, text_line)

	write (num_to_string, '(F6.3)') CR_XC
	text_line = "Crossover factor CR_XC:       " // num_to_string
	call write_var(output_unit, text_line)

	write (num_to_string, '(F6.3)') F_CR
	text_line = "Combined factor F_CR:         " // num_to_string
	call write_var(output_unit, text_line)

	call skip_line(output_unit)

	write (num_to_string, '(ES16.6)') bestval
	text_line = "Best objective function:   " // num_to_string
	call write_var(output_unit, text_line)

	call skip_line(output_unit)

	text_line = "       b(i)   best value(i)"
	call write_var(output_unit, text_line)
	text_line = "------------------------------"
	call write_var(output_unit, text_line)

	do i=1, Dim_XC
		write (text_line, '(A8)') "b"
		write (num_to_string, '(I9)') i
		text_line = trim(text_line) // adjustl(num_to_string)
		write (num_to_string, '(ES17.6)') bestmem_XC(i)
		text_line = trim(text_line) // trim(num_to_string)
		call write_var(output_unit, text_line)
    end do

	call skip_line(output_unit)

	close(output_unit)


end subroutine write_de_results
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine write_de_history(iteration, npop, npar, population_params, population_fitness)
	use data_const
	use strings
	use data_waterde
	use data_DE
	implicit none

	integer(kind=IB) :: iteration
	integer(kind=IB) :: npop
	integer(kind=IB) :: npar
	real(kind=RP), dimension(npop, npar) :: population_params
	real(kind=RP), dimension(npop) :: population_fitness

	integer(kind=IB) :: i, k
	integer(kind=IB) :: output_unit
	character (STRLEN) :: nfields
	character (MSGMAX) :: fmt_header
	character (MSGMAX) :: fmt_list


	output_unit = isave
	open(output_unit, file=fbench, access = 'append', status = 'old')

	write (output_unit, '(A, I12)') "# iteration: ", iteration

	write (nfields, '(I2)') npar

	fmt_header = "(A, T4, A, T14, " // trim(adjustl(nfields)) // "(A, I2, A, 5X), 6X, A)"
	fmt_list   = "(I9, T12, "       // trim(adjustl(nfields)) // "(ES12.4), 6X, ES12.4)"

	write (output_unit, fmt_header) &
			CMNT, "pop. i", ( "par(", k, ")", k = 1, npar ), "pop. fitness"
	do i = 1, npop
		write (output_unit, fmt_list) &
				i, population_params(i,1:npar), population_fitness(i)
	enddo

	call skip_line(output_unit)

	close(output_unit)


end subroutine write_de_history
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
subroutine help_message()
	use data_const
	use data_waterde
	implicit none

	integer(kind=IB) :: stat
	character (BUFMAX) :: buffer = ""


! 	open(idata, file=fhelp)
!
! 	do
! 		read(idata, '(A)', iostat=stat) buffer
! 		if (stat /= 0) then
! 			exit
! 		else
! 			write(iscreen, '(A)') trim(buffer)
! 		endif
! 	enddo
!
! 	close(idata)

	write(iscreen, *) "usage: waterDE [-h] [-c CONFIG] [-l LOG] [-e] [-f] DATA"
	write(iscreen, *) ""
	write(iscreen, *) "mandatory argument:"
	write(iscreen, *) "  DATA                  input data filename"
	write(iscreen, *) ""
	write(iscreen, *) "optional arguments:"
	write(iscreen, *) "  -h, --help            show this help message and exit"
	write(iscreen, *) "  -c CONFIG, --config CONFIG"
	write(iscreen, *) "                        use common CONFIG configuration file"
	write(iscreen, *) "  -e, --extdplot        save extended output data file"
	write(iscreen, *) "  -f, --fitness         save fitness history"
	write(iscreen, *) "  -l LOG, --log LOG     save all output to LOG file"


end subroutine help_message
!-------------------------------------------------------------------------------
