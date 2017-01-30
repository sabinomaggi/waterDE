!-------------------------------------------------------------------------------
module data_const
    implicit none

    integer(kind=4), parameter :: IB = 4
    integer(kind=4), parameter :: RP = 8

    integer(kind=4), parameter :: FILEMAX = 255
    integer(kind=4), parameter :: PATHMAX = 1024

    integer(kind=4), parameter :: BUFMAX  = 1024
    integer(kind=4), parameter :: MSGMAX  = 256
    integer(kind=4), parameter :: STRLEN = 32
    integer(kind=4), parameter :: NMODEL = 3
    integer(kind=4), parameter :: NOPTIM = 3

    real(kind=RP),   parameter :: PI = 3.141592654
    real(kind=RP),   parameter :: EXP1 = exp(1.0)

    logical :: EXTD = .false.
    logical :: FITH = .false.


end module data_const
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
module strings
    implicit none

!    character (2),  parameter  :: LF   = "\r\n"
!    character (2),  parameter  :: TAB  = "\t"
    character (2),  parameter  :: LF   = achar(13) // achar(10)
    character (2),  parameter  :: TAB  = achar(9)
    character (35), parameter  :: HR   = repeat("-", 35)

    character (2),  parameter  :: CMNT   = "# "
!     character (2),  parameter  :: H1     = "# "
!     character (5),  parameter  :: H4     = "#### "
!     character (6),  parameter  :: H5     = "##### "
    character (2),  parameter  :: STRONG = "**"

    character (5),  parameter  :: XVAR = " psi "
    character (7),  parameter  :: YVAR = " theta "
    character (5),  parameter  :: ITER = " iter "
    character (9),  parameter  :: FITN = " fitness "


end module strings
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
module data_waterde
    use data_const
    implicit none

    integer(kind=IB), parameter :: iscreen = 6
    integer(kind=IB), parameter :: idata   = 7
    integer(kind=IB), parameter :: isave   = 8
    integer(kind=IB), parameter :: iconf   = 9
    integer(kind=IB), parameter :: ilog    = 10
    integer(kind=IB), parameter :: irnd    = 11

    character (FILEMAX) :: fdata    = ""
    character (FILEMAX) :: fsave    = ""
    character (FILEMAX) :: flog     = ""
    character (FILEMAX) :: fbench   = ""
    character (FILEMAX) :: basename = ""
    character (FILEMAX) :: fconf_default = ""
    character (FILEMAX) :: fconf_custom  = ""

    character (8), parameter :: fhelp = "help.txt"


! implemented models
    character (STRLEN) ::  model
    character (STRLEN), dimension(NMODEL) :: &
        model_list = [character (len(model_list)) :: &
        "Brooks-Corey", "Rossi-Nimmo", "Van_Genuchten"]

! implemented optimimizations
    character (STRLEN) ::  optim
    character (STRLEN), dimension(NOPTIM) :: &
        optim_list =  [character (len(optim_list)) :: &
        "de", "swarm", "other"]


! number of dimensions (not used for now)
   integer(kind=IB) :: nD

! maximum number of experimental data points
    integer(kind=IB), parameter :: n_max_dat = 10000
    integer(kind=IB) :: n_dat

    real(kind=RP) :: x_min
    real(kind=RP) :: x_max
    real(kind=RP) :: y_min
    real(kind=RP) :: y_max

    real(kind=RP) :: x0

! store experimental and calculated data
	real(kind=RP) :: xweight
    real(kind=RP), allocatable :: x_obs(:)
    real(kind=RP), allocatable :: y_obs(:)
    real(kind=RP), allocatable :: x_calc(:)
    real(kind=RP), allocatable :: y_calc(:)
    real(kind=RP), allocatable :: x_calc_extd(:)
    real(kind=RP), allocatable :: y_calc_extd(:)

! DE fit parameters
    integer(kind=IB), parameter :: n_max_model_params = 20
    integer(kind=IB) :: n_model_params
    integer(kind=IB) :: n_extd
    real(kind=RP), dimension(n_max_model_params) :: par_min
    real(kind=RP), dimension(n_max_model_params) :: par_max
    real(kind=RP), allocatable :: fitness_history(:)

    integer(kind=IB) :: save_every = 0

! seed of random number generator (default = 1)
	integer(kind=IB) :: seed = 1


end module data_waterde
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
module data_DE
    use data_const
    use data_waterde
    implicit none

    integer(kind=IB) :: NP
    integer(kind=IB) :: Dim_XC
    integer(kind=IB) :: itermax
    integer(kind=IB) :: strategy
    integer(kind=IB) :: refresh
    integer(kind=IB), dimension(3) :: method

    real(kind=RP) :: VTR
    real(kind=RP) :: CR_XC

    real(kind=RP) :: F_XC
    real(kind=RP) :: F_CR

    real(kind=RP), allocatable :: XCmin(:)
    real(kind=RP), allocatable :: XCmax(:)
    real(kind=RP), allocatable :: bestmem_XC(:)
    real(kind=RP), allocatable :: bestval_HIST(:)

    integer(kind=IB) :: nfeval
    real(kind=RP) :: bestval

end module data_DE
!-------------------------------------------------------------------------------
