!-------------------------------------------------------------------------------
!    
! Differential Evolution for Optimal Control Problems
!
!-------------------------------------------------------------------------------
!  This Fortran 90 program translates from the original MATLAB version of
!  differential evolution (DE).
!  This FORTRAN 90 code has been tested on Compaq Visual Fortran v6.1 and
!  gfortran.
!
!  Any users new to the DE are encouraged to read the article of Storn and Price. 
!
!  Storn, R., and Price, K.V., (1996). Minimizing the real function of the 
!    ICEC'96 contest by differential evolution. IEEE conf. on Evolutionary 
!    Computation, 842-844.
!
!  This Fortran 90 program has been written by Dr. Feng-Sheng Wang
!  Department of Chemical Engineering, National Chung Cheng University,
!  Chia-Yi 621, Taiwan, e-mail: chmfsw@ccunix.ccu.edu.tw
!
!  Minor modifications by Sabino Maggi, CNR-IRSA, Bari, Italy
!
!-------------------------------------------------------------------------------
!  obj                : User provided subroutine for evaluting the objective function
!                     : subroutine obj(xc,fitness), where
!                     :     "xc" is the real decision parameter vector (input),
!                     :     "fitness" is the fitness value (output)
!
!  Dim_XC             : Dimension of the real decision parameters
!  XCmin(Dim_XC)      : Lower bound of the real decision parameters
!  XCmax(Dim_XC)      : Upper bound of the real decision parameters
!
!  VTR                : Expected fitness value to be reached
!
!  NP                 : Population size
!  itermax            : Maximum number of iterations
!
!  F_XC               : Mutation scaling factor for real (decision) parameters
!  CR_XC              : Crossover factor for real (decision) parameters
!
!  strategy           : Strategy of mutation operations used in HDE
!  refresh            : Intermediate output will be produced after 'refresh'
!                     : iterations.
!                     : No intermediate output will be produced if refresh < 1.
!  iwrite             : Fortran unit specifier for writing to an external data file
!
!  bestmem_XC(Dim_XC) : Best calculated real decision parameters.
!  bestval            : Best calculated objective function
!  nfeval             : Number of function calls
!
!  method(1)          : == 0 -> fixed mutation scaling factors  (F_XC)
!                     : == 1 -> random mutation scaling factors (F_XC = [0, 1])
!                     : == 2 -> random mutation scaling factors (F_XC = [-1, 1]) 
!
!  method(2)          : == 1 -> random combined factor (F_CR) used for strategy = 6
!                     :        in the mutation operation 
!                     : /= 1 -> fixed combined factor provided by the user

! method(3)           : == 1 -> save results in a data file
!                     : /= 1 -> terminal output only
!-------------------------------------------------------------------------------

subroutine DE_Fortran90(obj, Dim_XC, XCmin, XCmax, VTR, NP, itermax, F_XC, &
           CR_XC, strategy, refresh, iwrite, bestmem_XC, bestval, nfeval, &
		   F_CR, method)

	 use data_const, only : IB, RP
     use data_waterde, only : save_every
	 use data_DE, only : bestval_HIST
	 implicit none
	 integer(kind=IB), intent(in) :: NP, Dim_XC, itermax, strategy,   &
	                                 iwrite, refresh
     real(kind=RP), intent(in) :: VTR, CR_XC
	 real(kind=RP) :: F_XC, F_CR
	 real(kind=RP), dimension(Dim_XC), intent(in) :: XCmin, XCmax
     real(kind=RP), dimension(Dim_XC), intent(inout) :: bestmem_XC
	 real(kind=RP), intent(out) :: bestval
	 integer(kind=IB), intent(out) :: nfeval     
     real(kind=RP), dimension(NP,Dim_XC) :: pop_XC, bm_XC, mui_XC, mpo_XC,   &
	                                        popold_XC, rand_XC, ui_XC
     integer(kind=IB) :: i, ibest, iter
     integer(kind=IB), dimension(NP) :: rot, a1, a2, a3, a4, a5, rt
     integer(kind=IB), dimension(4) :: ind
     real(kind=RP) :: tempval
	 real(kind=RP), dimension(NP) :: val
     real(kind=RP), dimension(Dim_XC) :: bestmemit_XC
     real(kind=RP), dimension(Dim_XC) :: rand_C1
	 integer(kind=IB), dimension(3), intent(in) :: method
	 external  obj
	 intrinsic max, min, random_number, mod, abs, any, all, maxloc
	 interface
        function randperm(num)
		   use data_const, only : IB	
		   implicit none
	       integer(kind=IB), intent(in) :: num
           integer(kind=IB), dimension(num) :: randperm
        end function randperm
     end interface
 !!-----Initialize a population --------------------------------------------!!

        pop_XC=0.0_RP
        do i=1,NP
           call random_number(rand_C1)
           pop_XC(i,:)=XCmin+rand_C1*(XCmax-XCmin)
		end do
 
!!--------------------------------------------------------------------------!!

!!------Evaluate fitness functions and find the best member-----------------!!
     val=0.0_RP
     nfeval=0
     ibest=1
     call obj(pop_XC(1,:), val(1))
     bestval=val(1)
     nfeval=nfeval+1
     do i=2,NP
        call obj(pop_XC(i,:), val(i))
        nfeval=nfeval+1
        if (val(i) < bestval) then
           ibest=i
	       bestval=val(i)
        end if
     end do  	 
     bestmemit_XC=pop_XC(ibest,:)
     bestmem_XC=bestmemit_XC
!!--------------------------------------------------------------------------!!

     bm_XC=0.0_RP
     rot=(/(i,i=0,NP-1)/)
     iter=1 
!!------Perform evolutionary computation------------------------------------!! 

     do while (iter <= itermax)
        popold_XC=pop_XC

!!------Mutation operation--------------------------------------------------!!
        ind=randperm(4)
        a1=randperm(NP)
        rt=mod(rot+ind(1),NP)
        a2=a1(rt+1)
        rt=mod(rot+ind(2),NP)
        a3=a2(rt+1)
        rt=mod(rot+ind(3),NP)
        a4=a3(rt+1)
        rt=mod(rot+ind(4),NP)
        a5=a4(rt+1)
        bm_XC=spread(bestmemit_XC, DIM=1, NCOPIES=NP)

!----- Generating a random scaling factor--------------------------------!
		select case (method(1))
		case (1)
		   call random_number(F_XC)
		case(2)
		   call random_number(F_XC)
           F_XC=2.0_RP*F_XC-1.0_RP
		end select

!---- select a mutation strategy-----------------------------------------!
		select case (strategy)
        case (1)
           ui_XC=bm_XC+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))

	    case default
           ui_XC=popold_XC(a3,:)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))

	    case (3)
           ui_XC=popold_XC+F_XC*(bm_XC-popold_XC+popold_XC(a1,:)-popold_XC(a2,:))

	    case (4)
           ui_XC=bm_XC+F_XC*(popold_XC(a1,:)-popold_XC(a2,:)+popold_XC(a3,:)-popold_XC(a4,:))

		case (5)
		   ui_XC=popold_XC(a5,:)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:)+popold_XC(a3,:) &
		         -popold_XC(a4,:))
        case (6) ! A linear crossover combination of bm_XC and popold_XC
           if (method(2) == 1) call random_number(F_CR) 
		   ui_XC=popold_XC+F_CR*(bm_XC-popold_XC)+F_XC*(popold_XC(a1,:)-popold_XC(a2,:))

        end select
!!--------------------------------------------------------------------------!!
!!------Crossover operation-------------------------------------------------!!
        call random_number(rand_XC)
           mui_XC=0.0_RP
           mpo_XC=0.0_RP
        where (rand_XC < CR_XC)
           mui_XC=1.0_RP
!           mpo_XC=0.0_RP
        elsewhere
!           mui_XC=0.0_RP
           mpo_XC=1.0_RP
        end where

		ui_XC=popold_XC*mpo_XC+ui_XC*mui_XC
!!--------------------------------------------------------------------------!!
!!------Evaluate fitness functions and find the best member-----------------!!
        do i=1,NP
!!------Confine each of feasible individuals in the lower-upper bound-------!!
		   ui_XC(i,:)=max(min(ui_XC(i,:),XCmax),XCmin)
           call obj(ui_XC(i,:), tempval)
	       nfeval=nfeval+1
	       if (tempval < val(i)) then
	          pop_XC(i,:)=ui_XC(i,:)
	          val(i)=tempval
	          if (tempval < bestval) then
	             bestval=tempval
		         bestmem_XC=ui_XC(i,:)
              end if
           end if
        end do
        bestmemit_XC=bestmem_XC

!SM-start
		bestval_HIST(iter) = bestval
		if ( save_every > 0 .and. &
		(mod(iter, save_every) == 0 .or. iter == 1 .or. iter == itermax) ) then
			call write_de_history(iter, NP, Dim_XC, pop_XC, val)
		endif
!SM-end

	    if( (refresh > 0) .and. (mod(iter,refresh)==0)) then
   		     if (method(3)==1) write(unit=iwrite,FMT=203) iter
		     write(unit=*, FMT=203) iter	
 		     do i=1,Dim_XC
		         if (method(3)==1) write(unit=iwrite, FMT=202) i, bestmem_XC(i)
				 write(*,FMT=202) i,bestmem_XC(i)
             end do
			 if (method(3)==1) write(unit=iwrite, FMT=201) bestval
			 write(unit=*, FMT=201) bestval

        end if
        iter=iter+1
        if ( bestval <= VTR .and. refresh > 0) then
           write(unit=iwrite, FMT=*) ' The best fitness is smaller than VTR'
		   write(unit=*, FMT=*) 'The best fitness is smaller than VTR' 
           exit
        endif
	 end do
!!------end the evolutionary computation------------------------------!!
201 format(2x, 'bestval =', ES14.7, /)
202 format(5x, 'bestmem_XC(', I3, ') =', ES12.5)
203 format(2x, 'No. of iteration  =', I8)
end subroutine DE_Fortran90


function randperm(num)
  use data_const, only : IB, RP
  implicit none
  integer(kind=IB), intent(in) :: num
  integer(kind=IB) :: number, i, j, k
  integer(kind=IB), dimension(num) :: randperm
  real(kind=RP), dimension(num) :: rand2
  intrinsic random_number
  call random_number(rand2)
  do i=1,num
     number=1
     do j=1,num
        if (rand2(i) > rand2(j)) then
	       number=number+1
        end if
     end do
     do k=1,i-1
        if (rand2(i) <= rand2(k) .and. rand2(i) >= rand2(k)) then
	       number=number+1
        end if
     end do
     randperm(i)=number
  end do
  return
end function randperm
