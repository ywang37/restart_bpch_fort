module state_restart_mod

 implicit none
 private

 public :: set_state_restart
 public :: cleanup_state_restart

 type, public :: RestStat

     real*8,  allocatable     :: stt(:,:,:,:)
     real*8                   :: tau0, tau1

 end type RestStat




 contains
!
!------------------------------------------------------------------------------
!
 subroutine set_state_restart( input_opt, grid_opt, restart_state )

     use input_opt_mod,        only : OptInput
     use grid_opt_mod,         only : OptGrid

     implicit none

     type(OptInput),    intent(in)    :: input_opt
     type(OptGrid),     intent(in)    :: grid_opt
     type(RestStat),    intent(inout) :: restart_state

     allocate( restart_state%stt(grid_opt%iipar, &
         grid_opt%jjpar, grid_opt%llpar, input_opt%n_tracers) )

 end subroutine
!
!------------------------------------------------------------------------------
!
 subroutine cleanup_state_restart( restart_state )

     implicit none

     type(RestStat),    intent(inout) :: restart_state

     deallocate( restart_state%stt )

 end subroutine
!
!------------------------------------------------------------------------------
!








end module state_restart_mod
