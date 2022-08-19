module state_restart_mod

 implicit none
 private

 public :: set_state_restart
 public :: cleanup_state_restart

 type, public :: RestStat

     real*8,  allocatable     :: tracer(:,:,:)

 end type RestStat




 contains
!
!------------------------------------------------------------------------------
!
 subroutine set_state_restart( grid_opt, restart_state )

     use grid_opt_mod,         only : OptGrid

     implicit none

     type(OptGrid),     intent(in)    :: grid_opt
     type(RestStat),    intent(inout) :: restart_state


     allocate( restart_state%tracer(grid_opt%iipar, &
         grid_opt%jjpar, grid_opt%llpar) )


 end subroutine
!
!------------------------------------------------------------------------------
!
 subroutine cleanup_state_restart(restart_state)

     implicit none

     type(RestStat),    intent(inout) :: restart_state

     deallocate( restart_state%tracer )

 end subroutine
!
!------------------------------------------------------------------------------
!








end module state_restart_mod
