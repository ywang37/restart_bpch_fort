module regrid_mod

 implicit none

 private

 public :: regrid_restart

 contains

!
!------------------------------------------------------------------------------
!
 subroutine regrid_restart(input_opt, in_grid_opt, out_grid_opt, &
         in_restart_state, out_restart_state)

 use error_mod,               only : error_stop
 use grid_opt_mod
 use input_opt_mod,           only : OptInput
 use state_restart_mod,       only : RestStat

 implicit none

 type(OptInput),    intent(in)    :: input_opt
 type(OptGrid),     intent(in)    :: in_grid_opt
 type(OptGrid),     intent(in)    :: out_grid_opt
 type(RestStat),    intent(in)    :: in_restart_state
 type(RestStat),    intent(out)   :: out_restart_state

 ! local variables
 real*8,    allocatable :: in_tmp_h(:,:)
 real*8,    allocatable :: out_tmp_all(:,:,:,:)
 integer                :: in_iipar, in_jjpar, in_llpar
 integer                :: in_iipar_h, in_jjpar_h
 integer                :: out_iipar, out_jjpar, out_llpar
 integer                :: n_tracers
 integer                :: in_h_step, out_h_step

 real*8                 :: tmp

 integer                :: n, k
 integer                :: i0, j0, i1, j1, i2, j2

 ! get dims
 n_tracers = input_opt%n_tracers
 in_iipar  = in_grid_opt%iipar
 in_jjpar  = in_grid_opt%jjpar
 in_llpar  = in_grid_opt%llpar
 out_iipar = out_grid_opt%iipar
 out_jjpar = out_grid_opt%jjpar
 out_llpar = out_grid_opt%llpar

 ! tau
 out_restart_state%tau0 = in_restart_state%tau0
 out_restart_state%tau1 = in_restart_state%tau1

 if ( (trim(in_grid_opt%h_res) == res4) .and. &
     (trim(out_grid_opt%h_res) == res2) ) then

     ! in grid

     in_h_step = 4
     out_h_step = 2

     in_iipar_h = in_iipar * in_h_step
     in_jjpar_h = in_jjpar * in_h_step
     allocate( in_tmp_h(in_iipar_h,in_jjpar_h) )

     ! out tmp grid
     allocate( out_tmp_all(out_iipar, out_jjpar, in_llpar, n_tracers) )

     do n = 1, n_tracers, 1
         do k = 1, in_llpar, 1

             ! in_tmp_h
             do j0 = 1, in_jjpar, 1
                 do i0 = 1, in_iipar, 1

                     in_tmp_h( (i0-1) * in_h_step + 1 : i0 * in_h_step, &
                               (j0-1) * in_h_step + 1 : j0 * in_h_step) = & 
                                                 in_restart_state%stt(i0,j0,k,n)

                 end do
             end do

             ! out_tmp_all
             do j1 = 1, out_jjpar, 1
                 do i1 = 1, out_iipar, 1

                     if (i1 < out_iipar) then

                         tmp = sum( in_tmp_h( (i1-1) * out_h_step + 2 : i1 * out_h_step + 1, &
                                              (j1-1) * out_h_step + 2 : j1 * out_h_step + 1) )
                         tmp = tmp / (out_h_step * out_h_step)

                     else

                         tmp = sum( in_tmp_h( (i1-1) * out_h_step + 2 : i1 * out_h_step    , &
                                              (j1-1) * out_h_step + 2 : j1 * out_h_step + 1) )
                         tmp = tmp / out_h_step

                     end if

                     out_tmp_all(i1,j1,k,n) = tmp

                 end do
             end do


         end do
     end do

     if ( in_grid_opt%llpar == out_grid_opt%llpar ) then
         out_restart_state%stt = out_tmp_all
     else
         call error_stop(' vertical error', 'regrid_mod.f90: regrid_h')
     end if


 else
     call error_stop('error', 'regrid_mod.f90: regrid_h')
 end if

 deallocate( in_tmp_h    )
 deallocate( out_tmp_all )



 end subroutine regrid_restart
!
!------------------------------------------------------------------------------
!
end module regrid_mod
