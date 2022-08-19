module grid_opt_mod

 implicit none
 private

 public :: set_grid_opt
 public :: cleanup_grid_opt


 ! 
 character(len=7), parameter   :: res4 = '4.0x5.0'
 character(len=7), parameter   :: res2 = '2.0x2.5'

 type, public :: OptGrid

     character(len=255)       :: h_res
     real*8                   :: lon_min, lon_max
     real*8                   :: lat_min, lat_max
     logical                  :: half_polar
     logical                  :: is_nested

     integer                  :: iipar, jjpar, llpar
     integer                  :: iglob, jglob, lglob

     real*8                   :: disize, djsize

 end type OptGrid
    
    

 contains
!
!------------------------------------------------------------------------------
!
 subroutine set_grid_opt( input_opt, grid_opt, grid_in_out )

     use error_mod,          only : error_stop
     use input_opt_mod,      only : OptInput

     implicit none

     type(OptInput),    intent(in)    :: input_opt
     type(OptGrid),     intent(inout) :: grid_opt
     character(len=*),  intent(in)    :: grid_in_out
     
     if ( grid_in_out == 'in' ) then

         grid_opt%h_res  = input_opt%in_h_res
         grid_opt%llpar  = input_opt%in_n_layer

         grid_opt%lon_min = -180.0
         grid_opt%lon_max =  180.0
         grid_opt%lat_min =  -90.0
         grid_opt%lat_max =   90.0

         grid_opt%half_polar = .true.
         grid_opt%is_nested  = .false.

     else if ( grid_in_out == 'out' ) then

         grid_opt%h_res = input_opt%out_h_res
         grid_opt%llpar = input_opt%out_n_layer

         grid_opt%lon_min = input_opt%out_lon_min
         grid_opt%lon_max = input_opt%out_lon_max
         grid_opt%lat_min = input_opt%out_lat_min
         grid_opt%lat_max = input_opt%out_lat_max

         grid_opt%half_polar = input_opt%out_half_polar
         grid_opt%is_nested  = input_opt%out_is_nested

     else
         call error_stop('in_out_error', 'grid_opt_mod: set_grid_opt')

     end if

     select case ( trim(grid_opt%h_res) )

         case (res4)

             grid_opt%iglob = 72
             grid_opt%jglob = 46
             grid_opt%lglob = 72

             grid_opt%disize = 5.0
             grid_opt%djsize = 4.0

         case (res2)

             grid_opt%iglob = 144
             grid_opt%jglob = 91
             grid_opt%lglob = 72

             grid_opt%disize = 2.5
             grid_opt%djsize = 2.0

         case default

             call error_stop('res_error', 'grid_opt_mod: set_grid_opt')

     end select

     ! global
     if ( .not. grid_opt%is_nested ) then

         grid_opt%iipar = grid_opt%iglob
         grid_opt%jjpar = grid_opt%jglob

     ! nested
     else
         call error_stop('nested_error', 'grid_opt_mod: set_grid_opt')
     end if

 end subroutine set_grid_opt
!
!------------------------------------------------------------------------------
!
 subroutine cleanup_grid_opt
 end subroutine cleanup_grid_opt
!
!------------------------------------------------------------------------------
!
    
end module grid_opt_mod
