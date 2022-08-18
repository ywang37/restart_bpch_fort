module input_opt_mod

 implicit none
 private

 public :: set_input_opt
 public :: cleanup_input_opt

 type, public :: OptInput

     !--------------
     ! CONTROL MENU
     !--------------
     integer                  :: nymd, nhms
     ! out_rst_f is not in control menu, but is related to in_rst_f
     character(len=255)       :: in_rst_f, out_rst_f 
     character(len=255)       :: run_dir

     !----------------
     ! INPUT GRID MENU
     !----------------
     character(len=255)       :: in_h_res
     integer                  :: in_n_layer

     !-----------------
     ! OUTPUT GRID MENU
     !-----------------
     character(len=255)       :: out_h_res
     real*8                   :: out_lon_min, out_lon_max
     real*8                   :: out_lat_min, out_lat_max
     logical                  :: out_half_polar
     integer                  :: out_n_layer
     logical                  :: out_is_nested

 end type OptInput


 contains

 subroutine set_input_opt( Input_Opt )

     type(OptInput), intent(inout) :: Input_Opt

     Input_Opt%in_rst_f  = ''
     Input_Opt%out_rst_f = ''
     Input_Opt%run_dir   = ''


 end subroutine set_input_opt

 subroutine cleanup_input_opt
 end subroutine cleanup_input_opt

end module input_opt_mod
