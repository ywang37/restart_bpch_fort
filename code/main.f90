program main

 use grid_opt_mod
 use input_mod,             only : read_input_file
 use input_opt_mod
 use time_mod,              only : SYSTEM_TIMESTAMP


 implicit none


 type(OptInput)          :: Input_Opt   ! Input Options object

 type(OptGrid)           :: in_grid_opt, out_grid_opt


 ! read input file
 call set_input_opt( Input_Opt )
 call read_input_file( Input_Opt )

 ! set grid
 call set_grid_opt( input_opt, in_grid_Opt,  'in'  )
 call set_grid_opt( input_opt, out_grid_Opt, 'out' )












 ! System time stamp
 WRITE( 6, 99  ) SYSTEM_TIMESTAMP()
 99 FORMAT( /, 2x, '===>  END MACHINE TIME: ', a, '  <===', / )

end program main
