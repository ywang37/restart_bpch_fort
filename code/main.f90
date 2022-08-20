program main

 use grid_opt_mod
 use input_mod,             only : read_input_file
 use input_opt_mod
 use regrid_mod,            only : regrid_restart
 use restart_mod,           only : read_restart_file, make_restart_file
 use state_restart_mod
 use time_mod,              only : SYSTEM_TIMESTAMP


 implicit none


 type(OptInput)          :: Input_Opt   ! Input Options object

 type(OptGrid)           :: in_grid_opt, out_grid_opt

 type(RestStat)          :: in_restart_state, out_restart_state


 ! read input file
 call set_input_opt( Input_Opt )
 call read_input_file( Input_Opt )

 ! set grid
 call set_grid_opt( input_opt, in_grid_Opt,  'in'  )
 call set_grid_opt( input_opt, out_grid_Opt, 'out' )


 ! set restart
 call set_state_restart( input_opt, in_grid_opt,  in_restart_state  )
 call set_state_restart( input_opt, out_grid_opt, out_restart_state )
 print*, 'aaa'
 print*, allocated(in_restart_state%stt)
 print*, allocated(out_restart_state%stt)
 print*, 'bbb'

 ! read restart
 call read_restart_file( input_opt, in_restart_state )

  print*, '111'
  print*, allocated(in_restart_state%stt)
  print*, allocated(out_restart_state%stt)
  print*, '222'

 ! regrid
 call regrid_restart( input_opt, in_grid_opt, out_grid_opt, &
     in_restart_state, out_restart_state )

 print*, '3333'
 print*, allocated(in_restart_state%stt)
 print*, allocated(out_restart_state%stt)
 print*, '4444'

 ! make restart
 call make_restart_file( input_opt, out_restart_state, out_grid_opt )

 ! cleanup
 print*, 'ccc'
 print*, allocated(in_restart_state%stt)
 print*, allocated(out_restart_state%stt)
 print*, 'ddd'
 call cleanup_state_restart( in_restart_state  )
 call cleanup_state_restart( out_restart_state )

 ! System time stamp
 WRITE( 6, 99  ) SYSTEM_TIMESTAMP()
 99 FORMAT( /, 2x, '===>  END MACHINE TIME: ', a, '  <===', / )

end program main
