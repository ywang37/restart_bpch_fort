MODULE input_mod
!
!******************************************************************************
! Module input_mod read input file (input.cwaod)
! (ywang, 04/20/2016)
!******************************************************************************
!
 IMPLICIT NONE

 ! Make everything PRIVATE...
 PRIVATE

 ! ... except these routines
 PUBLIC :: read_input_file

 !=========================================================
 ! MODULE Variables
 !=========================================================
 LOGICAL            :: VERBOSE  = .FALSE.
 INTEGER, PARAMETER :: FIRSTCOL = 26
 CHARACTER(LEN=255) :: filename = 'input.restart'
 CHARACTER(LEN=255) :: topTitle 
 INTEGER, PARAMETER :: MAXDIM   = 255
 INTEGER            :: IU_rt

 !=========================================================
 ! MODULE ROUTINES -- follow below the "CONTAINS" statement
 !=========================================================
 CONTAINS
!
!------------------------------------------------------------------------------
!
 SUBROUTINE read_input_file( Input_Opt )
!
!******************************************************************************
! Suboutine read_input_file is the driver program for reading input file
! (input.cwaod)
! (ywang, 04/20/2016)
!******************************************************************************
!
 ! Refernece to F90 modules
 USE CHARPAK_MOD, ONLY : STRREPL
 USE FILE_MOD,    ONLY : IOERROR
 USE inquireMod,  ONLY : findFreeLUN
 USE TIME_MOD,    ONLY : SYSTEM_TIMESTAMP
 use input_opt_mod, only : OptInput

 type(OptInput), intent(inout) :: Input_Opt

 ! Local variables
 LOGICAL            :: EOF
 INTEGER            :: IOS
 CHARACTER(LEN=1)   :: TAB   = ACHAR(9)
 CHARACTER(LEN=1)   :: SPACE = ' '
 CHARACTER(LEN=255) :: LINE

 ! System time stamp
 WRITE( 6, 99  ) SYSTEM_TIMESTAMP()
 99 FORMAT( /, 2x, '===>  START MACHINE TIME: ', a, '  <===', / )

 ! Echo output
 WRITE( 6, '(a  )' ) REPEAT( '=', 79 )
 WRITE( 6, 100     ) TRIM( FILENAME )
 100 FORMAT( 'READ_INPUT_FILE: Reading ', a )

 ! Find a free file LUN
 IU_rt = findFreeLUN()

 OPEN( IU_rt, FILE=TRIM(filename), STATUS='OLD', IOSTAT=IOS )
 IF ( IOS /= 0 ) CALL IOERROR( IOS, IU_rt,'read_input_file:1' )

 ! Read TOPTITLE for binary punch file
 TOPTITLE = READ_ONE_LINE( EOF  )
 IF ( EOF ) RETURN

 ! Loop until EOF
 DO

    ! Read a line from the file, exit if EOF
    LINE = READ_ONE_LINE( EOF )
    IF ( EOF ) EXIT

    ! Replace tab characters in LINE (if any) w/ spaces
    CALL STRREPL( LINE, TAB, SPACE )

    IF ( INDEX( LINE, 'CONTROL MENU'   ) > 0 ) THEN
        CALL READ_CONTROL_MENU( Input_Opt )

    ELSE IF ( INDEX( LINE, 'INPUT GRID MENU' ) > 0 ) THEN
        CALL read_input_grid_menu( Input_Opt )

    ELSE IF ( INDEX( LINE, 'OUTPUT GRID MENU'   ) > 0 ) THEN
        CALL read_output_grid_menu( Input_Opt )

    ELSE IF ( INDEX( LINE, 'END OF FILE'      ) > 0 ) THEN
        EXIT

    ENDIF

 ENDDO

 ! Close input file
 CLOSE( IU_rt )

 ! Echo output
 WRITE( 6, '(a)' ) REPEAT( '=', 79 )

 END SUBROUTINE read_input_file
!
!------------------------------------------------------------------------------
!
SUBROUTINE read_control_menu( Input_Opt )

 ! References to F90 modules
 use time_mod,      only : expand_date
 use input_opt_mod, only : OptInput

 type(OptInput), intent(inout) :: Input_Opt

 ! Local variables
 INTEGER            :: N
 CHARACTER(LEN=255) :: subStrs(MAXDIM)

 !=========================================================
 ! read_control_menu begins here
 !=========================================================

 CALL split_one_line( subStrs, N, 2, 'read_control_menu:1' )
 READ( subStrs(1:N), * ) Input_Opt%nymd, Input_Opt%nhms

 CALL split_one_line( subStrs, N, 1, 'read_control_menu:2' )
 READ( subStrs(1:N), '(A)' ) Input_Opt%in_rst_f

 CALL split_one_line( subStrs, N, 1, 'read_control_menu:3' )
 READ( subStrs(1:N), '(A)' ) Input_Opt%run_dir

 ! Separator line
 CALL split_one_line( subStrs, N, 1, 'read_control_menu:4' )

 !=========================================================
 ! Print to screen
 !=========================================================
 WRITE( 6, '(/,a)') '%%% CONTROL MENU %%%'
 WRITE( 6, '(  a)') repeat( '-', 48 )
 write( 6, 101    ) 'YYYYMMDD, hhmmss        : ', Input_Opt%nymd, Input_Opt%nhms
 WRITE( 6, 100    ) 'Input restart file      : ', trim(Input_Opt%in_rst_f)
 write( 6, 100    ) 'Run directory           : ', trim(Input_Opt%run_dir)

 100 FORMAT( a, a )
 101 format( a, i8.8, 1x, i6.6)

 ! Replace YYYY, MM, DD, HH tokens in FILENAME w/ actual values
 call expand_date( Input_Opt%in_rst_f, Input_Opt%nymd, Input_Opt%nhms )

 ! get full path
 Input_Opt%out_rst_f = Input_Opt%in_rst_f
 Input_Opt%in_rst_f = trim(Input_Opt%run_dir) // trim(Input_Opt%in_rst_f)
 Input_Opt%out_rst_f = trim(Input_Opt%run_dir) // 'out_' // trim(Input_Opt%out_rst_f)

 END SUBROUTINE read_control_menu
!
!------------------------------------------------------------------------------
!
SUBROUTINE read_input_grid_menu( Input_Opt )

 ! References to F90 modules
 use error_mod,     only : error_stop
 use input_opt_mod, only : OptInput
 use parameter_mod

 type(OptInput), intent(inout) :: Input_Opt

 ! Local variables
 INTEGER            :: N
 CHARACTER(LEN=255) :: subStrs(MAXDIM)

 !=========================================================
 ! read_input_grid_menu begins here
 !=========================================================

 CALL split_one_line( subStrs, N, 1, 'read_input_grid_menu:1' )
 READ( subStrs(1:N), '(a)' ) Input_Opt%in_h_res

 CALL split_one_line( subStrs, N, 1, 'read_input_grid_menu:2' )
 READ( subStrs(1:N), * ) Input_Opt%in_n_layer

 ! Separator line
 CALL split_one_line( subStrs, N, 1, 'read_input_grid_menu:3' )

 !=========================================================
 ! Print to screen
 !=========================================================
 WRITE( 6, '(/,a)') '%%% INPUT GRID MENU %%%'
 WRITE( 6, '(  a)') repeat( '-', 48 )
 write( 6, 110    ) 'In grid resolution      : ', trim(Input_Opt%in_h_res)
 WRITE( 6, 111    ) 'In number of levels     : ', Input_Opt%in_n_layer
 if ( (Input_Opt%in_n_layer /= n_layer) .and. &
      (Input_Opt%in_n_layer /=n_layer_red) ) then
     call error_stop('error', 'input_mod.f90: read_input_grid_menu')
 end if

 110 FORMAT( a, a )
 111 format( a, i2)

 END SUBROUTINE read_input_grid_menu
!
!------------------------------------------------------------------------------
!
SUBROUTINE read_output_grid_menu( Input_Opt )

 ! References to F90 modules
 use error_mod,     only : error_stop
 use input_opt_mod, only : OptInput
 use parameter_mod

 type(OptInput), intent(inout) :: Input_Opt

 ! Local variables
 INTEGER            :: N
 CHARACTER(LEN=255) :: subStrs(MAXDIM)

 !=========================================================
 ! read_output_grid_menu begins here
 !=========================================================

 CALL split_one_line( subStrs, N, 1, 'read_output_grid_menu:1' )
 READ( subStrs(1:N), '(a)' ) Input_Opt%out_h_res

 CALL split_one_line( subStrs, N, 2, 'read_output_grid_menu:2' )
 READ( subStrs(1:N), * ) Input_Opt%out_lon_min, Input_Opt%out_lon_max

 CALL split_one_line( subStrs, N, 2, 'read_output_grid_menu:3' )
 READ( subStrs(1:N), * ) Input_Opt%out_lat_min, Input_Opt%out_lat_max

 CALL split_one_line( subStrs, N, 1, 'read_output_grid_menu:4' )
 READ( subStrs(1:N), * ) Input_Opt%out_half_polar

 CALL split_one_line( subStrs, N, 1, 'read_output_grid_menu:5' )
 READ( subStrs(1:N), * ) Input_Opt%out_n_layer

 CALL split_one_line( subStrs, N, 1, 'read_output_grid_menu:6' )
 READ( subStrs(1:N), * ) Input_Opt%out_is_nested

 ! Separator line
 CALL split_one_line( subStrs, N, 1, 'read_ouput_grid_menu:7' )

 !=========================================================
 ! Print to screen
 !=========================================================
 WRITE( 6, '(/,a)') '%%% INPUT GRID MENU %%%'
 WRITE( 6, '(  a)') repeat( '-', 48 )
 write( 6, 110    ) 'Out grid resolution     : ', trim(Input_Opt%out_h_res)
 write( 6, 111    ) 'Out longitude min/max   : ', Input_Opt%out_lon_min, &
                                                  Input_Opt%out_lon_max
 write( 6, 111    ) 'Out latitude min/max    : ', Input_Opt%out_lat_min, &
                                                  Input_Opt%out_lat_max
 write( 6, 112    ) ' Half-sized polar boxes?: ', Input_Opt%out_half_polar
 write( 6, 113    ) 'Out number of levels    : ', Input_Opt%out_n_layer
 if ( ((Input_Opt%out_n_layer /= n_layer) .and. &
      (Input_Opt%out_n_layer /=n_layer_red)) .or. &
      (Input_Opt%out_n_layer > Input_Opt%in_n_layer) ) then
     call error_stop('error', 'output_mod.f90: read_input_grid_menu')
 end if
 write( 6, 112    ) 'Out nested grid?        : ', Input_Opt%out_is_nested

 110 FORMAT( a, a )
 111 format( a, f6.1, 1x, f6.1)
 112 FORMAT( a, l )
 113 FORMAT( a, i2 )

 END SUBROUTINE read_output_grid_menu
!
!------------------------------------------------------------------------------
!
      FUNCTION READ_ONE_LINE( EOF, LOCATION ) RESULT( LINE )
!
!******************************************************************************
!  Subroutine READ_ONE_LINE reads a line from the input file.  If the global 
!  variable VERBOSE is set, the line will be printed to stdout.  READ_ONE_LINE
!  can trap an unexpected EOF if LOCATION is passed.  Otherwise, it will pass
!  a logical flag back to the calling routine, where the error trapping will
!  be done. (bmy, 7/20/04)
! 
!  Arguments as Output:
!  ===========================================================================
!  (1 ) EOF      (CHARACTER) : Logical flag denoting EOF condition
!  (2 ) LOCATION (CHARACTER) : Name of calling routine; traps premature EOF
!
!  Function value:
!  ===========================================================================
!  (1 ) LINE     (CHARACTER) : A line of text as read from the file
!
!  NOTES:
!******************************************************************************
!      
      ! References to F90 modules
      USE FILE_MOD, ONLY : IOERROR

      ! Arguments
      LOGICAL,          INTENT(OUT)          :: EOF
      CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: LOCATION

      ! Local variables
      INTEGER                                :: IOS
      CHARACTER(LEN=255)                     :: LINE, MSG

      !=================================================================
      ! READ_ONE_LINE begins here!
      !=================================================================

      ! Initialize
      EOF = .FALSE.

      ! Read a line from the file
      READ( IU_rt, '(a)', IOSTAT=IOS ) LINE

      ! IO Status < 0: EOF condition
      IF ( IOS < 0 ) THEN
         EOF = .TRUE.

         ! Trap unexpected EOF -- stop w/ error msg if LOCATION is passed
         ! Otherwise, return EOF to the calling program
         IF ( PRESENT( LOCATION ) ) THEN
            MSG = 'READ_ONE_LINE: error at: ' // TRIM( LOCATION )
            WRITE( 6, '(a)' ) MSG
            WRITE( 6, '(a)' ) 'Unexpected end of file encountered!'
            WRITE( 6, '(a)' ) 'STOP in READ_ONE_LINE (input_mod.f)'
            WRITE( 6, '(a)' ) REPEAT( '=', 79 )
            STOP
         ELSE
            RETURN
         ENDIF
      ENDIF

      ! IO Status > 0: true I/O error condition
      IF ( IOS > 0 ) CALL IOERROR( IOS, IU_rt, 'read_one_line:1' )

      ! Print the line (if necessary)
      IF ( VERBOSE ) WRITE( 6, '(a)' ) TRIM( LINE )

      ! Return to calling program
      END FUNCTION READ_ONE_LINE
!
!------------------------------------------------------------------------------
!
      SUBROUTINE SPLIT_ONE_LINE( SUBSTRS, N_SUBSTRS, N_EXP, LOCATION )
!
!******************************************************************************
!  Subroutine SPLIT_ONE_LINE reads a line from the input file (via routine 
!  READ_ONE_LINE), and separates it into substrings. (bmy, 7/20/04)
!
!  SPLIT_ONE_LINE also checks to see if the number of substrings found is 
!  equal to the number of substrings that we expected to find.  However, if
!  you don't know a-priori how many substrings to expect a-priori, 
!  you can skip the error check.
! 
!  Arguments as Input:
!  ===========================================================================
!  (3 ) N_EXP     (INTEGER  ) : Number of substrings we expect to find
!                               (N_EXP < 0 will skip the error check!)
!  (4 ) LOCATION  (CHARACTER) : Name of routine that called SPLIT_ONE_LINE
!
!  Arguments as Output:
!  ===========================================================================
!  (1 ) SUBSTRS   (CHARACTER) : Array of substrings (separated by " ")
!  (2 ) N_SUBSTRS (INTEGER  ) : Number of substrings actually found
!
!  NOTES:
!******************************************************************************
!
      ! References to F90 modules
      USE CHARPAK_MOD, ONLY: STRSPLIT

      ! Arguments
      CHARACTER(LEN=255), INTENT(OUT) :: SUBSTRS(MAXDIM)
      INTEGER,            INTENT(OUT) :: N_SUBSTRS
      INTEGER,            INTENT(IN)  :: N_EXP
      CHARACTER(LEN=*),   INTENT(IN)  :: LOCATION

      ! Local varaibles
      LOGICAL                         :: EOF
      CHARACTER(LEN=255)              :: LINE, MSG

      !=================================================================
      ! SPLIT_ONE_LINE begins here!
      !=================================================================      

      ! Create error msg
      MSG = 'SPLIT_ONE_LINE: error at ' // TRIM( LOCATION )

      !=================================================================
      ! Read a line from disk
      !=================================================================
      LINE = READ_ONE_LINE( EOF )

      ! STOP on End-of-File w/ error msg
      IF ( EOF ) THEN
         WRITE( 6, '(a)' ) TRIM( MSG )
         WRITE( 6, '(a)' ) 'End of file encountered!'
         WRITE( 6, '(a)' ) 'STOP in SPLIT_ONE_LINE (input_mod.f)!'
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         STOP
      ENDIF

      !=================================================================
      ! Split the lines between spaces -- start at column FIRSTCOL
      !=================================================================
      CALL STRSPLIT( LINE(FIRSTCOL:), ' ', SUBSTRS, N_SUBSTRS )

      ! Sometimes we don't know how many substrings to expect,
      ! if N_EXP is greater than MAXDIM, then skip the error check
      IF ( N_EXP < 0 ) RETURN

      ! Stop if we found the wrong 
      IF ( N_EXP /= N_SUBSTRS ) THEN
         WRITE( 6, '(a)' ) TRIM( MSG )
         WRITE( 6, 100   ) N_EXP, N_SUBSTRS
         WRITE( 6, '(a)' ) 'STOP in SPLIT_ONE_LINE (input_mod.f)!'
         WRITE( 6, '(a)' ) REPEAT( '=', 79 )
         STOP
 100     FORMAT( 'Expected ',i2, ' substrs but found ',i3 )
      ENDIF

      ! Return to calling program
      END SUBROUTINE SPLIT_ONE_LINE
!
!------------------------------------------------------------------------------
!
END MODULE input_mod
