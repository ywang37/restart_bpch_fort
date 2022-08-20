! $Id: restart_mod.f,v 1.2 2012/03/01 22:00:27 daven Exp $
      MODULE RESTART_MOD
!
!******************************************************************************
!  Module RESTART_MOD contains variables and routines which are used to read
!  and write GEOS-CHEM restart files, which contain tracer concentrations
!  in [v/v] mixing ratio. (bmy, 6/25/02, 12/16/05)
!
!  Module Variables:
!  ============================================================================
!  (1 ) INPUT_RESTART_FILE   : Full path name of the restart file to be read
!  (2 ) OUTPUT_RESTART_FILE  : Full path name (w/ tokens!) of output file
!
!  Module Routines:
!  ============================================================================
!  (1 ) MAKE_RESTART_FILE    : Writes restart file to disk
!  (2 ) READ_RESTART_FILE    : Reads restart file from disk
!  (3 ) CONVERT_TRACER_TO_VV : Converts from [ppbv], [ppmv], etc to [v/v]
!  (4 ) CHECK_DIMENSIONS     : Ensures that restart file contains global data
!  (5 ) COPY_STT             : Converts [v/v] to [kg] and stores in STT
!  (6 ) CHECK_DATA_BLOCKS    : Makes sure we have read in data for each tracer
!  (7 ) SET_RESTART          : Gets restart filenames from "input_mod.f"
!
!  GEOS-CHEM modules referenced by restart_mod.f
!  ============================================================================
!  (1 ) bpch2_mod.f          : Module w/ routines for binary punch file I/O
!  (2 ) error_mod.f          : Module w/ NaN and other error check routines
!  (3 ) file_mod.f           : Module w/ file unit numbers and error checks
!  (4 ) grid_mod.f           : Module w/ horizontal grid information
!  (5 ) logical_mod.f        : Module w/ GEOS-CHEM logical switches
!  (6 ) time_mod.f           : Module w/ routines for computing time & date
!  (7 ) tracer_mod.f         : Module w/ GEOS-CHEM tracer array STT etc.
!
!  NOTES:
!  (1 ) Moved routines "make_restart_file.f"" and "read_restart_file.f" into
!        this module.  Also now internal routines to "read_restart_file.f"
!        are now a part of this module.  Now reference "file_mod.f" to get
!        file unit numbers and error checking routines. (bmy, 6/25/02)
!  (2 ) Now reference AD from "dao_mod.f".  Now reference "error_mod.f".
!        Also added minor bug fix for ALPHA platform. (bmy, 10/15/02)
!  (3 ) Now references "grid_mod.f" and the new "time_mod.f" (bmy, 2/11/03)
!  (4 ) Added error-check and cosmetic changes (bmy, 4/29/03)
!  (5 ) Removed call to COPY_STT_FOR_OX, it's obsolete (bmy, 8/18/03)
!  (6 ) Add fancy output (bmy, 4/26/04)
!  (7 ) Added routine SET_RESTART.  Now reference "logical_mod.f" and
!        "tracer_mod.f" (bmy, 7/20/04)
!  (8 ) Removed obsolete routines TRUE_TRACER_INDEX and COPY_DATA_FOR_CO_OH
!        (bmy, 6/28/05)
!  (9 ) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  (10) Now pass TAU via the arg list in MAKE_RESTART_FILE (bmy, 12/15/05)
!  (11) Add MAKE_CSPEC_FILE and READ_CSPEC_FILE routines to save and read
!        CSPEC_FULL restart files (dkh, 02/12/09)
!******************************************************************************
!
      IMPLICIT NONE

      !=================================================================
      ! MODULE PRIVATE DECLARATIONS -- keep certain internal variables
      ! and routines from being seen outside "restart_mod.f"
      !=================================================================

      ! Make everything PRIVATE ...
      PRIVATE

      ! ... except these routines
      PUBLIC  :: MAKE_RESTART_FILE
      PUBLIC  :: READ_RESTART_FILE



      !=================================================================
      ! MODULE VARIABLES
      !=================================================================
!      CHARACTER(LEN=255) :: INPUT_RESTART_FILE
!      CHARACTER(LEN=255) :: OUTPUT_RESTART_FILE

      !=================================================================
      ! MODULE ROUTINES -- follow below the "CONTAINS" statement
      !=================================================================
      CONTAINS

!------------------------------------------------------------------------------

      SUBROUTINE MAKE_RESTART_FILE( input_opt, out_restart_state, 
     &        out_grid_opt )
!
!******************************************************************************
!  Subroutine MAKE_RESTART_FILE creates GEOS-CHEM restart files of tracer
!  mixing ratios (v/v), in binary punch file format. (bmy, 5/27/99, 12/16/05)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) YYYYMMDD : Year-Month-Date
!  (2 ) HHMMSS   :  and Hour-Min-Sec for which to create a restart file
!  (3 ) TAU      : GEOS-CHEM TAU value corresponding to YYYYMMDD, HHMMSS
!
!  NOTES:
!  (1 ) Now use function NYMD_STRING from "time_mod.f" to generate a
!        Y2K compliant string for all data sets. (bmy, 6/22/00)
!  (2 ) Reference F90 module "bpch2_mod.f" which contains routines BPCH2_HDR,
!        BPCH2, and GET_MODELNAME for writing data to binary punch files.
!        (bmy, 6/22/00)
!  (3 ) Now do not write more than NTRACE data blocks to disk.
!        Also updated comments. (bmy, 7/17/00)
!  (4 ) Now use IOS /= 0 to trap both I/O errors and EOF. (bmy, 9/13/00)
!  (5 ) Added to "restart_mod.f".  Also now save the entire grid to the
!        restart file. (bmy, 6/24/02)
!  (6 ) Bug fix: Remove duplicate definition of MM.  This causes compile-time
!        problems on the ALPHA platform. (gcc, bmy, 11/6/02)
!  (7 ) Now references functions GET_OFFSET, GET_YOFFSET from "grid_mod.f".
!        Now references function GET_TAU from "time_mod.f".  Now added a call
!        to DEBUG_MSG from "error_mod.f" (bmy, 2/11/03)
!  (8 ) Cosmetic changes (bmy, 4/29/03)
!  (9 ) Now reference STT, N_TRACERS, TCVV from "tracer_mod.f".  Also now
!        remove hardwired output restart filename.   Now references LPRT
!        from "logical_mod.f". (bmy, 7/20/04)
!  (10) Remove references to CMN_DIAG and TRCOFFSET.  Now call GET_HALFPOLAR
!        from "bpch2_mod.f" to get the HALFPOLAR flag value for GEOS or GCAP
!        grids. (bmy, 6/28/05)
!  (11) Now make sure all USE statements are USE, ONLY (bmy, 10/3/05)
!  (12) Add TAU to the argument list (bmy, 12/16/05)
!******************************************************************************
!
!      ! References to F90 modules
      USE BPCH2_MOD,   ONLY : BPCH2
      USE BPCH2_MOD,   ONLY : OPEN_BPCH2_FOR_WRITE
      USE ERROR_MOD,   ONLY : DEBUG_MSG
      USE FILE_MOD,    ONLY : IU_RST,        IOERROR

      use grid_opt_mod,        only : OptGrid
      use input_opt_mod,       only : OptInput
      use state_restart_mod,   only : RestStat

      ! Arguments
      type(OptInput),   intent(in)    :: input_opt
      type(RestStat),   intent(in)    :: out_restart_state
      type(OptGrid),    intent(in)    :: out_grid_opt

      ! Local Variables
      INTEGER              :: I,    I0, IOS, J,  J0, L, N
      INTEGER              :: YYYY, MM, DD,  HH, SS
      integer              :: iipar, jjpar, llpar
      character(len=255)   :: filename
      integer              :: n_tracers

      real*4, allocatable  :: tracer(:,:,:)


      ! For binary punch file, version 2.0
      REAL*4               :: LONRES, LATRES
      INTEGER              :: HALFPOLAR
      INTEGER, PARAMETER   :: CENTER180 = 1

      CHARACTER(LEN=20)    :: MODELNAME
      CHARACTER(LEN=40)    :: CATEGORY
      CHARACTER(LEN=40)    :: UNIT
      CHARACTER(LEN=40)    :: RESERVED = ''
      CHARACTER(LEN=80)    :: TITLE
      real*8               :: tau0, tau1

      !=================================================================
      ! MAKE_RESTART_FILE begins here!
      !=================================================================

      n_tracers = input_opt%n_tracers

      tau0 = out_restart_state%tau0
      tau1 = out_restart_state%tau1

      ! Initialize some variables
      iipar = size(out_restart_state%stt, 1)
      jjpar = size(out_restart_state%stt, 2)
      llpar = size(out_restart_state%stt, 3)
      allocate(tracer(iipar,jjpar,llpar))
      TRACER(:,:,:) = 0e0

      ! Define variables for BINARY PUNCH FILE OUTPUT
      TITLE    = 'GEOS-CHEM Restart File: ' //
     &           'Instantaneous Tracer Concentrations (v/v)'
      UNIT     = 'v/v'
      CATEGORY = 'IJ-AVG-$'
      LONRES   = out_grid_opt%disize
      LATRES   = out_grid_opt%djsize

      MODELNAME = 'GEOS'

      if (out_grid_opt%half_polar) then
          HALFPOLAR = 1
      else
          HALFPOLAR = 0
      end if

      ! Get the nested-grid offsets
      I0 = out_grid_opt%i0
      J0 = out_grid_opt%j0

      !=================================================================
      ! Open the restart file for output -- binary punch format
      !=================================================================

       ! Copy the output restart file name into a local variable
       filename = input_opt%out_rst_f


      WRITE( 6, 100 ) TRIM( FILENAME )
 100  FORMAT( '     - MAKE_RESTART_FILE: Writing ', a )

      ! Open restart file for output
      CALL OPEN_BPCH2_FOR_WRITE( IU_RST, FILENAME, TITLE )

      !=================================================================
      ! Write each tracer to the restart file
      !=================================================================
      DO N = 1, N_TRACERS

         DO L = 1, LLPAR
         DO J = 1, JJPAR
         DO I = 1, IIPAR
            TRACER(I,J,L) = out_restart_state%STT(I,J,L,N)
         ENDDO
         ENDDO
         ENDDO

         ! and store in temporary variable TRACER
         CALL BPCH2( IU_RST,    MODELNAME, LONRES,    LATRES,
     &               HALFPOLAR, CENTER180, CATEGORY,  N,
     &               UNIT,      TAU0,      TAU1,      RESERVED,
     &               IIPAR,     JJPAR,     LLPAR,     I0+1,
     &               J0+1,      1,         TRACER )
      ENDDO

      ! Close file
      CLOSE( IU_RST )

      ! Return to calling program
      END SUBROUTINE MAKE_RESTART_FILE
!
!------------------------------------------------------------------------------

      SUBROUTINE READ_RESTART_FILE( input_opt, in_restart_state )
!
!******************************************************************************
!  Subroutine READ_RESTART_FILE initializes GEOS-CHEM tracer concentrations
!  from a restart file (binary punch file format) (bmy, 5/27/99, 12/16/05)
!
!  Arguments as input:
!  ============================================================================
!  (1 ) YYYYMMDD : Year-Month-Day
!  (2 ) HHMMSS   :  and Hour-Min-Sec for which to read restart file
!
!  NOTES:
!  (1 ) Now check that N = NTRACER - TRCOFFSET is valid.
!        Also reorganize some print statements  (bmy, 10/25/99)
!  (2 ) Now pass LFORCE, LSPLIT via CMN_SETUP. (bmy, 11/4/99)
!  (3 ) Cosmetic changes, added comments (bmy, 3/17/00)
!  (4 ) Now use function NYMD_STRING from "time_mod.f" to generate a
!        Y2K compliant string for all data sets. (bmy, 6/22/00)
!  (5 ) Broke up sections of code into internal subroutines.  Also updated
!        comments & cleaned up a few things. (bmy, 7/17/00)
!  (6 ) Now use IOS /= 0 to trap both I/O errors and EOF. (bmy, 9/13/00)
!  (7 ) Print max & min of tracer regardless of the units (bmy, 10/5/00)
!  (8 ) Removed obsolete code from 10/00 (bmy, 12/21/00)
!  (9 ) Removed obsolete commented out code (bmy, 4/23/01)
!  (10) Added updates from amf for tagged Ox run.  Also updated comments
!        and made some cosmetic changes (bmy, 7/3/01)
!  (11) Bug fix: if starting from multiox restart file, then NTRACER
!        will be greater than 40  but less than 60.  Adjust COPY_STT_FOR_OX
!        accordingly. (amf, bmy, 9/6/01)
!  (12) Now reference TRANUC from "charpak_mod.f" (bmy, 11/15/01)
!  (13) Updated comments (bmy, 1/25/02)
!  (14) Now reference AD from "dao_mod.f" (bmy, 9/18/02)
!  (15) Now added a call to DEBUG_MSG from "error_mod.f" (bmy, 2/11/03)
!  (16) Remove call to COPY_STT_FOR_OX, it's obsolete. (bmy, 8/18/03)
!  (17) Add fancy output string (bmy, 4/26/04)
!  (18) No longer use hardwired filename.  Also now reference "logical_mod.f"
!        and "tracer_mod.f" (bmy, 7/20/04)
!  (19) Remove code for obsolete CO-OH simulation.  Also remove references
!        to CMN_DIAG and TRCOFFSET.   Change tracer name format string to A10.
!        (bmy, 6/24/05)
!  (20) Updated comments (bmy, 12/16/05)
!******************************************************************************
!
      ! References to F90 modules
      USE BPCH2_MOD,           ONLY : OPEN_BPCH2_FOR_READ
      USE ERROR_MOD,           ONLY : DEBUG_MSG
      USE FILE_MOD,            ONLY : IU_RST,      IOERROR
      use input_opt_mod,       only : OptInput
      use state_restart_mod,   only : RestStat


      ! Arguments
      type(OptInput),   intent(in)    :: input_opt
      type(RestStat),   intent(inout) :: in_restart_state

      ! Local Variables
      INTEGER             :: I, IOS, J, L, N
      REAL*8              :: SUMTC
      integer             :: iipar, jjpar, llpar
      character(len=255)  :: filename

      real*4, allocatable :: tracer(:,:,:)

      logical             :: first

      ! For binary punch file, version 2.0
      INTEGER             :: NI,     NJ,     NL
      INTEGER             :: IFIRST, JFIRST, LFIRST
      INTEGER             :: NTRACER,   NSKIP
      INTEGER             :: HALFPOLAR, CENTER180
      REAL*4              :: LONRES,    LATRES
      REAL*8              :: ZTAU0,     ZTAU1
      CHARACTER(LEN=20)   :: MODELNAME
      CHARACTER(LEN=40)   :: CATEGORY
      CHARACTER(LEN=40)   :: UNIT
      CHARACTER(LEN=40)   :: RESERVED

      !=================================================================
      ! READ_RESTART_FILE begins here!
      !=================================================================

      ! Initialize some variables
      iipar = size(in_restart_state%stt, 1)
      jjpar = size(in_restart_state%stt, 2)
      llpar = size(in_restart_state%stt, 3)
      allocate(tracer(iipar,jjpar,llpar))
      TRACER(:,:,:) = 0e0

      first = .true.

      !=================================================================
      ! Open restart file and read top-of-file header
      !=================================================================

      ! Copy input file name to a local variable
      filename = input_opt%in_rst_f

      ! Echo some input to the screen
      WRITE( 6, '(a)'   ) REPEAT( '=', 79 )
      WRITE( 6, '(a,/)' ) 'R E S T A R T   F I L E   I N P U T'
      WRITE( 6, 100 ) TRIM( FILENAME )
 100  FORMAT( 'READ_RESTART_FILE: Reading ', a )

      ! Open the binary punch file for input
      CALL OPEN_BPCH2_FOR_READ( IU_RST, FILENAME )

      !=================================================================
      ! Read concentrations -- store in the TRACER array
      !=================================================================
      DO
         READ( IU_RST, IOSTAT=IOS )
     &     MODELNAME, LONRES, LATRES, HALFPOLAR, CENTER180

         ! IOS < 0 is end-of-file, so exit
         IF ( IOS < 0 ) EXIT

         ! IOS > 0 is a real I/O error -- print error message
         IF ( IOS > 0 ) CALL IOERROR( IOS,IU_RST,'read_restart_file:4' )

         READ( IU_RST, IOSTAT=IOS )
     &        CATEGORY, NTRACER,  UNIT, ZTAU0,  ZTAU1,  RESERVED,
     &        NI,       NJ,       NL,   IFIRST, JFIRST, LFIRST,
     &        NSKIP

         if (first) then
             in_restart_state%tau0 = ZTAU0
             in_restart_state%tau1 = ZTAU1
             first = .false.
         end if

         IF ( IOS /= 0 ) CALL IOERROR( IOS,IU_RST,'read_restart_file:5')

         READ( IU_RST, IOSTAT=IOS )
     &        ( ( ( TRACER(I,J,L), I=1,NI ), J=1,NJ ), L=1,NL )

         IF ( IOS /= 0 ) CALL IOERROR( IOS,IU_RST,'read_restart_file:6')

         !==============================================================
         ! Assign data from the TRACER array to the STT array.
         !==============================================================

         ! Only process concentration data (i.e. mixing ratio)
         IF ( CATEGORY(1:8) == 'IJ-AVG-$' ) THEN

             !print*, 'NTRACER = ', NTRACER
             in_restart_state%stt(:,:,:,NTRACER) = tracer(:,:,:)

         ENDIF
      ENDDO

      ! Close file
      CLOSE( IU_RST )

      ! Return to calling program
      END SUBROUTINE READ_RESTART_FILE

!------------------------------------------------------------------------------

      END MODULE RESTART_MOD
