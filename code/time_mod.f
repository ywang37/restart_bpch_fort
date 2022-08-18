! $Id: time_mod.f,v 1.1 2010/08/12 15:24:08 xxu Exp $
      MODULE TIME_MOD
!
!******************************************************************************
!  TIME_MOD contains MODIS date and time variables, and routines for accessing
!  them. (xxu, 8/12/10, 8/12/10)
!
!  Module Variables:
!  ============================================================================
!  (1 ) NYMDb       (INTEGER) : YYYYMMDD at beginning of run
!  (2 ) NHMSb       (INTEGER) : HHMMSS   at beginning of run
!  (3 ) NYMDe       (INTEGER) : YYYYMMDD at end       of run
!  (4 ) NHMSe       (INTEGER) : HHMMSS   at end       of run
!  (5 ) NYMD        (INTEGER) : YYYYMMDD at current timestep
!  (6 ) NHMS        (INTEGER) : HHMMSS   at current timestep
!  (7 ) YEAR        (INTEGER) : Current year (YYYY format)
!  (8 ) MONTH       (INTEGER) : Current month value (1-12)
!  (9 ) DAY         (INTEGER) : Current day of the month (1-31)
!  (10) HOUR        (INTEGER) : Current hour of the day (0-23)
!  (11) MINUTE      (INTEGER) : Current minute of the hour (0-59)
!  (12) SECOND      (INTEGER) : Current second of the minute (0-59)
!  (13) NSEASON     (INTEGER) : Season flag (1=DJF, 2=MAM, 3=JJA, 4=SON)
!  (14) DAY_OF_YEAR (INTEGER) : Current day of year (0-365 or 0-366)
!  (15) ELAPSED_MIN (INTEGER) : Elapsed minutes since the end   of the run
!  (16) TAU         (REAL*8 ) : Current TAU value (hours since 0 GMT 1/1/1985)
!  (17) TAUb        (REAL*8 ) : TAU value at beginning of GEOS-CHEM run
!  (18) TAUe        (REAL*8 ) : TAU value at end       of GEOS-CHEM run
!  (19) GMT         (REAL*8 ) : Current Greenwich Mean Time (0.0 - 23.999 hrs)
!  (20) JD85        (REAL*8 ) : Astronomical Julian Day at 0 GMT 1/1/1985
!
!  Module Routines
!  ============================================================================
!  (1 ) SET_CURRENT_TIME  : Updates time variables for current timestep
!  (2 ) SET_BEGIN_TIME    : Initializes NYMDb, NHMSb, TAUb variables
!  (3 ) SET_END_TIME      : Initializes NYMDe, NHMSe, TAUe variables
!  (4 ) PRINT_CURRENT_TIME: Prints date time in YYYY/MM/DD, HH:MM:SS format
!  (5 ) TIMESTAMP_STRING  : Returns a string "YYYY/MM/DD HH:MM:SS"
!  (6 ) YMD_EXTRACT       : Extracts YYYY, MM, DD from a YYYYMMDD format number
!  (7 ) EXPAND_DATE       : Replaces date/time tokens w/ actual values
!  (8 ) SYSTEM_DATE_TIME  : Returns the system date and time
!  (9 ) SYSTEM_TIMESTAMP  : Returns a string with the system date and time
!  (10) GET_JD            : Returns Astronomical Julian Date for NYMD, NHMS
!  (  ) GET_ELAPSED_MIN   : Returns the elapsed minutes since the start of run
!  (  ) GET_ELAPSED_SEC   : Returns the elapsed seconds since the start of run
!  (20) GET_NYMDb         : Returns the YYYYMMDD at the beginning of the run
!  (21) GET_NHMSb         : Returns the HHMMSS   at the beginning of the run
!  (22) GET_NYMDe         : Returns the YYYYMMDD at the end of the run
!  (23) GET_NHMSe         : Returns the HHMMSS   at the end of the run
!  (24) GET_NYMD          : Returns the YYYYMMDD at the current time
!  (25) GET_NHMS          : Returns the HHMMSS   at the current time
!  (28) GET_MONTH         : Returns the current month (1-12)
!  (29) GET_DAY           : Returns the current day of month (1-31)
!  (30) GET_YEAR          : Returns the current year (YYYY)
!  (31) GET_HOUR          : Returns the current hour (0-23)
!  (32) GET_MINUTE        : Returns the current minute (0-59)
!  (33) GET_SECOND        : Returns the current second (0-59)
!  (34) GET_DAY_OF_YEAR   : Returns the current day of the year (0-366)
!  (35) GET_DAY_OF_WEEK   : Returns the current day of the week (0-6)
!  (36) GET_GMT           : Returns the current GMT (0.0 - 23.999)
!  (37) GET_TAU           : Returns the current TAU value (hrs since 1/1/1985)
!  (38) GET_TAUb          : Returns TAU value at beginning of GEOS-CHEM run
!  (39) GET_TAUe          : Returns TAU value at end of GEOS-CHEM run
!  (40) GET_DIAGb         : Returns TAU value at start of diagnostic interval
!  (41) GET_DIAGe         : Returns TAU value at end of diagnostic interval
!  
!  NOTES
!******************************************************************************
!
      IMPLICIT NONE

      !=================================================================
      ! MODULE PRIVATE DECLARATIONS -- keep certain internal variables 
      ! and routines from being seen outside "time_mod.f"
      !=================================================================

      ! Make everything PUBLIC...
      PUBLIC

      ! ... except those variables
      PRIVATE           :: NYMDb,      NHMSb,       NYMDe
      PRIVATE           :: NHMSe,      NYMD,        NHMS
      PRIVATE           :: MONTH,      DAY,         YEAR
      PRIVATE           :: HOUR,       MINUTE,      SECOND
      PRIVATE           :: NSEASON,    DAY_OF_YEAR, ELAPSED_MIN
      PRIVATE           :: TAU,        TAUb,        TAUe
      PRIVATE           :: GMT
      PRIVATE           :: TS_DYN,     TS_DIAG
      PRIVATE           :: CT_DYN
      PRIVATE           :: JD85

      ! ... and, these variables
      !=================================================================
      ! MODULE VARIABLES
      !=================================================================

      ! Date and time variables
      INTEGER           :: NYMDb,      NHMSb
      INTEGER           :: NYMDe,      NHMSe
      INTEGER           :: NYMD,       NHMS
      INTEGER           :: Month,      YEAR,       DAY
      INTEGER           :: HOUR,       MINUTE,     SECOND
      INTEGER           :: NSEASON,    DAY_OF_YEAR
      INTEGER           :: ELAPSED_MIN
      REAL*8            :: TAU,        TAUb,        TAUe 
      REAL*8            :: GMT

      ! Timesteps
      INTEGER           :: TS_DYN,     TS_DIAG
      
      ! Timestep counters
      INTEGER           :: CT_DYN      

      ! Astronomical Julian Date at 0 GMT, 1 Jan 1985
      REAL*8, PARAMETER :: JD85 = 2446066.5d0

      !=================================================================
      ! MODULE ROUTINES -- follow below the "CONTAINS" statement 
      !=================================================================
      CONTAINS
 
!------------------------------------------------------------------------------
 
      SUBROUTINE SET_CURRENT_TIME
!
!******************************************************************************
!  Subroutine SET_CURRENT_TIME takes in the elapsed time in minutes since the 
!  start of a GEOS-CHEM simulation and sets the GEOS-CHEM time variables 
!  accordingly. (bmy, 2/5/03, 10/3/06)
!
!  NOTES:
!  (1 ) GCAP/GISS fields don't have leap years, so if JULDAY says it's 
!        Feb 29th, reset MONTH, DAY, JD1 to Mar 1st. (swu, bmy, 8/29/05)
!  (2 ) Now references "define.h".  Now add special handling to skip from
!        Feb 28th to Mar 1st for GCAP model. (swu, bmy, 4/24/06)
!  (3 ) Fix bug in case of GCAP fields for runs that start during leap year
!       and after February 29 (phs, 9/27/06)  
!******************************************************************************
!
      ! References to F90 modules
      USE JULDAY_MOD, ONLY : JULDAY, CALDATE

      ! Local variables
      LOGICAL :: IS_LEAPYEAR
      REAL*4  :: TMP
      REAL*8  :: JD0, JD1, JD_JAN_1

      !=================================================================
      ! SET_CURRENT_TIME begins here!
      !=================================================================

      ! JD0: Astronomical Julian Date at start of GEOS-CHEM run
      JD0 = GET_JD( NYMDb, NHMSb )

      ! JD1: Astronomical Julian Date at current time
      JD1 = JD0 + ( DBLE( ELAPSED_MIN ) / 1440d0 )

      ! Call CALDATE to compute the current YYYYMMDD and HHMMSS
      CALL CALDATE( JD1, NYMD, NHMS )

      ! Extract current year, month, day from NYMD
      CALL YMD_EXTRACT( NYMD, YEAR, MONTH, DAY )

      ! Extract current hour, minute, second from NHMS
      CALL YMD_EXTRACT( NHMS, HOUR, MINUTE, SECOND )

      ! Fix minutes & seconds for display purposes (esp. for 1x1)
      IF ( SECOND              == 59 ) SECOND = 0
      IF ( MOD( MINUTE+1, 10 ) == 0  ) MINUTE = MINUTE + 1

      !=================================================================
      ! Compute other GEOS-CHEM timing variables
      !=================================================================

      ! Current Greenwich Mean Time
      GMT         = ( DBLE( HOUR )            ) +
     &              ( DBLE( MINUTE ) / 60d0   ) +
     &              ( DBLE( SECOND ) / 3600d0 )

      ! Days elapsed in this year (0-366)
      DAY_OF_YEAR = JD1 - JULDAY( YEAR, 1, 0d0 )

      ! TAU value (# of hours since 1 Jan 1985)
      ! NOTE: TMP is REAL*4 to prevent precision problems
      TMP         = ( JD1 - JD85 ) * 24e0
      TAU         = DBLE( TMP )

      ! Season index (1=DJF, 2=MAM, 3=JJA, 4=SON)
      SELECT CASE ( MONTH )
         CASE ( 12, 1, 2 )
            NSEASON = 1
         CASE ( 3, 4, 5 )
            NSEASON = 2
         CASE ( 6, 7, 8 )
            NSEASON = 3
         CASE ( 9, 10, 11 )
            NSEASON = 4
      END SELECT

      ! Return to calling program
      END SUBROUTINE SET_CURRENT_TIME

!------------------------------------------------------------------------------

      SUBROUTINE SET_BEGIN_TIME( THISNYMDb, THISNHMSb )
!
!******************************************************************************
!  Subroutine SET_BEGIN_TIME initializes NYMDb, NHMSb, and TAUb, which are the
!  YYYYMMDD, HHMMSS, and hours since 1/1/1985 corresponding to the beginning 
!  date and time of a GEOS-CHEM run. (bmy, 2/5/03, 7/20/04)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) THISNYMDb (INTEGER) : YYYYMMDD at beginning of GEOS-CHEM run
!  (2 ) THISNHMSb (INTEGER) : HHMMSS   at beginning of GEOS-CHEM run
!
!  NOTES:
!  (1 ) Added error check for THISNHMSb (bmy, 7/20/04)
!******************************************************************************
!
      ! References to F90 modules
      USE ERROR_MOD,  ONLY : ERROR_STOP

      ! Arguments
      INTEGER, INTENT(IN) :: THISNYMDb, THISNHMSb

      ! Local variables
      REAL*4              :: TMP

      !=================================================================
      ! SET_BEGIN_TIME begins here!
      !=================================================================

      ! Make sure NHMSb is valid
      IF ( THISNHMSb > 235959 ) THEN
         CALL ERROR_STOP( 'NHMSb cannot be greater than 23:59:59!',
     &                    'SET_BEGIN_TIME (time_mod.f)' )
      ENDIF

      ! Make sure THISNYMDb uses 4 digits for the year
      ! and is not less than 1985/01/01
      IF ( THISNYMDb < 19850101 ) THEN
         CALL ERROR_STOP( 'NYMDb must be in the format YYYYMMDD!',
     &                    'SET_BEGIN_TIME (time_mod.f)' )

      ENDIF

      ! Initialize NYMDb, NHMSb
      NYMDb = THISNYMDb
      NHMSb = THISNHMSb

      ! TAUb value (TMP is REAL*4 to prevent precision problems)
      TMP   = ( GET_JD( NYMDb, NHMSb ) - JD85 ) * 24e0
      TAUb  = DBLE( TMP )

      ! Also initialize ELAPSED_MIN
      ELAPSED_MIN = 0

      ! Return to calling program
      END SUBROUTINE SET_BEGIN_TIME

      SUBROUTINE SET_END_TIME( THISNYMDe, THISNHMSe )
!
!******************************************************************************
!  Subroutine SET_END_TIME initializes NYMDe, NHMSe, and TAUe, which are the
!  YYYYMMDD, HHMMSS, and hours since 1/1/1985 corresponding to the ending 
!  date and time of a GEOS-CHEM run. (bmy, 2/5/03, 7/20/04)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) THISNYMDe (INTEGER) : YYYYMMDD at end of GEOS-CHEM run
!  (2 ) THISNHMSe (INTEGER) : HHMMSS   at end of GEOS-CHEM run
!
!  NOTES:
!  (1 ) Added error check for THISNHMSb (bmy, 7/20/04)
!******************************************************************************
!
      ! References to F90 modules
      USE ERROR_MOD,  ONLY : ERROR_STOP

      ! Arguments
      INTEGER, INTENT(IN) :: THISNYMDe, THISNHMSe

      ! Local variables
      REAL*4              :: TMP

      !=================================================================
      ! SET_END_TIME begins here!
      !=================================================================

      ! Error check to make sure 
      IF ( THISNHMSe > 235959 ) THEN
         CALL ERROR_STOP( 'NHMSe cannot be greater than 23:59:59!',
     &                    'SET_END_TIME (time_mod.f)' )
      ENDIF

      ! Make sure THISNYMDb uses 4 digits for the year
      ! and is not less than 1985/01/01
      IF ( THISNYMDe < 19850101 ) THEN
         CALL ERROR_STOP( 'NYMDe must be in the format YYYYMMDD!',
     &                    'SET_END_TIME (time_mod.f)' )

      ENDIF

      ! Initialize NYMDe, NHMSe
      NYMDe = THISNYMDe
      NHMSe = THISNHMSe

      ! TAUe value (TMP is REAL*4 to prevent precision problems)
      TMP   = ( GET_JD( NYMDe, NHMSe ) - JD85 ) * 24e0
      TAUe  = DBLE( TMP )

      ! Return to calling program
      END SUBROUTINE SET_END_TIME

!------------------------------------------------------------------------------

      SUBROUTINE SET_TIMESTEPS( DYNAMICS )
!
!******************************************************************************
!  Subroutine SET_TIMESTEPS initializes the timesteps for dynamics, convection,
!  chemistry, and emissions.  Counters are also zeroed. (bmy, 3/21/03,10/20/05)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) DYNAMICS   (INTEGER) : Dynamic timestep          [minutes]
!
!  NOTES:
!  (1 ) Suppress some output lines (bmy, 7/20/04)
!  (2 ) Also zero CT_XTRA (tmf, bmy, 10/20/05)
!******************************************************************************
!
      ! Arguments
      INTEGER, INTENT(IN) :: DYNAMICS

      !=================================================================
      ! SET_TIMESTEPS begins here!
      !=================================================================

      ! Initialize timesteps
      TS_DYN  = DYNAMICS

      ! Zero timestep counters
      CT_DYN  = 0

      ! Echo to stdout
      WRITE( 6, '(/,a)' ) 'SET_TIMESTEPS: setting AMF timesteps!'
      WRITE( 6, '(  a)' ) '-------------------------------------------'
      WRITE( 6, '(''Dynamics Timestep [min] : '', i4 )' ) TS_DYN

      ! Return to calling program
      END SUBROUTINE SET_TIMESTEPS

!------------------------------------------------------------------------------

      SUBROUTINE SET_CT_DYN( INCREMENT, RESET )
!
!******************************************************************************
!  Subroutine SET_CT_DYN increments CT_DYN, the counter of dynamic
!  timesteps executed thus far. (bmy, 3/21/03)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) INCREMENT (LOGICAL) : If T, then will increment counter
!  (2 ) RESET     (LOGICAL) : If T, then will reset counter to zero!
!
!  NOTES:
!******************************************************************************
!
      ! Arguments
      LOGICAL, INTENT(IN), OPTIONAL :: INCREMENT, RESET

      !=================================================================
      ! SET_CT_DYN begins here!
      !=================================================================
      IF ( PRESENT( INCREMENT ) ) THEN
         CT_DYN = CT_DYN + 1
      ELSE IF ( PRESENT( RESET ) ) THEN
         CT_DYN = 0
      ENDIF

      ! Return to calling program
      END SUBROUTINE SET_CT_DYN

!------------------------------------------------------------------------------

      SUBROUTINE SET_ELAPSED_MIN
!     
!******************************************************************************
!  Subroutine SET_ELAPSED_MIN increments the number of elapsed minutes by
!  the dynamic timestep TS_DYN. (bmy, 3/21/03)
!******************************************************************************
!     
      !=================================================================
      ! SET_ELAPSED_MIN begins here!
      !=================================================================
      ELAPSED_MIN = ELAPSED_MIN + TS_DYN

      ! Return to calling program
      END SUBROUTINE SET_ELAPSED_MIN

!------------------------------------------------------------------------------

      SUBROUTINE PRINT_CURRENT_TIME
!
!******************************************************************************
!  Subroutine PRINT_CURRENT_TIME prints the date, GMT time, and elapsed
!  hours of a GEOS-CHEM simulation. (bmy, 3/21/03)
!
!  NOTES:
!******************************************************************************
!
      ! Local variables
      REAL*4 :: E_HOURS

      !=================================================================
      ! PRINT_CURRENT_TIME begins here!
      !=================================================================

      ! Hours since start of run
      E_HOURS = REAL( ELAPSED_MIN ) / 60e0

      ! Write quantities
      WRITE( 6, 100 ) YEAR, MONTH, DAY, HOUR, MINUTE, E_HOURS

      ! Format string
 100  FORMAT( '---> DATE: ', i4.4, '/', i2.2, '/', i2.2,
     &            '  GMT: ', i2.2, ':', i2.2, '  X-HRS: ', f11.3 )

      ! Return to calling program
      END SUBROUTINE PRINT_CURRENT_TIME
!
!------------------------------------------------------------------------------
!
      FUNCTION TIMESTAMP_STRING( YYYYMMDD, HHMMSS ) RESULT( TIME_STR )
!
!******************************************************************************
!  TIMESTAMP_STRING returns a formatted string "YYYY/MM/DD HH:MM" for the a
!  date and time specified by YYYYMMDD and HHMMSS.  If YYYYMMDD and HHMMSS are
!  omitted, then TIMESTAMP_STRING will create a formatted string for the 
!  current date and time. (bmy, 3/21/03, 12/2/03)
!                                                                          
!  NOTES:
!******************************************************************************
!   
      ! Arguments
      INTEGER, INTENT(IN), OPTIONAL :: YYYYMMDD, HHMMSS

      ! Local variables
      INTEGER                       :: THISYEAR, THISMONTH,  THISDAY
      INTEGER                       :: THISHOUR, THISMINUTE, THISSECOND

      ! Function value
      CHARACTER(LEN=19)             :: TIME_STR

      !=================================================================
      ! TIMESTAMP_STRING begins here!
      !=================================================================

      ! If YYYYMMDD is passed, then use that date.  Otherwise use the 
      ! current date stored in global variables YEAR, MONTH, DAY.
      IF ( PRESENT( YYYYMMDD ) ) THEN
         CALL YMD_EXTRACT( YYYYMMDD, THISYEAR, THISMONTH, THISDAY )
      ELSE
         THISYEAR  = YEAR
         THISMONTH = MONTH
         THISDAY   = DAY
      ENDIF

      ! If HHMMSS is passed, then use that time.  Otherwise use the 
      ! current time stored in global variables HOUR and MINUTE.
      IF ( PRESENT( HHMMSS ) ) THEN
         CALL YMD_EXTRACT( HHMMSS, THISHOUR, THISMINUTE, THISSECOND )
      ELSE
         THISHOUR   = HOUR
         THISMINUTE = MINUTE
         THISSECOND = SECOND
      ENDIF

      ! For other platforms, we can just use a FORTRAN internal write
      WRITE( TIME_STR, 100 ) THISYEAR, THISMONTH,  THISDAY, 
     &                       THISHOUR, THISMINUTE, THISSECOND

      ! Format statement
 100  FORMAT( i4.4, '/', i2.2, '/', i2.2,' ', i2.2, ':', i2.2,':', i2.2)

      ! Return to calling program
      END FUNCTION TIMESTAMP_STRING
!
!------------------------------------------------------------------------------
!
      SUBROUTINE YMD_EXTRACT( NYMD, Y, M, D )
!
!******************************************************************************
!  Subroutine YMD_EXTRACT extracts the year, month, and date from an integer
!  variable in YYYYMMDD format.  It can also extract the hours, minutes, and
!  seconds from a variable in HHMMSS format. (bmy, 11/21/01)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) NYMD (INTEGER) : Variable in YYYYMMDD (or HHMMSS) format
!  
!  Arguments as Output:
!  ============================================================================
!  (2 ) Y    (INTEGER) : Variable that returns YYYY (or HH - hours  )
!  (3 ) M    (INTEGER) : Variable that returns MM   (or MM - minutes)
!  (4 ) D    (INTEGER) : Variable that returns DD   (or SS - seconds)
!
!  NOTES:
!******************************************************************************
!
      ! Arguments
      INTEGER, INTENT(IN)  :: NYMD
      INTEGER, INTENT(OUT) :: Y, M, D

      ! Local variables
      REAL*8               :: REM

      !=================================================================
      ! YMD_EXTRACT begins here!
      !=================================================================

      ! Extract YYYY from YYYYMMDD 
      Y = INT( DBLE( NYMD ) / 1d4 )

      ! Extract MM from YYYYMMDD
      REM = DBLE( NYMD ) - ( DBLE( Y ) * 1d4 )
      M   = INT( REM / 1d2 )

      ! Extract DD from YYYYMMDD
      REM = REM - ( DBLE( M ) * 1d2 )
      D   = INT( REM )

      ! Return to calling program
      END SUBROUTINE YMD_EXTRACT
!
!------------------------------------------------------------------------------
!
      SUBROUTINE EXPAND_DATE( FILENAME, YYYYMMDD, HHMMSS )
!
!******************************************************************************
!  Subroutine EXPAND_DATE replaces "YYYYMMDD" and "hhmmss" tokens within
!  a filename string with the actual values. (bmy, 6/27/02, 7/20/04)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) FILENAME (CHARACTER) : Filename with tokens to replace
!  (2 ) YYYYMMDD (INTEGER  ) : Current Year-Month-Day (must have 8 digits!)
!  (3 ) HHMMSS   (INTEGER  ) : Current Hour-Minute-Seconds
!  
!
!  Arguments as Output:
!  ============================================================================
!  (1 ) FILENAME (CHARACTER) : Modified filename 
! 
!  NOTES:
!  (1 ) Routine is orginally from 'time_mod.f' by bmy. (8/12/10)
!  (2 ) Now added 'XXX' for DAY_of_YEAR. (8/13/10)
!******************************************************************************
!
      ! References to F90 modules
      USE CHARPAK_MOD,    ONLY : STRREPL
      USE TOOL_KIT_MOD,   ONLY : ANNUAL_JD

      ! Arguments
      CHARACTER(LEN=*), INTENT(INOUT) :: FILENAME
      INTEGER,          INTENT(IN)    :: YYYYMMDD, HHMMSS

      ! Local variables
      INTEGER                         :: YYYY, YY, MM, DD, HH, II, SS
      INTEGER                         :: XXX
      CHARACTER(LEN=2)                :: MM_STR, DD_STR
      CHARACTER(LEN=2)                :: HH_STR, II_STR, SS_STR
      CHARACTER(LEN=2)                :: YY_STR
      CHARACTER(LEN=4)                :: YYYY_STR
      CHARACTER(LEN=3)                :: XXX_STR

      !=================================================================
      ! EXPAND_DATE begins here!
      !=================================================================

      ! Extract today's date into year, month, and day sections
      CALL YMD_EXTRACT( YYYYMMDD, YYYY, MM, DD )
      
      ! Convert date to day-of-year
      XXX = ANNUAL_JD( YYYY, MM, DD )

      ! Extract today's time into HH, MM, and SS sections
      ! (rename minutes to II so as not to overwrite MM)
      CALL YMD_EXTRACT( HHMMSS, HH, II, SS )

      ! 2-digit year number (e.g. "97" instead of "1997")
      YY = YYYY - 1900
      IF ( YY >= 100 ) YY = YY - 100

      ! For other platforms, use an F90 internal write (bmy, 9/29/03)
      WRITE( YYYY_STR, '(i4.4)' ) YYYY
      WRITE( YY_STR,   '(i2.2)' ) YY
      WRITE( MM_STR,   '(i2.2)' ) MM
      WRITE( DD_STR,   '(i2.2)' ) DD
      WRITE( HH_STR,   '(i2.2)' ) HH
      WRITE( II_STR,   '(i2.2)' ) II
      WRITE( XXX_STR,  '(i3.3)' ) XXX

      ! Replace YYYY, MM, DD, HH tokens w/ actual values 
      CALL STRREPL( FILENAME, 'YYYY', YYYY_STR )
      CALL STRREPL( FILENAME, 'YY',   YY_STR   )
      CALL STRREPL( FILENAME, 'MM',   MM_STR   )
      CALL STRREPL( FILENAME, 'DD',   DD_STR   )
      CALL STRREPL( FILENAME, 'hh',   HH_STR   )
      CALL STRREPL( FILENAME, 'mm',   II_STR   )
      CALL STRREPL( FILENAME, 'XXX',  XXX_STR  )

      ! Return to calling program
      END SUBROUTINE EXPAND_DATE
!
!------------------------------------------------------------------------------
!
      SUBROUTINE SYSTEM_DATE_TIME( SYS_NYMD, SYS_NHMS )
!
!******************************************************************************
!  Subroutine SYSTEM_DATE_TIME returns the actual local date and time 
!  (as opposed to the model date and time).  (bmy, 5/2/05)
!
!  Arguments as Output:
!  ============================================================================
!  (1 ) SYS_NYMD (INTEGER) : System date (local time) in YYYYMMDD format
!  (2 ) SYS_NHMS (INTEGER) : System time (local time) in HHMMSS   format
!
!  NOTES: 
!******************************************************************************
!
      ! Arguments
      INTEGER, INTENT(OUT) :: SYS_NYMD
      INTEGER, INTENT(OUT) :: SYS_NHMS

      ! Local variables
      INTEGER              :: V(8)
      CHARACTER(LEN=8)     :: D
      CHARACTER(LEN=10)    :: T

      !=================================================================
      ! SYSTEM_DATE_TIME begins here!
      !=================================================================

      ! Initialize
      D = 'ccyymmdd'
      T = 'hhmmss.sss'

      ! Call the F90 intrinsic routine DATE_AND_TIME
      ! Return values are (/YYYY, MM, DD, GMT_MIN, HH, MM, SS, MSEC/)
      CALL DATE_AND_TIME( DATE=D, TIME=T, VALUES=V )

      ! Save to YYYYMMDD and HHMMSS format
      SYS_NYMD = ( V(1) * 10000 ) + ( V(2) * 100 ) + V(3)
      SYS_NHMS = ( V(5) * 10000 ) + ( V(6) * 100 ) + V(7)

      ! Return to calling program
      END SUBROUTINE SYSTEM_DATE_TIME
!
!------------------------------------------------------------------------------
!
      FUNCTION SYSTEM_TIMESTAMP() RESULT( STAMP )
!
!******************************************************************************
!  Function SYSTEM_TIMESTAMP returns a 16 character string with the system
!  date and time in YYYY/MM/DD HH:MM format. (bmy, 5/3/05)
!
!  NOTES:
!******************************************************************************
!
      ! Local variables
      INTEGER           :: SYS_NYMD, SYS_NHMS
      CHARACTER(LEN=19) :: STAMP

      !=================================================================
      ! SYSTEM_TIMESTAMP begins here!
      !=================================================================

      ! Get system date and time
      CALL SYSTEM_DATE_TIME( SYS_NYMD, SYS_NHMS )

      ! Create a string w/ system date & time
      STAMP = TIMESTAMP_STRING( SYS_NYMD, SYS_NHMS )

      ! Return to calling program 
      END FUNCTION SYSTEM_TIMESTAMP
!
!------------------------------------------------------------------------------
!
      FUNCTION GET_JD( THISNYMD, THISNHMS ) RESULT( THISJD )
!
!******************************************************************************
!  Function GET_JD is a wrapper for the JULDAY routine.  Given the current
!  NYMD and NHMS values, GET_JD will return the current astronomical Julian
!  date. (bmy, 3/21/03)
! 
!  Arguments as Input:
!  ============================================================================
!  (1 ) THISNYMD (INTEGER) : YYYYMMDD value
!  (2 ) THISNHMS (INTEGER) : HHMMSS value
!
!  NOTES:
!******************************************************************************
!
      ! References to F90 m odules
      USE JULDAY_MOD, ONLY : JULDAY

      ! Arguments
      INTEGER, INTENT(IN)  :: THISNYMD, THISNHMS

      ! Local variables
      INTEGER              :: Y, M, D, H, MI, S
      REAL*8               :: DAY

      ! Function variable
      REAL*8               :: THISJD

      !=================================================================
      ! GET_JD begins here!
      !=================================================================

      ! Extract year, month, day from NYMDb
      CALL YMD_EXTRACT( THISNYMD, Y, M, D )

      ! Extract hour, minute, second from NHMSb
      CALL YMD_EXTRACT( THISNHMS, H, MI, S )

      ! Decimal day (including fractional part) 
      DAY  = DBLE( D ) + ( DBLE( H  ) / 24d0    ) +
     &                   ( DBLE( MI ) / 1440d0  ) +
     &                   ( DBLE( S  ) / 86400d0 )

      ! Compute astronomical Julian day at start of run
      THISJD = JULDAY( Y, M, DAY )

      ! Return to the calling program
      END FUNCTION GET_JD

!------------------------------------------------------------------------------

      FUNCTION GET_ELAPSED_MIN() RESULT( THIS_ELAPSED_MIN )
!
!******************************************************************************
!  Function GET_ELAPSED_MIN returns the elapsed minutes since the start of
!  a GEOS_CHEM run to the calling program (bmy, 3/21/03) 
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THIS_ELAPSED_MIN

      !=================================================================
      ! GET_ELAPSED_MIN begins here!
      !=================================================================     
      THIS_ELAPSED_MIN = ELAPSED_MIN

      ! Return to calling program
      END FUNCTION GET_ELAPSED_MIN

!------------------------------------------------------------------------------

      FUNCTION GET_ELAPSED_SEC() RESULT( THIS_ELAPSED_SEC )
!
!******************************************************************************
!  Function GET_ELAPSED_SEC returns the elapsed minutss since the start of
!  a GEOS_CHEM run to the calling program (bmy, 3/21/03) 
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THIS_ELAPSED_SEC

      !=================================================================
      ! GET_ELAPSED_SEC begins here!
      !=================================================================     
      THIS_ELAPSED_SEC = ELAPSED_MIN * 60

      ! Return to calling program
      END FUNCTION GET_ELAPSED_SEC

!------------------------------------------------------------------------------

      FUNCTION GET_NYMDb() RESULT( THISNYMDb )
!
!******************************************************************************
!  Function GET_NYMDb returns the NYMDb value (YYYYMMDD at the beginning of 
!  the run) to the calling program. (bmy, 3/21/03)
! 
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THISNYMDb

      !=================================================================
      ! GET_NYMDb begins here!
      !=================================================================
      THISNYMDb = NYMDb

      ! Return to calling program
      END FUNCTION GET_NYMDb

!------------------------------------------------------------------------------

      FUNCTION GET_NHMSb() RESULT( THISNHMSb )
!
!******************************************************************************
!  Function GET_NHMSb returns the NHMSb value (HHMMSS at the beginning
!  of the run) to the calling program. (bmy, 3/21/03)
! 
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THISNHMSb

      !=================================================================
      ! GET_NHMSb begins here!
      !=================================================================
      THISNHMSb = NHMSb

      ! Return to calling program
      END FUNCTION GET_NHMSb

!------------------------------------------------------------------------------

      FUNCTION GET_NYMDe() RESULT( THISNYMDe )
!
!******************************************************************************
!  Function GET_NYMDe returns the NYMDe value (YYYYMMDD at the end of 
!  the run) to the calling program. (bmy, 3/21/03)
! 
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THISNYMDe

      !=================================================================
      ! GET_NYMDe begins here!
      !=================================================================
      THISNYMDe = NYMDe

      ! Return to calling program
      END FUNCTION GET_NYMDe

!------------------------------------------------------------------------------

      FUNCTION GET_NHMSe() RESULT( THISNHMSe )
!
!******************************************************************************
!  Function GET_NHMSe returns the NHMSe value (HHMMSS at the end
!  of the run) to the calling program. (bmy, 3/21/03)
! 
!  NOTES:
!******************************************************************************
!     
      ! Function value
      INTEGER :: THISNHMSe
      
      !=================================================================
      ! GET_NHMSe begins here!
      !=================================================================
      THISNHMSe = NHMSe
      
      ! Return to calling program
      END FUNCTION GET_NHMSe

!------------------------------------------------------------------------------
      
      FUNCTION GET_NYMD() RESULT( THISNYMD )
!
!******************************************************************************
!  Function GET_NYMD returns the current NYMD value (YYYYMMDD) to the 
!  calling program. (bmy, 2/5/03)
! 
!  NOTES:
!******************************************************************************
!     
      ! Function value
      INTEGER :: THISNYMD
      
      !=================================================================
      ! GET_NYMD begins here!
      !=================================================================
      THISNYMD = NYMD
      
      ! Return to calling program
      END FUNCTION GET_NYMD

!------------------------------------------------------------------------------

      FUNCTION GET_NHMS() RESULT( THISNHMS )
!
!******************************************************************************
!  Function GET_NHMS returns the current NHMS value (HHMMSS) to the 
!  calling program. (bmy, 2/5/03)
! 
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THISNHMS

      !=================================================================
      ! GET_NHMS begins here!
      !=================================================================
      THISNHMS = NHMS

      ! Return to calling program
      END FUNCTION GET_NHMS

!------------------------------------------------------------------------------

      FUNCTION GET_MONTH() RESULT( THISMONTH )
!
!******************************************************************************
!  Function GET_MONTH returns the current month to the calling program.
!  (bmy, 2/5/03)
! 
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THISMONTH

      !=================================================================
      ! GET_MONTH begins here!
      !=================================================================
      THISMONTH = MONTH

      ! Return to calling program
      END FUNCTION GET_MONTH

!------------------------------------------------------------------------------

      FUNCTION GET_DAY() RESULT( THISDAY )
!
!******************************************************************************
!  Function GET_DAY returns the current day to the calling program.
!  (bmy, 2/5/03)
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THISDAY

      !=================================================================
      ! GET_DAY begins here!
      !=================================================================
      THISDAY = DAY

      ! Return to calling program
      END FUNCTION GET_DAY

!------------------------------------------------------------------------------

      FUNCTION GET_YEAR() RESULT( THISYEAR )
!
!******************************************************************************
!  Function GET_YEAR returns the current year to the calling program.
!  (bmy, 2/5/03)
!******************************************************************************
!
      ! Function value
      INTEGER :: THISYEAR

      !=================================================================
      ! GET_YEAR begins here!
      !=================================================================
      THISYEAR = YEAR

      ! Return to calling program
      END FUNCTION GET_YEAR

!------------------------------------------------------------------------------

      FUNCTION GET_HOUR() RESULT( THISHOUR )
!
!******************************************************************************
!  Function GET_HOUR returns the current hour to the calling program.
!  (bmy, 2/5/03)
!
!  NOTES:
!******************************************************************************
!     
      ! Function value
      INTEGER :: THISHOUR
      
      !=================================================================
      ! GET_HOUR begins here!
      !=================================================================
      THISHOUR = HOUR
      
      ! Return to calling program
      END FUNCTION GET_HOUR

!------------------------------------------------------------------------------
      
      FUNCTION GET_MINUTE() RESULT( THISMINUTE )
!
!******************************************************************************
!  Function GET_MINUTE returns the current minute to the calling program
!  (bmy, 2/5/03)
! 
!  NOTES:
!******************************************************************************
!     
      ! Function value
      INTEGER :: THISMINUTE
      
      !=================================================================
      ! GET_MINUTE begins here!
      !=================================================================
      THISMINUTE = MINUTE
      
      ! Return to calling program
      END FUNCTION GET_MINUTE

!------------------------------------------------------------------------------

      FUNCTION GET_SECOND() RESULT( THISSECOND )
!
!******************************************************************************
!  Function GET_SECOND returns the current seconds to the calling program.
!  (bmy, 2/5/03)
!******************************************************************************
!
      ! Function value
      INTEGER :: THISSECOND

      !=================================================================
      ! GET_SECOND begins here!
      !=================================================================
      THISSECOND = SECOND

      ! Return to calling program
      END FUNCTION GET_SECOND

!------------------------------------------------------------------------------

      FUNCTION GET_DAY_OF_YEAR() RESULT( THISDAYOFYEAR )
!
!******************************************************************************
!  Function GET_DAY_OF_YEAR returns the current day of the year (0-365 or
!  0-366 for leap years) to the calling program. (bmy, 2/5/03)
! 
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THISDAYOFYEAR

      !=================================================================
      ! GET_DAY_OF_YEAR begins here!
      !=================================================================
      THISDAYOFYEAR = DAY_OF_YEAR

      ! Return to calling program
      END FUNCTION GET_DAY_OF_YEAR

!------------------------------------------------------------------------------

      FUNCTION GET_DAY_OF_WEEK() RESULT( DAY_NUM )
!
!******************************************************************************
!  Function GET_DAY_OF_WEEK returns the day of the week as a number:
!  Sun=0, Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6.  (bmy, 11/5/04)
!
!  Reference:
!  ============================================================================
!  "Practical Astronomy with Your Calculator", 3rd Ed.  Peter Duffett-Smith,
!    Cambridge UP, 1992, p9.
!
!  NOTES:
!******************************************************************************
!
      ! References to F90 modules
      USE JULDAY_MOD, ONLY : JULDAY

      ! Return value
      INTEGER :: DAY_NUM

      ! Local variables
      REAL*8  :: A, B, JD, THISDAY

      !=================================================================
      ! GET_DAY_OF_WEEK begins here!
      !=================================================================

      ! Get fractional day
      THISDAY = DAY                 + ( HOUR   / 24d0    ) +
     &          ( MINUTE / 1440d0 ) + ( SECOND / 86400d0 )

      ! Get current Julian date 
      JD      = JULDAY( YEAR, MONTH, THISDAY )

      ! Add 1.5 to JD and divide by 7
      A       = ( JD + 1.5d0 ) / 7d0

      ! Take fractional part and multiply by 7
      B       = ( A - INT( A ) ) * 7d0

      ! Round to nearest integer -- this is the day number!
      DAY_NUM = INT( B + 0.5d0 )

      ! Return to calling program
      END FUNCTION GET_DAY_OF_WEEK

!------------------------------------------------------------------------------

      FUNCTION GET_GMT() RESULT( THISGMT )
!
!******************************************************************************
!  Function GET_GMT returns the current Greenwich Mean Time to the calling
!  program. (bmy, 2/5/03)
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      REAL*8 :: THISGMT

      !=================================================================
      ! GET_GMT begins here!
      !=================================================================
      THISGMT = GMT

      ! Return to calling program
      END FUNCTION GET_GMT

!------------------------------------------------------------------------------

      FUNCTION GET_TAU() RESULT( THISTAU )
!
!******************************************************************************
!  Function GET_TAU returns the current TAU (# of hours since 1 Jan 1985) 
!  value to the calling program. (bmy, 2/5/03)
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      REAL*8 :: THISTAU

      !=================================================================
      ! GET_TAUb begins here!
      !=================================================================
      THISTAU = TAU

      ! Return to calling program
      END FUNCTION GET_TAU

!------------------------------------------------------------------------------

      FUNCTION GET_TAUb() RESULT( THISTAUb )
!
!******************************************************************************
!  Function GET_TAUb returns TAUb (# of hours since 1 Jan 1985 at the
!  start of a GEOS-CHEM run) to the calling program. (bmy, 2/5/03)
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      REAL*8 :: THISTAUb

      !=================================================================
      ! GET_TAUb begins here!
      !=================================================================
      THISTAUb = TAUb

      ! Return to calling program
      END FUNCTION GET_TAUb

!------------------------------------------------------------------------------

      FUNCTION GET_TAUe() RESULT( THISTAUe )
!
!******************************************************************************
!  Function GET_TAUe returns TAUe (# of hours since 1 Jan 1985 at the 
!  end of a GEOS-CHEM run) to the calling program. (bmy, 2/5/03)
!
!  NOTES:  
!******************************************************************************
!
      ! Function value
      REAL*8 :: THISTAUe

      !=================================================================
      ! GET_TAUe begins here!
      !=================================================================
      THISTAUe = TAUe

      ! Return to calling program
      END FUNCTION GET_TAUe

      FUNCTION GET_SEASON() RESULT( THISSEASON )
!
!******************************************************************************
!  Function GET_SEASON returns the climatological season number 
!  (1=DJF, 2=MAM, 3=JJA, 4=SON) to the calling program. (bmy, 3/21/03)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) THISMONTH (INTEGER) : Current month (1-12)
!
!  NOTES:
!******************************************************************************
!      
      ! Function value
      INTEGER :: THISSEASON

      !=================================================================
      ! GET_SEASON begins here!
      !=================================================================
      THISSEASON = NSEASON

      ! Return to calling program
      END FUNCTION GET_SEASON

!------------------------------------------------------------------------------

      FUNCTION GET_TS_DYN() RESULT( THIS_TS_DYN )
!
!******************************************************************************
!  Function GET_TS_DIAG returns the diagnostic timestep in minutes to the
!  calling program. (bmy, 3/21/03)
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THIS_TS_DYN

      !=================================================================
      ! GET_TS_DYN begins here!
      !=================================================================
      THIS_TS_DYN = TS_DYN

      ! Return to calling program
      END FUNCTION GET_TS_DYN

!------------------------------------------------------------------------------

      FUNCTION GET_CT_DYN() RESULT( THIS_CT_DYN ) 
!
!******************************************************************************
!  Function GET_CT_CHEM returns the dynamic timestep counter to the
!  calling program. (bmy, 3/21/03)
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      INTEGER :: THIS_CT_DYN

      !=================================================================
      ! GET_CT_DYN begins here!
      !=================================================================
      THIS_CT_DYN = CT_DYN

      ! Return to calling program
      END FUNCTION GET_CT_DYN

!------------------------------------------------------------------------------

      FUNCTION ITS_TIME_FOR_DYN() RESULT( FLAG )
!
!******************************************************************************
!  Function ITS_TIME_FOR_DYN returns TRUE if it is time to do chemistry
!  and false otherwise. (bmy, 3/21/03)
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      LOGICAL :: FLAG

      !=================================================================
      ! ITS_TIME_FOR_DYN begins here!
      !=================================================================
      FLAG = ( MOD( ELAPSED_MIN, TS_DYN ) == 0 )

      ! Return to calling program
      END FUNCTION ITS_TIME_FOR_DYN

!------------------------------------------------------------------------------

      FUNCTION ITS_TIME_FOR_EXIT() RESULT( FLAG )
!
!******************************************************************************
!  Function ITS_TIME_FOR_EXIT returns TRUE if it is the end of the run 
!  (i.e. TAU >= TAUe) and false otherwise. (bmy, 3/21/03)
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      LOGICAL :: FLAG
      
      !=================================================================
      ! ITS_FOR_EXIT begins here!
      !=================================================================
      FLAG = ( TAU >= TAUe )

      ! Return to calling program
      END FUNCTION ITS_TIME_FOR_EXIT

!------------------------------------------------------------------------------

      FUNCTION ITS_A_LEAPYEAR( YEAR_IN, FORCE ) RESULT( IS_LEAPYEAR )
!
!******************************************************************************
!  Function ITS_A_LEAPYEAR tests to see if a year is really a leapyear. 
!  (bmy, 3/17/99, 4/24/06)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) YEAR_IN (INTEGER) : (OPTIONAL) Specify a year to test for leapyear
!  (2 ) FORCE   (LOGICAL) : (OPTIONAL) Do not exit if using GCAP met fields  
!
!  NOTES: 
!  (1 ) Now remove YEAR from ARG list; use the module variable (bmy, 3/21/03)
!  (2 ) Now add YEAR_IN as an optional argument.  If YEAR_IN is not passed,
!        then test if the current year is a leapyear (bmy, 9/25/03)
!  (3 ) Now always return FALSE for GCAP (swu, bmy, 8/29/05)
!  (4 ) Now add FORCE argument to force ITS_A_LEAPYEAR to return a value
!        instead of just returning with FALSE for the GCAP met fields.
!        (swu, bmy, 4/24/06)
!******************************************************************************
!
      ! Arguments
      INTEGER, INTENT(IN), OPTIONAL :: YEAR_IN
      LOGICAL, INTENT(IN), OPTIONAL :: FORCE

      ! Local variables
      INTEGER                       :: THISYEAR
      LOGICAL                       :: THISFORCE

      ! Function value
      LOGICAL                       :: IS_LEAPYEAR

      !=================================================================
      ! LEAPYEAR begins here!
      !=================================================================

      ! If YEAR_IN is passed, use that value; otherwise use the value 
      ! of the current year as stored in module variable YEAR.
      IF ( PRESENT( YEAR_IN ) ) THEN
         THISYEAR = YEAR_IN
      ELSE
         THISYEAR = YEAR
      ENDIF

      ! If FORCE is passed, use that value, otherwise default to .FALSE.
      IF ( PRESENT( FORCE ) ) THEN
         THISFORCE = FORCE
      ELSE
         THISFORCE = .FALSE.
      ENDIF

      !=================================================================
      ! A leap year is:
      ! (1) evenly divisible by 4 (if not a century year)
      ! (2) evenly divisible by 4, 100, and 400 (if a century year)
      !
      ! EXAMPLES:
      ! (a) 1992 is a leap year since it is evenly divisible by 4, 
      !     and is not a century year (i.e. it doesn't end in '00').
      !
      ! (b) 1900 is NOT a leap year, since while being evenly divisible 
      !     by 4 and 100, it is NOT divisible by 400.
      !
      ! (c) 2000 is a leap year, since it is divisible by 
      !     4, 100, and 400.
      !=================================================================
      IS_LEAPYEAR = .FALSE.

      IF ( MOD( THISYEAR, 4 ) == 0 ) THEN
         IF ( MOD( THISYEAR, 100 ) == 0 ) THEN
            IF ( MOD( THISYEAR, 400 ) == 0 ) THEN
               IS_LEAPYEAR = .TRUE.
            ENDIF
         ELSE
            IS_LEAPYEAR = .TRUE.
         ENDIF
      ENDIF

      ! Return to calling program
      END FUNCTION ITS_A_LEAPYEAR

!------------------------------------------------------------------------------

      FUNCTION ITS_A_NEW_YEAR() RESULT( IS_NEW_YEAR )
!
!******************************************************************************
!  Function ITS_A_NEW_YEAR returns TRUE if it's the first of a new month
!  (it also returns TRUE on the first timestep of the run).  This is useful
!  for setting flags for reading in data. (bmy, 4/1/04)
!
!  NOTES:
!  (1 ) Bug fix: Need month & day to be 1 (bmy, 11/1/05)
!******************************************************************************
!
      ! Function value
      LOGICAL :: IS_NEW_YEAR

      !=================================================================
      ! ITS_A_NEW_YEAR begins here!
      !=================================================================
      IF ( MONTH == 1 .and. DAY == 1 .and. NHMS == 000000 ) THEN

         ! A new year is Jan 1 at 0 GMT
         IS_NEW_YEAR = .TRUE.

      ELSE IF ( NYMD == NYMDb .and. NHMS == NHMSb ) THEN

         ! Also return TRUE if it's the start of the run
         ! (since files will need to be read in from disk)
         IS_NEW_YEAR = .TRUE.

      ELSE

         ! Otherwise, it's not a new year
         IS_NEW_YEAR = .FALSE.

      ENDIF

      ! Return to calling program
      END FUNCTION ITS_A_NEW_YEAR

!------------------------------------------------------------------------------

      FUNCTION ITS_A_NEW_MONTH() RESULT( IS_NEW_MONTH )
!
!******************************************************************************
!  Function ITS_A_NEW_MONTH returns TRUE if it's the first of a new month
!  (it also returns TRUE on the first timestep of the run).  This is useful
!  for setting flags for reading in data. (bmy, 4/1/04)
!
!  NOTES:
!******************************************************************************
!     
      ! Function value
      LOGICAL :: IS_NEW_MONTH
      
      !=================================================================
      ! ITS_A_NEW_MONTH begins here!
      !=================================================================
      IF ( DAY == 1 .and. NHMS == 000000 ) THEN
         
         ! Test for the 1st of the month at 0 GMT
         IS_NEW_MONTH = .TRUE.
      
      ELSE IF ( NYMD == NYMDb .and. NHMS == NHMSb ) THEN
         
         ! Also return TRUE if it's the start of the run
         ! (since files will need to be read in from disk)
         IS_NEW_MONTH = .TRUE.
      
      ELSE
         
         ! Otherwise, it's not a new year
         IS_NEW_MONTH = .FALSE.
         
      ENDIF
      
      ! Return to calling program
      END FUNCTION ITS_A_NEW_MONTH

!------------------------------------------------------------------------------
      
      FUNCTION ITS_MIDMONTH() RESULT( IS_MIDMONTH )
!
!******************************************************************************
!  Function ITS_MIDMONTH returns TRUE if it's the middle of a month
!  -sas 10/10/05
!
!  NOTES:
!******************************************************************************
!
      ! Function value
      LOGICAL :: IS_MIDMONTH

      !=================================================================
      ! ITS_MIDMONTH begins here!
      !=================================================================

      ! Test for the 16th of the month at 0 GMT
      IS_MIDMONTH = ( DAY == 16 .and. NHMS == 000000 )

      ! Return to calling program
      END FUNCTION ITS_MIDMONTH

!------------------------------------------------------------------------------

      FUNCTION ITS_A_NEW_DAY( ) RESULT( IS_NEW_DAY )
!
!******************************************************************************
!  Function ITS_A_NEW_DAY returns TRUE if it's the first timestep of a new
!  day (it also returns TRUE on the first timestep of the run).  This is 
!  useful for setting flags for reading in data. (bmy, 4/1/04)
!
!  NOTES:
!******************************************************************************
!
      ! Arguments
      LOGICAL :: IS_NEW_DAY

      !=================================================================
      ! ITS_A_NEW_DAY begins here!
      !=================================================================
      IF ( NHMS == 000000 ) THEN

         ! Test if it's 0 GMT
         IS_NEW_DAY = .TRUE.

      ELSE IF ( NYMD == NYMDb .and. NHMS == NHMSb ) THEN

         ! Also return TRUE if it's the start of the run
         ! (since files will need to be read in from disk)
         IS_NEW_DAY = .TRUE.

      ELSE

         ! Otherwise, it's not a new year
         IS_NEW_DAY = .FALSE.

      ENDIF

      ! Return to calling program
      END FUNCTION ITS_A_NEW_DAY

!------------------------------------------------------------------------------

      FUNCTION ITS_A_NEW_SEASON( ) RESULT( IS_NEW_SEASON )
!
!******************************************************************************
!  Function ITS_A_NEW_SEASON returns TRUE if it's a new season or FALSE
!  if it's not a new season.  Seasons are (1=DJF, 2=MAM, 3=JJA, 4=SON).
!  (bmy, 7/20/04)
!
!  NOTES:
!******************************************************************************
!     
      ! Function value 
      LOGICAL       :: IS_NEW_SEASON
      
      ! Local variables
      INTEGER, SAVE :: LAST_SEASON = -1
      
      !=================================================================
      ! ITS_A_NEW_SEASON begins here!
      !=================================================================
      IF ( NSEASON /= LAST_SEASON ) THEN
         IS_NEW_SEASON = .TRUE.
         LAST_SEASON   = NSEASON
      ELSE
         IS_NEW_SEASON = .FALSE.
      ENDIF
      
      ! Return to calling program
      END FUNCTION ITS_A_NEW_SEASON

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
      ! End of module
      END MODULE TIME_MOD
