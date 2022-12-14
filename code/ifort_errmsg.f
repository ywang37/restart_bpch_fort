! $Id: ifort_errmsg.f,v 1.2 2006/04/21 15:40:01 bmy Exp $
      FUNCTION IFORT_ERRMSG( ERROR_NUM ) RESULT( MSG )
!
!******************************************************************************
!  Function IFORT_ERRMSG returns an error message string that corresponds
!  to an I/O error number obtained via the IOSTAT or STAT specifiers 
!  (bmy, 11/30/05)
!
!  Arguments as Input:
!  ============================================================================
!  (1 ) ERROR_NUM (INTEGER) : Error condition from IOSTAT
!
!  Reference:
!  ============================================================================
!  (1 ) Intel Fortran Language Reference, v9.0
!
!  NOTES:
!******************************************************************************
!      
      ! Arguments
      INTEGER, INTENT(IN) :: ERROR_NUM

      ! Local variables
      CHARACTER(LEN=255)  :: MSG

      !=================================================================
      ! IFORT_ERRMSG begins here!
      !=================================================================

      ! Select a error message based on the error codes 
      ! for Intel Fortran Compiler v9.0.
      SELECT CASE( ERROR_NUM ) 
         CASE( 0   ) 
            MSG = ''
         CASE( 1   ) 
            MSG = 'Not a Fortran-specific error'
         CASE( 8   )
            MSG = 'Internal consistency check failure'
         CASE( 9   )
            MSG = 'Permission to access file denied'
         CASE( 10  )
            MSG = 'Cannot overwrite existing file'
         CASE( 11  )
            MSG = 'Unit not connected'
         CASE( 17  )
            MSG = 'Syntax error in NAMELIST input'
         CASE( 18  )
            MSG = 'Too many values for NAMELIST variable'
         CASE( 19  )
            MSG = 'Invalid reference to variable in NAMELIST input'
         CASE( 20  )
            MSG = 'REWIND error'
         CASE( 21  )
            MSG = 'Duplicate file specifications'
         CASE( 22  )
            MSG = 'Input record too long'
         CASE( 23  )
            MSG = 'Backspace error'
         CASE( 24  )
            MSG = 'END-OF-FILE during read'
         CASE( 25  )
            MSG = 'Record number outside range'
         CASE( 26  )
            MSG = 'OPEN or DEFINE FILE required'
         CASE( 27  )
            MSG = 'Too many records in I/O statement'
         CASE( 28  )
            MSG = 'CLOSE error'
         CASE( 29 )
            MSG = 'File not found'
         CASE( 30 )
            MSG = 'OPEN failure'
         CASE( 31  )
            MSG = 'Mixed file access modes'
         CASE( 32  )
            MSG = 'Invalid logical unit number'
         CASE( 33  )
            MSG = 'ENDFILE error'
         CASE( 34  )
            MSG = 'Unit already open'
         CASE( 35  )
            MSG = 'Segmented record format error'
         CASE( 36  )
            MSG = 'Attempt to access non-existent record'
         CASE( 37  )
            MSG = 'Inconsistent record length'
         CASE( 38  )
            MSG = 'Error during write'
         CASE( 39  )
            MSG = 'Error during read'
         CASE( 40  )
            MSG = 'Recursive I/O operation'
         CASE( 41  )
            MSG = 'Insufficient virtual memory'
         CASE( 42  )
            MSG = 'No such device'
         CASE( 43  )
            MSG = 'File name specification error'
         CASE( 44  )
            MSG = 'Inconsistent record type'
         CASE( 45  )
            MSG = 'Keyword value error in OPEN statement'
         CASE( 46  )
            MSG = 'Inconsistent OPEN/CLOSE parameters'
         CASE( 47  )
            MSG = 'Write to READONLY file'
         CASE( 48  )
            MSG = 'Invalid argument to Fortran Run-Time Library'
         CASE( 51 )
            MSG = 'Inconsistent file organization'
         CASE( 53  )
            MSG = 'No current record'
         CASE( 55  )
            MSG = 'DELETE error'
         CASE( 57  )
            MSG = 'FIND error'
         CASE( 58  )
            MSG = 'Format syntax error'
         CASE( 59  )
            MSG = 'List-directed I/O syntax error'
         CASE( 60  )
            MSG = 'Infinite format loop'
         CASE( 61 )
            MSG = 'Format/variable type mismatch'
         CASE( 62  )
            MSG = 'Syntax error in format'
         CASE( 63 )
            MSG = 'Output conversion error'
         CASE( 64  )
            MSG = 'Input conversion error'
         CASE( 65  )
            MSG = 'Floating invalid'
         CASE( 66  )
            MSG = 'Output statement overflows record'
         CASE( 67  )
            MSG = 'Input statement requires too much data'
         CASE( 68  )
            MSG = 'Variable format expression value error'
         CASE( 69  )
            MSG = 'Process interrupted (SIGINT)'
         CASE( 70  )
            MSG = 'Integer overflow'
         CASE( 71  )
            MSG = 'Integer divide by zero'
         CASE( 72  )
            MSG = 'Floating overflow'
         CASE( 73  )
            MSG = 'Floating divide by zero'
         CASE( 74  )
            MSG = 'Floating underflow'
         CASE( 75  )
            MSG = 'Floating point exception'
         CASE( 76 )
            MSG = 'IOT trap signal'
         CASE( 77  )
            MSG = 'Subscript out of range'
         CASE( 78  )
            MSG = 'Process killed (SIGTERM)'
         CASE( 79  )
            MSG = 'Process quit (SIGQUIT)'
         CASE( 95 )
            MSG = 'Floating-point conversion failed'
         CASE( 96 )
            MSG = 'F_UFMTENDIAN env variable was ignored: bad syntax'
         CASE( 108 )
            MSG = 'Cannot stat file'
         CASE( 120 )
            MSG = 'Operation requires seek ability'
         CASE( 138 )
            MSG = 'Array index out of bounds (SIGILL)'
         CASE( 139 )
            MSG = 'Array index out of bounds'
         CASE( 140 )
            MSG = 'Floating inexact'
         CASE( 144 )
            MSG = 'Reserved operand'
         CASE( 145 )
            MSG = 'Assertion error'
         CASE( 146 )
            MSG = 'Null pointer error'
         CASE( 147 )
            MSG = 'Stack overflow'
         CASE( 148 )
            MSG = 'String length error'
         CASE( 149 )
            MSG = 'Substring error'
         CASE( 150 )
            MSG = 'Range error'
         CASE( 151 )
            MSG = 'Allocatable array is already allocated'
         CASE( 152 )
            MSG = 'Unresolved contention for RTL global resource'
         CASE( 153 )
            MSG = 'Allocatable array or pointer is not allocated'
         CASE( 173 )
            MSG = 'A pointer passed to DEALLOCATE points to an array'
            MSG = TRIM( MSG ) // ' that cannot be deallocated'
         CASE( 174 )
            MSG = 'SIGSEGV: seg fault or program stack overflow'
         CASE( 175 )
            MSG = 'DATE argument to DATE_AND_TIME is too short,'
            MSG = TRIM( MSG ) // ' required LEN=8'
         CASE( 176 )
            MSG = 'TIME argument to DATE_AND_TIME is too short,'
            MSG = TRIM( MSG ) // ' required LEN=10'
         CASE( 177 )
            MSG = 'ZONE argument to DATE_AND_TIME is too short,'
            MSG = TRIM( MSG ) // ' required LEN=5'
         CASE( 178 )
            MSG = 'Divide by zero'
         CASE( 179 )
            MSG = 'Cannot allocate array:'
            MSG = TRIM( MSG ) // ' overflow in array size calculation'
         CASE( 256 )
            MSG = 'Unformatted I/O to unit open for formatted transfers'
         CASE( 257 )
            MSG = 'Formatted I/O to unit open for unformatted transfers'
         CASE( 264 )
            MSG = 'Operation requires file to be on disk or tape'
         CASE( 265 )
            MSG = 'Operation requires sequential file organization'
            MSG = TRIM( MSG ) // ' and access'
         CASE( 266 )
            MSG = 'Fortran abort routine called'
         CASE( 268 )
            MSG = 'End of record during read'         
         CASE( 269 )
            MSG = 'Floating invalid traps'
         CASE( 298 )
            MSG = 'Floating overflow traps'
         CASE( 299 )
            MSG = 'Divide-by-zero traps'
         CASE( 300 )
            MSG = 'Floating underflow traps'
         CASE DEFAULT
            MSG = 'Unknown error'
      END SELECT

      ! Return to calling program
      END FUNCTION IFORT_ERRMSG
