!==============================================================================
! Module ERROR_MODULE                                             (22-Jun-2017)
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
MODULE ERROR_MODULE
!==============================================================================
   USE CONSTANTS_MODULE
   USE UTILITIES_MODULE
   IMPLICIT NONE

   !---------------------------------------------------------------------------
   ! Module parameters
   !---------------------------------------------------------------------------
   INTEGER, PARAMETER :: NO_ERROR               = 0
   INTEGER, PARAMETER :: UNKNOWN_ERROR          = 1

   INTEGER, PARAMETER :: AQUIFER_ERROR          = 11
   INTEGER, PARAMETER :: ASCII_ERROR            = 12
   INTEGER, PARAMETER :: BUFFERZONE_ERROR       = 13
   INTEGER, PARAMETER :: CAPTUREZONE_ERROR      = 14
   INTEGER, PARAMETER :: CONDUCTIVITY_ERROR     = 15
   INTEGER, PARAMETER :: ESRI_ERROR             = 16
   INTEGER, PARAMETER :: FILE_ERROR             = 17
   INTEGER, PARAMETER :: OBS_ERROR              = 18
   INTEGER, PARAMETER :: SURFER_ERROR           = 19
   INTEGER, PARAMETER :: THICKNESS_ERROR        = 20
   INTEGER, PARAMETER :: TRACK_ERROR            = 21
   INTEGER, PARAMETER :: VERBOSITY_ERROR        = 22
   INTEGER, PARAMETER :: WELL_ERROR             = 23

   INTEGER, PARAMETER :: DATA_ERROR             = 101
   INTEGER, PARAMETER :: A_ISNAN_ERROR          = 102
   INTEGER, PARAMETER :: B_ISNAN_ERROR          = 103
   INTEGER, PARAMETER :: A_ISSINGULAR_ERROR     = 104
   INTEGER, PARAMETER :: INVALID_PARAM_ERROR    = 105
   INTEGER, PARAMETER :: A_ISNOTPD_ERROR        = 106

   INTEGER, PARAMETER :: AQUIFER_NOT_PREPARED_ERROR      = 201
   INTEGER, PARAMETER :: CONDUCTIVITY_NOT_PREPARED_ERROR = 202
   INTEGER, PARAMETER :: THICKNESS_NOT_PREPARED_ERROR    = 203
   INTEGER, PARAMETER :: GRID_NOT_PREPARED_ERROR         = 204

   !===========================================================================
   ! INTERFACES
   !===========================================================================

   INTERFACE CommandError
      MODULE PROCEDURE CommandError
   END INTERFACE

   INTERFACE FileError
      MODULE PROCEDURE FileError
   END INTERFACE

   INTERFACE ExecutionError
      MODULE PROCEDURE ExecutionError
   END INTERFACE

CONTAINS
   !---------------------------------------------------------------------------
   ! CommandError
   !---------------------------------------------------------------------------
   SUBROUTINE CommandError( ErrNo, LineNo, Line, FileName )
      ! Declare the arguments.
      INTEGER,      INTENT(IN) :: ErrNo
      INTEGER,      INTENT(IN) :: LineNo
      CHARACTER(*), INTENT(IN) :: Line
      CHARACTER(*), INTENT(IN) :: FileName

      ! Process the error.
      IF( ErrNo .GT. 0 ) THEN
         WRITE(LUNIT,*)
         WRITE(LUNIT,*) ' Input error on line ', LineNo, ' in ', FileName(1:LEN_TRIM(FileName))
         WRITE(LUNIT,*) Line(1:LEN_TRIM(Line))
         WRITE(LUNIT,*)

         WRITE(*,*)
         WRITE(*,*) ' Input error on line ', LineNo, ' in ', FileName(1:LEN_TRIM(FileName))
         WRITE(*,*) Line(1:LEN_TRIM(Line))
         WRITE(*,*)
      END IF

      ! Print out specifiic error information.
      SELECT CASE( ErrNo )
         CASE (AQUIFER_NOT_PREPARED_ERROR)
            WRITE(LUNIT,*) 'Error Number ', ErrNo
            WRITE(LUNIT,*) 'The porosity must be specified before creating a capture zone.'
            WRITE(LUNIT,*) 'An AQUIFER command is required.'
            WRITE(LUNIT,*)

            WRITE(*,*) 'Error Number ', ErrNo
            WRITE(*,*) 'The porosity must be specified before creating a capture zone.'
            WRITE(*,*) 'An AQUIFER command is required.'
            WRITE(*,*)

         CASE (CONDUCTIVITY_NOT_PREPARED_ERROR)
            WRITE(LUNIT,*) 'Error Number ', ErrNo
            WRITE(LUNIT,*) 'The conductivity must be specified before creating a capture zone.'
            WRITE(LUNIT,*) 'A CONDUCTIVITY or CONDUCTIVITYDISTRIBUTION command is required.'
            WRITE(LUNIT,*)

            WRITE(*,*) 'Error Number ', ErrNo
            WRITE(*,*) 'The conductivity must be specified before creating a capture zone.'
            WRITE(*,*) 'A CONDUCTIVITY or CONDUCTIVITYDISTRIBUTION command is required.'
            WRITE(*,*)

         CASE (GRID_NOT_PREPARED_ERROR)
            WRITE(LUNIT,*) 'Error Number ', ErrNo
            WRITE(LUNIT,*) 'The grid must be specified before creating a capture zone.'
            WRITE(LUNIT,*) 'An ASCII, ESRI, or SURFER command is required.'
            WRITE(LUNIT,*)

            WRITE(*,*) 'Error Number ', ErrNo
            WRITE(*,*) 'The grid must be specified before creating a capture zone.'
            WRITE(*,*) 'An ASCII, ESRI, or SURFER command is required.'
            WRITE(*,*)

         CASE (THICKNESS_NOT_PREPARED_ERROR)
            WRITE(LUNIT,*) 'Error Number ', ErrNo
            WRITE(LUNIT,*) 'The thickness must be specified before creating a capture zone.'
            WRITE(LUNIT,*) 'A THICKNESS or THICKNESSDISTRIBUTION command is required.'
            WRITE(LUNIT,*)

            WRITE(*,*) 'Error Number ', ErrNo
            WRITE(*,*) 'The thickness must be specified before creating a capture zone.'
            WRITE(*,*) 'A THICKNESS or THICKNESSDISTRIBUTION command is required.'
            WRITE(*,*)

         CASE DEFAULT
            WRITE(LUNIT,*) 'Error Number ', ErrNo
            WRITE(LUNIT,*)

            WRITE(*,*) 'Error Number ', ErrNo
            WRITE(*,*)
      END SELECT
   END SUBROUTINE CommandError

   !---------------------------------------------------------------------------
   ! FileError
   !---------------------------------------------------------------------------
   SUBROUTINE FileError( FileName )
      ! Declare the arguments.
      CHARACTER(*), INTENT(IN) :: FileName

      ! Process the error.
      WRITE(LUNIT,*)
      WRITE(LUNIT,*) ' Can not open output file: ', FileName
      WRITE(LUNIT,*)

      WRITE(*,*)
      WRITE(*,*) ' Can not open output file: ', FileName
      WRITE(*,*)
   END SUBROUTINE FileError

   !---------------------------------------------------------------------------
   ! ExecutionError
   !---------------------------------------------------------------------------
   SUBROUTINE ExecutionError( ErrNo, FileName, FunctionName )
      ! Declare the arguments.
      INTEGER,      INTENT(IN) :: ErrNo
      CHARACTER(*), INTENT(IN) :: FileName
      CHARACTER(*), INTENT(IN) :: FunctionName

      ! Print out specifiic error information.
      SELECT CASE( ErrNo )
         CASE (DATA_ERROR)
            WRITE(LUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(LUNIT,*) 'There are too few (<6) active OBS to proceed.'
            WRITE(LUNIT,*)

            WRITE(SUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(SUNIT,*) 'There are too few (<6) active OBS to proceed.'
            WRITE(SUNIT,*)

            CALL Terminate( ErrNo )

         CASE (A_ISNAN_ERROR)
            WRITE(LUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(LUNIT,*) 'Matrix <A> is NAN.'
            WRITE(LUNIT,*)

            WRITE(SUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(SUNIT,*) 'Matrix <A> is NAN.'
            WRITE(SUNIT,*)

            CALL Terminate( ErrNo )

         CASE (B_ISNAN_ERROR)
            WRITE(LUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(LUNIT,*) 'Matrix <b> is NAN. '
            WRITE(LUNIT,*)

            WRITE(SUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(SUNIT,*) 'Matrix <b> is NAN. '
            WRITE(SUNIT,*)

            CALL Terminate( ErrNo )

         CASE (A_ISSINGULAR_ERROR)
            WRITE(LUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(LUNIT,*) 'Matrix <A> is singular.'
            WRITE(LUNIT,*)

            WRITE(SUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(SUNIT,*) 'Matrix <A> is singular.'
            WRITE(SUNIT,*)

            CALL Terminate( ErrNo )

         CASE (INVALID_PARAM_ERROR)
            WRITE(LUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(LUNIT,*) 'Invalid parameters.'
            WRITE(LUNIT,*)

            WRITE(SUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(SUNIT,*) 'Invalid parameters.'
            WRITE(SUNIT,*)

            CALL Terminate( ErrNo )

         CASE (A_ISNOTPD_ERROR)
            WRITE(LUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(LUNIT,*) ' Matrix <A> is not positive definite. '
            WRITE(LUNIT,*)

            WRITE(SUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(SUNIT,*) ' Matrix <A> is not positive definite. '
            WRITE(SUNIT,*)

            CALL Terminate( ErrNo )

         CASE DEFAULT
            WRITE(LUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(LUNIT,*) 'Error Number ', ErrNo
            WRITE(LUNIT,*)

            WRITE(SUNIT,*) 'FATAL ERROR: ', FileName, '|', FunctionName
            WRITE(SUNIT,*) 'Error Number ', ErrNo
            WRITE(SUNIT,*)

            CALL Terminate( ErrNo )
      END SELECT
   END SUBROUTINE ExecutionError


!==============================================================================
END MODULE ERROR_MODULE
!==============================================================================
