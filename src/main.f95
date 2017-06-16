!==============================================================================
! main.f95                                                        (16-Jun-2017)
!
! Notes:
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
! PROGRAM main
!==============================================================================
PROGRAM main
   USE DRIVER_MODULE
   USE MODEL_MODULE
   IMPLICIT NONE

   ! Declare local parameters.
   INTEGER, PARAMETER :: MAX_LINES   = 5000  ! maximum # of input lines
   INTEGER, PARAMETER :: BUFFER_SIZE = 256   ! maximum line length

   ! Declare local variables and parameters.
   INTEGER                :: LineNo, ErrNo
   CHARACTER(BUFFER_SIZE) :: Line, Command, Args

   CHARACTER(50)  :: InputFile, LogFile
   TYPE(T_MODEL)  :: Model

   ! Open the files, and write out the banner.
   WRITE(SUNIT,*)
   CALL WriteBanner( SUNIT )

   CALL GetFileNames( LogFile, InputFile )
   OPEN( UNIT=IUNIT, FILE=InputFile, ACTION='READ'  )
   OPEN( UNIT=LUNIT, FILE=LogFile,   ACTION='WRITE', RECL=BUFFER_SIZE )

   CALL WriteBanner( LUNIT )

   ! Initialize stuff.
   CALL Initiate

   ! Initialize the random number generator using the fixed default setting.
   CALL RANDOMIZE( .FALSE. )

   ! Process the input file one line at a time.
   WRITE(LUNIT,*) 'Processing commands'
   WRITE(LUNIT,*)

   WRITE(SUNIT,*) 'Processing commands'
   WRITE(SUNIT,*)

   DO LineNo = 1, MAX_LINES
      ! Read in the next line.
      READ(IUNIT,'(A256)',END=10) Line

      ! Parse the command.
      CALL ParseCommand( Line, Command, Args )

      IF( LEN_TRIM( Command ) .GT. 0 ) THEN

         ! Echo the input line to the log file.
         WRITE(LUNIT,*) Command(1:LEN_TRIM(Command)), ' ', Args(1:LEN_TRIM(Args))

         ! Attempt to process the command.
         CALL ProcessCommand( Command, Args, Model, Errno )

         IF( ErrNo .GT. 0 ) THEN
            CALL CommandError( ErrNo, LineNo, Line, InputFile )
            EXIT
         ELSE IF( ErrNo .LT. 0 ) THEN
            EXIT
         END IF
      END IF
   END DO

   ! Report the elapsed time and stop.
10 CALL Terminate( ErrNo )
   STOP
END PROGRAM main
