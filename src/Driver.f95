!==============================================================================
! Driver                                                          (28-Jun-2017)
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
MODULE DRIVER_MODULE
!==============================================================================
   USE CONSTANTS_MODULE
   USE ERROR_MODULE
   IMPLICIT NONE

CONTAINS

   !==============================================================================
   ! GetFileNames
   !==============================================================================
   SUBROUTINE GetFileNames( LogFile, OneFile )
      IMPLICIT NONE

      ! Declare the arguments.
      CHARACTER(*), INTENT(OUT) :: OneFile
      CHARACTER(*), INTENT(OUT) :: LogFile

      ! Declare the local variables.
      CHARACTER(50) :: Filename
      LOGICAL       :: Exist

      ! Process the command line.
      IF( COMMAND_ARGUMENT_COUNT() .EQ. 1 ) THEN
         CALL GETARG( 1, Filename )

         Filename = ADJUSTL(FIlename)
         OneFile = Filename(1:LEN_TRIM(Filename)) // '.one'

         ! Check that the specified file exists.
         INQUIRE( FILE=OneFile, EXIST=Exist )
         IF( .NOT. Exist ) THEN
            WRITE(*,*)
            WRITE(*,*) '    WARNING: Can not find: ', ADJUSTL(OneFile(1:LEN_TRIM(OneFile)))
            WRITE(*,*)
         ELSE
            LogFile = Filename(1:LEN_TRIM(Filename)) // '_log.txt'
            RETURN
         END IF
      END IF

      ! Loop until an input file has been specified and found.
      DO
         ! Question the user for the command filename.
         WRITE(*,'(A30)', ADVANCE='NO') ' Enter the command file name: '
         READ(*,*) Filename

         Filename = ADJUSTL(FIlename)
         OneFile = Filename(1:LEN_TRIM(Filename)) // '.one'

         ! Check that the specified file exists.
         INQUIRE( FILE=OneFile, EXIST=Exist )
         IF( .NOT. Exist ) THEN
            WRITE(*,*)
            WRITE(*,*) '    WARNING: Can not find: ', ADJUSTL(OneFile(1:LEN_TRIM(OneFile)))
            WRITE(*,*)
         ELSE
            LogFile = Filename(1:LEN_TRIM(Filename)) // '_log.txt'
            EXIT
         END IF
      END DO

   END SUBROUTINE GetFileNames


   !==============================================================================
   ! ParseCommand
   !==============================================================================
   SUBROUTINE ParseCommand( Line, Command, Buffer )
      USE UTILITIES_MODULE
      IMPLICIT NONE

      ! Declare the arguments.
      CHARACTER(*), INTENT(IN)  :: Line
      CHARACTER(*), INTENT(OUT) :: Command
      CHARACTER(*), INTENT(OUT) :: Buffer

      ! Declare local variables.
      INTEGER :: i

      ! Trim leading blanks.
      Buffer = ADJUSTL( Line )

      ! Trim trailing comments.
      i = SCAN( Buffer, '!' )
      IF( i .GT. 0 ) THEN
         Buffer = Buffer(:i-1)
      END IF

      ! Extract the Command (delimited by whitespace).
      IF( LEN_TRIM( Buffer ) .GT. 0 ) THEN
         i = SCAN( Buffer, ' ' )
         IF( i .GT. 0 ) THEN
            Command = Buffer(:i-1)
            Buffer  = ADJUSTL( Buffer(i+1:) )
         ELSE
            Command = Buffer
            Buffer  = ' '
         END IF
         CALL ToUpper( Command )
      ELSE
         Command = ' '
      END IF
   END


   !==============================================================================
   ! ProcessCommand
   !
   ! Notes:
   !
   ! o   Comments are delimited by a prefix "!" character.  Any input beyond
   !     the first "!" in a line is ignored.
   !
   ! o   Available commands:
   !
   !        AQUIFER                    (Base) (Porosity)
   !        BUFFERZONE                 (Radius)
   !        CAPTUREZONE                (Dur) (Step) (Xo) (Yo) (Radius) (Width) (maxSep) (nSim) (Filename)
   !        CONDUCTIVITY               (k) (Weight)
   !        CONDUCTIVITYDISTRIBUTION   (Dist) (Avg) (Std) (Zmin) (Zmax) (Count)
   !        ESRI                       (nRows) (nCols) (Xmin) (Ymin) (Cellsize) (mRows) (mCols)
   !        MESSAGE                    (Text)
   !        OBS                        (X) (Y) (Zavg) (Zstd)
   !        RANDOMIZE
   !        RESET
   !        STOP
   !        SURFER                     (nRows) (nCols) (Xmin) (Xmax) (Ymin) (Ymax) (mRows) (mCols)
   !        THICKNESS                  (Thickness) (Weight)
   !        THICKNESSDISTRIBUTION      (Dist) (Avg) (Std) (Zmin) (Zmax) (Count)
   !        VERBOSITY                  (Level)
   !        WELL                       (X) (Y) (Radius) (Q)
   !
   !==============================================================================
   SUBROUTINE ProcessCommand( Command, Buffer, Model, ErrNo )
      USE MODEL_MODULE
      USE ERROR_MODULE
      USE NUMERIC_MODULE
      USE UTILITIES_MODULE
      IMPLICIT NONE

      ! Declare arguments
      CHARACTER(*),  INTENT(IN)    :: Command
      CHARACTER(*),  INTENT(IN)    :: Buffer
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Declare local variables and parameters.
      INTEGER, PARAMETER     :: BUFFER_SIZE = 256   ! maximum line length
      CHARACTER(BUFFER_SIZE) :: Distribution, Args

      CHARACTER(50)          :: Filename

      INTEGER                :: nRows, nCols, mRows, mCols
      INTEGER                :: Count, nSims, i, Level

      REAL(8)                :: Xmin, Xmax, Ymin, Ymax, Cellsize
      REAL(8)                :: Base, Thickness, Porosity
      REAL(8)                :: Duration, Step, Xo, Yo, Radius, Width, maxSep
      REAL(8)                :: Conductivity, Weight
      REAL(8)                :: X, Y, Zavg, Zstd, Q
      REAL(8)                :: dZ, Zmin, Zmax, Zlower, Zupper, Zscore
      REAL(8)                :: Mu, Sigma

      ! Process the Commands.

      !-----------------------
      ! AQUIFER (Base) (Porosity)
      !-----------------------
      IF( Command .EQ. 'AQUIFER' ) THEN
         ErrNo = AQUIFER_ERROR

         READ( Buffer, *, ERR=10 ) Base, Porosity
         ! WRITE(LUNIT,*) 'AQUIFER ', Base, Porosity

         IF( Porosity .LE. 0 .OR. Porosity .GE. 1 ) GOTO 10

         CALL SetProperties( Model, Base, Porosity, ErrNo )

      !-----------------------
      ! ASCII (nRows) (nCols) (Xmin) (Xmax) (Ymin) (Ymax) (mRows) (mCols)
      !-----------------------
      ELSE IF( Command .EQ. 'ASCII' ) THEN
         ErrNo = ASCII_ERROR

         READ( Buffer, *, ERR=10 ) nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols
         ! WRITE(LUNIT,*) 'ASCII ', nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols

         IF( nRows .LE. 0 .OR. nCols .LE. 0 .OR. Xmin .GT. Xmax .OR. Ymin .GT. Ymax .OR. mRows .LT. nRows .OR. mCols .LT. nCols ) GOTO 10

         CALL SetAsciiGrid( Model, nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols, ErrNo )

      !-----------------------
      ! BUFFERZONE (Radius)
      !-----------------------
      ELSE IF( Command .EQ. 'BUFFERZONE' ) THEN
         ErrNo = BUFFERZONE_ERROR

         READ( Buffer, *, ERR=10 ) Radius
         ! WRITE(LUNIT,*) 'BUFFERZONE ', Radius

         IF( Radius .LT. real(0,8) ) GOTO 10

         CALL SetBufferZone( Model, Radius, ErrNo )

      !-----------------------
      ! CAPTUREZONE (Dur) (Step) (Xo) (Yo) (Radius) (Width) (maxSep) (nSims) (Filename)
      !-----------------------
      ELSE IF( Command .EQ. 'CAPTUREZONE' ) THEN
         ErrNo = CAPTUREZONE_ERROR

         READ( Buffer, *, ERR=10 ) Duration, Step, Xo, Yo, Radius, Width, maxSep, nSims, Filename
         ! WRITE(LUNIT,*) 'CAPTUREZONE ', Duration, Step, Xo, Yo, Radius, Width, maxSep, nSims, ADJUSTL(TRIM(Filename))

         IF( Duration .LT. 0 .OR. Step .LE. 0 .OR. Radius .LE. 0 .OR. Width .LE. 0 .OR. maxSep .LE. 0 .OR. nSims .LT. 1 ) GOTO 10

         CALL SetTrack( Model, Duration, Step, Xo, Yo, Radius, Width, maxSep, ErrNo )
         CALL CreateCaptureZone( Model, nSims, ADJUSTL(Filename), ErrNo )

      !-----------------------
      ! CONDUCTIVITY (k) (Weight)
      !-----------------------
      ELSE IF( Command .EQ. 'CONDUCTIVITY' ) THEN
         ErrNo = CONDUCTIVITY_ERROR

         READ( Buffer, *, ERR=10 ) Conductivity, Weight
         ! WRITE(LUNIT,*) 'CONDUCTIVITY ', Conductivity, Weight

         IF( Conductivity .LE. 0 .OR. Weight .LT. 0 ) GOTO 10

         CALL AddConductivity( Model, Conductivity, Weight, ErrNo )

      !-----------------------
      ! CONDUCTIVITYDISTRIBUTION (Dist) (Avg) (Std) (Zmin) (Zmax) (Count)
      !-----------------------
      ELSE IF( Command .EQ. 'CONDUCTIVITYDISTRIBUTION' ) THEN
         ErrNo = CONDUCTIVITY_ERROR

         Call ParseCommand( Buffer, Distribution, Args )
         Call ToUpper( Distribution )

         READ( Args, *, ERR=10 ) Mu, Sigma, Zmin, Zmax, Count
         ! WRITE(LUNIT,*) 'CONDUCTIVITYDISTRIBUTION ', ADJUSTL(TRIM(Distribution)), Mu, Sigma, Zmin, Zmax, Count

         IF( Sigma .LT. 0 .OR. Zmin .GT. Zmax .OR. Count .LT. 1 ) GOTO 10

         dZ = (Zmax - Zmin)/Count

         DO i = 1, Count
            Zlower = Zmin + (i-1)*dZ
            Zupper = Zmin + (i)*dZ
            Zscore = ( Zupper + Zlower ) / 2

            Weight = GaussianCDF( Zupper ) - GaussianCDF( Zlower )

            IF( Distribution .EQ. 'NORMAL' ) THEN
               Conductivity = Mu + Zscore*Sigma
            ELSEIF( Distribution .EQ. 'LOGNORMAL' ) THEN
               Conductivity = EXP( Mu + Zscore*Sigma )
            ELSE
               GOTO 10
            END IF

            IF( Conductivity .GT. 0 ) THEN
               CALL AddConductivity( Model, Conductivity, Weight, ErrNo )
            END IF
         END DO

      !-----------------------
      ! ESRI (nRows) (nCols) (Xmin) (Ymin) (Cellsize) (mRows) (mCols)
      !-----------------------
      ELSE IF( Command .EQ. 'ESRI' ) THEN
         ErrNo = ESRI_ERROR

         READ( Buffer, *, ERR=10 ) nRows, nCols, Xmin, Ymin, Cellsize, mRows, mCols
         ! WRITE(LUNIT,*) 'ESRI ', nRows, nCols, Xmin, Ymin, Cellsize, mRows, mCols

         IF( nRows .LE. 0 .OR. nCols .LE. 0 .OR. Cellsize .LE. 0 .OR. mRows .LT. nRows .OR. mCols .LT. nCols ) GOTO 10

         CALL SetEsriGrid( Model, nRows, nCols, Xmin, Ymin, Cellsize, mRows, mCols, ErrNo )

      !-----------------------
      ! MESSAGE (Text)
      !-----------------------
      ELSE IF( Command .EQ. 'MESSAGE' ) THEN
         ! WRITE(LUNIT,*) 'MESSAGE ', ADJUSTL(TRIM(Buffer))
         WRITE(*,*) ADJUSTL( TRIM( Buffer ) )
         ErrNo = 0

      !-----------------------
      ! OBS (X) (Y) (Zavg) (Zstd)
      !-----------------------
      ELSE IF( Command .EQ. 'OBS' ) THEN
         ErrNo = OBS_ERROR

         READ( Buffer, *, ERR=10 ) X, Y, Zavg, Zstd
         ! WRITE(LUNIT,*) 'OBS ', X, Y, Zavg, Zstd

         IF( Zstd .LT. 0 ) GOTO 10

         CALL AddObs( Model, X, Y, Zavg, Zstd, ErrNo )

      !-----------------------
      ! RANDOMIZE
      !-----------------------
      ELSE IF( Command .EQ. 'RANDOMIZE' ) THEN
         ! WRITE(LUNIT,*) 'RANDOMIZE '

         CALL Randomize( .TRUE. )
         ErrNo = 0

      !-----------------------
      ! RESET
      !-----------------------
      ELSE IF( Command .EQ. 'RESET' ) THEN
         ! WRITE(LUNIT,*) 'RESET '

         CALL ResetAll( Model )
         ErrNo = 0

      !-----------------------
      ! STOP
      !-----------------------
      ELSE IF( Command .EQ. 'STOP' ) THEN
         ! WRITE(LUNIT,*) 'STOP '
         ErrNo = -1

      !-----------------------
      ! SURFER (nRows) (nCols) (Xmin) (Xmax) (Ymin) (Ymax) (mRows) (mCols)
      !-----------------------
      ELSE IF( Command .EQ. 'SURFER' ) THEN
         ErrNo = SURFER_ERROR

         READ( Buffer, *, ERR=10 ) nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols
         ! WRITE(LUNIT,*) 'SURFER ', nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols

         IF( nRows .LE. 0 .OR. nCols .LE. 0 .OR. Xmin .GT. Xmax .OR. Ymin .GT. Ymax .OR. mRows .LT. nRows .OR. mCols .LT. nCols ) GOTO 10

         CALL SetSurferGrid( Model, nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols, ErrNo )

      !-----------------------
      ! THICKNESS (Thickness) (Weight)
      !-----------------------
      ELSE IF( Command .EQ. 'THICKNESS' ) THEN
         ErrNo = THICKNESS_ERROR

         READ( Buffer, *, ERR=10 ) Thickness, Weight
         ! WRITE(LUNIT,*) 'THICKNESS ', Thickness, Weight

         IF( Thickness .LE. 0 .OR. Weight .LT. 0 ) GOTO 10

         CALL AddThickness( Model, Thickness, Weight, ErrNo )

      !-----------------------
      ! THICKNESSDISTRIBUTION (Dist) (Avg) (Std) (Zmin) (Zmax) (Count)
      !-----------------------
      ELSE IF( Command .EQ. 'THICKNESSDISTRIBUTION' ) THEN
         ErrNo = THICKNESS_ERROR

         Call ParseCommand( Buffer, Distribution, Args )
         Call ToUpper( Distribution )

         READ( Args, *, ERR=10 ) Mu, Sigma, Zmin, Zmax, Count
         ! WRITE(LUNIT,*) 'THICKNESSDISTRIBUTION ', ADJUSTL(TRIM(Distribution)), Mu, Sigma, Zmin, Zmax, Count

         IF( Sigma .LT. 0 .OR. Zmin .GT. Zmax .OR. Count .LT. 1 ) GOTO 10

         dZ = (Zmax - Zmin)/Count

         DO i = 1, Count
            Zlower = Zmin + (i-1)*dZ
            Zupper = Zmin + (i)*dZ
            Zscore = ( Zupper + Zlower ) / 2

            Weight = GaussianCDF( Zupper ) - GaussianCDF( Zlower )

            IF( Distribution .EQ. 'NORMAL' ) THEN
               Thickness = Mu + Zscore*Sigma
            ELSE IF ( Distribution .EQ. 'LOGNORMAL' ) THEN
               Thickness = EXP( Mu + Zscore*Sigma )
            ELSE
               GOTO 10
            END IF

            IF( Thickness .GT. 0 ) THEN
               CALL AddThickness( Model, Thickness, Weight, ErrNo )
            END IF
         END DO

      !-----------------------
      ! VERBOSITY (Level)
      !-----------------------
      ELSE IF( Command .EQ. 'VERBOSITY' ) THEN
         ErrNo = VERBOSITY_ERROR

         READ( Buffer, *, ERR=10 ) Level
         ! WRITE(LUNIT,*) 'VERBOSITY ', Level

         CALL SetVerbosity( Model, Level, ErrNo )

      !-----------------------
      ! WELL (X) (Y) (Radius) (Q)
      !-----------------------
      ELSE IF( Command .EQ. 'WELL' ) THEN
         ErrNo = WELL_ERROR

         READ( Buffer, *, ERR=10 ) X, Y, Radius, Q
         ! WRITE(LUNIT,*) 'WELL ', X, Y, Radius, Q

         IF( Radius .LE. 0 ) GOTO 10

         CALL AddWell( Model, X, Y, Radius, Q, ErrNo )

      !-----------------------
      ! UNKNOWN
      !-----------------------
      ELSE
         ErrNo = UNKNOWN_ERROR
      END IF

   10 RETURN
   END SUBROUTINE ProcessCommand

!==============================================================================
END MODULE DRIVER_MODULE
!==============================================================================
