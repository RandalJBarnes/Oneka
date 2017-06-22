!==============================================================================
! Module CAPTUREZONE_MODULE                                       (22-Jun-2017)
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
MODULE CAPTUREZONE_MODULE
!==============================================================================
   USE CONSTANTS_MODULE
   USE ERROR_MODULE
   IMPLICIT NONE

   !===========================================================================
   ! Type T_CAPTUREZONE
   !===========================================================================
   TYPE T_CAPTUREZONE
   PRIVATE
      ! Grid geometry.
      INTEGER :: nRows    = 0          ! # of grid rows
      INTEGER :: nCols    = 0          ! # of grid columns

      INTEGER :: mRows    = 0          ! maximum # of grid rows
      INTEGER :: mCols    = 0          ! maximum # of grid columns

      REAL(8) :: Xmin     =  INFINITY  ! left edge of grid
      REAL(8) :: Xmax     = -INFINITY  ! right edge of grid
      REAL(8) :: DeltaX   = -INFINITY  ! grid cell width

      REAL(8) :: Ymin     =  INFINITY  ! bottom edge of grid
      REAL(8) :: Ymax     = -INFINITY  ! top edge of grid
      REAL(8) :: DeltaY   = -INFINITY  ! grid cell height

      ! Total weight.
      REAL(8) :: Weight   = 0          ! accumulated pseudo-probability

      ! Registration grid.
      LOGICAL, POINTER, DIMENSION(:,:) :: RGrid => NULL()

      ! Probability grid.
      REAL(8), POINTER, DIMENSION(:,:) :: PGrid => NULL()

   END TYPE T_CAPTUREZONE

   !===========================================================================
   ! INTERFACES
   !===========================================================================
   INTERFACE CreateAsciiGridFile
      MODULE PROCEDURE CreateAsciiGridFile_CaptureZone
   END INTERFACE

   INTERFACE CreateEsriGridFile
      MODULE PROCEDURE CreateEsriGridFile_CaptureZone
   END INTERFACE

   INTERFACE CreateSurferGridFile
      MODULE PROCEDURE CreateSurferGridFile_CaptureZone
   END INTERFACE

   INTERFACE DistanceSquared
      MODULE PROCEDURE DistanceSquared_CaptureZone
   END INTERFACE

   INTERFACE Initialize
      MODULE PROCEDURE Initialize_CaptureZone
   END INTERFACE

   INTERFACE Insert
      MODULE PROCEDURE Insert_CaptureZone
   END INTERFACE

   INTERFACE Register
      MODULE PROCEDURE Register_CaptureZone
   END INTERFACE

   INTERFACE Reset
      MODULE PROCEDURE Reset_CaptureZone
   END INTERFACE

   INTERFACE SetGrid
      MODULE PROCEDURE SetGrid_CaptureZone
   END INTERFACE

   INTERFACE ExpandGrid
      MODULE PROCEDURE ExpandGrid_CaptureZone
   END INTERFACE

!------------------------------------------------------------------------------
CONTAINS
   !---------------------------------------------------------------------------
   ! CreateAsciiGridFile_CaptureZone
   !
   ! Notes:
   !  o  This routine writes out the entire grid using a simple, space-
   !     delimited, ASCII text file format.
   !---------------------------------------------------------------------------
   SUBROUTINE CreateAsciiGridFile_CaptureZone( CaptureZone, Filename )

   ! Declare the arguments
   TYPE(T_CAPTUREZONE), INTENT(IN) :: CaptureZone
   CHARACTER(*),        INTENT(IN) :: Filename

   ! Declare local parameters.
   INTEGER, PARAMETER :: GUNIT = 31

   ! Declare local variables
   INTEGER :: row, col
   REAL(8) :: Z
   CHARACTER(50) :: BaseFilename, GridFilename

   ! Make certain that the grids have been filled.
   IF( CaptureZone%Weight .LE. 0 ) RETURN

   ! Add the extension to the filename.
   BaseFilename = ADJUSTL(Filename)
   GridFilename = BaseFilename(1:LEN_TRIM(BaseFilename)) // '.txt'

   ! Open the file, with a record length of 8, and write out the header information.
   OPEN( UNIT=GUNIT, FILE=GridFilename, ACTION="WRITE", STATUS="REPLACE", ERR=10 )

   WRITE(GUNIT,'(I10, 1X, I10)') CaptureZone%nRows, CaptureZone%nCols
   WRITE(GUNIT,'(E16.8, 1X, E16.8)') CaptureZone%Xmin, CaptureZone%Xmax
   WRITE(GUNIT,'(E16.8, 1X, E16.8)') CaptureZone%Ymin, CaptureZone%Ymax

   DO row = 1, CaptureZone%nRows
      DO col = 1, CaptureZone%nCols
         Z = CaptureZone%PGrid(row,col) / CaptureZone%Weight
         WRITE(GUNIT,'(E12.4, 1X)', ADVANCE='NO') Z
      END DO
      WRITE(GUNIT,*)
   END DO

   CLOSE(GUNIT)

   WRITE(LUNIT,*)
   WRITE(LUNIT,*) 'ASCII grid file created: ', ADJUSTL(TRIM(GridFilename))
   WRITE(LUNIT,*)

   WRITE(SUNIT,*)
   WRITE(SUNIT,*) 'ASCII grid file created: ', ADJUSTL(TRIM(GridFilename))
   WRITE(SUNIT,*)

   RETURN

   ! Process the file error.
10 CALL FileError( GridFilename )
   RETURN

   END SUBROUTINE CreateAsciiGridFile_CaptureZone


   !---------------------------------------------------------------------------
   ! CreateEsriGridFile_CaptureZone
   !
   ! Notes:
   !  o  This routine writes out a binary ESRI grid file and the associated
   !     ESRI header file.
   !---------------------------------------------------------------------------
   SUBROUTINE CreateEsriGridFile_CaptureZone( CaptureZone, Filename )

   ! Declare the arguments
   TYPE(T_CAPTUREZONE), INTENT(IN) :: CaptureZone
   CHARACTER(*),        INTENT(IN) :: Filename

   ! Declare local parameters.
   INTEGER, PARAMETER :: GUNIT = 31
   INTEGER, PARAMETER :: HUNIT = 32

   REAL(4), PARAMETER :: BLANKVAL = 3.402823466E+38 / 2

   ! Declare local variables
   INTEGER :: row, col, j
   REAL(8) :: Z
   CHARACTER(50) :: BaseFilename, GridFilename, HeadFilename

   ! Make certain that the grids have been filled.
   IF( CaptureZone%Weight .LE. 0 ) RETURN

   ! Add the extension to the filename.
   BaseFilename = ADJUSTL(Filename)
   GridFilename = BaseFilename(1:LEN_TRIM(BaseFilename)) // '.flt'
   HeadFilename = BaseFilename(1:LEN_TRIM(BaseFilename)) // '.hdr'

   ! Write the header file.
   OPEN( UNIT=HUNIT, FILE=HeadFilename, ACTION='WRITE', STATUS='REPLACE', ERR=10 )

   WRITE(HUNIT,*) 'NCOLS ',        CaptureZone%nCols
   WRITE(HUNIT,*) 'NROWS ',        CaptureZone%nRows
   WRITE(HUNIT,*) 'XLLCORNER ',    CaptureZone%Xmin
   WRITE(HUNIT,*) 'YLLCORNER ',    CaptureZone%Ymin
   WRITE(HUNIT,*) 'CELLSIZE ',     CaptureZone%DeltaX
   WRITE(HUNIT,*) 'NODATA_VALUE ', BLANKVAL
   WRITE(HUNIT,*) 'BYTEORDER LSBFIRST'

   CLOSE(HUNIT)

   WRITE(LUNIT,*)
   WRITE(LUNIT,*) 'ESRI header file created: ', ADJUSTL(TRIM(HeadFilename))

   WRITE(SUNIT,*)
   WRITE(SUNIT,*) 'ESRI header file created: ', ADJUSTL(TRIM(HeadFilename))

   ! Write out the grid file.
   OPEN( UNIT=GUNIT, FILE=GridFilename, ACTION="WRITE", FORM="UNFORMATTED", ACCESS="DIRECT", RECL=4, STATUS="REPLACE", ERR=20 )

   ! CHANGED: Changed the record order to start at the upper left with rec=1. (RJB, 22-Jun-2017)
   ! The code used to be "j = (row-1)*CaptureZone%nCols + col".

   DO row = CaptureZone%nRows, 1, -1
      DO col = 1, CaptureZone%nCols
         j = (CaptureZone%nRows-row)*CaptureZone%nCols + col
         Z = CaptureZone%PGrid(row,col) / CaptureZone%Weight
         WRITE(GUNIT, REC=j) REAL( Z, 4 )
      END DO
   END DO

   CLOSE(GUNIT)

   WRITE(LUNIT,*) 'ESRI grid file created: ', ADJUSTL(TRIM(GridFilename))
   WRITE(LUNIT,*)

   WRITE(SUNIT,*) 'ESRI grid file created: ', ADJUSTL(TRIM(GridFilename))
   WRITE(SUNIT,*)

   RETURN

   ! Process the file error.
10 CALL FileError( HeadFilename )
   RETURN

20 CALL FileError( GridFilename )
   RETURN

   END SUBROUTINE CreateEsriGridFile_CaptureZone


   !---------------------------------------------------------------------------
   ! CreateSurferGridFile_CaptureZone
   !
   ! Notes:
   !  o  This routine writes out a binary Surfer grid file, using the
   !     Surfer 6.0 grid file format (i.e. "DSBB").
   !---------------------------------------------------------------------------
   SUBROUTINE CreateSurferGridFile_CaptureZone( CaptureZone, Filename )

   ! Declare the arguments
   TYPE(T_CAPTUREZONE), INTENT(IN) :: CaptureZone
   CHARACTER(*),        INTENT(IN) :: Filename

   ! Declare local parameters.
   INTEGER, PARAMETER :: GUNIT = 31
   ! REAL(4), PARAMETER :: SURFER_BLANKVAL = 3.402823466E+38 / 2

   ! Declare local variables
   INTEGER :: row, col, j
   REAL(8) :: Z, Zmin, Zmax
   CHARACTER(50) :: BaseFilename, GridFilename

   ! Make certain that the grids have been filled.
   IF( CaptureZone%Weight .LE. 0 ) RETURN

   ! Determine the grid extremes.
   Zmin = MINVAL( CaptureZone%PGrid ) / CaptureZone%Weight
   Zmax = MAXVAL( CaptureZone%PGrid ) / CaptureZone%Weight

   ! Add the extension to the filename.
   BaseFilename = ADJUSTL(Filename)
   GridFilename = BaseFilename(1:LEN_TRIM(BaseFilename)) // '.grd'

   ! Open the file, with a record length of 8, and write out the header information.
   OPEN( UNIT=GUNIT, FILE=GridFilename, ACTION="WRITE", FORM="UNFORMATTED", ACCESS="DIRECT", RECL=8, STATUS="REPLACE", ERR=10 )

   WRITE(GUNIT, REC=1) 'DSBB', INT( CaptureZone%nCols, 2 ), INT( CaptureZone%nRows, 2 )
   WRITE(GUNIT, REC=2) REAL( CaptureZone%Xmin, 8 )
   WRITE(GUNIT, REC=3) REAL( CaptureZone%Xmax, 8 )
   WRITE(GUNIT, REC=4) REAL( CaptureZone%Ymin, 8 )
   WRITE(GUNIT, REC=5) REAL( CaptureZone%Ymax, 8 )
   WRITE(GUNIT, REC=6) REAL( Zmin, 8 )
   WRITE(GUNIT, REC=7) REAL( Zmax, 8 )

   CLOSE(GUNIT)

   ! Reopen the file, with a record length of 4, and write out the grid.
   OPEN( UNIT=GUNIT, FILE=GridFilename, ACTION="WRITE", FORM="UNFORMATTED", ACCESS="DIRECT", RECL=4, STATUS="OLD", ERR=10 )

   DO row = 1, CaptureZone%nRows
      DO col = 1, CaptureZone%nCols
         j = 14 + (row-1)*CaptureZone%nCols + col
         Z = CaptureZone%PGrid(row,col) / CaptureZone%Weight
         WRITE(GUNIT, REC=j) REAL( Z, 4 )
      END DO
   END DO

   CLOSE(GUNIT)

   WRITE(LUNIT,*)
   WRITE(LUNIT,*) 'SURFER grid file created: ', ADJUSTL(TRIM(GridFilename))
   WRITE(LUNIT,*)

   WRITE(SUNIT,*)
   WRITE(SUNIT,*) 'SURFER grid file created: ', ADJUSTL(TRIM(GridFilename))
   WRITE(SUNIT,*)

   RETURN

   ! Process the file error.
10 CALL FileError( GridFilename )
   RETURN

   END SUBROUTINE CreateSurferGridFile_CaptureZone


   !---------------------------------------------------------------------------
   ! DistanceSquared_CaptureZone
   !
   !  Computes the distance squared between the (cx,cy) and the line segment
   !  defined by [(ax,ay),(bx,by)].  Note, this is the distance to the line
   !  segment, not the distance to the line.
   !---------------------------------------------------------------------------
   FUNCTION DistanceSquared_CaptureZone( ax, ay, bx, by, cx, cy ) RESULT( D2 )

   ! Declare the arguments.
   REAL(8), INTENT(IN) :: ax, ay, bx, by, cx, cy
   REAL(8)             :: D2

   ! Declare the local variables.
   REAL(8) :: bax, bay, cax, cay
   REAL(8) :: perpdot, dot
   REAL(8) :: length2, alpha2, beta2

   ! Compute the distance squared.
   bax = bx - ax
   bay = by - ay

   cax = cx - ax
   cay = cy - ay

   perpdot = bax*cay - bay*cax
   dot     = bax*cax + bay*cay
   length2 = bax*bax + bay*bay

   alpha2  = perpdot*perpdot / length2
   beta2   = dot*dot / length2

   IF( dot < 0 ) THEN
      D2 = alpha2 + beta2
   ELSE IF( beta2 > length2 ) THEN
      D2 = alpha2 + beta2 - 2*dot + length2
   ELSE
      D2 = alpha2
   END IF

   END FUNCTION DistanceSquared_CaptureZone


   !---------------------------------------------------------------------------
   ! Initialize_CaptureZone
   !---------------------------------------------------------------------------
   SUBROUTINE Initialize_CaptureZone( CaptureZone )

   ! Declare the arguments.
   TYPE(T_CAPTUREZONE), INTENT(INOUT) :: CaptureZone

   ! Reinitialize the capture zone grids.
   CaptureZone%RGrid  = .FALSE.
   CaptureZone%PGrid  = 0
   CaptureZone%Weight = 0

   END SUBROUTINE Initialize_CaptureZone


   !---------------------------------------------------------------------------
   ! Insert_CaptureZone
   !
   ! Notes:
   !  o  This routine inserts a single linear segment of a single track into
   !     the capture zone RGrid registration array.
   !---------------------------------------------------------------------------
   SUBROUTINE Insert_CaptureZone( CaptureZone, ax, ay, bx, by, Width )

   ! Declare the arguments.
   TYPE(T_CAPTUREZONE), INTENT(INOUT) :: CaptureZone
   REAL(8),             INTENT(IN)    :: ax
   REAL(8),             INTENT(IN)    :: ay
   REAL(8),             INTENT(IN)    :: bx
   REAL(8),             INTENT(IN)    :: by
   REAL(8),             INTENT(IN)    :: Width

   ! Declare the local variables.
   INTEGER :: i, j, left, right, top, bottom
   REAL(8) :: Cx, Cy, W2

   ! Make certain that the grid is large enough.
   CALL ExpandGrid( CaptureZone, ax, ay )
   CALL ExpandGrid( CaptureZone, bx, by )

   ! Determine the sub-grid limits.
   left   = MAX(1,                 INT( (MIN(ax,bx) - Width - CaptureZone%Xmin) / CaptureZone%DeltaX ) + 1 )
   right  = MIN(CaptureZone%nCols, INT( (MAX(ax,bx) + Width - CaptureZone%Xmin) / CaptureZone%DeltaX ) + 1 )

   bottom = MAX(1,                 INT( (MIN(ay,by) - Width - CaptureZone%Ymin) / CaptureZone%DeltaY ) + 1 )
   top    = MIN(CaptureZone%nRows, INT( (MAX(ay,by) + Width - CaptureZone%Ymin) / CaptureZone%DeltaY ) + 1 )

   ! Check each node within the sub-grid to see if it is within the
   ! track width of the path line segment.
   W2 = Width * Width

   DO i = left, right
      DO j = bottom, top
         IF( CaptureZone%RGrid(j,i) .EQV. .FALSE. ) THEN
            Cx = CaptureZone%Xmin + i*CaptureZone%DeltaX
            Cy = CaptureZone%Ymin + j*CaptureZone%DeltaY

            IF( DistanceSquared( ax, ay, bx, by, Cx, Cy ) < W2 ) THEN
               CaptureZone%RGrid(j,i) = .TRUE.
            END IF
         END IF
      END DO
   END DO

   END SUBROUTINE Insert_CaptureZone


   !---------------------------------------------------------------------------
   ! Register_CaptureZone
   !---------------------------------------------------------------------------
   SUBROUTINE Register_CaptureZone( CaptureZone, W )

   ! Declare the arguments.
   TYPE(T_CAPTUREZONE), INTENT(INOUT) :: CaptureZone
   REAL(8),             INTENT(IN)    :: W

   ! Delcare local variables.
   INTEGER :: i,j

   ! Update the probability grid.
   ! WHERE( CaptureZone%RGrid ) CaptureZone%PGrid = CaptureZone%PGrid + W
   DO i = 1, CaptureZone%nRows
      DO j = 1, CaptureZone%nCols
         IF( CaptureZone%RGrid(i,j) ) THEN
            CaptureZone%PGrid(i,j) = CaptureZone%PGrid(i,j) + W
         END IF
      END DO
   END DO

   CaptureZone%Weight = CaptureZone%Weight + W

   ! Reset the registration grid.
   CaptureZone%RGrid = .FALSE.

   END SUBROUTINE Register_CaptureZone


   !---------------------------------------------------------------------------
   ! Reset_CaptureZone
   !---------------------------------------------------------------------------
   SUBROUTINE Reset_CaptureZone( CaptureZone )

   ! Declare the arguments.
   TYPE(T_CAPTUREZONE), INTENT(INOUT) :: CaptureZone

   ! Zero out the grid geometry.
   CaptureZone%nRows    = 0
   CaptureZone%nCols    = 0

   CaptureZone%Xmin     = INFINITY
   CaptureZone%Xmax     = -INFINITY
   CaptureZone%Ymin     = INFINITY
   CaptureZone%Ymax     = -INFINITY

   CaptureZone%mRows    = 0
   CaptureZone%mCols    = 0

   CaptureZone%DeltaX   = 0
   CaptureZone%DeltaY   = 0

   ! Deallocate the grids.
   IF( ASSOCIATED( CaptureZone%RGrid ) ) DEALLOCATE( CaptureZone%RGrid )
   IF( ASSOCIATED( CaptureZone%PGrid ) ) DEALLOCATE( CaptureZone%PGrid )

   END SUBROUTINE Reset_CaptureZone


   !---------------------------------------------------------------------------
   ! SetGrid_CaptureZone
   !---------------------------------------------------------------------------
   FUNCTION SetGrid_CaptureZone( CaptureZone, nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols ) RESULT( Flag )

   ! Declare the arguments.
   TYPE(T_CAPTUREZONE), INTENT(INOUT) :: CaptureZone

   INTEGER,             INTENT(IN)    :: nRows
   INTEGER,             INTENT(IN)    :: nCols

   REAL(8),             INTENT(IN)    :: Xmin
   REAL(8),             INTENT(IN)    :: Xmax
   REAL(8),             INTENT(IN)    :: Ymin
   REAL(8),             INTENT(IN)    :: Ymax

   INTEGER,             INTENT(IN)    :: mRows
   INTEGER,             INTENT(IN)    :: mCols

   ! Declare the local variables.
   LOGICAL                            :: Flag

   ! Check the parameters.
   IF( nRows .LE. 0 .OR. nCols .LE. 0 .OR. Xmin .GT. Xmax .OR. Ymin .GT. Ymax .OR. mRows .LT. nRows .OR. mCols .LT. nCols ) THEN
      Flag = .FALSE.
      RETURN
   END IF

   ! Setup the grid geometry.
   CaptureZone%nRows    = nRows
   CaptureZone%nCols    = nCols

   CaptureZone%Xmin     = Xmin
   CaptureZone%Xmax     = Xmax
   CaptureZone%Ymin     = Ymin
   CaptureZone%Ymax     = Ymax

   CaptureZone%mRows    = mRows
   CaptureZone%mCols    = mCols

   CaptureZone%DeltaX   = (Xmax - Xmin)/(nCols-1)
   CaptureZone%DeltaY   = (Ymax - Ymin)/(nRows-1)

   ! Initialize the grids.
   IF( ASSOCIATED( CaptureZone%RGrid ) ) DEALLOCATE( CaptureZone%RGrid )
   ALLOCATE( CaptureZone%RGrid( nRows, nCols ) )
   CaptureZone%RGrid = .FALSE.

   IF( ASSOCIATED( CaptureZone%PGrid ) ) DEALLOCATE( CaptureZone%PGrid )
   ALLOCATE( CaptureZone%PGrid( nRows, nCols ) )
   CaptureZone%PGrid = 0

   Flag = .TRUE.
   END FUNCTION SetGrid_CaptureZone


   !---------------------------------------------------------------------------
   ! ExpandGrid_CaptureZone
   !  This routine expands the grids (RGrid and PGrid) to include the point
   !  [X,Y].  The contents of the grids are maintained.
   !
   ! Notes:
   !  o  This routine allocates a new grid, copies the contents from the old
   !     grid, and then deallocates the old grids.  Thus, there must be suffi-
   !     cient memory to hold two copies simultaneously.
   !---------------------------------------------------------------------------
   SUBROUTINE ExpandGrid_CaptureZone( CaptureZone, X, Y )

   ! Declare the arguments.
   TYPE(T_CAPTUREZONE), INTENT(INOUT) :: CaptureZone

   REAL(8),             INTENT(IN)    :: X
   REAL(8),             INTENT(IN)    :: Y

   ! Declare local variables.
   LOGICAL, POINTER, DIMENSION(:,:) :: RGrid => NULL()
   REAL(8), POINTER, DIMENSION(:,:) :: PGrid => NULL()

   INTEGER :: nRows, nCols
   INTEGER :: RowShift, ColShift
   INTEGER :: Err
   REAL(8) :: Xmin, Xmax, Ymin, Ymax

   ! Check that the current grid has been allocated.
   IF( CaptureZone%nCols .EQ. 0 .OR. CaptureZone%nRows .EQ. 0 ) THEN
      RETURN
   END IF

   ! Check if expansion is required.
   IF( X .GE. CaptureZone%Xmin .AND. X .LE. CaptureZone%Xmax .AND. Y .GE. CaptureZone%Ymin .AND. Y .LE. CaptureZone%Ymax ) THEN
      RETURN
   END IF

   ! Check if expansion is possible.
   IF( CaptureZone%nRows .EQ. CaptureZone%mRows .AND. CaptureZone%nCols .EQ. CaptureZone%mCols ) THEN
      RETURN
   END IF

   ! Determine the new grid geometry.
   nRows = CaptureZone%nRows
   nCols = CaptureZone%nCols

   ColShift = 0
   RowShift = 0

   Xmin  = CaptureZone%Xmin
   Xmax  = CaptureZone%Xmax

   Ymin  = CaptureZone%Ymin
   Ymax  = CaptureZone%Ymax

   DO WHILE( X .GT. Xmax .AND. nCols .LT. CaptureZone%mCols )
      Xmax = Xmax + CaptureZone%DeltaX
      nCols = nCols + 1
   END DO

   DO WHILE( X .LT. Xmin .AND. nCols .LT. CaptureZone%mCols )
      Xmin = Xmin - CaptureZone%DeltaX
      nCols = nCols + 1
      ColShift = ColShift + 1
   END DO

   DO WHILE( Y .GT. Ymax .AND. nRows .LT. CaptureZone%mRows )
      Ymax = Ymax + CaptureZone%DeltaY
      nRows = nRows + 1
   END DO

   DO WHILE( Y .LT. Ymin .AND. nRows .LT. CaptureZone%mRows )
      Ymin = Ymin - CaptureZone%DeltaY
      nRows = nRows + 1
      RowShift = RowShift + 1
   END DO

   ! Check if expansion is necessary.
   IF( nRows .EQ. CaptureZone%nRows .AND. nCols .EQ. CaptureZone%nCols ) THEN
      RETURN
   END IF

   ! Allocate the new grids.
   NULLIFY( RGrid )
   ALLOCATE( RGrid( nRows, nCols ), STAT=Err )
   IF( Err .NE. 0 ) THEN
      WRITE(*,*)
      WRITE(*,*)     'ERROR: Memory allocation error during grid expansion.'
      WRITE(LUNIT,*)
      WRITE(LUNIT,*) 'ERROR: Memory allocation error during grid expansion.'
      STOP
   END IF

   NULLIFY( PGrid )
   ALLOCATE( PGrid( nRows, nCols ), STAT=Err )
   IF( Err .NE. 0 ) THEN
      WRITE(*,*)
      WRITE(*,*)     'ERROR: Memory allocation error during grid expansion.'
      WRITE(LUNIT,*)
      WRITE(LUNIT,*) 'ERROR: Memory allocation error during grid expansion.'
      STOP
   END IF

   ! Copy the old contents into the new grids.
   RGrid = .FALSE.
   RGrid( RowShift+1:RowShift+CaptureZone%nRows, ColShift+1:ColShift+CaptureZone%nCols ) = CaptureZone%RGrid( 1:CaptureZone%nRows, 1:CaptureZone%nCols )

   PGrid = 0
   PGrid( RowShift+1:RowShift+CaptureZone%nRows, ColShift+1:ColShift+CaptureZone%nCols ) = CaptureZone%PGrid( 1:CaptureZone%nRows, 1:CaptureZone%nCols )

   ! Replace old grids with new ones.
   DEALLOCATE( CaptureZone%RGrid )
   CaptureZone%RGrid => RGrid

   DEALLOCATE( CaptureZone%PGrid )
   CaptureZone%PGrid => PGrid

   CaptureZone%nRows    = nRows
   CaptureZone%nCols    = nCols

   CaptureZone%Xmin     = Xmin
   CaptureZone%Xmax     = Xmax
   CaptureZone%Ymin     = Ymin
   CaptureZone%Ymax     = Ymax

   ! Log the expansion, and return.
   WRITE(LUNIT,'(''Grid Expansion: '', 2I6, 4F15.2 )' ) nRows, nCols, Xmin, Xmax, Ymin, Ymax

   IF( CaptureZone%nRows .EQ. CaptureZone%mRows ) THEN
      WRITE(LUNIT,'(''Note: the grid expansion is limited by the maximum number of rows.'')' )
   END IF

   IF( CaptureZone%nCols .EQ. CaptureZone%mCols ) THEN
      WRITE(LUNIT,'(''Note: the grid expansion is limited by the maximum number of columns.'')' )
   END IF

   END SUBROUTINE ExpandGrid_CaptureZone

!==============================================================================
END MODULE CAPTUREZONE_MODULE
!==============================================================================
