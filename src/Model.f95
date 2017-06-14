!==============================================================================
! Module MODEL_MODULE                                            (14-Jun-2017)
!
! Written by:
! 	Dr. Randal J. Barnes
!   Department of Civil, Environmental, and Geo- Engineering
!   University of Minnesota
!   <barne003@umn.edu>
!
! Copyright (c) 2017, Randal J. Barnes
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
!     * Redistributions of source code must retain the above copyright
!       notice, this list of conditions and the following disclaimer.
!     * Redistributions in binary form must reproduce the above copyright
!       notice, this list of conditions and the following disclaimer in the
!       documentation and/or other materials provided with the distribution.
!     * Neither the name of the <organization> nor the
!       names of its contributors may be used to endorse or promote products
!       derived from this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
! ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
! WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
! DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
! DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
! (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
! LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
! ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
! (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!==============================================================================

!==============================================================================
MODULE MODEL_MODULE
!==============================================================================
   USE AQUIFER_MODULE
   USE CAPTUREZONE_MODULE
   USE CONSTANTS_MODULE
   USE ERROR_MODULE
   USE NUMERIC_MODULE
   USE REGIONAL_MODULE
   USE WELL_MODULE
   IMPLICIT NONE

   !---------------------------------------------------------------------------
   ! Module parameters
   !---------------------------------------------------------------------------
   INTEGER, PARAMETER :: MAX_WELL    = 1000     ! maximum number of wells
   INTEGER, PARAMETER :: MAX_OBS     = 1000     ! maximum number of observations
   INTEGER, PARAMETER :: MAX_COND    = 1000     ! maximum number of conductivity values
   INTEGER, PARAMETER :: MAX_THICK   = 1000     ! maximum number of thickness values

   INTEGER, PARAMETER :: MIN_TRACKS  = 36       ! minimum number of tracks in a pack
   INTEGER, PARAMETER :: MAX_TRACKS  = 1000     ! maximum number of tracks in a pack
   REAL(8), PARAMETER :: MIN_D_THETA = 0.005    ! minimum angle between track starts [radians]

   INTEGER, PARAMETER :: ESRI_GRID   = 1
   INTEGER, PARAMETER :: SURFER_GRID = 2
   INTEGER, PARAMETER :: ASCII_GRID  = 3

   REAL(8), PARAMETER :: MIN_PVAR    = 0.01     ! minimum allowed discharge potential variance
   REAL(8), PARAMETER :: INFLUENTIAL = 1.0      ! minimum change to be considered "influential"

   !===========================================================================
   ! Type T_COND
   !===========================================================================
   TYPE T_COND
      REAL(8) :: K      ! hydraulic conductivity
      REAL(8) :: Weight ! pseudo-probability
   END TYPE T_COND


   !===========================================================================
   ! Type T_OBS
   !===========================================================================
   TYPE T_OBS
      REAL(8) :: X         ! X coordinate
      REAL(8) :: Y         ! Y coordinate
      REAL(8) :: Zev       ! Z expected value
      REAL(8) :: Zstd      ! Z standard deviation
      LOGICAL :: isActive  ! is the observation currently active
   END TYPE T_OBS


   !===========================================================================
   ! Type T_THICK
   !===========================================================================
   TYPE T_THICK
      REAL(8) :: Thickness ! aquifer thickness
      REAL(8) :: Weight    ! pseudo-probability
   END TYPE T_THICK

   !===========================================================================
   ! Type T_GEOHYDROLOGY
   !===========================================================================
   TYPE T_GEOHYDROLOGY
      REAL(8) :: ERec   ! Recharge expected value
      REAL(8) :: SRec   ! Recharge standard deviation
      REAL(8) :: VRec   ! Recharge variance

      REAL(8) :: EQx    ! Magnitude of Qx at the origin expected value
      REAL(8) :: SQx    ! Magnitude of Qx at the origin standard deviation
      REAL(8) :: VQx    ! Magnitude of Qx at the origin variance

      REAL(8) :: EQy    ! Magnitude of Qy at the origin expected value
      REAL(8) :: SQy    ! Magnitude of Qy at the origin standard deviation
      REAL(8) :: VQy    ! Magnitude of Qy at the origin variance

      REAL(8) :: EMag   ! Magnitude of discharge at the origin expected value
      REAL(8) :: SMag   ! Magnitude of discharge at the origin standard deviation
      REAL(8) :: VMag   ! Magnitude of discharge at the origin variance

      REAL(8) :: EDir   ! Direction of discharge at the origin expected value
      REAL(8) :: SDir   ! Direction of discharge at the origin standard deviation
      REAL(8) :: VDir   ! Direction of discharge at the origin variance
   END TYPE T_GEOHYDROLOGY

   !===========================================================================
   ! Type T_MODEL
   !===========================================================================
   TYPE T_MODEL
   PRIVATE
      TYPE(T_AQUIFER)     :: Aquifer
      TYPE(T_REGIONAL)    :: Regional
      TYPE(T_CAPTUREZONE) :: CaptureZone

      INTEGER :: nWell = 0
      TYPE(T_WELL), DIMENSION( MAX_WELL ) :: Well

      INTEGER :: nObs = 0
      TYPE(T_OBS),  DIMENSION( MAX_OBS ) :: Obs

      INTEGER :: nCond = 0
      TYPE(T_COND), DIMENSION( MAX_COND ) :: Cond

      INTEGER :: nThick = 0
      TYPE(T_THICK), DIMENSION( MAX_THICK ) :: Thick

      ! Grid type.
      INTEGER :: GridType = ASCII_GRID

      ! Tracking parameters.
      REAL(8) :: Duration = 0          ! time limit
      REAL(8) :: Step     = 0          ! target step size
      REAL(8) :: Xo       = 0          ! starting circle X
      REAL(8) :: Yo       = 0          ! starting circle Y
      REAL(8) :: Radius   = 0          ! starting circle radius
      REAL(8) :: Width    = 0          ! particle track (half-) width
      REAL(8) :: maxSep   = 0          ! max separation of adjacent tracks end points
      INTEGER :: nTracks  = 0          ! number of tracks

      ! Reporting parameters.
      INTEGER :: Verbosity = 1         ! control the reporting level in the log file

      ! Buffer zone radius.
      REAL(8) :: BufferZone = 0        ! radius of the buffer zone aroung each pumping well. [L]
   END TYPE T_MODEL


   !===========================================================================
   ! INTERFACES
   !===========================================================================

   INTERFACE AddConductivity
      MODULE PROCEDURE AddConductivity_Model
   END INTERFACE

   INTERFACE AddObs
      MODULE PROCEDURE AddObs_Model
   END INTERFACE

   INTERFACE AddThickness
      MODULE PROCEDURE AddThickness_Model
   END INTERFACE

   INTERFACE AddWell
      MODULE PROCEDURE AddWell_Model
   END INTERFACE

   INTERFACE PackOfTracks
      MODULE PROCEDURE PackOfTracks_Model
   END INTERFACE

   INTERFACE BackwardTrack
      MODULE PROCEDURE BackwardTrack_Model
   END INTERFACE

   INTERFACE CreateCaptureZone
      MODULE PROCEDURE CreateCaptureZone_Model
   END INTERFACE

   INTERFACE Discharge
      MODULE PROCEDURE Discharge_Model
   END INTERFACE

   INTERFACE Fit
      MODULE PROCEDURE Fit_Model
   END INTERFACE

   INTERFACE ComputeGeohydrology
      MODULE PROCEDURE ComputeGeohydrology_Model
   END INTERFACE

   INTERFACE Head
      MODULE PROCEDURE Head_Model
   END INTERFACE

   INTERFACE Potential
      MODULE PROCEDURE Potential_Model
   END INTERFACE

   INTERFACE ResetAll
      MODULE PROCEDURE ResetAll_Model
   END INTERFACE

   INTERFACE RKSolve
      MODULE PROCEDURE RKSolve_Model
   END INTERFACE

   INTERFACE SetEsriGrid
      MODULE PROCEDURE SetEsriGrid_Model
   END INTERFACE

   INTERFACE SetSurferGrid
      MODULE PROCEDURE SetSurferGrid_Model
   END INTERFACE

   INTERFACE SetAsciiGrid
      MODULE PROCEDURE SetAsciiGrid_Model
   END INTERFACE

   INTERFACE SetBufferZone
      MODULE PROCEDURE SetBufferZone_Model
   END INTERFACE

   INTERFACE SetProperties
      MODULE PROCEDURE SetProperties_Model
   END INTERFACE

   INTERFACE SetTrack
      MODULE PROCEDURE SetTrack_Model
   END INTERFACE

   INTERFACE SetVerbosity
      MODULE PROCEDURE SetVerbosity_Model
   END INTERFACE

   INTERFACE Velocity
      MODULE PROCEDURE Velocity_Model
   END INTERFACE

CONTAINS

   !---------------------------------------------------------------------------
   ! AddConductivity_Model
   !---------------------------------------------------------------------------
   SUBROUTINE AddConductivity_Model( Model, K, Weight, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      REAL(8),       INTENT(IN)    :: K
      REAL(8),       INTENT(IN)    :: Weight
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Add the new conductivity pair.
      IF( Model%nCond .LT. MAX_COND .AND. K .GT. 0 ) THEN
         Model%nCond = Model%nCond + 1
         Model%Cond( Model%nCond ) = T_COND( K, Weight )
         ErrNo = NO_ERROR
      ELSE
         ErrNo = CONDUCTIVITY_ERROR
      END IF
   END SUBROUTINE AddConductivity_Model


   !---------------------------------------------------------------------------
   ! AddObs_Model
   !---------------------------------------------------------------------------
   SUBROUTINE AddObs_Model( Model, X, Y, Zev, Zstd, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      REAL(8),       INTENT(IN)    :: X
      REAL(8),       INTENT(IN)    :: Y
      REAL(8),       INTENT(IN)    :: Zev
      REAL(8),       INTENT(IN)    :: Zstd
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Add the new observation.
      IF( (Model%nObs .LT. MAX_OBS) .AND. (Zstd .GT. 0) ) THEN
         Model%nObs = Model%nObs + 1
         Model%Obs( Model%nObs ) = T_OBS( X, Y, Zev, Zstd, .TRUE. )
         ErrNo = NO_ERROR
      ELSE
         ErrNo = OBS_ERROR
      END IF
   END SUBROUTINE AddObs_Model


   !---------------------------------------------------------------------------
   ! AddThickness_Model
   !---------------------------------------------------------------------------
   SUBROUTINE AddThickness_Model( Model, Thickness, Weight, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      REAL(8),       INTENT(IN)    :: Thickness
      REAL(8),       INTENT(IN)    :: Weight
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Add the new Thickness pair.
      IF( Model%nThick .LT. MAX_THICK .AND. Thickness .GT. 0 ) THEN
         Model%nThick = Model%nThick + 1
         Model%Thick( Model%nThick ) = T_THICK( Thickness, Weight )
         ErrNo = NO_ERROR
      ELSE
         ErrNo = THICKNESS_ERROR
      END IF
   END SUBROUTINE AddThickness_Model


   !---------------------------------------------------------------------------
   ! AddWell_Model
   !---------------------------------------------------------------------------
   SUBROUTINE AddWell_Model( Model, X, Y, Radius, Q, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      REAL(8),       INTENT(IN)    :: X
      REAL(8),       INTENT(IN)    :: Y
      REAL(8),       INTENT(IN)    :: Radius
      REAL(8),       INTENT(IN)    :: Q
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Add the new well.
      IF( Model%nWell .LT. MAX_WELL .AND. Radius .GT. 0 ) THEN
         Model%nWell = Model%nWell + 1
         Model%Well( Model%nWell ) = T_WELL( X, Y, Radius, Q )
         ErrNo = NO_ERROR
      ELSE
         ErrNo = WELL_ERROR
      END IF
   END SUBROUTINE AddWell_Model


   !---------------------------------------------------------------------------
   ! PackOfTracks_Model
   !
   ! Notes:
   !
   ! o   This routine computes a time-limited capture zone for a well at the
   !     specified location, based upon the specified model.
   !
   ! o   The capture zone is determined by backward particle tracking starting
   !     at Nt points located uniformly about a circle of radius R centered at
   !     (Xo, Yo).
   !
   !---------------------------------------------------------------------------
   FUNCTION PackOfTracks_Model( Model ) RESULT( Status )
      ! Declare the arguments
      TYPE(T_MODEL), INTENT(INOUT) :: Model

      INTEGER                      :: Status

      ! Declare the local variables.
      INTEGER :: p, q, nTracks
      REAL(8) :: X, Y, sep
      LOGICAL :: Done

      REAL(8), DIMENSION(MAX_TRACKS) :: Theta
      REAL(8), DIMENSION(MAX_TRACKS) :: Xf
      REAL(8), DIMENSION(MAX_TRACKS) :: Yf
      INTEGER, DIMENSION(MAX_TRACKS) :: next

      ! Generate the initial suite of tracks. The first and last are duplicates
      ! so that the infill logic works correctly across the 2pi-to-0 jump.
      DO p = 1, MIN_TRACKS+1
         ! Uniformly distribute the starting points around a circle
         ! centered at (Xo,Yo) with the specified Radius.
         Theta(p) = DBLE(p-1)/DBLE(MIN_TRACKS) * TWO_PI

         X = Model%Xo + Model%Radius * COS(Theta(p))
         Y = Model%Yo + Model%Radius * SIN(Theta(p))

         Status = BackwardTrack_Model( Model, X,Y );
         IF( Status .NE. SUCCESS ) RETURN

         Xf(p) = X
         Yf(p) = Y
         next(p) = p+1
      END DO

      nTracks = MIN_TRACKS+1
      next(nTracks) = 0

      ! Densify the pack of tracks where needed.
      Done = .FALSE.
      DO WHILE( .NOT. Done )
         Done = .TRUE.

         p = 1
         DO WHILE( next(p) .NE. 0 )
            q = next(p)
            IF( Theta(q)-Theta(p) > MIN_D_THETA ) THEN
               sep = SQRT( (Xf(p)-Xf(q))**2 + (Yf(p)-Yf(q))**2 )

               IF( sep > Model%maxSep ) THEN
                  nTracks = nTracks + 1
                  Theta(nTracks) = (Theta(p)+Theta(q))/2.0

                  X = Model%Xo + Model%Radius * COS(Theta(nTracks))
                  Y = Model%Yo + Model%Radius * SIN(Theta(nTracks))

                  Status = BackwardTrack_Model( Model, X,Y );
                  IF( Status .NE. SUCCESS ) RETURN

                  Xf(nTracks) = X
                  Yf(nTracks) = Y

                  next(nTracks) = next(p)
                  next(p) = nTracks

                  Done = .FALSE.
               END IF
            END IF

            p = q
         END DO
      END DO

      Model%nTracks = nTracks
   END FUNCTION PackOfTracks_Model


   !---------------------------------------------------------------------------
   ! BackwardTrack_Model
   !
   ! Notes:
   !
   ! o   This routine computes a time-limited backward particle track starting
   !     at the specified (X,Y).
   !
   ! o   On entrance, (X,Y) is the starting point of the backtrack. On exit,
   !     (X,Y) is the ending point of the track.
   !
   ! o   This routine emplements an adaptive stepsize for the particle tracking.
   !---------------------------------------------------------------------------
   FUNCTION BackwardTrack_Model( Model, X,Y ) RESULT( Status )
      ! Declare the arguments
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      REAL(8), INTENT(INOUT)       :: X, Y

      INTEGER                      :: Status

      ! Declare the local variables.
      REAL(8) :: dX, dY, S, dS, T, dT, Tol
      REAL(8) :: Xn, Yn, Xe, Ye

      REAL(8), PARAMETER :: TOLERANCE = 1D-3
      REAL(8), PARAMETER :: MIN_DT    = 1D-7

      ! Set the adaptive stepsize tolerance.
      Tol = TOLERANCE * Model%Step

      ! Start with a small stepsize -- if it is too small, the
      ! adaptive control will increase it.
      dT = Model%Duration / REAL(1000,8)
      T  = 0
      S  = 0

      ! Step from T = 0 to Tmax by dT increments.
      DO
         dT = MIN( dT, Model%Duration-T )

         ! Numerically integrate the differential equation using an
         ! fifth-order Runge-Kutta-Fehlberg method.  This method computes
         ! an approximate local error.  If the error is acceptable, then
         ! exit this loop. If the error is too large, then cut dT in
         ! half and try again.
         DO
            Status = RKSolve( Model, -dT, X, Y, Xn, Yn, Xe, Ye )
            IF( Status .LT. 0 ) RETURN

            IF( Xe .LT. Tol .AND. Ye .LT. Tol ) EXIT
            dT = dT/2

            IF( dT .LT. MIN_DT ) THEN
               Status = DID_NOT_CONVERGE
               RETURN
            END IF
         END DO

         dX = Xn - X
         dY = Yn - Y
         dS = SQRT( dX*dX + dY*dY )

         S  = S + dS
         T  = T + dT

         ! If the stepsize is not yet too large, then increase
         ! it by 1.5.  Note the asymmetry:  when appropriate, the
         ! stepsize is decreased by a factor 2, but only increased
         ! by a factor of 1.5.
         IF( dS .LT. Model%Step ) THEN
            dT = 1.5*dT
         END IF

         IF( S .GE. Model%Step ) THEN
            CALL Insert( Model%CaptureZone, X, Y, Xn, Yn, Model%Width )
            S = 0.0
         END IF

         IF( T .GE. Model%Duration ) THEN
            CALL Insert( Model%CaptureZone, X, Y, Xn, Yn, Model%Width )
            EXIT
         END IF

         X = Xn
         Y = Yn
      END DO

      Status = SUCCESS
   END FUNCTION BackwardTrack_Model


   !---------------------------------------------------------------------------
   ! CreateCaptureZone_Model
   !---------------------------------------------------------------------------
   SUBROUTINE CreateCaptureZone_Model( Model, nSims, Filename, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      INTEGER,       INTENT(IN)    :: nSims
      CHARACTER(*),  INTENT(IN)    :: Filename
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Declare the local variables.
      INTEGER :: h, i, j, k

      REAL(8), DIMENSION(6)         :: Avg      ! (6x1) parameter expected value matrix
      REAL(8), DIMENSION(6)         :: Std      ! (6x1) parameter standard deviation matrix
      REAL(8), DIMENSION(6,6)       :: Cov      ! (6x6) covariance matrix
      TYPE(T_GEOHYDROLOGY)          :: Geo      ! geohydrology statistics

      REAL(8), DIMENSION(nSims,6)   :: P        ! matrix of realizations
      REAL(8), DIMENSION(6)         :: Param    ! parameter vector

      INTEGER                       :: Status
      REAL(8)                       :: Weight

      INTEGER                       :: nActive
      REAL(8)                       :: dX, dY

      REAL(8), DIMENSION(Model%nCond, Model%nThick) :: AvgRec
      REAL(8), DIMENSION(Model%nCond, Model%nThick) :: AvgMag
      REAL(8), DIMENSION(Model%nCond, Model%nThick) :: AvgDir

      INTEGER, DIMENSION(Model%nCond, Model%nThick) :: nSuccess
      INTEGER, DIMENSION(Model%nCond, Model%nThick) :: nTracks

      ! Check the arguments.
      IF( nSims .LT. 1 .OR. Model%nCond .LT. 1 .OR. Model%nThick .LT. 1 ) THEN
         ErrNo = CAPTUREZONE_ERROR
         RETURN
      END IF

      ! Deactivate the observation wells within the Buffer Zone of any pumping well.
      DO i = 1,Model%nObs
         Model%Obs(i)%isActive = .TRUE.
      END DO

      DO i = 1,Model%nObs
         DO j = 1, Model%nWell
            dX = Model%Obs(i)%X - Model%Well(j)%X
            dY = Model%Obs(i)%Y - Model%Well(j)%Y
            IF( HYPOT(dX,dY) .LE. Model%BufferZone ) THEN
               Model%Obs(i)%isActive = .FALSE.
               WRITE(LUNIT,'(''  --- Obs('', I3, '') deactivated due to proximity with Well('', I3, ''): '', F10.2, '' < '', F10.2)') &
               i, j, HYPOT(dX,dY), Model%BufferZone
            END IF
         END DO
      END DO

      DO i = 1,Model%nObs
         IF( Model%Obs(i)%isActive ) nActive = nActive + 1
      END DO

      ! Make certain that there are enough active observations to actually
      ! do the fit and subsequent simulations.
      IF( nActive .LT. 6 ) THEN
         ErrNo = DATA_ERROR
         CALL ExecutionError( DATA_ERROR, 'Model.f95', 'CreateCaptureZone' )
         RETURN
      END IF

      ! Initialize the capture zone grids.  This zeros out all of the
      ! probabilities and counts.
      CALL Initialize( Model%CaptureZone )

      nSuccess = 0
      nTracks  = 0

      ! Sweep through the conductivity values.
      DO i = 1, Model%nCond
         CALL SetConductivity( Model%Aquifer, Model%Cond(i)%K )

         ! Sweep through the thickness values.
         DO h = 1, Model%nThick
            CALL SetThickness( Model%Aquifer, Model%Thick(h)%Thickness )

            IF( Model%Verbosity .GE. 1) THEN
               WRITE(LUNIT,*)
               WRITE(LUNIT,*)
               WRITE(LUNIT,'(''==============================================================================='')')
               WRITE(LUNIT,'(''Simulation Batch: '', I5 )') (i-1)*Model%nThick + h
               WRITE(LUNIT,'(''==============================================================================='')')

               WRITE(LUNIT,*)
               WRITE(LUNIT,'(''Stochastic Aquifer Parameters'')')
               WRITE(LUNIT,'(''----------------------------------------'')')
               WRITE(LUNIT,'(''                     Value       Weight '')')
               WRITE(LUNIT,'(''----------------------------------------'')')
               WRITE(LUNIT,'(''Conductivity: '', E12.4, 1X, F12.6)') Model%Cond(i)%K, Model%Cond(i)%Weight
               WRITE(LUNIT,'(''Thickness:    '', E12.4, 1X, F12.6)') Model%Thick(h)%Thickness, Model%Thick(h)%Weight
               WRITE(LUNIT,'(''----------------------------------------'')')
            END IF

            ! Fit the regional parameters and compute the associated statistics.
            CALL Fit( Model, Avg, Std, Cov, Geo )

            AvgRec(i,h) = Geo%ERec
            AvgMag(i,h) = Geo%EMag
            AvgDir(i,h) = Geo%EDir

            ! Simulate parameters.
            CALL MVNormalRNG(nSims, 6, Avg, Cov, P)

            ! Report the simulated parameters.
            IF( Model%Verbosity .GE. 4 ) THEN
               WRITE(LUNIT,*)
               WRITE(LUNIT,'(''Simulated Parameters'')')
               WRITE(LUNIT,'(''------------------------------------------------------------------------------'')')
               WRITE(LUNIT,'(''      A            B            C            D            E            F      '')')
               WRITE(LUNIT,'(''------------------------------------------------------------------------------'')')
               DO j = 1, nSims
                  WRITE(LUNIT, '( 6(E12.4,1X) )' ) (P(j,k), k=1,6)
               END DO
               WRITE(LUNIT,'(''------------------------------------------------------------------------------'')')
               WRITE(LUNIT,*)
            END IF

            ! High-level progress report.
            WRITE(*,*)
            WRITE(*, '( I3, '' of '', I3, 5X, ''Conductivity = '', E10.3, 5X, ''Thickness = '', F10.3)' ) &
               (i-1)*Model%nThick + h, Model%nCond*Model%nThick, Model%Cond(i)%K, Model%Thick(h)%Thickness

            ! Sweep through the simulations.
            DO j = 1, nSims
               Param(1:6) = P(j,1:6)
               CALL SetParameters( Model%Regional, Param )

               ! For the current k and t, and the current parameters, backtrack.
               Status = PackOfTracks( Model )

               SELECT CASE( Status )
                  CASE( SUCCESS )
                     ! Register the realization.  By using the product of the
                     ! two weights, we are assuming that the conductivity and
                     ! the thickness are statistically independent.
                     Weight = Model%Cond(i)%Weight * Model%Thick(h)%Weight
                     CALL Register( Model%CaptureZone, Weight )
                     WRITE(*,'(''+'')', ADVANCE='NO')

                     nSuccess(i,h) = nSuccess(i,h) + 1
                     nTracks(i,h)  = nTracks(i,h)  + Model%nTracks

                  CASE( DRY_AQUIFER )
                     WRITE(*,'(''-'')', ADVANCE='NO')

                  CASE( DID_NOT_CONVERGE )
                     WRITE(*,'(''X'')', ADVANCE='NO')

                  CASE( UNKNOWN_FAILURE )
                     WRITE(*,'(''?'')', ADVANCE='NO')
                     nTracks(i,h) = 0
               END SELECT

               IF( MOD(j,100) .EQ. 0 ) THEN
                   WRITE(SUNIT, '('' ('', I4, '')'')') j
               END IF
            END DO      ! next simulation
            WRITE(*,*)

         END DO         ! next thickness
      END DO            ! next conductivity
      WRITE(*,*)

      ! Create the grid file (ESRI or SURFER).
      IF( Model%GridType .EQ. ESRI_GRID ) THEN
         CALL CreateEsriGridFile( Model%CaptureZone, Filename )
      ELSE IF( Model%GridType .EQ. SURFER_GRID ) THEN
         CALL CreateSurferGridFile( Model%CaptureZone, Filename )
      ELSE         ! ( Model%GridType .EQ. ASCII_GRID )
         CALL CreateAsciiGridFile( Model%CaptureZone, Filename )
      ENDIF

      ! Report the summary tables.
      IF( Model%Verbosity .GE. 1 ) THEN
         WRITE(LUNIT,*)
         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''==============================================================================='')')
         WRITE(LUNIT,'(''Simulation Summaries'')')
         WRITE(LUNIT,'(''==============================================================================='')')

         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Counts'')')
         WRITE(LUNIT,'(''--------------------------'')')
         WRITE(LUNIT,'(''# Pumping Wells:   '', I6)') Model%nWell
         WRITE(LUNIT,'(''# Obs:             '', I6)') Model%nObs
         WRITE(LUNIT,'(''# Active Obs:      '', I6)') nActive
         WRITE(LUNIT,'(''# Inactive Obs:    '', I6)') Model%nObs - nActive
         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''# Thicknesses:     '', I6)') Model%nThick
         WRITE(LUNIT,'(''# Conductivities:  '', I6)') Model%nCond
         WRITE(LUNIT,'(''# Simulations per: '', I6)') nSims
         WRITE(LUNIT,'(''--------------------------'')')

         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Recharge [L/T]'')')
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         WRITE(LUNIT,'(11X, 50F11.2)') ( Model%Thick(h)%Thickness, h = 1, Model%nThick )
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         DO i = 1, Model%nCond
            WRITE(LUNIT,'(E10.3, '':'', 50E11.3)') Model%Cond(i)%K, ( AvgRec(i,h), h = 1, Model%nThick )
         END DO
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )

         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Q Magnitude [L^2/T]'')')
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         WRITE(LUNIT,'(11X, 50F11.2)') ( Model%Thick(h)%Thickness, h = 1, Model%nThick )
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         DO i = 1, Model%nCond
            WRITE(LUNIT,'(E10.3, '':'', 50E11.3)') Model%Cond(i)%K, ( AvgMag(i,h), h = 1, Model%nThick )
         END DO
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )

         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Q Direction [deg]'')')
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         WRITE(LUNIT,'(11X, 50F11.2)') ( Model%Thick(h)%Thickness, h = 1, Model%nThick )
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         DO i = 1, Model%nCond
            WRITE(LUNIT,'(E10.3, '':'', 50F11.1)') Model%Cond(i)%K, ( RAD_TO_DEG*AvgDir(i,h), h = 1, Model%nThick )
         END DO
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )

         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Number of Successes'')')
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         WRITE(LUNIT,'(11X, 50F11.2)') ( Model%Thick(h)%Thickness, h = 1, Model%nThick )
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         DO i = 1, Model%nCond
            WRITE(LUNIT,'(E10.3, '':'', 50I11)') Model%Cond(i)%K, ( nSuccess(i,h), h = 1, Model%nThick )
         END DO
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )

         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Number of Tracks'')')
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         WRITE(LUNIT,'(11X, 50F11.2)') ( Model%Thick(h)%Thickness, h = 1, Model%nThick )
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
         DO i = 1, Model%nCond
            WRITE(LUNIT,'(E10.3, '':'', 50I11)') Model%Cond(i)%K, ( nTracks(i,h), h = 1, Model%nThick )
         END DO
         WRITE(LUNIT,'(''------------'', 50A11)') ( '-----------', h = 1, Model%nThick )
      END IF

      ! Set the model parameters back to the last fitted average.
      CALL SetParameters( Model%Regional, Avg )

      ErrNo = NO_ERROR
   END SUBROUTINE CreateCaptureZone_Model


   !---------------------------------------------------------------------------
   ! Discharge_Model
   !---------------------------------------------------------------------------
   FUNCTION Discharge_Model( Model, X, Y ) RESULT( Q )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(IN)  :: Model    ! model of interest
      REAL(8),       INTENT(IN)  :: X        ! X coordinate of interest
      REAL(8),       INTENT(IN)  :: Y        ! Y coordinate of interest

      REAL(8), DIMENSION(2)      :: Q        ! resulting discharge

      ! Declare the local variables.
      INTEGER i

      ! Compute the sum of the components.
      Q = Discharge( Model%Regional, X, Y )

      DO i = 1, Model%nWell
         Q = Q + Discharge( Model%Well(i), X, Y )
      END DO
   END FUNCTION Discharge_Model


   !---------------------------------------------------------------------------
   ! Fit
   !
   ! Notes:
   !  o  The pumping well of interest is used as the origin of the model. This
   !     choice affects the fitted parameters A through F.
   !---------------------------------------------------------------------------
   SUBROUTINE Fit_Model( Model, Avg, Std, Cov, Geo )
      ! Declare the arguments.
      TYPE(T_MODEL),           INTENT(INOUT) :: Model ! model to fit
      REAL(8), DIMENSION(6),   INTENT(OUT)   :: Avg   ! (6x1) parameter expected value matrix
      REAL(8), DIMENSION(6),   INTENT(OUT)   :: Std   ! (6x1) parameter standard deviation matrix
      REAL(8), DIMENSION(6,6), INTENT(OUT)   :: Cov   ! (6x6) covariance matrix
      TYPE(T_GEOHYDROLOGY),    INTENT(OUT)   :: Geo   ! geohydrology statistics

      ! Declare the local variables.
      INTEGER  :: i, j, k, nActive
      REAL(8)  :: dx, dy, dDir

      REAL(8), DIMENSION( Model%nObs )             :: Pev      ! expected value of the discharge potentials from measurements
      REAL(8), DIMENSION( Model%nObs )             :: Pstd     ! standard deviation of the discharge potentials from measurement
      REAL(8), DIMENSION( Model%nObs )             :: Pwell    ! discharge potential due to the wells at the observation locations

      REAL(8), DIMENSION( Model%nObs, 6 )          :: X        ! model matrix
      REAL(8), DIMENSION( Model%nObs, Model%nObs ) :: Vinv     ! inverse of the observation variance matrix
      REAL(8), DIMENSION( Model%nObs )             :: b        ! measured right-hand-side vector
      REAL(8), DIMENSION(6,6)                      :: L        ! Cholesky decomposition of (X' * Vinv * X)

      LOGICAL, DIMENSION( Model%nObs, 6)           :: XMASK, XXMASK
      LOGICAL, DIMENSION( Model%nObs)              :: BMASK, BBMASK
      LOGICAL, DIMENSION( Model%nObs, Model%nObs ) :: VMASK, VVMASK

      REAL(8), ALLOCATABLE                         :: XX(:,:)
      REAL(8), ALLOCATABLE                         :: VVinv(:,:)
      REAL(8), ALLOCATABLE                         :: bb(:)

      REAL(8), DIMENSION(6)                        :: AA
      REAL(8), DIMENSION(6,6)                      :: CC
      TYPE(T_GEOHYDROLOGY)                         :: GG

      REAL(8), DIMENSION( Model%nObs, 5)           :: DFBETAS  ! leave-one-out analysis for each observation

      ! Use the pumping well of interest as the model origin.
      CALL SetOrigin( Model%Regional, Model%Xo, Model%Yo )

      ! Setup the regression matrix (X), the inverse of the observation
      ! variance matrix (Vinv), and the right-hand-side (Y).
      Vinv = 0

      DO i = 1, Model%nObs
         dx   = Model%Obs(i)%X - Model%Xo
         dy   = Model%Obs(i)%Y - Model%Yo

         X(i,1) = dx*dx
         X(i,2) = dy*dy
         X(i,3) = dx*dy
         X(i,4) = dx
         X(i,5) = dy
         X(i,6) = 1

         CALL ElevationToPotentialStats( Model%Aquifer, Model%Obs(i)%Zev, Model%Obs(i)%Zstd, Pev(i), Pstd(i) )

         Pwell(i) = 0
         DO j = 1, Model%nWell
           Pwell(i) = Pwell(i) - Potential( Model%Well(j), Model%Obs(i)%X, Model%Obs(i)%Y )
         END DO

         b(i) = Pev(i) - Pwell(i)
         Vinv(i,i) = 1/MAX(Pstd(i)*Pstd(i), MIN_PVAR)
      END DO

      ! Create the masks for the active observations.
      XMASK = .TRUE.
      BMASK = .TRUE.
      VMASK = .TRUE.

      nActive = 0

      DO i = 1, Model%nObs
         IF( Model%Obs(i)%isActive .EQV. .TRUE. ) THEN
            nActive = nActive + 1
         ELSE
            XMASK(i,:) = .FALSE.

            VMASK(:,i) = .FALSE.
            VMASK(i,:) = .FALSE.

            BMASK(i)   = .FALSE.
         END IF
      END DO

      ! Allocate the working storage.
      ALLOCATE( XX( nActive, 6 ) )
      ALLOCATE( VVinv( nActive, nActive ) )
      ALLOCATE( bb( nActive ) )

      ! Compute the solution to the weighted least squares problem.
      XX    = RESHAPE( PACK(X,   XMASK), SHAPE(XX) )
      bb    = RESHAPE( PACK(b,   BMASK), SHAPE(bb) )
      VVinv = RESHAPE( PACK(Vinv,VMASK), SHAPE(VVinv) )

      CALL CholeskyDecomposition( 6, MATMUL(MATMUL(TRANSPOSE(XX), VVinv), XX), L )
      CALL CholeskySolve( 6, L, MATMUL(MATMUL(TRANSPOSE(XX), VVinv), bb), Avg )

      ! Compute the asymptotice parameter covariance matrix.
      CALL CholeskyInverse( 6, L, Cov )

      ! Set the parameters to the fitted values.
      CALL SetParameters( Model%Regional, Avg )

      ! Compute the parameter standard deviations.
      DO j = 1, 6
         Std(j) = SQRT( MAX( Cov(j,j), real(0,8) ) )
      END DO

      CALL ComputeGeohydrology( Avg, Cov, Geo )

      !------------------------------------------
      ! Write out the level-2 fit-by-fit summaries to the log file.
      !------------------------------------------
      IF( Model%Verbosity .GE. 2) THEN
         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Fitted Model Parameters using (Xo,Yo) = ('', F14.2, '','', F14.2, '')'' )') Model%Xo, Model%Yo
         WRITE(LUNIT,'(''--------------------------------------------------------------------------------'')')
         WRITE(LUNIT,'(''             Average      Std Dev                    Correlations'')')
         WRITE(LUNIT,'(''--------------------------------------------------------------------------------'')')
         WRITE(LUNIT,'(''A (xx): '', E12.4, 1X, E12.4, 5X, 6(F6.2,1X) )') Avg(1), Std(1), (Cov(1,k)/(Std(1)*Std(k)), k=1,1)
         WRITE(LUNIT,'(''B (yy): '', E12.4, 1X, E12.4, 5X, 6(F6.2,1X) )') Avg(2), Std(2), (Cov(2,k)/(Std(2)*Std(k)), k=1,2)
         WRITE(LUNIT,'(''C (xy): '', E12.4, 1X, E12.4, 5X, 6(F6.2,1X) )') Avg(3), Std(3), (Cov(3,k)/(Std(3)*Std(k)), k=1,3)
         WRITE(LUNIT,'(''D  (x): '', E12.4, 1X, E12.4, 5X, 6(F6.2,1X) )') Avg(4), Std(4), (Cov(4,k)/(Std(4)*Std(k)), k=1,4)
         WRITE(LUNIT,'(''E  (y): '', E12.4, 1X, E12.4, 5X, 6(F6.2,1X) )') Avg(5), Std(5), (Cov(5,k)/(Std(5)*Std(k)), k=1,5)
         WRITE(LUNIT,'(''F  ( ): '', E12.4, 1X, E12.4, 5X, 6(F6.2,1X) )') Avg(6), Std(6), (Cov(6,k)/(Std(6)*Std(k)), k=1,6)
         WRITE(LUNIT,'(''--------------------------------------------------------------------------------'')')

         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Resulting Geohydrologic Conditions'')')
         WRITE(LUNIT,'(''---------------------------------------------------'')')
         WRITE(LUNIT,'(''                  Average      Std Dev       Units '')')
         WRITE(LUNIT,'(''---------------------------------------------------'')')
         WRITE(LUNIT,'(''Recharge:    '', E12.3, 1X, E12.3, ''       [L/T]'')') Geo%ERec, Geo%SRec
         WRITE(LUNIT,'(''Qx:          '', E12.3, 1X, E12.3, ''     [L^2/T]'')') Geo%EQx, Geo%SQx
         WRITE(LUNIT,'(''Qy:          '', E12.3, 1X, E12.3, ''     [L^2/T]'')') Geo%EQy, Geo%SQy
         WRITE(LUNIT,'(''Q Magnitude: '', E12.3, 1X, E12.3, ''     [L^2/T]'')') Geo%EMag, Geo%SMag
         WRITE(LUNIT,'(''Q Direction: '', F12.1, 1X, F12.1, ''       [deg]'')') RAD_TO_DEG*Geo%EDir, RAD_TO_DEG*Geo%SDir
         WRITE(LUNIT,'(''---------------------------------------------------'')')
      END IF

      ! TODO: regression diagonostics
      !     sign test of residuals
      !     test of normality for residuals
      !     test that the variance of the weighted residulas is around 1

      ! TODO: infulential data
      !     DFFIT
      !     z-score of residuals w/ p-value

      ! Re-allocate the working storage.
      DEALLOCATE(XX)
      DEALLOCATE(VVinv)
      DEALLOCATE(bb)

      ALLOCATE( XX( nActive-1, 6 ) )
      ALLOCATE( VVinv( nActive-1, nActive-1 ) )
      ALLOCATE( bb( nActive-1 ) )

      ! Do the leave-one-out analysis for all active observations.
      ! Inactive observations are marked by INF.
      IF( Model%Verbosity .GE. 3) THEN
         DO i = 1, Model%nObs
            IF( Model%Obs(i)%isActive .EQV. .TRUE. ) THEN
               XXMASK = XMASK
               XXMASK(i,:) = .FALSE.
               XX = RESHAPE( PACK(X,XXMASK), SHAPE(XX) )

               BBMASK = BMASK
               BBMASK(i) = .FALSE.
               bb = RESHAPE( PACK(b,BBMASK), SHAPE(bb) )

               VVMASK = VMASK
               VVMASK(:,i) = .FALSE.
               VVMASK(i,:) = .FALSE.
               VVinv = RESHAPE( PACK(Vinv,VVMASK), SHAPE(VVinv) )

               CALL CholeskyDecomposition( 6, MATMUL(MATMUL(TRANSPOSE(XX), VVinv), XX), L )
               CALL CholeskySolve( 6, L, MATMUL(MATMUL(TRANSPOSE(XX), VVinv), bb), AA )
               CALL CholeskyInverse( 6, L, CC )
               CALL ComputeGeohydrology( AA, CC, GG )

               IF( GG%SRec > 0 .AND. Geo%SRec > 0 ) THEN
                  DFBETAS(i,1) = (GG%ERec - Geo%ERec)/Geo%SRec
               ELSE
                  DFBETAS(i,1) = INFINITY
               END IF

               IF( GG%SQx > 0 .AND. Geo%SQx > 0 ) THEN
                  DFBETAS(i,2) = (GG%EQx  - Geo%EQx )/Geo%SQx
               ELSE
                  DFBETAS(i,2) = INFINITY
               END IF

               IF( GG%SQy > 0 .AND. Geo%SQy > 0 ) THEN
                  DFBETAS(i,3) = (GG%EQy  - Geo%EQy )/Geo%SQy
               ELSE
                  DFBETAS(i,3) = INFINITY
               END IF

               IF( GG%SMag > 0 .AND. Geo%SMag > 0 ) THEN
                  DFBETAS(i,4) = (GG%EMag - Geo%EMag)/Geo%SMag
               ELSE
                  DFBETAS(i,4) = INFINITY
               END IF

               IF( GG%SDir > 0 .AND. Geo%SDir > 0 ) THEN
                  dDir = GG%EDir - Geo%EDir
                  IF( dDir .LE. -ONE_PI ) dDir = dDir + TWO_PI
                  IF( dDir .GE.  ONE_PI ) dDir = TWO_PI - dDir
                  DFBETAS(i,5) = dDir/Geo%SDir
               ELSE
                  DFBETAS(i,5) = INFINITY
               END IF
            END IF
         END DO

         WRITE(LUNIT,*)
         WRITE(LUNIT,'(''Leave-one-out Influential Data Indentification'')')
         WRITE(LUNIT,'(''-------------------------------------------------------------------------------------'')')
         WRITE(LUNIT,'(''                                          Scaled           Scaled           Scaled   '')')
         WRITE(LUNIT,'(''                                       Change in        Change in        Change in   '')')
         WRITE(LUNIT,'(''Index            X            Y         Recharge        Magnitude        Direction   '')')
         WRITE(LUNIT,'(''-------------------------------------------------------------------------------------'')')
         DO i = 1, Model%nObs
            WRITE(LUNIT,'(I5, 1X, F12.2, 1X, F12.2, 5X)', ADVANCE='NO') i, Model%Obs(i)%X, Model%Obs(i)%Y

            IF( Model%Obs(i)%isActive .EQV. .FALSE. ) THEN
               WRITE(LUNIT,'(4X, ''INACTIVE         INACTIVE         INACTIVE'')')
            ELSE
               IF( ISINF(DFBETAS(i,1)) ) THEN
                  WRITE(LUNIT,'(9X, ''N/A'', 5X)', ADVANCE='NO')
               ELSE IF( ABS(DFBETAS(i,1)) .GE. INFLUENTIAL ) THEN
                  WRITE(LUNIT,'(F12.2, ''<<<< '')', ADVANCE='NO') DFBETAS(i,1)
               ELSE
                  WRITE(LUNIT,'(F12.2, 5X)', ADVANCE='NO') DFBETAS(i,1)
               END IF

               IF( ISINF(DFBETAS(i,4)) ) THEN
                  WRITE(LUNIT,'(9X, ''N/A'', 5X)', ADVANCE='NO')
               ELSE IF( ABS(DFBETAS(i,4)) .GE. INFLUENTIAL ) THEN
                  WRITE(LUNIT,'(F12.2, ''<<<< '')', ADVANCE='NO') DFBETAS(i,4)
               ELSE
                  WRITE(LUNIT,'(F12.2, 5X)', ADVANCE='NO') DFBETAS(i,4)
               END IF

               IF( ISINF(DFBETAS(i,5)) ) THEN
                  WRITE(LUNIT,'(9X, ''N/A'', 5X)', ADVANCE='NO')
               ELSE IF( ABS(DFBETAS(i,5)) .GE. INFLUENTIAL ) THEN
                  WRITE(LUNIT,'(F12.2, ''<<<< '')', ADVANCE='NO') DFBETAS(i,5)
               ELSE
                  WRITE(LUNIT,'(F12.2, 5X)', ADVANCE='NO') DFBETAS(i,5)
               END IF
               WRITE(LUNIT,*)
            END IF
         END DO
         WRITE(LUNIT,'(''-------------------------------------------------------------------------------------'')')
         WRITE(LUNIT,*)
      END IF

   END SUBROUTINE Fit_Model

   !---------------------------------------------------------------------------
   ! ComputeGeohydrology
   !
   !  Compute the geohydrology statistics (rechange, magnitude and
   !  direction of the regional discharge at the origin) using first-order
   !  second moment approximations.
   !---------------------------------------------------------------------------
   SUBROUTINE ComputeGeohydrology_Model( Avg, Cov, Geo )
      ! Declare the arguments.
      REAL(8), DIMENSION(6),   INTENT(IN)    :: Avg   ! (6x1) parameter expected value matrix
      REAL(8), DIMENSION(6,6), INTENT(IN)    :: Cov   ! (6x6) covariance matrix
      TYPE(T_GEOHYDROLOGY),    INTENT(OUT)   :: Geo   ! geohydrology statistics

      ! Declare the local variables.
      REAL(8) :: T, Qx, Qy
      REAL(8) :: U, dUdQx, dUdQy, d2UdQx2, d2UdQy2, d2UdQxQy
      REAL(8) :: D, dDdQx, dDdQy, d2DdQx2, d2DdQy2, d2DdQxQy

      ! Statistics for the regional uniform recharge.
      Geo%ERec = -2 * ( Avg(1) + Avg(2) )
      Geo%VRec = 4*Cov(1,1) + 4*Cov(2,2) + 8*Cov(1,2)
      Geo%SRec = SQRT( MAX( Geo%VRec, real(0,8) ) )

      ! Statistics for discharge at the origin.
      Geo%EQx = -Avg(4)
      Geo%VQx = Cov(4,4)
      Geo%SQx = SQRT( MAX( Geo%VQx, real(0,8) ) )

      Geo%EQy = -Avg(5)
      Geo%VQy = Cov(5,5)
      Geo%SQy = SQRT( MAX( Geo%VQy, real(0,8) ) )

      ! Statistics for the magnitude and direction of the regional discharge at the origin.
      Qx = -Avg(4)
      Qy = -Avg(5)
      T = Qx**2 + Qy**2

      IF( T .GT. real(0,8) ) THEN
         ! Statistics for the magnitude.
         U = SQRT( T )

         dUdQx   = Qx / U
         d2UdQx2 = ( 1 - dUdQx**2 ) / U

         dUdQy   = Qy / U
         d2UdQy2 = ( 1 - dUdQy**2 ) / U

         d2UdQxQy = -( dUdQx * dUdQy ) / U

         Geo%EMag = U + 0.5*Cov(4,4)*d2UdQx2 + 0.5*Cov(5,5)*d2UdQy2 + Cov(4,5)*d2UdQxQy
         Geo%VMag = Cov(4,4)*dUdQx**2 + Cov(5,5)*dUdQy**2 + 2*Cov(4,5)*dUdQx*dUdQy
         Geo%SMag = SQRT( MAX( Geo%VMag, real(0,8) ) )

         ! Statistics for the direction.
         D = DATAN2( Qy, Qx )

         dDdQx   = -Qy / T
         d2DdQx2 =  2*Qx*Qy / T**2

         dDdQy   =  Qx / T
         d2DdQy2 = -2*Qx*Qy / T**2

         d2DdQxQy = ( Qy**2 - Qx**2 ) / T**2

         Geo%EDir = D + 0.5*Cov(4,4)*d2DdQx2 + 0.5*Cov(5,5)*d2DdQy2 + Cov(4,5)*d2DdQxQy
         Geo%VDir = Cov(4,4)*dDdQx**2 + Cov(5,5)*dDdQy**2 + 2*Cov(4,5)*dDdQx*dDdQy
         Geo%SDir = SQRT( MAX( Geo%VDir, real(0,8) ) )
      ELSE
         Geo%EMag = 0
         Geo%VMag = -1
         Geo%SMag = -1

         Geo%EDir = 0
         Geo%VDir = -1
         Geo%SDir = -1
      END IF
   END SUBROUTINE ComputeGeohydrology_Model


   !---------------------------------------------------------------------------
   ! Head_Model
   !---------------------------------------------------------------------------
   FUNCTION Head_Model( Model, X, Y ) RESULT( Head )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(IN) :: Model  ! model of interest
      REAL(8),       INTENT(IN) :: X      ! X coordinate of interest
      REAL(8),       INTENT(IN) :: Y      ! Y coordinate of interest

      REAL(8)                   :: Head   ! resulting head

      ! Compute the head.
      Head = PotentialToHead( Model%Aquifer, Potential( Model, X, Y ) )
   END FUNCTION Head_Model


   !---------------------------------------------------------------------------
   ! Potential_Model
   !---------------------------------------------------------------------------
   FUNCTION Potential_Model( Model, X, Y ) RESULT( Phi )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(IN) :: Model     ! model of interest
      REAL(8),       INTENT(IN) :: X         ! X coordinate of interest
      REAL(8),       INTENT(IN) :: Y         ! Y coordinate of interest

      REAL(8)                   :: Phi       ! resulting discharge potential

      ! Declare the local variables.
      INTEGER i

      ! Compute the sum of the components.
      Phi = Potential( Model%Regional, X, Y )

      DO i = 1, Model%nWell
         Phi = Phi + Potential( Model%Well(i), X, Y )
      END DO
   END FUNCTION Potential_Model


   !---------------------------------------------------------------------------
   ! ResetAll_Model
   !---------------------------------------------------------------------------
   SUBROUTINE ResetAll_Model( Model )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model

      CALL Reset( Model%Aquifer )
      CALL Reset( Model%Regional )
      CALL Reset( Model%CaptureZone )

      Model%nWell    = 0
      Model%Well     = T_WELL( 0, 0, 0, 0 )

      Model%nObs     = 0
      Model%Obs      = T_OBS( 0, 0, 0, 0, .TRUE. )

      Model%nCond    = 0
      Model%Cond     = T_COND( 0, 0 )

      Model%nThick   = 0
      Model%Thick    = T_THICK( 0, 0 )

      Model%GridType = SURFER_GRID

      Model%Duration = 0
      Model%Step     = 0
      Model%Xo       = 0
      Model%Yo       = 0
      Model%Radius   = 0
      Model%Width    = 0
      Model%maxSep   = 0
      Model%nTracks  = 0
   END SUBROUTINE ResetAll_Model


   !---------------------------------------------------------------------------
   ! RKSolve_Model
   !
   ! Arguments:
   !
   !     Model       the current groundwater model (IN).
   !     dT          the current time step (IN).
   !     Xo,Yo       the starting point (IN).
   !     Xn,Yn       the computed ending point (OUT).
   !     Xe,Ye       the computed approximate magnitude of the error (OUT).
   !
   ! Notes:
   ! o   This routine does one step of integration of the particle velocity.
   !
   ! o   If dT < 0 then the step is a backtrack, if dT > 0 the step is a
   !     foreward track.
   !
   ! o   This routine carries out one step of a fifth-order Runge-Kutta-
   !     Fehlberg method using the Cash-Karp constants.  This variation of
   !     the embedded Runge-Kutta method is recommended by Press et al.
   !     (1992, p.716-717).
   !
   ! o   The returned arguments (Xe,Ye) are exstimates of the local error
   !     generated by comparing the fifth-order Runge-Kutta solution to the
   !     embedded fourth-order Runge-Kutta solution.
   !
   ! References:
   !
   ! o   Press, W. H., S. A. Teukolsky, W. T. Vetterling, and B. P. Flannery,
   !     1992, Numerical Recipes in C: The Art of Scientific Computing, 2nd
   !     edition, Cambridge University Press, 994 pp.
   !---------------------------------------------------------------------------
   FUNCTION RKSolve_Model( Model, dT, Xo, Yo, Xn, Yn, Xe, Ye ) RESULT( Status )
      ! Declare arguments
      TYPE(T_MODEL), INTENT(IN)  :: Model
      REAL(8),       INTENT(IN)  :: dT          ! time step
      REAL(8),       INTENT(IN)  :: Xo, Yo      ! old point
      REAL(8),       INTENT(OUT) :: Xn, Yn      ! new point
      REAL(8),       INTENT(OUT) :: Xe, Ye      ! estimated error magnitude

      INTEGER                    :: Status

      ! Declare local variables.
      REAL(8), DIMENSION(2) :: V
      REAL(8), DIMENSION(6) :: dX, dY

      ! Cash-Karp parameters for embedded Runga-Kutta method.  These constants
      ! are taken from a table on page 717 of Press et al. (1992).
      REAL(8), DIMENSION(2:6,1:5), PARAMETER :: B = &
         RESHAPE( SOURCE = (/ &
         DBLE(1)/DBLE(5), DBLE(3)/DBLE(40), DBLE(3)/DBLE(10), DBLE(-11)/DBLE(54), DBLE(1631)/DBLE(55296), &
         DBLE(0), DBLE(9)/DBLE(40), DBLE(-9)/DBLE(10), DBLE(5)/DBLE(2), DBLE(175)/DBLE(512), &
         DBLE(0), DBLE(0), DBLE(6)/DBLE(5), DBLE(-70)/DBLE(27), DBLE(575)/DBLE(13824), &
         DBLE(0), DBLE(0), DBLE(0), DBLE(35)/DBLE(27), DBLE(44275)/DBLE(110592), &
         DBLE(0), DBLE(0), DBLE(0), DBLE(0), DBLE(253)/DBLE(4096) /), SHAPE = (/5,5/))

      REAL(8), DIMENSION(1:6), PARAMETER :: C = (/ &
         DBLE(37)/DBLE(378), DBLE(0), DBLE(250)/DBLE(621), DBLE(125)/DBLE(594), &
         DBLE(0), DBLE(512)/DBLE(1771) /)

      REAL(8), DIMENSION(1:6), PARAMETER :: D = (/ &
         DBLE(2825)/DBLE(27648), DBLE(0), DBLE(18575)/DBLE(48384), &
         DBLE(13525)/DBLE(55296), DBLE(277)/DBLE(14336), DBLE(1)/DBLE(4) /)

      ! The Cash-Karp variation on the Runge-Kutta-Fehlberg method.

      V = Velocity( Model, Xo, Yo, Status )
      IF( Status .LT. 0 ) RETURN

      dX(1) = V(1) * dT
      dY(1) = V(2) * dT

      Xn = Xo + B(2,1)*dX(1)
      Yn = Yo + B(2,1)*dY(1)

      V = Velocity( Model, Xn, Yn, Status )
      IF( Status .LT. 0 ) RETURN

      dX(2) = V(1) * dT
      dY(2) = V(2) * dT

      Xn = Xo + B(3,1)*dX(1) + B(3,2)*dX(2)
      Yn = Yo + B(3,1)*dY(1) + B(3,2)*dY(2)

      V = Velocity( Model, Xn, Yn, Status )
      IF( Status .LT. 0 ) RETURN

      dX(3) = V(1) * dT
      dY(3) = V(2) * dT

      Xn = Xo + B(4,1)*dX(1) + B(4,2)*dX(2) + B(4,3)*dX(3)
      Yn = Yo + B(4,1)*dY(1) + B(4,2)*dY(2) + B(4,3)*dY(3)

      V = Velocity( Model, Xn, Yn, Status )
      IF( Status .LT. 0 ) RETURN

      dX(4) = V(1) * dT
      dY(4) = V(2) * dT

      Xn = Xo + B(5,1)*dX(1) + B(5,2)*dX(2) + B(5,3)*dX(3) + B(5,4)*dX(4)
      Yn = Yo + B(5,1)*dY(1) + B(5,2)*dY(2) + B(5,3)*dY(3) + B(5,4)*dY(4)

      V = Velocity( Model, Xn, Yn, Status )
      IF( Status .LT. 0 ) RETURN

      dX(5) = V(1) * dT
      dY(5) = V(2) * dT

      Xn = Xo + B(6,1)*dX(1) + B(6,2)*dX(2) + B(6,3)*dX(3) + B(6,4)*dX(4) + B(6,5)*dX(5)
      Yn = Yo + B(6,1)*dY(1) + B(6,2)*dY(2) + B(6,3)*dY(3) + B(6,4)*dY(4) + B(6,5)*dY(5)

      V = Velocity( Model, Xn, Yn, Status )
      IF( Status .LT. 0 ) RETURN

      dX(6) = V(1) * dT
      dY(6) = V(2) * dT

      Xn = Xo + C(1)*dX(1) + C(2)*dX(2) + C(3)*dX(3) + C(4)*dX(4) + C(5)*dX(5) + C(6)*dX(6)
      Yn = Yo + C(1)*dY(1) + C(2)*dY(2) + C(3)*dY(3) + C(4)*dY(4) + C(5)*dY(5) + C(6)*dY(6)

      ! Compute the approximate magnitude of the error.
      Xe = Xo + D(1)*dX(1) + D(2)*dX(2) + D(3)*dX(3) + D(4)*dX(4) + D(5)*dX(5) + D(6)*dX(6)
      Xe = ABS( Xn - Xe )

      Ye = Yo + D(1)*dY(1) + D(2)*dY(2) + D(3)*dY(3) + D(4)*dY(4) + D(5)*dY(5) + D(6)*dY(6)
      Ye = ABS( Yn - Ye )

      Status = SUCCESS
   END FUNCTION RKSolve_Model


   !---------------------------------------------------------------------------
   ! SetEsriGrid
   !---------------------------------------------------------------------------
   SUBROUTINE SetEsriGrid_Model( Model, nRows, nCols, Xmin, Ymin, Cellsize, mRows, mCols, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      INTEGER,       INTENT(IN)    :: nRows
      INTEGER,       INTENT(IN)    :: nCols
      REAL(8),       INTENT(IN)    :: Xmin
      REAL(8),       INTENT(IN)    :: Ymin
      REAL(8),       INTENT(IN)    :: Cellsize
      INTEGER,       INTENT(IN)    :: mRows
      INTEGER,       INTENT(IN)    :: mCols
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Declare local variables.
      REAL(8) :: Xmax, Ymax

      ! Set the model's regional parameters.
      Model%GridType = ESRI_GRID

      Xmax = Xmin + (nCols-1)*Cellsize
      Ymax = Ymin + (nRows-1)*Cellsize

      IF( .NOT. SetGrid( Model%CaptureZone, nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols ) ) THEN
         ErrNo = CAPTUREZONE_ERROR
      ELSE
         ErrNo = NO_ERROR
      END IF
   END SUBROUTINE SetEsriGrid_Model


   !---------------------------------------------------------------------------
   ! SetSurferGrid
   !---------------------------------------------------------------------------
   SUBROUTINE SetSurferGrid_Model( Model, nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      INTEGER,       INTENT(IN)    :: nRows
      INTEGER,       INTENT(IN)    :: nCols
      REAL(8),       INTENT(IN)    :: Xmin
      REAL(8),       INTENT(IN)    :: Xmax
      REAL(8),       INTENT(IN)    :: Ymin
      REAL(8),       INTENT(IN)    :: Ymax
      INTEGER,       INTENT(IN)    :: mRows
      INTEGER,       INTENT(IN)    :: mCols
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Set the model's regional parameters.
      Model%GridType = SURFER_GRID

      IF( .NOT. SetGrid( Model%CaptureZone, nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols ) ) THEN
         ErrNo = CAPTUREZONE_ERROR
      ELSE
         ErrNo = NO_ERROR
      END IF
   END SUBROUTINE SetSurferGrid_Model

   !---------------------------------------------------------------------------
   ! SetAsciiGrid
   !---------------------------------------------------------------------------
   SUBROUTINE SetAsciiGrid_Model( Model, nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      INTEGER,       INTENT(IN)    :: nRows
      INTEGER,       INTENT(IN)    :: nCols
      REAL(8),       INTENT(IN)    :: Xmin
      REAL(8),       INTENT(IN)    :: Xmax
      REAL(8),       INTENT(IN)    :: Ymin
      REAL(8),       INTENT(IN)    :: Ymax
      INTEGER,       INTENT(IN)    :: mRows
      INTEGER,       INTENT(IN)    :: mCols
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Set the model's regional parameters.
      Model%GridType = ASCII_GRID

      IF( .NOT. SetGrid( Model%CaptureZone, nRows, nCols, Xmin, Xmax, Ymin, Ymax, mRows, mCols ) ) THEN
         ErrNo = CAPTUREZONE_ERROR
      ELSE
         ErrNo = NO_ERROR
      END IF
   END SUBROUTINE SetAsciiGrid_Model


   !---------------------------------------------------------------------------
   ! SetBufferZone
   !---------------------------------------------------------------------------
   SUBROUTINE SetBufferZone_Model( Model, Radius, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      REAL(8),       INTENT(IN)    :: Radius
      INTEGER,       INTENT(OUT)   :: ErrNo

      Model%BufferZone = Radius

      ErrNo = NO_ERROR
   END SUBROUTINE SetBufferZone_Model


   !---------------------------------------------------------------------------
   ! SetProperties
   !---------------------------------------------------------------------------
   SUBROUTINE SetProperties_Model( Model, Base, Porosity, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      REAL(8),       INTENT(IN)    :: Base
      REAL(8),       INTENT(IN)    :: Porosity
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Set the aquifer parameters.
      CALL SetBase( Model%Aquifer, Base )
      CALL SetPorosity( Model%Aquifer, Porosity )

      ErrNo = NO_ERROR
   END SUBROUTINE SetProperties_Model


   !---------------------------------------------------------------------------
   ! SetTrack
   !---------------------------------------------------------------------------
   SUBROUTINE SetTrack_Model( Model, Duration, Step, Xo, Yo, Radius, Width, maxSep, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      REAL(8),       INTENT(IN)    :: Duration
      REAL(8),       INTENT(IN)    :: Step
      REAL(8),       INTENT(IN)    :: Xo
      REAL(8),       INTENT(IN)    :: Yo
      REAL(8),       INTENT(IN)    :: Radius
      REAL(8),       INTENT(IN)    :: Width
      REAL(8),       INTENT(IN)    :: maxSep
      INTEGER,       INTENT(OUT)   :: ErrNo

      ! Setup the tracking parameters.
      IF( Duration .LE. 0 .OR. Step .LE. 0 .OR. Radius .LT. 0 .OR. Width .LE. 0 .OR. maxSep .LE. 0 ) THEN
         ErrNo = TRACK_ERROR
      ELSE
         Model%Duration = Duration
         Model%Step     = Step
         Model%Xo       = Xo
         Model%Yo       = Yo
         Model%Radius   = Radius
         Model%Width    = Width
         Model%maxSep   = maxSep
         Model%nTracks  = 0

         ErrNo = NO_ERROR
      END IF
   END SUBROUTINE SetTrack_Model


   !---------------------------------------------------------------------------
   ! SetVerbosity
   !---------------------------------------------------------------------------
   SUBROUTINE SetVerbosity_Model( Model, Level, ErrNo )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(INOUT) :: Model
      INTEGER,       INTENT(IN)    :: Level
      INTEGER,       INTENT(OUT)   :: ErrNo

      Model%Verbosity = Level

      ErrNo = NO_ERROR
   END SUBROUTINE SetVerbosity_Model


   !---------------------------------------------------------------------------
   ! Velocity_Model
   !---------------------------------------------------------------------------
   FUNCTION Velocity_Model( Model, X, Y, Status ) RESULT( V )
      ! Declare the arguments.
      TYPE(T_MODEL), INTENT(IN)  :: Model  ! model of interest
      REAL(8),       INTENT(IN)  :: X      ! X coordinate of interest
      REAL(8),       INTENT(IN)  :: Y      ! Y coordinate of interest
      INTEGER,       INTENT(OUT) :: Status ! result of the calculation

      REAL(8), DIMENSION(2)      :: V      ! resulting velocity

      ! Declare local variables.
      REAL(8) :: H

      ! Compute the velocity.
      H = Head( Model, X, Y )
      IF( H .LT. 0 ) THEN
         V = 0
         Status = DRY_AQUIFER
      ELSE
         V = DischargeToVelocity( Model%Aquifer, H, Discharge(Model,X,Y) )
         Status = SUCCESS
      END IF
   END FUNCTION Velocity_Model


!==============================================================================
END MODULE MODEL_MODULE
!==============================================================================