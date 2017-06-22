!==============================================================================
! Module AQUIFER_MODULE                                           (22-Jun-2017)
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
MODULE AQUIFER_MODULE
!==============================================================================
   USE CONSTANTS_MODULE
   IMPLICIT NONE

   !===========================================================================
   ! Type T_AQUIFER
   !===========================================================================
   TYPE T_AQUIFER
      REAL(8) :: Base         = 0         ! base elevation of aquifer
      REAL(8) :: Thickness    = INFINITY  ! aquifer thickness
      REAL(8) :: Porosity     = -1        ! aquifer porosity
      REAL(8) :: Conductivity = 1         ! hydraulic conductivity k
   END TYPE T_AQUIFER

   !===========================================================================
   ! INTERFACES
   !===========================================================================
   INTERFACE DischargeToVelocity
      MODULE PROCEDURE DischargeToVelocity_Aquifer
   END INTERFACE

   INTERFACE ElevationToPotential
      MODULE PROCEDURE ElevationToPotential_Aquifer
   END INTERFACE

   INTERFACE ElevationToPotentialStats
      MODULE PROCEDURE ElevationToPotentialStats_Aquifer
   END INTERFACE

   INTERFACE HeadToPotential
      MODULE PROCEDURE HeadToPotential_Aquifer
   END INTERFACE

   INTERFACE PotentialToElevation
      MODULE PROCEDURE PotentialToElevation_Aquifer
   END INTERFACE

   INTERFACE PotentialToHead
      MODULE PROCEDURE PotentialToHead_Aquifer
   END INTERFACE

   INTERFACE Reset
      MODULE PROCEDURE Reset_Aquifer
   END INTERFACE

   INTERFACE SetBase
      MODULE PROCEDURE SetBase_Aquifer
   END INTERFACE

   INTERFACE SetConductivity
      MODULE PROCEDURE SetConductivity_Aquifer
   END INTERFACE

   INTERFACE SetPorosity
      MODULE PROCEDURE SetPorosity_Aquifer
   END INTERFACE

   INTERFACE SetThickness
      MODULE PROCEDURE SetThickness_Aquifer
   END INTERFACE

CONTAINS

   !---------------------------------------------------------------------------
   ! DischargeToVelocity_Aquifer
   !---------------------------------------------------------------------------
   FUNCTION DischargeToVelocity_Aquifer( Aquifer, Head, Q ) RESULT( V )

   ! Declare the arguments.
   TYPE(T_AQUIFER),       INTENT(IN)  :: Aquifer
   REAL(8),               INTENT(IN)  :: Head
   REAL(8), DIMENSION(2), INTENT(IN)  :: Q

   REAL(8), DIMENSION(2)              :: V

   ! Compute the velocity
   IF( Head > Aquifer%Thickness ) THEN
      V = Q / ( Aquifer%Thickness * Aquifer%Porosity )
   ELSE
      V = Q / ( Head * Aquifer%Porosity )
   END IF

   END FUNCTION DischargeToVelocity_Aquifer


   !---------------------------------------------------------------------------
   ! ElevationToPotential_Aquifer
   !---------------------------------------------------------------------------
   FUNCTION ElevationToPotential_Aquifer( Aquifer, Z ) RESULT( Potential )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(IN) :: Aquifer
   REAL(8),         INTENT(IN) :: Z

   REAL(8)                     :: Potential

   ! Compute the potential.
   Potential = HeadToPotential( Aquifer, Z-Aquifer%Base )

   END FUNCTION ElevationToPotential_Aquifer


   !---------------------------------------------------------------------------
   ! ElevationToPotentialStats_Aquifer
   !---------------------------------------------------------------------------
   SUBROUTINE ElevationToPotentialStats_Aquifer( Aquifer, Zev, Zstd, Pev, Pstd )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(IN)  :: Aquifer
   REAL(8),         INTENT(IN)  :: Zev       ! expected value of piezometric elevation
   REAL(8),         INTENT(IN)  :: Zstd      ! standard deviation of piezometric elevation
   REAL(8),         INTENT(OUT) :: Pev       ! expected value of discharge potential
   REAL(8),         INTENT(OUT) :: Pstd      ! standard deviation of discharge potential

   ! Declare local variables.
   REAL(8) :: Head

   ! Compute the expected value and the standard deviation of the discharge
   ! potential from the expected value and the standard deviation of the piezometric
   ! elevation, using a first-order, second moment approximation.
   Head = Zev - Aquifer%Base

   IF( Head < Aquifer%Thickness ) THEN
      Pev = 0.5*Aquifer%Conductivity * ( Head**2 + Zstd**2 )
      Pstd = Aquifer%Conductivity * ABS(Head) * Zstd
   ELSE
      Pev = Aquifer%Conductivity * Aquifer%Thickness * ( Head - 0.5*Aquifer%Thickness )
      Pstd = Aquifer%Conductivity * Aquifer%Thickness * Zstd
   END IF

   END SUBROUTINE ElevationToPotentialStats_Aquifer


   !---------------------------------------------------------------------------
   ! HeadToPotential_Aquifer
   !---------------------------------------------------------------------------
   FUNCTION HeadToPotential_Aquifer( Aquifer, Head ) RESULT( Potential )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(IN) :: Aquifer
   REAL(8),         INTENT(IN) :: Head

   REAL(8)                     :: Potential

   ! Compute the discharge potential from the given head.
   IF( Head < Aquifer%Thickness ) THEN
      Potential = 0.5*Aquifer%Conductivity * Head * Head
   ELSE
      Potential = Aquifer%Conductivity * Aquifer%Thickness * ( Head - 0.5*Aquifer%Thickness )
   END IF

   END FUNCTION HeadToPotential_Aquifer


   !---------------------------------------------------------------------------
   ! PotentialToElevation_Aquifer
   !---------------------------------------------------------------------------
   FUNCTION PotentialToElevation_Aquifer( Aquifer, Potential ) RESULT( Elevation )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(IN) :: Aquifer
   REAL(8),         INTENT(IN) :: Potential

   REAL(8)                     :: Elevation

   ! Compute the elevation.
   Elevation = PotentialToHead( Aquifer, Potential ) + Aquifer%Base

   END FUNCTION PotentialToElevation_Aquifer


   !---------------------------------------------------------------------------
   ! PotentialToHead_Aquifer
   !
   ! Notes:
   !
   ! o   A negative discharge potential is returned as a 0 head.
   !---------------------------------------------------------------------------
   FUNCTION PotentialToHead_Aquifer( Aquifer, Potential ) RESULT( Head )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(IN) :: Aquifer
   REAL(8),         INTENT(IN) :: Potential

   REAL(8)                     :: Head

   ! Declare local variables.
   REAL(8) :: Term

   ! Compute the head from the given discharge potential.
   Term = 0.5 * Aquifer%Conductivity * Aquifer%Thickness**2

   IF( Potential < EPSILON(1.0_8) ) THEN
      Head = -1
   ELSE IF( Potential < Term ) THEN
      Head = SQRT( 2*Potential / Aquifer%Conductivity )
   ELSE
      Head  = ( Potential + Term ) / ( Aquifer%Conductivity * Aquifer%Thickness )
   END IF

   END FUNCTION PotentialToHead_Aquifer


   !---------------------------------------------------------------------------
   ! Reset_Aquifer
   !---------------------------------------------------------------------------
   SUBROUTINE Reset_Aquifer( Aquifer )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(INOUT) :: Aquifer

   ! Reset the parameters the defaults.
   Aquifer%Base         = 0
   Aquifer%Thickness    = INFINITY
   Aquifer%Porosity     = -1
   Aquifer%Conductivity = 1

   END SUBROUTINE Reset_Aquifer


   !---------------------------------------------------------------------------
   ! SetBase_Aquifer
   !---------------------------------------------------------------------------
   SUBROUTINE SetBase_Aquifer( Aquifer, Base )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(INOUT) :: Aquifer
   REAL(8),         INTENT(IN)    :: Base

   ! Set the conductivity.
   Aquifer%Base = Base

   END SUBROUTINE SetBase_Aquifer


   !---------------------------------------------------------------------------
   ! SetConductivity_Aquifer
   !---------------------------------------------------------------------------
   SUBROUTINE SetConductivity_Aquifer( Aquifer, Conductivity )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(INOUT) :: Aquifer
   REAL(8),         INTENT(IN)    :: Conductivity

   ! Set the conductivity.
   Aquifer%Conductivity = Conductivity

   END SUBROUTINE SetConductivity_Aquifer


   !---------------------------------------------------------------------------
   ! SetPorosity_Aquifer
   !---------------------------------------------------------------------------
   SUBROUTINE SetPorosity_Aquifer( Aquifer, Porosity )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(INOUT) :: Aquifer
   REAL(8),         INTENT(IN)    :: Porosity

   ! Set the conductivity.
   Aquifer%Porosity = Porosity

   END SUBROUTINE SetPorosity_Aquifer


   !---------------------------------------------------------------------------
   ! SetThickness_Aquifer
   !---------------------------------------------------------------------------
   SUBROUTINE SetThickness_Aquifer( Aquifer, Thickness )

   ! Declare the arguments.
   TYPE(T_AQUIFER), INTENT(INOUT) :: Aquifer
   REAL(8),         INTENT(IN)    :: Thickness

   ! Set the conductivity.
   Aquifer%Thickness = Thickness

   END SUBROUTINE SetThickness_Aquifer


!==============================================================================
END MODULE AQUIFER_MODULE
!==============================================================================
