!==============================================================================
! Module REGIONAL_MODULE                                          (20-Jun-2017)
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
MODULE REGIONAL_MODULE
!==============================================================================
   USE CONSTANTS_MODULE
   USE ERROR_MODULE
   USE NUMERIC_MODULE
   IMPLICIT NONE

   !===========================================================================
   ! Type T_REGIONAL
   !===========================================================================
   TYPE T_REGIONAL
   PRIVATE
      REAL(8) :: Xo = 0    ! local origin easting
      REAL(8) :: Yo = 0    ! local origin northing

      REAL(8) :: A  = 0    ! coefficient for (X - Xo)**2
      REAL(8) :: B  = 0    ! coefficient for (Y - Yo)**2
      REAL(8) :: C  = 0    ! coefficient for (X - Xo) (Y - Yo)
      REAL(8) :: D  = 0    ! coefficient for (X - Xo)
      REAL(8) :: E  = 0    ! coefficient for (Y - Yo)
      REAL(8) :: F  = 0    ! constant
   END TYPE T_REGIONAL

   !===========================================================================
   ! INTERFACES
   !===========================================================================
   INTERFACE Potential
      MODULE PROCEDURE Potential_Regional
   END INTERFACE

   INTERFACE Discharge
      MODULE PROCEDURE Discharge_Regional
   END INTERFACE

   INTERFACE GetOrigin
      MODULE PROCEDURE GetOrigin_Regional
   END INTERFACE

   INTERFACE GetParameters
      MODULE PROCEDURE GetParameters_Regional
   END INTERFACE

   INTERFACE Reset
      MODULE PROCEDURE Reset_Regional
   END INTERFACE

   INTERFACE SetOrigin
      MODULE PROCEDURE SetOrigin_Regional
   END INTERFACE

   INTERFACE SetParameters
      MODULE PROCEDURE SetParameters_Regional
   END INTERFACE


CONTAINS
   !---------------------------------------------------------------------------
   ! Potential_Regional
   !---------------------------------------------------------------------------
   FUNCTION Potential_Regional( Regional, X, Y ) RESULT( Phi )

   ! Declare the arguments.
   TYPE(T_REGIONAL), INTENT(IN)  :: Regional ! regional flow of interest
   REAL(8),          INTENT(IN)  :: X        ! X coordinate of interest
   REAL(8),          INTENT(IN)  :: Y        ! Y coordinate of interest

   REAL(8)                       :: Phi      ! resulting discharge potential

   ! Declare the local variables.
   REAL(8) :: DX, DY

   ! Compute the regional component.
   DX = X - Regional%Xo
   DY = Y - Regional%Yo

   Phi = Regional%A*DX*DX + Regional%B*DY*DY + Regional%C*DX*DY + &
      Regional%D*DX + Regional%E*DY + Regional%F

   END FUNCTION Potential_Regional

   !---------------------------------------------------------------------------
   ! Discharge_Regional
   !---------------------------------------------------------------------------
   FUNCTION Discharge_Regional( Regional, X, Y ) RESULT( Q )

   ! Declare the arguments.
   TYPE(T_REGIONAL), INTENT(IN)  :: Regional ! regional flow of interest
   REAL(8),          INTENT(IN)  :: X        ! X coordinate of interest
   REAL(8),          INTENT(IN)  :: Y        ! Y coordinate of interest

   REAL(8), DIMENSION(2)         :: Q        ! resulting discharge

   ! Declare the local variables.
   REAL(8) :: DX, DY

   ! Compute the regional component.
   DX = X - Regional%Xo
   DY = Y - Regional%Yo

   Q(1) = -( 2*Regional%A*DX + Regional%C*DY + Regional%D )
   Q(2) = -( 2*Regional%B*DY + Regional%C*DX + Regional%E )

   END FUNCTION Discharge_Regional


   !---------------------------------------------------------------------------
   ! GetOrigin
   !---------------------------------------------------------------------------
   SUBROUTINE GetOrigin_Regional( Regional, Xo, Yo )

   ! Declare the arguments.
   TYPE(T_REGIONAL), INTENT(INOUT) :: Regional
   REAL(8),          INTENT(OUT)   :: Xo
   REAL(8),          INTENT(OUT)   :: Yo

   ! Set the parameter vector.
   Xo = Regional%Xo
   Yo = Regional%Yo

   END SUBROUTINE GetOrigin_Regional


   !---------------------------------------------------------------------------
   ! GetParameters
   !---------------------------------------------------------------------------
   SUBROUTINE GetParameters_Regional( Regional, P )

   ! Declare the arguments.
   TYPE(T_REGIONAL),      INTENT(INOUT) :: Regional ! regional model to fit
   REAL(8), DIMENSION(6), INTENT(OUT)   :: P        ! parameter vector

   ! Set the parameter vector.
   P(1) = Regional%A
   P(2) = Regional%B
   P(3) = Regional%C
   P(4) = Regional%D
   P(5) = Regional%E
   P(6) = Regional%F

   END SUBROUTINE GetParameters_Regional


   !---------------------------------------------------------------------------
   ! Reset_Regional
   !---------------------------------------------------------------------------
   SUBROUTINE Reset_Regional( Regional )

   ! Declare the arguments.
   TYPE(T_REGIONAL), INTENT(INOUT) :: Regional

   ! Reset the parameters the defaults.
   Regional%Xo = 0
   Regional%Yo = 0

   Regional%A  = 0
   Regional%B  = 0
   Regional%C  = 0
   Regional%D  = 0
   Regional%E  = 0
   Regional%F  = 0

   END SUBROUTINE Reset_Regional


   !---------------------------------------------------------------------------
   ! SetOrigin
   !---------------------------------------------------------------------------
   SUBROUTINE SetOrigin_Regional( Regional, Xo, Yo )

   ! Declare the arguments.
   TYPE(T_REGIONAL), INTENT(INOUT) :: Regional
   REAL(8),          INTENT(IN)    :: Xo
   REAL(8),          INTENT(IN)    :: Yo

   ! Set the parameter vector.
   Regional%Xo = Xo
   Regional%Yo = Yo

   END SUBROUTINE SetOrigin_Regional


   !---------------------------------------------------------------------------
   ! SetParameters
   !---------------------------------------------------------------------------
   SUBROUTINE SetParameters_Regional( Regional, P )

   ! Declare the arguments.
   TYPE(T_REGIONAL),      INTENT(INOUT) :: Regional ! regional model to fit
   REAL(8), DIMENSION(6), INTENT(IN)    :: P        ! parameter vector

   ! Do a simple validation check.
   IF( ANY(ISNAN(P)) .OR. ANY(ISINF(P)) ) THEN
      CALL ExecutionError( INVALID_PARAM_ERROR, 'Regional.f95', 'SetParameters' )
   END IF

   ! Set the parameter vector.
   Regional%A  = P(1)
   Regional%B  = P(2)
   Regional%C  = P(3)
   Regional%D  = P(4)
   Regional%E  = P(5)
   Regional%F  = P(6)

   END SUBROUTINE SetParameters_Regional


!==============================================================================
END MODULE REGIONAL_MODULE
!==============================================================================
