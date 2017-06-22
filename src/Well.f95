!==============================================================================
! Module WELL_MODULE                                              (22-Jun-2017)
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
MODULE WELL_MODULE
!==============================================================================
   USE CONSTANTS_MODULE
   IMPLICIT NONE

   !===========================================================================
   ! Type T_WELL
   !===========================================================================
   TYPE T_WELL
      REAL(8) :: X = 0  ! X coordinate
      REAL(8) :: Y = 0  ! Y coordinate
      REAL(8) :: R = 0  ! radius
      REAL(8) :: Q = 0  ! discharge
   END TYPE T_WELL

   !===========================================================================
   ! INTERFACES
   !===========================================================================
   INTERFACE Potential
      MODULE PROCEDURE Potential_Well
   END INTERFACE

   INTERFACE Discharge
      MODULE PROCEDURE Discharge_Well
   END INTERFACE


CONTAINS
   !---------------------------------------------------------------------------
   ! Potential_Well
   !---------------------------------------------------------------------------
   FUNCTION Potential_Well( Well, X, Y ) Result( Phi )

   ! Declare the arguments.
   TYPE(T_WELL), INTENT(IN) :: Well    ! well of interest
   REAL(8),      INTENT(IN) :: X       ! X coordinate of interest
   REAL(8),      INTENT(IN) :: Y       ! Y coordinate of interest

   REAL(8)                  :: Phi     ! resulting discharge potential

   ! Declare the local variables.
   REAL(8) :: R

   ! Compute the discharge potential for the well.
   R = HYPOT(X - Well%X, Y - Well%Y)

   IF( R .LT. Well%R ) THEN
      Phi = Well%Q/TWO_PI * LOG(Well%R)
   ELSE
      Phi = Well%Q/TWO_PI * LOG(R)
   END IF

   END FUNCTION Potential_Well


   !---------------------------------------------------------------------------
   ! Discharge_Well
   !---------------------------------------------------------------------------
   FUNCTION Discharge_Well( Well, X, Y ) RESULT( Q )

   ! Declare the arguments.
   TYPE(T_WELL), INTENT(IN) :: Well    ! well of interest
   REAL(8),      INTENT(IN) :: X       ! X coordinate of interest
   REAL(8),      INTENT(IN) :: Y       ! Y coordinate of interest

   REAL(8), DIMENSION(2)    :: Q       ! resulting discharge vector

   ! Declare the local variables.
   REAL(8) :: DX, DY, R2

   ! Compute the discharge for the well.
   DX = X - Well%X
   DY = Y - Well%Y
   R2 = DX*DX + DY*DY

   IF( R2 .LT. Well%R*Well%R ) THEN
      Q = 0
   ELSE
      Q(1) = -Well%Q/TWO_PI * DX/R2
      Q(2) = -Well%Q/TWO_PI * DY/R2
   END IF

   END FUNCTION Discharge_Well


!==============================================================================
END MODULE WELL_MODULE
!==============================================================================
