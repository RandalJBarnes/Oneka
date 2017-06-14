!==============================================================================
! Module WELL_MODULE                                              (14-Jun-2017)
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
   REAL(8) :: DX, DY, R2

   ! Compute the discharge potential for the well.   
   DX  = X - Well%X 
   DY  = Y - Well%Y
   R2  = DX*DX + DY*DY
   
   IF( R2 .LT. Well%R*Well%R ) THEN
      Phi = Well%Q/FOUR_PI * LOG( Well%R*Well%R )
   ELSE
      Phi = Well%Q/FOUR_PI * LOG( R2 )
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
