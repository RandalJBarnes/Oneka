!==============================================================================
! Module GEOHYDROLOGY_MODULE                                      (22-Jun-2017)
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
MODULE GEOHYDROLOGY_MODULE
!==============================================================================

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
   ! INTERFACES
   !===========================================================================

   INTERFACE ComputeGeohydrologyStatistics
      MODULE PROCEDURE ComputeGeohydrologyStatistics
   END INTERFACE

!------------------------------------------------------------------------------
CONTAINS

   !---------------------------------------------------------------------------
   ! ComputeGeohydrologyStatistics
   !
   !  Compute the geohydrology statistics (rechange, magnitude and
   !  direction of the regional discharge at the origin) using first-order
   !  second moment approximations.
   !---------------------------------------------------------------------------
   PURE FUNCTION ComputeGeohydrologyStatistics( Avg, Cov ) RESULT( Geo )
      ! Declare the arguments.
      REAL(8), DIMENSION(6),   INTENT(IN)    :: Avg   ! (6x1) parameter expected value matrix
      REAL(8), DIMENSION(6,6), INTENT(IN)    :: Cov   ! (6x6) covariance matrix

      TYPE(T_GEOHYDROLOGY)                   :: Geo   ! geohydrology statistics

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
   END FUNCTION ComputeGeohydrologyStatistics

!==============================================================================
END MODULE GEOHYDROLOGY_MODULE
!==============================================================================
