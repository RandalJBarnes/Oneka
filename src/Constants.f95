!==============================================================================
! Module CONSTANTS_MODULE                                         (22-Jun-2017)
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
MODULE CONSTANTS_MODULE
!==============================================================================
   USE,INTRINSIC :: IEEE_ARITHMETIC
   IMPLICIT NONE

   !===========================================================================
   ! Micellaneous numerical constants
   !===========================================================================
   REAL(8), PARAMETER :: ONE_PI               = 3.141592653589793238462643_8
   REAL(8), PARAMETER :: TWO_PI               = 6.283185307179586476925287_8
   REAL(8), PARAMETER :: FOUR_PI              = 12.56637061435917295385057_8
   REAL(8), PARAMETER :: HALF_PI              = 1.570796326794896619231321_8
   REAL(8), PARAMETER :: SQRT_PI              = 1.772453850905516027298167_8
   REAL(8), PARAMETER :: ONE_OVER_PI          = 0.318309886183790671537767_8
   REAL(8), PARAMETER :: ONE_OVER_SQRT_PI     = 0.564189583547756286948080_8
   REAL(8), PARAMETER :: TWO_OVER_SQRT_PI     = 1.128379167095512573996160_8
   REAL(8), PARAMETER :: ONE_OVER_SQRT_TWO_PI = 0.398942280401432677939946_8
   REAL(8), PARAMETER :: EULER_GAMMA          = 0.577215664901532860606512_8

   REAL(8), PARAMETER :: DEG_TO_RAD           = ONE_PI/180.0_8
   REAL(8), PARAMETER :: RAD_TO_DEG           = 180.0_8/ONE_PI

   REAL(8), PARAMETER :: EPS                  = EPSILON(1.0_8)
   REAL(8), PARAMETER :: INFINITY             = HUGE(1.0_8)

   COMPLEX(8), PARAMETER :: CMPLX_I           = DCMPLX(0.0_8, 1.0_8)
   COMPLEX(8), PARAMETER :: TWO_PI_I          = DCMPLX(0.0_8, TWO_PI)

   !===========================================================================
   ! I/O Unit numbers
   !===========================================================================
   INTEGER, PARAMETER :: IUNIT            = 22    ! unit number for input file
   INTEGER, PARAMETER :: LUNIT            = 23    ! unit number for report file
   INTEGER, PARAMETER :: SUNIT            =  6    ! unit number for user's screen

   !===========================================================================
   ! Status flags
   !===========================================================================
   INTEGER, PARAMETER :: SUCCESS          =  1
   INTEGER, PARAMETER :: DRY_AQUIFER      = -1
   INTEGER, PARAMETER :: UNKNOWN_FAILURE  = -2
   INTEGER, PARAMETER :: DID_NOT_CONVERGE = -3


!==============================================================================
END MODULE CONSTANTS_MODULE
!==============================================================================
