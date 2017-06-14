!==============================================================================
! Module CONSTANTS_MODULE                                         (14-Jun-2017)
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
