!==============================================================================
! Module UTILITIES_MODULE                                         (14-Jun-2017)
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
MODULE UTILITIES_MODULE
!==============================================================================
   USE CONSTANTS_MODULE
   IMPLICIT NONE

   !---------------------------------------------------------------------------
   ! Module parameters
   !---------------------------------------------------------------------------
   CHARACTER(*), PARAMETER :: Version = '(14-Jun-2017 [BETA])'
   
   !---------------------------------------------------------------------------
   ! Module variables
   !---------------------------------------------------------------------------
   REAL(8), SAVE :: CPU_Start, CPU_Stop
   
CONTAINS

   !---------------------------------------------------------------------------
   ! Initiate
   !---------------------------------------------------------------------------
   SUBROUTINE Initiate
      ! Initialize the CPU clock.
      CALL CPU_TIME( CPU_Start )
      
   END SUBROUTINE Initiate

   !---------------------------------------------------------------------------
   ! Terminate
   !---------------------------------------------------------------------------
   SUBROUTINE Terminate( ErrNo )
      ! Declare the arguments.
      INTEGER,      INTENT(IN) :: ErrNo
      
      ! Report the cpu statistics.
      CALL CPU_TIME( CPU_Stop )

      WRITE(LUNIT,*)
      WRITE(LUNIT,'(A15,F10.2,A8)') ' Elapsed time: ', CPU_Stop - CPU_Start, ' seconds'
      WRITE(LUNIT,*)
      
      WRITE(SUNIT,*)
      WRITE(SUNIT,'(A15,F10.2,A8)') ' Elapsed time: ', CPU_Stop - CPU_Start, ' seconds'
      WRITE(SUNIT,*)

      ! Write out a terminating message.
      IF( ErrNo .LE. 0 ) THEN
         WRITE(LUNIT,*) 'Successful completion.'
         WRITE(SUNIT,*) 'Successful completion.'      
      ELSE
         WRITE(LUNIT,*) 'Terminated without completion.'
         WRITE(SUNIT,*) 'Terminated without completion.'      
      END IF 
      WRITE(SUNIT,*)
      WRITE(LUNIT,*)

      CALL TimeStamp( LUNIT )
      
      ! Close the opened files, and end execution.
      CLOSE( IUNIT )
      CLOSE( LUNIT )
      STOP
      
   END SUBROUTINE Terminate

   
   !---------------------------------------------------------------------------
   ! TimeStamp
   !---------------------------------------------------------------------------
   SUBROUTINE TimeStamp( OUNIT )
      IMPLICIT NONE

      ! Declare the arguments.
      INTEGER :: OUNIT  ! output unit number

      ! Declare local variables.
      CHARACTER(24) :: DateAndTime

      ! Write out the time stamp.
      CALL FDATE( DateAndTime )
      WRITE(OUNIT,*) DateAndTime

   END SUBROUTINE TimeStamp
   

   !---------------------------------------------------------------------------
   ! ToUpper
   !---------------------------------------------------------------------------
   SUBROUTINE ToUpper( String )
      IMPLICIT NONE

      ! Declare arguments.
      CHARACTER(*), INTENT(INOUT) :: String

      ! Declare local variables.
      INTEGER :: i

      ! Shift all alphbetic characters to upper case.
      DO i = 1, LEN_TRIM( String )
         SELECT CASE( String(i:i) )
         CASE('a':'z')
            String(i:i) = ACHAR( IACHAR( String(i:i) ) - IACHAR('a') + IACHAR('A') )
         END SELECT
      END DO

   END SUBROUTINE ToUpper
   

   !---------------------------------------------------------------------------
   ! WriteBanner
   !---------------------------------------------------------------------------
   SUBROUTINE WriteBanner( OUNIT )
      IMPLICIT NONE

      ! Declare the arguments.
      INTEGER :: OUNIT  ! output unit number

      ! Write out the banner.
      WRITE(OUNIT,*) '==========================================='
      WRITE(OUNIT,*) ' ONEKA ', Version
      WRITE(OUNIT,*)
      WRITE(OUNIT,*) ' R. Barnes, University of Minnesota '
      WRITE(OUNIT,*) ' R. Soule,  Minnesota Department of Health '
      WRITE(OUNIT,*) '==========================================='
      WRITE(OUNIT,*)

      CALL TimeStamp( OUNIT )
      WRITE(OUNIT,*) 

   END SUBROUTINE WriteBanner


!==============================================================================
END MODULE UTILITIES_MODULE
!==============================================================================   