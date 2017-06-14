!==============================================================================
! PROGRAM Main
!==============================================================================
PROGRAM Main
   USE TEST_NUMERIC_MODULE

   IMPLICIT NONE
   INTEGER :: nSuccess, nFail
   REAL(8) :: CPU_Start, CPU_Stop  
  
   CALL CPU_TIME( CPU_Start )  
  
  
   CALL test_Numeric( nSuccess, nFail )
   
   
   CALL CPU_TIME( CPU_Stop )
   
   WRITE(*,*)
   WRITE(*,*) 'TEST ONEKA RESULTS'
   WRITE(*,*) '   nSuccess = ', nSuccess
   WRITE(*,*) '   nFail    = ', nFail
   WRITE(*,*)   
   WRITE(*,'(A15,F10.2,A8)') 'Elapsed time: ', CPU_Stop - CPU_Start, ' seconds'
   WRITE(*,*)
   
END PROGRAM MAIN