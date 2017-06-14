!==============================================================================
! TEST_NUMERIC
!==============================================================================

!==============================================================================
MODULE TEST_NUMERIC_MODULE
!==============================================================================
USE CONSTANTS_MODULE
USE NUMERIC_MODULE
IMPLICIT NONE

!---------------------------------------------------------------------------
! Visibility
!---------------------------------------------------------------------------
PUBLIC test_Numeric


!==============================================================================
! Module procedures
!==============================================================================
CONTAINS

!------------------------------------------------------------------------------
! test_Numeric
!------------------------------------------------------------------------------
SUBROUTINE test_Numeric( nSuccess, nFail )
   ! Declare the arguments.
   INTEGER, INTENT(OUT) :: nSuccess
   INTEGER, INTENT(OUT) :: nFail

   ! Declare local variables.
   LOGICAL :: Flag

   ! Initialize.
   nSuccess = 0
   nFail    = 0

   CALL test_CholeskyDecomposition( Flag )
   IF( Flag ) THEN
      nSuccess = nSuccess + 1
   ELSE
      nFail = nFail + 1
   END IF

   CALL test_CholeskySolve( Flag )
   IF( Flag ) THEN
      nSuccess = nSuccess + 1
   ELSE
      nFail = nFail + 1
   END IF

   CALL test_CholeskyInverse( Flag )
   IF( Flag ) THEN
      nSuccess = nSuccess + 1
   ELSE
      nFail = nFail + 1
   END IF


END


!------------------------------------------------------------------------------
! test_CholeskyDecomposition
!------------------------------------------------------------------------------
SUBROUTINE test_CholeskyDecomposition( Flag )
   ! Declare the arguments.
   LOGICAL, INTENT(OUT) :: Flag

   REAL(8), DIMENSION(6,6) :: C, A, L

   C = RESHAPE((/ 1,1,1,1,1,1, 0,2,2,2,2,2, 0,0,3,3,3,3, 0,0,0,4,4,4, 0,0,0,0,5,5, 0,0,0,0,0,6 /), SHAPE(C))
   A = MATMUL( C, TRANSPOSE(C) )

   CALL CholeskyDecomposition(6, A, L)

   Flag = ALL( ABS(L-C) .LE. 1e-12 )

   IF( .NOT. Flag ) THEN
      CALL Dump(6,C-L,6,6)
   END IF

END SUBROUTINE test_CholeskyDecomposition


!------------------------------------------------------------------------------
! test_CholeskySolve
!------------------------------------------------------------------------------
SUBROUTINE test_CholeskySolve( Flag )
   ! Declare the arguments.
   LOGICAL, INTENT(OUT) :: Flag

   REAL(8), DIMENSION(6,6) :: C, A, L
   REAL(8), DIMENSION(6)   :: b, x, y

   C = RESHAPE((/ 1,1,1,1,1,1, 0,2,2,2,2,2, 0,0,3,3,3,3, 0,0,0,4,4,4, 0,0,0,0,5,5, 0,0,0,0,0,6 /), SHAPE(C))
   A = MATMUL( C, TRANSPOSE(C) )
   b = RESHAPE((/ 21, 81, 171, 267, 342, 378 /), SHAPE(b))
   y = RESHAPE((/ 6, 5, 4, 3, 2, 1 /), SHAPE(y))

   CALL CholeskyDecomposition(6, A, L)
   CALL CholeskySolve(6, L, b, x)

   Flag = ALL( ABS(x-y) .LT. 1e-12 )

   IF( .NOT. Flag ) THEN
      CALL Dump(6,x-y,6,1)
   END IF
END SUBROUTINE test_CholeskySolve


!------------------------------------------------------------------------------
! test_CholeskyInverse
!------------------------------------------------------------------------------
SUBROUTINE test_CholeskyInverse( Flag )
   ! Declare the arguments.
   LOGICAL, INTENT(OUT) :: Flag

   REAL(8), DIMENSION(6,6) :: C, A, Ainv, L, B, Eye

   C = RESHAPE((/ 1,1,1,1,1,1, 0,2,2,2,2,2, 0,0,3,3,3,3, 0,0,0,4,4,4, 0,0,0,0,5,5, 0,0,0,0,0,6 /), SHAPE(C))
   A = MATMUL( C, TRANSPOSE(C) )
   Eye = RESHAPE((/ 1,0,0,0,0,0, 0,1,0,0,0,0, 0,0,1,0,0,0, 0,0,0,1,0,0, 0,0,0,0,1,0, 0,0,0,0,0,1 /), SHAPE(Eye))

   CALL CholeskyDecomposition(6, A, L)

   CALL CholeskyInverse(6, L, Ainv)
   B = MATMUL(A,Ainv)
   Flag = ALL( ABS(B - Eye) .LE. 1e-12 )

   IF( .NOT. Flag ) THEN
      CALL Dump(6,B,6,6)
   END IF

END SUBROUTINE test_CholeskyInverse


!==============================================================================
END MODULE TEST_NUMERIC_MODULE
!==============================================================================
