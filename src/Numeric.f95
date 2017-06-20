!==============================================================================
! NUMERIC                                                         (20-Jun-2017)
!
! Written by:
! 	   Dr. Randal J. Barnes
!     Department of Civil, Environmental, and Geo- Engineering
!     University of Minnesota
!     <barne003@umn.edu>
!==============================================================================

!==============================================================================
MODULE NUMERIC_MODULE
!==============================================================================
USE CONSTANTS_MODULE
USE ERROR_MODULE
USE ISO_FORTRAN_ENV, ONLY: INT64
IMPLICIT NONE
PRIVATE

!---------------------------------------------------------------------------
! Visibility
!---------------------------------------------------------------------------
PUBLIC CholeskyDecomposition
PUBLIC CholeskySolve
PUBLIC CholeskyInverse

PUBLIC MGSLeastSquares
PUBLIC RSPDInv

PUBLIC GaussianCDF
PUBLIC GaussianRN
PUBLIC GaussianRNG
PUBLIC MVNormalRNG
PUBLIC Randomize
PUBLIC ISINF
PUBLIC Dump

!==============================================================================
! Module procedures
!==============================================================================
CONTAINS

!------------------------------------------------------------------------------
! CholeskyDecomposition
!
!     This routine computes Cholesky decomposition of a real, symmertric,
!     positive definite matrix A.
!
! Arguments:
!     N  matrix dimension
!     A  a real, symmetric positive definite matrix.
!
!     L  the Cholesky decomposition of A; that is, the lower triangular
!        matrix L, where L L' = A.
!
! Notes:
!
! o   This routine is based upon Golub and Van Loan, 1983, Algorithm 5.2-1,
!     page 89.
!
! o   If a diagonal element is less than EPSILON the routine terminates.
!
! References:
! o   Golub, G.H., and Van Loan, C.F., 1983, MATRIX COMPUTATIONS, Johns
!     Hopkins University Press, Baltimore, Maryland, 476 pp.
!------------------------------------------------------------------------------
SUBROUTINE CholeskyDecomposition(N, A, L)
   ! Declare the arguments.
   INTEGER,                 INTENT(IN)  :: N
   REAL(8), DIMENSION(N,N), INTENT(IN)  :: A
   REAL(8), DIMENSION(N,N), INTENT(OUT) :: L

   ! Declare the local variables.
   INTEGER :: i, k

   ! Carry out the Cholesky decomposition on matrix "A".
   L = A

   DO k = 1, N
      L(k,k) = L(k,k) - DOT_PRODUCT( L(k,1:k-1), L(k,1:k-1) )
      IF( L(k,k) .LT. EPS ) CALL ExecutionError( A_ISNOTPD_ERROR, 'Numeric.f95', 'CholeskyDecomposition')  ! A is not positive definite.
      L(k,k) = SQRT( L(k,k) )

      DO i = k+1, N
         L(i,k) = ( L(i,k) - DOT_PRODUCT( L(i,1:k-1), L(k,1:k-1) ) ) / L(k,k)
         L(k,i) = 0
      END DO
   END DO
END SUBROUTINE CholeskyDecomposition


!------------------------------------------------------------------------------
! CholeskySolve
!
!     This routine solves the system of linear equations given by A x = b,
!     where A = L * L'. That is, L is the Cholesky factorizion of matrix A.
!
! Arguments:
!     N  matrix dimension
!
!     L  the Cholesky decomposition of A; that is, the (NxN) lower triangular
!        matrix L, where L L' = A.
!
!     b  the (Nx1) right-hand-side matrix.
!
!     x  the (Nx1) solution matrix.
!
! Notes:
! o   The Cholesky decomposition must be carried out before calling this
!     routine.
!
! o   This routine works in the following manner
!
!       L y = b  then  L'x = y
!
! o   This routine is based upon Golub and Van Loan, 1983, Algorithms 4.1-1
!     and 4.1-2, page 53.
!
! References:
! o   Golub, G.H., and Van Loan, C.F., 1983, MATRIX COMPUTATIONS, Johns
!     Hopkins University Press, Baltimore, Maryland, 476 pp.
!------------------------------------------------------------------------------
SUBROUTINE CholeskySolve(N, L, b, x)
   ! Declare the arguments.
   INTEGER,                 INTENT(IN)  :: N
   REAL(8), DIMENSION(N,N), INTENT(IN)  :: L
   REAL(8), DIMENSION(N),   INTENT(IN)  :: b
   REAL(8), DIMENSION(N),   INTENT(OUT) :: x

   ! Declare the local variables.
   INTEGER :: i, j

   ! Initialize.
   x = b

   ! Solve L y = b using forward elimination.
   DO i = 1, N
      DO j = 1, i-1
         x(i) = x(i) - L(i,j)*x(j)
      END DO
      x(i) = x(i) / L(i,i)
   END DO

   ! Solve L' x = y using back substitution.
   DO i = N, 1, -1
      DO j = i+1, N
         x(i) = x(i) - L(j,i)*x(j)
      END DO
      x(i) = x(i) / L(i,i)
   END DO
END SUBROUTINE CholeskySolve


!------------------------------------------------------------------------------
! CholeskyInverse
!
!     Compute the inverse of a real, symmetric, positive definite matrix A,
!     where A = L * L'.
!
! Notes:
!
! o   The inversion in place for an upper triangular matrix is based upon
!     the psudo-code given in Stewart (1998, p. 179).
!
! o   The matrices A and Ainv may be the same space in memory.
!
! References:
! o   Stewart, G., 1998, "Matrix Algorithms - Volume I: Basic Decompositions",
!     SIAM, Philadelphia, 458pp., ISBN 0-89871-414-1.
!------------------------------------------------------------------------------
SUBROUTINE CholeskyInverse(N, L, Ainv)

   ! Declare the arguments.
   INTEGER,                 INTENT(IN)  :: N
   REAL(8), DIMENSION(N,N), INTENT(IN)  :: L
   REAL(8), DIMENSION(N,N), INTENT(OUT) :: Ainv

   ! Declare the local variables.
   INTEGER :: i, k
   REAL(8), DIMENSION(N,N) :: U

   ! Initialize.
   U = TRANSPOSE(L)

   ! Invert U in place.
   DO k = 1, N
      U(k,k) = 1.0_8/U(k,k)

      DO i = 1, k-1
         U(i,k) = -U(k,k) * DOT_PRODUCT( U(i,i:k-1), U(i:k-1, k) )
      END DO
   END DO

   ! A = L L' = L U --> Ainv = Uinv Linv = Uinv (Uinv)'
   Ainv = MATMUL(U, TRANSPOSE(U))

END SUBROUTINE CholeskyInverse


!------------------------------------------------------------------------------
! MGSLeastSquares
!
!     Compute the least-squares solution to the overdetermined system of
!     linear equations:
!
!       A x = b
!
!     using a modified Gram-Schmidt orthogonalization algorithm.
!
! Arguments:
!     A        (m x n) coefficient matrix.               <OVER_WRITTEN>
!     b        (m x 1) right-hand-side column vector.    <OVER_WRITTEN>
!     x        (n x 1) solution column vector.
!
! Notes:
!
! o   WARNING: This is a DESTRUCTIVE routine.  A and b are both modified
!     during the execution of this routine.
!
! o   Matrix A must have at least as many rows as columns (i.e. m >= n),
!     and it must have full column rank:  i.e. rank(A) = n.
!
! o   This routine uses a modified Gram-Schmidt algorithm.  See Golub
!     and Van Loan (1996, Algorithm 5.2.5, p.231).  The Matrix A is
!     rewritten using a "thin" QR factorization:
!
!        A = Q R
!
!     where R (n x n) is upper triangular, and Q (m x n) with orthogonal
!     columns
!
!        Q'Q = I
!
!     Thus, the solution to the least squares problem can be given by
!     computing a simple back-substitution solution to
!
!       Rx = Q'b
!
!     See Golub and Van Loan (1996, Algorithm 3.1.2, p. 89).
!
! o   However, following the recommendation of Golub and Van Loan (1996),
!     Section 5.3.5, p. 233, the error properties of the solution can be
!     improved significantly by computing the factorization of the
!     augmented matrix
!
!        ( A b )  =  (Q q) ( R  z )
!                          ( 0  p )
!
!                 =  ( QR  Qz+qp )
!
!     thus
!
!        b  =  Qz + qp
!
!     and
!
!        Q'b  =  Q'Qz + Q'qp
!             =  z
!
!     we back-substitute on
!
!        R x  =  z
!
! o   In this implementation, Q overwrites A, q overwrites b, and
!     z is stored in x (which is then overwritten by the solution
!     during back-substitution).
!
! References:
!
! o   Golub, G. H., and C. F. Van Loan, 1996, MATRIX COMPUTATIONS (3rd
!     Edition), Johns Hopkins University Press, Baltimore Maryland,
!     ISBN 0-8018-5414-8.
!------------------------------------------------------------------------------
SUBROUTINE MGSLeastSquares( m, n, A, x, b )

   ! Declare the arguments.
   INTEGER,                 INTENT(IN)    :: m     ! # of equations
   INTEGER,                 INTENT(IN)    :: n     ! # of unknowns
   REAL(8), DIMENSION(m,n), INTENT(INOUT) :: A     ! coefficient matrix
   REAL(8), DIMENSION(n),   INTENT(OUT)   :: x     ! solution vector
   REAL(8), DIMENSION(m),   INTENT(INOUT) :: b     ! rhs vector

   ! Declare the local variables and parameters.
   INTEGER :: j, k

   ! Allocate the necessary local memory for the upper-triangular matrix R.
   ! The augmenting column "z" will be stored in "x".
   REAL(8), DIMENSION(n,n) :: R

   ! Do a simple validation on the input.
   IF( ANY(ISNAN(A)) .OR. ANY(ISINF(A)) ) CALL ExecutionError( A_ISNAN_ERROR, 'Numeric.f95', 'MGSLeastSquares' )
   IF( ANY(ISNAN(b)) .OR. ANY(ISINF(b)) ) CALL ExecutionError( B_ISNAN_ERROR, 'Numeric.f95', 'MGSLeastSquares' )

   ! Carry out the modified Gram-Schmidt orthogonalization.
   DO k = 1, n
      R(k,k) = SQRT( DOT_PRODUCT( A(1:m,k), A(1:m,k) ) )
      IF( R(k,k) .LT. EPS ) CALL ExecutionError( A_ISSINGULAR_ERROR, 'Numeric.f95', 'MGSLeastSquares' )  ! Not full column rank.
      A(1:m,k) = A(1:m,k)/R(k,k)

      DO j = k+1, n
         R(k,j) = DOT_PRODUCT( A(1:m,k), A(1:m,j) )
         A(1:m,j) = A(1:m,j) - A(1:m,k)*R(k,j)
      END DO

      ! Apply the orthogonalization to the augmented part of the system.
      x(k) = DOT_PRODUCT( A(1:m,k), b(1:m) )
      b(1:m) = b(1:m) - A(1:m,k)*x(k)
   END DO

   ! Compute x = R~z using back-substitution.  Recall that z is stored in x
   ! at this point.
   x(n) = x(n)/R(n,n)

   DO k = n-1, 1, -1
      x(k) = (x(k) - DOT_PRODUCT( R(k,k+1:n), x(k+1:n) ))/R(k,k)
   END DO

END SUBROUTINE MGSLeastSquares


!------------------------------------------------------------------------------
! RSPDInv
!
!     Compute the inverse of a real, symmetric, positive definite matraix A.
!
! Notes:
!
! o   The inversion in place for an upper triangular matrix is based upon
!     the psudo-code given in Stewart (1998, p. 179).
!
! o   The matrices A and Ainv may be the same space in memory.
!
! References:
!
!     Stewart, G., 1998, "Matrix Algorithms - Volume I: Basic Decompositions",
!     SIAM, Philadelphia, 458pp., ISBN 0-89871-414-1.
!------------------------------------------------------------------------------
SUBROUTINE RSPDInv(N, L, Ainv)

   ! Declare the arguments.
   INTEGER,                 INTENT(IN)  :: N
   REAL(8), DIMENSION(N,N), INTENT(IN)  :: L
   REAL(8), DIMENSION(N,N), INTENT(OUT) :: Ainv

   ! Declare the local variables.
   INTEGER :: i, k
   REAL(8), DIMENSION(N,N) :: U

   ! Compute the Cholesky decomposition.
   ! CALL CholeskyDecomposition(N, A, L)
   U = TRANSPOSE(L)

   ! Invert U in place.
   DO k = 1, N
      U(k,k) = 1.0_8/U(k,k)

      DO i = 1, k-1
         U(i,k) = -U(k,k) * DOT_PRODUCT( U(i,i:k-1), U(i:k-1, k) )
      END DO
   END DO

   ! A = L L' = L U --> Ainv = Uinv Linv = Uinv (Uinv)'
   Ainv = MATMUL(U, TRANSPOSE(U))

END SUBROUTINE RSPDInv

!------------------------------------------------------------------------------
! GaussianCDF
!
!     The Pr(z <= Z) for a Gaussian (standard Normal) distribution.
!
! Notes:
!
! o   This is an implementation of the rational approximation given by
!     Abramowitz & Stegun (26.2.17).
!
! o   The |error| < 7.5 x 10^-8.
!
! o   The tails are squashed at Z = +/- 5;  Pr(z<-5) = 0, Pr(z>+5) = 1.
!
! References:
!
! o   Abramowitz, M., and I. Stegun, 1972, "Handbook of Mathematical
!     Functions", Dover, New York, 1046 pp., ISBN 486-61272-4.
!------------------------------------------------------------------------------
FUNCTION GaussianCDF( Z ) RESULT( CDF )

   ! Declare the arguments.
   REAL(8), INTENT(IN) :: Z
   REAL(8) :: CDF

   ! Declare the local variables.
   REAL(8) :: E, T, P

   ! Deal with the extreme cases.
   IF( Z .LE. -5.0_8 ) THEN
      CDF = 0.0_8
   ELSEIF( Z .GE. 5.0_8 ) THEN
      CDF = 1.0_8
   ELSE
      ! Abramowitz & Stegun (26.2.17)
      E = ONE_OVER_SQRT_TWO_PI * EXP( -0.5_8*Z*Z )
      T = 1.0_8 / ( 1.0_8 + 0.2316419_8*ABS(Z) )
      P = ((((1.330274429_8*T - 1.821255978_8 )*T + 1.781477937_8)*T - 0.356563782_8)*T + 0.319381530_8)*T

      IF( Z .GT. 0.0_8 ) THEN
         CDF = 1.0_8 - E*P
      ELSE
         CDF = E*P
      ENDIF
   ENDIF

END FUNCTION GaussianCDF


!------------------------------------------------------------------------------
! GaussianRN
!
!     Generate a Gaussian (standard Normal) pseudo-random number.
!
! Notes:
!
! o   This code is based upon Flannery et al. (1986, p. 203).
!
! o   This routine computes two Gaussian pseudo-random number simultaneously,
!     using the Box-Mueller transformation.
!
! References:
!
! o   Press, W. B. Flannery, S. Teukolsky, and W. Vetterling, 1986, "Numerical
!     Recipes - The Art of Scientific Computing", Cambridge Univerty Press,
!     Cambridge, 818 pp., ISBN 0-521-30811-9.
!------------------------------------------------------------------------------
FUNCTION GaussianRN() RESULT(Z)

   ! Declare the arguments.
   REAL(8) :: Z

   ! Declare the local variables.
   LOGICAL, SAVE               :: Stored = .FALSE.
   REAL(8), SAVE, DIMENSION(2) :: V
   REAL(8)                     :: R

   ! Create new random variables, only if needed.
   IF( Stored ) THEN
      Z = V(2)
   ELSE
      R = 2
      DO WHILE( R .GE. 1 )
         CALL RANDOM_NUMBER( V )
         V = 2*V - 1
         R = V(1)*V(1) + V(2)*V(2)
      END DO

      V = SQRT(-2*LOG(R)/R) * V
      Z = V(1)
   END IF

END FUNCTION GaussianRN


!------------------------------------------------------------------------------
! GaussianRNG
!
!     Create a vector of independent, Gaussian, pseudo-random numbers.
!------------------------------------------------------------------------------
SUBROUTINE GaussianRNG(nSims, Z)

   ! Declare the arguments.
   INTEGER,               INTENT(IN)  :: nSims
   REAL(8), DIMENSION(:), INTENT(OUT) :: Z

   ! Declare the local variables.
   INTEGER :: n

   ! Generate a vector of independent pseudo-random variables.
   DO n = 1, nSims
      Z(n) = GaussianRN()
   END DO
END SUBROUTINE GaussianRNG


!------------------------------------------------------------------------------
! MVNormalRNG
!
!     Generates a set of pseudorandom vectors from a multivariate normal
!     distribution with mean vector Mu, and variance-covariance matrix Sigma.
!
! Notes:
!
! o   The rows of V are independent random vectors.  The columns of each are
!     are correlated random variables.
!------------------------------------------------------------------------------
SUBROUTINE MVNormalRNG( nSims, nVars, Mu, Sigma, V )

   ! Declare the arguments.
   INTEGER,                          INTENT(IN)  :: nSims
   INTEGER,                          INTENT(IN)  :: nVars
   REAL(8), DIMENSION(nVars),        INTENT(IN)  :: Mu
   REAL(8), DIMENSION(nVars, nVars), INTENT(IN)  :: Sigma

   REAL(8), DIMENSION(nSims, nVars), INTENT(OUT) :: V

   ! Declare the local variables.
   INTEGER :: m
   REAL(8), DIMENSION(nVars,nVars) :: C
   REAL(8), DIMENSION(nVars)       :: Z

   ! Cholesky decomposition of Sigma.
   CALL CholeskyDecomposition(nVars, Sigma, C)

   ! Simulate the vectors one at a time.
   DO m = 1, nSims
      CALL GaussianRNG(nVars, Z)
      V(m,1:nVars) = Mu(1:nVars) + MATMUL(C, Z)
   END DO

END SUBROUTINE MVNormalRNG


!------------------------------------------------------------------------------
! Randomize
!
!   Initialize the random number generator.
!
! Arguments:
!   flag   If .TRUE. the random number generator is initialized with an array
!          of seed values that guarnatee a different set of random numbers
!          from one run to the next.
!
!          If .FALSE. the random number generator is initialized to DEFAULT
!          seeting, so that every run generates the same sequence of random
!          numbers. Thus, this is NOT randomized.
!
! Notes:
! o This routine is based on the comparable routine given as an example in
!   the GFORTRAN manual.
!------------------------------------------------------------------------------
SUBROUTINE Randomize( flag )
   ! Declare the arguments.
   LOGICAL, INTENT(IN)  :: flag

   ! Declare the local variables.
   INTEGER, ALLOCATABLE :: seed(:)
   INTEGER :: i, n, dat(8), pid
   INTEGER(INT64) :: s, t

   ! Query the compiler for the size of the random seed array.
   CALL RANDOM_SEED(SIZE = n)
   ALLOCATE( seed(n) )

   ! Randomize or set to default.
   IF( flag ) THEN
      ! Generate the array of seed values by XOR:ing the current time and pid.
      ! The PID is useful in case one launches multiple instances of the same
      ! program in parallel. Then using this value in a crude LCG generator.
      CALL DATE_AND_TIME( VALUES=dat )
      t = (dat(1) - 2010) * 365_int64 * 24 * 60 * 60 * 1000 &
         + dat(2) * 31_int64 * 24 * 60 * 60 * 1000 &
         + dat(3) * 24_int64 * 60 * 60 * 1000 &
         + dat(5) * 60 * 60 * 1000 &
         + dat(6) * 60 * 1000 &
         + dat(7) * 1000 &
         + dat(8)

      pid = GETPID()
      s = IEOR(t, INT(pid, KIND(t)))

      DO i = 1, n
         s = mod(s * 279470273_INT64, 4294967291_INT64)
         seed(i) = INT( MOD(s, INT(HUGE(0), INT64)), KIND(0) )
      END DO
   ELSE
      DO i = 1, n
         seed(i) = i
      END DO
   END IF

   ! After all that, set the random seed array.
   CALL RANDOM_SEED( PUT=seed )
   DEALLOCATE( seed )
END SUBROUTINE Randomize

!------------------------------------------------------------------------------
! ISFINITE
!------------------------------------------------------------------------------
ELEMENTAL FUNCTION ISINF( A )
   LOGICAL ISINF
   REAL(8), INTENT(IN) :: A

   ISINF = (A .GE. INFINITY)
END FUNCTION ISINF

!------------------------------------------------------------------------------
! Dump
!------------------------------------------------------------------------------
SUBROUTINE Dump(OUNIT, A, M, N)
   ! Declare the arguments.
   INTEGER, INTENT(IN)                 :: OUNIT
   INTEGER, INTENT(IN)                 :: M
   INTEGER, INTENT(IN)                 :: N
   REAL(8), DIMENSION(M,N), INTENT(IN) :: A

   ! Declare the local variables.
   INTEGER :: i, j

   WRITE(*,*)
   DO i = 1, M
      WRITE(OUNIT, '(12(E12.4, 1X))') (A(i,j), j = 1,N)
   END DO
END SUBROUTINE Dump

!==============================================================================
END MODULE NUMERIC_MODULE
!==============================================================================
