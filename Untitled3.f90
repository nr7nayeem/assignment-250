PROGRAM FixedPointIteration
  IMPLICIT NONE

  REAL :: x, x_new, tol
  INTEGER :: max_iter, iter

  ! Initial guess and parameters
  x= 2.0         ! Initial guess
  tol = 1.0E-6    ! Tolerance for stopping criteria
  max_iter = 100  ! Maximum number of iterations

  PRINT *, "Fixed Point Iteration for x^4 - x - 10 = 0"
  PRINT *, "Iter   x            g(x)         |x_new - x|"

  DO iter = 1, max_iter
    x_new= (x + 10.0)**0.25   ! g(x) = (x + 10)^(1/4)

    PRINT "(I3, 2X, F10.6, 2X, F10.6, 2X, F10.6)", iter, x, x_new, ABS(x_new - x)

    ! Check if convergence criteria is met
    IF (ABS(x_new - x) < tol) THEN
      PRINT *, "Converged to root:", x_new
      PRINT *, "Number of iterations:", iter
      EXIT
    END IF

    x = x_new  ! Update x for the next iteration
  END DO

  IF (iter == max_iter) THEN
    PRINT *, "Did not converge within the maximum number of iterations."
  END IF

END PROGRAM FixedPointIteration


