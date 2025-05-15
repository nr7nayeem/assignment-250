program fixed_point
    implicit none
    real(8) :: x0, x1, tol
    integer :: max_iter, iter
    real(8) :: g, diff

    ! Define initial guess, tolerance, and maximum iterations
    x0 = 0.5  ! Initial guess
    tol = 1.0d-6 ! Desired tolerance
    max_iter = 100 ! Maximum number of iterations

    ! Fixed point function g(x) = x - f(x) -> example: x = cos(x)
    g = cos(x0)   ! Modify this as needed for the problem

    ! Print table header
    print *, 'Iteration    x0        |x1 - x0|'
    print *, '-----------------------------------'

    ! Start iteration loop
    do iter = 1, max_iter
        x1 = g    ! Perform the iteration step
        diff = abs(x1 - x0)  ! Compute the difference between x1 and x0

        ! Print the iteration table
        print *, iter, x0, diff

        ! Check for convergence
        if (diff < tol) then
            print *, 'Fixed point is: ', x1
            print *, 'Number of iterations: ', iter
            stop
        end if

        x0 = x1    ! Update x0 for the next iteration
    end do

    ! If max iterations are exceeded, print a message
    print *, 'Fixed point method did not converge in the maximum number of iterations.'

end program fixed_point
