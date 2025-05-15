program question_1
    implicit none
    real :: toll, w
    integer :: i, j, iter, max_iter
    real :: a(6,6), b(6), x0(6), x(6), s
    real :: error

    ! Initialize
    toll = 1.0E-3
    max_iter = 100
    w = 1.1  ! Relaxation factor

    a = RESHAPE([ &
        4., -1., -1.,  0., -1.,  0., &
       -2.,  4., -1.,  0., -1.,  0., &
        0., -1.,  4.,  0.,  0., -1., &
       -1.,  0.,  0.,  4., -1.,  0., &
        0., -1.,  0., -1.,  4., -1., &
        0.,  0., -1.,  0., -1.,  4.  &
    ], shape(a))

    b = (/ 0., 5., 0., 6., -2., 6. /)
    x0 = 0.0
    x = 0.0

    ! Start Iteration
    do iter = 1, max_iter
        error = 0.0
        print *, "Iteration:", iter
        do i = 1, 6
            s = 0.0
            do j = 1, 6
                if (j /= i) then
                    s = s + a(i, j) * x(j)  ! Use current x values for SOR
                end if
            end do
            x0(i) = x(i)
            x(i) = (1.0 - w) * x(i) + w * (b(i) - s) / a(i, i)  ! Update using relaxation factor
            error = error + abs(x(i) - x0(i))
        end do

        ! Print current x values with 5 decimal places
        do i = 1, 6
            print *, "x(", i, ") = ", x(i)
        end do
        print *, "---------------------------"

        ! Convergence check
        if (error < toll) exit
    end do

    ! Final solution
    print *, "Final solution after", iter, "iterations:"
    do i = 1, 6
        print *, "x(", i, ") = ", x(i)
    end do

end program

