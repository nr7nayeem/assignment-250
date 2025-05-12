program question_1
    implicit none
    real :: toll
    integer :: i, j, iter, max_iter
    real :: a(6,6), b(6), x0(6), x(6), s
    real :: error


    toll = 1.0E-3
    max_iter = 100

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


    do iter = 1, max_iter
        error = 0.0
        print *, "Iteration:", iter
        do i = 1, 6
            s = 0.0
            do j = 1, 6
                if (j /= i) then
                    s = s + a(i, j) * x0(j)
                end if
            end do
            x(i) = (b(i) - s) / a(i, i)
            error = error + abs(x(i) - x0(i))
        end do

        do i = 1, 6
            print *, "x(", i, ") = ", x(i)
        end do
        print *, "---------------------------"


        if (error < toll) exit


        x0 = x
    end do

    ! Final result
    print *, "Final solution after", iter, "iterations:"
    do i = 1, 6
        print *, "x(", i, ") = ", x(i)
    end do

end program

