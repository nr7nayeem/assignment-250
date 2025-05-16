program simple_gauss
    implicit none
    real :: a(3,4)
    real :: x1, x2, x3
    real :: factor
    integer :: i, j

    ! Input augmented matrix manually
    open(3,file="matrix.txt")
    do i=1,3
        read(3,*)(a(i,j),j=1,4)
    end do

    ! Eliminate x1 from row 2 and 3
    factor = a(2,1)/a(1,1)
    do j = 1,4
        a(2,j) = a(2,j) - factor * a(1,j)
    end do

    factor = a(3,1)/a(1,1)
    do j = 1,4
        a(3,j) = a(3,j) - factor * a(1,j)
    end do

    ! Eliminate x2 from row 3
    factor = a(3,2)/a(2,2)
    do j = 2,4
        a(3,j) = a(3,j) - factor * a(2,j)
    end do

    ! Back-substitution

    x3 = a(3,4)/a(3,3)
    x2 = (a(2,4) - a(2,3)*x3)/a(2,2)
    x1 = (a(1,4) - a(1,2)*x2 - a(1,3)*x3)/a(1,1)

    ! Output
    print *, "Solution:"
    print *, "x1 = ", x1
    print *, "x2 = ", x2
    print *, "x3 = ", x3
end program simple_gauss
