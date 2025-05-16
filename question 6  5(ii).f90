
program simple_gauss
    implicit none
    real :: a(3,4),U(3,4),L(3,4),y(3)
    real :: x1, x2, x3
    real :: factor1,factor2,factor3,factor
    integer :: i, j

    open(3,file="matrix.txt")
    do i=1,3
        read(3,*)(a(i,j),j=1,4)
    end do
!---------------------------------------
    do i=1,3
        do j=1,4
            L(i,j)=a(i,j)
        end do
    end do
!---------------------------------------
!make  U matrix
    factor1 = a(2,1)/a(1,1)
    do j = 1,4
        a(2,j) = a(2,j) - factor1 * a(1,j)
    end do

    factor2 = a(3,1)/a(1,1)
    do j = 1,4
        a(3,j) = a(3,j) - factor2 * a(1,j)
    end do

    factor3 = a(3,2)/a(2,2)
    do j = 2,4
        a(3,j) = a(3,j) - factor3 * a(2,j)
    end do

    do i=1,3
        do j=1,3
            U(i,j)=a(i,j)
        end do
    end do
    ! now make Lower triangular matrix
    do i=1,3
        do j=1,3
            L(i,j)=0
        end do
    end do
    do i=1,3
        L(i,i)=1
    end do
L(2,1)=factor1
L(3,1)=factor2
L(3,2)=factor3
!done L matrix
!---------------------------------------------
! now to solve the L matrix
    ! Eliminate x1 from row 2 and 3
    factor = L(2,1)/L(1,1)
    do j = 1,4
        L(2,j) = L(2,j) - factor * L(1,j)
    end do

    factor = L(3,1)/L(1,1)
    do j = 1,4
        L(3,j) = L(3,j) - factor * L(1,j)
    end do

    factor = L(3,2)/L(2,2)
    do j = 2,4
        L(3,j) = L(3,j) - factor * L(2,j)
    end do
!------------------------------------------
    do i=1,3
        U(i,4)=L(i,4)
    end do
!------------------------------------------
!now solve the U matrix using back substituion
    x3 = U(3,4)/U(3,3)
    x2 = (U(2,4) - U(2,3)*x3)/U(2,2)
    x1 = (U(1,4) - U(1,2)*x2 - U(1,3)*x3)/U(1,1)
    ! Output
    print *, "Solution:"
    print *, "x1 = ", x1
    print *, "x2 = ", x2
    print *, "x3 = ", x3
end program simple_gauss
