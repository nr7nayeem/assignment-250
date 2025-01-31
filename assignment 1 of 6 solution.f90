program assignment_1_of_6_NR7
    implicit none
    integer :: r1, c1, r2, c2,i,j
    real, allocatable :: A(:,:), B(:,:), C(:,:), C_builtin(:,:)
    logical :: is_multiplicable

! Read matrix dimensions from the user
    print *, 'Enter rows and columns for Matrix A (r1 c1):'
    read *, r1, c1
    print *, 'Enter rows and columns for Matrix B (r2 c2):'
    read *, r2, c2

! Check if multiplication is possible
    is_multiplicable = check_multiplication(r1, c1, r2, c2)
    if (.not. is_multiplicable) then
        print *, 'Matrix multiplication is not possible: c1 must equal r2'
        stop
    end if

    allocate(A(r1, c1), B(r2, c2), C(r1, c2), C_builtin(r1, c2))

! Read matrices from files
    open(10, file='matrix_A.txt', status='old')
    read(10, *) A
    close(10)

    open(20, file='matrix_B.txt', status='old')
    read(20, *) B
    close(20)


    call matrix_multiply(A, B, C, r1, c1, c2)
    C_builtin = matmul(A, B)
    print*"compute the matrix multiplication"
    do i=1,2
        print*,(c(i,j),j=1,2)
    end do
    print*,"Verify using built-in matmul function "
    do i=1,2
        print*,(C_builtin(i,j),j=1,2)
    end do

contains

    logical function check_multiplication(r1, c1, r2, c2)
        integer, intent(in) :: r1, c1, r2, c2
        check_multiplication = (c1 == r2)
    end function check_multiplication

    subroutine matrix_multiply(A, B, C, r1, c1, c2)
        integer, intent(in) :: r1, c1, c2
        real, intent(in) :: A(r1, c1), B(c1, c2)
        real, intent(out) :: C(r1, c2)
        integer :: i, j, k
        C = 0.0
        do i = 1, r1
            do j = 1, c2
                do k = 1, c1
                    C(i, j) = C(i, j) + A(i, k) * B(k, j)
                end do
            end do
        end do
    end subroutine matrix_multiply
end program
!                       stay       with     NR7
