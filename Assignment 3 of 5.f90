program fibonacci_series
    implicit none
    integer :: n, i

    print *, 'First 20 Fibonacci Numbers using Recursive Function:'
    do i = 1, 20
        print *, 'F(', i, ') = ', fib_recursive(i)
    end do

    print *, '----------------------------------------'

    print *, 'First 20 Fibonacci Numbers using Non-Recursive Function:'
    do i = 1, 20
        print *, 'F(', i, ') = ', fib_non_recursive(i)
    end do

contains

    ! Recursive Fibonacci Function
    recursive function fib_recursive(n) result(fib)
        integer, intent(in) :: n
        integer :: fib
        if (n == 1) then
            fib = 0
        else if (n == 2) then
            fib = 1
        else
            fib = fib_recursive(n-1) + fib_recursive(n-2)
        end if
    end function fib_recursive

    ! Non-Recursive Fibonacci Function (Loop Based)
    function fib_non_recursive(n) result(fib)
        integer, intent(in) :: n
        integer :: fib, i
        integer :: a, b, temp

        a = 0
        b = 1

        if (n == 1) then
            fib = a
        else if (n == 2) then
            fib = b
        else
            do i = 3, n
                temp = a + b
                a = b
                b = temp
            end do
            fib = b
        end if
    end function fib_non_recursive

end program fibonacci_series

