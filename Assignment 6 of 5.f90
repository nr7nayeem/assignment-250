program combination_permutation
    implicit none
    integer :: n, r
    integer :: resultP, resultC

    ! Calculate 10P6
    n = 10
    r = 6
    resultP = nPr(n, r)
    print *, '10P6 = ', resultP

    ! Calculate 10C4
    n = 10
    r = 4
    resultC = nCr(n, r)
    print *, '10C4 = ', resultC

    ! Calculate 10C6
    n = 10
    r = 6
    resultC = nCr(n, r)
    print *, '10C6 = ', resultC

contains

    ! Recursive Factorial Function
    recursive function factorial(x) result(fact)
        integer, intent(in) :: x
        integer :: fact

        if (x == 0 .or. x == 1) then
            fact = 1
        else
            fact = x * factorial(x-1)
        end if
    end function factorial

    ! Function to calculate nCr
    integer function nCr(n, r)
        integer, intent(in) :: n, r
        nCr = factorial(n) / (factorial(r) * factorial(n - r))
    end function nCr

    ! Function to calculate nPr
    integer function nPr(n, r)
        integer, intent(in) :: n, r
        nPr = factorial(n) / factorial(n - r)
    end function nPr

end program combination_permutation

