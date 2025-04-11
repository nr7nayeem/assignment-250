program combination_permutation
    implicit none
    integer :: n, r
    integer :: resultP, resultC

    ! Calculate and print permutations (10P6)
    print *, '10P6 = ', factorial(10) / factorial(10 - 6)

    ! Calculate and print combination (10C4)
    print *, '10C4 = ', factorial(10) / (factorial(4) * factorial(10 - 4))

    ! Calculate and print combination (10C6)
    print *, '10C6 = ', factorial(10) / (factorial(6) * factorial(10 - 6))

contains

    ! Recursive Factorial Function
    recursive function factorial(x) result(fact)
        integer, intent(in) :: x
        integer :: fact

        if (x == 0 .or. x == 1) then
            fact = 1
        else
            fact = x * factorial(x - 1)
        end if
    end function factorial
end program combination_permutation
