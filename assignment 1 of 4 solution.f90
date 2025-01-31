program assignment_1of_4_recursive_NR7
    implicit none
    integer :: a, b, result
    print *, "Enter two positive integers:"
    read *, a, b
    result = gcd_recursive_func(a, b)
    print *, "The GCD of", a, "and", b, "is:", result
contains
    recursive function gcd_recursive_func(a, b) result(gcd)
        integer, intent(in) :: a, b
        integer :: gcd
        if (b == 0) then
            gcd = a
        else
        gcd = gcd_recursive_func(b, mod(a, b))
        end if
    end function gcd_recursive_func
end program gcd_recursive
!                              stay        with      NR7
