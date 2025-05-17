program assignment_4ii_simpsons_13_NR7
    implicit none
    real:: a, b, h, area, s1, s2, x
    integer:: i, n

    n = 30
    a = 0.0
    b = 1.0
    h = (b - a) / real(n)
    s1 = 0.0
    s2 = 0.0

    ! Sum of f(x_i) where i is odd (1,3,5,...,n-1)
    do i = 1, n - 1, 2
        x = a + h * real(i)
        s1 = s1 + f(x)
    end do

    ! Sum of f(x_i) where i is even (2,4,6,...,n-2)
    do i = 2, n - 2, 2
        x = a + h * real(i)
        s2 = s2 + f(x)
    end do

    area = (h / 3.0) * (f(a) + 4.0 * s1 + 2.0 * s2 + f(b))
    print *, 'Approximate area = ', area

contains

    real function f(x)
        real, intent(in) :: x
        f = 1.0 / (1.0 + x**2)
    end function

end program

! stay with NR7
