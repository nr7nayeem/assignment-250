program assignment_4iii_simpsons_38_NR7
    implicit none
    real :: a, b, h, area, s1, s2, x
    integer :: i, n

    ! Integration limits and step size
    n = 30         ! Must be a multiple of 3
    a = 0.0
    b = 1.0
    h = (b - a) / real(n)

    s1 = 0.0   ! For terms with coefficient 3 (i mod 3 ≠ 0)
    s2 = 0.0   ! For terms with coefficient 2 (i mod 3 = 0, excluding i = 0 and n)

    do i = 1, n - 1
        x = a + h * real(i)
        if (mod(i, 3) == 0) then
            s2 = s2 + f(x)
        else
            s1 = s1 + f(x)
        end if
    end do

    area = (3.0 * h / 8.0) * (f(a) + 3.0 * s1 + 2.0 * s2 + f(b))
    print *, 'Approximate area = ', area

contains

    real function f(x)
        real, intent(in) :: x
        f = 1.0 / (1.0 + x**2)
    end function f

end program assignment_4iii_simpsons_38_NR7

! stay with NR7

