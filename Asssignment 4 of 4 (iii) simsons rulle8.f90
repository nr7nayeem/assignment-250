program assignment_4iii_simpsons_38_NR7
    implicit none
    real :: a, b, h, area, s1, s2, s3, x
    integer :: i, n

    ! Define integration limits
    n = 30
    a = 0.
    b = 1.
    h = (b - a) / real(n)
    s1 = 0.
    s2 = 0.
    s3 = 0.

    !1st Sum
    do i = 0, (n/3)-1
        x = h + (3. * h * real(i))
        s1 = s1 + f(x)
    end do

    ! second Sum
    do i = 0, (n/3)-1
        x = 2. * h + (3. * h * real(i))
        s2 = s2 + f(x)
    end do

    !last  Sum
    do i = 0, (n/3)-1
        x = 3. * h + (3. * h * real(i))
        s3 = s3 + f(x)
    end do

    ! Apply Simpson's 3/8 Rule formula
    area = (3. * h / 8.) * (f(a) + 3. * s1 + 3. * s2 + 2. * s3 + f(b))
    print*, area

contains
    real function f(x)
        real, intent(in) :: x
        f = 1. / (1. + x**2)
    end function f
end program simpsons_38
! stay with NR7

