program assignment_4iv_weddle_NR7
    implicit none
    real :: a, b, h, area, s1, s2, s3, s4, s5, x
    integer :: i, n

    ! Define integration limits
    n = 30  ! Must be a multiple of 6
    a = 0.
    b = 1.
    h = (b - a) / real(n)

    ! Check if n is a multiple of 6
    if (mod(n, 6) /= 0) then
        print*, "Error: n must be a multiple of 6!"
        stop
    end if

    s1 = 0.
    s2 = 0.
    s3 = 0.
    s4 = 0.
    s5 = 0.

    ! Sum for x1
    do i = 0, (n/6)-1
        x = h + (6. * h * real(i))
        s1 = s1 + f(x)
    end do

    ! Sum for x2
    do i = 0, (n/6)-1
        x = 2. * h + (6. * h * real(i))
        s2 = s2 + f(x)
    end do

    ! Sum for x3
    do i = 0, (n/6)-1
        x = 3. * h + (6. * h * real(i))
        s3 = s3 + f(x)
    end do

    ! Sum for x4
    do i = 0, (n/6)-1
        x = 4. * h + (6. * h * real(i))
        s4 = s4 + f(x)
    end do

    ! Sum for x5
    do i = 0, (n/6)-1
        x = 5. * h + (6. * h * real(i))
        s5 = s5 + f(x)
    end do

    ! Apply Weddle’s Rule formula
    area = (3. * h / 10.) * (f(a) + 5. * s1 + s2 + 6. * s3 + s4 + 5. * s5 + f(b))

    print*, area

contains
    real function f(x)
        real, intent(in) :: x
        f = 1. / (1. + x**2)
    end function f

end program weddle
! stay with NR7

