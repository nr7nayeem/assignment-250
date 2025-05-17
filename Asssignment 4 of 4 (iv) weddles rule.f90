program assignment_4iv_weddle_NR7
    implicit none
    real :: a, b, h, area, s1, s2, s3, s4, s5, first, last
    integer :: i, n

    ! Define integration limits
    n = 30  ! Must be a multiple of 6
    a = 0.0
    b = 1.0
    h = (b - a) / real(n)

    ! Check if n is a multiple of 6
    if (mod(n, 6) /= 0) then
        print*, "Error: n must be a multiple of 6!"
        stop
    end if

    ! Initialize sums
    s1 = 0.0  ! for 5*f(x1)
    s2 = 0.0  ! for f(x2)
    s3 = 0.0  ! for 6*f(x3)
    s4 = 0.0  ! for f(x4)
    s5 = 0.0  ! for 5*f(x5)
    area = 0.0

    ! Loop through each segment of 6 intervals (7 points)
    do i = 0, n - 6, 6
        s1 = s1 + f(a + h * real(i + 1))
        s2 = s2 + f(a + h * real(i + 2))
        s3 = s3 + f(a + h * real(i + 3))
        s4 = s4 + f(a + h * real(i + 4))
        s5 = s5 + f(a + h * real(i + 5))
    end do

    ! Add the first and last function values
    first = 0.0
    last = 0.0
    do i = 0, n - 6, 6
        first = first + f(a + h * real(i))
        last = last + f(a + h * real(i + 6))
    end do

    ! Apply Weddle’s Rule formula
    area = (3.0 * h / 10.0) * (first + 5.0 * s1 + s2 + 6.0 * s3 + s4 + 5.0 * s5 + last)

    ! Display results
    print*, 'Weddle’s Rule Integration Result:'
    print*, 'Approximate area =', area

contains

    real function f(x)
        real, intent(in) :: x
        f = 1.0 / (1.0 + x**2)
    end function f

end program assignment_4iv_weddle_NR7


! stay with NR7

