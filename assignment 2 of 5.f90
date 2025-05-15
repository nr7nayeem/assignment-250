program pp
    implicit none
    integer, parameter :: k = selected_int_kind(21)
    real :: l
    integer(k) :: i, days
    do i = 1, 64
        l = 5.0 *H(i)/ 86400.0
        days = int(l + 0.9999999, kind=k)
        write(*,"(I4,3x,I20,3x,I20)") i, H(i), days

    end do
    print*," connection relation and solution "
    do i=1, 64
        write(*,"(I4,3x,I20,3x,I20)") i, H(i), 2**i-1
    end do

contains
    recursive function H(x) result(t)
        integer(k) :: t
        integer(k), intent(in) :: x
        if (x == 1) then
            t = 1
        else
            t = 2 * H(x - 1) + 1
        end if
    end function H
end program pp


