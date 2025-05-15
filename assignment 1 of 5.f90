program assignment_1
    implicit none
    real(8) :: M
    integer :: n
    logical :: is_prime

    print*, "******** The 1st 70 Mersenne numbers are below ********"
    write(*, "(A10, 10x, A15,7x,A11)") "Number", "Mersenne Numbers","check prime"
     write(*,*) "-------------------------------------------------------"

    do n = 0, 70
        M = 2.0**n - 1.0
        is_prime = check_prime(M)
        write(*,*) "-------------------------------------------------------"

        ! Print Mersenne numbers and check if prime
        if (is_prime) then
            write(*, "(I7, 15x, Es10.3, 10x, A)") n, M, "Prime"
        else
            write(*, "(I7, 15x, Es10.3, 10x, A)") n, M, "Not Prime"
        end if
    end do

contains

    logical function check_prime(num)
        real(8), intent(in) :: num
        integer :: i
        check_prime = .true.

        if (num <= 1) then
            check_prime = .false.
            return
        end if

        do i = 2, int(sqrt(num))
            if (mod(num, real(i)) == 0) then
                check_prime = .false.
                return
            end if
        end do
    end function check_prime

end program

