program mersenne_primes
    implicit none
    integer, parameter :: ik = selected_int_kind(18)
    integer(ik) :: n, m
    logical :: is_prime

    print *, "Mersenne Number Classification (M_n = 2^n - 1):"
    print *, "----------------------------------------------"

    do n = 0, 69
        m = 2_ik**n - 1_ik
        if (isMersennePrime(m)) then
            print *, "M(", n, ") =", m, " --> Prime"
        else
            print *, "M(", n, ") =", m, " --> Not Prime"
        end if
    end do

contains

    logical function isMersennePrime(x)
        integer(ik), intent(in) :: x
        integer(ik) :: i
        real(8) :: root

        if (x <= 1) then
            isMersennePrime = .false.
            return
        end if

        if (mod(x, 2) == 0) then
            isMersennePrime = .false.
            return
        end if

        root = sqrt(real(x, kind=8))
        do i = 3, int(root), 2
            if (mod(x, i) == 0) then
                isMersennePrime = .false.
                return
            end if
        end do

        isMersennePrime = .true.
    end function isMersennePrime

