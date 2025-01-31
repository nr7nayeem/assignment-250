program assignment_1_of_5_NR7
    implicit none
    integer::n,fermat
    logical::isPrime
    do n=0,4
        fermat=(2**(2**n))+1
        isPrime=cheak_prime(fermat)
        print*,n,"the prime number is ",fermat," -> true or false ?     ans is " ,isPrime
    end do
contains
    function cheak_prime(fermat) result(isPrime)
            IMPLICIT NONE
            integer,intent(IN)::fermat
            integer::i

            logical::isPrime
            do i=1,fermat/2
                if(mod(fermat,i)==0)then
                    isPrime=.FALSE.
                end if
                    isPrime=.TRUE.
            end do
    end function
end program
!                              stay        with      NR7
