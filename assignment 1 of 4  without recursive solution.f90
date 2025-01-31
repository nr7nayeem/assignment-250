program assignment_1of_4_without_recursive_NR7
    implicit none
    integer::a,b,i,temp
    read*,a,b
    if(a>b)then
        do i=1,1000
            if (b== 0)exit
            temp = b
            b = mod(a, b)
            a = temp
        end do
        print*,"the GCD is " ,a
    else
        do i=1,1000
            if(a==0)exit
            temp = a
            a = mod(b, a)
            b = temp
        end do
        print*,"the GCD is " ,b
    end if

end program
!                              stay        with      NR7
