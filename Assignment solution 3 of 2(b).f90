!                       NR7
program lagrange_polynomial!                       NR7
    implicit none!                       NR7
    real :: x(10),y(10),p,l(10),f!                       NR7
    integer :: i ,n ,j!                       NR7
    n=5!                       NR7
    print*," enter the value of x (3) "!                       NR7
    read *, p!                       NR7
    open (2,file='input2.2.txt')!                       NR7
    do i = 1, n!                       NR7
        read(2,*) x(i),y(i)!                       NR7
    end do!                       NR7
    close(2)!                       NR7
    l(1)=1.0!                       NR7
    do i=1,n!                       NR7
        if (i/=1)then!                       NR7
            l(1)=l(1)*((p-x(i))/(x(1)-x(i)))!                       NR7
        end if!                       NR7
    end do!                       NR7
    l(2)=1.0!                       NR7
    do i=1,n!                       NR7
        if (i/=2)then!                       NR7
            l(2)=l(2)*((p-x(i))/(x(2)-x(i)))!                       NR7
        end if!                       NR7
    end do!                       NR7
    l(3)=1.0!                       NR7
    do i=1,n!                       NR7
        if (i/=3)then!                       NR7
            l(3)=l(3)*((p-x(i))/(x(3)-x(i)))!                       NR7
        end if!                       NR7
    end do!                       NR7
    l(4)=1.0!                       NR7
    do i=1,n!                       NR7
        if (i/=4)then!                       NR7
            l(4)=l(4)*((p-x(i))/(x(4)-x(i)))!                       NR7
        end if!                       NR7
    end do!                       NR7
    l(5)=1.0!                       NR7
    do i=1,n!                       NR7
        if (i/=5)then!                       NR7
            l(5)=l(5)*((p-x(i))/(x(5)-x(i)))!                       NR7
        end if!                       NR7
    end do!                       NR7
    f=0.0!                       NR7
    do i =1 ,n!                       NR7
        f=f+l(i)*y(i)!                       NR7
    end do!                       NR7
    open(44,file="output2.2.txt")!                       NR7
        write(44,*)"the value of x is : " ,f!                       NR7
    close (44)!                       NR7
    print*,'the output show in the file of "output2.2.txt" '!                       NR7
end program!                       NR7
