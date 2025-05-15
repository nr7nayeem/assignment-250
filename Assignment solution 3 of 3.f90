!                       NR7
program divided_difference_table!                       NR7
    implicit none!                       NR7
    integer, parameter :: n = 6!                       NR7
    real :: x(n), y(n), dd_table(n, n+1),p,t,f,q!                       NR7
    integer :: i, j
    open (2,file='input3.txt')!                       NR7
    do i = 1, n
        read(2,*) x(i),y(i)!                       NR7
    end do!                       NR7
    close(2)!                       NR7
    do i = 1, n!                       NR7
        dd_table(i, 1) = x(i)!                       NR7
    end do!                       NR7
    do i = 1, n!                       NR7
        dd_table(i, 2) = y(i)
    end do!                       NR7
    do j =3,n+1!                       NR7
        do i=1,n-1!                       NR7
            dd_table(i,j) = dd_table(i+1, j-1) -dd_table(i, j-1)!                       NR7
        end do!                       NR7
    end do!                       NR7
print*," the divided difference table is : "!                       NR7
    do i = 1, n!                       NR7
        do j = 1,n+1-i+1!                       NR7
            write(*, "(F10.5)",advance="no") dd_table(i, j)!                       NR7
        end do!                       NR7
        print *!                       NR7
    end do!                       NR7

print*,"Enter the value of x(38°). "!                       NR7
read*,q!                       NR7
    f=dd_table(1,2)!                       NR7
    t=1.0!                       NR7
        p=(q-dd_table(1,1))/(dd_table(2,1)-dd_table(1,1))!                       NR7
    do i= 0,4!                       NR7
        t=t*((p-real(i))/(real(i)+1.0))!                       NR7
        f=f +(t*dd_table(1.0,real(i)+3.0))!                       NR7
    end do!                       NR7
    open(3,file="output3.txt")!                       NR7
        write(3,*)"the value of sin (38°). ",f!                       NR7
    close(3)!                       NR7
    print *, "the result show in output3.txt file"!                       NR7
end program divided_difference_table!                       NR7
!                       NR7
