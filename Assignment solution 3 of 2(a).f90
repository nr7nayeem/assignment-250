!                       NR7
program divided_difference_table!                       NR7
    implicit none!                       NR7
    integer, parameter :: n = 5!                       NR7
    real :: x(n), y(n), dd_table(n, n+1),t,f,p!                       NR7
    integer :: i, j!                       NR7
    open (2,file='input2.txt')!                       NR7
    do i = 1, n!                       NR7
        read(2,*) x(i),y(i)!                       NR7
    end do!                       NR7
    close(2)!                       NR7
    do i = 1, n!                       NR7
        dd_table(i, 1) = x(i)!                       NR7
    end do!                       NR7
    do i = 1, n!                       NR7
        dd_table(i, 2) = y(i)!                       NR7
    end do!                       NR7
    do j =3,n+1!                       NR7
        do i=1,n-j+2!                       NR7
            dd_table(i,j) = (dd_table(i+1, j-1)-dd_table(i, j-1))/(dd_table(i+j-2,1)-dd_table(i,1))
        end do!                       NR7
    end do!                       NR7
print*," the divided difference table is : "!                       NR7
    do i = 1, n!                       NR7
        do j = 1,n+1-i+1!                       NR7
            write(*, "(F10.5)",advance="no") dd_table(i, j)!                       NR7
        end do!                       NR7
        print *!                       NR7
    end do!                       NR7
    print*,"Enter the value of x for interpolated "!                       NR7
   read*,p!                       NR7
    f=dd_table(1,2)!                       NR7
    t=1.0!                       NR7
    do i= 0,3!                       NR7
        t=t*(p-dd_table(i+1,1))!                       NR7
        f=f+(t*dd_table(1,i+3))!                       NR7
    end do!                       NR7
    open(34,file="output2.txt")!                       NR7
    write(34,*) "the value of x is : " ,f!                       NR7
    close(3)!                       NR7
    print *, 'the output show in the file of "output2.txt" '!                       NR7
end program divided_difference_table!                       NR7

