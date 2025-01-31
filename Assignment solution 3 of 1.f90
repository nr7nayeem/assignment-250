!                                                                     NR7
program divided_difference_table!                                     NR7
    implicit none!                                                 NR7
    integer, parameter :: n = 6!                                 NR7
    real :: x(n), y(n), dd_table(n, n+1),p,t,f1,f2,q1,q2!                       NR7
    integer :: i, j!                                             NR7
    open (2,file='input.txt')!                                     NR7
    do i = 1, n!                                              NR7
        read(2,*) x(i),y(i)!                       NR7
    end do!                                              NR7
    close(2)!                               NR7
    do i = 1, n!                       NR7
        dd_table(i, 1) = x(i)!                       NR7
    end do!                       NR7
    do i = 1, n!                       NR7
        dd_table(i, 2) = y(i)!                       NR7
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
print*,"enter the value of x(8) at 8 AM "!                       NR7
read*,q1!                       NR7
print*,"enter the value of x(16) at 4 PM "!                       NR7
read*,q2!                       NR7
    f1=dd_table(1,2)!                       NR7
    t=1.0!                       NR7
        p=(q1-dd_table(1,1))/(dd_table(2,1)-dd_table(1,1))!                       NR7
    do i= 0,4!                       NR7
        t=t*((p-real(i))/(real(i)+1.0))!                       NR7
        f1=f1 +(t*dd_table(1.0,real(i)+3.0))!                       NR7
    end do!                       NR7
    f2=dd_table(6,2)!                       NR7
    t=1.0!                       NR7
        p=(q2-dd_table(6,1))/(dd_table(2,1)-dd_table(1,1))!                       NR7
         do i= 0,4!                       NR7
        t=t*((p+real(i))/(real(i)+1.0))!                       NR7
        f2=f2 +(t*dd_table(5.0-i,real(i)+3.0))!                       NR7
    end do!                       NR7
    open(3,file="output.txt")!                       NR7
        write(3,*)"the river’s water level at 8:00 AM is  : ",f1!                       NR7
        write(3,*)"the river’s water level at 8:00 AM is  : ",f2!                       NR7
    close(3)!                       NR7
    print *, "the result show in output.txt file"!                       NR7
end program divided_difference_table!                       NR7
