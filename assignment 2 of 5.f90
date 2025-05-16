program Assignment_2
    implicit none
    real::n,H,day ,M
    write(*,"(A,10X,A,10X,A)")"number","Hn" , "day"
    do n=1,8

    H= hanoi(n**2.)
    day=H*(5./86400.)
    write(*,"(F4.1,6x ,Es10.2,6X, E10.3)")n**2, H,day
    end do
    write(*,*)"the relation between the Hn and Mn "
    do n=1,8
        H= hanoi(n)
        M=2**n-1
        write(*,"(I5,5X , I5)")int(H),int(M)
    end do
contains
    recursive function hanoi(n) result(f)
    real,intent(in) :: n
    real:: f
    if(n==1)then
        f=1.
    else
        f=2.*hanoi(n-1.)+1.
    end if
    end function
end program


