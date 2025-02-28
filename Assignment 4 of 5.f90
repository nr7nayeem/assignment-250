program assignment_5_romberg_NR7
    implicit none
    real,parameter :: a=0.,b=1.,tolerance=0.000005
    real :: x,h,RT(7,7),s,exact
    integer :: n, i ,J,k
    do k = 1,7
        n = 2**(k-1)
        s=0.
        h=(b-a)/n
        do i=1,n-1
            x=a+h*real(i)
            s=s+f(x)
        end do
        RT(k,1)=(h/2.)*(f(a)+2.*s+f(b))
    end do

    do j=2,k
        do i=1,k+1-j
            RT(i,j)=RT(i+1,j-1)+((RT(i+1,j-1)-RT(i,j-1))/(4.0**(j-1)-1.0))
        end do
         if (abs(RT(1,j) - RT(1,j-1)) < tolerance)exit
    end do

    do i = 1,k
        do j = 1,k+1-i
            write(*, "(F12.6)",advance="no") RT(i, j)!                       NR7
        end do
            write(*,*)
    end do
    exact=Sf(1.)-Sf(0.)
    write(*,*)"root is : ", RT(1,k)
    write(*,*)"exact value : " ,exact
    Write(*,*)"compare the result : error is = ", abs(RT(1,k)-exact)
CONTAINS
real function f(x)
real,intent(in) :: x
f= 1/(1+x**2)
end function
real function Sf(x)
real,intent(in) :: x
Sf= atan(x)
end function
end program
! stay with NR7
