program assignment_4i_trapezoidal_NR7
    implicit none
    real :: x,s,h,a,b,area
    integer :: n, i
    a=0.
    b=1.
    n=30
    s=0.0
    h=(b-a)/n
    do i=1,n-1
        x=a+h*real(i)
        s=s+f(x)
    end do
    area=(h/2.)*(f(a)+2.*s+f(b))
    print*,area
contains
real function f(x)
real,intent(in) :: x
f=1/(1+x**2)
end function
end program
! stay with NR7
