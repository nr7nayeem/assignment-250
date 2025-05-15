program assignment_4ii_simpsons_13_NR7
    implicit none
    real:: a,b,h,area,s1,s2,x
    integer:: i,n
    n=30
    a=0.
    b=1.
    h=(b-a)/real(n)
    s1=0.
    s2=0.
    do i=0,(n/2)-1
        x=h+(2.*h*real(i))
        s1=s1+f(x)
    end do
    do i=0,(n/2)-1
        x=2.*h+(2.*h*real(i))
        s2=s2+f(x)

    end do
    area=(h/3.)*(f(a)+4.*s1+2.*s2+f(b))
    print*,area
contains
real function f(x)
real ,intent (in) :: x
f=1/(1+x**2)
end function
end program
! stay with NR7
