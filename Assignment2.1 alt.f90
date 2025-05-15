program bisection
    implicit none
    real::a,b,root
    integer::n
    a=1.0
    b=3.0
    call bisect(a,b,root,n)
    do n=1,100
        print*,root,n
    end do
    contains
    real function f(x)
        implicit none
        real::x
        f = 3.0*x+Sin(x)-exp(x)
    end function

    subroutine bisect(a,b,root,n)
        implicit none
        real::a,b,c,tollr,error,root
        integer::n
        n=1
        tollr=0.000000000000000000000000023
        do n=1,100
       c=(a+b)/2.0
        if (f(a)*f(b)<0.0)then
            b=c
        else
            a=c
        end if
        error = abs((a-b)/c)
        if (error<=tollr) then
            root=c
        end if
        end do




    end subroutine
end program
