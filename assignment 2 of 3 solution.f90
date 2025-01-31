 program assignment_2_of_3_NR7
    implicit none
    integer::i
    real::x,x1
    open(unit=10,file="outputNewtonRaphson.txt")

    !(i)
    write(10,*)"(i) when initial point is -5 "
    x=-5
    write(10,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","f'(Pn-1)","Pn","f(Pn)","rel err"
    do i=1,100
        x1=x-(f(x)/df(x))
        if(abs(x1-x)>0.000001)then
            write(10,"(I2,5F15.6)")i,x,df(x),x1,f(x1),abs(x1-x)
            x=x1
        end if
    end do
    write(10,*)"the real root is  " ,x1

    !(ii)
    write(10,*)" (ii) when initial point is -2 "
    x=-2
    write(10,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","f'(Pn-1)","Pn","f(Pn)","rel err"
    do i=1,100
        x1=x-(f(x)/df(x))
        if(abs(x1-x)>0.000001)then
            write(10,"(I2,5F15.6)")i,x,df(x),x1,f(x1),abs(x1-x)
            x=x1
        end if
    end do
    write(10,*)"the real root is  " ,x1

    !(iii)
    write(10,*)" (iii) when initial point is 1 "
    x=1
    write(10,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","f'(Pn-1)","Pn","f(Pn)","rel err"
    do i=1,100
        x1=x-(f(x)/df(x))
        if(abs(x1-x)>0.000001)then
            write(10,"(I2,5F15.6)")i,x,df(x),x1,f(x1),abs(x1-x)
            x=x1
        end if
    end do
    write(10,*)"the real root is  " ,x1

    !iv
    write(10,*)" (iv) when initial point is 4 "
    x=4
    write(10,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","f'(Pn-1)","Pn","f(Pn)","rel err"
    do i=1,100
        x1=x-(f(x)/df(x))
        if(abs(x1-x)>0.000001)then
            write(10,"(I2,5F15.6)")i,x,df(x),x1,f(x1),abs(x1-x)
            x=x1
        end if
    end do
    write(10,*)"the real root is  " ,x1


write(10,*)"            stay            with                    NR7"
contains
    real function f(x)
        implicit none
        real,intent(in)::x
        f=5.0*(x**2.0)+cos(3.*x)-2.*exp(x)-exp(-x)
    end function
    real function df(x)
        implicit none
        real,intent(in)::x
        df=10.*x-sin(3.*x)-2.*exp(x)+exp(-x)
    end function
 end program

