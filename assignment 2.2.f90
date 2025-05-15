program fixed
    implicit none
    real::a,b
    integer::i
    open(unit=11,file="outputfixed.txt")

write(11,*)"--------(i)-----------"

write(11,*)"for the function (a) g(x)"
    a=0.5
    if(abs(dg(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=g(a)
            if (abs(b-a)>0.000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (a) g(x) ? because does not convergent"
    end if

write(11,*)"for the function (b) g(x)"
    if(abs(dh(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=h(a)
            if (abs(b-a)>0.0000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (b) g(x) ? because does not convergent"
    end if

write(11,*)"for the function (c) g(x)"
    if(abs(df(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=f(a)
            if (abs(b-a)>0.0000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (c) g(x) ? because does not convergent"
    end if


write(11,*)"--------(ii)-----------"

write(11,*)"for the function (a) g(x)"
    a=1.0
    if(abs(dg(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=g(a)
            if (abs(b-a)>0.000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (a) g(x) ? because does not convergent"
    end if

write(11,*)"for the function (b) g(x)"
    if(abs(dh(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=h(a)
            if (abs(b-a)>0.0000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (b) g(x) ? because does not convergent"
    end if

write(11,*)"for the function (c) g(x)"
    if(abs(df(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=f(a)
            if (abs(b-a)>0.0000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (c) g(x) ? because does not convergent"
    end if



write(11,*)"--------(iii)-----------"

write(11,*)"for the function (a) g(x)"
    a=1.5
    if(abs(dg(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=g(a)
            if (abs(b-a)>0.000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (a) g(x) ? because does not convergent"
    end if

write(11,*)"for the function (b) g(x)"
    if(abs(dh(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=h(a)
            if (abs(b-a)>0.0000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (b) g(x) ? because does not convergent"
    end if

write(11,*)"for the function (c) g(x)"
    if(abs(df(a))<1.0)then
        write(11,"(A3,5x,A7,9x,A8,2x,A9,7x,A9,11x,A7)")"itr","P(n-1)","Pn","f(Pn)","rel err"
        do i=1,100
            b=f(a)
            if (abs(b-a)>0.0000001)then
                write(11,"(I2,4F15.6)")i,a,b,M(b),abs(b-a)
                a=b
            end if
        end do
    else
        write(11,*)"not right for (c) g(x) ? because does not convergent"
    end if

contains

real function M(x)
implicit none
    real,intent(in)::x
    M=(x**4)-x-10
end function


real function g(x)
    implicit none
    real,intent(in)::x
    g=(sqrt(x+10.0))/x
end function
real function dg(x)
    implicit none
    real,intent(in)::x
    dg=1/(2.0*x*sqrt(x+10.0))-(sqrt(x+10.0)/x**2.0)
end function


real function h(x)
    implicit none
    real,intent(in)::x
    h=(x+10.0)**(1.0/4.0)
end function
real function dh(x)
    implicit none
    real,intent(in)::x
    dh=1/(4.0*(x+10.0)**(3.0/4.0))
end function


real function f(x)
    implicit none
    real,intent(in)::x
    f=10.0/(x**3.0-1.0)
end function
real function df(x)
    implicit none
    real,intent(in)::x
    df=(-30.0*(x**2.0))/((x**3.0-1.0)**2.0)
end function
end program
