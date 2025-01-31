program assignment_2_of_1_NR7
    implicit none
    real::a,b,c
    integer::i
    open(unit=12,file="outputBisection.txt")
    if(f(1.0)*f(3.)<0)then
        write(12,*)" Root has in this is interval [1,3] | "," because  " ,"f(1) = ",f(1.0),"&  ","f(3) = ",f(3.0)
    end if
    a=1.0
    b=3.0
    write(12,"(A5,5x,A7,6x,A8,6x,A9,8x,A9,7x,A7)")"itr","a","b","x","f(x)","rel err"
    do i=1,100
        c =(a+b)/2.0
        write(12,"(I4,4F15.6,E18.7)")i,a,b,c,f(c),abs((b-a)/c)
        if(f(a)*f(c)<0.0)then
            b=c
        else
            a=c
        end if
        if((abs(a-b)/c)<1.0E-8)then
            write(12,*)"Root is = ",c
            exit
        end if
    end do


write(12,*)"                     stay               with                      NR7"
contains
    real function f(x)
        implicit none
        real,intent(in)::x
        f=3*x+sin(x)-exp(x)
    end function
end program
