program assignment_2_of_4_NR7
    implicit none
    real::x1,x2,x3,x0,x22
    integer::i
    open(unit=12,file="outputSecant.txt")
    if(f(0.0)*f(2.)<0)then
        write(12,*)" Root has in this is interval [0,4] | "," because  " ,"f(0) = ",f(0.0),"&  ","f(2) = ",f(2.0)
    end if
    x0=0.0
    x1=2.0
    write(12,"(A5,5x,A7,6x,A8,6x,A9,8x,A9,7x,A7)")"itr","x0","x1","x2","f(x2)","abs err"
    do i=1,100
       x2 = x1 - (f(x1) * ((x1 - x0) / (f(x1) - f(x0))))
        if(abs(x2-x1)<1.0E-6)then
            write(12,*)"Root is = ",x2
            exit
        end if
        write(12,"(I4,4F15.6,E18.7)")i,x0,x1,x2,f(x2),abs((x2-x0)/x2)
            x0=x1
            x1=x2
    end do
write(12,*)"                     stay               with                      NR7"
contains
    real function f(x)
        implicit none
        real,intent(in)::x
        f=(4.*(x**3.))-1.0-exp((x**2.)/2.)
    end function
end program
