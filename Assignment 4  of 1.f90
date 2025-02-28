program assignment_4of_1_NR7
    implicit none
    real::R1,R2,h,h1,h2,R3,h3,R4,h4
    real,parameter::x=5.0

    write(*,*)"three point mid point formula : "
    h1=-0.25
    R1=(f(x+h1)-f(x-h1))/(2.*h1)
    write(*,*) "root is : " ,R1


    write(*,*)"three point end point formula : "
    h2=0.25
    R2=(-3.*f(x)+4.*f(x+h2)-f(x+2.*h2))/(2.*h2)
    write(*,*) "root is : " ,R2


    write(*,*)"five point mid point formula : "
    h3=0.5
    R3=(f(x-2.0*h3)-8.0*f(x-h3)+8.0*f(x+h3)-f(x+2.0*h3))/(12.0*h3)
    write(*,*) "root is : " ,R3


    write(*,*)"five point end point formula : "
    h4=0.50
    R4=(-25.0*f(x) +48.0*f(x+h4)-36.0*f(x+2.0*h4)+16.0*f(x+3.0*h4)-3.0*f(x+4.0*h4))/(12.0*h4)
    write(*,*) "root is : " ,R4


contains
real function f(x)
real ,intent(in) :: x
f=60.0-(60.0*exp(-x/5.0))
end function
end program
