program test
    implicit none
    integer::i,Rel
    real:: formula
    do i=1,10
        Rel=f(i)
        formula=((3./2.)*i**2.)+(5./2.)*i+1.
        write(*,"(I7,I7,5X,F7.2)")i,Rel,formula
    end do



CONTAINS

    recursive function f(x) result(t)
        integer ,intent(in) :: x
        integer ::t

        if(x==0)then
            t=1
        else if(x==1)then
            t=5
        else
            t=2*f(x-1)-f(x-2)+3
        end if
    end function
end program
