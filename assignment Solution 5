program prac
    implicit none
    integer,parameter ::k = selected_int_kind(30)
    integer(k)::M,n


    do n= 0,69
        M=2_k**n-1_k
        if(cp(M))then
            write(*,"(I3,3x,I19,2A)")n ,M," ->" ,"prime"
            print*,"-------------------------------------"
        else
            write(*,"(I3,3x,I19,2A)")n ,M," ->" , "not prime "
            print*,"-------------------------------------"
        end if
    end do
    contains
    logical function cp(x)
    integer(k),intent(in)::x
    integer::i
    if(x<1)then
        cp=.false.
        return
    end if
    do i=2,int(sqrt(real(x)))
        if (mod(x,i)==0)then
            cp=.false.
            return
        end if
    end do
    cp=.true.
    end function
end program

