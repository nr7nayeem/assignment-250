program p
    implicit none
    integer,parameter::ik=selected_int_kind(21)
    integer(ik)::n,H,D
    real::l
    do n=1,64
        H=Hanoi(n)
        l=(5.*H)/86400
        D=int(l+0.99999999999,kind=ik)
        write(*,*) n, H,D
    end do
    contains
    recursive function hanoi(x) result(t)
    integer(ik),intent(in) :: x
    integer (ik)::t
    if(x==1)then
        t=1
        else
        t=2*hanoi(x-1)+1
    end if
    end function
end program


