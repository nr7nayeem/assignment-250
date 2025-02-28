program assignment_4_of_2_NR7
    implicit none
    real::RT(5,5),x,h
    integer :: i,j
    x=2.0
    h=0.1
    do i=1,5
        RT(i,1)=(f(x+(h/2.**real(i)))-f(x-(h/2.**real(i))))/(2.*(h/2.**real(i)))
    end do

    do j=2,5
        do i=1,6-j
            RT(i,j)=RT(i+1,j-1)+((RT(i+1,j-1)-RT(i,j-1))/(4.0**(j-1)-1.0))
        end do
    end do
    do i = 1,5
        do j = 1,6-i
            write(*, "(F12.6)",advance="no") RT(i, j)!                       NR7
        end do
            write(*,*)
    end do
    write(*,*)"root is : ", RT(1,5)

contains
real function f(x)
    real,intent(in)::x
    f=x*exp(x)
end function
end program
! stay with NR7

