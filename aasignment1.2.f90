program assignment_2
    implicit none
    integer,allocatable,dimension(:)::a
    integer::n,i,small,J

    read(*,*)n
    allocate(a(n))
    if(n.LE.5)then
        print*,"the number is less than 5"

    end if
    do i=1,n
        read(*,*)a(i)
    end do
    do i=1,n
        small=a(i)
        do j=i+1,n
            if(a(i)<a(j))then
                small=a(i)
                a(i)=a(j)
                a(j)=small
            end if
        end do
    end do
    do i=1,n
        print*,a(i)
    end do




end program
