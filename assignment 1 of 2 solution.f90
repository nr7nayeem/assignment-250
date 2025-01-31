program assignment1_of_2_NR7
    implicit none
    integer,allocatable::a(:)
    integer::i,j,small,n
    print*,"the number of array size"
    read(*,*)n
    if(n<5)then
        print*,"enter the array number must be greater or equal 5 "
    end if
    allocate(a(n))
    print*,"enter the element of the array "
    do i=1,n
        read*,a(i)
    end do
    do i=1,n
        print*,a(i)
    end do
    do i=1,5
        small=a(i)
        do j=i+1,n
            if(a(i)<a(j))then
                small=a(i)
                a(i)=a(j)
                a(j)=small
            end if
        end do
    end do
    print*,"descending order list "
    do i=1,n
        print*,a(i)
    end do
     print*,"ascending order list "
    do i=n,1,-1
        print*,a(i)
    end do
end program
!                 stay      with     NR7
