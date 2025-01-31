program assignment_1_of_3_NR7
    implicit none
    real::matrix(6,6)
    integer::i,j
    do i=1,6
        do j=1,6
            call random_number (matrix(i,j))
            matrix(i,j)=10.0+matrix(i,j)*10
        end do
    end do
    write(10,*)"F format "
    do i=1,6
        write(10,"(36F15.8)")(matrix(i,j),j=1,6)
    end do
    write(11,*)"E format "
    do i=1,6
        write(11,"(36E15.7)")(matrix(i,j),j=1,6)
    end do
    write(12,*)"ES format "
    do i=1,6
        write(12,"(36ES15.7)")(matrix(i,j),j=1,6)
    end do
    open(unit=10,file="alq3_F.txt")
    open(unit=11,file="alq3_E.txt")
    open(unit=12,file="alq3_ES.txt")
    print*,"check the result in file "
end program
!                     stay        with         NR7


