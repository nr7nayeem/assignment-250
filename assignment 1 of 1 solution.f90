program asignment1_of_1_NR7
    implicit none
    integer::i,j,mat(4,4),d3,d2
!create file for input and output
    open(unit=10,file=("inputp1.txt"))
    open(unit=11,file=("outputp1.txt"))
!READ THE MATRIX FROM FILE
    do i =1,4
      read(10,*)(mat(i,j),j=1,4)
    end do
!SHOW THE MATRIX
    do i=1,4
     write(11,"(10I2) ")(mat(i,j),j=1,4)
    end do
!SHOE THE ALL ELEMENT IN VERTICAL LINE
    do j=1,4
       do i=1,4
        write(11,"(10I2) ")mat(i,j)
        end do
    end do
!SHOW THE ALL ELEMENT IN HORIZONTAL LINE
    do i=1,4
       do j=1,4
        write(11,"(10I2) ",advance="no")mat(i,j)
        end do
    end do
!show the 1st row 1st column
    write(11,"(10I2)")(mat(1,i),i=1,4)
    do i=2,4
    write(11,"(10I2)")mat(i,1)
    end do
!SUM OF MAJOR AND MINOR DIAGONAL
    d3=0
    do i=1,4
        d3=d3+mat(i,i)**3
    end do
     d2=0
    do i=1,4
        d2=d2+mat(i,5-i)**2
    end do
    write(11,*)d3,">",d2
!transpose the matrix
    do j=1,4
        write(11,"(10I2) ")(mat(i,j),i=1,4)
    end do
    print*,"the output show the file "
end program
!                          stay         with        NR7

