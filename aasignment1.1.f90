program simple_matrix
    implicit none
    integer::matA(4,4)
    integer::i,j,s,w
    matA=reshape((/1,5,0,8,7,2,9,1,3,6,3,7,4,0,5,6/),(/4,4/))
    do i=1,4
        write(*,'(100I3)')(matA(i,j),j=1,4)
    end do
    print*,matA(1,2)
    do i=1,4
        do j=1,4
            write(*,'(101I3)',advance='no')matA(i,j)
        end do
    end do



     do i=1,4
        do j=1,4
            write(*,'(101I2)')matA(i,j)
        end do
    end do



        print*,"the main diagonal"
 s=0
    do i = 1, 4
        s=s+matA(i,i)
        print*,matA(i,i)
    end do
        print *,s**3
 w=0
    do i = 1, 4
        w=w+matA(i,4-i+1)
        print*,matA(i,4-i+1)
    end do
        print *,w**2
        Print*,"transfose matrix is "
        do j=1,4
            write(*,'(1020I3)'),(matA(i,j),i=1,4)
        end do








end program simple_matrix

