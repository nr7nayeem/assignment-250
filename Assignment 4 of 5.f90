program print_8bit_binary_high_to_low
    implicit none
    integer :: n, i

    print *, 'Decimal   Binary (Low to High))'
    print *, '-------   ---------------------'

    do n = 0, 255
        write(*,'(I3, 3X)', advance="no") n  ! Decimal Number
        do i = 7, 0, -1                      ! From bit 7 to bit 0
            if (btest(n, i)) then
                write(*,'(A)', advance="no") '1'
            else
                write(*,'(A)', advance="no") '0'
            end if
        end do
        print*   ! New line
    end do
    print *, 'Decimal   Binary (High to Low)'

        print *, '-------   ---------------------'

    do n = 255,0,-1
        write(*,'(I3, 3X)', advance="no") n  ! Decimal Number
        do i = 7, 0, -1                      ! From bit 7 to bit 0
            if (btest(n, i)) then
                write(*,'(A)', advance="no") '1'
            else
                write(*,'(A)', advance="no") '0'
            end if
        end do
        print*   ! New line
    end do

end program print_8bit_binary_high_to_low

