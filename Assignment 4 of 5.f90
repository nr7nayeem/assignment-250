program print_8bit_binary_filtered
    implicit none
    integer :: n, i
    character(len=8) :: bin
    logical :: is_valid
    print *, 'Valid 8-bit Binary Strings (Ascending Order):'
    print *, 'Decimal   Binary'
    print *, '-------   --------'

    ! Ascending Order
    do n = 1, 256
        bin = '00000000'   ! Initialize with all '0's (fixed length 8 characters)

        ! Build the binary string
        do i = 8, 1, -1
            if (btest(n, i)) then
                bin(i:i) = '1'   ! Set the corresponding position to '1'
            else
                bin(i:i) = '0'   ! Set the corresponding position to '0'
            end if
        end do

        ! Filter out strings containing "101" or "100"
        is_valid = index(bin, '101') == 0 .and. index(bin, '100') == 0

        if (is_valid) then
            write(*,'(I3, 3X, A)') n, bin
        end if
    end do


    print *, 'Valid 8-bit Binary Strings (descending Order):'
    print *, 'Decimal   Binary'
    print *, '-------   --------'

    ! descending Order
    do n = 256,1,-1
        bin = '00000000'   ! Initialize with all '0's (fixed length 8 characters)

        ! Build the binary string
        do i = 8, 1, -1
            if (btest(n, i)) then
                bin(i:i) = '1'   ! Set the corresponding position to '1'
            else
                bin(i:i) = '0'   ! Set the corresponding position to '0'
            end if
        end do

        ! Filter out strings containing "101" or "100"
        is_valid = index(bin, '101') == 0 .and. index(bin, '100') == 0

        if (is_valid) then
            write(*,'(I3, 3X, A)') n, bin
        end if
    end do
end program

