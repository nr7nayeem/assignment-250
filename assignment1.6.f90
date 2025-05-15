program matrix_multiplication

  implicit none
  integer :: r1, c1, r2, c2, i, j, k
  real, allocatable :: A(:,:), B(:,:), C(:,:)

  ! Get dimensions of matrices from user
  write(*,*) 'Enter number of rows in matrix A:'
  read(*,*) r1
  write(*,*) 'Enter number of columns in matrix A:'
  read(*,*) c1
  write(*,*) 'Enter number of rows in matrix B:'
  read(*,*) r2
  write(*,*) 'Enter number of columns in matrix B:'
  read(*,*) c2

  ! Check if multiplication is possible
  if (c1 /= r2) then
    write(*,*) 'Matrix multiplication is not possible.'
    stop
  end if

  ! Allocate memory for matrices
  allocate(A(r1,c1), B(r2,c2), C(r1,c2))

  ! Read matrices from files (adjust file names as needed)
  open(unit=10, file='matrix_A.txt', status='old')
  open(unit=20, file='matrix_B.txt', status='old')
  read(10,*) ((A(i,j), j=1,c1), i=1,r1)
  read(20,*) ((B(i,j), j=1,c2), i=1,r2)
  close(10)
  close(20)

  ! Multiply matrices
  do i = 1, r1
    do j = 1, c2
      C(i,j) = 0.0
      do k = 1, c1
        C(i,j) = C(i,j) + A(i,k)*B(k,j)
      end do
    end do
  end do

  ! Print the result
  write(*,*) 'Product of the matrices:'
  write(*,*) C

  deallocate(A, B, C)

end program matrix_multiplication


