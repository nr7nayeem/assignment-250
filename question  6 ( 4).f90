program gram_schmidt_orthonormal
  implicit none
  integer :: i, j
  real :: a(3,3), q(3,3), proj(3), dot
  real :: norm

  ! Step 1: Define the input row vectors
  a(1,:) = (/ 1.0, 1.0, 1.0 /)
  a(2,:) = (/ 1.0, 1.0, 0.0 /)
  a(3,:) = (/ 1.0, 0.0, 0.0 /)

  ! Step 2: Initialize and normalize the first orthonormal vector
  q(1,:) = a(1,:)
  norm = sqrt(dot_product(q(1,:), q(1,:)))
  q(1,:) = q(1,:) / norm

  ! Step 3: Apply Gram-Schmidt and normalize
  do i = 2, 3
     q(i,:) = a(i,:)
     do j = 1, i-1
        dot = dot_product(q(i,:), q(j,:))
        proj = dot * q(j,:)
        q(i,:) = q(i,:) - proj
     end do
     norm = sqrt(dot_product(q(i,:), q(i,:)))
     if (norm /= 0.0) then
        q(i,:) = q(i,:) / norm
     end if
  end do

  ! Step 4: Output orthonormal vectors
  print *, 'Orthonormal set:'
  do i = 1, 3
     print *, 'Vector ', i, ': ', q(i,:)
  end do

end program gram_schmidt_orthonormal

