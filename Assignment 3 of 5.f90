program truth_table
  implicit none
  integer :: i, j, k
  logical :: p, q, r
  logical :: expr1, expr2
  integer :: count1, count2

  count1 = 0
  count2 = 0

  print *, 'Truth Table for (p ∧ q) ∨ r and (p ∨ r) ∧ (q ∨ r)'
  print *, '-----------------------------------------------'
  print *, ' p   q   r  | (p ∧ q) ∨ r  | (p ∨ r) ∧ (q ∨ r)'
  print *, '-----------------------------------------------'

  do i = 0, 1
     do j = 0, 1
        do k = 0, 1
           p = (i == 1)
           q = (j == 1)
           r = (k == 1)

           expr1 = (p .and. q) .or. r
           expr2 = (p .or. r) .and. (q .or. r)

           if (expr1) count1 = count1 + 1
           if (expr2) count2 = count2 + 1

           print '(3L5, 3X, L16, 3X, L16)', p, q, r, expr1, expr2
        end do
     end do
  end do

  if (count1 == 8) then
     print *, '(p ∧ q) ∨ r is a tautology.'
  else
     print *, '(p ∧ q) ∨ r is NOT a tautology.'
  end if

  if (count2 == 8) then
     print *, '(p ∨ r) ∧ (q ∨ r) is a tautology.'
  else
     print *, '(p ∨ r) ∧ (q ∨ r) is NOT a tautology.'
  end if

end program truth_table

