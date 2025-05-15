program bisection_with_table
  implicit none
  real :: a, b, x, f, tol, relative_error
  integer :: iter, max_iter
  a = 1.0
  b = 3.0
  tol = 0.00000001
  max_iter = 100
  iter = 0
  write(*, '(2x, "Iteration Number", 5x, "a", 5x, "         b", 10x,             "x", 14x,       "f(x)", 5x,     "Relative Error")')
  write(*, '(5x, "---------------", 5x, "---", 5x, "---", 5x, "---", 5x, "-----", 5x, "-------------")')
  do iter = 1, max_iter
    x = (a + b) / 2.0
    f = func(x)
    relative_error = abs((b - a) / x)
    write(*, '(5x, I5, 5x, F10.6, 5x, F10.6, 5x, F10.6, 5x, F10.6, 5x, F10.6)') &
         iter, a, b, x, f, relative_error
    if (func(b) * func(a) .lt. 0.0) then
      b = x
    else
      a = x
    end if
     if (abs((b-a)/x) .le. tol) then
      exit
    end if
  end do
    write(*,*) "x =", x, "   f(x) =", f
contains
  real function func(x)
    implicit none
    real,intent(in):: x
    func = 3.0 *x+ sin(x) - exp(x)
  end function
end program bisection_with_table
