program assignment_4_of_3_NR7
    implicit none
    real :: h, a, a0, ah, a2h,a3h,a4h,a5h,a6h,velocity1,acceleration1,velocity2,acceleration2

    a = 0.
    h = 0.1

    a0 = 30.13
    ah = 31.62
    a2h = 32.87
    a3h = 33.64
    a4h = 33.95
    a5h = 33.81
    a6h = 33.24

velocity1=(-147.0*a0+360.0*ah-450*a2h+400.0*a3h-225.0*a4h+72.0*a5h-10.0*a6h)/(60.0*h)
acceleration1= (490.0*a0 -2700.0*ah +5400*a2h -5880.0*a3h +3920.0*a4h -1470.0*a5h +245.0*a6h)/(180.0*h**2)

    print *,"velocity at 0.0 s =  ", velocity1
    print*, "      "
    print *, "acceleration at 0.0 s =  " ,acceleration1
print*, "      "

    a = 0.6
    h = -0.1

    a0 = 33.24
    ah = 33.81
    a2h = 33.95
    a3h = 33.64
    a4h = 32.87
    a5h = 31.62
    a6h = 30.13
velocity2=(-147.0*a0+360.0*ah-450*a2h+400.0*a3h-225.0*a4h+72.0*a5h-10.0*a6h)/(60.0*h)
acceleration2= (490.0*a0 -2700.0*ah +5400*a2h -5880.0*a3h +3920.0*a4h -1470.0*a5h +245.0*a6h)/(180.0*h**2)
    print *,"velocity at 0.6 s =  ", velocity2
    print*, "      "
    print *, "acceleration at 0.6 s  = " ,acceleration2

end program
