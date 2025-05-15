program amicable_number
    implicit none
    integer::num1,num2,count
    count=0
    do num1=1,100000
        if(count>=10)exit
        num2=sum_of_divisors(num1)
        if(num2>num1.and.sum_of_divisors(num2)==num1)then
            print*,num1,num2
            count=count+1
        end if
    end do
    contains
    integer function sum_of_divisors(n)
        implicit none
        integer,intent(in)::n
        integer::i
        sum_of_divisors=0
        do i=1,n/2
            if(mod(n,i)==0)then
                sum_of_divisors=sum_of_divisors+i
            end if
        end do
    end function
end
