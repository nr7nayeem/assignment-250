PROGRAM FermatNumbers
  IMPLICIT NONE
  INTEGER :: n
  INTEGER(8):: fermat
  LOGICAL :: isPrime
  PRINT *, "n", "Fermat Number", "Is Prime?"
  DO n = 0, 4
     fermat = 2**(2**n) + 1
     isPrime = CheckPrime(fermat)
     PRINT*, n, fermat,"->", isPrime
  END DO
CONTAINS
  FUNCTION CheckPrime(number) RESULT(isPrime)
    IMPLICIT NONE
    INTEGER(8), INTENT(IN) :: number
    LOGICAL :: isPrime
    INTEGER(8) :: i
    isPrime = .TRUE.
    DO i = 2, number / 2
       IF (MOD(number, i) == 0) THEN
          isPrime = .FALSE.
       END IF
    END DO
  END FUNCTION CheckPrime
END PROGRAM FermatNumbers
