        PROGRAM primos
        INTEGER :: n,i,j,m,u
        PRINT*, "Muestra los n primeros numeros primos"
        READ*,n


        m=0

        DO i=2,100000,1

           DO j=2,i-1,1
               u=j
              IF(MOD(i,j)==0) EXIT

           END DO

           IF(MOD(i,u)/=0) THEN
           PRINT*,i
           m=m+1
           END IF

           IF (m==n) EXIT
        END DO







        READ*,
        END PROGRAM primos
