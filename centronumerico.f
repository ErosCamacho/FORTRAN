        PROGRAM centronumerico
        INTEGER :: x,i,j,sum1,sum2
        DO x=1,100000,1
        
        sum1=0
        DO i=1,x-1,1
        sum1=sum1+i
        END DO
        
        sum2=0
        DO j=x+1,100000,1
        sum2=sum2+j
        IF (sum1==sum2) THEN
        PRINT*,x,"Intervalo","(","1",x-1,")","(",x+1,j,")"
        END IF
        IF (sum1==sum2) EXIT
        END DO



        END DO
        
        PRINT*, "Fin"
        READ*,
        END PROGRAM centronumerico
