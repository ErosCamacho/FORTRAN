        PROGRAM practica43
        
        CALL BuscarNumero
        
        PRINT*,"FIN"
        READ*,
        
        CONTAINS
        SUBROUTINE BuscarNumero
        INTEGER :: n,i,j,suma
        PRINT*,"Introduzca el numero hasta el que queremos llegar"
        READ*,n
        DO i=1,n,1
        suma=0
              DO j=1,i-1,1
                IF(MOD(i,j)==0) THEN
                suma=suma+j
                END IF
              END DO
         IF(suma==i) THEN
         PRINT*,i
         END IF
        END DO
        END SUBROUTINE BuscarNumero
        
        END PROGRAM practica43
