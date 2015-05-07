        PROGRAM fibonaci
        
        INTEGER :: valor,resultado
        
        PRINT*,"Inserte un numero"
        READ*,valor
        
        resultado=serie(valor)
        
        !PRINT*,resultado
        READ*,
        
        
        CONTAINS
        
        FUNCTION serie(x) RESULT(num)
        INTEGER :: num,x,i
        num=2
        IF(x>=3) THEN
        PRINT*,"1"
        PRINT*,"1"
        END IF
        DO i=1,x-2,1
        PRINT*,num
        num=num+i
        END DO
        
        IF(x==1.OR.x==2) THEN
        num=1
        DO i=1,x,1
        PRINT*,"q",num
        END DO
        END IF
        
        END FUNCTION serie
        
        END PROGRAM fibonaci
