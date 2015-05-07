        PROGRAM integral2
        
        REAL :: x,estamos,siguiente,arect,atri,at,b,a
        INTEGER :: intervalo
        
        PRINT*,"Introduce el limite inferior"
        READ*,a
        PRINT*,"Introduce el limite superior"
        READ*,b
        PRINT*,"Introducir numero de intervalos"
        READ*,intervalo
        
        x=(b-a)/intervalo
        estamos=a
        at=0

        DO i=1,intervalo,1
        siguiente=estamos+x
        arect=x*(estamos**2)
        atri=x*((siguiente**2)-(estamos**2))*0.5
        at=at+arect+atri
        estamos=estamos+x
        END DO
        PRINT*,"La funci¢n x^2 tiene como area aproximada:",at
        
        
          read*,
        
        END PROGRAM integral2

