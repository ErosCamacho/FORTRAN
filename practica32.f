        PROGRAM practica32
        
        REAL :: a,b,total,incremento
        INTEGER :: intervalo

        CALL EntradaDatos
        CALL CalculoArea
        CALL SalidaResultados
        
        READ*,
        CONTAINS

        SUBROUTINE EntradaDatos
        PRINT*,"Introduce el limite inferior"
        READ*,a
        PRINT*,"Introduce el limite superior"
        READ*,b
        PRINT*,"Introducir numero de intervalos"
        READ*,intervalo
        END SUBROUTINE EntradaDatos

        SUBROUTINE CalculoArea
        incremento=(b-a)/intervalo
        total = area(incremento,a,intervalo)
        END SUBROUTINE CalculoArea

        FUNCTION area(x,estamos,n) RESULT(at)
        REAL :: at,arect,atri,x,estamos,siguiente
        INTEGER :: i,n
        at=0
        DO i=1,n,1
        siguiente=estamos+x
        arect=x*(f(estamos))
        atri=x*(f(siguiente)-f(estamos))*0.5
        at=at+arect+atri
        estamos=estamos+x
        END DO
        END FUNCTION
        

        FUNCTION f(funcion) RESULT(res)
        REAL :: funcion, res
        res=(1/funcion)
        END FUNCTION
        
        SUBROUTINE SalidaResultados
         PRINT*,"El area tiene un valor de:",total
        END SUBROUTINE SalidaResultados

        END PROGRAM  practica32

