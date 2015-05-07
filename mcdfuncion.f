	PROGRAM mcd

	INTEGER,DIMENSION(10) :: Q,W
	INTEGER :: numero1,numero2,resultado





	PRINT*,"Introduce un numero"
	READ*, numero1
	PRINT*,"Introduce otro numero"
	READ*,numero2



        Q=divisores(numero1)
        W=divisores(numero2)
        
        resultado=maximo(Q,W)
        PRINT*,"El maximo comun divisor es:", resultado

        READ*,

        CONTAINS
        
        FUNCTION maximo(A,B) RESULT(num2)
        INTEGER :: A(10),B(10),C(10)
        INTEGER :: m,n,o,l,k,num1,num2
        
        DO k=1,10,1
	C(k)=1
	END DO

	DO m=1,10,1
		DO n=1,10,1
		IF(A(m) == B(n) .AND. A(m)/=1 .AND. B(n)/=1) THEN
		C(m)=A(m)
		END IF
		IF(A(m) == B(n)) EXIT
		END DO
	END DO

	DO o=1,10,1
	IF(C(o) == C(o+1)) THEN
	C(o)=1
	END IF
	END DO

	num1=1
	DO l=1,10,1
	num1=num1*C(l)
	num2=num1
	END DO
        END FUNCTION maximo

        
        FUNCTION divisores(numero) RESULT (Y)
        INTEGER :: j,i,x,numero
        INTEGER, DIMENSION(10) :: Y
        DO k=1,10,1
        Y(k)=1
        END DO
        x=numero
	j=1
	i=2
	DO WHILE (i<=x)
        IF (MOD(numero,i)==0) THEN
		DO WHILE (MOD(numero,i)==0)
			numero=numero/i
			Y(j)=i
			j=j+1
		END DO
	ELSE
	i=i+1
	END IF
	END DO
        END FUNCTION divisores

	END PROGRAM mcd
