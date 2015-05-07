        PROGRAM mcm

	INTEGER,DIMENSION(10) :: Q,W
	INTEGER :: numero1,numero2,resultado



	PRINT*,"Introduce un numero"
	READ*, numero1
	PRINT*,"Introduce otro numero"
	READ*,numero2

        Q=divisores(numero1)
        W=divisores(numero2)
        PRINT*,Q
        PRINT*,W
        resultado=minimo(Q,W)
        PRINT*,"El minimo comun multiplo es:",resultado
        
        READ*,
        
        CONTAINS

        FUNCTION minimo(A,B) RESULT(num1)
        INTEGER,DIMENSION(10) :: A,B
        INTEGER :: m,n,num,num1,l
        DO m=1,10,1
                DO n=1,10,1
		        IF(A(m) == B(n)) EXIT
		END DO
                B(n)=1
	END DO

        PRINT*,A
        PRINT*,B
        num=1
        num1=1
        DO l=1,10,1
        num=A(l)*B(l)
        num1=num1*num
        END DO
        END FUNCTION minimo
        
        FUNCTION divisores(numero) RESULT (Y)
        INTEGER :: j,i,x,numero,k
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
        
	END PROGRAM mcm
