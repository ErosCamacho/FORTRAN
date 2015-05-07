	PROGRAM practica52
	INTEGER :: matr	(1000,1000)
	LOGICAL :: resultado
	matr=0

	PRINT*,"Inserte el numero de filas de su matriz"
	READ*,n

	PRINT*,"Inserte la matriz"
	READ*,matr(1:n,1:n)

	resultado=curiosa(matr(1:n,1:n),n)

	IF (resultado .eqv. .TRUE.) THEN
	PRINT*,"Es curiosa"
	ELSE
	PRINT*,"No es curiosa"
	END IF

        PRINT*,"FIN"
        READ*,

	CONTAINS
	FUNCTION curiosa(matriz,d) RESULT(valor)
	INTEGER :: matriz(:,:)
	INTEGER, DIMENSION(1000) :: vector
	INTEGER :: i,j,k,d,a,b
	LOGICAL :: valor
	vector=0
	DO i=1,d
		DO k=1,d
		vector(i)=vector(i)+matriz(k,i)
		END DO
	END DO
	j=1
	DO i=d+1,2*d
		DO k=1,d
		vector(i)=vector(i)+matriz(j,k)
		END DO
	j=j+1
	END DO

	valor=.FALSE.
	DO a=1,2*d,1
		DO b=1,2*d,1
		IF(vector(a)==vector(b)) THEN
		valor=.TRUE.
		END IF
		END DO
	END DO
	END FUNCTION

	END PROGRAM practica52
