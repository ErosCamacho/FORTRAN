        PROGRAM determinante
        INTEGER,DIMENSION(4,4) :: matriz
        INTEGER :: valor
        READ*,matriz
        valor=det4(matriz)
        DO i=1,4
        PRINT*,matriz(i,1),matriz(i,2),matriz(i,3),matriz(i,4)
        END DO
        PRINT*,valor
        READ*,


	CONTAINS
	FUNCTION det4(matriz4) RESULT(num4)
	INTEGER, DIMENSION(4,4) :: matriz4
	INTEGER,DIMENSION(3,3) :: nueva
	INTEGER :: num4,i,j,k,a,b
	nueva=0
	num4=0
        DO i=1,4
        b=1
        j=1
        
        DO WHILE (j<=4)
        IF(i==j) THEN
        j=j+1
        ELSE
            DO k=1,3
            nueva(b,k)=matriz4(j,k+1)
            END DO
        b=b+1
        j=j+1
        END IF
        END DO
        
        DO a=1,4
           IF(MOD(a,2)/=2) THEN
           suma=suma+(matriz4(a,1)*det3(nueva))
           ELSE
           suma=suma-(matriz4(a,1)*det3(nueva))
           END IF
        END DO
        END DO
	END FUNCTION
	
	
        FUNCTION det3(matriz3) RESULT(num3)
	INTEGER, DIMENSION(3,3) :: matriz3
	INTEGER :: num3,i
	num3=0
	DO i=1,3
	IF (i==1) THEN
        num3=num3+(matriz3(1,1)*det2(matriz3(2:3,2:3)))
        ELSE IF (i==2) THEN
        num3=num3-(matriz3(2,1)*det2(matriz3(1:3:2,2:3)))
        ELSE
        num3=num3+(matriz3(3,1)*det2(matriz3(1:2,2:3)))
        END IF
        END DO
	END FUNCTION

	FUNCTION det2(matriz2) RESULT(num2)
	INTEGER, DIMENSION(2,2) :: matriz2
	INTEGER :: num2,j
	num2=0
        DO j=1,2
        IF (MOD(j,2)/=0) THEN
        num2=num2+(matriz2(1,1)*matriz2(2,2))
        ELSE
        num2=num2-(matriz2(2,1)*matriz2(1,2))
        END IF
        END DO
	END FUNCTION

	END PROGRAM determinante
