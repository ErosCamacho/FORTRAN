	PROGRAM mcd

	INTEGER,DIMENSION(10) :: A,B,C
	INTEGER :: numero1,numero2,k,i,j,y,z
	INTEGER :: m,n,num,num1,l,o



	PRINT*,"Introduce un numero"
	READ*, numero1
	PRINT*,"Introduce otro numero"
	READ*,numero2

	y=numero1
	z=numero2

	DO k=1,10,1
	A(k)=1
	B(k)=1
	C(k)=1
	END DO

	!PRINT*,A


	j=1
	i=2

	DO WHILE (i<=y)

	IF (MOD(numero1,i)==0) THEN
		DO WHILE (MOD(numero1,i)==0)
			numero1=numero1/i
			A(j)=i
			j=j+1
		END DO
	ELSE
	i=i+1
	END IF
	END DO

	i=2
	j=1

	DO WHILE (i<=z)

	IF (MOD(numero2,i)==0) THEN
		DO WHILE (MOD(numero2,i)==0)
			numero2=numero2/i
			B(j)=i
			j=j+1
		END DO
	ELSE
	i=i+1
	END IF
	END DO

	PRINT*,A
	PRINT*,B

	num=1

	DO m=1,10,1

	!IF(A(m)/=num) THEN

		DO n=1,10,1
		IF(A(m) == B(n) .AND. A(m)/=1 .AND. B(n)/=1) THEN
		C(m)=A(m)
		END IF
		IF(A(m) == B(n)) EXIT
		END DO
		!m=m+1
	!ELSE
	!m=m+1
	!END IF
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

	PRINT*,C
	PRINT*,num2

	END PROGRAM mcd
