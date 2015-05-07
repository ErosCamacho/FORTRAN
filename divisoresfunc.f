        PROGRAM divisoresfunc

	INTEGER :: numero,div,i,u

	PRINT*,"Introduzca un numero"
	READ*,numero

	div=divisores(numero)

        READ*,
	CONTAINS
  	FUNCTION primo(x) RESULT (y)

	INTEGER :: x,i,j,u
	LOGICAL :: y


		DO i=2,x/2,1
		!print*,"i",i
			y=.TRUE.
			IF(MOD(x,i)==0) THEN
			y=.FALSE.
			END IF
			IF(MOD(x,i)==0) EXIT
                          !print*,"y",y
		END DO

	END FUNCTION

	FUNCTION divisores(a) RESULT (b)

	INTEGER :: a,b,k
	LOGICAL :: m



		DO k=2,a,1
		m=.FALSE.
		!print*,"k2",k
                !print*,"m2",m
		IF(MOD(a,k)==0) THEN
		m=primo(k)
		!print*,"m",m
		END IF

		IF(m .eqv. .TRUE.) THEN
		PRINT*,k
		END IF

		END DO


	END FUNCTION

	END PROGRAM divisoresfunc
