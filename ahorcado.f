	PROGRAM ahorcado

	CHARACTER (30) :: palabra,respuesta
	CHARACTER (1) :: letra
	INTEGER :: intentos,num,i,j,longitud,k,p,acertar

	PRINT*, "Introduzca palabra"
	READ*, palabra
	PRINT*, "Numero de intentos"
	READ*, intentos

        longitud=LEN(TRIM(palabra))

        DO j=1,30,1
        IF(j<=longitud)THEN
        respuesta(j:j)="-"
        ELSE
        respuesta(j:j)=" "
        END IF
        END DO

        PRINT*,respuesta
        
        i=1
        acertar=0
	DO WHILE (i<=intentos)
	PRINT*, "INTRODUZCA UNA LETRA"
	READ*, letra

	IF (INDEX(palabra,letra)/=0) THEN
	       DO k=1,longitud,1
               IF (palabra(k:k) .eq. letra ) THEN
               respuesta(k:k)=letra
               acertar=acertar+1
               END IF
               END DO
               PRINT*,respuesta
        ELSE
        i=i+1
        END IF
        
        IF(acertar==longitud) THEN
        PRINT*,"­HAS ACERTADO LA PALABRA!"
        END IF
        
        IF(acertar==longitud) EXIT
        
	END DO


        READ*,



	END PROGRAM
