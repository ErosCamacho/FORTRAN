        PROGRAM practica7_2
        CHARACTER,ALLOCATABLE :: cadena1(:),cadena2(:)
        INTEGER :: dim1,dim2,contador
        
        PRINT*,"Introduce la dimension de la cadena1"
        READ*,dim1
        ALLOCATE(cadena1(1))
        LEN(cadena1)=dim1
        PRINT*,"Introduce la cadena"
        READ*,cadena1
        
        PRINT*,"Introduce la dimension de la cadena2"
        READ*,dim2
        ALLOCATE(cadena2(1))
        LEN_TRIM(cadena2)=dim2
        PRINT*,"Introduce la cadena2"
        READ*,cadena2

        IF(dim1>=dim2) THEN
        DO i=1,dim1
        contador=0
           DO j=1,dim2
           IF(cadena1(i)==cadena2(j)) THEN
           contador=contador+1
           END IF
           END DO
           IF(contador/=0) THEN
           PRINT*,cadena1(i),contador
           END IF
        END DO
        ELSE IF(dim1<dim2) THEN

        END IF

        
        
        END PROGRAM practica7_2
