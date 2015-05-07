        PROGRAM idempotente
        INTEGER,ALLOCATABLE,DIMENSION(:,:) :: matriz,matriz2
        INTEGER :: orden
        LOGICAL :: resultado
        PRINT*,"Introduce el orden de su matriz cuadrada"
        READ*,orden
        ALLOCATE(matriz(1:orden,1:orden),matriz2(1:orden,1:orden))
        PRINT*,"Introduzca su matriz"
        READ*,matriz
        resultado=.TRUE.
        
        matriz2=multiplicar(matriz,orden)
        resultado=probar(matriz,matriz2,orden)
        PRINT*,matriz2
        PRINT*,matriz
        
        IF (resultado .eqv. .TRUE.) THEN
        PRINT*,"Son idempotentes"
        ELSE
        PRINT*,"No lo son"
        END IF
        
        READ*,
        CONTAINS
        
        FUNCTION multiplicar(m1,n) RESULT(m2)
        INTEGER,DIMENSION(:,:) :: m1
        INTEGER,ALLOCATABLE,DIMENSION(:,:) :: m2
        INTEGER :: n
        ALLOCATE(m2(1:n,1:n))
        DO i=1,n
           DO j=1,n
           m2(i,j)=m1(i,j)*m1(j,i)
           END DO
        END DO
        END FUNCTION
        
        FUNCTION probar(mat1,mat2,n) RESULT(res)
        INTEGER,DIMENSION(:,:) :: mat1,mat2
        LOGICAL :: res
        INTEGER :: n,i,j
        i=1
        res=.TRUE.
        DO WHILE (i<=n)
        j=1
             DO WHILE (j<=n)
             IF (mat1(i,j) /= mat2(i,j)) THEN
             res=.FALSE.
             END IF
             j=j+1
             END DO
        i=i+1
        END DO

        
        END FUNCTION
        END PROGRAM idempotente
