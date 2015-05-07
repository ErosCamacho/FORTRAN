        PROGRAM practica53
        INTEGER,DIMENSION(1000,1000)::m1,m2,res1
        INTEGER :: f1,c1,f2,c2,opcion,u
        m1=0
        m2=0
        
        DO
        !MENU PRINCIPAL
        PRINT*,"OPERACIONES CON MATRICES"
        PRINT*,
        PRINT*,"Selecciona la opcion que desea:"
        PRINT*,
        PRINT*,"1) Suma de matrices"
        PRINT*,"2) Multiplicaci¢n de matrices"
        PRINT*,"3) Multiplicar sus traspuestas"
        PRINT*,"4) SALIR"
        PRINT*,
        READ*,opcion
        
        !SUMA DE MATRICES
        DO WHILE (opcion==1)
           u=0
           CALL Introducir_matrices
           
           IF(f1==f2 .AND. c1==c2) THEN
           res1(1:f1,1:c1)=suma(m1(1:f1,1:c1),m2(1:f2,1:c2),f1,c1)
                PRINT*,res1(1:f1,1:c1)
                PRINT*,"Numero de filas:",f1
                PRINT*,"Numero de columnas:",c1
                PRINT*,
                u=1
           ELSE
               PRINT*,"Las matrices introducidas no son v lidas"
               PRINT*,"Introduzcalas de nuevo"
               PRINT*,"PULSA ENTER"
               READ*,
           END IF
           
           IF(u==1) EXIT
           
        END DO
        
        !MULTIPLICACION DE MATRICES
        DO WHILE (opcion==2)
           u=0
           CALL Introducir_matrices

           IF(c1==f2) THEN
           res1(1:f1,1:c2)=mult(m1(1:f1,1:c1),m2(1:f2,1:c2),f1,c1,c2)
                PRINT*,res1(1:f1,1:c2)
                PRINT*,"Numero de filas:",f1
                PRINT*,"Numero de columnas:",c2
                PRINT*,
                u=1
           ELSE
               PRINT*,"Las matrices introducidas no son v lidas"
               PRINT*,"Introduzcalas de nuevo"
               PRINT*,"PULSA ENTER"
               READ*,
           END IF

           IF(u==1) EXIT

        END DO
        
        !MULTIPLICACION DE TRASPUESTAS
        DO WHILE (opcion==3)
           u=0
           CALL Introducir_matrices

           IF(f1==c2) THEN
           res1(1:c1,1:f2)=tras(m1(1:f1,1:c1),m2(1:f2,1:c2),f1,c1,f2,c2)
                PRINT*,res1(1:c1,1:f2)
                PRINT*,"Numero de filas:",c1
                PRINT*,"Numero de columnas:",f2
                PRINT*,
                u=1
           ELSE
               PRINT*,"Las matrices introducidas no son v lidas"
               PRINT*,"Introduzcalas de nuevo"
               PRINT*,"PULSA ENTER"
               READ*,
           END IF

           IF(u==1) EXIT

        END DO
        
        !SALIDA
        IF (opcion==4) EXIT
        
        END DO
        PRINT*,"FIN"
        READ*,
        
        
        CONTAINS
        SUBROUTINE Introducir_matrices
        PRINT*,"Introduce la primera matriz"
        PRINT*,"Numero de filas"
        READ*,f1
        PRINT*,"Numero de columnas"
        READ*,c1
        PRINT*,"Introducir matriz"
        READ*,m1(1:f1,1:c1)

        PRINT*,"Introduce la segunda matriz"
        PRINT*,"Numero de filas"
        READ*,f2
        PRINT*,"Numero de columnas"
        READ*,c2
        PRINT*,"Introducir matriz"
        READ*,m2(1:f2,1:c2)
        END SUBROUTINE
        
        !FUNCION SUMA
        FUNCTION suma(matriz1,matriz2,f,c) RESULT(mres1)
        INTEGER :: matriz1(:,:)
        INTEGER :: matriz2(:,:)
        INTEGER :: f,c,i,j
        INTEGER,DIMENSION(1000,1000) :: mres1
        mres1=0
        
        DO i=1,f
           DO j=1,c
           mres1(i,j)=matriz1(i,j)+matriz2(i,j)
           END DO
        END DO
        END FUNCTION
        
        !FUNCION MULTIPLICACION
        FUNCTION mult(matriz1,matriz2,fi1,co1,co2) RESULT(mres2)
        INTEGER :: matriz1(:,:)
        INTEGER :: matriz2(:,:)
        INTEGER :: fi1,co1,co2,i,j,k,l
        INTEGER,DIMENSION(1000,1000) :: mres2
        mres2=0

        DO i=1,fi1
           DO j=1,co2
              DO k=1,co1
              mres2(i,j)=mres2(i,j)+(matriz1(i,k)*matriz2(k,j))
              END DO
           END DO
        END DO

        END FUNCTION
        
        !FUNCION TRASPUESTA Y MULTIPLICACION
       FUNCTION tras(matriz1,matriz2,f1,c1,f2,c2) RESULT(mres3)
        INTEGER :: matriz1(:,:)
        INTEGER :: matriz2(:,:)
        INTEGER, DIMENSION(1000,1000) :: mn1,mn2
        INTEGER :: f1,c1,f2,c2,i,j
        INTEGER,DIMENSION(1000,1000) :: mres3
        mres3=0
        
        DO i=1,f1
           DO j=1,c1
              mn1(j,i)=matriz1(i,j)
           END DO
        END DO
        DO i=1,f2
           DO j=1,c2
              mn2(j,i)=matriz2(i,j)
           END DO
        END DO
       mres3(1:c1,1:f2)=mult(mn1(1:c1,1:f1),mn2(1:c2,1:f2),c1,f1,f2)

        
       END FUNCTION

        END PROGRAM practica53
