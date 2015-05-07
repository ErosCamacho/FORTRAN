        PROGRAM practica54
        INTEGER,DIMENSION(1000,1000) :: matriz
        INTEGER,DIMENSION(1000) :: vector
        INTEGER :: n,min,max,vmin,vmax,numero
        REAL :: med
        CHARACTER(1) :: eleccion
        matriz=0
        vector=0
        PRINT*,"Introduzca la dimension de la matriz"
        READ*,n
        
        PRINT*,"Introduzca la matriz"
        READ*,matriz(1:n,1:n)
        
        min=minimo(matriz,n)
        PRINT*,"El valor minimo es:",min
        
        max=maximo(matriz,n)
        PRINT*,"El valor maximo es:",max
        
        med=media(matriz,n)
        PRINT*,"La media de los valores es:",med
        
        PRINT*,
        PRINT*,"Introduzca fila(f) o columna(c)"
        READ*,eleccion
        PRINT*,"Introduzca el numero de esa fila o columna"
        READ*,numero
        PRINT*,
        
        IF (eleccion .eq. "f") THEN
        DO k=1,n
        vector(k)=matriz(numero,k)
        END DO
        ELSE IF (eleccion .eq. "c") THEN
        DO k=1,n
        vector(k)=matriz(k,numero)
        END DO
        END IF
        
        PRINT*,vector
        
        vmin=vectorminimo(vector,n)
        PRINT*,"El valor minimo es:",vmin

        vmax=vectormaximo(vector,n)
        PRINT*,"El valor maximo es:",vmax
        
        
        PRINT*,"FIN"
        READ*,
        
        CONTAINS
        FUNCTION minimo(mat,dim) RESULT(num)
        INTEGER :: mat(:,:)
        INTEGER :: num,dim
        num=0
        DO i=1,dim
           DO j=1,dim
           num=num+mat(i,j)
           END DO
        END DO

        DO i=1,dim
           DO j=1,dim
               IF(mat(i,j)<num) THEN
               num=mat(i,j)
               END IF
           END DO
        END DO
        END FUNCTION
        
        FUNCTION maximo(mat,dim) RESULT(num)
        INTEGER :: mat(:,:)
        INTEGER :: num,dim
        num=0
        DO i=1,dim
           DO j=1,dim
               IF(mat(i,j)>num) THEN
               num=mat(i,j)
               END IF
           END DO
        END DO
        END FUNCTION
        
        FUNCTION media(mat,dim) RESULT(num)
        INTEGER :: mat(:,:)
        INTEGER :: dim
        REAL :: num
        num=0
        DO i=1,dim
           DO j=1,dim
           num=num+mat(i,j)
           END DO
        END DO

        num=num/(dim*dim)
        END FUNCTION
        
        FUNCTION vectorminimo(vec,dim) RESULT(num)
        INTEGER :: vec(:)
        INTEGER :: num,dim
        num=0
        DO i=1,dim
           num=num+vec(i)
        END DO

        DO i=1,dim
               IF(vec(i)<num) THEN
               num=vec(i)
               END IF
           END DO
        END FUNCTION

        FUNCTION vectormaximo(vec,dim) RESULT(num)
        INTEGER :: vec(:)
        INTEGER :: num,dim
        num=0
        DO i=1,dim
               IF(vec(i)>num) THEN
               num=vec(i)
               END IF
           END DO
        END FUNCTION
        
        
        END PROGRAM practica54
