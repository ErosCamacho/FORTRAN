        PROGRAM repaso3
        INTEGER,ALLOCATABLE :: numeros(:)
        INTEGER :: max
        
        WRITE(*,*) "Introduzca el valor maximo de numeros"
        READ*,max
        
        ALLOCATE(numeros(max-1))
        
        DO i=2,max
        numeros(i-1)=i
        END DO
        
        numeros=primos(numeros,max)
        
        PRINT*,"FIN"
        READ*,
        
        CONTAINS
        
        FUNCTION primos(vector,max)
        INTEGER :: vector(:)
        INTEGER :: i,max
        
        DO i=1,max-1
        IF (vector(i)/=0) THEN
           DO j=1,max-1
        IF(i/=j.AND.vector(j)/=0.AND.MOD(vector(j),vector(i))==0) THEN
        vector(j)=0
        END IF
            END DO
        END IF
        END DO

        DO i=1,max-1
        IF(vector(i)/=0) THEN
        WRITE(*,*) vector(i)
        END IF
        END DO
        END FUNCTION
        
        END PROGRAM repaso3
