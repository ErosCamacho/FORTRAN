        PROGRAM repaso2
        INTEGER,ALLOCATABLE :: matriz(:,:),vector(:)
        INTEGER,DIMENSION(2) :: dimen
        INTEGER :: u,d
        
        WRITE(*,*) "Introduzca la dimensi¢n de su matriz MxN"
        READ(*,*) dimen
        
        d=dimen(1)/2
        ALLOCATE(matriz(dimen(1),dimen(2)))
        ALLOCATE(vector(d))
        
        WRITE(*,*) "Introduzca su matriz"
        READ(*,*) matriz

        DO i=1,dimen(1)
        WRITE(*,*) matriz(i,:)
        END DO
        
        DO i=2,dimen(1),2
        u=i/2
        vector(u)=maximo(matriz(i,:),dimen(2))
        END DO

        u=0
        DO i=1,d
           IF (MOD(vector(i),2)/=0) THEN
           u=u+vector(i)
           END IF
        END DO
        
        WRITE(*,"(A,I5)") "El valor de la suma es:",u
        
        PRINT*,"FIN"
        READ*,
        
        CONTAINS
        FUNCTION maximo(vector,dimen) RESULT(max)
        INTEGER :: vector(:),dimen,max,i
        
        max=vector(1)
        
        DO i=2,dimen
           IF (max<=vector(i)) THEN
           max=vector(i)
           END IF
        END DO
        
        END FUNCTION
        
        END PROGRAM repaso2
