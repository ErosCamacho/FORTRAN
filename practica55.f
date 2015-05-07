        PROGRAM practica55
        INTEGER,DIMENSION(2008:2010,1:12,1:4) :: datos
        INTEGER,DIMENSION(1000) :: valor
        INTEGER :: u,ano,mes,ciudad
        REAL :: media
        CHARACTER(1) :: respuesta
        DO i=2008,2010
          DO j=1,12
             DO k=1,4
             datos(i,j,k)=rand()*rand()*20*j*k
             END DO
          END DO
        END DO
        
        PRINT*,"Ciudad 1"
        DO i=2008,2010
           DO j=1,12
        PRINT*,"A¤o",i,"Mes",j,"Valor",datos(i,j,1)
        END DO
        END DO
        
        PRINT*,"Ciudad 2"
        DO i=2008,2010
           DO j=1,12
        PRINT*,"A¤o",i,"Mes",j,"Valor",datos(i,j,2)
        END DO
        END DO
        
        PRINT*,"Ciudad 3"
        DO i=2008,2010
           DO j=1,12
        PRINT*,"A¤o",i,"Mes",j,"Valor",datos(i,j,3)
        END DO
        END DO
        
        PRINT*,"Ciudad 4"
        DO i=2008,2010
           DO j=1,12
        PRINT*,"A¤o",i,"Mes",j,"Valor",datos(i,j,4)
        END DO
        END DO
        
        PRINT*,"Calcular la media de los datos que usted elija"
        u=1
        media=0
        DO
        PRINT*,"Introduzca a¤o"
        READ*,ano
        PRINT*,"Introduzca mes"
        READ*,mes
        PRINT*,"Introduzca ciudad"
        READ*,ciudad

        media=media+datos(ano,mes,ciudad)
        
        PRINT*,"¨Desea finalizar? Si(S) , No(N)?"
        READ*,respuesta
        
        IF(respuesta .eq. "S") THEN
        media=media/u
        EXIT
        ELSE
        u=u+1
        END IF
        END DO
        
        PRINT*,"La media de los valores escogidos es:",media
        
        CALL minimo
        CALL maximo
        
        READ*,
        
        CONTAINS
        SUBROUTINE minimo
        INTEGER :: num,i,j,k,a,b,c,d,e,f
        num=0
        DO i=2008,2010
           DO j=1,12
              DO k=1,4
           num=num+datos(i,j,k)
              END DO
           END DO
        END DO

        DO a=2008,2010
           DO b=1,12
              DO c=1,4
               IF(datos(a,b,c)<num) THEN
               num=datos(a,b,c)
               END IF
              END DO
           END DO
        END DO
        
        PRINT*,"El minimo valor:",num
        PRINT*,"Se alcanzo"
        DO d=2008,2010
           DO e=1,12
              DO f=1,4
               IF(datos(d,e,f)==num) THEN
               PRINT*,"A¤o:",d
               PRINT*,"Mes:",e
               PRINT*,"Ciudad:",f
               PRINT*,
               END IF
              END DO
           END DO
        END DO
        
        END SUBROUTINE

        SUBROUTINE maximo
        INTEGER :: num,i,j,k,a,b,c
        num=0
        DO i=2008,2010
           DO j=1,12
              DO k=1,4
               IF(datos(i,j,k)>num) THEN
               num=datos(i,j,k)
               END IF
              END DO
           END DO
        END DO
        
        PRINT*,"El maximo valor:",num
        PRINT*,"Se alcanzo"
        DO a=2008,2010
           DO b=1,12
              DO c=1,4
               IF(datos(a,b,c)==num) THEN
               PRINT*,"A¤o:",a
               PRINT*,"Mes:",b
               PRINT*,"Ciudad:",c
               PRINT*,
               END IF
              END DO
           END DO
        END DO
        END SUBROUTINE
        
        END PROGRAM practica55
