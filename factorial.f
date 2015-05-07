        PROGRAM factorial
        
        INTEGER :: num,total
        
        PRINT*,"Introduce un numero"
        READ*,num
        
        total=fact(num)
        
        WRITE(*,*)"El factorial de:",num,"es",total

        CONTAINS
        
        FUNCTION fact(x) RESULT(sum)
        INTEGER :: x,sum
        sum=1
        DO i=x,1,-1
        sum=sum*x
        END DO
        END FUNCTION fact
        
        END PROGRAM factorial
