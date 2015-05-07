        PROGRAM derivada2

        REAL (KIND=16) :: x,h,m


        PRINT*,"Punto"
        READ*,x

        h=0.00000000000000001
        
        
        m=(((x+h)**2)-(x**2))*(1/h)
        
        PRINT*,m
        
        
        READ*,
        
        END PROGRAM derivada2
