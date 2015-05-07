        PROGRAM repaso1
        INTEGER :: opcion,number,digitos
        INTEGER,DIMENSION(2) :: vectornum
        CHARACTER(LEN=100):: num,num2
        
        DO WHILE (opcion/=4)
        PRINT*,""
        WRITE(*,"(A20)")"MENU"
        WRITE(*,*) "Selecciona la opcion"
        WRITE(*,*) "1) Como un dato INTEGER"
        WRITE(*,*) "2) Como un vector de dos INTEGER: numero y d¡gitos"
        WRITE(*,*) "3) Como cadena de caracteres"
        WRITE(*,*) "4) SALIR"
        READ*,opcion

        SELECT CASE (opcion)
               CASE(1)
                      PRINT*,""
                      WRITE(*,*) "Introduzca su numero"
                      READ*,number
                      number=Programa1(number)
               CASE(2)
                      PRINT *,""
                      WRITE(*,"(A20)") "Numero"
                      READ(*,"(10X,I20)") vectornum(1)
                      WRITE(*,"(A20)") "Digitos"
                      READ(*,"(10X,I20)") vectornum(2)

                      number=Programa2(vectornum)
               CASE(3)

                      WRITE(*,"(10X,A)") "","Introduzca su numero"
                      READ(*,*) num
                      num=Programa3(num)

               CASE(4)
               CASE DEFAULT
               WRITE(*,"(A,I2,A)") "El numero",opcion," no es v lido"
        END SELECT
        END DO
        
        PRINT*,"FIN"
        READ*,
        
        CONTAINS
        
        !PROGRAMA NUMERO 1
        FUNCTION Programa1(numero)
        INTEGER :: numero,numbis,long,j,i
        INTEGER,ALLOCATABLE,DIMENSION(:) :: vector
        !Con esto calculo la longitud del numero viendo el numero de
        !cifras que tiene multiplicando su divisor por 10 en cada
        !incremento, ya que si tiene 10 es que tiene una cifra mas
        i=1
        long=1
        DO WHILE ((numero/i)>=10)
        long=long+1
        i=i*10
        END DO
        
        !Creo un vector de longitud la del numero
        
        ALLOCATE(vector(long))

        !En cada comp del vector meto el numero dividido por el cociente
        !que obtengo de la i, como cada vez se lo he ido quitando me
        !sale el numero separado perfectamente.
        
        numbis=numero
        DO j=1,long
        vector(j)=numbis/i
        numbis=numbis-vector(j)*i
        i=i/10
        END DO

        !Voy a calcular el valor de las componentes del vector elevadas
        !a la longitud del numero
        numbis=0
        DO j=1,long
        numbis=numbis+(vector(j)**long)
        END DO

        IF (numbis==numero) THEN
        WRITE(*,*) "El numero ES narcisista"
        ELSE
        WRITE(*,*) "El numero NO ES narcisista"
        END IF
        
        END FUNCTION Programa1
        
        
        
        !PROGRAMA NUMERO 2
        FUNCTION Programa2(vecnumero)
        INTEGER,DIMENSION(2) :: vecnumero
        INTEGER :: numbis,j,i
        INTEGER,ALLOCATABLE,DIMENSION(:) :: vector

        ALLOCATE(vector(vecnumero(2)))

        numbis=vecnumero(1)

        i=10**(vecnumero(2)-1)

        DO j=1,vecnumero(2)
        vector(j)=numbis/i
        numbis=numbis-vector(j)*i
        i=i/10
        END DO
        
        numbis=0
        DO j=1,vecnumero(2)
        numbis=numbis+(vector(j)**vecnumero(2))
        END DO

        IF (numbis==vecnumero(1)) THEN
        WRITE(*,*) "El numero ES narcisista"
        ELSE
        WRITE(*,*) "El numero NO ES narcisista"
        END IF

        END FUNCTION Programa2
        
        
        
        
        
        !PROGRAMA NUMERO 3
        FUNCTION Programa3(num) RESULT(n)
        CHARACTER(LEN=100) :: num,n
        INTEGER:: num2,digitos,num3
        INTEGER,ALLOCATABLE :: datos(:)
        
        digitos=LEN_TRIM(num)
        ALLOCATE(datos(digitos))
        
        !De aqui saco los valores en formato caracter
        digitos=0
        i=0
        DO WHILE (digitos<=LEN_TRIM(num))
        i=i+1
        num2=0
        num2=INDEX(num,ACHAR(i))
        IF (num2/=0) THEN
        digitos=digitos+1
        datos(num2)=i-48

        END IF
        END DO

        PRINT*,datos
        !Voy a formar el entero
        
        digitos=LEN_TRIM(num)
        i=10**(digitos-1)
        num2=0
        DO j=1,digitos
        num2=num2+(datos(j)*i)
        i=i/10
        END DO
        
        PRINT*,num2
        
        num3=0
        !Voy a comprobar la condicion
        DO j=1,digitos
        num3=num3+(datos(j)**digitos)
        END DO
        
        IF (num2==num3) THEN
        WRITE(*,*) "El numero ES narcisista"
        ELSE
        WRITE(*,*) "El numero NO ES narcisista"
        END IF

        END FUNCTION Programa3
        
        END PROGRAM repaso1
