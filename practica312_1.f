	PROGRAM practica312_1

        CALL Leer_datos
        
        CONTAINS
        
        SUBROUTINE Leer_datos
        INTEGER :: opcion,mult,min
	INTEGER,DIMENSION(10) :: vector
	vector=0
	DO WHILE(opcion/=6)
	opcion=0
	PRINT*,""
	PRINT*,"1. A¤adir elemento"
	PRINT*,"2. Eliminar elemento"
	PRINT*,"3. Mostrar vector"
	PRINT*,"4. Numero de multiplos del 3"
	PRINT*,"5. Minimo elemento"
	PRINT*,"6. Terminar"
        PRINT*,"OPCION"
	READ*,opcion
	SELECT CASE (opcion)
	CASE(1)
	vector=anadir(vector)
	CASE(2)
	vector=eliminar(vector)
	CASE(3)
	PRINT*,vector
	CASE(4)
        mult=multiplos(vector)
        PRINT*,"Existen",mult,"multiplos del 3"
	CASE(5)
	min=minimo(vector)
	PRINT*,"El valor minimo es",min
	CASE(6)
        CASE DEFAULT
        PRINT*,"El valor introducido no es valido"
	END SELECT
	END DO

        END SUBROUTINE
        
	!FUNCION ANADIR

	FUNCTION anadir(v) RESULT(w)
	INTEGER,DIMENSION(10) :: v,w
	INTEGER :: k,cond1

	cond1=lleno(v)
	IF (cond1 == 1) THEN
	PRINT*,"El vector esta lleno"
	ELSE
	k=1
	DO WHILE (v(k)/=0)
	k=k+1
	END DO
	PRINT*,"Inserte el numero"
	READ*,v(k)
	END IF
	w=v
	END FUNCTION

	!FUNCION ELIMINAR

	FUNCTION eliminar(v) RESULT(w)
	INTEGER,DIMENSION(10) :: v,w
	INTEGER :: n,i,cond2
	cond2=lleno(v)
	w=0
	IF(cond2 /= 0) THEN
	PRINT*,"Inserte el elemento a eliminar"
	READ*,n
	i=1
	IF(i<=n) THEN
	DO i=1,n-1
	w(i)=v(i)
	END DO
	DO i=n+1,10
	w(i-1)=v(i)
	END DO
	END IF
	ELSE
	PRINT*,"El vector esta vacio"

	END IF
	v=w
	END FUNCTION

	!FUNCION MULTIPLOS

	FUNCTION multiplos(v) RESULT(num)
	INTEGER :: v(:)
	INTEGER :: c,num
	num=0
	DO c=1,10
	IF(MOD(v(c),3)==0 .AND. v(c)/=0) THEN
	num=num+1
	END IF
	END DO
	END FUNCTION

        !FUNCION MINIMO
        FUNCTION minimo(v) RESULT(num)
        INTEGER :: v(:)
        INTEGER :: d,num
        num=v(1)
        DO d=2,10
           IF(num > v(d)) THEN
           num=v(d)
           END IF
        END DO
        END FUNCTION
	!FUNCION LLENO

	FUNCTION lleno(x) RESULT(llen)
	INTEGER :: x(:)
	INTEGER :: llen
	llen=3
	IF (x(10)/=0) THEN
	llen=1
	ELSE IF (x(1)==0) THEN
	llen=0
	END IF

	END FUNCTION

	END PROGRAM practica312_1
