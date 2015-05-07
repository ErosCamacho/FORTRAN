
	PROGRAM practica1012_1
	TYPE registro
	CHARACTER (40) :: apellidos
	CHARACTER (10) :: documento
	CHARACTER (8) :: fecha
	END TYPE registro

	TYPE(registro),ALLOCATABLE :: final(:)
        TYPE(registro),ALLOCATABLE :: datos(:)
	INTEGER :: dim,e,p,q,contador,opcion
	!MENU
	DO WHILE (e/=4)
	PRINT*,""
	PRINT*,"1)Introducir datos iniciales"
	PRINT*,"2)Modificar datos"
	PRINT*,"3)Ordenar datos"
	PRINT*,"4)SALIR"
	PRINT*,""
	READ*,e
	SELECT CASE (e)
		CASE(1)
			CALL Leer_datos
		CASE(2)
                        DO q=1,dim
			PRINT*,final(q)
			END DO
			PRINT*,"1.+) A¤adir datos"
			PRINT*,"2.-) Eliminar datos"
			PRINT*,"3.ATRAS"
			READ*,opcion
                        IF(opcion==1) THEN
                        i=i+1
                        CALL Introducir
                        ELSE IF (opcion==2) THEN
                        CALL Borrar
                        END IF
			
		CASE(3)
                       PRINT*,"1.Ordenar datos por orden alfabetico"
                       PRINT*,"2.Ordenar datos por fecha de nacimiento"
                       PRINT*,"3.ATRAS"
                       READ*,opcion
                       IF(opcion==1) THEN
		       final=Ord_alf(final,dim)
		       ELSE IF (opcion==2) THEN
		       final=Ord_fecha(final,dim)
		       END IF

	END SELECT
	END DO


        READ*,

	CONTAINS
	
	SUBROUTINE Borrar
	TYPE(registro),ALLOCATABLE :: reg4(:)
	INTEGER :: eliminar
	PRINT*,"Seleccione el elemento a eliminar"
        DO j=1,dim
        PRINT*,j,final(j)
        END DO
	
	READ*,eliminar
	
	dim=dim-1
	
	ALLOCATE(reg4(dim))
	
	
	k=1
	j=1
	DO WHILE(k<=dim+1)
	IF (eliminar/=k) THEN
	reg4(j)=final(k)
	j=j+1
	END IF
	
	k=k+1
	END DO
	
	DO j=1,dim
	PRINT*,reg4(j)
	END DO

        DEALLOCATE(final)
	ALLOCATE(final(dim))
	
        final=reg4
	
	END SUBROUTINE
	
	SUBROUTINE Introducir
	TYPE(registro),ALLOCATABLE :: reg5(:)
	dim=dim+1

	ALLOCATE(reg5(dim-1))
	
	DO j=1,dim-1
	reg5(j)=final(j)
	END DO

        DEALLOCATE(final)
	ALLOCATE(final(dim))

	DO j=1,dim-1
	final(j)=reg5(j)
	END DO
	

                PRINT*,"===================="
                PRINT*,"ARCHIVO NUMERO",dim
		PRINT*,"Inserte los apellidos"
		READ*,final(dim)%apellidos
		PRINT*,"Inserte el DNI"
		READ*,final(dim)%documento
		PRINT*,"Inserte la fecha de nacimiento"
		READ*,final(dim)%fecha

	END SUBROUTINE


	FUNCTION Ord_fecha(reg,n) RESULT(reg3)
	TYPE(registro),ALLOCATABLE :: reg(:)
        TYPE(registro),ALLOCATABLE :: reg3(:)
	CHARACTER(4) :: ano
	CHARACTER(2) :: dia,mes
        CHARACTER(8) :: fecha,nac
        INTEGER :: contador,n
        
        ALLOCATE(reg3(n))
        reg3=reg
        
        DO j=1,n
        ano=reg(j)%fecha(5:8)
        mes=reg(j)%fecha(3:4)
        dia=reg(j)%fecha(1:2)
        fecha=ano//mes//dia
        reg(j)%fecha=fecha
        END DO

        DO k=1,n
	contador=1
	nac=reg(1)%fecha

	DO j=1,n

	IF (nac<=reg(j)%fecha) THEN
	nac=reg(j)%fecha
	contador=j
	END IF
	END DO

	reg3(k)=reg(contador)

	DO j=1,n
	IF(nac==reg(j)%fecha) THEN
	reg(j)%fecha="000000"
	END IF
	END DO

	END DO

        reg=reg3
        
        DO j=1,n
        ano=reg(j)%fecha(1:4)
        mes=reg(j)%fecha(5:6)
        dia=reg(j)%fecha(7:8)
        fecha=dia//mes//ano
        reg(j)%fecha=fecha
        END DO

        DO j=1,n
        PRINT*,reg(j)
        END DO
	
	END FUNCTION

	FUNCTION Ord_alf(reg1,n) RESULT(reg2)
	TYPE(registro),ALLOCATABLE,DIMENSION(:) :: reg1,reg2
	CHARACTER(30) :: palabra
	INTEGER :: contador,n

        ALLOCATE(reg2(n))
        


	reg2=reg1


	DO k=1,n
	contador=1
	palabra=reg1(1)%apellidos
	DO j=1,n

	IF (palabra<=reg1(j)%apellidos) THEN
	palabra=reg1(j)%apellidos
	contador=j
	END IF
	END DO

	reg2(n-k+1)=reg1(contador)

	DO j=1,n
	IF(palabra==reg1(j)%apellidos) THEN
	reg1(j)%apellidos="000000"
	END IF
	END DO


	END DO

	DO j=1,n
	PRINT*,reg2(j)
	END DO

	END FUNCTION

	SUBROUTINE Leer_datos
	IF(e==1) THEN
	PRINT*,"Inserte el numero de registros"
	READ*,dim
        END IF
        ALLOCATE(final(dim))
	DO i=1,dim
                PRINT*,"===================="
                PRINT*,"ARCHIVO NUMERO",i
		PRINT*,"Inserte los apellidos"
		READ*,final(i)%apellidos
		PRINT*,"Inserte el DNI"
		READ*,final(i)%documento
		PRINT*,"Inserte la fecha de nacimiento"
		READ*,final(i)%fecha
	END DO


	END SUBROUTINE

	END PROGRAM practica1012_1
