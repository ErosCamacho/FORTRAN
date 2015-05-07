        PROGRAM verdia
        
	CHARACTER(10) :: ndia

	CALL nombre

	PRINT*,"Era ",ndia

        READ*,

	CONTAINS

	SUBROUTINE nombre
	INTEGER :: dia,mes,ano
	INTEGER :: codigo
	REAL :: modulo
	PRINT*,"Introduzca su fecha"
	READ*,dia
	READ*,mes
	READ*,ano
	ano=ano-1900

	ano=ano+(ano/4)+1
	ano=ano+dia

		SELECT CASE (mes)
			CASE (1)
			codigo=9
			CASE(2,3,11)
			codigo=3
			CASE(4,7)
			codigo=6
			CASE(5)
			codigo=1
			CASE(6)
			codigo=4
			CASE(8)
			codigo=2
			CASE(9,12)
			codigo=5
			CASE(10)
			codigo=0
		END SELECT

	ano=ano+codigo

	modulo=MOD(ano,7)+1

		IF (modulo==3) THEN
			ndia="Lunes"
		ELSE IF (modulo==4) THEN
			ndia="Martes"
		ELSE IF (modulo==5) THEN
			ndia="Miercoles"
		ELSE IF (modulo==6) THEN
			ndia="Jueves"
		ELSE IF (modulo==7) THEN
			ndia="Viernes"
		ELSE IF (modulo==1) THEN
			ndia="Sabado"
		ELSE IF (modulo==2) THEN
			ndia="Domingo"
		END IF


	END SUBROUTINE nombre

	END PROGRAM verdia
