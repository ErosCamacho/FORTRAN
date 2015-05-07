 	PROGRAM practica24

	CHARACTER(1) :: letra
	REAL :: num,compra
	INTEGER :: a,b,c,d,e,f
	INTEGER :: g,h,i,j,k,l
	REAL :: resto,resto1,resto2,resto3,resto4,resto5,resto6
	REAL :: resto7,resto8,resto9,resto10,resto11,resto12

	num=0

	DO
	PRINT*, "Introduzca una cantidad a depositar"
	READ*, letra
		IF(letra .eq. "A") THEN
		num = num + 50
		PRINT*, num

		ELSE IF(letra .eq. "B") THEN
		num = num + 20
		PRINT*, num

		ELSE IF(letra .eq. "C") THEN
		num = num + 10
		PRINT*, num

		ELSE IF(letra .eq. "D") THEN
		num = num + 5
		PRINT*, num

		ELSE IF(letra .eq. "E") THEN
		num = num + 2
		PRINT*, num

		ELSE IF(letra .eq. "F") THEN
		num = num + 1
		PRINT*, num

		ELSE IF(letra .eq. "G") THEN
		num = num + 0.50
		PRINT*, num

		ELSE IF(letra .eq. "H") THEN
		num = num + 0.20
		PRINT*, num

		ELSE IF(letra .eq. "I") THEN
		num = num + 0.10
		PRINT*, num

		ELSE IF(letra .eq. "J") THEN
		num = num + 0.050
		PRINT*, num

		ELSE IF(letra .eq. "K") THEN
		num = num + 0.020
		PRINT*, num

		ELSE IF(letra .eq. "L") THEN
		num = num + 0.010
		PRINT*, num

		ELSE IF(letra .ne. "Z") THEN
		PRINT *, "La letra introducida no es valida"

		END IF

		IF (letra .eq. "Z") EXIT

	END DO

	WRITE (*,*) "Usted tiene un valor total de:",num,"euros."

	PRINT*, "Introduzca el valor de su compra"
	READ*, compra

	resto=num-compra

	a=resto/50.0
	resto2=mod(resto,50.0)

	b=resto2/20.0
	resto3=mod(resto2,20.0)

	c=resto3/10.0
	resto4=mod(resto3,10.0)

	d=resto4/5.0
	resto5=mod(resto4,5.0)

	e=resto5/2.0
	resto6=mod(resto5,2.0)

	f=resto6
	resto7=(resto6)*100.0

	g=resto7/50.0
	resto8=mod(resto7,50.0)

	h=resto8/20.0
	resto9=mod(resto8,20.0)

	i=resto9/10.0
	resto10=mod(resto9,10.0)

	j=resto10/5.0
	resto11=mod(resto10,5.0)

	k=resto11/2.0
	resto12=mod(resto11,2.0)

	l=resto12

	PRINT*,"Tiene que pagar", resto
	PRINT*,"Su cambio es"
	PRINT*,"Billetes de 50 euros:",a
	PRINT*,"Billetes de 20 euros:",b
	PRINT*,"Billetes de 10 euros:",c
	PRINT*,"Billetes de 5 euros:",d
	PRINT*,"Monedas de 2 euros:",e
	PRINT*,"Monedas de 1 euros:",f
	PRINT*,"Monedas de 50 centimos:",g
	PRINT*,"Monedas de 20 centimos:",h
	PRINT*,"Monedas de 10 centimos:",i
	PRINT*,"Monedas de 5 centimos:",j
	PRINT*,"Monedas de 2 centimos:",k
	PRINT*,"Monedas de 1 centimos:",l

        READ*,

	END PROGRAM practica24
