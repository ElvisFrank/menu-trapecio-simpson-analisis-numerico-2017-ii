!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!			ELVIS FRANK DOMINGUEZ VIDAL						!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!					contenedor de funciones					!
module funciones
	
	save

	contains

	function f(x)
		real,intent(in)::x
		real::f
		f=cos(x)
	end function f

end module funciones

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!					contenedor de metodos					!
module subrutinas
	!USE, intrinsic :: iso_fortran_env, ONLY: WP => REAL64
	use funciones

	implicit none
	
	save

	contains

	Subroutine trapecio

		integer::n,j
		real::a,b,h,I,s,olgura,min,max,x1,x2,A1,A2
		!real,dimension(10000)::x
		print*,'ingrese el extremo izquierdo a:'
		read(*,*)a
		print*,'ingrese el extremo derecho b:'
		read(*,*)b
		print*,'ingrese el numero de particiones n:'
		read(*,*)n
		h=abs(a-b)/n
		I=0;
		do j=1,n
			x1=a+((j-1)*h)
			x2=a+((j)*h)

			s=(f(x2))-(f(x1))

			A1=((x2-x1)*(f(x1)))
			A2=(A1)+((x2-x1)*(h/2))

			I=I+A2;
		end do
		print*,'El valor de la intgral es I=',I

		call guardarMaxima(a,b,n)

	End Subroutine trapecio
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	Subroutine simpson
		!!!*** Este programa calcula la integral de una funcion usando el metodo del simpson*
		integer::n,j
		real::a,b,h,I,s1,s2
		real,dimension(1000)::x
		print*,'ingrese el extremo izquierdo a:'
		read(*,*)a
		print*,'ingrese el extremo derecho b:'
		read(*,*)b
		print*,'ingrese el numero de particiones n:'
		read(*,*)n
		h=(b-a)/n
		!x(0)=a
		!x(n)=b
		s1=0
		s2=0
		do j=1,n-1
			if (mod(j,2)==0)then
				x(j)=a+j*h
				s1=s1+f(x(j))
			else
				x(j)=a+j*h
				s2=s2+f(x(j))
			end if
		end do
		I=(f(a)+2*s1+4*s2+f(b))*h/3
		print*,'El valor de la integral es I:',I

		call guardarMaxima(a,b,n)
	End Subroutine simpson

	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	Subroutine guardarMaxima(a,b,n)

		real,intent(inout)::a,b
		integer,intent(inout)::n
		real::min,max,h,olgura
		integer::j

		olgura=abs(a-b)
		h=olgura/n

		max=b+abs(0.1*olgura)
		min=a-abs(0.1*olgura)

		open(unit=1,file='graficaWxMaxima.wxm')
			write(1,200)'/* [wxMaxima batch file version 1] [ DO NOT EDIT BY HAND! ]*/'
			write(1,200)'/* [ Created with wxMaxima version 16.04.2 ] */'
			write(1,200)
			write(1,200)'/* [wxMaxima: input   start ] */'
			
			write(1,*) 'wxdraw2d(color=blue,'
			write(1,*)'explicit(cos(x), x,',min,',',max,'),'

			write(1,*)'head_both   = false,'
			write(1,*)'head_length = 0.000001,'
			write(1,*)'line_width = 0.63,'
			write(1,*)'head_angle = 1,'
			write(1,*)'head_type = nofilled,'
			write(1,*)'line_type = solid,'
			write(1,*)'color = orange,'
			do j=0,n
				write(1,*)'vector([',a+abs(j*h),',0],[0,',f(a+abs(j*h)),']),'				
			end do

			write(1,*)'color=red,'
			write(1,*)'head_both   = true,'
			write(1,*)'vector([',a,', 0],[',olgura,', 0]),'

			write(1,*)'grid=true,'
			write(1,*) 'xlabel="X",'
			write(1,*) 'ylabel="Y",'
			write(1,*) 'title="Simpson - Trapecio");'
			write(1,200)'/* [wxMaxima: input   end   ] */'
			write(1,200)
			write(1,200)'/* Maxima cant load/batch files which end with a comment! */'
			write(1,200)'Created with wxMaxima"$'
		close(1)

		200 FORMAT(T1,A)

	End Subroutine guardarMaxima

end module subrutinas


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!						menu principal						!
program menuTrapecioSimpson
	use subrutinas
	use funciones

	implicit none
	integer:: opc
	character:: cont

	156 continue
	print*, '|----------------------------------------------|'
	print*, '     SELECIONE UNA DE LAS OPCIONES'
	print*, '|----------------------------------------------|'
	print*, '	1 -PROGRAMA TRAPECIO'
	print*, '	2 -PROGRAMA SIMPSON'
	print*, '|----------------------------------------------|'
	read*, opc

	select case(opc)
		case(1)
			call trapecio
		case(2)
			call simpson
		case default
		print*, '¡Opcion invalida intente nuevamente..!'
		goto 156

	End select
	print*, '|----------------------------------------------|'
	print*, 'Presione s para continuar'
	Print*, ''
	read*, cont
	if (cont=='s')then
		goto 156
	End if

end program menuTrapecioSimpson
