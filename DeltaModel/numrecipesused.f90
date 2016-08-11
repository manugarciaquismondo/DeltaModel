MODULE random
IMPLICIT NONE
INTEGER  SEEDS(8)
REAL(4) :: RM1=3.8580247E-6, RM2=7.4373773E-6 
INTEGER :: M1=259200,IA1=7141,IC1=54773
INTEGER :: M2=134456,IA2=8121,IC2=28411
INTEGER :: M3=243000,IA3=4561,IC3=51349
END MODULE


!-NUMERICAL-RECIPES----------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
SUBROUTINE INDEXX(N,ARRIN,INDX)
real*8 ARRIN(N)
!---changed following from 2 to 4
integer*4 INDX(N)
INTEGER i, j, n 

   DO 11 J=1,N
	 INDX(J)=J
11 CONTINUE
   L=N/2+1
   IR=N
10 CONTINUE
   IF(L.GT.1)THEN
	 L=L-1
	 INDXT=INDX(L)
	 Q=ARRIN(INDXT)
   ELSE
	 INDXT=INDX(IR)
	 Q=ARRIN(INDXT)
	 INDX(IR)=INDX(1)
	 IR=IR-1
	 IF(IR.EQ.1)THEN
	   INDX(1)=INDXT
	   RETURN
	 ENDIF
  ENDIF
  I=L
  J=L+L
20 IF(J.LE.IR)THEN
	 IF(J.LT.IR)THEN
	   IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
	 ENDIF
	 IF(Q.LT.ARRIN(INDX(J)))THEN
	   INDX(I)=INDX(J)
	   I=J
	   J=J+J
	 ELSE
	   J=IR+1
	 ENDIF
	 GO TO 20
  ENDIF
  INDX(I)=INDXT
  GO TO 10
  END

!-----------------------------------------------------------------------
real(8) FUNCTION ran11(IDUM)    !generate a uniform 0 to 1 random number
INTEGER idum
DIMENSION R(97)
INTEGER, SAVE:: IX1, IX2, IX3
INTEGER:: J
PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
PARAMETER (M2=134456,IA2=8121,IC2=28411,RM2=7.4373773E-6)
PARAMETER (M3=243000,IA3=4561,IC3=51349)
DATA IFF /0/
SAVE R

IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
	IFF=1
	IX1=MOD(IC1-IDUM,M1)
	IX1=MOD(IA1*IX1+IC1,M1)
	IX2=MOD(IX1,M2)
	IX1=MOD(IA1*IX1+IC1,M1)
	IX3=MOD(IX1,M3)
	DO 11 J=1,97
	   IX1=MOD(IA1*IX1+IC1,M1)
	   IX2=MOD(IA2*IX2+IC2,M2)
	   R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
11  CONTINUE
	IDUM=1
ENDIF

IX1=MOD(IA1*IX1+IC1,M1)
IX2=MOD(IA2*IX2+IC2,M2)
IX3=MOD(IA3*IX3+IC3,M3)
J=1+(97*IX3)/M3
IF(J.GT.97.OR.J.LT.1)THEN
   print *,'problem in ran11, J=',J

ENDIF
ran11=R(J)
R(J)=(FLOAT(IX1)+FLOAT(IX2)*RM2)*RM1
RETURN
END

!-----------------------------------------------------------------------
real(8) FUNCTION gasdev11(IDUM) !RETURNS A NORMALLY DISTRIBUTED DEVIATE WITH
								!ZERO MEAN AND UNIT VARIANCE, USING ran11(IDUM)
								!AS THE SOURCE FOR THE UNIFORM DEVIATES.
	  INTEGER IDUM
	  INTEGER ISET
	  REAL(8) FAC,GSET,RSQ,V1,V2,ran11!,RAN2
	  SAVE ISET,GSET
	  DATA ISET/0/
	  IF (ISET.EQ.0) THEN
1       V1=2.*ran11(IDUM)-1.
		V2=2.*ran11(IDUM)-1.
		RSQ=V1**2+V2**2
		IF(RSQ.GE.1..OR.RSQ.EQ.0.)GOTO 1
		FAC=SQRT(-2.*LOG(RSQ)/RSQ)
		GSET=V1*FAC
		gasdev11=V2*FAC
		ISET=1
	  ELSE
		gasdev11=GSET
		ISET=0
	  ENDIF
	  RETURN
	  END

!-----------------------------------------------------------------------
subroutine trian(tmin,tmode,tmax,tdev) !generates a triangular deviate
use time_etc                           !FROM K.A. ROSE (not numerical recipe)
implicit none
real(8) :: pran,tmin,tmode,tmax,tdev,u,x
	  u=pran(1)
	  if(u.le.0.5)x=sqrt(0.5*u)
	  if(u.gt.0.5)x=1.0-sqrt(0.5*(1.0-u))
	  if(x.lt.0.0)x=0.0
	  if(x.gt.1.0)x=1.0
	  if(x.le.0.5)tdev=tmin+2.0*(tmode-tmin)*x
	  if(x.gt.0.5)tdev=2.0*tmode-tmax+2.0*(tmax-tmode)*x
 return
end

!-----------------------------------------------------------------------
SUBROUTINE indexx_sp(arr,index) !four subroutines that return a list of array posns that identify order of lengths, smallest to largest
USE nrtype; USE nrutil, ONLY : arth,assert_eq,nrerror,swap
IMPLICIT NONE
REAL(SP), DIMENSION(:), INTENT(IN) :: arr
INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: index
INTEGER(I4B), PARAMETER :: NN=15, NSTACK=50
REAL(SP) :: a
INTEGER(I4B) :: n,k,i,j,indext,jstack,l,r
INTEGER(I4B), DIMENSION(NSTACK) :: istack
n=assert_eq(size(index),size(arr),'indexx_sp')
index=arth(1,1,n)
jstack=0
l=1
r=n
do
	if (r-l < NN) then
		do j=l+1,r
			indext=index(j)
			a=arr(indext)
			do i=j-1,l,-1
				if (arr(index(i)) <= a) exit
				index(i+1)=index(i)
			end do
			index(i+1)=indext
		end do
		if (jstack == 0) RETURN
		r=istack(jstack)
		l=istack(jstack-1)
		jstack=jstack-2
	else
		k=(l+r)/2
		call swap(index(k),index(l+1))
		call icomp_xchg(index(l),index(r))
		call icomp_xchg(index(l+1),index(r))
		call icomp_xchg(index(l),index(l+1))
		i=l+1
		j=r
		indext=index(l+1)
		a=arr(indext)
		do
			do
				i=i+1
				if (arr(index(i)) >= a) exit
			end do
			do
				j=j-1
				if (arr(index(j)) <= a) exit
			end do
			if (j < i) exit
			call swap(index(i),index(j))
		end do
		index(l+1)=index(j)
		index(j)=indext
		jstack=jstack+2
		if (jstack > NSTACK) call nrerror('indexx: NSTACK too small')
		if (r-i+1 >= j-l) then
			istack(jstack)=r
		istack(jstack-1)=i
			r=j-1
		else
			istack(jstack)=j-1
			istack(jstack-1)=l
			l=i
		end if
	end if
end do
CONTAINS
!BL
SUBROUTINE icomp_xchg(i,j)
INTEGER(I4B), INTENT(INOUT) :: i,j
INTEGER(I4B) :: swp
if (arr(j) < arr(i)) then
	swp=i
	i=j
	j=swp
end if
END SUBROUTINE icomp_xchg
END SUBROUTINE indexx_sp

SUBROUTINE indexx_i4b(iarr,index)
USE nrtype; USE nrutil, ONLY : arth,assert_eq,nrerror,swap
IMPLICIT NONE
INTEGER(I4B), DIMENSION(:), INTENT(IN) :: iarr
INTEGER(I4B), DIMENSION(:), INTENT(OUT) :: index
INTEGER(I4B), PARAMETER :: NN=15, NSTACK=50
INTEGER(I4B) :: a
INTEGER(I4B) :: n,k,i,j,indext,jstack,l,r
INTEGER(I4B), DIMENSION(NSTACK) :: istack
n=assert_eq(size(index),size(iarr),'indexx_sp')
index=arth(1,1,n)
jstack=0
l=1
r=n
do
	if (r-l < NN) then
		do j=l+1,r
			indext=index(j)
			a=iarr(indext)
			do i=j-1,l,-1
				if (iarr(index(i)) <= a) exit
				index(i+1)=index(i)
			end do
			index(i+1)=indext
		end do
		if (jstack == 0) RETURN
		r=istack(jstack)
		l=istack(jstack-1)
		jstack=jstack-2
	else
		k=(l+r)/2
		call swap(index(k),index(l+1))
		call icomp_xchg(index(l),index(r))
		call icomp_xchg(index(l+1),index(r))
		call icomp_xchg(index(l),index(l+1))
		i=l+1
		j=r
		indext=index(l+1)
		a=iarr(indext)
		do
			do
				i=i+1
				if (iarr(index(i)) >= a) exit
			end do
			do
				j=j-1
				if (iarr(index(j)) <= a) exit
		end do
			if (j < i) exit
			call swap(index(i),index(j))
		end do
		index(l+1)=index(j)
		index(j)=indext
		jstack=jstack+2
		if (jstack > NSTACK) call nrerror('indexx: NSTACK too small')
		if (r-i+1 >= j-l) then
			istack(jstack)=r
			istack(jstack-1)=i
			r=j-1
		else
			istack(jstack)=j-1
			istack(jstack-1)=l
			l=i
		end if
	end if
end do
CONTAINS
!BL
SUBROUTINE icomp_xchg(i,j)
INTEGER(I4B), INTENT(INOUT) :: i,j
INTEGER(I4B) :: swp
if (iarr(j) < iarr(i)) then
	swp=i
	i=j
	j=swp
end if
END SUBROUTINE icomp_xchg
END SUBROUTINE indexx_i4b


!-----------------------------------------------------------------------
real(8) FUNCTION pran(ithread)    !generate a uniform 0 to 1 random number
use random

IMPLICIT NONE
INTEGER  ithread
INTEGER  IDUM, IFF(8), I, J
INTEGER  IX1(8), IX2(8), IX3(8)
REAL(4)  R(97,8)
SAVE R, IX1, IX2, IX3
DATA IFF /8 * 0/

I    = ithread
IDUM = seeds(I)

IF (IDUM.LT.0.OR.IFF(I).EQ.0) THEN
	IFF(I)=1
	IX1(I)=MOD(IC1-IDUM,M1)
	IX1(I)=MOD(IA1*IX1(I)+IC1,M1)
	IX2(I)=MOD(IX1(I),M2)
	IX1(I)=MOD(IA1*IX1(I)+IC1,M1)
	IX3(I)=MOD(IX1(I),M3)
	DO 11 J=1,97
	   IX1(I)=MOD(IA1*IX1(I)+IC1,M1)
	   IX2(I)=MOD(IA2*IX2(I)+IC2,M2)
	   R(J,I)=(FLOAT(IX1(I))+FLOAT(IX2(I))*RM2)*RM1
11  CONTINUE
	IDUM=1
ENDIF

seeds(I) = IDUM

IX1(I)=MOD(IA1*IX1(I)+IC1,M1)
IX2(I)=MOD(IA2*IX2(I)+IC2,M2)
IX3(I)=MOD(IA3*IX3(I)+IC3,M3)
J=1+(97*IX3(I))/M3
IF(J.GT.97.OR.J.LT.1)THEN
   print *,'problem in pran, J=',J
ENDIF
pran = R(J,I)
R(J,I)=(FLOAT(IX1(I))+FLOAT(IX2(I))*RM2)*RM1

RETURN
END
!
!real(8) FUNCTION pran(ithread)
!USE random
!IMPLICIT NONE
!INTEGER  ithread
!pran=0.5D0
!END

!  (C) COPR. 1986-92 NUMERICAL RECIPES SOFTWARE &OL`.