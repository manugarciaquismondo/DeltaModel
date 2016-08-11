! Read parameters from xml files
SUBROUTINE read_parameter_data(paramfilename)
    USE physicalenv
    USE preybase
    USE species
    USE indivattributes
    USE bioenergetics
    USE movement
    USE reproduction
    USE time_etc
    USE random
    integer :: ii
    character(len=60) :: paramfilename
    INTEGER, PARAMETER :: parameterfiledescriptor=270
    REAL(8) :: inputargs(8)
    OPEN (parameterfiledescriptor, file = paramfilename, STATUS = 'UNKNOWN')
    READ (parameterfiledescriptor, *) RM1, RM2, M1, M2, M3, IA1, IA2, IA3, IC1, IC2, IC3
    READ (parameterfiledescriptor, *) (inundationdays(ii),ii=1, inundationtimestamps)
	READ (parameterfiledescriptor, *) (inundationhours(ii),ii=1, inundationtimestamps)
	READ (parameterfiledescriptor, *) (speciesid(ii),ii=1, nfwsp)
	READ (parameterfiledescriptor, *) minfeedhr, maxfeedhr
    READ (parameterfiledescriptor, *) (fileflag(ii),ii=1, 23)
    READ (parameterfiledescriptor, *) (watsched(ii),ii=1, waterschedyrs)
	READ (parameterfiledescriptor, *) zerolevel, addwater, minprob, maxSAV, minworth 
	READ (parameterfiledescriptor, *) calorcoeff, inundmax, cellmax, cellsize, idum
	READ (parameterfiledescriptor, *) pdfa
	READ (parameterfiledescriptor, *) pdfb
	READ (parameterfiledescriptor, *) pdfc
	READ (parameterfiledescriptor, *) pdfd
	READ (parameterfiledescriptor, *) pdfe
	READ (parameterfiledescriptor, *) indpreywght
	READ (parameterfiledescriptor, *) initpreynum
	READ (parameterfiledescriptor, *) preyenerden
	DO j=1, preytypes
	    READ (parameterfiledescriptor, *) (preymult(j,ii),ii=1,habtype)
	END DO 
	READ (parameterfiledescriptor, *) spposn
	DO j=1, nfwsp
	    READ (parameterfiledescriptor, *) (earlymort(ii,j),ii=1, 3)
	END DO
	DO j=1, nfwsp
	    READ (parameterfiledescriptor, *) (mortmult(j,ii),ii=1, habtype)
	END DO
	DO j=1, nfwsp
	    READ (parameterfiledescriptor, *) (preyv(j,ii),ii=1, preytypes)
	END DO
	DO j=1, nfwsp
	    READ (parameterfiledescriptor, *) (preyk(j,ii),ii=1, preytypes)
	END DO
	READ (parameterfiledescriptor, *) (cmaxscalar(j),j=1, nfwsp)
	READ (parameterfiledescriptor, *) (cmaxexpo(j),j=1, nfwsp)
	READ (parameterfiledescriptor, *) (cmaxoptt(j),j=1, nfwsp)
	READ (parameterfiledescriptor, *) (cmaxmaxt(j),j=1, nfwsp)
	READ (parameterfiledescriptor, *) (cmaxtheta(j),j=1, nfwsp)
	READ (parameterfiledescriptor, *) (pvalue(j),j=1, nfwsp)
	READ (parameterfiledescriptor, *) (egscalar(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (egexpo(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (exscalar(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (sdascalar(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (respscalar(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (respexpo(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (respoptt(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (respmaxt(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (resptheta(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (act(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (predenerden(j),j=1, nfwsp)
	DO j=1, nfwsp
	    READ (parameterfiledescriptor,*) (initwt(j,ii),ii=1, 2)
	END DO
	
	READ (parameterfiledescriptor,*) (metmorphwt(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (lw_scalar(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (lw_expo(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (matlen(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (breedtype(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (minspwnday(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (maxspwnday(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (feca(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (fecb(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (fecc(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (batches(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (eggwght(j),j=1, nfwsp)
	DO j=1, nfwsp
	    READ (parameterfiledescriptor,*) (fracta(j,ii),ii=1, 3)
	END DO
	DO j=1, nfwsp
	    READ (parameterfiledescriptor,*) (fractc(j,ii),ii=1, 3)
	END DO
	READ (parameterfiledescriptor,*) (nhood(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (emermult(j),j=1, nfwsp)
	READ (parameterfiledescriptor,*) (fiterror(j),j=1, nfwsp)
    CLOSE (parameterfiledescriptor) 

END SUBROUTINE