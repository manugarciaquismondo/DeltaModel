!----------------------------------------------------------------------
SUBROUTINE inundation  !at start of each year, for each hour and cell, calc water depth
USE time_etc           !(m) from 5-d running ave of water level (m) and cell elev (m),
USE physicalenv        !and keep a running tot of hrs in yr that cell is inundated. If cell
IMPLICIT NONE          !heavily inundated (according to 'inundmax'), store that cell's elev (m)
					   !If wet or dry year, use idealized inund for high elevs (retains pattern of
					   !woody and hi_emergents). Store inund data data so re-used in later yrs.
INTEGER wlp                    !water level profile (06/07, 07/08, ..., 10/11)
INTEGER inundflag(5)           !flag indicating whether (1) or not (0) inundation already calculated for water level profile
INTEGER hr                     !position in 'waterlevel' that corresponds to hour and day of interest
INTEGER inundstore(numwaterprofiles,cols,rows), wettodrystore(numwaterprofiles,cols,rows), drytowetstore(numwaterprofiles,cols,rows)!stored inund values (one for each water level profiles used in model)
REAL(8) watdep(cols,rows), previousdep      !water depth (m) in ea cell (local to avoid conflict w/ global array 'waterdepth')
INTEGER xx, yy
INTEGER ii, jj
INTEGER previouswlpyear, previouswlp

PRINT *, "Calculating annual inundation (hours)..."
drytowet=0;wettodry=0
wlp = watsched(watyr)                     !store water level profile (e.g., 07/08, 10/11) as local shorthand
IF (year .LT. 2) inundflag=0              !zero flag array at start of sim

IF ((inundflag(wlp) .EQ. 1) .AND. (wlp .NE. watsched(1))) THEN           !if inundation already calculated for given water level profile. Error detected when loading profile from the first year.
 inund = inundstore(wlp,:,:)              !extract stored calcs
 drytowet=drytowetstore(wlp,:,:)
 wettodry=wettodrystore(wlp,:,:)
ELSE                                      !otherwise, inundation needs to be calculated
 inund = 0                                !zero temp counter array at start of year
 maxelev = -20.D0                         !begin with an impossibly low cell elev as max(m)
 DO xx = 1, ndays                         !loop over days,
  DO yy = 1, nhours                       !hours,
   DO ii = 1, cols                        !sub-grid columns,
	DO jj = 1, rows                       !and rows.
	 IF (year .GT. 1 .AND. yy .EQ. nhours .AND. xx .EQ. ndays) THEN !if not first year AND last hour of last day
	  IF (meannualwatlev(wlp) .GT. meannualwatlev(1)+0.01D0 .AND. idealinund(ii,jj) .LE. 1700) THEN !if current yr on ave wetter than first yr (+0.01 to avoid rounding errors) AND inund < 1701 hrs during ideal year
	   inund(ii,jj) = idealinund(ii,jj)   !set cell inund to ideal
	   CYCLE                              !go on to next cell (row) in loop
	  END IF
	  IF (meannualwatlev(wlp) .LT. meannualwatlev(1)-0.01D0 .AND. idealinund(ii,jj) .LE. 591) THEN !if current yr on ave drier than first yr (-0.01 to avoid rounding errors) AND inund could produce woody in ideal year
	   inund(ii,jj) = idealinund(ii,jj)   !set cell inund to ideal
	   CYCLE                              !go on to next cell in loop
	  END IF
	 END IF !not first year
	 hr = (xx - 1) * nhours + yy          !store hour of year as local shorthand
	 watdep(ii,jj) = avewater(wlp,hr) - cellelev(ii,jj) !calc water depth, where >0 = wet, <=0 = dry (m)
	 IF (hr .EQ. 1) THEN
	    IF (year .EQ. 1) THEN
	        previousdep= watdep(ii,jj) !If we are in the first hour, there is no previous depth, so we use the current one
	    ELSE
	        previouswlpyear=watyr-1
	        IF (previouswlpyear .EQ. 0) previouswlpyear=waterschedyrs
	        previouswlp=watsched(previouswlpyear)
	        previousdep= avewater(previouswlp,ndays*nhours) - cellelev(ii,jj)
        END IF
	 ELSE
	     previousdep=avewater(wlp,hr-1) - cellelev(ii,jj)
	 END IF
	 IF (watdep(ii,jj) .GT. 0.D0) THEN    !if depth > 0 (m)
	  inund(ii,jj) = inund(ii,jj) + 1     !keep a running total of inundated hours
	  IF(previousdep .LE. 0.D0) THEN
	    drytowet(ii,jj)=drytowet(ii,jj)+1
	  END IF
	  ELSE
	  IF(previousdep .GT. 0.D0) THEN
	    wettodry(ii,jj)=wettodry(ii,jj)+1
	  END IF
	 END IF
	 IF (inund(ii,jj) .EQ. inundmax) THEN !if cell fully inundated according to 'inundmax' (hours)
	  IF (cellelev(ii,jj) .GT. maxelev) maxelev = cellelev(ii,jj) !and cell elev is > maxelev, set maxelev to cell elev (m)
	 END IF                               !this "IF/THEN" finds highest cell elev (i.e., shallowest cell) that will give 'inundmax' (m)
	END DO !rows
   END DO !columns
  END DO !hours
 END DO !days
 inundflag(wlp) = 1                       !flag inundation calculated for given water level profile
 inundstore(wlp,:,:) = inund              !store inundation data
 drytowetstore(wlp,:,:)=drytowet
 wettodrystore(wlp,:,:)=wettodry
 IF (year .LT. 2) idealinund = inund      !if first year, save inund as ideal inund (needed for protecting high-elevation hab types in future)
END IF !inundation flag

IF (fileflag(6) .EQ. 1) THEN              !if output desired
 outflag = 7; CALL output(0,0,0,0,0,0,0)                 !output inundated hours (7=file 77)
END IF !grid data are to be output
IF (fileflag(24) .EQ. 1) THEN              !if output desired
 outflag = 25; CALL output(0,0,0,0,0,0,0)                 !output inundated water profile (25=file 250)
END IF !grid data are to be output

END SUBROUTINE
