!----------------------------------------------------------------------
SUBROUTINE timecounter !reprints start date and time, gets end date and time,
USE time_etc           !calculate simulation run-time in days, hours, mintues,
IMPLICIT NONE          !and seconds (fails if sim runs over New Years or Feb 29)
INTEGER startday, enday  !julian day that sim started and ended (1 to 365). Ignores leap years

PRINT *, ""                                 !re-print start date (1=day, 2=month, 3=year) and start time (2=hour, 3=minute, 4=second)
WRITE (*, 11) startdate, startime
11 FORMAT (' Start date (dd/mm/yyyy) and time (h:m:s): ', I2.2, '/', I2.2, '/', I4.4, ', ', I2.2, ':', I2.2, ':', I2.2) !output start date and time
CALL idate(endate); CALL itime(endtime)     !get start date (1=day, 2=month, 3=year) and start time (2=hour, 3=minute, 4=second)
endate(3) = endate(3) + 2000                !convert year from "yy" to "yyyy"
WRITE (*, 22) endate, endtime
22 FORMAT ('   End date (dd/mm/yyyy) and time (h:m:s): ', I2.2, '/', I2.2, '/', I4.4, ', ', I2.2, ':', I2.2, ':', I2.2) !output end date and time

IF (startdate(2) .EQ. 1) startday = startdate(1)       !determine julian day for start of sim
IF (startdate(2) .EQ. 2) startday = 31 + startdate(1)
IF (startdate(2) .EQ. 3) startday = 59 + startdate(1)
IF (startdate(2) .EQ. 4) startday = 90 + startdate(1)
IF (startdate(2) .EQ. 5) startday = 120 + startdate(1)
IF (startdate(2) .EQ. 6) startday = 151 + startdate(1)
IF (startdate(2) .EQ. 7) startday = 181 + startdate(1)
IF (startdate(2) .EQ. 8) startday = 212 + startdate(1)
IF (startdate(2) .EQ. 9) startday = 243 + startdate(1)
IF (startdate(2) .EQ. 10) startday = 273 + startdate(1)
IF (startdate(2) .EQ. 11) startday = 304 + startdate(1)
IF (startdate(2) .EQ. 12) startday = 334 + startdate(1)
IF (endate(2) .EQ. 1) enday = endate(1)                !determine julian day for end of sim
IF (endate(2) .EQ. 2) enday = 31 + endate(1)
IF (endate(2) .EQ. 3) enday = 59 + endate(1)
IF (endate(2) .EQ. 4) enday = 90 + endate(1)
IF (endate(2) .EQ. 5) enday = 120 + endate(1)
IF (endate(2) .EQ. 6) enday = 151 + endate(1)
IF (endate(2) .EQ. 7) enday = 181 + endate(1)
IF (endate(2) .EQ. 8) enday = 212 + endate(1)
IF (endate(2) .EQ. 9) enday = 243 + endate(1)
IF (endate(2) .EQ. 10) enday = 273 + endate(1)
IF (endate(2) .EQ. 11) enday = 304 + endate(1)
IF (endate(2) .EQ. 12) enday = 334 + endate(1)

runtime(1) = enday - startday               !days
runtime(2) = endtime(1) - startime(1)       !hours
IF (runtime(2) .LT. 0) THEN                 !if negative value, into next day but earlier hour (i.e., not yet full 24 hours)
 runtime(2) = 24 - startime(1) + endtime(1) !recalc hours
 runtime(1) = runtime(1) - 1                !reduce day by 1
END IF
runtime(3) = endtime(2) - startime(2)       !minutes
IF (runtime(3) .LT. 0) THEN                 !if negative value, into next hour but earlier minute (i.e., not yet full 60 minutes)
 runtime(3) = 60 - startime(2) + endtime(2) !recalc minutes
 IF (runtime(2) .GT. 0) THEN                !if hours not equal
  runtime(2) = runtime(2) - 1               !reduce hours by 1
 ELSE                                       !otherwise, hours equal
  runtime(2) = 24 - 1                       !reduce hours by 1
  IF (runtime(1) .GT. 0) runtime(1) = runtime (1) - 1 !if days equal, reduce days by 1
 END IF
END IF
runtime(4) = endtime(3) - startime(3)       !seconds
IF (runtime(4) .LT. 0) THEN                 !if negative value, into next minute but earlier second (i.e., not yet full 60 seconds)
 runtime(4) = 60 - startime(3) + endtime(3) !recalc seconds
 IF (runtime(3) .GT. 0) THEN                !if minutes not equal
  runtime(3) = runtime(3) - 1               !reduce minutes by 1
 ELSE                                       !otherwise, minutes equal
  runtime(3) = 60 - 1                       !recalc minutes
  IF (runtime(2) .GT. 0) THEN               !if hours not equal
   runtime(2) = runtime(2) - 1              !reduce hours by 1
  ELSE                                      !otherwise, hours equal
   runtime(2) = 24 - 1                      !reduce hours by 1
   IF (runtime(1) .GT. 0) runtime(1) = runtime (1) - 1 !if days not equal, reduce days by 1
  END IF
 END IF
END IF

WRITE (*, 33) runtime; 33 FORMAT (' Run time (d,h,m,s): ', I2.2, ':', I2.2, ':', I2.2, ':', I2.2)
PRINT *, ""

END SUBROUTINE