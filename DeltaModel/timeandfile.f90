MODULE time_etc
USE omp_lib
IMPLICIT NONE
  INTERFACE
	SUBROUTINE idate(SDATE)
	  INTEGER(4), INTENT(OUT) :: SDATE(3)
	END SUBROUTINE idate
  END INTERFACE

!MISC
INTEGER idum                       ! a random number seed
INTEGER indiv                      ! an individual ID (array element)

!TIME
INTEGER, PARAMETER :: nhours = 24  ! number of hours in a day
INTEGER, PARAMETER :: ndays = 360  ! number of days in a year
INTEGER nyears
INTEGER hour, jday, year           ! hour in day of sim (1 = midnight to 1 am), julian day in year of sim (1 = March 1), year of sim
INTEGER cumhr, cumday              ! cumulative hours (across all sim days and years), cumulative days (across all sim years)
INTEGER startdate(3), startime(3)  ! sim start date (d,mo,y), sim start time (h,m,s)
INTEGER endate(3), endtime(3)      ! sim end date (d,mo,y), sim end time (h,m,s)
INTEGER runtime(4)                 ! run time (days,hours,minutes,seconds)
INTEGER, PARAMETER :: numoutputfiles=25
!OUT FLAGS (and "isw" switches that control degree of output over time and indivs)

INTEGER trackid(5)                 ! list of indivs for which fine-scale movement details are to be output
								   ! CAUTION: if array dimension changed, change manually in MAIN and subroutine 'output'
INTEGER iswhourout                 ! 1 = every hour, 2 = every 2nd, ..., nhours=last
INTEGER iswdayout                  ! 1 = every day, 2 = every 2nd, ..., ndays=last
INTEGER iswyearout                 ! 1 = every year, 2 = every 2nd, ..., nyears=last
INTEGER isspeciesout               ! 0=output data for all sp, 1 to 5=output data for sp 1 to 5
INTEGER iswindivout                ! 1 = every indiv, 2 = every 2nd, 3 = 3rd, ..., totindivs=all
INTEGER outflag                    ! flag for distinguishing among output files when calling subroutine 'output' (2=file 22, 3=file 33, 4=file 44, etc)
REAL(8) profilingtime, profilingstarttime, profilingendtime
INTEGER fileflag(numoutputfiles)               ! output file flag. CAUTION: if output file(s) added, need to re-dimension this array
!$OMP THREADPRIVATE(outflag,indiv)

INTEGER(kind=omp_nest_lock_kind), PRIVATE :: filelock(numoutputfiles)
CONTAINS
	SUBROUTINE time_etc_module_init
	  IMPLICIT NONE
	  INTEGER i
	  DO i=1, numoutputfiles
		CALL OMP_init_nest_lock(filelock(i))
		CALL OMP_unset_nest_lock(filelock(i))
	  END DO
	END SUBROUTINE time_etc_module_init

	SUBROUTINE file_lock(i)
	  INTEGER i;
	  CALL OMP_set_nest_lock(filelock(i))
	END SUBROUTINE file_lock

	SUBROUTINE file_unlock(i)
	  INTEGER i;
	  CALL OMP_unset_nest_lock(filelock(i))
	END SUBROUTINE file_unlock
END MODULE

