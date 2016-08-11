!     ******************************************************************
!     deltamodel
!     Copyright(c) Louisiana Stae University 2000
!
!     Created: 1/14/2010 3:21:04 PM
!     Author : KENNETH ROSE
!     Last change: PV 8/12/2014 12:25:25 PM
!     ******************************************************************

!TO-DO / QUESTIONS
!split xmort into habitat and pred (save and check)
!keep track of mort due to hab and pred (save and check)


!once calibrated, check if faster to pass old fishbycell to new one or to insert indiv loop between existing two
  !(remove 'olds' from module and 'fiteval', move call to just before call to updatechohort, move zeros to 'fishbycell', d.n. zero maxcoord, make mxwrth local, loop over alive indivs in fishbycell)
!future depth calculated ~same way in 'futuredepth', 'fitnhood', 'move' x 2. Create separate subroutine?
!PREY: include habitat mult for minimum g wwt of prey left by pred?

!Previous launch on February 7-10, 2015. Compare it with back then.
!OpenMP DOES NOT WORK. DO NOT USE. 

#define C_MODE
#define UPDATE_WITHOUT_WEIGHT(indindex) xworth(indindex)
#define UPDATE_WITH_WEIGHT(indindex) (xworthchng(indindex) * xtotwght(indindex))
#define UPDATE_STARVATION_TROPHIC_TRANSFER_WITHOUT_WEIGHT_NOT_SENESCENT(indindex) trophictransfer%withoutworth%starvation%notsenescent(xspecies(indindex))=trophictransfer%withoutworth%starvation%notsenescent(xspecies(indindex))+UPDATE_WITHOUT_WEIGHT(indindex)
#define UPDATE_STARVATION_TROPHIC_TRANSFER_WITHOUT_WEIGHT_SENESCENT(indindex) trophictransfer%withoutworth%starvation%senescent(xspecies(indindex))=trophictransfer%withoutworth%starvation%senescent(xspecies(indindex))+UPDATE_WITHOUT_WEIGHT(indindex)
#define UPDATE_STRANDING_TROPHIC_TRANSFER_WITHOUT_WEIGHT_NOT_SENESCENT(indindex) trophictransfer%withoutworth%stranding%notsenescent(xspecies(indindex))=trophictransfer%withoutworth%stranding%notsenescent(xspecies(indindex))+UPDATE_WITHOUT_WEIGHT(indindex)
#define UPDATE_STRANDING_TROPHIC_TRANSFER_WITHOUT_WEIGHT_SENESCENT(indindex) trophictransfer%withoutworth%stranding%senescent(xspecies(indindex))=trophictransfer%withoutworth%stranding%senescent(xspecies(indindex))+UPDATE_WITHOUT_WEIGHT(indindex)
#define UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_WITHOUT_WEIGHT_NOT_SENESCENT(indindex) trophictransfer%withoutworth%naturalmortality%notsenescent(xspecies(indindex))=trophictransfer%withoutworth%naturalmortality%notsenescent(xspecies(indindex))+UPDATE_WITHOUT_WEIGHT(indindex)
#define UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_WITHOUT_WEIGHT_SENESCENT(indindex) trophictransfer%withoutworth%naturalmortality%senescent(xspecies(indindex))=trophictransfer%withoutworth%naturalmortality%senescent(xspecies(indindex))+UPDATE_WITHOUT_WEIGHT(indindex)
#define UPDATE_STARVATION_TROPHIC_TRANSFER_WITH_WEIGHT_NOT_SENESCENT(indindex) trophictransfer%withworth%starvation%notsenescent(xspecies(indindex))=trophictransfer%withworth%starvation%notsenescent(xspecies(indindex))+UPDATE_WITH_WEIGHT(indindex)
#define UPDATE_STARVATION_TROPHIC_TRANSFER_WITH_WEIGHT_SENESCENT(indindex) trophictransfer%withworth%starvation%senescent(xspecies(indindex))=trophictransfer%withworth%starvation%senescent(xspecies(indindex))+UPDATE_WITH_WEIGHT(indindex)
#define UPDATE_STRANDING_TROPHIC_TRANSFER_WITH_WEIGHT_NOT_SENESCENT(indindex) trophictransfer%withworth%stranding%notsenescent(xspecies(indindex))=trophictransfer%withworth%stranding%notsenescent(xspecies(indindex))+UPDATE_WITH_WEIGHT(indindex)
#define UPDATE_STRANDING_TROPHIC_TRANSFER_WITH_WEIGHT_SENESCENT(indindex) trophictransfer%withworth%stranding%senescent(xspecies(indindex))=trophictransfer%withworth%stranding%senescent(xspecies(indindex))+UPDATE_WITH_WEIGHT(indindex)
#define UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_WITH_WEIGHT_NOT_SENESCENT(indindex) trophictransfer%withworth%naturalmortality%notsenescent(xspecies(indindex))=trophictransfer%withworth%naturalmortality%notsenescent(xspecies(indindex))+UPDATE_WITH_WEIGHT(indindex)
#define UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_WITH_WEIGHT_SENESCENT(indindex) trophictransfer%withworth%naturalmortality%senescent(xspecies(indindex))=trophictransfer%withworth%naturalmortality%senescent(xspecies(indindex))+UPDATE_WITH_WEIGHT(indindex)
#define UPDATE_STARVATION_TROPHIC_TRANSFER_NOT_SENESCENT(indindex) UPDATE_STARVATION_TROPHIC_TRANSFER_WITHOUT_WEIGHT_NOT_SENESCENT(indindex); UPDATE_STARVATION_TROPHIC_TRANSFER_WITH_WEIGHT_NOT_SENESCENT(indindex)
#define UPDATE_STRANDING_TROPHIC_TRANSFER_NOT_SENESCENT(indindex) UPDATE_STRANDING_TROPHIC_TRANSFER_WITHOUT_WEIGHT_NOT_SENESCENT(indindex); UPDATE_STRANDING_TROPHIC_TRANSFER_WITH_WEIGHT_NOT_SENESCENT(indindex)
#define UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_NOT_SENESCENT(indindex) UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_WITHOUT_WEIGHT_NOT_SENESCENT(indindex); UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_WITH_WEIGHT_NOT_SENESCENT(indindex)
#define UPDATE_STARVATION_TROPHIC_TRANSFER_SENESCENT(indindex) UPDATE_STARVATION_TROPHIC_TRANSFER_WITHOUT_WEIGHT_SENESCENT(indindex); UPDATE_STARVATION_TROPHIC_TRANSFER_WITH_WEIGHT_SENESCENT(indindex)
#define UPDATE_STRANDING_TROPHIC_TRANSFER_SENESCENT(indindex) UPDATE_STRANDING_TROPHIC_TRANSFER_WITHOUT_WEIGHT_SENESCENT(indindex); UPDATE_STRANDING_TROPHIC_TRANSFER_WITH_WEIGHT_SENESCENT(indindex)
#define UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_SENESCENT(indindex) UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_WITHOUT_WEIGHT_SENESCENT(indindex); UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_WITH_WEIGHT_SENESCENT(indindex)
#define TEST_MACRO() PRINT *, "Test macro started"; PRINT *, "Test macro completed"
!DEC$ IF (.NOT. DEFINED(_OPENMP))
INCLUDE 'debugconstants.f90'
MODULE omp_lib_kinds
 INTEGER, PARAMETER :: omp_nest_lock_kind = int_ptr_kind()
END MODULE
MODULE omp_lib
USE omp_lib_kinds
CONTAINS
 INTEGER FUNCTION omp_get_num_procs()
  omp_get_num_procs = 1
 END FUNCTION

 SUBROUTINE omp_set_num_threads(num_threads)
  INTEGER num_threads
 END SUBROUTINE

 INTEGER FUNCTION omp_get_thread_num()
  omp_get_thread_num = 0
 END FUNCTION

 INTEGER FUNCTION omp_get_num_threads()
  omp_get_num_threads = 1
 END FUNCTION

 SUBROUTINE omp_init_nest_lock(lock)
  INTEGER(kind=omp_nest_lock_kind) lock;
 END SUBROUTINE

 SUBROUTINE omp_set_nest_lock(lock)
  INTEGER(kind=omp_nest_lock_kind) lock;
 END SUBROUTINE

 SUBROUTINE omp_unset_nest_lock(lock)
  INTEGER(kind=omp_nest_lock_kind) lock;
 END SUBROUTINE

 INTEGER FUNCTION omp_test_nest_lock(lock)
  INTEGER(kind=omp_nest_lock_kind) lock;
  omp_test_nest_lock = 1
 END FUNCTION
END MODULE
!DEC$ ENDIF

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
INTEGER, PARAMETER :: numoutputfiles = 26
!OUT FLAGS (and "isw" switches that control degree of output over time and indivs)
INTEGER fileflag(numoutputfiles)               ! output file flag. CAUTION: if output file(s) added, need to re-dimension this array
INTEGER trackid(5)                 ! list of indivs for which fine-scale movement details are to be output
								   ! CAUTION: if array dimension changed, change manually in MAIN and subroutine 'output'
INTEGER iswhourout                 ! 1 = every hour, 2 = every 2nd, ..., nhours=last
INTEGER isspecieswhourout
INTEGER iswdayout                  ! 1 = every day, 2 = every 2nd, ..., ndays=last
INTEGER iswyearout                 ! 1 = every year, 2 = every 2nd, ..., nyears=last
INTEGER isspeciesout               ! 0=output data for all sp, 1 to 5=output data for sp 1 to 5
INTEGER iswindivout                ! 1 = every indiv, 2 = every 2nd, 3 = 3rd, ..., totindivs=all
INTEGER outflag                    ! flag for distinguishing among output files when calling subroutine 'output' (2=file 22, 3=file 33, 4=file 44, etc)
REAL(8) profilingtime, profilingstarttime, profilingendtime

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

MODULE random
IMPLICIT NONE
INTEGER  SEEDS(8)
REAL(4) :: RM1=3.8580247E-6, RM2=7.4373773E-6 
INTEGER :: M1=259200,IA1=7141,IC1=54773
INTEGER :: M2=134456,IA2=8121,IC2=28411
INTEGER :: M3=243000,IA3=4561,IC3=51349
END MODULE

!-----------------------------------------------------------------------
MODULE physicalenv      !the physical environment (grid, elevation, temperature, habitat, etc)
USE time_etc
IMPLICIT NONE

!GRID(S)

INTEGER, PARAMETER :: delftcols = 2495 ! max number of columns in any interpolated DELFT grid
INTEGER, PARAMETER :: delftrows = 3745 ! max number of rows in any interpolated DELFT grid
INTEGER, PARAMETER :: topo = 2         !select DELFT 'topo' (i.e., elevation) input file (1=topo1(early), 2=topo2(middle), 3=topo3(late))
INTEGER, PARAMETER :: waterschedyrs = 9, numwaterprofiles=5

!INTEGER, PARAMETER :: subgrid(4) = (/ 250, 1220, 820, 2600 /) ! topo1 full grid (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 250, 1750, 600, 2750 /) ! topo2 full grid (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 250, 1950, 700, 2750 /) ! topo3 full grid (1=min_col, 2=max_col, 3=min_row, 4=max_row)
INTEGER, PARAMETER :: subgrid(4) = (/ 570, 709, 1800, 1939 /) ! topo1 and topo2 main subgrid, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 557, 696, 1770, 1909 /) ! topo3 main subgrid, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 727, 866, 1932, 2071 /)  ! topo2 subgrid 1, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 885, 1024, 2065, 2204 /) ! topo2 subgrid 2, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 674, 813, 1625, 1764 /)  ! topo2 subgrid 3, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 778, 917, 1450, 1589 /)  ! topo2 subgrid 4, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)

INCLUDE 'subgriddimensions.f90'

INTEGER, PARAMETER :: cellnumber = cols*rows         ! total number of cells in sub-grid

!ELEVATION, WATER LEVEL, DEPTH, INUNDATION
REAL(8) zerolevel              ! water level that defines marsh edge. Increase to make delta wetter (m)
REAL(8) addwater               ! water added to 'waterlevel' so that marsh species in marsh habitats (does not affect inundation)
REAL(8) cellelev(cols,rows)    ! bottom elevation of ea cell according to interpolated DELFT output (m)
REAL(8) waterlevel(5,ndays*nhours) ! hourly water levels for 5 yrs (1 value for entire grid in any given hr) (m)
REAL(8) avewater(5,ndays*nhours) ! 5-day (120 hr) running average of water levels for ea hour and 5 yrs (1 value for entire grid in any given hr) (m)
REAL(8) meannualwatlev(5)      ! water level profile specific average annual 'avewater' water level (m)
REAL(8) waterdepth(cols,rows)  ! water depth in ea hour and cell (relativized to 'zerolevel') (m)
REAL(8) watervol(cols,rows)    ! water volume in ea hour and cell (m**3)
REAL(8) maxelev                ! the highest cell elev (i.e., shallowest cell) that will result in 'inundmax' (m)
INTEGER watsched(waterschedyrs)            ! 9-year schedule of water years
INTEGER watyr                  ! year (posn) in 'watsched' that water level data are extracted from. 9-year repeating loop
INTEGER depthflag              ! for calling 'depth' 0 = "at start of sim, 1 = "from within hour loop"
INTEGER inund(cols,rows)       ! water profile specific annual running total of hours inundated (=> some threshold water depth)
INTEGER idealinund(cols,rows)  ! record of inundation in ea cell in first year, which has ideal water levels
INTEGER wetarray(cols,rows)    ! flags cells that are dry (0) or wet (1)
INTEGER drytowet(cols, rows), wettodry(cols, rows)
!TEMPERATURE
REAL(8) wtemp                     ! daily water temperature in all cells (oC)

!HABITAT
INTEGER, PARAMETER :: habtype = 6 ! number of habitat types (1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh))
INTEGER habitat(cols,rows)        ! habitat type in ea cell
INTEGER inundmax                  ! maximum inundation (hours). At or above this threshold, cells are either SAV or deep water
INTEGER habmask(habtype,2,cellnumber) ! mask array that stores cell coord of all hab types
INTEGER habcount(habtype)         ! number of cell coord of ea hab type stored in 'habmask' (used as row index)
REAL(8) minprob                   ! smallest habitat probty of interest - values below threshold are ignored
REAL(8) maxSAV                    ! deepest mean water depth at which SAV(2) occur; all deep water(1) at depths above this threshold (m)
REAL(8) pdfa(habtype)             ! hab-specific paremater of Beta probability density function (values assinged to hab types 2-5 only)
REAL(8) pdfb(habtype)             ! hab-specific paremater of Beta probability density function (values assinged to hab types 2-5 only)
REAL(8) pdfc(habtype)             ! hab-specific paremater of Beta probability density function (values assinged to hab types 2-5 only)
REAL(8) pdfd(habtype)             ! hab-specific paremater of Beta probability density function (values assinged to hab types 2-5 only)
REAL(8) pdfe(habtype)             ! hab-specific paremater of Beta probability density function (values assinged to hab types 2-5 only)

INTEGER, PARAMETER :: inundationtimestamps = 4
INTEGER :: inundationdays(inundationtimestamps) = (/ 241, 299, 300, 117 /)
INTEGER :: inundationhours(inundationtimestamps) = (/ 5, 23, 4, 24 /)

INTEGER(kind=omp_nest_lock_kind), PRIVATE :: celllock(cols,rows)
CONTAINS
  SUBROUTINE physicalenv_module_init()
	INTEGER c,r;
	DO c=1, cols
	  DO r=1, rows
		CALL OMP_init_nest_lock(celllock(c,r));
	  END DO
	END DO
  END SUBROUTINE physicalenv_module_init

  SUBROUTINE cell_lock(c,r)
	INTEGER c,r;
	call OMP_set_nest_lock(celllock(c,r));
  END SUBROUTINE cell_lock

  SUBROUTINE cell_unlock(c,r)
	INTEGER c,r;
	call OMP_unset_nest_lock(celllock(c,r));
  END SUBROUTINE cell_unlock
END MODULE

!-----------------------------------------------------------------------
MODULE preybase         !2 zooplankton and 3 zoobenthos
USE time_etc            !(1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)
USE physicalenv
IMPLICIT NONE

INTEGER, PARAMETER :: preytypes = 5 ! number of prey types (1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)
REAL(8) minpreyden                  ! minimum density of prey left in cell after foraging by indivs (g wwt/m**2 or 3)
REAL(8) preya(preytypes), preyb(preytypes), preyc(preytypes), preyd(preytypes) ! parameters for prey turnover rate
REAL(8) indpreywght(preytypes)       ! wght (g wwt) of an individual of ea prey type
REAL(8) initpreynum(preytypes)       ! numeric density of ea prey type in ea cell at start of sim (/m**2 for benth, /m**3 for zoop)
 REAL(8) preymult(preytypes,habtype) ! habitat-specific multiplier of the 'initpreynum' of ea prey type
REAL(8) unitpreynum(preytypes,cols,rows) ! numeric density of ea prey type in ea cell (/m**2 for benth, /m**3 for zoop)
REAL(8) unitpreybmss(preytypes,cols,rows)! biomass density (g wwt) of ea prey type in ea cell (/m**2 for benth, /m**3 for zoop)
REAL(8) aveunitnum(habtype+1,preytypes) ! average numeric density of ea prey type in ea hab type and entire grid (/m**2 for benth, /m**3 for zoop)
REAL(8) aveunitbmss(habtype+1,preytypes)! average biomass density of ea prey type in ea hab type and entire grid (/m**2 for benth, /m**3 for zoop)

END MODULE

!-----------------------------------------------------------------------
MODULE species  !species and stage attributes (number, age classes, age class sizes, mortality, etc.)
USE physicalenv
IMPLICIT NONE

INTEGER, PARAMETER :: speciespool = 4  ! number of sp to select from when building food web
INTEGER, PARAMETER :: nfwsp = 4        ! number of food web sp
 !CAUTION: if 'nfwsp' changed, manually re-dimension 'speciesid', 'ageclasses', 'ageclassspace', 'ageclasspopsize', 'indivs', 'totindivs' (below)!!
INTEGER :: speciesid(nfwsp) = (/ 1, 2, 3, 4 /)   ! ID numbers of food web sp (1=GS, 2=IS, 3=GK, 4=BA)
 !CAUTION: if ID values changed, manually change values in 'ageclasses', 'ageclassspace', 'ageclasspopsize' (below)

 !The following is a list of parameter values (ageclasses, ageclassspace, ageclasspopsize) for ea sp
 !species ID  species name    ageclasses  min ageclassspace
 !1           shrimp (GS)     2           277 (5x=1400)      !ageclasses from Pattillo et al 1997 (pg 84)
 !2           silverside (IS) 2           200 (10x=2000)     !ageclasses from Pattillo et al 1997 (pg 183)
 !3           killifish (GK)  3           201 (5x=1025)      !ageclasses from Pattillo et al 1997 (pg 176)
 !4           anchovy (BA)    3           220 (5x=1100)      !ageclasses from Pattillo et al 1997 (pg 153)
 !NOTE: used to also follow 5240 croaker indivs

INTEGER, PARAMETER :: ageclasses(nfwsp) = (/ 2, 2, 3, 3 /) ! number of age classes in a sp (if changed, manually change 'maxacctage' below, species 'initwt', and out file 44)
INTEGER, PARAMETER :: maxacctage = 3 ! max number of age classes used when accounting/outputting abundance-by-age (= oldest age class of longest-lived sp)
!INTEGER, PARAMETER :: ageclassspace(nfwsp) = (/ 1400, 2000, 1025, 1100 /) ! sp-specific number of model indivs per age class (super-indivs)
INTEGER, PARAMETER :: ageclassspace(nfwsp) = (/ 1400, 2000, 1025, 1100 /) ! sp-specific number of model indivs per age class (super-indivs)
REAL(8), PARAMETER :: ageclassweights(nfwsp)= (/30.D0, 15.D0, 20.D0, 50.D0/)
REAL(8), PARAMETER :: ageclasspopsize(nfwsp) = (/ ageclassspace(1)*ageclassweights(1), & ! sp-specific pop size in ea age class ("real" indivs, not super-indivs)
												  ageclassspace(2)*ageclassweights(2), &
												  ageclassspace(3)*ageclassweights(3), &
												  ageclassspace(4)*ageclassweights(4) /)
INTEGER, PARAMETER :: indivs(nfwsp) = (/ ageclasses(1)*ageclassspace(1), & ! total number of indivs of ea sp (super-indivs)
										 ageclasses(2)*ageclassspace(2), &
										 ageclasses(3)*ageclassspace(3), &
										 ageclasses(4)*ageclassspace(4) /)
INTEGER, PARAMETER :: totindivs = indivs(1)+indivs(2)+indivs(3)+indivs(4) ! total number of indivs in community
INTEGER :: cellmax = 700    ! total model indivs allowed in ea cell

INTEGER spposn(speciespool)            ! position of all sp in 'speciesid' array (used to extract sp-specific param values)
INTEGER fwsp                           ! global shortand for position of specific sp in all 'nfwsp'-dimensioned arrays
INTEGER totestab                       ! count of established indivs (i.e., indivs not recruited on a given day)
INTEGER totnew                         ! count of new recruits on that day
INTEGER totavail                       ! count of indiv space available for allocation to recruits
INTEGER split(nfwsp)                   ! sp-specific count of indiv space used to split high-worth indivs at end of year
INTEGER totsplit                       ! count across all sp of indiv space used to split high-worth indivs at end of year
INTEGER totunused                      ! count across all sp of indiv space not use (i.e., wasted) at end of year
INTEGER indivbycell(nfwsp,cols,rows)   ! total model indivs of ea sp in ea cell
!INTEGER idbycell(nfwsp,cols,rows,cellmax) ! list of indiv IDs in ea cell
INTEGER maxcoord(nfwsp,2)              ! coord of cell w/ sp-specific max worth across grid for hour
REAL(8) initwt(nfwsp,maxacctage)       ! init wght of age-1+ indivs of ea sp (used when assigning attributes from 'initindivs'). Age-0 (YOY) wght assigned at recruitment. (g wwt)
REAL(8) earlymort(3,nfwsp)             ! hourly inst nat mort rate of cohort (early development stages egg, yolk, feed larv) of ea sp
 REAL(8) mortmult(nfwsp,habtype)       ! predation as represented by sp- and habitat-specific multipliers of hourly inst mort rate for YOY, juv, adults
REAL(8) matlen(nfwsp)                  ! sp-specific, length-based maturity threshold (mature if .GE. this value) (mm)
REAL(8) maxlen(nfwsp)                  ! sp-specific maximum "observed" total length (mm)
REAL(8) worthbycell(nfwsp,cols,rows)   ! total of worths (= abundance) of ea sp in ea cell
 REAL(8) mxwrth(nfwsp)                 ! highest recorded worth by sp in all cells (reset hourly)
REAL(8) bmassbycell(nfwsp,cols,rows), annualbmass(nfwsp)   ! total of biomass (g wwt) of each sp in ea cell
REAL(8) mortloss(nfwsp,5)              ! total sp-specific worth lost to nat mort, starvation, stranding, predation and senescence ea day
REAL(8) cummortloss(5)                 ! total worth lost to nat mort, starvation, stranding, predation and senescence ea year
INTEGER spwndayscounter(nfwsp)
INTEGER longindivscounter(nfwsp)
INTEGER aliveindivs(nfwsp)
INTEGER senescentcount(nfwsp)
REAL(8) senescentvalue(nfwsp)
INTEGER notcalculatedspawningdays(nfwsp)
REAL(8) netproductivity(nfwsp,cols,rows), productivityvalues(nfwsp,cols,rows) !net productivity (g wwt = weight gained or lost * worth) of each species in each cell
REAL(8) speciesspawnloss(nfwsp)
REAL(8) speciesweightdiff(nfwsp)
REAL(8) speciesweightandworthdiff(nfwsp)
INTEGER speciesweightdiffnumber(nfwsp)
REAL(8) worthannualsumming(nfwsp)
REAL(8) speciesworthdailysumming(nfwsp)
REAL(8) growthproductivity(nfwsp)
REAL(8) worthtimesddwannualsumming(nfwsp)
REAL(8) sumworth(nfwsp)
INTEGER extinctquery(nfwsp)
TYPE SENESCENCETROPHICTRANSFERTYPE
    REAL(8) senescent(nfwsp)
    REAL(8) notsenescent(nfwsp)
END TYPE SENESCENCETROPHICTRANSFERTYPE

TYPE TROPHICTRANSFERTYPE
        TYPE(SENESCENCETROPHICTRANSFERTYPE) starvation
        TYPE(SENESCENCETROPHICTRANSFERTYPE) stranding
        TYPE(SENESCENCETROPHICTRANSFERTYPE)  naturalmortality
END TYPE TROPHICTRANSFERTYPE

TYPE TROPHICTRANSFEROVERALLTYPE
        TYPE(TROPHICTRANSFERTYPE) withoutworth
        TYPE(TROPHICTRANSFERTYPE) withworth
END TYPE TROPHICTRANSFEROVERALLTYPE



TYPE(TROPHICTRANSFEROVERALLTYPE) :: trophictransfer

INTEGER(kind=omp_nest_lock_kind), PRIVATE :: specieslock(nfwsp)
CONTAINS
  SUBROUTINE species_module_init()
	INTEGER ifwsp
	DO ifwsp=1, nfwsp
	  CALL OMP_init_nest_lock(specieslock(ifwsp));
	END DO
  END SUBROUTINE species_module_init

  SUBROUTINE species_lock(ifwsp)
	INTEGER, INTENT(IN) :: ifwsp
	call OMP_set_nest_lock(specieslock(ifwsp));
  END SUBROUTINE species_lock

  SUBROUTINE species_unlock(ifwsp)
	INTEGER, INTENT(IN) :: ifwsp
	call OMP_unset_nest_lock(specieslock(ifwsp));
  END SUBROUTINE species_unlock
END MODULE

!-----------------------------------------------------------------------
MODULE indivattributes
USE species
USE time_etc
IMPLICIT NONE

REAL(8) xrand(totindivs)       ! an indiv's random number (changed hourly)
INTEGER xorder(totindivs)      ! an indiv's random position in 'indiv' loop (as determined by 'xrand')

REAL(8) xworth(totindivs)      ! an individual's worth (number of actual indiv that a super-indiv represents)
 REAL(8) minworth              ! threshold worth below which indiv considered dead
REAL(8) xsomwght(totindivs)    ! body wght; all non-reproductive tissues (g wwt)
REAL(8) xgonwght(totindivs)    ! gonad wght; indiv's reproductive store (g wwt)
REAL(8) xtotwght(totindivs)    ! total wght; sum of soma and gonads (g wwt)
 REAL(8) lw_scalar(nfwsp)      ! sp-specific scalar 'a' of the l-w regression
 REAL(8) lw_expo(nfwsp)        ! sp-specific exponent 'b' of the l-w regression
REAL(8) xstrvwght(totindivs)   ! threhsold wght below which indiv starves (g wwt)
REAL(8) xsomchng(totindivs)    ! change in somatic wght in a day (g wwt)
REAL(8) xgonchng(totindivs)    ! change in gonadal wght in a day (g wwt)
REAL(8) xtotchng(totindivs)    ! change in total wght in a day (g wwt)
REAL(8) xlen(totindivs)        ! total length based on total wght (mm)
REAL(8) xlenchng(totindivs)    ! change in total length over day (mm)
REAL(8) xmort(totindivs)       ! hourly instantaneous natural mortality rate (/hr)
 INTEGER mortflag              ! flag indicating where in model subroutine 'mortcalc' called from (1=assign(init), 2=assign(recruit), 3=weightchange)
REAL(8) xmindep(totindivs)     ! shallowest cell depth given indiv length (m)
REAL(8) xbreedsom(totindivs)   ! somatic wght used to limit reprod invest (if capital spawner, assigned @ mat & then on ea spawn end day (or 7 d before spawn season if IS), if income, assigned @ mat if mat in season otherwise on spawn day) (g wwt)
REAL(8) xtrgtinvst(totindivs)  ! ideal annual invest in reprod given wght-basd mort when reprod invest begins, expressed as prop'n of somatic g wwt
REAL(8) xreprdinvst(totindivs) ! seasonal running tot of g wwt invested in reprod. Unaffected by gonad wght loss. Reset at end of spawn season (g wwt)
REAL(8) xfec(totindivs)        ! fecundity (number of eggs, regardless of how ripe) as a function of 'xweight'
REAL(8) xbatchwght(totindivs)  ! wght of single egg batch (g wwt)
REAL(8) xeggprdn(totindivs)    ! annual running total of eggs spawned by indiv. Reset at start of year
INTEGER xspecies(totindivs)    ! 1=shrimp, 2=silverside, 3=killifish, 4=croaker, 5=anchovy
INTEGER xalive(totindivs)      ! 0=unassigned, 1=alive, 2=dead(nat mort incl. predation), 3=dead(starved), 4=dead(stranded), 5=dead(old)
INTEGER xagedays(totindivs)    ! age in days (cumulative)
INTEGER xageyrs(totindivs)     ! age in years (1 = age-0)
INTEGER xstage(totindivs)      ! later life stage (1=YOY, 2=juvenile, 3=adult)
INTEGER xmat(totindivs)        ! 0=immature, 1=mature
INTEGER xspwnday(totindivs)    ! jday in spawn season when indiv can begin spawning
INTEGER xfecflag(totindivs)    ! flag that keeps track of whether fecundity has (1) or has not (0) been calculated for a spawn season
							   ! (doubles as indicator of whether or not indiv can spawn according to jday and water temp)
INTEGER xbtchcnt(totindivs)    ! seasonal running total number of times an indiv has spawned. Reset at end of spawn season
INTEGER xsensce(totindivs)     ! flag that is turned on (1) when indiv is in last year of life and can no longer spawn (because of gonad size and max investment, max batches, or end of spawn season)
INTEGER hoursdry(totindivs)    ! number of hours that indiv has been in a dry cell
REAL(8) xspawnloss(totindivs)
INTEGER assignflag             ! a flag for assigning attributes in different ways. See subroutine 'assingattributes'
							   ! (1=initialize, 2=recruit, 3=split worth, 4=zero old)

REAL(8) xprevwght(totindivs), xavgwghtdiff(totindivs)
REAL(8) xprevwghtandworth(totindivs), xavgwghtandworthdiff(totindivs)
REAL(8) xworthchng(totindivs)
REAL(8) xworthdailysumming(totindivs)

!DEC$ IF DEFINED(TRACE_INDIVIDUALS)
INTEGER, PARAMETER:: numberofselectedindividuals = 100
INTEGER individualalive(totindivs)
INTEGER, PARAMETER:: numberofdefinedspecies = 2
INTEGER:: definedspecies(numberofdefinedspecies)=(/1, 2/)
INTEGER:: speciescounter(numberofdefinedspecies)=(/0, 0/)
!DEC$ ENDIF
!$OMP THREADPRIVATE(mortflag)
CONTAINS
!DEC$ IF DEFINED(TRACE_INDIVIDUALS)


SUBROUTINE filterindividuals()
    INTEGER :: indivcounter, indivindex
    indivcounter=1
    individualalive=0
    DO indivindex=1, totindivs
        IF (isselectedspecies(xspecies(indivindex)) .EQ. 1) THEN
            individualalive(indivindex)=1
            indivcounter=indivcounter+1
        END IF
        IF (indivcounter .GT. numberofselectedindividuals) THEN
            EXIT
        END IF
    END DO
END SUBROUTINE filterindividuals

INTEGER FUNCTION isselectedindividual(indivindex)
INTEGER:: indivindex, indiviterator
isselectedindividual=individualalive(indivindex)
!isselectedindividual=1
END FUNCTION isselectedindividual

SUBROUTINE checkandclearindividual(indivindex)
INTEGER, INTENT(IN):: indivindex
IF(xalive(indivindex) .NE. 1) THEN
	CALL clearindividual(indivindex)
END IF
END SUBROUTINE checkandclearindividual

INTEGER FUNCTION isselectedspecies(speciesindex)
INTEGER:: speciesindex, speciesiterator
isselectedspecies=0
DO speciesiterator=1, numberofdefinedspecies
    IF (definedspecies(speciesiterator) .EQ. speciesindex .AND. (speciescounter(speciesiterator) .LT. numberofselectedindividuals/numberofdefinedspecies)) THEN
        isselectedspecies=1
        speciescounter(speciesiterator)=speciescounter(speciesiterator)+1
        EXIT
    END IF
END DO
END FUNCTION isselectedspecies
SUBROUTINE registerindividual(indiv)
INTEGER:: indiv
!  IF(jday .NE. 1 .OR. year .NE. 1) THEN
      IF (isselectedspecies(xspecies(indiv))) THEN
        IF (individualalive(indiv) .EQ. -1) THEN
            individualalive(indiv)=1
        END IF
     END IF
!  END IF
END SUBROUTINE registerindividual
SUBROUTINE clearindividual(indiv)
INTEGER:: indiv
IF (individualalive(indiv) .EQ. 1) THEN
    individualalive(indiv)=0
END IF
END SUBROUTINE clearindividual
!DEC$ ENDIF
REAL(8) FUNCTION sumaverageweightdifference(species)
INTEGER :: species, speciesiterator
sumaverageweightdifference=0.D0
DO speciesiterator=1, totindivs
    IF(xalive(speciesiterator) .AND. xspecies(speciesiterator) .EQ. species) THEN
        sumaverageweightdifference = sumaverageweightdifference+xavgwghtdiff(speciesiterator)
    END IF
END DO 
END FUNCTION sumaverageweightdifference
REAL(8) FUNCTION sumaverageweightandworthdifference(species)
INTEGER :: species, speciesiterator
sumaverageweightandworthdifference=0.D0
DO speciesiterator=1, totindivs
    IF(xalive(speciesiterator) .AND. xspecies(speciesiterator) .EQ. species) THEN
        sumaverageweightandworthdifference = sumaverageweightandworthdifference+xavgwghtandworthdiff(speciesiterator)
    END IF
END DO 
END FUNCTION sumaverageweightandworthdifference
INTEGER FUNCTION countlivingindividuals(species)
INTEGER :: species, speciesiterator
countlivingindividuals=0
DO speciesiterator=1, totindivs
    IF(xalive(speciesiterator) .AND. xspecies(speciesiterator) .EQ. species) THEN
        countlivingindividuals = countlivingindividuals+1
    END IF
END DO 
IF(countlivingindividuals .EQ. 0) THEN
    countlivingindividuals=1
END IF
END FUNCTION countlivingindividuals

SUBROUTINE calculatesumworths()
INTEGER:: indiviterator
sumworth=0.D0
DO indiviterator=1,totindivs
    IF(xalive(indiviterator) .EQ. 1) THEN
        sumworth(xspecies(indiviterator))=sumworth(xspecies(indiviterator))+xworth(indiviterator)
    END IF
END DO
END SUBROUTINE calculatesumworths

END MODULE

!-----------------------------------------------------------------------
MODULE bioenergetics !including diet/feeding and bioenergetics assoc w/ reproduction
USE preybase
USE species
IMPLICIT NONE

INTEGER :: minfeedhr = 7  !earliest hour in day that indiv of all sp can feed (defines feeding window)
INTEGER :: maxfeedhr = 18 !latest hour in day that indiv of all sp can feed (defines feeding window)
INTEGER :: totfeedhr !number of hours in feeding window
INTEGER feedhr                 ! hour in feeding window
REAL(8) predenerden(nfwsp)     ! energy density of ea sp (Joules / g wwt)
REAL(8) preyenerden(preytypes) ! energy density of ea prey type (Joules / g wwt)
REAL(8) calorcoeff             ! energy converstion rate (calorific coefficient) (Joules/g O2)
!ENERGY GAINS
REAL(8) xcmax(totindivs)       ! max consumption (g prey / g wwt of indiv / day)
REAL(8) pvalue(nfwsp)          ! proportion of daily 'xcmax' actually consumed (g prey / g wwt of indiv / day)
 REAL(8) cmaxscalar(nfwsp)     ! sp-specific scalar 'a' of the cmax-wght regression
 REAL(8) cmaxexpo(nfwsp)       ! sp-specific exponent 'b' of the cmax-wght regression
 REAL(8) cmaxoptt(nfwsp)       ! sp-specific optimal temperature for cmax (oC)
 REAL(8) cmaxmaxt(nfwsp)       ! sp-specific max temperature for cmax (oC)
 REAL(8) cmaxtheta(nfwsp)      ! sp-specific theta for cmax (~Q10)
INTEGER preyv(nfwsp,preytypes) ! sp-specific vulnerability of ea prey type 0 = invuln, 1 = vuln
REAL(8) preyk(nfwsp,preytypes) ! sp-specific half saturation const of ea prey type. Larger values mean less consump (g wwt/m**2 for benth, g wwt/m**3 for zoop)
REAL(8) realcons(totindivs,preytypes) ! indiv-specific amount of ea prey type consumed (daily running tot of realized consump) (g wwt)
REAL(8) totcons(totindivs)     ! indiv-specific amount of all prey types consumed (daily running tot) (g wwt / g wwt of indiv)
REAL(8) preypropn(totindivs,preytypes) ! indiv-specific contribution (g wwt) of ea prey type to 'totcons'
REAL(8) dietbase(totindivs,preytypes) ! mass of each prey type consumed by indiv during day (g wwt)
!ENERGY LOSSES
REAL(8) egscalar(nfwsp)        ! proportion of consumption lost to egestion
REAL(8) egexpo(nfwsp)          ! exponent for temperature dependence of egestion
REAL(8) exscalar(nfwsp)        ! proportion of consumption lost to excretion
REAL(8) sdascalar(nfwsp)       ! proportion of consumption lost to specific dynamic action
REAL(8) resp                   ! consumption lost to respiration (g O2 / g wwt of indiv / day)
 REAL(8) respscalar(nfwsp)     ! sp-specific scalar 'a' of the respiration-wght regression
 REAL(8) respexpo(nfwsp)       ! sp-specific exponent 'b' of the respiration-wght regression
 REAL(8) respoptt(nfwsp)       ! sp-specific optimal temperature for respiration (oC)
 REAL(8) respmaxt(nfwsp)       ! sp-specific max temperature for respiration (oC)
 REAL(8) resptheta(nfwsp)      ! sp-specific theta for respiration (~Q10)
 REAL(8) act(nfwsp)            ! sp-specific activity constant
REAL(8) enerchng               ! total daily change in predator energy due to energy gains (consumption)
							   ! and losses (egestion, respiration, etc) summed over all prey (J)
!$OMP THREADPRIVATE(enerchng, resp)
END MODULE

!-----------------------------------------------------------------------
MODULE movement
USE time_etc
USE preybase
USE species
USE bioenergetics
IMPLICIT NONE

REAL(8) expcons(preytypes)! amount of ea prey type that indiv expected to consume in 'nhood' cell in next hour (g wwt)
REAL(8) x_dist(totindivs) ! horizontal component of dist from origin (m)
REAL(8) y_dist(totindivs) ! vertical component of dist from origin (m)
REAL(8) expfit            ! expected fitness, which is expected biomass (as a result of expected worth and expected weight change) in candidate cell (g wwt)
REAL(8) fiterror(nfwsp)   ! sp-specific error rate for fitness calc (0.01 = 1%)
REAL(8) fitstore(cols,rows)! array that stores exp fit vals by cell for re-use by indiv within hour
REAL(8) maxfit            ! highest recorded expected fitness during fitness neighborhood search (expected weight change x expected worth)
REAL(8) neerest           ! nearest recorded future wet cell during emergency neighborhood search (m)
REAL(8) deepest           ! highest recorded depth during emergency neighborhood search (m)
REAL(8) mintri, modtri, maxtri !minimum, maximum, and modular values for defining a triangular distribution
REAL(8) randtri           ! value drawn from triangular distribution
INTEGER inithab(nfwsp)    ! sp-specific hab type in which to place new (init and recruit) indivs
INTEGER i_col(totindivs)  ! i'th column in which indiv located
INTEGER j_row(totindivs)  ! j'th row in which indiv located
INTEGER nhood(nfwsp)      ! species-specific search radius (cells)
INTEGER emermult(nfwsp)   ! species-specific multiplier of 'nhood'
INTEGER timesteps         ! number of times that an indiv can move in 1 hour given its length and distance to corner of fitness nhood
INTEGER nhdflag           ! flag that indicates if neighborhood needs to be evaluated (b/c next hour or new cell)
						  ! (1) or if target cell unchanged (0)
INTEGER movetype          ! identifies if movement based on depth (1) or fitness (0)
INTEGER fitsearch         ! flag used in subroutine 'fitnhood' so that calls to other subroutines allow fitness
						  ! assessment w/o updating indiv or prey ! (0 = not called and 1 = called during 'nhood' search)
INTEGER fitflag(cols,rows)! flag indicating exp fitness already evaluated for cell and hour (flag is set to 'indiv' and zeroed at start of each hour)
INTEGER emersearch        ! flag used in subroutine 'emernhood' so that calls to subroutine 'futuredepth' allows
						  ! depth assessment w/o getting confused with assessment of current cell
INTEGER evalwet(cols,rows)! flag indicating future wetness already evaluated for given hour and candidate cell (flag is set to 'indiv' for re-use by indivs and zeroed at start of each hour)
INTEGER wetstore(cols,rows) ! stores candidate cell wetness (0 = wet, 1 = dry) for re-use within hour by indivs
INTEGER emertype          ! identifies if emergency-based movement towards future wet (1) or deepest (0) cell
INTEGER newcol            ! target column in 'nhood' according to fitness- or depth-based movement
INTEGER newrow            ! target row in 'nhood' acccording to fitness- or depth-based movement
INTEGER strandcount(nfwsp,cols,rows) ! sp-specific, sim-long count of number of model indivs lost to stranding in ea cell
INTEGER rescuecount       ! sim-long count of number of model indivs moved to avoid edge effects

!$OMP THREADPRIVATE(fitsearch, newcol, newrow, emersearch, mintri, &
!$OMP modtri, maxtri, expfit, expcons, movetype, randtri, nhdflag, &
!$OMP emertype, timesteps, neerest, deepest, maxfit, fitstore, & 
!$OMP fitflag, wetstore, evalwet)

END MODULE

!-----------------------------------------------------------------------
MODULE reproduction !including cohort development and recruitment
USE time_etc        !but not bioenergetics or indiv attributes assoc
USE bioenergetics   !w/ reproduction
USE species
IMPLICIT NONE

REAL    spwnlen(nfwsp,totindivs) !sp-specific list of lengths (mm) of indivs that are large enough to spawn
REAL(8) feca(nfwsp)             ! sp-specific term 'a' of the fecundity - total wght relation
REAL(8) fecb(nfwsp)             ! sp-specific term 'b' of the fecundity - total wght relation
REAL(8) fecc(nfwsp)             ! sp-specific term 'c' of the fecundity - total wght relation
REAL(8) eggwght(nfwsp)          ! sp-specific mean egg wght (g wwt)
REAL(8) cohortabund(3,ndays,nfwsp) ! abundance by early life stage (egg, yolk, feed larv), day and food web sp
REAL(8) cohortftemp(3,ndays,nfwsp) ! fract temp by early life stage (egg, yolk, feed larv), day, and food web sp
 REAL(8) fracta(nfwsp,3)        ! sp-specific coeff 'Da' of fractional temp based cohort dev. funct (1=eggs,2=yolk,3=feeding)
 REAL(8) fractc(nfwsp,3)        ! sp-specific coeff 'Dc' of fractional temp based cohort dev. funct (1=eggs,2=yolk,3=feeding)
REAL(8) dayeggprdn(nfwsp)       ! sp-specific daily egg production
REAL(8) lftblin(3+maxacctage,nfwsp) ! sp-specific tot number entering early (egg, yolk, feed larv) and later (YOY, age-1, ..., age-max) life stages
REAL(8) lftblout(3+maxacctage,nfwsp) ! sp-specific tot number leaving early (egg, yolk, feed larv) and later (YOY, age-1, ..., age-max) life stages
REAL(8) lftbldurn(3+maxacctage,nfwsp) ! sp-specific length in days of early (egg, yolk, larv) and late (YOY, age-1, ..., age-max) life stages
REAL(8) cohorttot(3+maxacctage,nfwsp) ! tot size of cohort by early life stage (egg, yolk, feed larv) and food web sp (sum of all daily cohorts - includes nat mort))
REAL(8) metmorphwt(nfwsp)       ! starting wght (g wwt) of feeding larvae of ea sp
REAL(8) newyoy(nfwsp)           ! total number of NEW yoy of ea sp (i.e., abundance of fully developed feeding larv cohort)
REAL(8) avenewrth(nfwsp)        ! mean worth of newly recruited indivs of ea sp (= new yoy / daily recruit space)
REAL(8) ageabund(nfwsp,maxacctage) ! sp- and age-specific sum of worth
REAL(8) agep(nfwsp,maxacctage)  ! sp- and age-specific running total of p (proportion of cmax)
REAL(8) agelen(nfwsp,maxacctage)! sp- and age-specific running total of lengths (mm)
REAL(8) agewgt(nfwsp,maxacctage)! sp- and age-specific running total of weights (g wwt)
REAL(8) avediet(speciespool,preytypes) ! sp- and prey-specific running total of consumption (g wwt)
REAL(8) habdist(nfwsp,habtype)  ! sp-specific running total of abundance by hab type (worth)
REAL(8) agemeanp(nfwsp,maxacctage)! sp- and age-specific p value (proportion of cmax)
REAL(8) agemeanlen(nfwsp,maxacctage)! sp- and age-specific mean length (mm)
REAL(8) agemeanwgt(nfwsp,maxacctage)! sp- and age-specific mean weight (g wwt)
INTEGER spwncnt(nfwsp)          ! sp-specific count of indivs that are large enough to spawn
INTEGER spwnid(nfwsp,totindivs) !sp-specific list of indivs that are large enough to spawn
INTEGER oldindivs(totindivs)    ! an array for storing IDs of indivs that are older than max_age
INTEGER oldcount                ! a counter for storing IDs in succession
INTEGER spaceavail              ! a counter of available space (equals a recruit's indiv ID)
INTEGER breedtype(nfwsp)        ! a flag that identifies sp as income (0) or capital (1) breeder
INTEGER priortytype(nfwsp)      ! a flag that identifies tissue type to protect during wght loss (assuming above starve wght)
								! (0=somatic, 1=gonad until max batches)
INTEGER minspwnday(nfwsp)       ! minimum spawn jday for subroutine 'recruitment' (determines initial distrib'n of recruit space over time
INTEGER maxspwnday(nfwsp)       ! maximum spawn jday for subroutine 'recruitment' (determines initial distrib'n of recruit space over time
INTEGER batches(nfwsp)          ! number of egg batches that ea sp can have in a spawn season
INTEGER cohortflag(3,ndays,nfwsp)  ! an integer flag that indivates if eggs, yolk larv, and feed larv are present (1) or absent (0) by day and food web sp
INTEGER newyoyflag(nfwsp)       ! an integer flag that indivates if 'imminent' recruits are present (1) or absent (0) for a food web sp
INTEGER frstrecday(nfwsp)       ! first possible jday of recruitment given spawn season and cohort development times
INTEGER lastrecday(nfwsp)       ! last possible jday of recruitment given spawn season and cohort development times
INTEGER initrecdist(nfwsp,ndays)!distribution of rec space over days in year for ea sp after accounting for 'unused' space due to rounding
INTEGER modrecdist(nfwsp,ndays) ! new sp-specific distrib'n of rec space over ndays after 'extra' yoy space on a given day is not used
INTEGER recruitcount(nfwsp)     ! running total of recruits allocated in a year
INTEGER lftbloutflag(3+maxacctage,nfwsp) ! flag indicating that at least one cohort (early life stage) or indiv (later life stages) left stage

END MODULE

INCLUDE 'numrecipesall.f90'     ! include numerical recipes file

!-MAIN PROGRAM---------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------

PROGRAM deltamodel
USE time_etc
USE physicalenv
USE preybase
USE species
USE indivattributes
USE bioenergetics
USE movement
USE reproduction
USE random
IMPLICIT NONE
INTEGER i, j, ifwsp
INTEGER num_threads, omp_chunk, timeOffsetBetweenIterations, argCounter
INTEGER narg, prevyear !#of arg & counter of arg
CHARACTER(len=256)::fileRoute, waterDir, outputDir, inputYear, paramfile, timeOffsetString !Arg name
totfeedhr = maxfeedhr - minfeedhr + 1
num_threads = omp_get_num_procs()
CALL OMP_SET_NUM_THREADS(num_threads)
!CALL OMP_SET_NUM_THREADS(8)
omp_chunk = totindivs/num_threads
waterDir=''
PRINT "(A,I1,A)"," Execution using ",num_threads," processors."
narg=COMMAND_ARGUMENT_COUNT()
nyears=2
argCounter=0
IF (narg .gt. argCounter) THEN
    argCounter=argCounter+1
	call GET_COMMAND_ARGUMENT(argCounter, fileRoute)
	fileRoute= fileRoute(:LEN_TRIM(fileRoute))
	if(narg .gt. argCounter) then
	    argCounter=argCounter+1
	    call GET_COMMAND_ARGUMENT(argCounter, waterDir)
	    waterDir= waterDir(:LEN_TRIM(waterDir))
	    waterDir=trim(waterDir)//'/'
	    if(narg .gt. argCounter) then
	        argCounter=argCounter+1
	        call GET_COMMAND_ARGUMENT(argCounter, outputDir)
	        outputDir= outputDir(:LEN_TRIM(outputDir))
	        outputDir=trim(outputDir)//'/'
	        if(narg .gt. argCounter) then
	            argCounter=argCounter+1
	            call GET_COMMAND_ARGUMENT(argCounter, inputYear)
	            read(inputYear,*) nyears
	        end if
	    end if
	end if
END IF
idum = -1212           !set random number seed

DO i=1,num_threads
 seeds(i) = idum*i
END DO

CALL time_etc_module_init
CALL physicalenv_module_init
CALL species_module_init

CALL idate(startdate); CALL itime(startime) !get start date (1=day, 2=month, 3=year) and start time (2=hour, 3=minute, 4=second)
startdate(3) = startdate(3) + 2000          !convert year from "yy" to "yyyy"
WRITE (*, 111) startdate, startime
111 FORMAT (' Start date (dd/mm/yyyy) and time (h:m:s): ', I2.2, '/', I2.2, '/', I4.4, ', ', I2.2, ':', I2.2, ':', I2.2) !output start date and time
PRINT *, ""

DO i = 1, nfwsp     !loop over food web species to check that all age classes will be output
 IF (ageclasses(i) .GT. maxacctage) THEN
  DO j = 1, 25
   CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
  END DO
  PRINT *, "Error in main program:"
  PRINT *, "age classes modeled > age classes output for food web species", i
  PRINT *, "The variable 'maxacctage' is too small."
  PRINT *, "Simulation stopped."
  CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
  STOP
 END IF
END DO

PRINT *, "Interpolated elevations are from DELFT topo            ", topo
PRINT *, "Number of sim years                                    ", nyears

!--output limiters (alphabetical order by file name)-------------------
fileflag(1) = 1  !do (1) or do not (0) output 'out_allocatedspace.out' (file 33, space allocated or wasted)
!---------------------
fileflag(2)  = 0 !do (1) or do not (0) output 'out_griddepth.m' (file 66, water depth (m) of ea cell)
fileflag(3)  = 1 !do (1) or do not (0) output 'out_gridelevation.m' (file 55, elevation (m) of ea cell)
fileflag(4)  = 1 !do (1) or do not (0) output 'out_gridfishbycell.out' (file 99, sp-specific totals for superindiv, worth (abundance), and biomass in ea cell)
fileflag(5)  = 1 !do (1) or do not (0) output 'out_gridhabitat.m' (file 100, habitat type in ea cell)
fileflag(6)  = 1 !do (1) or do not (0) output 'out_gridinundhrs.m' (file 77, total hours inundated in a year for ea cell)
fileflag(7)  = 0 !do (1) or do not (0) output 'out_gridpreybycell.out' (file 160, abundance and biomass (m**2 or m**3) of 5 base prey types in ea cell)
fileflag(8)  = 1 !do (1) or do not (0) output 'out_gridpreymeans.out' (file 170, mean abundance and biomass (m**2 or m**3) of 5 base prey types by hab type)
!---------------------
fileflag(9)  = 0 !do (1) or do not (0) output 'out_indivs_added.out' (file 110, initial attributes of all indivs that are initialized or recruited)
fileflag(10) = 0 !do (1) or do not (0) output 'out_indivs_alive.out' (file 22, traits of alive indivs)
fileflag(11) = 1 !do (1) or do not (0) output 'out_indivs_dead.out' (file 120, traits of dead indivs (nat mort, starved, stranded, predated, old))
fileflag(12) = 0 !do (1) or do not (0) output 'out_indivs_diet.out' (file 150, indiv diets: g wwt consumed by prey type, species, stage)
fileflag(13) = 0 !do (1) or do not (0) output 'out_indivs_move.out' (file 130, all movement info (including sub-hour) for up to 5 user-defined indivs)
 trackid = (/ 0, 0, 0, 0, 0 /) !list of indiv IDs for which details of fine-scale movement are to be output (max 5 indivs)
!---------------------
fileflag(14) = 1 !do (1) or do not (0) output 'out_input_echo.out' (file 12, all input and parameter values)
fileflag(15) = 1 !do (1) or do not (0) output 'out_stage_abunds.out' (file 44, abundance in ea stage and sp)
fileflag(16) = 0 !do (1) or do not (0) output 'out_stage_means.out' (file 140, mean length, weight, and density of ea age and sp)
fileflag(17) = 1 !do (1) or do not (0) output 'out_summary_diet.out' (file 190, mean diet by sp and prey type)
fileflag(18) = 1 !do (1) or do not (0) output 'out_summary_habdist.out' (file 200, distribn of abund by sp and hab type)
fileflag(19) = 0 !do (1) or do not (0) output 'out_summary_lifetable.out' (file 180, life table by sp and stage)
fileflag(20) = 0 !do (1) or do not (0) output 'out_summary_maxden.out' (file 210, highest density (worth/m2) on grid by sp)
fileflag(21) = 1 !do (1) or do not (0) output 'out_summary_worthlost.out' (file 220, worth lost by sp to nat mort, starvation, stranding, predation)
fileflag(22) = 1 !do (1) or do not (0) output 'out_summary_profiling.out' (file 220, worth lost by sp to nat mort, starvation, stranding, predation)
fileflag(23) = 1 !do (1) or do not (0) output 'out_gridproductivity.out' (file 230, net productivity (g wwt = weight gained or lost *worth) by species and cell)
fileflag(24) = 1 !do (1) or do not (0) output 'out_waterprofile.out' (file 250, water profile (cell inundated or not))
fileflag(25) = 1 !do (1) or do not (0) output 'out_gridproductivity.out' (file 230, net productivity (g wwt = weight gained or lost *worth) by species and cell)
fileflag(26) = 0 !do (1) or do not (0) output 'out_spawnloss.out'

iswhourout = 24  !1 = every hour, 2 = every 2nd, 3 = 3rd, ..., nhours = last (daily)
isspecieswhourout=1
iswdayout = 1    !1 = every day, 2 = every 2nd, 3 = 3rd, ..., ndays = last (annually)
iswyearout = 1   !1 = every year, 2 = every 2nd, 3 = 3rd, ..., nyears = last (end of sim)
isspeciesout = 0 !0 = all food web sp, 1 = 1, 2 = 2, 3 = 3, etc. (for files 22, 44, 99, 110, 120, 130, 140, 150, 180, 190)
iswindivout = 1  !1 = every indiv, 2 = every 2nd, 3 = 3rd, ..., totindivs = last

!--output statement----------------------------------------------------
IF (SUM(fileflag)-fileflag(14) .EQ. 0) THEN
 PRINT *, "NO OUTPUT FOR THIS SIM"
ELSE
 PRINT *, "Hourly output rate (1=every hour, 2=every other, etc.):", iswhourout
 PRINT *, "Daily output rate (1=every day, 2=every other, etc.):  ", iswdayout
 PRINT *, "Yearly output rate (1=every year, 2=every other, etc.):", iswyearout
 IF (nfwsp .GT. 1) PRINT *, "Species output (0=all, 1=GS, 2=IS, 3=GK, 4=BA):        ", isspeciesout
 IF (nfwsp .EQ. 1) PRINT *, "Species output (0=all, 1=GS, 2=IS, 3=GK, 4=BA):        ", speciesid(1)
 IF (SUM(fileflag(9:13))-fileflag(11) .GE. 1) PRINT *, "Indiv output rate (1=all, 2=every other, etc.):        ", iswindivout
 PRINT *, ""
 PRINT *, "Output includes file(s)"
 IF (fileflag(1) .EQ. 1) PRINT *, " out_allocatedspace.out"
 IF (fileflag(2) .EQ. 1) PRINT *, " out_griddepth.m"
 IF (fileflag(3) .EQ. 1) PRINT *, " out_gridelevation.m"
 IF (fileflag(4) .EQ. 1) PRINT *, " out_gridfishbycell.out"
 IF (fileflag(5) .EQ. 1) PRINT *, " out_gridhabitat.m"
 IF (fileflag(6) .EQ. 1) PRINT *, " out_gridinundhrs.m"
 IF (fileflag(7) .EQ. 1) PRINT *, " out_gridpreybycell.out"
 IF (fileflag(8) .EQ. 1) PRINT *, " out_gridpreymeans.out"
 IF (fileflag(9) .EQ. 1) PRINT *, " out_indivs_added.out"
 IF (fileflag(10) .EQ. 1) PRINT *, " out_indivs_alive.out"
 IF (fileflag(11) .EQ. 1) PRINT *, " out_indivs_dead.out"
 IF (fileflag(12) .EQ. 1) PRINT *, " out_indivs_diet.out"
 IF (fileflag(13) .EQ. 1) THEN
  PRINT *, " out_indivs_move.out"
  DO i = 1, 5
   IF (trackid(i) .GT. 1) PRINT *, "  for indiv", trackid(i)
  END DO
 END IF
 IF (fileflag(14) .EQ. 1) PRINT *, " out_input_echo.out"
 IF (fileflag(15) .EQ. 1) PRINT *, " out_stage_abunds.out"
 IF (fileflag(16) .EQ. 1) PRINT *, " out_stage_means.out"
 IF (fileflag(17) .EQ. 1) PRINT *, " out_summary_diet.out"
 IF (fileflag(18) .EQ. 1) PRINT *, " out_summary_habdist.out"
 IF (fileflag(19) .EQ. 1) PRINT *, " out_summary_lifetable.out"
 IF (fileflag(20) .EQ. 1) PRINT *, " out_summary_maxden.out"
 IF (fileflag(21) .EQ. 1) PRINT *, " out_summary_worthlost.out"
 IF (fileflag(22) .EQ. 1) PRINT *, " out_summary_profiling.out"
 IF (fileflag(23) .EQ. 1) PRINT *, " out_gridproductivity.out"
 IF (fileflag(24) .EQ. 1) PRINT *, " out_waterprofile.out"
 IF (fileflag(25) .EQ. 1) PRINT *, " out_annualbiomass.out"
 IF (fileflag(26) .EQ. 1) PRINT *, " out_spawnloss.out"
END IF !fileflag
print *, ""
if(narg .gt. 0) then
    PRINT *, "Topo file is ",trim(fileRoute)
    if(narg .gt. 1) then
        PRINT *, "Water directory is ",trim(waterDir)
        if(narg .gt. 2) then
            PRINT *, "Output directory is ",trim(outputDir)
        end if
    end if
end if
print *, ""
!--open input files----------------------------------------------------
IF(narg .lt. 1) THEN
    IF (topo .EQ. 1) OPEN (11, file = 'interptopo1.dat', STATUS = 'UNKNOWN') !input elevations (m) from DELFT topo1 (young delta)
    IF (topo .EQ. 2) OPEN (11, file = 'interptopo2.dat', STATUS = 'UNKNOWN') !input elevations (m) from DELFT topo2 (middle-aged delta)
    IF (topo .EQ. 3) OPEN (11, file = 'interptopo3.dat', STATUS = 'UNKNOWN') !input elevations (m) from DELFT topo3 (old delta)
ELSE
	open (11, file = fileRoute, STATUS = 'UNKNOWN')
end if

OPEN (81, file = trim(waterDir)//'waterlevels1_0708.dat', STATUS = 'UNKNOWN')!input raw and 5-d averaged hourly water levels from Atchafalaya Delta, 3/1/2007-2/23/08 (Station ID 8764227)(m)
OPEN (82, file = trim(waterDir)//'waterlevels2_0809.dat', STATUS = 'UNKNOWN')!input raw and 5-d averaged hourly water levels from Atchafalaya Delta, 3/1/2008-2/23/09 (Station ID 8764227)(m)
OPEN (83, file = trim(waterDir)//'waterlevels3_0910.dat', STATUS = 'UNKNOWN')!input raw and 5-d averaged hourly water levels from Atchafalaya Delta, 3/1/2009-2/23/10 (Station ID 8764227)(m)
OPEN (84, file = trim(waterDir)//'waterlevels4_1011.dat', STATUS = 'UNKNOWN')!input raw and 5-d averaged hourly water levels from Atchafalaya Delta, 3/1/2010-2/23/11 (Station ID 8764227)(m)
OPEN (85, file = trim(waterDir)//'waterlevels5_0607.dat', STATUS = 'UNKNOWN')!input raw and 5-d averaged hourly water levels from Atchafalaya Delta, 3/1/2006-2/23/07 (Station ID 8764227)(m)

!--open output files---------------------------------------------------
OPEN (12, file = trim(outputDir)//'out_input_echo.out', STATUS = 'UNKNOWN')      !output all input and parameter values
OPEN (22, file = trim(outputDir)//'out_indivs_alive.out', STATUS = 'UNKNOWN')    !output traits of alive indivs
OPEN (33, file = trim(outputDir)//'out_allocatedspace.out', STATUS = 'UNKNOWN')  !output space allocated or wasted
OPEN (44, file = trim(outputDir)//'out_stage_abunds.out', STATUS = 'UNKNOWN')    !output abundance in ea stage and sp
OPEN (55, file = trim(outputDir)//'out_gridelevation.m', STATUS = 'UNKNOWN')     !output elevation (m) of ea cell
OPEN (66, file = trim(outputDir)//'out_griddepth.m', STATUS = 'UNKNOWN')         !output water depth (m) of ea cell
OPEN (77, file = trim(outputDir)//'out_gridinundhrs.m', STATUS = 'UNKNOWN')      !output total hours inundated in a year for ea cell
OPEN (99, file = trim(outputDir)//'out_gridfishbycell.out', STATUS = 'UNKNOWN')  !output sp-specific totals for superindiv, worth (abundance), and biomass in ea cell
OPEN (100, file = trim(outputDir)//'out_gridhabitat.m', STATUS = 'UNKNOWN')      !output habitat type in ea cell
OPEN (110, file = trim(outputDir)//'out_indivs_added.out', STATUS = 'UNKNOWN')   !output initial attributes of all indivs that are allocated (recruited) or initialized
OPEN (120, file = trim(outputDir)//'out_indivs_dead.out', STATUS = 'UNKNOWN')    !output traits of dead indivs (nat mort, starved, stranded, predated, old)
OPEN (130, file = trim(outputDir)//'out_indivs_move.out', STATUS = 'UNKNOWN')    !output all movement info (including sub-hour) for up to 5 user-defined indivs
OPEN (140, file = trim(outputDir)//'out_stage_means.out', STATUS = 'UNKNOWN')    !output mean length, weight, and density of ea stage and sp
OPEN (150, file = trim(outputDir)//'out_indivs_diet.out', STATUS = 'UNKNOWN')    !output indiv diets: g wwt consumed by prey type, species, stage
OPEN (160, file = trim(outputDir)//'out_gridpreybycell.out', STATUS = 'UNKNOWN') !output abundance and biomass of 5 base prey types by unit (m**2 or m**3) and cell
OPEN (170, file = trim(outputDir)//'out_gridpreymeans.out', STATUS = 'UNKNOWN')  !output mean abundance and biomass (#/m**2 or m**3 and cell) of 5 base prey types by hab type and grid
OPEN (180, file = trim(outputDir)//'out_lifetable.out', STATUS = 'UNKNOWN')      !output life table by sp and stage
OPEN (190, file = trim(outputDir)//'out_summary_diet.out', STATUS = 'UNKNOWN')   !output mean diet by sp and prey type
OPEN (200, file = trim(outputDir)//'out_summary_habdist.out', STATUS = 'UNKNOWN')!output mean abund-based habitat distribn by sp and hab type
OPEN (210, file = trim(outputDir)//'out_summary_maxden.out', STATUS = 'UNKNOWN') !output highest density (worth/m**2) on grid by sp
OPEN (220, file = trim(outputDir)//'out_summary_worthlost.out', STATUS = 'UNKNOWN')!output worth lost by sp to nat mort, starvation, stranding, predation
OPEN (230, file = trim(outputDir)//'out_summary_profiling.out', STATUS = 'UNKNOWN')!output worth lost by sp to nat mort, starvation, stranding, predation
OPEN (240, file = trim(outputDir)//'out_gridproductivity.out', STATUS = 'UNKNOWN')!output net productivity (g wwt = weight lost or gained or lost * worth) by species and cell)
OPEN (250, file = trim(outputDir)//'out_waterprofile.out', STATUS = 'UNKNOWN')!output net productivity (g wwt = weight lost or gained or lost * worth) by species and cell)
OPEN (260, file = trim(outputDir)//'out_annualbiomass.out', STATUS = 'UNKNOWN')!output net productivity (g wwt = weight lost or gained or lost * worth) by species and cell)
OPEN (300, file = trim(outputDir)//'out_spawnloss.out', STATUS = 'UNKNOWN')!output net productivity (g wwt = weight lost or gained or lost * worth) by species and cell)
!DEC$ IF DEFINED(TRACE_INDIVIDUALS)
OPEN (310, file = trim(outputDir)//'out_spawnlosssummary.out', STATUS = 'UNKNOWN')!output net productivity (g wwt = weight lost or gained or lost * worth) by species and cell)
!DEC$ ENDIF
!--write output headers------------------------------------------------
WRITE (22, 222)
222 FORMAT ('year', 1X, 'day', 1X, 'cum_day', 1X, 'hour', 1X, 'cum_hr', 1X, 'indiv', 1X, 'fw_sp', 1X, 'sp_id', 1X, 'age_yrs', 1X, &
			'age_dys', 6X, 'worth', 1X, 'alive', 1X, 'stage', 7X, 'cmax', 1X, 'p_cmax', 3X, &
			'som_wght', 3X, 'som_chng', 3X, 'gon_wght', 3X, 'gon_chng', 3X, 'tot_wght', 3X, &
			'tot_chng', 5X, 'length', 3X, 'len_chng', 7X, 'M/hr', 1X, 'mature', 1X, 'spawn_jday', 1X, 'fec_flag', 2X, &
			'breed_som', 1X, 'trgt_invst', 1X, 'reprod_invst', 3X, 'btch_fec', 2X, 'btch_wght', 2X, &
			'egg_prodn', 1X, 'btch_cnt', 1X, 'col', 1X, 'row', 1X, 'x_dist', 1X, 'y_dist', 1X, 'depth', 1X, 'habitat', 1X, 'senesc', &
			1X, 'mindep', 4X, 'strwght', 2X, 'spawnloss')
WRITE (33, 333)
333 FORMAT ('year', 1X, 'day', 1X, 'hour', 1X, 'tot_space', 1X, 'estab_indivs', 1X, &
			'new_indivs', 1X, 'avail_space', 1X, 'split_space', 1X, 'unused_space', 1X 'water_temperature')
WRITE (44, 444)
444 FORMAT ('year', 1X, 'cum_day', 1X, 'hour', 1X, 'fw_sp', 1X, 'sp_id', 1X, 'dayeggprod', 1X, 'alive_eggs', 2X, &
			'yolk_larv', 2X, 'feed_larv', 7X, 'age0', 7X, 'age1', 7X, 'age2', 6X, &
			'total', 2X, 'avenewrth', 2X, 'senescent_count', 1X, 'senescent_worth', 1X, 'not_spawningday')
WRITE (99, 999)
999 FORMAT ('year', 1X, 'day', 1X, 'hour', 1X, 'fw_sp', 1X, 'sp_id', 2X, 'col', 2X, 'row', 1X, &
			'habitat', 1X, 'indivs', 2X, 'abundance', 4X, 'biomass')
WRITE (110, 1110)
1110 FORMAT ('year', 1X, 'day', 1X, 'hour', 1X, 'ass_type', 1X, 'indiv', 1X, 'fw_sp', 1X, 'sp_id', 1X, &
			'age_yrs', 1X, 'age_dys', 6X, 'worth', 1X, 'alive', 1X, 'stage', 1X, 'mature', 3X, &
			'som_wght', 3X, 'gon_wght', 3X, 'tot_wght', 5X, 'length', 7X, 'M/hr', 1X, &
			'col', 1X, 'row', 1X, 'x_dist', 1X, 'y_dist', 1X, 'habtype', 1X, 'depth')
WRITE (120, 2220)
2220 FORMAT ('year', 1X, 'day', 1X, 'cum_day', 1X, 'hour', 1X, 'cum_hr', 1X, 'indiv', 1X, 'fw_sp', 1X, 'sp_id', 1X, 'age_yrs', 1X, &
			'age_dys', 6X, 'worth', 1X, 'alive', 1X, 'stage', 7X, 'cmax', 1X, 'p_cmax', 3X, &
			'som_wght', 3X, 'som_chng', 3X, 'gon_wght', 3X, 'gon_chng', 3X, 'tot_wght', 3X, &
			'tot_chng', 5X, 'length', 3X, 'len_chng', 7X, 'M/hr', 1X, 'mature', 1X, 'spawn_jday', 1X, 'fec_flag', 2X, &
			'breed_som', 1X, 'trgt_invst', 1X, 'reprod_invst', 3X, 'btch_fec', 2X, 'btch_wght', 2X, &
			'egg_prodn', 1X, 'btch_cnt', 1X, 'col', 1X, 'row', 1X, 'x_dist', 1X, 'y_dist', 1X, 'depth', 1X, 'habitat')

WRITE (130, 3330)
3330 FORMAT ('year', 1X, 'day', 1X, 'cum_day', 1X, 'hour', 1X, 'cum_hr', 1X, 'timesteps', 1X, &
			'timestep', 1X, 'indiv', 1X, 'fw_sp', 1X, 'sp_id', 1X, 'age_yrs', 1X, 'age_dys', 1X, &
			'col', 1X, 'row', 1X, 'x_dist', 1X, 'y_dist', 1X, 'wtr_dep', 1X, 'min_dep', 1X, 'hab', 5X, &
			'length', 1X, 'nhood', 1X, 'nhd_flag', 4X, 'fittest', 1X, 'nearest', 1X, &
			'deepest', 1X, 'target_c', 1X, 'target_r')
WRITE (140, 4440)
4440 FORMAT ('year', 1X, 'cum_day', 1X, 'hour', 1X, 'fw_sp', 1X, 'sp_id', 5X, &
			'age0_p', 5X, 'age1_p', 5X, 'age2_p', 3X, &
			'age0_len', 3X, 'age1_len', 3X, 'age2_len', 4X, 'max_len', 3X, &
			'age0_wgt', 3X, 'age1_wgt', 3X, 'age2_wgt')

WRITE (150, 5550)
5550 FORMAT ('year', 1X, 'day', 1X, 'cum_day', 1X, 'hour', 1X, 'cum_hr', 1X, 'indiv', 1X, &
			'fw_sp', 1X, 'sp_id', 1X, 'stage', 6X, 'worth', 3X, 'tot_wght', 5X, 'length', 4X, &
			'S_benth', 4X, 'M_benth', 4X, 'L_benth', 5X, 'S_zoop', 5X, 'M_zoop')
WRITE (160, 6660)
6660 FORMAT ('year', 1X, 'day', 1X, 'hour', 1X, 'preytype', 1X, 'col', 1X, 'row', 1X, 'habitat', 4X, &
			'#/m2or3', 4X, 'g/m2or3')
WRITE (170, 7770)
7770 FORMAT ('year', 1X, 'day', 1X, 'hour', 1X, 'preytype', 1X, 'hab(last=grid)', 1X, &
			'ave_#/m2or3', 1X, 'ave_g/m2or3')
WRITE (180, 8880)
8880 FORMAT ('year', 1X, 'fw_sp', 1X, 'sp_id', 1X, 'stage', 3X, 'ave_durn', 6X, &
			'enter', 6X, 'leave', 1X, 'S_over_durn', 4X, 'daily_Z')
WRITE (190, 9990)
9990 FORMAT ('year', 1X, 'cum_day', 1X, 'hour', 1X, 'fw_sp', 1X, 'sp_id', 6X, 'tot_g', 4X, &
			'S_benth', 4X, 'M_benth', 4X, 'L_benth', 5X, &
			'S_zoop', 5X, 'M_zoop')
WRITE (200, 2222)
2222 FORMAT ('year', 1X, 'cum_day', 1X, 'hour', 1X, 'fw_sp', 1X, 'sp_id', 3X, 'mean_dep', 4X, &
			'opn_wtr', 8X, 'SAV', 6X, 'lo_em', 6X, &
			'hi_em', 6X, 'woody', 7X, 'bare')
WRITE (210, 2120)
2120 FORMAT ('year', 1X, 'day', 1X, 'hour', 1X, 'fw_sp', 1X, 'sp_id', 1X, 'col', 1X, 'row', 1X, &
			'depth', 1X, '#/cell', 4X, 'fish/m2', 7X, 'g/m2')
WRITE (220, 2201)
2201 FORMAT ('year', 1X, 'day', 1X, 'hour', 1X, 'fw_sp', 1X, 'sp_id', 6X, 'natrl', 6X, 'starv', 6X, &
			'strnd', 6X, 'predn', 4x, 'senesce')
WRITE (230, 2301)
2301 FORMAT ('year', 1X, 'day', 1X, 'hour', 1X, 'stage', 1X, 'time')
WRITE (240, 2401)
2401 FORMAT ('year', 1X, 'fw_sp', 1X, 'col', 1X, 'row', 3X, 'net_prod', 4X, 'roth_net_prod')
WRITE (250, 2501)
2501 FORMAT ('year', 1X, 'col', 1X, 'row', 1X 'depth', 1X, 'drytowet', 1X, 'wettodry')
WRITE (260, 2601)
2601 FORMAT ('year', 1X, 'fw_sp', 1X, 'sp_id', 4X, 'biomass', 2X, 'spawnloss', 2X, 'specieswghtdiff', 2X 'specieswghtandworthdiff', 2X, 'avgweight', 2x, 'avgweightandworth', 2X, 'starvationtrophictransfernotsenescent', 2X, 'starvationtrophictransfersenescent', 2X, 'strandingtrophictransfernotsenescent', 2X, 'strandingtrophictransfersenescent', 2X, 'naturalmortalitytrophictransfernotsenescent', 2X, 'naturalmortalitytrophictransfersenescent', 2X, 'livingindivs', 2X, 'weightchanges')
WRITE (300, 3001)
3001 FORMAT ('year', 1X, 'day', 1X, 'cum_day', 1X, 'hour', 1X, 'cum_hr', 1X, 'indiv', 1X, 'fw_sp', 1X, 'sp_id', 2X, 'spawnloss', 2X, 'btch_wght')
!--parameterize habitat------------------------------------------------
!--1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh)

  watsched = (/ 1, 4, 5, 1, 4, 2, 1, 4, 3 /) !define schedule of water years
											 !(1=07/08(ave), 2=08/09(wet), 3=09/10(wet), 4=10/11(ave), 5=06/07(dry))
  zerolevel=0.465D0                    !m (increase to make delta wetter for habitat types)
  addwater=0.2D0                       !m (increase to make delta wetter for biology but not hab types)
  inundmax=8640                        !hours

  minprob=0.01D0                       !smallest habitat probty of interest - values below threshold are ignored
  maxSAV=0.6D0                         !deepest water depth at which SAV(2) are found (m)
  pdfa(1)=0.D0; pdfa(2)=0.0945081D0; pdfa(3)=0.0321531D0; pdfa(4)=0.002D0;         pdfa(5)=1.D0;    pdfa(6)=0.007D0
  pdfb(1)=0.D0; pdfb(2)=8736.4587D0; pdfb(3)=6300.D0;     pdfb(4)=1900.D0;         pdfb(5)=0.D0;    pdfb(6)=8700.D0
  pdfc(1)=0.D0; pdfc(2)=19633.478D0; pdfc(3)=1895442.D0;  pdfc(4)=611572290000.D0; pdfc(5)=8700.D0; pdfc(6)=-650000000000.D0
  pdfd(1)=0.D0; pdfd(2)=81.192998D0; pdfd(3)=7151.3901D0; pdfd(4)=3.4209558D0;     pdfd(5)=3.D0;    pdfd(6)=15.D0
  pdfe(1)=0.D0; pdfe(2)=35.216598D0; pdfe(3)=16.D0;       pdfe(4)=778201830.D0;    pdfe(5)=230.D0;  pdfe(6)=1300000000.D0

!--parameterize prey base----------------------------------------------
!--1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop

  minpreyden=0.01D0 !minimum density of prey left in cell after foraging by indivs (g wwt/m**2 or 3)

  preya(1)=0.0249D0; preya(2)=0.0088D0; preya(3)=-0.1207D0; preya(4)=0.0329D0;  preya(5)=0.0329D0   !(all Sable et al in prep)
  preyb(1)=0.0044D0; preyb(2)=0.0039D0; preyb(3)=0.1266D0;  preyb(4)=0.00069D0; preyb(5)=0.00069D0
  preyc(1)=4.503D0;  preyc(2)=5.959D0;  preyc(3)=1.3005D0;  preyc(4)=4.02D0;    preyc(5)=4.02
  preyd(1)=355.38D0; preyd(2)=658.11D0; preyd(3)=4057.6D0;  preyd(4)=355.82D0;  preyd(5)=355.82D0

  indpreywght(1)=0.000225D0; indpreywght(2)=0.003D0; indpreywght(3)=0.08D0     !g wwt (Sable et al in prep)
   indpreywght(4)=0.00000954D0; indpreywght(5)=0.0000795D0
  initpreynum(1)=25000.D0; initpreynum(2)=2500.D0; initpreynum(3)=22.D0        !#/m**2 for benth, #/m**3 for zoop (Sable et al in prep)
   initpreynum(4)=179300.D0; initpreynum(5)=19000.D0

  preyenerden(1)=4429.D0; preyenerden(2)=3690.D0; preyenerden(3)=5598.D0; preyenerden(4)=2250.D0; preyenerden(5)=3684.D0 !(Joules/gwet) (Sable et al in prep)

  !multipliers of eqbm prey density ('initpreynum') by hab type (rows) and prey type (columns)
  !(1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh))
  !prey 1              prey 2               prey 3               prey 4               prey 5
  preymult(1,1)=0.6D0; preymult(2,1)=0.6D0; preymult(3,1)=0.6D0; preymult(4,1)=1.3D0; preymult(5,1)=1.3D0 !hab type 1
  preymult(1,2)=0.7D0; preymult(2,2)=0.7D0; preymult(3,2)=0.7D0; preymult(4,2)=1.1D0; preymult(5,2)=1.1D0 !hab type 2
  preymult(1,3)=1.D0;  preymult(2,3)=1.D0;  preymult(3,3)=1.D0;  preymult(4,3)=0.5D0; preymult(5,3)=0.5D0 !hab type 3
  preymult(1,4)=1.1D0; preymult(2,4)=1.1D0; preymult(3,4)=1.1D0; preymult(4,4)=0.2D0; preymult(5,4)=0.2D0 !hab type 4
  preymult(1,5)=0.01D0;preymult(2,5)=0.01D0;preymult(3,5)=0.01D0;preymult(4,5)=0.01D0;preymult(5,5)=0.01D0 !hab type 5
  preymult(1,6)=0.8D0; preymult(2,6)=0.8D0; preymult(3,6)=0.8D0; preymult(4,6)=1.D0;  preymult(5,6)=1.D0  !hab type 6

!--parameterize species/biology----------------------------------------
!--1=shrimp, 2=silverside, 3=killifish, 4=anchovy

 minworth=0.0001D0          !minimum worth (same for all sp)
 calorcoeff=13556.D0        !calorific coefficient (same for all sp) (Joules / g O2)

 DO i = 1, nfwsp            !loop over food web species

  !SHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMPSHRIMP
  IF (speciesid(i) .EQ. 1) THEN !if species i is 1 (shrimp), extract the following parameter values
   !store position of sp in all 'nfwsp'-dimensionsed arrays (so extract corrected param value)
	 spposn(1)=i
   !stage-specific nat inst mortality (calibrated) (hourly)
	 earlymort(1,i)=0.0014D0; earlymort(2,i)=0.D0; earlymort(3,i)=0.001808D0 !egg, yolk, feed larv
   !predation as hap-specific multipliers of nat inst mort. Identical for stages YOY, juv, adult
   !(1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh))
	 mortmult(i,1)=2.5D0;  mortmult(i,2)=1.75D0; mortmult(i,3)=0.85D0; mortmult(i,4)=0.75D0; mortmult(i,5)=3.D0; mortmult(i,6)=1.1D0
   !hab type in which to deposit new (init and recruit) indivs
	 inithab(i)=2 !SAV
   !cmax scalar, expo, optt (oC), maxt (oC), theta (Sable et al in prep)
	 cmaxscalar(i)=0.13775D0; cmaxexpo(i)=-0.16455D0; cmaxoptt(i)=29.D0; cmaxmaxt(i)=35.D0; cmaxtheta(i)=2.D0
   !proportion of maximum daily consumption (cmax) - used solely to check bioenergetics and calibrate k in fucntional response (g / g/ day)
	 pvalue(i)=0.55D0
   !vulnerability of benth (1,2,3=S,M,L) and zoop (4,5=S,L) prey to pred (0=invuln, 1=vuln)
	 preyv(i,1)=1; preyv(i,2)=1; preyv(i,3)=0; preyv(i,4)=0; preyv(i,5)=0
   !half saturation constants for benth (1,2,3=S,M,L) and zoop (4,5=S,L) prey (calibrated) (g wwt / cell)
	 preyk(i,1)=25.D0; preyk(i,2)=111.D0; preyk(i,3)=0.D0; preyk(i,4)=0.D0; preyk(i,5)=0.D0
   !scalars for egestion, excretion, specific dynamic action (Sable et al in prep)
	 egscalar(i)=0.17D0; egexpo(i)=0.D0; exscalar(i)=0.02D0; sdascalar(i)=0.1D0 !'egexpo' only used for species ID 4 (anchovy)
   !respiration scalar, expo, optt (oC), maxt (oC), theta, act (Sable et al in prep)
	 respscalar(i)=0.013967D0; respexpo(i)=-0.1844D0; respoptt(i)=31.D0; respmaxt(i)=36.D0; resptheta(i)=2.25D0; act(i)=1.D0
   !predator energy density  (Sable et al in prep) (Joules/g wwt)
	 predenerden(i)=3832.9D0
   !initialization wght for all age classes. YOY wght will be assigned in 'recruitment' (calibrated) (g wwt)
   !CAUTION: number of age classes must equal 'ageclasses(i)'
	 initwt(i,1)=0.D0; initwt(i,2)=0.1725D0
   !starting wght of feeding larvae (Sable et al in prep) (g wwt)
	 metmorphwt(i)=0.011D0
   !scalar and expo of the length-wght relation (Sable et al in prep)
	 lw_scalar(i)=0.00002D0; lw_expo(i)=3.025D0
   !length at or above which indiv is mature (Sable et al in prep) (mm)
	 matlen(i)=20.D0
   !breeder type (0=income breeder, 1=capital breeder)
	 breedtype(i)=0
   !minimum and maximum spawning jday (from Pattillo et al 1997)
	 minspwnday(i)=1; maxspwnday(i)=240  !March 1 to Oct 30
   !terms a, b, and c of the fecundity-total wght relation (Sable et al in prep)
	 feca(i)=124.23D0; fecb(i)=540.94D0; fecc(i)=1.765D0 !"c" only used for species ID 1 (shrimp)
   !number of batches (Sable et al in prep)
	 batches(i)=3
   !mean egg wght (Sable code) (g wwt)
	 eggwght(i)=0.000368D0
   !coeff 'Da' of fractional, temp-based cohort dev. funct for early stages (1=eggs,2=yolk,3=feeding)
	 fracta(i,1)=1139.6D0; fracta(i,2)=0.D0; fracta(i,3)=19505.D0 !(Sable et al in prep)
   !coeff 'Dc' of fractional, temp-based cohort dev. funct for early stages (1=eggs,2=yolk,3=feeding)
	 fractc(i,1)=0.0518D0; fractc(i,2)=0.D0; fractc(i,3)=0.1295D0 !(Sable et al in prep)
   !search radius when fitness movement (cells), multiplier of search radius when emergency movement
	 nhood(i)=2; emermult(i)=5
   !error rate in fitness calc (0.01 = 1%)
	 fiterror(i)=0.01D0
  END IF !species 1

  !SILVERSIDESILVERSIDESILVERSIDESILVERSIDESILVERSIDESILVERSIDESILVERSIDESILVERSIDESILVERSIDESILVERSIDE
  IF (speciesid(i) .EQ. 2) THEN !if species i is 2 (silverside), extract the following parameter values
   !store position of sp in all 'nfwsp'-dimensionsed arrays (so extract corrected param value)
	 spposn(2)=i
   !stage-specific nat inst mortality (calibrated) (hourly)
	 earlymort(1,i)=0.007D0; earlymort(2,i)=0.00316D0; earlymort(3,i)=0.001673D0 !egg, yolk, feed larv
   !predation as hap-specific multipliers of nat inst mort. Identical for stages YOY, juv, adult
   !(1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh))
	mortmult(i,1)=0.9D0; mortmult(i,2)=0.9D0; mortmult(i,3)=1.3D0; mortmult(i,4)=1.5D0; mortmult(i,5)=5.D0;  mortmult(i,6)=1.3D0
   !hab type in which to deposit new (init and recruit) indivs
	 inithab(i)=1 !SAV
   !cmax scalar, expo, optt (oC), maxt (oC), theta (Sable et al in prep)
	 cmaxscalar(i)=0.325D0; cmaxexpo(i)=-0.43D0; cmaxoptt(i)=31.D0; cmaxmaxt(i)=34.D0; cmaxtheta(i)=2.5D0
   !proportion of maximum daily consumption (cmax) - used solely to check bioenergetics and calibrate k in fucntional response (g / g/ day)
	 pvalue(i)=0.5D0
   !vulnerability of benth (1,2,3=S,M,L) and zoop (4,5=S,L) prey to pred (0=invuln, 1=vuln)
	 preyv(i,1)=0; preyv(i,2)=0; preyv(i,3)=0; preyv(i,4)=1; preyv(i,5)=1
   !half saturation constants for benth (1,2,3=S,M,L) and zoop (4,5=S,L) prey (calibrated) (g wwt / cell)
	 preyk(i,1)=0.D0; preyk(i,2)=0.D0; preyk(i,3)=0.D0; preyk(i,4)=1.2D0; preyk(i,5)=12.D0
   !scalars for egestion, excretion, specific dynamic action (Sable et al in prep)
	 egscalar(i)=0.01D0; egexpo(i)=0.D0; exscalar(i)=0.01D0; sdascalar(i)=0.1D0 !'egexpo' only used for species ID 4 (anchovy)
   !respiration scalar, expo, optt (oC), maxt (oC), theta, act (Sable et al in prep)
	 respscalar(i)=0.025835D0; respexpo(i)=-0.297D0; respoptt(i)=33.D0; respmaxt(i)=36.D0; resptheta(i)=2.25D0; act(i)=1.2D0
   !predator energy density (Sable et al in prep) (Joules/g wwt)
	 predenerden(i)=6756.D0
   !initialization wght for all age classes. YOY wght will be assigned in 'recruitment' (calibrated) (g wwt)
   !CAUTION: number of age classes must equal 'ageclasses(i)'
	 initwt(i,1)=0.D0; initwt(i,2)=0.5575D0
   !starting wght of feeding larvae (Sable et al in prep) (g wwt)
	 metmorphwt(i)=0.022D0
   !scalar and expo of the length-wght relation (Sable et al in prep)
	 lw_scalar(i)=0.000006D0; lw_expo(i)=2.95D0
   !length at or above which indiv is mature (Sable et al in prep) (mm)
	 matlen(i)=45.D0
   !breeder type (0=income breeder, 1=capital breeder)
	 breedtype(i)=1
   !minimum and maximum spawning jday (from Pattillo et al 1997)
	 minspwnday(i)=1; maxspwnday(i)=210  !March 1 to Sept 30
   !terms a, b, and c of the fecundity-total wght relation (Sable et al in prep)
	 feca(i)=29.63D0; fecb(i)=0.0367D0; fecc(i)=0.D0 !"c" only used for species ID 1 (shrimp)
   !number of batches (Sable et al in prep)
	 batches(i)=6
   !mean egg wght (Sable code) (g wwt)
	 eggwght(i)=0.00038D0
   !coeff 'Da' of fractional, temp-based cohort dev. funct for early stages (1=eggs,2=yolk,3=feeding)
	 fracta(i,1)=1502.D0; fracta(i,2)=72.D0; fracta(i,3)=697.D0 !(Sable et al in prep)
   !coeff 'Dc' of fractional, temp-based cohort dev. funct for early stages (1=eggs,2=yolk,3=feeding)
	 fractc(i,1)=0.0768D0; fractc(i,2)=0.0D0; fractc(i,3)=0.0348D0 !(Sable et al in prep)
   !search radius when fitness movement (cells), multiplier of search radius when emergency movement
	 nhood(i)=2; emermult(i)=5
   !error rate in fitness calc (0.01 = 1%)
	 fiterror(i)=0.01D0
  END IF !species 2

  !KILLIFISHKILLIFISHKILLIFISHKILLIFISHKILLIFISHKILLIFISHKILLIFISHKILLIFISHKILLIFISHKILLIFISHKILLIFISH
  IF (speciesid(i) .EQ. 3) THEN !if species i is 3 (killifish), extract the following parameter values
   !store position of sp in all 'nfwsp'-dimensionsed arrays (so extract corrected param value)
	 spposn(3)=i
   !stage-specific nat inst mortality (calibrated) (hourly)
	 earlymort(1,i)=0.004D0; earlymort(2,i)=0.001537D0; earlymort(3,i)=0.001064D0 !egg, yolk, feed larv
   !predation as hap-specific multipliers of nat inst mort. Identical for stages YOY, juv, adult
   !(1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh))
	 mortmult(i,1)=5.D0; mortmult(i,2)=0.5D0; mortmult(i,3)=0.5D0; mortmult(i,4)=2.D0; mortmult(i,5)=5.D0; mortmult(i,6)=2.D0
   !hab type in which to deposit new (init and recruit) indivs
	 inithab(i)=2 !SAV
   !cmax scalar, expo, optt (oC), maxt (oC), theta (Sable et al in prep)
	 cmaxscalar(i)=0.2D0; cmaxexpo(i)=-0.25D0; cmaxoptt(i)=29.D0; cmaxmaxt(i)=35.D0; cmaxtheta(i)=2.22D0
   !proportion of maximum daily consumption (cmax) - used solely to check bioenergetics and calibrate k in fucntional response (g / g/ day)
	 pvalue(i)=0.42D0
   !vulnerability of benth (1,2,3=S,M,L) and zoop (4,5=S,L) prey to pred (0=invuln, 1=vuln)
	 preyv(i,1)=1; preyv(i,2)=1; preyv(i,3)=1; preyv(i,4)=0; preyv(i,5)=0
   !half saturation constants for benth (1,2,3=S,M,L) and zoop (4,5=S,L) prey (calibrated) (g wwt / cell)
	 preyk(i,1)=100.D0; preyk(i,2)=67.D0; preyk(i,3)=7.D0; preyk(i,4)=0.D0; preyk(i,5)=0.D0
   !scalars for egestion, excretion, specific dynamic action (Sable et al in prep)
	 egscalar(i)=0.1D0; egexpo(i)=0.D0; exscalar(i)=0.06D0; sdascalar(i)=0.1D0 !'egexpo' only used for species ID 4 (anchovy)
   !respiration scalar, expo, optt (oC), maxt (oC), theta, act (Sable et al in prep)
	 respscalar(i)=0.02D0; respexpo(i)=-0.17D0; respoptt(i)=32.D0; respmaxt(i)=36.D0; resptheta(i)=2.25D0; act(i)=1.25D0
   !predator energy density  (Sable et al in prep) (Joules/g wwt)
	 predenerden(i)=5020.8D0
   !initialization wght for all age classes. YOY wght will be assigned in 'recruitment' (calibrated) (g wwt)
   !CAUTION: number of age classes must equal 'ageclasses(i)'
	 initwt(i,1)=0.D0; initwt(i,2)=2.056D0; initwt(i,3)=3.5D0
   !starting wght of feeding larvae (Sable et al in prep) (g wwt)
	 metmorphwt(i)=0.077D0
   !scalar and expo of the length-wght relation (Sable et al in prep)
	 lw_scalar(i)=0.000026434D0; lw_expo(i)=2.8787D0
   !length at or above which indiv is mature (from Sable et al in prep) (mm)
	 matlen(i)=50.D0
   !breeder type (0=income breeder, 1=capital breeder)
	 breedtype(i)=0
   !minimum and maximum spawning jday (from Pattillo et al 1997)
	 minspwnday(i)=1; maxspwnday(i)=210  !March 1 to Sept 30
   !terms a, b, and c of the fecundity-total wght relation (Sable et al in prep)
	 feca(i)=21.87D0; fecb(i)=7.721D0; fecc(i)=0.D0 !"c" only used for species ID 1 (shrimp)
   !number of batches (Sable et al in prep)
	 batches(i)=10
   !mean egg wght (Sable code) (g wwt)
	 eggwght(i)=0.000438D0
   !coeff 'Da' of fractional, temp-based cohort dev. funct for early stages (1=eggs,2=yolk,3=feeding)
	 fracta(i,1)=1435.3D0; fracta(i,2)=72.D0; fracta(i,3)=795.6D0 !(Sable et al in prep)
   !coeff 'Dc' of fractional, temp-based cohort dev. funct for early stages (1=eggs,2=yolk,3=feeding)
	 fractc(i,1)=0.0607D0; fractc(i,2)=0.D0; fractc(i,3)=0.0373D0 !(Sable et al in prep)
   !search radius when fitness movement (cells), multiplier of search radius when emergency movement
	 nhood(i)=2; emermult(i)=5
   !error rate in fitness calc (0.01 = 1%)
	 fiterror(i)=0.01D0
  END IF !species 3

  !ANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVYANCHOVY
  IF (speciesid(i) .EQ. 4) THEN !if species i is 5 (anchovy), extract the following parameter values
   !store position of sp in all 'nfwsp'-dimensionsed arrays (so extract corrected param value)
	 spposn(4)=i
   !stage-specific nat inst mortality (calibrated) (hourly) (after Rose et al. 1999 except for feed larv, which is Sable et al. in prep)
	 earlymort(1,i)=0.24D0; earlymort(2,i)=0.00354D0; earlymort(3,i)=0.001036D0 !egg, yolk, feed larv
   !predation as hap-specific multipliers of nat inst mort. Identical for stages YOY, juv, adult
   !(1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh))
	 mortmult(i,1)=0.9D0; mortmult(i,2)=1.D0; mortmult(i,3)=1.25D0; mortmult(i,4)=1.5D0; mortmult(i,5)=3.D0; mortmult(i,6)=1.25D0
   !hab type in which to deposit new (init and recruit) indivs
	 inithab(i)=1 !deep, open water
   !cmax scalar, expo, optt (oC), maxt (oC), theta (Sable et al in prep)
	 cmaxscalar(i)=0.41D0; cmaxexpo(i)=-0.33D0; cmaxoptt(i)=29.D0; cmaxmaxt(i)=37.D0; cmaxtheta(i)=2.22D0
   !proportion of maximum daily consumption (cmax) - used solely to check bioenergetics and calibrate k in functional response (g / g/ day)
	 pvalue(i)=0.6D0
   !vulnerability of benth (1,2,3=S,M,L) and zoop (4,5=S,L) prey to pred (0=invuln, 1=vuln)
	 preyv(i,1)=0; preyv(i,2)=0; preyv(i,3)=0; preyv(i,4)=1; preyv(i,5)=1
   !half saturation constants for benth (1,2,3=S,M,L) and zoop (4,5=S,L) prey (calibrated) (g wwt / cell)
	 preyk(i,1)=0.D0; preyk(i,2)=0.D0; preyk(i,3)=0.D0; preyk(i,4)=20.D0; preyk(i,5)=6.D0
   !scalars for egestion, excretion, specific dynamic action (Sable et al in prep)
	 egscalar(i)=0.77D0; egexpo(i)=-0.4D0; exscalar(i)=0.15D0; sdascalar(i)=0.1D0 !'egexpo' only used for species ID 4 (anchovy)
   !respiration scalar, expo, optt (oC), maxt (oC), theta, act (Sable et al in prep)
	 respscalar(i)=0.0115D0; respexpo(i)=-0.346D0; respoptt(i)=32.D0; respmaxt(i)=40.D0; resptheta(i)=2.25D0; act(i)=2.D0
   !predator energy density (Sable et al in prep) (Joules/g wwt)
	 predenerden(i)=4186.D0
   !initialization wght for all age classes. YOY wght will be assigned in 'recruitment' (calibrated) (g wwt)
   !CAUTION: number of age classes must equal 'ageclasses(i)'
	 initwt(i,1)=0.D0; initwt(i,2)=1.347D0; initwt(i,3)=2.6D0
   !starting wght of feeding larvae (Sable et al in prep) (g wwt)
	 metmorphwt(i)=0.064D0
   !scalar and expo of the length-wght relation (Sable et al in prep)
	 lw_scalar(i)=0.000026D0; lw_expo(i)=2.814D0
   !length at or above which indiv is mature (Sable et al in prep) (mm)
	 matlen(i)=45.D0
   !breeder type (0=income breeder, 1=capital breeder)
	 breedtype(i)=0
   !minimum and maximum spawning jday (from Pattillo et al 1997)
	 minspwnday(i)=1; maxspwnday(i)=225  !March 1 to Oct 15
   !terms a, b, and c of the fecundity-total wght relation (Zastrow et al. 1991)
	 feca(i)=304.79D0; fecb(i)=404.64D0; fecc(i)=0.D0 !"c" only used for species ID 1 (shrimp)
   !number of batches (Patillo et al. 1997: 1 brood every 4 days over a 225-day spawn season)
	 batches(i)=55
   !mean egg wght (g wwt) (estim from Zastrow et al. 1991 using relation betw hydrated ova and overy wght, a hydrated GSI of 16.83%, somatic mass betw 0.8 to 3.2 g)
	 eggwght(i)=0.0002D0
   !coeff 'Da' of fractional, temp-based cohort dev. funct for early stages (1=eggs,2=yolk,3=feeding)
	 fracta(i,1)=201.12D0; fracta(i,2)=150.D0; fracta(i,3)=14005.D0 !(1-2 from Rose et al 1999, 3 from Sable et al in prep)
   !coeff 'Dc' of fractional, temp-based cohort dev. funct for early stages (1=eggs,2=yolk,3=feeding)
	 fractc(i,1)=0.0856D0; fractc(i,2)=0.055D0; fractc(i,3)=0.114D0 !(1-2 from Rose et al 1999, 3 from Sable et al in prep)
   !search radius when fitness movement (cells), multiplier of search radius when emergency movement
	 nhood(i)=2; emermult(i)=5
   !error rate in fitness calc (0.01 = 1%)
	 fiterror(i)=0.01D0
  END IF !species 4
 END DO !food web species

if(narg .gt. argCounter) then
    argCounter=argCounter+1
    call GET_COMMAND_ARGUMENT(argCounter, paramfile)
    CALL read_parameter_data(paramfile)
end if

!--echo input----------------------------------------------------------
IF (fileflag(14) .EQ. 1) CALL inputecho

!--initialize physical environment-------------------------------------

CALL readelev   !read in elevation (m) of ea cell


IF (fileflag(3) .EQ. 1) THEN                 !if output desired
 outflag = 5; CALL output(0,0,0,0,0,0,0)                    !output cell elevations (5=file 55)
END IF

CALL readwater  !read in, convert, and relativize hourly water levels (m) for year

depthflag = 0   !flag that 'depth' called at start of sim
CALL depth      !use water level (m) in first hr to identify wet cells for use in 'initindiv' and 'recruitment'

watyr = 1       !indicate water year to use for first year
prevyear=year
year=1
CALL inundation !use hourly water levels for year to calc cell unundation (hrs/year)
year=prevyear
CALL habitats   !use Guerry Holms's elevation- and inundation-based rules to define habitat in ea cell

!--initialize individuals----------------------------------------------
CALL initindiv      !initialize indivs of all sp and age classes except for age-0 which are created in 'recruitment'
CALL recruitwindow  !calc first and last possible recruit day from first and last possilbe spawn day
feedhr = 0          !set feed hour to 0 (1 = 'minfeedhr')

extinctquery=0
!DEC$ IF (.NOT. DEFINED(_OPENMP))
PRINT *, "OpenMP not defined"
!DEC$ ELSE
PRINT *, "OpenMP defined"
!DEC$ ENDIF
!DEC$ IF DEFINED (TRACE_INDIVIDUALS)
PRINT *, "Individuals traced"
individualalive=-1
!DEC$ ELSE
PRINT *, "Individuals not traced"
!DEC$ ENDIF
!DEC$ IF DEFINED (TEST_MODE)
PRINT *, "Test mode enabled"
!DEC$ ELSE
PRINT *, "Test mode disabled"
!DEC$ ENDIF
!DEC$ IF DEFINED (TEST_BIOMASS)
PRINT *, "Biomass test enabled"
!DEC$ ELSE
PRINT *, "Biomass test disabled"
!DEC$ ENDIF
#ifdef C_MODE
    PRINT *, "C mode enabled"
#else
    PRINT *, "C mode disabled"
#endif
timeOffsetBetweenIterations=0
!--loop over years yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy start years
DO year = 1, nyears
 cohortabund=0.D0; cohortflag=0; cohortftemp=0.D0       !zero arrays assoc w/ development of early cohorts (eggs, yolks and feed larv) (from 'updatecohort')
 spwncnt=0; spwnid=0; spwnlen=0.D0; xspwnday=0.D0       !zero arrays assoc w/ spawning order (from 'spawners', 'spawnorder')
 xfec=0.D0; xeggprdn=0.D0; xbtchcnt=0                   !zero annual running tot of sp egg prodn, and batch count (from 'spawn')
 newyoyflag=0; recruitcount=0                           !zero recruit flag and counters (from 'updatecohort', 'recruitment')
 cummortloss=0.D0; netproductivity=0.D0                 !zero cumulative annual losses of worth
 rescuecount=0.D0
 lftbldurn=0.D0; lftblin=0.D0; lftbloutflag=0           !zero life table stage durations, number entering, leaving flag (from 'spawn', 'updatecohort', 'sumstages')
 productivityvalues=0.D0
annualbmass=0.D0
speciesspawnloss=0.D0
speciesweightdiff=0.D0

speciesweightdiffnumber=0
trophictransfer%withoutworth%starvation%notsenescent=0.D0
trophictransfer%withoutworth%starvation%senescent=0.D0
trophictransfer%withoutworth%stranding%notsenescent=0.D0
trophictransfer%withoutworth%stranding%senescent=0.D0
trophictransfer%withoutworth%naturalmortality%notsenescent=0.D0
trophictransfer%withoutworth%naturalmortality%senescent=0.D0
trophictransfer%withworth%starvation%notsenescent=0.D0
trophictransfer%withworth%starvation%senescent=0.D0
trophictransfer%withworth%stranding%notsenescent=0.D0
trophictransfer%withworth%stranding%senescent=0.D0
trophictransfer%withworth%naturalmortality%notsenescent=0.D0
trophictransfer%withworth%naturalmortality%senescent=0.D0
worthannualsumming=0.D0
worthtimesddwannualsumming=0.D0
 DO i = 1, nfwsp                    !loop over food web sp
  DO j = 1, 3+maxacctage            !loop over life stages
   IF (j .GE. 5) THEN               !if age-1 (i.e., in second year) or older
	lftblin(j,i) = lftblout(j-1,i)  !transfer number leaving previous stage to number entering current stage
   END IF !stage
  END DO !stages
 END DO !food web sp
 lftblout=0.D0                      !zero number leaving

 IF (year .GT. 1) CALL inundation   !use hourly water levels for year to calc cell unundation (hrs/year)
 IF (year .GT. 1) CALL habitats     !use Guerry Holms's elevation- and inundation-based rules to define habitat in ea cell

!--loop over julian days dddddddddddddddddddddddddddddddddddddddddddddd start days
DO jday = 1, ndays
 dayeggprdn=0.D0                                                                      !zero daily sp-specific egg production (from 'spawn')
 totestab=0; totnew=0; totavail=0; split=0; totsplit=0; totunused=0                   !zero allocated space counters
 avenewrth=0                                                                          !zero sp-specific daily record of average worth of new recruits (from 'recruitment')
 xcmax=0.D0; totcons=0.D0; realcons=0.D0; xsomchng=0.D0; xgonchng=0.D0; xtotchng=0.D0 !zero out bioenergetics
 xworthchng=0.D0
 xspawnloss=0.D0
 agep=0.D0; agemeanp=0.D0                                                             !zero sp- and age-specific sums and means related to p (from 'freecons' and 'sumstages')
 dietbase=0.D0; avediet=0.D0                                                          !zero daily running diet totals for output (from 'freecons')
 mortloss=0.D0
 notcalculatedspawningdays=0
 speciesweightandworthdiff=0.D0
 speciesworthdailysumming=0.D0
 xworthdailysumming=0.D0
 cumday = cumday + 1                !add to running total of simulated days

 CALL temperature                   !use julain day to calc water temperature across entire grid, turn on and off sp-specific spawn flag
 IF (jday .EQ. 255) CALL mudflats   !Nov 15, convert all SAV and low emergents (habitats 2 and 3) to unvegetated mud flats (habitat 6). Reset March 1
 CALL recruitment                   !recruit as indivs cohorts that, in prev. day, developed into YOY
!--loop over hours hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh start hours
DO hour = 1+timeOffsetBetweenIterations, nhours, timeOffset
 aveunitnum=0.D0; aveunitbmss=0.D0  !zero prey averages (from 'preydynam')
 cohorttot=0.D0                     !zero abundance by early life stage (egg, yolk, feed larv) and food web sp (from 'updatecohort')
 ageabund=0.D0; agelen=0.D0; agemeanlen=0.D0; maxlen=0.D0; agewgt=0.D0; agemeanwgt=0.D0 !zero sp- and (st)age-specific sums, means, maxes (from 'sumstages')
 indivbycell=0; worthbycell=0.D0; bmassbycell=0.D0; mxwrth=0.D0; maxcoord=0; habdist=0.D0; !zero fish-by-cell sums and max worth obs in hr, corresponding coord, and abund by sp and hab type (from 'fishbycell')
 !idbycell=0 
 fitflag=0; evalwet=0.D0            !zero flags indicating that exp fitness and future cell wetness have been evaluated and stored for re-use by indiv within hour

 cumhr = cumhr + 1                  !add to running total of simulated hours
 IF (hour .GE. minfeedhr .AND. hour .LE. maxfeedhr) feedhr = feedhr + 1 !keep track of hour in feed window
 IF (feedhr .GT. totfeedhr) feedhr = 1 !if new feeding window, reset 'feedhr'

 depthflag = 1                      !flag that 'depth' called from within hour loop
 CALL depth                         !use hourly water level (m) to calc cell depth (m) and vol (m**3)
 CALL preydynam                     !update prey abund and density by applying population growth rate to base prey (includes hab multipliers)

!--loop over individuals (1 of 2) i1i1i1i1i1i1i1i1i1i1i1i1i1i1i1i1i1i1i start indivs1
IF (jday .EQ. 1 .AND. hour .EQ. 1) PRINT *, "Looping over individuals..."

IF (hour .EQ. 24) THEN              !sumspace time
  DO i = 1, totindivs               !individuals sumspace
	CALL sumspace(i)                !if end of day, sum across indivs to determine space used, available, split
  END DO                            !individuals sumspace
END IF                              !sumspace time

CALL cpu_time(profilingstarttime)
! Section to parallelize with OpenMP verified threadprivate global variables for: maxcons, hoursplit,futuredepth, fitnhood,
!$OMP PARALLEL DO DEFAULT(SHARED) & 
!$OMP PRIVATE(i, j, ifwsp) &
!$OMP SCHEDULE(DYNAMIC)
DO i = 1, totindivs           !loop over indivs (for movement and some odds-and-ends)
 indiv = i                    !set indiv for subroutines that reference it (at least for output())
 ifwsp = spposn(xspecies(i))  !store sp-specific position of i in 'speciesid'
 IF (breedtype(ifwsp) .EQ. 0 .AND. jday .EQ. 1 .AND. hour .EQ. 1) xreprdinvst(i)=0.D0
 IF (xalive(i) .EQ. 1) THEN   !if alive
  IF (hour .EQ. 1) CALL maxcons(i,ifwsp)  
  IF (waterdepth(i_col(i),j_row(i)) .GE. xmindep(i)) THEN !if water deep enough for indiv
   CALL hoursplit(i,ifwsp)          !divide hour into segments based on indiv's length and distance to corner of fitness nhood (m)
   DO j = 1, timesteps              !loop over sub-hour segments (as deterined by subroutine 'hoursplit')
	IF (waterdepth(i_col(i),j_row(i)) .GE. xmindep(i)) THEN !if water deep enough for indiv (i.e, if indiv able to move)
	 CALL futuredepth(i,j,0,0)      !look 3 hrs into future to determine if cell water depth falls below 'xmindep' (m)
	 IF (movetype .EQ. 0) CALL fitnhood(i,j,ifwsp)  !if water depth > 'xmindep' for next 3 hrs, eval adjacent cells based on fitness
	 IF (movetype .EQ. 1) CALL emernhood(i,j,ifwsp) !if water depth < 'xmindep' in next 3 hrs, eval adjacent cells based on water depth (m)
	 CALL move(j,i)                 !move towards cell w highest fitness, wet cell, or deepest cell
	ELSE                            !otherwise, water too shallow for indiv (indiv can't move)
	 EXIT                           !exit loop (no point in looping over additional j's)
	END IF !cell depth vs indiv's min dep
   END DO !timesteps
  END IF !cell depth vs indiv's min dep
  CALL natmort(i,ifwsp)         !apply natural mortality (or stranding mort if indiv in shallow water or dry cell)
  IF (xalive(i) .EQ. 1) THEN    !if indiv alive (did not suffer nat mort or strand mort)
   CALL fishbycell(i,ifwsp)     !add to sp-specific running totals for worth, biomass, etc. in ea cell
  END IF !alive
 END IF !alive
 IF(i .EQ. totindivs .AND. hour .EQ. 1 .AND. jday .EQ. 1) THEN
    PRINt *, "Processed ", totindivs, " individuals at the beginning of year ", year
 END IF
END DO  !individuals loop (1 of 2) i1i1i1i1i1i1i1i1i1i1i1i1i1i1i1i1i1i1 end indivs1
!$OMP END PARALLEL DO
CALL cpu_time(profilingendtime)
profilingtime=profilingendtime-profilingstarttime
IF (fileflag(22) .EQ. 1) THEN    !if output desire
	outflag = 23; CALL output(1,0,0,0,0,0,0) !output profiling time (23=file 230)
 END IF
DO i = 1, totindivs
  ifwsp = spposn(xspecies(i))
	IF (xalive(i) .EQ. 1) THEN
	 IF (xalive(i) .EQ. 1) THEN
	  IF (xlen(i) .GE. matlen(ifwsp) .AND. jday .EQ. minspwnday(ifwsp) .AND. hour .EQ. 1) THEN !if large enough to spawn AND first day of spawn season AND
		spwncnt(ifwsp) = spwncnt(ifwsp) + 1     !count indiv
		spwnid(ifwsp,spwncnt(ifwsp)) = i        !add indiv's ID to list of spanwers
		spwnlen(ifwsp,spwncnt(ifwsp)) = xlen(i) !store indiv's length (mm)
	 END IF !can spawn
   END IF !alive 
  END IF !alive
END DO 

CALL updatecohort                   !mortality, aging, and development of eggs, yolksac larvae, and feeding larvae

DO i = 1, nfwsp                     !loop over food web species
 IF (spwncnt(i) .GT. 0 .AND. jday .EQ. minspwnday(i) .AND. hour .EQ. 1) CALL spawnorder(i) !if spawners AND first day of spawn season AND first hr of day, order spawners by length
 IF (i .EQ. isspeciesout .OR. isspeciesout .EQ. 0) THEN !if outputting sp of interest or all sp
  IF (fileflag(4) .EQ. 1) THEN      !if output desired
   outflag = 9; CALL output(i,0,0,0,0,0,0) !output sp- and cell-specific tots for superindiv, worth (abundance), and biomass (9 = file 90)
  END IF
  IF (fileflag(18) .EQ. 1) THEN     !if output desired
   outflag = 20; CALL output(i,0,0,0,0,0,0) !output distribn of abund by sp and hab type (20 = file 200)
  END IF
  IF (fileflag(20) .EQ. 1) THEN     !if output desired
   outflag = 21; CALL output(i,0,0,0,0,0,0) !output highest density (worth/m2, #/m2, g/m2) on grid by sp (21 = file 210)
  END IF
 END IF !output one or all species
END DO !food web species
IF(hour .EQ. 1 .AND. jday .EQ. 1) THEN
	spwndayscounter=0
	longindivscounter=0
	aliveindivs=0
	DO i = 1, totindivs
		j=spposn(xspecies(i))
		IF (xspwnday(i) .GT. 0) THEN
			spwndayscounter(j) = spwndayscounter(j)+1
		END IF
		IF(xlen(i) .GE. matlen(j)) THEN
			longindivscounter(j) = longindivscounter(j)+1
		END IF
		IF(xalive(i) .EQ. 1) THEN
			aliveindivs(j) = aliveindivs(j)+1
		END IF
	END DO
	DO i = 1, nfwsp
	   PRINT *, "Spawn counter for species, value, counter, alive ", i, spwndayscounter(i), spwncnt(i), longindivscounter(i), aliveindivs(i)
	END DO
END IF
IF (hour .GE. minfeedhr .AND. hour .LE. maxfeedhr) THEN !if hr falls inside of feeding window
END IF

!--loop over individuals (2 of 2) i2i2i2i2i2i2i2i2i2i2i2i2i2i2i2i2i2i2i start indivs2
CALL randindivs                     !assign indivs a random number
CALL indexx(totindivs,xrand,xorder) !use indv's random number to assign it a random position in the loop
CALL cpu_time(profilingstarttime)
IF (hour .EQ. nhours) THEN        !on last hour, respire, allocate wght change to growth and(or) reprod/maturity
  CALL calculatesumworths
  !DEC$ IF DEFINED(REGISTER_ONLY_LAST_WORTH)
    DO i=1, nfwsp
        worthannualsumming(i)=worthannualsumming(i)+sumworth(i)
    END DO
  !DEC$ ENDIF
END IF
DO i = 1, totindivs                 !loop over indivs (for reproduction, consumption, mortality, growth)
 indiv = xorder(i)                  !extract 'indiv' from position 'i' of 'xorder'

 fwsp = spposn(xspecies(indiv))     !store sp-specific position of indiv in 'speciesid' as global shorthand

 IF (xalive(indiv) .EQ. 1) THEN     !if indiv alive after nat mort (incl. predation), stranding, starvation and stranding
  IF (hour .EQ. 1) THEN             !on first hour of day
   CALL maturity                    !update maturity and 'xstage' based on length
   CALL fecundity                   !estab fecundity and batch wght if jday = indiv's first spawn day of season
   IF (waterdepth(i_col(indiv),j_row(indiv)) .GE. xmindep(indiv)) CALL spawn !if cell deep enough, spawn if bioenergetics allow, reabsorb gonads at end of spawn season
  END IF

  !NEEDS TO BE RE-CALIBRATED
!  IF (hour .EQ. maxfeedhr) CALL forcecons !if last hr of feeding window, fix daily cons at prop'n of max. Use to check/calibrate bioen. Affects movement b/c base prey not actually consumed
  IF (hour .GE. minfeedhr .AND. hour .LE. maxfeedhr) CALL freecons(0,0,indiv,fwsp) ! should ignore ii,jj if hr falls inside of feeding window, eat, factor in metab losses (except for respir'n), decrement prey densities and(or) worths
!
  IF (fileflag(12) .EQ. 1 .AND.hour .EQ. nhours .AND. (fwsp .EQ. isspeciesout .OR. isspeciesout .EQ. 0)) THEN !if output desired AND last hour AND want specific species OR all sp
   outflag = 15; CALL output(0,0,0,0,0,0,0) !output indiv diets
  END IF !output desired

  IF (hour .EQ. nhours) THEN        !on last hour, respire, allocate wght change to growth and(or) reprod/maturity
   CALL respiration(indiv, fwsp)    !respire (based on wght at start of day, wtemp, activity const)
   CALL energychange(indiv, fwsp)   !combine consump, daytime losses, repiration, energy densities, etc
   CALL weightchange(fwsp)          !update somatic and gonadal mass, evaluate starvation, update length
   xprevwght(indiv)=xtotwght(indiv)
    xprevwghtandworth(indiv)=xtotwght(indiv)*xworth(indiv)
  END IF !last hour

  IF (xalive(indiv) .EQ. 1) THEN    !if indiv alive (did not starve)
   CALL sumstages                   !sum across alive indivs to get sp-specific abundance, density, and mean len and wght by stage (YOY, juv, adult)

   
  END IF !alive

 END IF !alive
    IF (fileflag(10) .EQ. 1 .AND. (fwsp .EQ. isspeciesout .OR. isspeciesout .EQ. 0)) THEN !if output desired AND alive AND (want specific species OR all sp)
	    outflag = 2; CALL output(0,0,0,0,0,0,0) !output indiv attributes (2=file 22)
      END IF !output desired (alive indivs)
END DO !individuals loop (2 of 2) i2i2i2i2i2i2i2i2i2i2i2i2i2i2i2i2i2i2i end indivs2
CALL cpu_time(profilingendtime)
profilingtime=profilingendtime-profilingstarttime
IF (fileflag(22) .EQ. 1) THEN    !if output desire
	outflag = 23; CALL output(2,0,0,0,0,0,0) !output profiling time (23=file 230)
 END IF
 IF (fileflag(1) .EQ. 1) THEN       !if output desired
  outflag = 3; CALL output(0,0,0,0,0,0,0) !output space allocation (e.g., used, available, unused) (3=file 33)
 END IF

 DO i = 1, nfwsp                    !loop over food web species
  IF (i .EQ. isspeciesout .OR. isspeciesout .EQ. 0) THEN !if outputting sp of interest or all sp
   IF (fileflag(15) .EQ. 1) THEN    !if output desired
   CALL countsenescentindividuals
	outflag = 4; CALL output(i,0,0,0,0,0,0) !output abundance-by-stage (4=file 44)
   END IF
   IF (fileflag(16) .EQ. 1) THEN    !if output desired
	outflag = 14; CALL output(i,0,0,0,0,0,0) !if last hour of day, output mean p, length and weight by age (14=file 140)
   END IF
   IF (fileflag(17) .EQ. 1) THEN    !if output desired
	outflag = 19; CALL output(i,0,0,0,0,0,0) !if last hour of day, output mean diet by sp, age, and prey type (19=file 190)
   END IF
   IF (fileflag(21) .EQ. 1) THEN    !if output desired
	outflag = 22; CALL output(i,0,0,0,0,0,0) !if last hour of day, output worth lost by sp to nat mort, starvation, stranding, predation (22=file 220)
   END IF
  END IF !output one or all species
 END DO !food web species

 IF (jday .EQ. ndays .AND. hour .EQ. nhours) THEN
  PRINT *, "natmortloss    ", cummortloss(1) !print to screen running totals of worth lost to different sources of mort
  PRINT *, "starveloss     ", cummortloss(2)
  PRINT *, "strandloss     ", cummortloss(3)
  PRINT *, "prednloss      ", cummortloss(4)
  PRINT *, "senesceloss    ", cummortloss(5)
  PRINT *, "rescues        ", rescuecount
 END IF
   
END DO  !hour loop hhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh end hours
timeOffsetBetweenIterations=MOD(hour, nhours)-1
 xagedays = xagedays + 1            !incr day age of all indivs by 1
 IF (jday .EQ. 1 .OR. MOD(jday,30) .EQ. 0) PRINT *, "year, day", year, jday  !print 'year' and 'jday' on first day of year and every 30 days thereafter
 !DEC$ IF DEFINED(TEST_BIOMASS)
   IF (fileflag(25) .EQ. 1) THEN !if net productivity output desired
   outflag = 26; CALL output(1,0,0,0,0,0,0) !output net productivity by species and cell (g wwt) (23=file 230)
  END IF !net productivity output desired
 !DEC$ ENDIF
    DO i = 1, nfwsp
        worthtimesddwannualsumming(i)=worthtimesddwannualsumming(i)+speciesweightandworthdiff(i)
    END DO
END DO  !julian day loop dddddddddddddddddddddddddddddddddddddddddddddd end days
    DO i = 1, nfwsp
        IF (.NOT. (worthannualsumming(i) .EQ. 0.D0)) THEN
            growthproductivity(i)=worthtimesddwannualsumming(i)/worthannualsumming(i)
        ELSE
            growthproductivity(i)=0.D0
        END IF
    END DO
 CALL extinctcheck                  !check for extinct species (no worth in any age class), stop sim if all species extinct
 DO i = 1, nfwsp                    !loop over food web species
  IF (fileflag(19) .EQ. 1 .AND. (i .EQ. isspeciesout .OR. isspeciesout .EQ. 0)) THEN !if output desired AND for specific sp OR all sp
   DO j = 1, 3+maxacctage           !loop over early stages (egg, yolk, feed larv) later stages (=ages) + 1
	outflag = 18; CALL output(i,0,0,0,0,0,0) !output life table (18=file 180)
   END DO !stages
  END IF !output control
  IF (fileflag(23) .EQ. 1) THEN !if net productivity output desired
   outflag = 24; CALL output(i,0,0,0,0,0,0) !output net productivity by species and cell (g wwt) (23=file 230)
  END IF !net productivity output desired
   IF (fileflag(25) .EQ. 1) THEN !if net productivity output desired
   outflag = 26; CALL output(i,0,0,0,0,0,0) !output net productivity by species and cell (g wwt) (23=file 230)
  END IF !net productivity output desired
 END DO !food web sp
 CALL age_years                     !incr year age of all indivs by 1, record IDs of indivs that are older than max age
 watyr = watyr + 1                  !increment 'watyr' so next year's water levels are from next water year in 'watsched'
 IF (watyr .GT. waterschedyrs) watyr = 1        !if 10+,  reset to first water year (only 9 water years)

 IF (MOD(year,iswyearout) .EQ. 0 .AND. SUM(fileflag) .GT. 0) THEN !summarize data output
  IF (nfwsp .GT. 1) PRINT *, "data output for year, species", year, isspeciesout
  IF (nfwsp .EQ. 1) PRINT *, "data output for year, species", year, speciesid(1)
  PRINT *, ""
 ELSE
  PRINT *, "ouput skipped for year", year
  PRINT *, ""
 END IF

END DO  !year loop yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy end years

CALL timecounter                    !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds

!play end music
CALL beepqq(440,500) !beep A4 (frequency in Hz, duration in milliseconds)
CALL beepqq(523,400) !beep C5(frequency in Hz, duration in milliseconds)
CALL sleepqq(8)      !sleep (duration in milliseconds)
CALL beepqq(523,250) !beep C5 (frequency in Hz, duration in milliseconds)
CALL beepqq(349,450) !beep F4 (frequency in Hz, duration in milliseconds)

PRINT *, "All your base are belong to us" !http://www.youtube.com/watch?v=5fV_KxVwZjU

END PROGRAM

!-SUBROUTINES----------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------


SUBROUTINE readelev     !at start of sim, read in interpolated elevation data (m) from DELFT
USE time_etc
USE physicalenv
IMPLICIT NONE
REAL(8) inputelev(cols,rows) !input elevation (interpolated DELFT data; m)
INTEGER ii, jj

DO jj = 1, rows                              !loop over all rows in DELFT grid
 READ (11, 111) (inputelev(ii,jj),ii=1, cols) !read in elevation data (m)
 111 FORMAT (3800(F8.5,1X))                       !(where 3800 is some number > rows)
END DO !rows

CLOSE (11)                                   !close file (so that it doesn't take up memory)

DO ii = 1, cols                              !loop over sub-grid
 DO jj = 1, rows
  cellelev(ii,jj) = inputelev(ii,jj) !extract sub-grid elevations (m)
 END DO !columns
END DO !rows

IF (fileflag(3) .EQ. 1) THEN                 !if output desired
 outflag = 5; CALL output(0,0,0,0,0,0,0)                    !output cell elevations (5=file 55)
END IF

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE readwater  !at start of simulation, read in raw water levels and 5-day
USE time_etc          !moving average of water levels (for vegetation) for ea hour
USE physicalenv       !and year. Calc and store annual average. Relativize both
IMPLICIT NONE         !values to 'zerolevel'. Also add 'addwater' to water levels
					  !for prey and fish
INTEGER hr  !local shorthand for hour of year
INTEGER xx, yy

DO xx = 1, ndays
 DO yy = 1, nhours
  hr = (xx-1)*nhours+yy         !store hour of year as local shortand
  READ (81, 881) waterlevel(1,hr), avewater(1,hr) !read water level and 5-day running ave for all hours in year 1
  881 FORMAT (18X, F6.3, 2X, F6.3)
  IF (xx .EQ. ndays .AND. yy .EQ. nhours) meannualwatlev(1) = SUM(avewater(1,:))/REAL(xx*yy) !calc and store ave water level for year (m)
  READ (82, 882) waterlevel(2,hr), avewater(2,hr) !read water level and 5-day running ave for all hours in year 2
  882 FORMAT (18X, F6.3, 2X, F6.3)
  IF (xx .EQ. ndays .AND. yy .EQ. nhours) meannualwatlev(2) = SUM(avewater(2,:))/REAL(xx*yy) !calc and store ave water level for year (m)
  READ (83, 883) waterlevel(3,hr), avewater(3,hr) !read water level and 5-day running ave for all hours in year 3
  883 FORMAT (18X, F6.3, 2X, F6.3)
  IF (xx .EQ. ndays .AND. yy .EQ. nhours) meannualwatlev(3) = SUM(avewater(3,:))/REAL(xx*yy) !calc and store ave water level for year (m)
  READ (84, 884) waterlevel(4,hr), avewater(4,hr) !read water level and 5-day running ave for all hours in year 4
  884 FORMAT (18X, F6.3, 2X, F6.3)
  IF (xx .EQ. ndays .AND. yy .EQ. nhours) meannualwatlev(4) = SUM(avewater(4,:))/REAL(xx*yy) !calc and store ave water level for year (m)
  READ (85, 885) waterlevel(5,hr), avewater(5,hr) !read water level and 5-day running ave for all hours in year 5
  885 FORMAT (18X, F6.3, 2X, F6.3)
  IF (xx .EQ. ndays .AND. yy .EQ. nhours) meannualwatlev(5) = SUM(avewater(5,:))/REAL(xx*yy) !calc and store ave water level for year (m)

 END DO !hours
END DO !days

CLOSE (81); CLOSE (82); CLOSE (83); CLOSE (84); CLOSE (85) !close file (so that it doesn't take up memory)

waterlevel = waterlevel + zerolevel + addwater !relativize all hourly levels to level that defines mean water's edge (m)
											   !and add water so that marsh species can access marsh (does not affect inundation)
avewater = avewater + zerolevel                !relativize all hourly running averages to level that defines marsh edge (m)

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE depth  !each hour, use water level (m) to calculate depth (m) and volume (m**3).
USE time_etc      !If called at beginning of sim, use water level from day 1 hour 1. If
USE physicalenv   !called at beginning of sim or end of day, make sure that at leat one wet
USE preybase      !cell (otherwise nowhere to assign new indivs). If called from within hour
IMPLICIT NONE     !hour loop, keep track of whether cell is wet or dry. Distinction needed
				  !for subroutine 'preydynam', which suspends prey dynamic in dry cells.
REAL(8) watlev  !local shorthand for 'waterlevel'
INTEGER ii, jj

IF (depthflag .EQ. 0) watlev = waterlevel(1,1)      !if start of sim, use first hour's water level (m)
IF (depthflag .EQ. 1) watlev = waterlevel(watsched(watyr),cumhr-(year-1)*ndays*nhours) !if within hour loop, extract water level for that hour (m)

DO ii = 1, cols                                     !loop over sub-grid columns,
 DO jj = 1, rows                                    !and rows
  waterdepth(ii,jj) = watlev - cellelev(ii,jj)      !calculate water depth (m), where >0 = wet, <=0 = dry
  IF (depthflag .EQ. 1) THEN                        !if within hour loop
   watervol(ii,jj) = cellsize * cellsize * waterdepth(ii,jj) !calc cell volume (m**3)
   IF (waterdepth(ii,jj) .GT. 0.D0) THEN            !if positive depth (i.e., cell wet)
	wetarray(ii,jj) = 1                             !flag wet cell
   ELSE                                             !otherwise, cell dry
	wetarray(ii,jj) = 0                             !flag dry cell
   END IF !water depth
  END IF !within hour loop
 END DO !sub-grid rows
END DO !sub-grid columns

IF (fileflag(2) .EQ. 1 .AND. depthflag .EQ. 1) THEN !if output desired and within hour loop
 outflag = 6; CALL output(0,0,0,0,0,0,0) !output water depths (6=file 66)
END IF

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE initindiv !at start of sim, initialize individual attributes
USE time_etc         !and record ID numbers of unused spaces
USE physicalenv
USE species
USE indivattributes
USE movement
USE reproduction
IMPLICIT NONE
INTEGER spaceused     !a record of number of indivs that have been initialized
INTEGER arrayposn     !stores 'spaceused' as an array position after looping through ea age class
INTEGER xx, yy, zz

PRINT *, "Initializing individuals..."

spaceused=0; oldcount=0; oldindivs=0  !zero out

DO xx = 1, nfwsp               !loop over food web species
 DO yy = 1, ageclasses(xx)     !loop over age classes within a species
  arrayposn = spaceused        !store used space counter as a starting position

  DO zz = arrayposn+1, arrayposn+ageclassspace(xx)  !loop over array space avail for age class
   IF (yy .LT. ageclasses(xx)) THEN        !if not last age class
	assignflag = 1; CALL assignattributes(xx,yy,zz)  !(1=initialize, 2=recruit, 3=split worth, 4=zero old)
	outflag = 11; CALL output(0,0,0,0,0,0,zz)        !output initial conditions for initialized indiv
	spaceused = spaceused + 1              !increment count of used-up space
   ELSE                                    !otherwise, last age class
	oldcount = oldcount + 1                !increment count of old indivs
	oldindivs(oldcount) = zz               !store ID number (array position)
	xspecies(zz) = speciesid(xx)           !assign species type
	xageyrs(zz) = yy                       !set age to yy (= ageclasses(xx) = max age)
	spaceused = spaceused + 1              !increase by 1 count of used-up space
   END IF !age class
  END DO !space
 END DO !age classes
END DO !species

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE recruitwindow  !at start of sim, loop over days and hours. For ea species, use
USE time_etc              !first and last possible spawn day, water temperature, and
USE physicalenv           !stage-specific development parameters to determine first and
USE species               !last possible recruit day. This info is used to spread recruit
USE reproduction          !space more evenly across days, and to split high-worth indivs
IMPLICIT NONE             !as soon as spawning season ends (instead of when year ends)
INTEGER spwnminmax(2,nfwsp)  !local shorthand for sp-specific min and max spawn day
INTEGER recminmax(2,nfwsp)   !sp-specific min and max recruit day
INTEGER next                 !next early development stage (allows shrimp to skip yolk stage)
INTEGER hr, sp, mm, co       !local do loop indices: hour, species, min/max, cohort

DO jday = MINVAL(minspwnday), ndays    !loop over days, starting w earliest spawn day across all food web sp
 CALL temperature

 DO hr = 1, nhours                     !loop over hours
  DO sp = 1, nfwsp                     !loop over food web sp
   IF (jday .EQ. MINVAL(minspwnday)) THEN !if first day of loop
	spwnminmax(1,sp) = minspwnday(sp)  !store sp-specific min spawn day
	spwnminmax(2,sp) = maxspwnday(sp)  !store sp-specific max spawn day
   END IF
   DO mm = 1, 2                        !loop over min and max values
	IF (jday .EQ. spwnminmax(mm,sp) .AND. hr .EQ. 1) cohortflag(1,mm,sp) = 1 !if start of min or max spawn day, flag that eggs are present
	DO co = 3, 1, -1                   !loop over cohorts (i.e., early development stages egg, yolk, feed larv)
	 IF (cohortflag(co,mm,sp) .EQ. 1) THEN    !if cohort of sp present on jday,
	  cohortftemp(co,mm,sp) = cohortftemp(co,mm,sp) + 1.D0 / (fracta(sp,co) * EXP(-fractc(sp,co) * wtemp)) !increase fract temp
	  IF (cohortftemp(co,mm,sp) .GE. 1.D0) THEN !if fractional temp of cohort for sp from day mm >=1,
	   IF (co .EQ. 3) THEN                      !if latest early development stage (feeding larv)
		recminmax(mm,sp) = jday + 1             !record recruitment on next day (subroutine 'recruitment' called at start of next day)
	   ELSE                                     !otherwise, yolk or egg
		next = co+1                             !calc next stage (yolk+1=feeding, egg+1=yolk)
		IF (speciesid(sp) .EQ. 1) next = co+2   !if shrimp, which do not have yolk stage, 'next' = egg+2 = feeding
		cohortflag(next,mm,sp) = 1              !flag cohort present for day mm and sp
	   END IF !cohort
	   cohortftemp(co,mm,sp) = 0.D0             !zero fractional temerature
	   cohortflag(co,mm,sp) = 0                 !zero flag
	  END IF !fractional temp
	 END IF !cohort flag
	END DO !early development stages
   END DO !min and max spawn days
   IF (jday .EQ. ndays .AND. hr .EQ. nhours) THEN !if last hour of last day
	frstrecday(sp) = recminmax(1,sp)              !pass first possible recruit day to global variable
	lastrecday(sp) = recminmax(2,sp)              !pass last possible recruit day to globabl variable
   END IF
  END DO !food web sp
 END DO !hours
END DO !days

END SUBROUTINE

INCLUDE 'inundation.f90'

!----------------------------------------------------------------------
SUBROUTINE habitats !at start of first year, assign habitat type to cell based on number of hours in year it
USE time_etc        !will be inundated. If heavily inund (defined by 'inundmax') and on ave >0.6 m deep, define
USE physicalenv     !as deep water (1). If heavily inund and <0.6 m, randomly define as deep water (1) or SAV
IMPLICIT NONE       !SAV (2) by applying a linear increase in SAV with decreasing depth. If not heavily inund,
					!loop over hab types 2-6. For each, use inund to draw a probty from a sp-specific beta
					!beta distribn. Then "stack" probty's on top of each other and re-scale so that stop of
					!stack = 1. Draw a random number betwn 0 and 1 from an even distribn and assign hab type
					!based on where in the "stack" the random number lies. Keep track of all hab type coord so
					!that marsh can be characterized and indivs initiated on optimal hab, and so that hab types
					!2 and 3 can be converted seasonally to mud flats (type 6) in subroutine 'mudflats'.
					!If dry yr and in ideal yr cell too wet for woody, remove woody from list of pot hab types.
					!1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh).
INTEGER xx
INTEGER wlp              !water level profile (06/07, 07/08, ..., 10/11)
INTEGER pothabtypes      !a running total of number of hab types that can occur in a cell given its elevation
INTEGER hablist(habtype) !a list of hab types that can occur in a cell given its elevation
REAL(8) rawprob(habtype) !the probty of hab type xx given annual inundation (returned by FUNCTION 'pdf')
REAL(8) rise, run, slope, intercept !elements of the linear eq'n for predicting SAV(2) in heavily inundated cells
REAL(8) prob             !the probty of SAV(2) in heavily unundated cells
REAL(8) pdf              !a FUNCTION that accepts inundation (hours), hab-specific coeffs, and returns a Beta probty ('rawprob')
REAL(8) totprob          !sum of 'rawprob' across all hab types
REAL(8) relprob(habtype) !'rawprob' relativized to 1 via 'totprob'
REAL(8) habprob(habtype) !a list of probtys of hab types that can occur in a cell given its elevation. Probtys are stacked, not overlapping
REAL(8) pran            !a random number, even dist, 0 - 1
REAL(8) rand             !a random number, even dist, 0 - 1 (heavily inundated cells) or 0 - largest value in 'habprob' (cells not heavily inund))
INTEGER ii, jj

PRINT *, "Assigning habitats..."

rawprob(1) = 0.D0
habmask=0; habcount=0 !zero habitat mask and associated counter
wlp = watsched(watyr)         !store water level profile (e.g., 07/08, 10/11) as local shorthand

DO ii = 1, cols               !loop over sub-grid columns
 DO jj = 1, rows              !and rows
  pothabtypes=0; hablist=0; habprob=0.D0    !zero counter and arrays for ea new cell

  IF (inund(ii,jj) .GE. inundmax) THEN      !if cell heavily inundated (hours)
   IF (zerolevel-cellelev(ii,jj) .GT. maxSAV) THEN !if, on ave, cell depth >= depth at which SAV occur (on ave b/c water levels center close to 'zerolevel')
	habitat(ii,jj) = 1                      !assume deep water cell (1)
	habcount(1) = habcount(1) + 1           !update hab-specific counter (used as row position in 'habmask')
	habmask(1,1,habcount(1)) = ii           !store hab-specific cell column in habcount'th row of habmask
	habmask(1,2,habcount(1)) = jj           !store hab-specific cell row in habcount'th row of habmask
   ELSE                                     !otherwise, cell depth < max SAV depth. Estim probty of SAV(2) from linear relation as follows
	rise = 0.98D0 - 0.2D0                   !range in probty from shallow to deep is [~'relprob' given Beta pdf @ 'maxinund'] - [Guerry Holm's 20% cover @ 0.6 m]
	run =  zerolevel - maxelev - maxSAV     !range in depth (m) is [~shallowest depth that will give 'maxinund'] - [Guerry Holm's 'maxSAV']
	slope = rise / run                      !calc line slope
	intercept = 0.2D0 - slope * maxSAV      !calc line intercept

	prob = slope * (zerolevel - cellelev(ii,jj)) + intercept !use line eq'n and mean cell depth (m) to generate a probty for SAV
	rand = pran(1)                      !choose random number betw 0 and 1, even dist

	IF (rand .GT. prob) THEN       !if random number > the prob for SAV(2),
	 habitat(ii,jj) = 1            !cell is deep water(1)
	 habcount(1) = habcount(1) + 1 !update hab-specific counter (used as row position in 'habmask')
	 habmask(1,1,habcount(1)) = ii !store hab-specific cell column in habcount'th row of habmask
	 habmask(1,2,habcount(1)) = jj !store hab-specific cell column in habcount'th row of habmask
	ELSE                           !otherwise, random number < the prob for SAV(2)
	 habitat(ii,jj) = 2            !cell is SAV(2)
	 habcount(2) = habcount(2) + 1 !update hab-specific counter (used as row position in 'habmask')
	 habmask(2,1,habcount(2)) = ii !store hab-specific cell column in habcount'th row of habmask
	 habmask(2,2,habcount(2)) = jj !store hab-specific cell column in habcount'th row of habmask
	END IF !random number
   END IF !cell depth vs. 'maxSAV'

  ELSE                                      !otherwise, cell not heavily inundated. Select among hab types as follows

   DO xx = 2, habtype                       !loop over habitat types 2 to 6
	rawprob(xx) = pdf(inund(ii,jj),pdfa(xx),pdfb(xx),pdfc(xx),pdfd(xx),pdfe(xx)) !use a FUNCTION to predict a Beta probty from cell inundation
   END DO

   IF (year .GT. 1) THEN                    !if not first year
	IF (meannualwatlev(wlp) .LT. meannualwatlev(1)-0.01D0 .AND. idealinund(ii,jj) .GT. 591) THEN !if current yr on ave drier than first yr (-0.01D0 to avoid rounding errors) AND cell too wet for woody during ideal yr
	 rawprob(5) = 0.D0                      !set woody probty to zero
	 IF (inund(ii,jj) .LE. 270) rawprob(4) = 1.D0 !if cell so dry that only woody, make it high emergent
	END IF !dry year and cell normally too wet for woody
   END IF !not first year

   totprob = SUM(rawprob)                   !sum raw probtys across all hab types

   DO xx = 2, habtype                       !loop over habitat types 2 to 6
	relprob(xx) = rawprob(xx) / totprob     !convert raw probty to a relative probty (0 to 1)

	IF (relprob(xx) .GT. minprob) THEN      !if relative probty of hab type xx > threshold minimum value
	 pothabtypes = pothabtypes + 1          !keep a running total of potential hab types
	 hablist(pothabtypes) = xx              !store ID number of the potential hab type in an array

	 IF (pothabtypes .EQ. 1) THEN           !if first time relative probty was calculated
	  habprob(pothabtypes) = relprob(xx)    !store it in an array
	 ELSE                                   !otherwise
	  habprob(pothabtypes) = relprob(xx) + habprob(pothabtypes-1) !store relative probtys as cumulative sums so that they are stacked instead of overlapping
	 END IF

	END IF !greater than 'minprob'
   END DO !hab type

   rand = pran(1) * habprob(pothabtypes)!draw a random number between 0 and largest value in 'habprob' from an even distribn

   DO xx = 1, pothabtypes                   !loop over potential suitable habitat types
	IF (rand .LE. habprob(xx)) THEN         !if this random numer =< probty,
	 EXIT                                   !exit 'pothabtypes' loop
	ELSE                                    !otherwise,
	 CYCLE                                  !go on to next probty in loop
	END IF
   END DO !hab count

   habitat(ii,jj) = hablist(xx)             !use EXIT 'pothabtypes' to extract hab type from hab list
   habcount(habitat(ii,jj)) = habcount(habitat(ii,jj)) + 1 !update hab-specific counter (used as row position in 'habmask')
   habmask(habitat(ii,jj),1,habcount(habitat(ii,jj))) = ii !store hab-specific cell column in habcount'th row of habmask
   habmask(habitat(ii,jj),2,habcount(habitat(ii,jj))) = jj !store hab-specific cell column in habcount'th row of habmask

  END IF !cell not heavily inundated
 END DO !rows
END DO !columns

IF (fileflag(5) .EQ. 1) THEN                !if output desired
 outflag = 10; CALL output(0,0,0,0,0,0,0)                  !output habitat types (10=file 100)
END IF !grid data are to be output

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE temperature  !at start of each day, calc water temperature (oC) as function
USE time_etc            !of julian day (from LDWF used in Shaye's model). Because 'jday'
USE physicalenv         != Mar 1 in model but Jan 1 in equation, jday is adjusted.
IMPLICIT NONE
REAL(8) date  !adjusted jday, which allows for temp calc correct date (e.g., jday 1 = Mar 1, not Jan 1)

IF (jday .LT. 301) date = REAL(jday + 60)      !if jday betw Mar 1 and end Oct, adjust so LDWF eqn gives correct 'wtemp'
IF (jday .GE. 301) date = REAL(jday - 300)     !if jday Nov 1 or later, adjust so LDWF eqn gives correct 'wtemp'
wtemp = 23.157 - (2.459 * SIN(date * 0.017)) - (7.161 * COS(date * 0.017)) !from LDWF in Shaye's model (oC)

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE mudflats  !on jday 255 (Nov 15), loop through 'habmask' and convert
USE time_etc         !SAV and low emergents (hab types 2 and 3) to mudlfats
USE physicalenv      !(hab type 6). Will be converted back to SAV and low
IMPLICIT NONE        !emergents at start of next year (March 1). Habitat
INTEGER xx
INTEGER ii, jj
					 !masks and counters NOT changed.
DO xx = 1, habcount(2) !loop over all cells that are SAV(2)
 ii = habmask(2,1,xx)  !extract column
 jj = habmask(2,2,xx)  !extract row
 habitat(ii,jj) = 6    !convert habitat type to 6 (unvegetated)
END DO
DO xx = 1, habcount(3) !loop over all cells that are low emergent(3)
 ii = habmask(3,1,xx)  !extract column
 jj = habmask(3,2,xx)  !extract row
 habitat(ii,jj) = 6    !convert habitat type to 6 (unvegetated)
END DO

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE recruitment !similar to 'initindiv': at start of each day: calculate worth,
USE time_etc           !assign other indiv attributes. At end of year, zero out
USE physicalenv        !wasted indiv space
USE species
USE indivattributes
USE movement
USE reproduction
IMPLICIT NONE
INTEGER spaceused        !counter for number of indivs that have been initialized
INTEGER arrayposn        !stores 'spaceused' as an array position after looping through ea age class
INTEGER nrecdays         !number of days that recruitment can occur (i.e., potential recruit season)
INTEGER recruitsperday(nfwsp)!"naive" number of recruits of ea sp that CAN be allocated on all days (d.n. incl 'unused' space due to rounding)
INTEGER unused           !recruit space left over after 'recruitsperday' calc. Applied to 'initrecdist' to even out worth
INTEGER extra            !recruit space left over on jday X when there are no yoy of species Y to recruit
INTEGER z                !local index for error do loop
INTEGER xx, yy, zz

spaceavail = oldcount   !store number of spaces available for all recruits in that year (= recruit's ID)

DO xx = 1, nfwsp        !loop over food web species

 IF (year .EQ. 1 .AND. jday .EQ. 1) THEN                !if start of sim
  nrecdays = lastrecday(xx) - frstrecday(xx) + 1        !calc number of days in potential recruit season

!DEC$ IF DEFINED (TEST_MODE)
  IF (nrecdays .GT. ageclassspace(xx)) THEN             !check that recruits/day will be >= 1
  DO z = 1, 25
   CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
  END DO
   PRINT *, "Error 1 in 'recruitment': recruits/day < 1 for food web sp", xx
   PRINT *, "'ageclassspace' must be at least", nrecdays
   PRINT *, "Simulation stopped."
   CALL timecounter    !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
   STOP
  END IF
!DEC$ ENDIF

  recruitsperday(xx) = INT(ageclassspace(xx) / REAL(nrecdays)) !calc number of recruits on ea day that recruitment can occur

  DO zz = frstrecday(xx), lastrecday(xx)                !loop over potential recruit season
   initrecdist(xx,zz) = recruitsperday(xx)              !associate 'recruitsperday' w/ ea jday of year in a starting (fixed) array
   modrecdist(xx,zz) = initrecdist(xx,zz)               !and in a modifiable array
  END DO

  unused = ageclassspace(xx) - recruitsperday(xx) * nrecdays !anticipate unused space (results when 'nrecdays' does not div evenly into 'ageclassspace')

  IF (unused .GT. 0) THEN                               !if unused space
   zz = lastrecday(xx)                                  !set "day" counter to end of potential recruit season
   DO WHILE (unused .GT. 0)                             !loop until unused space used up
	IF (zz .LT. frstrecday(xx)) zz = lastrecday(xx)     !if zz < 'frstrecday', reset to end of potential recruit season (i.e., start over)
	initrecdist(xx,zz) = initrecdist(xx,zz) + 1         !increase by 1 recruit space available on "day" zz in starting array
	modrecdist(xx,zz) = initrecdist(xx,zz)              !and in a modifiable array
	unused = unused - 1                                 !decrement by 1 unused space
	zz = zz - 1                                         !decrement by 1 "day" counter
   END DO
  END IF !unused space

 END IF !start of sim

 IF (newyoyflag(xx) .EQ. 0 .AND. jday .LT. lastrecday(xx)) THEN !if no YOY to recruit on jday AND not end of potential recruit season
  extra = modrecdist(xx,jday)                           !store unused recruit space for species and day
  zz = jday + 1                                         !set "day" counter to next day
  DO WHILE (extra .GT. 0)                               !loop until extra recruit space used up
   IF (zz .GT. lastrecday(xx)) zz = jday + 1            !if "day" counter > end of potential recruit season, reset to next day (i.e., start over)
   modrecdist(xx,zz) =  modrecdist(xx,zz) + 1           !increase by 1 recruit space available on "day" zz
   extra = extra - 1                                    !decrement by 1 unused space
   zz = zz + 1                                          !increase "day" counter by 1
  END DO
 END IF !no yoy to recruit and not last day

 IF (newyoyflag(xx) .EQ. 0 .AND. jday .EQ. lastrecday(xx)) THEN !if no yoy to recruit on jday AND end of potential recruit season
  DO yy = 1, modrecdist(xx,jday)                        !loop over unused space
   assignflag = 3                                       !use "n" spaces to split worth of "n" highest-worth indivs of sp xx
   CALL assignattributes(xx,yy,zz)                                !(1=initialize, 2=recruit, 3=split worth, 4=zero old)

   IF (year .EQ. 1) THEN                                !if year 1 (regardless of whether or not outupt desired)
	outflag = 11; CALL output(0,0,0,0,0,0,zz)                           !output initial conditions for split indivs (11=file 110)
   ELSE                                                 !otherwise, for all other years
	IF (fileflag(9) .EQ. 1 .AND. &                      !if output desired for specific species OR all species
	  (spposn(xspecies(oldindivs(spaceavail))) .EQ. isspeciesout .OR. isspeciesout .EQ. 0)) THEN
	 outflag = 11; CALL output(0,0,0,0,0,0,zz)                          !output initial conditions for split indivs (11=file 110)
	END IF !output desired
   END IF !year 1

   spaceavail = spaceavail - 1                          !decrement by 1 the amount of available space for recruits of all sp
   recruitcount(xx) = recruitcount(xx) + 1              !increase by 1 the number of recruits allocated for species xx
 
   IF (spaceavail .LT. 0) THEN                          !check that space not overallocated
	DO z = 1, 25
	 CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
	END DO
	PRINT *, "Error 2 in splitting algorithm of 'recruitment':"
	PRINT *, "at end of recruit season, used more space than available."
	PRINT *, "Simulation stopped."
	CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
	STOP
   END IF

  END DO !'wasted' recruit space

  IF (jday .EQ. MAXVAL(lastrecday) .AND. spaceavail .GT. 0) THEN !check that space not underallocated
   DO z = 1, 25
	CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
   END DO
   PRINT *, "Error 3 in splitting algorithm of 'recruitment':"
   PRINT *, "at end of recruit season, did not use all available space."
   PRINT *, "Leftover space:", spaceavail
   PRINT *, "Simulation stopped."
   CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
   STOP
  END IF

  totsplit = totsplit + split(xx)                           !sum split space across all sp (for output file 33 'out_allocatedspace.out')
  totunused = totunused + (modrecdist(xx,jday) - split(xx)) !sum unused space across all sp (for output file 33 'out_allocatedspace.out')

 END IF !no yoy to recruit and last day

 IF (newyoyflag(xx) .EQ. 1) THEN !if yoy to recruit for day ('newflag' turned on/off daily)
  IF (jday .LT. frstrecday(xx) .OR. jday .GT. lastrecday(xx)) THEN !if recruiting outside of
   DO z = 1, 25
	CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
   END DO
   PRINT *, "Error 4 in recruitment algorithm of 'recruitment':"
   PRINT *, "recruitment occuring outside of defined recruitment season."
   PRINT *, "Start and end of season:", frstrecday(xx), lastrecday(xx)
   PRINT *, "Error year, day, food web sp:", year, jday, xx
   PRINT *, "Simulation stopped."
   CALL timecounter !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
   STOP
  END IF

  IF (spaceavail .EQ. 0) THEN
   DO z = 1, 25
	CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
   END DO
   PRINT *, "Error 5 in recruitment algorithm of 'recruitment':"
   PRINT *, "Cohort ready to recruit but no space."
   PRINT *, "Error year, day, food web sp", year, jday, xx
   PRINT *, "Simulation stopped."
   CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
   STOP
  END IF

  avenewrth(xx) = newyoy(xx) / REAL(modrecdist(xx,jday))

  DO yy = 1, modrecdist(xx,jday) !loop over available recruit space for day
   assignflag = 2; CALL assignattributes(xx,yy,zz) !(1=initialize, 2=recruit, 3=split worth, 4=zero old)

   IF (year .EQ. 1) THEN !if year 1 (regardless of whether or not outupt desired)
	outflag = 11; CALL output(0,0,0,0,0,0,zz) !output initial conditions for newly recruited indivs (11=file 110)
   ELSE !otherwise, for all other years
	IF (fileflag(9) .EQ. 1 .AND. & !if output desired for specific species OR all species
	  (xspecies(oldindivs(spaceavail)) .EQ. isspeciesout .OR. isspeciesout .EQ. 0)) THEN
	 outflag = 11; CALL output(0,0,0,0,0,0,zz)              !output initial conditions for newly recruited indivs (11=file 110)
	END IF !output desired
   END IF !year 1

   spaceavail = spaceavail - 1              !decrement by 1 the amount of available space for recruits of all sp
   recruitcount(xx) = recruitcount(xx) + 1  !increase by 1 the number of recruits allocated for species xx

   IF (spaceavail .LT. 0) THEN              !check that space not overallocated
	DO z = 1, 25
	 CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
	END DO
	PRINT *, "Error 6 in recruitment algorithm of 'recruitment':"
	PRINT *, "number of recruited individuals exceeds available space."
	PRINT *, "Error year, day, food web sp", year, jday, xx
	PRINT *, "Simulation stopped."
	CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
	STOP
   END IF

  END DO !recruits
											!check that space not underallocated at end of year
  IF (xx .EQ. nfwsp .AND. jday .EQ. ndays .AND. spaceavail .GT. 0) THEN
   DO z = 1, 25
	CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
   END DO
   PRINT *, "Error 7 in recruitment algorithm of 'recruitment': at end of year,"
   PRINT *, "space used for recruitment is less than available space."
   PRINT *, "Leftover space:", spaceavail
   PRINT *, "Simulation stopped."
   CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
   STOP
  END IF

  newyoy(xx)=0.D0; newyoyflag(xx)=0         !zero array and reset flag (all recruits for day were allocated in loop)

 END IF !yoy to recruit
END DO !species

oldcount = spaceavail                       !change oldcount to spaceavail (accounts for recruit space used on jday)

IF (jday .EQ. ndays)  modrecdist = initrecdist !at end of year, reset updated recruit space to starting recruit space

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE preydynam !at start of sim, set prey types to eq'bm abund. Each hour after that,
USE time_etc         !apply logistic pop grow rate (hourly) that is calculated at start of
USE physicalenv      !day but used for next 23 hrs. Prey are not allowed to drop below
USE preybase         !'minpreyden' (g wwt/m**2 if benth, g wwt/m**3 if zoop). Prey dynamics
IMPLICIT NONE        !are suspended when cell is dry; resume when cell re-wets.
REAL(8) preyr(preytypes) !hourly population growth rate of ea prey type
REAL(8) Neq   !'initpreynum' (eq'bm density) modified by habitat (#/m**2 if benth, #/m**3 if zoop)
REAL(8) Nt    !local shorthand for 'unitpreynum' (#/m**2 if benth, #/m**3 if zoop)
INTEGER xx,yy,zz
INTEGER ii, jj

DO ii = 1, cols                                                       !loop over all columns
 DO jj = 1, rows                                                      !rows,
  DO xx = 1, preytypes                                                !and prey types

   IF (hour .EQ. 1 .AND. ii .EQ. 1 .AND. jj .EQ. 1) THEN              !if start of day and loop,
	preyr(xx) = preya(xx) + preyb(xx) * SIN(2.0 * 3.1416D0 * jday / preyd(xx) + preyc(xx)) !calc hourly pop grow rate (used for all other hrs in day)
   END IF

   IF (year .EQ. 1 .AND. jday .EQ. 1 .AND. hour .EQ. 1) THEN          !if start of sim
	IF (xx .LE. 3) unitpreynum(xx,ii,jj) = initpreynum(xx) * preymult(xx,habitat(ii,jj)) !if benth, init density given habitat (#/m**2)
	IF (xx .GE. 4) unitpreynum(xx,ii,jj) = initpreynum(xx) * preymult(xx,habitat(ii,jj)) !if zoop, init desity given habitat (#/m**3)
	unitpreybmss(xx,ii,jj) = unitpreynum(xx,ii,jj) * indpreywght(xx)  !convert #/m**2 or 3 to g wwt/m**2 or 3
   ELSE                                                               !otherwise, not start of sim
	IF (wetarray(ii,jj) .EQ. 1) THEN                                  !if wet cell
	 Nt = unitpreynum(xx,ii,jj)                                       !store #/m**2 or 3 as local shorthand (#/m**2)
	 Neq = initpreynum(xx) * preymult(xx,habitat(ii,jj))              !apply prey habitat mult to 'initpreynum', store locally (#/m**2 if benth, #/m**3 if zoop)
	 unitpreynum(xx,ii,jj) = Nt + preyr(xx) * Nt * (1.D0 - Nt / Neq)  !modify abundance in 1 cubic m via logistic growth (#/m**2 if benth, #/m**3 if zoop)
	 IF (unitpreynum(xx,ii,jj)*indpreywght(xx) .LT. minpreyden) unitpreynum(xx,ii,jj) = minpreyden / indpreywght(xx) !if density below 'minpreyden' (g wwt/m**2 or 3), set to 'minpreyden' as #/m**2 or 3
	 unitpreybmss(xx,ii,jj) = unitpreynum(xx,ii,jj) * indpreywght(xx) !convert #/m**2 or 3 to g wwt/m**2 or 3
	END IF !wet cell
   END IF !start of sim or any other time

   aveunitnum(habitat(ii,jj),xx) = (aveunitnum(habitat(ii,jj),xx) + (unitpreynum(xx,ii,jj)) / habcount(habitat(ii,jj)))    !calc aves by prey and hab type (#/m**2 or 3)
   aveunitbmss(habitat(ii,jj),xx) = (aveunitbmss(habitat(ii,jj),xx) + (unitpreybmss(xx,ii,jj)) / habcount(habitat(ii,jj))) !(g wwt/mm*2 or 3))

   IF (ii .EQ. cols .AND. jj .EQ. rows) THEN                          !if last cell
	DO zz = 1, habtype                                                !loop over hab types
	 aveunitnum(habtype+1,xx) = aveunitnum(habtype+1,xx) + (aveunitnum(zz,xx) * habcount(zz)) !weight number of prey by number of cells of hab type zz
	 IF (zz .EQ. habtype) THEN                                        !if last hab type
	  aveunitnum(habtype+1,xx) = aveunitnum(habtype+1,xx) / (cols * rows) !calc weighted ave (#/m**2 or 3)
	  aveunitbmss(habtype+1,xx) = aveunitnum(habtype+1,xx) * indpreywght(xx) !convert number to biomass (g wwt/m**2 or 3)
	 END IF
	END DO !hab types
   END IF !last cell

   IF (fileflag(7) .EQ. 1) THEN                                       !if output desired
	IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 .AND. MOD(hour,iswhourout) .EQ. 0) THEN !for year, hour, and day
	 outflag = 16; CALL output(0,0,ii,jj,xx,0,0)                                        !output prey abund and biomass by unit and cell
	END IF !year, hour, day limiters
   END IF !grid out flag

  END DO !prey types
 END DO !rows
END DO !columns

IF (fileflag(8) .EQ. 1) THEN                                          !if output desired
 IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 .AND. MOD(hour,iswhourout) .EQ. 0) THEN !for year, hour, and day
  DO xx = 1, preytypes
   DO yy = 1, habtype+1
	outflag = 17; CALL output(0,0,0,0,xx,yy,0)                                         !for ea hab type and entire grid, output ave prey abund and biomass by unit and cell
   END DO !hab types
  END DO !prey types
 END IF !year, hour, day limiters
END IF !grid out flag

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE sumspace(ind) !each day, sum across indivs to determine space that is being used
USE indivattributes      !by established and newly recruited indivs, space that is available,
USE movement             !and space that was used to split high-worth indivs
USE reproduction         !(for output file 33 'out_allocatedspace.out')
IMPLICIT NONE
INTEGER ind

	  !if not in 1st yr and column not 0-ed out, OR in 1st yr but >1-day old and column not 0-ed out
 IF ((xageyrs(ind) .GT. 1 .AND. i_col(ind) .NE. 0) &
   .OR. (xageyrs(ind) .EQ. 1 .AND. xagedays(ind) .GT. 1 .AND. i_col(ind) .NE. 0)) THEN
   totestab = totestab + 1              !established indiv
 END IF

	  !if in 1st yr and 1-day old
 IF (xageyrs(ind) .EQ. 1 .AND. xagedays(ind) .EQ. 1 .AND. i_col(ind) .NE. 0) THEN
  totnew = totnew + 1                   !newly recruited indiv (on that day)
 END IF

	  !if column position has been 0-ed out (column because shouldn't be zero; could use other attribs)
 IF (i_col(ind) .EQ. 0) THEN
  totavail = totavail + 1               !available indiv space
 END IF

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE randindivs  !each hour, loop over all indivs and assign
USE time_etc           !them a random number
USE species
USE indivattributes
IMPLICIT NONE
REAL(8) pran        !a random number, even dist, 0-1
INTEGER xx

 DO xx = 1, totindivs           !loop over indivs
  xrand(xx) = pran(1)       !assign a random number
 END DO

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE maxcons(ind,ifwsp) !at start of day (first hr), extract cmax parameter values
USE physicalenv               !based on species. If senescent (finished spawning and last
USE indivattributes           !year), cmax = maintenance con (i.e., indiv tries to meet
USE bioenergetics             !metabolic needs only) assuming cmax is realized. Otherwise,
USE reproduction              !extract parameter values depending on indiv tot wght (g wwt).
IMPLICIT NONE                 !Then use indiv wght, water temp (oC), and param values to
							  !calc max cons (g prey / g wwt of indiv / day). Multi-part
							  !calc. Cold-water formulation for all sp.
REAL(8) unused      !proportion of daily cons that goes unused (due to egest, exrete, sda)
REAL(8) enerden     !energy density of benth, zoop, or indiv prey (Joules / g wwt)
REAL(8) dietenerden !energy density of diet based on 'enerden' and prop'n of ea prey type in diet (J / g wwt)
REAL(8) sumenerden  !total energy density across all vuln prey types (Joules / g wwt)
REAL(8) tempeffect  !a temperature-based scalar of cmax (bell-shaped, ranges from 0 to 1)
REAL(8) ca, cb, ctm, cto, cq, ctl !local shorthand for cmaxscalar, cmaxexpo, cmaxmaxt, cmaxoptt, cmaxtheta, etc.
REAL(8) part1, part2, part3, part4, part5 !parts of the equation for calculating 'tempeffect'
INTEGER ind         !local shorthand for 'indiv'
INTEGER ifwsp
INTEGER vulncount   !number of vulnerable prey types
INTEGER xx

IF (xsensce(ind) .EQ. 1) THEN !if indiv senescent (i.e., max age AND gonads too small, max reprod invest, all batches spawned, end of spawn season)
 vulncount=0; sumenerden=0.D0; dietenerden=0.D0 !zero count, running total of energy density of vuln prey, energy density of diet (Joules / g wwt prey)
 unused = egscalar(ifwsp) + (1.D0 - egscalar(ifwsp)) * (exscalar(ifwsp) + sdascalar(ifwsp)) !calc propon'n of daily cons that will go unused due to egest, etc
 CALL respiration(ind,ifwsp) !anticipate day's respiration (g O2 / g wwt of indiv/ day)
 DO xx = 1, preytypes !loop over all potential prey (1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)
  IF (preyv(ifwsp,xx) .EQ. 1) THEN !if prey type vulnerable
   vulncount = vulncount + 1 !add to count of vulnerable prey types
   enerden = preyenerden(xx) !if prey type zoop or benth, use zoop or benth energy density (J/g wwt)
   dietenerden = dietenerden + enerden * preypropn(ind,xx)   !calc energy density of previous day's diet (J / g wwt prey)
   sumenerden = sumenerden + enerden
   preypropn(ind,xx) = 0.D0 !zero proportion
  END IF !vulnerable
 END DO !prey field
 IF (dietenerden .LE. 0.D0) dietenerden = sumenerden / REAL(vulncount) !if no prey consumed, assume prey in equal proportions
 xcmax(ind) = (resp * calorcoeff) / (dietenerden * (1.D0 - unused)) !use previous day's diet to estim maint cmax [(gO2/g indiv * J/gO2) / (J/g prey) = g wwt prey / g wwt indiv / day] (assumes cmax will be realized)
ELSE                                 !otherwise, not senescing (i.e., not in last year of life or, if in last year of life, spawning is still an option)
 ca = cmaxscalar(ifwsp)              !store cmax scalar as local shorthand
 cb = cmaxexpo(ifwsp)                !store cmax exponent as local shorthand
 ctm = cmaxmaxt(ifwsp)               !store cmax max temp as local shorthand
 cto = cmaxoptt(ifwsp)               !store cmax opt temp as local shorthand
 cq = cmaxtheta(ifwsp)               !store cmax theta as local shorthand
 part1 = (ctm - wtemp) / (ctm - cto)!calculate parts 1-5 of the temp effect
 IF (part1 .LE. 0.D0) part1 = 0.D0  !if warmer than max, set to 0
 part2 = SQRT(1.0 + 40.0 / (LOG(cq) * (ctm - cto + 2.0)))
 part3 = (1.0 + part2)**2
 part4 = 0.0025 * (LOG(cq)**2) * ((ctm - cto)**2)
 part5 = part3 * part4
 tempeffect = part1**part5 * EXP(part5 * (1.0 - part1)) !calc temp effect (0 to 1: 0 at max temp, 1 at opt temp, otherwise on a bell curve)
 xcmax(ind) = (ca * xtotwght(ind) ** cb) * tempeffect    !calc cmax for day (g prey / g wwt of indiv / day)
END IF !scenescent

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE hoursplit(ind,ifwsp) !each hour, divide hour into segments (time steps) of
USE time_etc          !equal duration (seconds). Segment number is determined by
USE indivattributes   !seconds that it takes indiv traveling 1 body length/second to
USE movement          !travel to nhood corner. Allows movement to occur on realistic
IMPLICIT NONE         !time scale (e.g., 10 mm shrimp can travel 36 m in 1 hr, which
					  !is too large a nhood search). Assumes fixed environment within hour.
REAL(8) sensedist !maximum senstory distance in mm of an indivual of species x (i.e., diagonal distance from far corner of current cell to center of corner cell of fitness nhood)
INTEGER ind,ifwsp

sensedist = 1000.D0 * SQRT((1.D0+cellsize*REAL(nhood(ifwsp)))**2+(1.D0+cellsize*REAL(nhood(ifwsp)))**2) !calculate maximum sensory distance, which is from far corner of current cell to center of fitness nhood (m)
timesteps = INT(3600.D0 * xlen(ind) / sensedist) !divide total distance travelled in 1 hr (mm) by sensory distance (mm)
IF (timesteps .EQ. 0) timesteps = 1              !if timesteps = 0 (can occur if indiv very small), force to 1.

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE futuredepth(ind,j,ii,jj) !in each segment (time step) of each hour, if next hour or new cell, indiv
USE time_etc           !looks 3 hours into future and calculates water depth (m) in ea hour. If
USE physicalenv        !water depth in any hour < 'xmindep', flag that indiv must conduct
USE indivattributes    !emergency (depth-based) 'nhood' search in subroutine 'emernhood'.
USE movement           !Otherwise, indiv will conduct fitness-based search in 'fitnhood'.
IMPLICIT NONE          !If subroutine called from 'emernhood' ('emersearch' = 1), evaluate future
					   !water depth of candidate cell.
REAL(8) futrdep  !future water depth in current cell (m)
INTEGER futrhr   !future hour of year (1 to 8640)
INTEGER c, r     !local shorthand for 'i_col(indiv)' and 'j_row(indiv)'
INTEGER ind, ii, jj
INTEGER xx, j

IF (emersearch .EQ. 0) THEN      !if eval current cell (istead of neighborhood)
 IF (j .EQ. 1 .OR. j .EQ. timesteps) nhdflag = 1 !if first or last timestep in hour, reset flag (b/c new environment OR target could dry next hour)
 IF (nhdflag .EQ. 1) THEN        !if in new environment (b/c first timestep or in different cell)
  c = i_col(ind); r = j_row(ind) !store coord of current cell as local shorthand
  movetype = 0                   !assume fitness-based 'nhood' search
 END IF !in new environment
ELSE                             !otherwise, evaluating neighborhood for subroutine 'emernhd'
 c = i_col(ind)+ii; r = j_row(ind)+jj !store coord of candidate cell as local shorthand
 emertype = 1                    !assume future wet cell (therefore target nearest future wet cell)
END IF !eval type

IF ((emersearch .EQ. 0 .AND. nhdflag .EQ. 1) .OR. emersearch .EQ. 1) THEN !if eval current cell and in new environment OR eval for 'emernhood'
 DO xx = 1, 3                    !look 3 hours into future
  futrhr = cumhr-(year-1)*ndays*nhours + xx !calc future cum hour
  IF (futrhr .GT. ndays*nhours) THEN !if future hr > max hrs in year
   futrhr = cumhr-(year-1)*ndays*nhours - ndays*nhours + xx !calc future hr near start of next yr
   IF (watyr .NE. waterschedyrs) futrdep = waterlevel(watsched(watyr+1),futrhr) - cellelev(c,r) !if not last water yr, use 1st hr of next yr to calc future depth (water level - cell elev) (m)
   IF (watyr .EQ. waterschedyrs) futrdep = waterlevel(1,futrhr) - cellelev(c,r) !if last water yr, use 1st hr of 1st water yr, to calc future depth (water level - cell elev) (m)
  ELSE                               !otherwise, future hr not > max hrs in year
   futrdep = waterlevel(watsched(watyr),futrhr) - cellelev(c,r) !calc future depth (water level - cell elev) (m)
  END IF !future hr vs max hr
  IF (futrdep .LT. xmindep(ind)) THEN !if future depth too shallow for indiv (m)
   IF (emersearch .EQ. 0) movetype = 1 !if evaluating current cell, activate emergency, depth-based 'nhood' search
   IF (emersearch .EQ. 1) emertype = 0 !if evaluating nhood for subroutine 'emernhood', flag future dry cell
   EXIT                                !exit loop (no point in looking further into future)
  END IF !future depth vs minimum depth
 END DO !3 hours into future
END IF !current and new OR eval for 'emerenhood'

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE fitnhood(ind,j,ifwsp)  !in each segment (time step) of each hour that a fitness-based 'nhood' search is
USE time_etc         !warranted, if cell in nhood is at least 'xmindep' m deep (if last time step, cell
USE physicalenv      !must also be at least 'xmindep' m deep in next hr), calc fitness (i.e., future
USE indivattributes  !biomass) as product of expected tot weight (estim using bioenergetics, where food
USE movement         !avail is propn of indiv's biomass among biomass of all predators in cell) and
IMPLICIT NONE        !expected worth (nat mort and, if prey sp and predator(s) present, expected worth
					 !lost to pred'n (% of worth / g of pred) based on 10-hr memory of worth lost to
					 !pred'n). If available, use stored fitness calc. Target cell w hightest expected
					 !fitness. Ties broken by random choice among tied cells.
INTEGER ifwsp
INTEGER ind, nhd, sp   !local shortand for 'indiv', 'nhood', 'xspecies'
INTEGER c, r           !local shortand for col and row
INTEGER futrhr         !next hour's 'cumhr' (1 to 8640)
INTEGER firstflag      !flag indicating first time evaluating a wet cell (1=first, 0=not first)
INTEGER tiecount       !running total of number of cells tied for 'maxfit'
INTEGER tiemask(2,(nhood(spposn(xspecies(ind)))*4+1)**2) !an array of coord of cells w 'maxfit' (max = (nhood*2+1)**2 but use *4+1 b/c nhood sometimes doubled)
INTEGER maskposn       !a randomly-generated position in a hab type mask. Max value = number of cells in mask
REAL(8) futrdep        !future water depth in candidate cell (m)
REAL(8) pran           !a random number, even dist, 0-1
INTEGER j
INTEGER ii,jj
INTEGER ithread

ithread = omp_get_thread_num() + 1

firstflag = 1               !set flag to indicate first time evaluating a wet cell
c = i_col(ind); r = j_row(ind) !store col and row as local shorthand
sp = xspecies(ind)
nhd = nhood(ifwsp)           !store sp-specific neighborhood as local shorthand
IF (sp .EQ. 3 .AND. habitat(c,r) .EQ. 1) THEN
	nhd=nhd*3
END IF
IF (nhdflag .EQ. 1) THEN    !if new hour or in new cell (i.e., environment or nhood changed)
 futrhr = cumhr-(year-1)*ndays*nhours+1   !calc 'cumhr' of next hour
 IF (futrhr .GT. ndays*nhours) futrhr = 1 !if future hr > max hrs in year, go to start of yr

 DO ii = -nhd, nhd          !loop over all columns in neighborhood
  IF (c+ii .LT. 1 .OR. c+ii .GT. cols) CYCLE !do not look off-grid

  DO jj = -nhd, nhd         !loop over all rows in neighborhood
   IF (r+jj .LT. 1 .OR. r+jj .GT. rows) CYCLE !do not look off-grid

   IF (cumhr-(year-1)*ndays*nhours+1 .GT. ndays*nhours) THEN !if future hr > max hrs in yr
	IF (watyr .NE. waterschedyrs) futrdep = waterlevel(watsched(watyr+1),futrhr) - cellelev(c+ii,r+jj) !if not last water yr, use 1st hr of next yr to calc future depth (water level - cell elev) (m)
	IF (watyr .EQ. waterschedyrs) futrdep = waterlevel(1,futrhr) - cellelev(c+ii,r+jj)         !if last water yr, use 1st hr of 1st water yr, to calc future depth (water level - cell elev) (m)
   ELSE                                   !otherwise, future hr not > max hrs in yr
	futrdep = waterlevel(watsched(watyr),futrhr) - cellelev(c+ii,r+jj) !calc future depth (water level - cell elev) (m)
   END IF !future hr vs max hrs in yr

   IF ((j .LT. timesteps .AND. waterdepth(c+ii,r+jj) .GE. xmindep(ind)) &   !if not last timestep and candidate cell currently deep enough
	 .OR. (j .EQ. timesteps .AND. waterdepth(c+ii,r+jj) .GE. xmindep(ind) & !OR last timestep and candidate cell currently deep enough
	 .AND. futrdep .GE. xmindep(ind))) THEN                                 !AND deep enough in next hour

	IF (fitflag(c+ii,r+jj) .EQ. ind) THEN !if fitness already evaluated by that individual
	 expfit = fitstore(c+ii,r+jj)       !extract expected fitness (g wwt)
	ELSE                                !otherwise, exp fitness net yet evaluated
	 CALL fiteval(ii,jj,ind,ifwsp)                       !evaluate fitness
	 fitstore(c+ii,r+jj) = expfit       !store exp fitness for re-use within hour (g wwt)
	 fitflag(c+ii,r+jj) = ind           !flag that fitness evaluated by individual (reset at start of each hour)
	END IF !fitness already evaluated

	IF (firstflag .EQ. 1) THEN     !if first time through loop
	 maxfit = expfit               !store fitness as maximum so far encountered (g wwt)
	 tiecount = 1                  !set mask counter to 1
	 tiemask(1,tiecount) = c+ii    !store first cell coord
	 tiemask(2,tiecount) = r+jj

	ELSE                           !otherwise, not first time through loop
	 IF (expfit .EQ. maxfit) THEN  !if expected fitness same as max observed fitness (g wwt)
	  tiecount = tiecount + 1      !increment mask counter
	  tiemask(1,tiecount) = c+ii   !add to store of cell coord
	  tiemask(2,tiecount) = r+jj
	 END IF !expfit = maxfit

	 IF (expfit .GT. maxfit) THEN  !if expected fitness highest so far encountered (g wwt)
	  maxfit = expfit              !store fitness as maximum so far encountered (g wwt)
	  tiecount = 1                 !reset mask counter
	  tiemask(1,tiecount) = c+ii   !store cell coord
	  tiemask(2,tiecount) = r+jj
	 END IF !expfit > maxfit
	END IF !index value

	firstflag = 0                  !turn off flag indicating first time evaluating a wet cell
   END IF !wet in next hour
  END DO !nhood rows
 END DO !nhood cols

 IF (tiecount .GT. 1) THEN         !if 2+ cells tied for fitness
  maskposn = INT(pran(ithread) * tiecount) + 1 !choose random position in array of tied cells
  IF (maskposn .GT. tiecount) maskposn = tiecount !if 'ran11(idum)' exactly 1, set 'maskposn' to max mask posn
 ELSE                              !otherwise, only one cell w highest fitness
  maskposn = tiecount              !set mask position to 1
 END IF !tiecount

 newcol = tiemask(1,maskposn)      !extract coord of cell
 newrow = tiemask(2,maskposn)

END IF !neighborhood search required

IF (fileflag(13) .EQ. 1 .AND. newcol .EQ. c .AND. newrow .EQ. r) THEN !if output desired AND targeting current cell
 outflag = 13; CALL output(0,j,0,0,0,0,0) !output details of fine-scale movement (13=file 130)
END IF

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE fiteval(ii,jj,ind,ifwsp)  !calc future fitness (i.e., biomass) in candidate cell as product of
USE time_etc        !expected weight and expected worth. Former is based on bioenergetics
USE physicalenv     !assuming current conditions in cell. Latter is combo of nat mort and
USE indivattributes !expected worth lost to pred'n (% of worth lost / g of pred based on
USE bioenergetics   !24-hr memory of worth lost to pred'n).
USE movement
IMPLICIT NONE
INTEGER ind, c, r           !local shortand for 'indiv', col, row
REAL(8) exptotwght          !expected future tot wght in candidate cell (g wwt)
REAL(8) expworth            !expected worth in candidate cell
REAL(8) pran                !a random number, even dist, 0 - 1
INTEGER ii, jj
INTEGER ithread
INTEGER ifwsp

ithread = omp_get_thread_num() + 1

c = i_col(ind); r = j_row(ind)                !store col and row as local shorthand
fitsearch = 1                                 !flag that subroutine being called during fitness neighborhood search (prevents actual changes to indivs and prey)
CALL freecons(ii,jj,ind,ifwsp)                !calc consumption in candidate cell (g prey / g wwt of indiv / hour)
CALL respiration(ind,ifwsp)                   !calc respiration in candidate cell (g O2 / g wwt of indiv / hour)
CALL energychange(ind,ifwsp)                  !calc energy change in candidate cell (J)
exptotwght = xtotwght(ind) + (enerchng / predenerden(ifwsp)) !convert tot expected ener change to tot wght (J / J/g = g wwt)
fitsearch = 0                                 !turn off flag
expworth = xworth(ind) * EXP(-xmort(ind) * mortmult(ifwsp,habitat(c+ii,r+jj))) !calc expected worth using indiv's nat mort as modified by predation (sp- and hab-speicific multiplier)
expfit = exptotwght * expworth * (pran(ithread) * fiterror(ifwsp) * 2.D0 + (1.D0 - fiterror(ifwsp))) !calc expected fitness, including error (g wwt)

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE emernhood(ind,j,ifwsp) !in each segment (time step) of each hour that a depth-based 'nhood' search is
USE time_etc         !warranted, double 'nhood'. Target closest cell that is wet. If not wet cells,
USE physicalenv      !target deepest. Ties broken by coin toss. Approach is similar to Roth et al 2008
USE indivattributes
USE movement
IMPLICIT NONE
REAL(8) newx, newy   !x and y distances to center of target cell (m)
REAL(8) distx, disty !horizontal and vertical components of dist betw indiv and center of candidate cell (m)
REAL(8) distance     !linear dist betw indiv and center of candidate cell (m)
REAL(8) pran         !a random number, even dist, 0-1
REAL(8) cointoss     !local name for 'ran11'
REAL(8) x, y         !local shortand for x_dist and y_dist
INTEGER futrwet      !future wetness of candidate cell (extracted from storage array)
INTEGER wetfound     !flag indicating future wet cell found
INTEGER ind, nhd     !local shortand for 'indiv' and 'nhood'
INTEGER c, r         !local shortand for col and row
INTEGER ifwsp
INTEGER ii, jj
INTEGER j
INTEGER ithread

ithread = omp_get_thread_num() + 1

neerest=2.D0*(cols*cellsize)!set smallest recorded dist to wet cell to impossibly large number (2x width of sub-grid) (m)
deepest=0.D0                !zero highest recorded depth in neighborhood (m)

IF (nhdflag .EQ. 1) THEN    !if new hour or in new cell (i.e., environment or nhood changed)
! ind = indiv                !store 'indiv' as local shorthand
 c = i_col(ind); r = j_row(ind) !store col and row as local shorthand
 x = x_dist(ind); y = y_dist(ind) !store x and y distances as local shorthand (m)
 nhd = nhood(ifwsp) * emermult(ifwsp) !increase radius of searchable neighborhood and store as local shorthand
 wetfound=0                 !zero flag
 newcol=c; newrow=r         !assume targeting current cell

 DO ii = -nhd, nhd          !loop over all columns in neighborhood
  IF (c+ii .LT. 1 .OR. c+ii .GT. cols) CYCLE  !do not look off-grid

  DO jj = -nhd, nhd         !loop over all rows in neighborhood
   IF (r+jj .LT. 1 .OR. r+jj .GT. rows) CYCLE !do not look off-grid

   newx = (c+ii) * cellsize - cellsize / 2.D0 !convert coord of candidate cell to distance. Assume center of cell (m)
   newy = (r+jj) * cellsize - cellsize / 2.D0
   distx = x - newx                           !calc horiz and vert dist between current and candidate (m)
   disty = y - newy
   distance = SQRT((distx**2) + (disty**2))   !calc linear dist betw indiv and center of candidate cell (m)

   IF ((wetfound .EQ. 0) .OR. (wetfound .EQ. 1 .AND. distance .LE. neerest)) THEN !if no wet cell yet OR candidate at least as close as best so far
	IF (evalwet(c+ii,r+jj) .NE. ind) THEN !if indiv has not yet evaluated future wetness in candidiate cell
	 emersearch = 1                     !flag subroutine 'futuredepth' being called from 'emernhood'
	 CALL futuredepth(ind,j,ii,jj)                   !evaluate future wetness of candidate cell (1 = wet in future)
	 emersearch = 0                     !turn off flag
	 wetstore(c+ii,r+jj) = emertype     !save future wetness in storage array for re-use by indiv within hour (never reset b/c read and write to array controlled by 'evalwet')
	 evalwet(c+ii,r+jj) = ind           !flag cell wetness evaluated by indiv in hour (use 'indiv' so only need to reset at start of each hour)
	END IF

	futrwet = wetstore(c+ii,r+jj)       !extract future wetness from indiv's storage array

	IF (futrwet .EQ. 1) THEN            !if candidate cell wet in future
	 deepest = 0.D0                     !zero deepest (for output file 130)
	 wetfound = 1                       !flag wet cell found

	 IF (distance .EQ. neerest) THEN    !if center of candiate cell same dist as closest so far
	  cointoss = pran(ithread)            !flip a coin
	  IF(cointoss .LT. 0.5D0) THEN      !if heads
	   newcol = c+ii; newrow = r+jj     !store cell coord (otherwise, if tails, neerest = neerest and target coord unchanged)
	  END IF !heads or tails            !(not perfect if >2 ties b/c biased towards last cell evaluated, but >2 ties rare)
	 ELSE                               !otherwise, distance < neerest (b/c first cell evaluated or candidate already deemed closer)
	  neerest = distance                !store distance as closest so far evaluated
	  newcol = c+ii; newrow = r+jj      !store cell coord
	 END IF !distance vs nearest
	END IF !future wet cell
   END IF !no wet cell found or new at least as close

   IF (wetfound .EQ. 0) THEN            !if future wet cell not yet found
	 neerest = 2.D0 * (cols*cellsize)   !reset neerest (for output file 130)
	IF (waterdepth(c+ii,r+jj) .EQ. deepest) THEN !if depth of candidate cell same as 'best' cell so far (m)
	 cointoss = pran(ithread)             !flip a coin
	 IF (cointoss .LT. 0.5D0) THEN      !if heads
	  newcol = c+ii; newrow = r+jj      !store cell coord (otherwise, if tails, deepest = deepest and cell coord unchanged)
	 END IF !heads or tails             !(not perfect if >2 ties b/c biased towards last cell evaluated, but >2 ties rare)
	END IF !depth = deepest
	IF (waterdepth(c+ii,r+jj) .GT. deepest) THEN !if candidate cell deepest so far evaluated (m)
	 deepest = waterdepth(c+ii,r+jj)    !store deepest water depth (m)
	 newcol = c+ii; newrow = r+jj       !store cell coord
	END IF !depth > deepest
   END IF !no wet cell found

  END DO !nhood rows
 END DO !nhood cols
END IF !nhdflag

IF (fileflag(13) .EQ. 1 .AND. newcol .EQ. i_col(ind) .AND. newrow .EQ. j_row(ind)) THEN !if output desired AND targeting current cell
 outflag = 13; CALL output(0,j,0,0,0,0,0) !output details of fine-scale movement (12=file 130)
END IF

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE move(j,ind)     !in each segment (time step) of each hour, if targeting different cell b/c
USE time_etc        !of emergency movement, simply place indiv in center of target cell. If
USE physicalenv     !targeting different cell b/c of fitness, move indiv toward target cell.
USE species         !Movement angle and dist is made crooked by different multipliers (betw
USE indivattributes !0 and 1) on both horiz and vert components BUT indiv cannot wander onto
USE movement        !dry cell or, if last time step, onto cell that is dry or will be dry in
					!next hour. If indiv goes off grid, use 'bouncing' boundary to position
					!back onto grid.
INTEGER c, r           !local shorthand for 'i_col(indiv)' and 'j_row(indiv)'
INTEGER futrhr         !next hour's 'cumhr' (1 to 8640)
INTEGER immnhd         !search radius that defines indiv's immediate neighborhood (cells0
INTEGER z              !local index for error do loop
REAL(8) x, y           !local shorthand for 'x_dist(indiv)' and 'y_dist(indiv)'
REAL(8) newx, newy     !x and y coord in target cell (m)
REAL(8) distx, disty   !x and y distances to target cell (m)
REAL(8) opp, adj       !opposite and adjacent sides of a triangle
REAL(8) mvdist         !linear distance between current and target location (m)
REAL(8) theta          !angle of movement defined by staight-line distance betw current and targt x,y distances
REAL(8) degr           !angle of movement in degrees
REAL(8) move_x, move_y !movement distances in x and y towards target
REAL(8) futrdep        !future water depth in current or (if future is dry) candidate cell (m)
REAL(8) shortest       !shortest distance to wet cell in immediate neighborhood if random movement placed indiv on dry cell (m)
REAL(8) pran           !random number, even dist betw 0 and 1
REAL(8) cointoss       !local name for 'ran11' when randomly choosing between two, equally distant wet cells
INTEGER j, ii, jj
INTEGER ithread
INTEGER ind

ithread = omp_get_thread_num() + 1

IF (newcol .NE. i_col(ind) .OR. newrow .NE. j_row(ind)) THEN !if at least one coord of target cell different
 IF (movetype .EQ. 1) THEN             !if depth-based movement
  c = newcol; r = newrow               !place indiv in target cell
  x = c * cellsize - cellsize / 2.D0   !set x and y distances from origin to center of new cell (m)
  y = r * cellsize - cellsize / 2.D0
 ELSE                                  !otherwise, fitness-based movement
  x = x_dist(ind); y = y_dist(ind)     !store distances from grid origin as local shorthand (m)
  newx = (newcol - 1) * cellsize + cellsize / 2.D0 !determine x amd y coord at center of target cell (m)
  newy = (newrow - 1) * cellsize + cellsize / 2.D0

  opp = (newy - y)                         !calculate opposite and adjacent based on where on grid
  adj = (newx - x)                         !fish is and where it wants to go (m)
  mvdist = SQRT((adj)**2 + (opp)**2)       !calculate linear distance between current and target cell (m) then apply multiplier (next 5 lines)
  mintri=0.D0                              !define min value of triangular distribn
  modtri=0.8D0                             !define modular value of triangular distribn. Incr or decr to shorten or lengthen move dist (mod cannot exceed 0.99 when mintri=0 and maxtri=1)
  maxtri=1.D0                              !define max value of triangular distribn
  CALL trian(mintri,modtri,maxtri,randtri) !draw a random number from triangular distribn
  mvdist = mvdist * randtri                !use random value from triangular distribn as a multiplier of move distance

  IF (adj .EQ. 0.D0) THEN                  !if movement is straight up or down
   IF (opp .GT. 0.D0) THEN                 !if movement is straight up
	theta = ACOS(-1.D0) / 2.D0             !angle of movement (radians) is pi/2
   ELSE                                    !otherwise, movement is straight down
	theta = 3.D0 * ACOS(-1.D0) / 2.D0      !angle of movement (radians) is 3pi/2
   END IF
  ELSE                                     !otherwise, movement is not straight up or down
   theta = ATAN(ABS(opp) / ABS(adj))       !calculate angle of movement in radians
  END IF

  IF (opp .GT. 0.D0 .AND. adj .LT. 0.D0) theta = ACOS(-1.D0) - theta        !if movement is up and left, correct theta
  IF (opp .LT. 0.D0 .AND. adj .LT. 0.D0) theta = ACOS(-1.D0) + theta        !if movement is down and left, correct theta
  IF (opp .LT. 0.D0 .AND. adj .GT. 0.D0) theta = 2.D0 * ACOS(-1.D0) - theta !if movement is down and right, correct theta
  IF (opp .EQ. 0.D0 .AND. adj .LT. 0.D0) theta = ACOS(-1.D0)                !if movement is straight left, correct theta

  degr = theta * 180.D0 / ACOS(-1.D0)      !convert radians to degrees (where ACOS(-1) = pi)
  mintri=-22.5D0                           !define min value of triangular distribn
  modtri=0.D0                              !define modular value of triangular distribn. Incr or decr to make movement more or less accurate, respectively. Max 0.99
  maxtri=22.5D0                            !define max value of triangular distribn
  CALL trian(mintri,modtri,maxtri,randtri) !draw a random number from triangular distribn
  degr = degr + randtri                    !use random number from triangular distrib as a multiplier of move angle
  IF (degr .LT. 0.D0) degr = 360.D0 + degr !correct negative degrees caused by randomization
  IF (degr .GT. 360.D0) degr = degr - 360.D0 !correct overly large degrees caused by randomization
  theta = degr * ACOS(-1.D0) / 180.D0      !convert degrees back to radians

  move_x = (mvdist * COS(theta))           !calculate distance moved in x direction
  move_y = (mvdist * SIN(theta))           !calculate distance moved in y direction
  x = x + move_x                           !move towards target (i.e., change x and y dist) (m)
  y = y + move_y

  IF (x .LT. 0.D0) x = ABS(x)              !if off-grid, change so that bounce off of grid wall
  IF (x .GT. cols*cellsize) x = cols * cellsize - (x - cols * cellsize)
  IF (y .LT. 0.D0) y = ABS(y)
  IF (y .GT. rows*cellsize) y = rows * cellsize - (y - rows * cellsize)

  c = INT(x / REAL(cellsize)) + 1          !convert new distances to new cell coordinates
  r = INT(y / REAL(cellsize)) + 1

  futrhr = cumhr-(year-1)*ndays*nhours+1   !calc 'cumhr' of next hour

  IF (futrhr .GT. ndays*nhours) THEN       !if future hr > max hrs in yr
   futrhr = 1                              !go to start of yr
   IF (watyr .NE. waterschedyrs) futrdep = waterlevel(watsched(watyr+1),futrhr) - cellelev(c,r) !if not last water yr, use 1st hr of next yr to calc future depth (water level - cell elev) (m)
   IF (watyr .EQ. waterschedyrs) futrdep = waterlevel(1,futrhr) - cellelev(c,r) !if last water yr, use 1st hr of 1st water yr, to calc future depth (water level - cell elev) (m)
  ELSE                                     !otherwise, future hr not > max hrs in yr
   futrdep = waterlevel(watsched(watyr),futrhr) - cellelev(c,r) !calc future depth (water level - cell elev) (m)
  END IF !future hr vs max hrs in yr

  IF ((j .LT. timesteps .AND. waterdepth(c,r) .LT. xmindep(ind)) &    !if not last time step and new cell currently too shallow
	.OR. (j .EQ. timesteps .AND. (waterdepth(c,r) .LT. xmindep(ind) & !OR last time step and new cell currently too shallow
	.OR. futrdep .LT. xmindep(ind)))) THEN                            !OR too shallow in next hour

   shortest = 3.D0 * cellsize !set shortest dist to wet cell in immediate neighborhood to impossibly large number (m)
   immnhd = 1                 !define search radius of immediate neighborhood (cells)

   DO ii = -immnhd, immnhd    !loop over all columns in immediate neighborhood
	IF (c+ii .LT. 1 .OR. c+ii .GT. cols) CYCLE  !do not look off-grid

	DO jj = -immnhd, immnhd   !loop over all rows in immediate neighborhood
	 IF (r+jj .LT. 1 .OR. r+jj .GT. rows) CYCLE !do not look off-grid

	 IF (cumhr-(year-1)*ndays*nhours+1 .GT. ndays*nhours) THEN !if future hr > max hrs in yr
	  IF (watyr .NE. waterschedyrs) futrdep = waterlevel(watsched(watyr+1),futrhr) - cellelev(c+ii,r+jj) !if not last water yr, use 1st hr of next yr to calc future depth (water level - cell elev) (m)
	  IF (watyr .EQ. waterschedyrs) futrdep = waterlevel(1,futrhr) - cellelev(c+ii,r+jj)         !if last water yr, use 1st hr of 1st water yr, to calc future depth (water level - cell elev) (m)
	 ELSE                                   !otherwise, future hr not > max hrs in yr
	  futrdep = waterlevel(watsched(watyr),futrhr) - cellelev(c+ii,r+jj) !calc future depth (water level - cell elev) (m)
	 END IF !future hr vs max hrs in yr

	 IF ((j .LT. timesteps .AND. waterdepth(c+ii,r+jj) .GE. xmindep(ind)) &   !if not last time step and candidate cell currently deep enough
	   .OR. (j .EQ. timesteps .AND. waterdepth(c+ii,r+jj) .GE. xmindep(ind) & !OR last time step and candidate cell currently deep enough
	   .AND. futrdep .GE. xmindep(ind))) THEN                                 !AND deep enough in next hour

	  IF (ii .EQ. 0 .AND. jj .EQ. 1) THEN      !if cardinal direction N
	   distx = 0.D0                            !calc x dist from current location to just beyond cell border
	   disty = (r * cellsize) - y + 0.01D0     !calc y dist from current location to just beyond cell border
	  END IF
	  IF (ii .EQ. 1 .AND. jj .EQ. 1) THEN      !if non-cardinal direction NE
	   distx = (c * cellsize) - x + 0.01D0     !calc x dist from current location to just beyond cell border
	   disty = (r * cellsize) - y + 0.01D0     !calc y dist from current location to just beyond cell border
	  END IF
	  IF (ii .EQ. 1 .AND. jj .EQ. 0) THEN      !if cardinal direction E
	   distx = (c * cellsize) - x + 0.01D0     !calc x dist from current location to just beyond cell border
	   disty = 0.D0                            !calc y dist from current location to just beyond cell border
	  END IF
	  IF (ii .EQ. 1 .AND. jj .EQ. -1) THEN     !if non-cardinal direction SE
	   distx = (c * cellsize) - x + 0.01D0     !calc x dist from current location to just beyond cell border
	   disty = (cellsize * (r-1)) - y - 0.01D0 !calc y dist from current location to just beyond cell border
	  END IF
	  IF (ii .EQ. 0 .AND. jj .EQ. -1) THEN     !if cardinal direction S
	   distx = 0.D0                            !calc x dist from current location to just beyond cell border
	   disty = (cellsize * (r-1)) - y - 0.01D0 !calc y dist from current location to just beyond cell border
	  END IF
	  IF (ii .EQ. -1 .AND. jj .EQ. -1) THEN    !if non-cardinal direction SW
	   distx = (cellsize * (c-1)) - x - 0.01D0 !calc x dist from current location to just beyond cell border
	   disty = (cellsize * (r-1)) - y - 0.01D0 !calc y dist from current location to just beyond cell border
	  END IF
	  IF (ii .EQ. -1 .AND. jj .EQ. 0) THEN     !if cardinal direction W
	   distx = (cellsize * (c-1)) - x - 0.01D0 !calc x dist from current location to just beyond cell border
	   disty = 0.D0                            !calc y dist from current location to just beyond cell border
	  END IF
	  IF (ii .EQ. -1 .AND. jj .EQ. 1) THEN     !if non-cardinal direction NW
	   distx = (cellsize * (c-1)) - x - 0.01D0 !calc x dist from current location to just beyond cell border
	   disty = (r * cellsize) - y + 0.01D0     !calc y dist from current location to just beyond cell border
	  END IF

	  distance = SQRT((distx**2) + (disty**2)) !calc straight-line dist from current to new loc

	  IF (distance .LE. shortest) THEN  !if distance to neighboring wet cell <= to shortest dist so far observed
	   IF (distance .EQ. shortest) THEN !if distance to neighboring wet cell = to shortest dist so far observed
		cointoss = pran(ithread)          !flip a coin
		IF(cointoss .LT. 0.5D0) THEN    !if heads
		 newx = x+distx; newy = y+disty !store dist coord (otherwise, if tails, shortest = shortest and dist coord unchanged)
		END IF !heads or tails          !(not perfect if >2 ties b/c biased towards last cell evaluated, but >2 ties highly unlikely)
	   ELSE                             !otherwise, distance < shortest
		shortest = distance             !store distance as shortest so far evaluated
		newx = x+distx; newy = y+disty  !store cell coord
	   END IF !distance < or = shortest
	  END IF !distance <= shortest

	 END IF !cell in immediate neighborhood wet (and wet in next time step)
	END DO !immnhd rows
   END DO !immnhd cols

   x = newx; y = newy               !store new distances as local shorthand
   c = INT(x / REAL(cellsize)) + 1  !convert new distances to new cell coordinates
   r = INT(y / REAL(cellsize)) + 1

  END IF !new cell dry (or dry in next time step)
 END IF ! move type

 IF (x .LT. 0.D0 .OR. x .GT. cols*cellsize .OR. y .LT. 0.D0 .OR. y .GT. rows*cellsize &
   .OR. c .LT. 1 .OR. c .GT. cols .OR. y .LT. 0.D0 .OR. y .GT. rows*cellsize) THEN !make sure indiv on-grid
  DO z = 1, 25
   CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
  END DO
  PRINT *, "Error in subroutine 'movement':"
  IF (x .LT. 0.D0 .OR. x .GT. cols*cellsize) PRINT *, "the x_dist of indiv ", ind, " is", x
  IF (y .LT. 0.D0 .OR. y .GT. rows*cellsize) PRINT *, "the y_dist of indiv ", ind, " is", y
  IF (c .LT. 1 .OR. c .GT. cols) PRINT *, "the i_col of indiv ", ind, " is", c
  IF (r .LT. 1 .OR. r .GT. rows) PRINT *, "the j_row of indiv ", ind, " is", r
  PRINT *, "The indiv is off-grid."
  PRINT *, "Simulation stopped."
  CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
  STOP
 END IF

 IF (movetype .EQ. 0) neerest=2.D0*(cols*cellsize)!set smallest recorded dist to wet cell to impossibly large number (2x width of sub-grid) (m)
 IF (movetype .EQ. 0) deepest=0.D0                !zero highest recorded depth in neighborhood (m)
 IF (movetype .EQ. 1) maxfit=0.D0                 !zero highest recorded fitness in neighborhood

 IF (fileflag(13) .EQ. 1) THEN !if output desired
  outflag = 13; CALL output(0,0,0,0,0,0,0) !output details of fine-scale movement (13=file 130)
 END IF

 IF (c .EQ. i_col(ind) .AND. r .EQ. j_row(ind)) THEN !if in same cell after movement
  nhdflag = 0                                            !flag that neighborhood search in next j not needed (target cell unchanged)
 ELSE                                                    !otherwise, moved to different cell
  nhdflag = 1                                            !flag that neighborhood search needed to find new target cell
 END IF

 i_col(ind) = c; j_row(ind) = r   !convert shorthand back to longhand for cell coord
 x_dist(ind) = x; y_dist(ind) = y !and distance from grid origin (m)

END IF !target cell not current cell

maxfit=0.D0                 !zero highest recorded fitness in neighborhood
neerest=2.D0*(cols*cellsize)!set smallest recorded dist to wet cell to impossibly large number (2x width of sub-grid) (m)
deepest=0.D0                !zero highest recorded depth in neighborhood (m)

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE fishbycell(ind,ifwsp)  !each hour, add indiv to sum of abundance, worth, and biomass
USE time_etc           !by sp and cell. Keep a record of model indiv IDs in ea cell
USE physicalenv        !as well as cell with highest recorded worth.
USE species
USE indivattributes
USE movement
USE reproduction
IMPLICIT NONE
INTEGER c, r, z  !shorthand for  "i_col(ind)" and "j_row(ind)", index for the error loop
INTEGER ind, ifwsp

c = i_col(ind); r = j_row(ind)    !store column and row as local shorthand
CALL species_lock(ifwsp)
CALL cell_lock(c,r)
indivbycell(ifwsp,c,r) = indivbycell(ifwsp,c,r) + 1 !add to running total of indivs of species 'sp' in cell
IF (indivbycell(ifwsp,c,r) .GT. cellmax) THEN      !if number of indivs in cell exceeds some maximum
 DO z = 1, 25
  CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
 END DO
 PRINT *, "Error in 'fishbycell': the addition of indiv", ind
 PRINT *, "has resulted in too many indivs. Species is", ifwsp
 PRINT *, "Number of indivs is", indivbycell(ifwsp,c,r)
 PRINT *, "which exceeds the maximum of", cellmax
 PRINT *, "in column and row", c, r
 PRINT *, "on year, day, hour", year, jday, hour
 PRINT *, "Simulation stopped."
 CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
 STOP
END IF !error
worthbycell(ifwsp,c,r) = worthbycell(ifwsp,c,r) + xworth(ind) !add to running total of worths of species 'sp' in cell
IF (worthbycell(ifwsp,c,r) .GT. mxwrth(ifwsp)) THEN !if worth in current cell greater than worth so far seen
 mxwrth(ifwsp) = worthbycell(ifwsp,c,r)             !store sp-specific worth
 maxcoord(ifwsp,1) = c; maxcoord(ifwsp,2) = r       !store coord of cell in which sp-specific max observed
END IF !max worth observed
bmassbycell(ifwsp,c,r) = bmassbycell(ifwsp,c,r) + xworth(ind) * xtotwght(ind) !add to running total of biomass (g wwt) of species 'sp' in cell
annualbmass(ifwsp)=annualbmass(ifwsp)+xworth(ind) * xtotwght(ind)
!idbycell(ifwsp,c,r,indivbycell(ifwsp,c,r)) = ind  !record ID of indiv in cell
habdist(ifwsp,habitat(c,r)) = habdist(ifwsp,habitat(c,r)) + xworth(ind) !add to running total of abund in ea hab type (worth)
CALL cell_unlock(c,r)
CALL species_unlock(ifwsp)
END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE natmort(ind,ifwsp)  !each hour, apply indiv's expected weight-based nat mort rate. If
USE time_etc        !indiv in deep enough water, add worth lost to nat mort (zero worth
USE physicalenv     !if too small). If in shallow water, add worth lost to strand mort
USE species         !(zero worth if too small). If shrimp stranded for >5 consecutive
USE indivattributes !hrs, assume dead as per Roth et al (2008). For all other strand sp,
USE movement        !assume dead when in wet but shallow cell for >5 cons hrs, or if
IMPLICIT NONE       !cell becomes dry (whichever comes first). To prevent edge effects,
					!max 100 indivs can strand in a cell that is within 5 cells of sub-
					!grid edge. After 100 strandings, indiv moved to random deep water.
INTEGER ind, c, r !local shortand for 'indiv', 'i-col', and 'j_row'
INTEGER maskposn  !randomly-generated position in hab type mask. Max value = number of cells in mask
REAL(8) pran     !random number, even dist, 0 - 1
REAL(8) nextworth
INTEGER ifwsp
INTEGER ithread

ithread = omp_get_thread_num() + 1
c = i_col(ind); r = j_row(ind)                        !store cell coord as local shorthand

CALL species_lock(ifwsp)
CALL cell_lock(c,r)
nextworth = xworth(ind) * EXP(-xmort(ind)* mortmult(ifwsp,habitat(c,r)))
xworthchng(ind) = xworth(ind) - nextworth !calc change in worth as result of nat mort (mod by predation via sp-specific hab multiplier)
xworth(ind) = nextworth !use mortality to decrement worth
!DEC$ IF (.NOT. DEFINED(REGISTER_ONLY_LAST_WORTH))
worthannualsumming(xspecies(ind))=worthannualsumming(xspecies(ind))+ xworth(ind)
!DEC$ ENDIF
speciesworthdailysumming(xspecies(ind))=speciesworthdailysumming(xspecies(ind))+ xworth(ind)
xworthdailysumming(ind)=xworthdailysumming(ind)+xworth(ind)
IF (waterdepth(c,r) .GE. xmindep(ind)) THEN           !if indiv in deep enough water after 'j' timesteps
 hoursdry(ind) = 0                                    !reset 'hours in dry cell' counter
 mortloss(ifwsp,1) = mortloss(ifwsp,1) + xworthchng(ind)    !add change in worth to daily running tot of worth lost to nat mort
 cummortloss(1) = cummortloss(1) + xworthchng(ind)          !add change in worth to annual running tot of worth lost to nat mort
 IF (xsensce(ind) .EQ. 0) THEN 
    netproductivity(ifwsp,c,r) = netproductivity(ifwsp,c,r) - (xworthchng(ind) * xtotwght(ind)) !if individula is not senescent, subtract change in biomass from annual running total (g wwt)
    productivityvalues(ifwsp,c,r) = productivityvalues(ifwsp,c,r) - (xworthchng(ind) * (xtotwght(ind))) !if individula is not senescent, subtract change in biomass from annual running total (g wwt)
    UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_NOT_SENESCENT(ind)
 ELSE
    UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_SENESCENT(ind)
 END IF
 IF (xworth(ind) .LT. minworth) THEN                  !if updated worth very small
  mortloss(ifwsp,1) = mortloss(ifwsp,1) + xworth(ind) !add change in worth to daily running tot of worth lost to nat mort
  cummortloss(1) = cummortloss(1) + xworth(ind)       !add change in worth to annual running tot of worth lost to nat mort
  IF (xsensce(ind) .EQ. 0) THEN 
    netproductivity(ifwsp,c,r) = netproductivity(ifwsp,c,r) - (xworth(ind) * xtotwght(ind)) !if individula is not senescent, subtract remaining biomass from annual running total (g wwt)
    productivityvalues(ifwsp,c,r) = productivityvalues(ifwsp,c,r) - (xworth(ind) * (xtotwght(ind))) !if individula is not senescent, subtract change in biomass from annual running total (g wwt)
    UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_NOT_SENESCENT(ind)
  ELSE
    UPDATE_NATURAL_MORTALITY_TROPHIC_TRANSFER_SENESCENT(ind)
  END IF
  xalive(ind) = 2                                     !remove indiv from population (0=unassigned, 1=alive, 2=dead(nat mort incl. predation), 3=dead(starved), 4=dead(stranded), 5=dead(old))
 END IF !small worth
ELSE                                                  !otherwise, water too shallow for indiv (indiv can't move)
 IF (fileflag(13) .EQ. 1) THEN                        !if output desired
  outflag = 13; CALL output(0,0,0,0,0,0,0)            !output details of fine-scale movement (13=file 130)
 END IF
 mortloss(ifwsp,3) = mortloss(ifwsp,3) + xworthchng(ind)    !add change in worth to daily running tot of worth lost to stranding
 cummortloss(3) = cummortloss(3) + xworthchng(ind)          !add change in worth to annual running tot of worth lost to stranding
 IF (xsensce(ind) .EQ. 0) THEN 
    netproductivity(ifwsp,c,r) = netproductivity(ifwsp,c,r) - (xworthchng(ind) * xtotwght(ind)) !if individula is not senescent, subract change in biomass from annual running total (g wwt)
    UPDATE_STRANDING_TROPHIC_TRANSFER_NOT_SENESCENT(ind)
 ELSE 
    UPDATE_STRANDING_TROPHIC_TRANSFER_SENESCENT(ind)
 END IF
 hoursdry(ind) = hoursdry(ind) + 1                    !increment count of hours stranded
 IF (xworth(ind) .LT. minworth .OR. hoursdry(ind) .GT. 5 & !if updated worth very small OR >5 hours in dry cell
   .OR. (xspecies(ind) .NE. 1 .AND. waterdepth(c,r) .LE. 0.D0)) THEN !OR (non-shrimp sp AND cell is dry)
  IF (xworth(ind) .GE. minworth .AND. strandcount(ifwsp,c,r) .EQ. 100 & !if updatd worth not very small AND 100 indivs of given sp stranded in same cell
	.AND. (c .LE. 5 .OR. c .GE. cols-4 .OR. r .LE. 5 .OR. r .GE. rows-4)) THEN ! AND indiv within 5 cells of sub-grid edge
   maskposn = INT(pran(ithread) * habcount(1)) + 1    !choose random mask position associated with 'deep water' habitat (type 1)
   IF (maskposn .GT. habcount(1)) maskposn = habcount(1) !if 'ran11(idum)' exactly 1, set 'maskposn' to max mask posn
   i_col(ind) = habmask(1,1,maskposn)                 !move indiv to randomly selected cell with deep water habitat (type 1)
   j_row(ind) = habmask(1,2,maskposn)
   rescuecount = rescuecount + 1                      !increment by one count of number of model indivs moved to avoid edge effects
  ELSE                                                !otherwise, updated worth very small OR <100 sp-specific strandings OR indiv not within 5 cells of sub-grid edge
   mortloss(ifwsp,3) = mortloss(ifwsp,3) + xworth(ind)!add change in worth to daily running tot of worth lost to stranding
   cummortloss(3) = cummortloss(3) + xworth(ind)      !add change in worth to annual running tot of worth lost to stranding
   IF (xsensce(ind) .EQ. 0) THEN 
    netproductivity(ifwsp,c,r) = netproductivity(ifwsp,c,r) - (xworth(ind) * xtotwght(ind)) !if individula is not senescent, subtract stranded biomass from annual running total (g wwt)
    UPDATE_STRANDING_TROPHIC_TRANSFER_NOT_SENESCENT(ind)
   ELSE 
    UPDATE_STRANDING_TROPHIC_TRANSFER_SENESCENT(ind)
   END IF
   IF (xworth(ind) .GE. minworth) strandcount(ifwsp,c,r) = strandcount(ifwsp,c,r) + 1 !if updated worth not ver small, add to count of indivs lost to stranding (by sp)
   xalive(ind) = 4                                    !remove indiv from population (0=unassigned, 1=alive, 2=dead(nat mort incl. predation), 3=dead(starved), 4=dead(stranded), 5=dead(old))
  END IF !worth, strand count in edge cells
 END IF !worth, hours dry, species, water level
END IF

CALL cell_unlock(c,r)
CALL species_unlock(ifwsp)

IF (xalive(ind) .EQ. 2 .OR. xalive(ind) .EQ. 4) THEN  !if indiv dead b/c of nat mort (incl. predation; 2) or stranding (4)
 IF (fileflag(11) .EQ. 1 .AND. (ifwsp .EQ. isspeciesout .OR. isspeciesout .EQ. 0)) THEN !if output desired AND (want specific species OR all sp)
  outflag = 12; CALL output(0,0,0,0,0,0,0)                           !output attributes of dead indiv (12=file 120)
 END IF !output desired (dead indivs)
 xworth(ind)=0.D0; xsomchng(ind)=0.D0; xgonchng(ind)=0.D0; xtotchng(ind)=0.D0; xlenchng(ind)=0.D0; xspawnloss(ind)=0.D0 !zero worth !som, gon, and total wght changes (g wwt)!length change (mm)
 xworthchng(ind)=0.D0;
ENDIF
END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE spawnorder(i) !on first hr of first day of spawn season, create list of array posns
USE time_etc             !that identify indivs by their lengths in order from smallest to
USE species              !largest. E.g., if lengths are 4.6, 0.5 and 0.9, list array = 2, 3, 1.
USE indivattributes      !Create triangular distribn of spawners by looping through indivs and
USE reproduction         !assigning ea a spawn day via the inverse cumulative distribution funct.
USE nr                   !numerical recipe
IMPLICIT NONE
INTEGER lowr, uppr       !local shortand for min and max spawn days
INTEGER mode             !mode of spawn days
INTEGER order(totindivs) !order of lengths from smallest to largest
INTEGER indcnt           !number of indivs that have been assigned to day in spawn season
INTEGER posn             !local index variable
REAL(8) inflect          !inflection point of the inverse cumulative distribution funct of a triangular distribn
REAL(8) cdf              !cumulative distribution function of a triangular distribn
INTEGER i

 order = 0               !zero list of array positions
 CALL indexx_sp(spwnlen(i,:),order)!create list of array positions that identify lengths in order from smallest to largest

 lowr = minspwnday(i)    !store maximum spawn day as local shorthand
 uppr = maxspwnday(i)    !store minimum spawn day as local shorthand
 mode = INT((uppr - lowr + 1) / 2.D0) !calc mode of spawn days
 inflect = REAL(mode - lowr) / REAL(uppr - lowr) !calc inflection point of the inverse cum dist funct

 indcnt = 1              !initialize count of individuals assigned to spawn day
 DO posn = totindivs, totindivs-spwncnt(i)+1, -1 !loop over spawners in reverse order (i.e., starting w largest). 'spwncnt'+1 b/c first is 'totindivs'
  cdf = (1.D0 / spwncnt(i)) * (indcnt - 0.5D0)
  IF (cdf .LT. inflect) xspwnday(spwnid(i,order(posn))) = INT(lowr + SQRT(cdf * (uppr - lowr) * (mode - lowr))) !if ascending arm of triangular dist, assign spawn day based on length (rank)
  IF (cdf .GE. inflect) xspwnday(spwnid(i,order(posn))) = INT(uppr - SQRT((1.D0 - cdf) * (uppr - lowr) * (uppr - mode))) !if descending arm of triangular dist, assign spawn day based on length (rank)
  indcnt = indcnt + 1   !increment count of indivs assigned a spawn day
 END DO !spawners

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE updatecohort !each hour, for each early life stage (eggs, yolksac larvae, feeding
USE time_etc            !larvae), apply natural mortality and keep a running total of abundances
USE species             !by species. Add to running total of fractional temperature. If this
USE reproduction        !total >= 1, move cohort to next stage (eggs->yolk larvae->feeding
IMPLICIT NONE           !larvae->yoy), update stage totals and flags.
INTEGER next   !next early development stage (allows shrimp to skip yolk stage)
INTEGER xx, yy, zz

DO xx = 1, jday                               !loop over days so far (b/c keeping track of cohorts from diff days)
 DO yy = 1, nfwsp                             !loop over food web species
  DO zz = 3, 1, -1                            !loop over cohorts (i.e., early development stages egg, yolk, feeding) in reverse order

   IF (cohortflag(zz,xx,yy) .EQ. 1) THEN      !if cohort zz of sp yy from day xx present,
	cohortabund(zz,xx,yy) = cohortabund(zz,xx,yy) * EXP(-earlymort(zz,yy)) !apply nat mort
	cohorttot(zz,yy) = cohorttot(zz,yy) + cohortabund(zz,xx,yy)        !add to hour's running total across cohorts by stage and sp
	cohortftemp(zz,xx,yy) = cohortftemp(zz,xx,yy) + 1.D0 / (fracta(yy,zz) * EXP(-fractc(yy,zz) * wtemp)) !increase fract temp

	IF (cohortftemp(zz,xx,yy) .GE. 1.D0) THEN !if fractional temp of cohort zz of sp yy from day xx >=1,
	 IF (zz .EQ. 3) THEN                      !if latest early development stage (feeding larv)
	  newyoy(yy) = newyoy(yy) + cohortabund(zz,xx,yy) !move cohort to yoy (recruits) array, where it experiences no mort until becomes indiv worth start of next day
	  newyoyflag(yy) = 1                      !flag cohort of recruits present for sp yy
	  lftblin(zz+1,yy) = lftblin(zz+1,yy) + cohortabund(zz,xx,yy) !add to annual running total of number of new recruits (YOY) by sp (not subject to nat mort)
	 ELSE                                     !otherwise, yolk or egg
	  next = zz+1                             !calc next stage (yolk+1=feeding, egg+1=yolk)
	  IF (speciesid(yy) .EQ. 1) next = zz+2   !if shrimp, which do not have yolk stage, 'next' = egg+2 = feeding
	  cohortabund(next,jday,yy) = cohortabund(zz,xx,yy) !move cohort to next early life stage
	  lftblin(next,yy) = lftblin(next,yy) + cohortabund(zz,xx,yy) !add to annual running total of number entering stage (not subject to nat mort)
	  cohorttot(next,yy) = cohorttot(next,yy) + cohortabund(zz,xx,yy) !add to hourly running total across cohorts by stage and sp (subject to nat mort)
	  cohortflag(next,jday,yy) = 1            !flag cohort of stage zz present for day and sp
	 END IF !cohort
	 cohorttot(zz,yy) = cohorttot(zz,yy) - cohortabund(zz,xx,yy) !decrement hour's running total across cohorts by stage and sp
	 IF (cohorttot(zz,yy) .LT. 0.D0) cohorttot(zz,yy) = 0.D0 !if negative, set to 0
	 lftbldurn(zz,yy) = lftbldurn(zz,yy) + (cohortabund(zz,xx,yy) * (jday - xx)) !add to sum of stage durations weighted by cohort abund (aved at end of yr)
	 lftblout(zz,yy) = lftblout(zz,yy) + cohortabund(zz,xx,yy) !add to running tot of number leaving stage
	 lftbloutflag(zz,yy) = 1
	 cohortabund(zz,xx,yy) = 0.D0             !zero out cohort
	 cohortflag(zz,xx,yy) = 0                 !turn off 'cohort present' flag
	END IF !fractional temp
   END IF !cohort present
  END DO !early development stages
 END DO !days so far
END DO !food web sp

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE maturity    !at start of day (first hr), update maturity status
USE time_etc           !based on length threshold (mm). If indiv matured
USE species            !inside of spawn season, set spawn day to current jday
USE indivattributes    !(otherwise spawn day assigned at start of next spawn
USE reproduction       !season based on indiv's length)
IMPLICIT NONE

IF (xmat(indiv) .EQ. 0 .AND. xlen(indiv) .GE. matlen(fwsp)) THEN !if indiv immature AND length above length threshold (mm)
 xmat(indiv) = 1                      !change status to mature (0=immature, 1=mature)
 xstage(indiv) = 3                    !update stage (1=YOY, 2=juvenile, 3=adult)
 IF (jday .GE. minspwnday(fwsp) .AND. jday .LE. maxspwnday(fwsp) .AND. xspwnday(indiv) .EQ. 0) xspwnday(indiv) = jday !if indiv matured during spawn season and spawn day not assigned, set spawn day to jday
END IF !immature

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE fecundity !at start of ea spawn season (as determined by jday), or
USE time_etc         !if indiv matures during spawn season, use indiv tot wght
USE indivattributes  !(g wwt) to calc sp-specific fecundity and batch wght,
USE reproduction     !and flag that calcs were made for season.
USE species
IMPLICIT NONE
INTEGER sp           !local shorthand for 'xspecies(indiv)'

sp = xspecies(indiv) !store 'xspecies(indiv)' as local shorthand

IF (xmat(indiv) .EQ. 1) THEN   !if mature
 IF (xspwnday(indiv) .LE. 0) THEN
   notcalculatedspawningdays(fwsp) = notcalculatedspawningdays(fwsp) +1
 END IF

 IF(sp .EQ. 2 .AND. jday .EQ. minspwnday(fwsp)+353) THEN !if IS and 7 days before start of spawn season
  xreprdinvst(indiv) = 0.D0                              !reset seasonal running tot of g wwt invested in reprod (g wwt)
  xbreedsom(indiv) = xsomwght(indiv)                     !if IS, which begins invest 7 days before spawn season, establish initial som wght that determines reprod invest (IS is 7 days before spawn season, capital is on spawn end day) (g wwt)
  xtrgtinvst(indiv) = 1.18D0 * (1.D0 - EXP(REAL(-ndays * nhours) * xmort(indiv))) !calc target annual reprod invest (g wwt invested as a prop'n of g wwt somatic wght)
 END IF !sp, jday
 IF (xfecflag(indiv) .EQ. 0 .AND. jday .EQ. xspwnday(indiv)) THEN !if fecundity not yet calculated AND jday = spawn day
  IF (breedtype(fwsp) .EQ. 0) THEN                       !if income spawner (which begins invest on spawn day)
   xbreedsom(indiv) = xsomwght(indiv)                    !establish initial som wght that determines reprod invest (IS is 7 days before spawn season, capital is on spawn end day) (g wwt)
   xtrgtinvst(indiv) = 1.18D0 * (1.D0 - EXP(REAL(-ndays * nhours) * xmort(indiv))) !calc target annual reprod invest (g wwt invested as a prop'n of g wwt somatic wght)
  END IF
  IF (sp .EQ. 1) xfec(indiv) = feca(fwsp) + fecb(fwsp) * xtotwght(indiv) ** fecc(fwsp) !if shrimp (GS)
  IF (sp .EQ. 2) xfec(indiv) = feca(fwsp) * xtotwght(indiv) ** fecb(fwsp)   !if silverside (IS)
  IF (sp .EQ. 3) xfec(indiv) = feca(fwsp) * LOG(xtotwght(indiv)) + fecb(fwsp)!if killifish (GK)
  IF (sp .EQ. 4) xfec(indiv) = (feca(fwsp) + fecb(fwsp) * xtotwght(indiv)) * REAL(batches(fwsp)) !if anchovy (BA) (multiply by number of batches b/c Zastrow et al. 1991 parameters are for single batch)
  xbatchwght(indiv) = (xfec(indiv) / REAL(batches(fwsp))) * eggwght(fwsp)   !calc mass of single egg batch (g wwt)
  xfecflag(indiv) = 1                                                       !flag that fecundity was calculated
 END IF !fecundity flag, jday
END IF !maturity

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE spawn     !at start of day (first hr), if in wet cell, indiv fecundity known,
USE time_etc         !batches left to spawn, and enough gonad for a batch, add one batch of
USE indivattributes  !eggs (* worth) to day's cohort, adjust flags, counter, indiv weights,
USE reproduction     !etc. If no more batches or last day of spawn season, and if capital
IMPLICIT NONE        !breeder, transfer gonad to soma and zero out flags, counters, etc.
REAL(8) eggs   !number of eggs produced by indiv
xspawnloss(indiv)=0.D0
IF (xfecflag(indiv) .EQ. 1) THEN                       !if fecundity flagged (i.e., can spawn b/c in season and batches to spawn)
 IF (xgonwght(indiv) .GE. xbatchwght(indiv)) THEN      !if enough gonad wght to convert to one egg batch (g wwt)
  eggs = (xbatchwght(indiv) / eggwght(fwsp)) * xworth(indiv) !calc egg production
  cohortabund(1,jday,fwsp) = cohortabund(1,jday,fwsp) + eggs !keep running total of eggs produced by food web sp and day
  cohorttot(1,fwsp) = cohorttot(1,fwsp) + eggs         !add to running total, across all cohorts and days, of eggs of given food web sp
  lftblin(1,fwsp) = lftblin(1,fwsp) + eggs             !add to annual running tot of eggs produced by sp (for life table - not subject to hourly nat mort)
  cohortflag(1,jday,fwsp) = 1                          !flag that eggs present for day and sp
  dayeggprdn(fwsp) = dayeggprdn(fwsp) + eggs           !add to daily running tot of eggs produced by sp (for output file 44 - not subject to hourly nat mort)
  xeggprdn(indiv) = xeggprdn(indiv) + eggs             !add to seasonal running total of indiv egg production
  xbtchcnt(indiv) = xbtchcnt(indiv) + 1                !add to seasonal running total of times (batches) spawned
  xgonwght(indiv) = xgonwght(indiv) - xbatchwght(indiv)!decrement gonad wght by mass of single egg batch (g wwt)
  xgonchng(indiv) = xgonchng(indiv) - xbatchwght(indiv)!update gonad wght change (g wwt)
  IF (xbatchwght(indiv) .LT. 0) THEN
    PRINT *, "Individual ",indiv, " of species ",xspecies(indiv), " has negative batch weight: ", xbatchwght(indiv), ". The total spawn loss for this indivdual is: ", xspawnloss(indiv)
  END IF
  ! Trace batch weight and spawning loss for traced individuals (output)
  xspawnloss(indiv) = xbatchwght(indiv)
  speciesspawnloss(xspecies(indiv))=speciesspawnloss(xspecies(indiv)) + xbatchwght(indiv)
  !DEC$ IF DEFINED(TRACE_INDIVIDUALS)
  CALL registerindividual(indiv)
  IF(isselectedindividual(indiv) .EQ. 1) THEN
    WRITE (310,*) "Register individual: ", indiv, " of species: ", xspecies(indiv), " on year: ", year, " day: ", jday, " and hour: ", hour, " with batch weight: ", xbatchwght(indiv), " and spawn loss: ", xspawnloss(indiv)
    IF (fileflag(26) .EQ. 1) THEN
        outflag=27; CALL output(0,0,0,0,0,0,0)
    END IF
  END IF
  
  !DEC$ ENDIF
  
  xtotwght(indiv) = xsomwght(indiv) + xgonwght(indiv)  !calculate new total wght (g wwt)
  xtotchng(indiv) = xsomchng(indiv) + xgonchng(indiv)  !update total wght change (g wwt)
  IF (breedtype(fwsp) .EQ. 1 .AND. xageyrs(indiv) .EQ. ageclasses(fwsp) & !if capital spawner AND in last year of life
	.AND. xgonwght(indiv) .LT. xbatchwght(indiv)) xsensce(indiv) = 1      !AND not enough gonad for a batch, trigger senescence (indiv can no longer spawn)
 END IF
 IF (xbtchcnt(indiv) .GE. batches(fwsp) .OR. jday+1 .GE. maxspwnday(fwsp)) THEN !if all batches spawned OR last day of spawn season
  xsomwght(indiv) = xsomwght(indiv) + xgonwght(indiv)  !convert gonad wght to somatic wght (g wwt)
  xsomchng(indiv) = xsomchng(indiv) + xgonwght(indiv)  !update somatic wght change (g wwt)
  xgonchng(indiv) = xgonchng(indiv) - xgonwght(indiv)  !update gonad wght change (g wwt)
  xgonwght(indiv) = 0.D0                               !zero gonad wght (g wwt)
  xfecflag(indiv) = 0                                  !unflag fecundity for all breed types (can no longer spawn b/c out of season or batches)
  xreprdinvst(indiv) = 0.D0                            !reset seasonal running tot of g wwt invested in reprod (g wwt)
  IF (xageyrs(indiv) .EQ. ageclasses(fwsp)) xsensce(indiv) = 1 !if indiv in last year of life, trigger senescence now that spawning is over
 END IF
END IF !fecundity flagged, spawn season, batches left to spawn

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE forcecons !in last feeding hr, if cell deep enough, by-pass funct
USE time_etc         !response by setting realized cons (g prey / g wwtindiv / day)
USE species          !to 0.5 to 1.0 times max cons (g prey / g wwtindiv / day).
USE indivattributes  !Fix at 1.0 (i.e., maintenance cons) if indiv scenescent
USE bioenergetics    !(finished spawning and max age). So that get realistic ener
USE movement         !changes, distrib forced realized consump evenly among vuln
USE reproduction     !prey tpes.
IMPLICIT NONE
REAL(8) p         !local shorthand for daily and hourly proportion of max daily consump actually consumed
REAL(8) cons      !total prey consumed (g prey/g wwt indiv/day)
INTEGER vulncount !number of prey types vulnerable to predator
INTEGER xx

IF (waterdepth(i_col(indiv),j_row(indiv)) .GE. xmindep(indiv)) THEN !if current cell deep enough
 vulncount = 0         !zero out counter for ea indiv
 IF (xsensce(indiv) .EQ. 1) THEN      !if indiv senescent (i.e., max age AND gonads too small, max reprod invest, all batches spawned, end of spawn season)
  p = 1.D0                            !if indiv is senescent (in last year and no longer spawning), allow maintenance cons
 ELSE                                 !otherwise, indiv is not senescent
  p = pvalue(spposn(xspecies(indiv))) !store sp-specific proportion of max daily consumption as local shorthand
 END IF
 cons = xcmax(indiv) * p   !calc daily consumption as proporn of daily max consumption (g prey/g wwt indiv/day)
 totcons(indiv) = cons     !set daily running cons total to 'p' (g wwt / g wwt of indiv)
 DO xx = 1, preytypes  !loop over all potential prey (1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)
  IF (preyv(fwsp,xx) .EQ. 1) vulncount = vulncount + 1 !if prey type vulnerable, add to running tot of vuln prey types
 END DO
 DO xx = 1, preytypes  !loop over all potential prey (1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)
  IF (preyv(fwsp,xx) .EQ. 1) realcons(indiv,xx) = cons / REAL(vulncount) !if prey type vulnerable, assign 1/n'th of cons to prey-specific total
 END DO
END IF !current cell wet

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE freecons(ii,jj,ind,ifwsp) !each hour, if current cell deep enough, calc cons (g wwt / g wwt indiv) of
USE indivattributes                  !ea prey type based on a type II funct response. Consumption cannot exceed20% in any hour.
USE movement                         !20% in any hour
USE reproduction
IMPLICIT NONE
REAL(8) denom       !denominator of the functional response (unitless, same for all prey types)
REAL(8) numer       !numerator of the functional response (unitless, prey type-specific)
REAL(8) cons        !prey-specific consumption in a square m (benth) or cubic m (all other prey) (g wwt / g wwt indiv)
REAL(8) unitcons    !'cons' expressed as g wwt prey/m**2 for benth, g wwt/m**3 for all other prey
INTEGER c, r        !local shorthand for "i_col(indiv)" and "j_row(indiv)", respectively
INTEGER ii, jj
INTEGER xx          !local loop index
INTEGER ind, ifwsp  !local 'indiv' and 'ifwsp' variables

denom=0.D0 !zero denominator of the functional response

 IF (fitsearch .EQ. 0) THEN !if subroutine called from MAIN (for legitimate cons),
  c=i_col(ind); r=j_row(ind) !store cell coord as local shorthands
 ELSE !otherwise, if subroutine called from subroutine 'fitnhood' (for expected cons),
  c=i_col(ind)+ii; r=j_row(ind)+jj !store 'nhood' cell coord as local shorthands
 END IF !fitsearch

 IF (waterdepth(c,r) .GE. xmindep(ind) .AND. (totcons(ind) .LT. xcmax(ind) .OR. fitsearch .EQ. 1)) THEN !if current/candidate cell deep enough AND (daily cmax not realized OR fit search)
  DO xx = 1, preytypes !loop over all potential prey (1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)
   IF (preyv(ifwsp,xx) .EQ. 1) THEN !if prey type vulnerable
	denom = denom + ((unitpreybmss(xx,c,r) * preyv(ifwsp,xx)) / preyk(ifwsp,xx)) !calc denominator of funct resp (unitless)
   END IF !vulnerable
  END DO !preytypes (for denom)
  DO xx = 1, preytypes !loop over all potential prey (1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)
   IF (preyv(ifwsp,xx) .EQ. 1) THEN  !if prey type vuln to indiv of that species
	numer = unitpreybmss(xx,c,r) * preyv(ifwsp,xx) / preyk(ifwsp,xx) !prey-specific numerator of the funct resp (unitless)
	cons = (xcmax(ind) * numer * 0.2D0) / (denom + 1.D0)  !prey-specific cons (up to 20% of cmax) in square m (benth) or cubic m (zoop and other indivs) (g prey / g wwt indiv)
	IF (xx .LE. 3) unitcons = cons * xtotwght(ind) * xworth(ind) / cellsize**2   !if benth, convert cons to g wwt/m**2
	IF (xx .GE. 4) unitcons = cons * xtotwght(ind) * xworth(ind) / watervol(c,r) !if zoop, convert cons to g wwt/m**3
	IF (unitpreybmss(xx,c,r)-unitcons .LT. 0.D0) THEN !if prey type over-consumed (g wwt/m**2 or 3)
	 unitcons = unitpreybmss(xx,c,r) - minpreyden !if benth or zoop, set unit cons to unit avail minus density of prey that pred cannot consume (g wwt/m**2 or 3)
	 IF (unitcons .LT. 0.D0) unitcons = 0.D0 !if negative result, force to zero
	 IF (xx .LE. 3) cons = (unitcons * cellsize**2) / (xtotwght(ind) * xworth(ind)) !if benth, convert unit cons (g/mm**2) back to cons (g prey/ g wwt indiv)
	 IF (xx .GE. 4) cons = (unitcons * watervol(c,r)) / (xtotwght(ind) * xworth(ind)) !if zoop, convert unit cons (g/m**3) back to cons (g prey/ g wwt indiv)
	END IF !over-consump of prey type
	IF (fitsearch .EQ. 1) expcons(xx) = cons !if evaluating fitness nhood (i.e., not actually consuming) store prey-specific cons, even if close to or above cmax (g prey / g wwt indiv)
	IF (fitsearch .EQ. 0) CALL conspreyupdate(c,r,xx,ind,cons) !if actually consuming, pass necessary variables to subroutine to compare cons to cmax and update consumed indiv or prey
   END IF !prey are vulnerable
  END DO !prey field
 END IF !cell wet and cmax not yet realized

 IF (hour .EQ. maxfeedhr .AND. fitsearch .EQ. 0) agep(spposn(xspecies(ind)),xageyrs(ind)) = & !if last hour that indiv can feed AND really consuming add to running tot of p (propn of cmax)
   agep(spposn(xspecies(ind)),xageyrs(ind)) + ((totcons(ind) / xcmax(ind)) * xworth(ind))
END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE conspreyupdate(c,r,xx,ind,cons)
USE indivattributes !Use 'cons' to update base prey and indiv prey. First reduce cons if total
USE reproduction    !cons > cmax or cell availability. Then update benth and zoop cell
					!densities by subtracting consumed from present in cell.
IMPLICIT NONE
REAL(8) cons        !prey-specific consumption in a square m (benth) or cubic m (all other prey) (g wwt / g wwt indiv) (FROM 'FREECONS')
REAL(8) temptotcons !temporary variable for storing running total of all prey consumed (g wwt)
REAL(8) unitcons    !'cons' expressed as g wwt prey/m**2 for benth, g wwt/m**3 for all other prey
INTEGER c, r        !local shorthand for "i_col(indiv)" and "j_row(indiv)", respectively (from 'freecons')
INTEGER xx          !loop loop index (from 'freecons')
INTEGER ind         !local 'indiv' (from 'freecons')
INTEGER z           !local index for error do loop

 temptotcons = totcons(ind) + cons            !temporarily store running total of consump of all prey types (g prey / g wwt indiv)
 IF (temptotcons .GE. xcmax(ind)) THEN        !if temporary running tot of cons >= cmax (g prey / g wwt indiv)
  cons = xcmax(ind) - totcons(ind)            !reduce cons for hour to available cons (g prey / g wwt indiv)
  realcons(ind,xx) = realcons(ind,xx) + cons  !add new 'cons' to prey-specific, daily running tot of realized cons (g prey / g wwt indiv)
  totcons(ind) = xcmax(ind)                   !set running tot for day to cons allowed by cmax (g prey / g wwt indiv)
 ELSE                                         !otherwise, consumption < than cmax so cons unchanged
  realcons(ind,xx) = realcons(ind,xx) + cons  !add orig cons to prey-specific, daily running tot of realized cons (g prey / g wwt indiv)
  totcons(ind) = SUM(realcons(ind,:))         !calc running tot of all prey consumed so far that day (g prey / g wwt indiv)
 END IF !cons vs cmax

 dietbase(ind,xx) = dietbase(ind,xx) + (cons * xtotwght(ind) * xworth(ind)) !add prey cons to indiv's diet (g wwt)
 avediet(xspecies(ind),xx) = avediet(xspecies(ind),xx) + (cons * xtotwght(ind) * xworth(ind)) !add prey cons to sp- and prey-specific diet (running tot of g wwt)
 IF (xx .LE. 3) unitcons = cons * xtotwght(ind) * xworth(ind) / cellsize**2   !if benth, convert cons to g wwt/m**2
 IF (xx .GE. 4) unitcons = cons * xtotwght(ind) * xworth(ind) / watervol(c,r) !if zoop or other indivs, convert cons to g wwt/m**3
 unitpreybmss(xx,c,r) = unitpreybmss(xx,c,r) - unitcons !subtract cons (g/m2 or 3) from biomass of prey in one square or cubic m (g/m**2 or 3)
 unitpreynum(xx,c,r) = unitpreybmss(xx,c,r) / indpreywght(xx) !convert biomass per unit to number per unit
 IF (unitpreybmss(xx,c,r) .LT. 0.D0) THEN    !check that unit prey bmass did not go negative (g wwt/m**2 or 3)
  DO z = 1, 25
   CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
  END DO
  PRINT *, "Error 2 in 'conspreyupdate': over-cons of prey type", xx
  PRINT *, "by individual", ind
  PRINT *, "in column and row", c, r
  PRINT *, "on year, day, hour", year, jday, hour
  PRINT *, "Simulation stopped."
  CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
  STOP
 END IF

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE respiration(ind,ifwsp)  !at end of day (last hr), use indiv wght (g wwt), temperature,
USE time_etc            !and sp-specific parameters to calc respiration (g O2 / g wwt
USE physicalenv         !of indiv/day)
USE indivattributes
USE bioenergetics
USE movement
IMPLICIT NONE
REAL(8) tempeffect                 !a temperature-based scalar of resp (+ve curve, ranges from 0 to 1)
REAL(8) ra, rb, rtm, rto, rq       !local shorthand for respscalar, respexpo, respmaxt, respoptt, resptheta
REAL(8) part1, part2, part3, part4 !equation parts for calc of 'tempeffect'
INTEGER ind, ifwsp

resp = 0.D0           !zero out 'resp' for ea indiv (g O2 / g wwt of indiv/ day)

ra = respscalar(ifwsp)                                        !store respscalar as local shorthand
rb = respexpo(ifwsp)                                          !store respexpo as local shorthand
rtm = respmaxt(ifwsp)                                         !store respmaxt as local shorthand
rto = respoptt(ifwsp)                                         !store respoppt as local shorthand
rq = resptheta(ifwsp)                                         !store resptheta as local shorthand
part1 = (rtm - wtemp) / (rtm - rto)                          !calc parts 1-4 of temp effect
IF (part1 .LE. 0.D0) part1 = 0.D0                            !if warmer than max, set to 0
part2 = LOG(rq) * (rtm - rto)
part3 = LOG(rq) * (rtm - rto + 2.0)
part4 = (part2 ** 2.0 * (1.0 + (1.0 + 40.0 / part3) ** 0.5) ** 2.0) / 400.0
tempeffect = part1 ** part4 * EXP(part4 * (1.0 - part1))     !calculate temp effect (0 to 1: 0 at low temps,
															  !1 at max temp, otherwise increasing curve)
resp = (ra * xtotwght(ind) ** rb) * tempeffect * act(ifwsp) !calc resp for tot wght (i.e., incl gonads) (g O2/g wwt/day)

IF (fitsearch .EQ. 1) resp = resp / 24.D0                    !if called from subroutine 'fitnhood', convert to hourly resp

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE energychange(ind,ifwsp) !at end of day, for each potential prey, calc energy loss to egestion,
USE time_etc            !excretion, and specific dynamic action. Subract these losses from
USE indivattributes     !realized consumption. Then use prey energy density and indiv wght to
USE bioenergetics       !convert grams consumed per g wwt indiv to Joules consumed. Finally,
USE movement            !subtract respiration scaled to indiv wght to convert J consumed to a
USE reproduction        !change in predator energy (J)
IMPLICIT NONE
REAL(8) spspcons    !local shorthand for sp-specific cons, expected or realized (g prey / g wwt indiv)
REAL(8) egestion    !prey wght lost to egestion (fecal waste) that day (g prey / g wwt indiv / day)
REAL(8) excretion   !prey wght lost to excretion (nitrogenous waste) that day (g prey / g wwt indiv / day)
REAL(8) sda         !prey wght lost to sda (digestion) that day (g prey / g wwt of indiv)
REAL(8) loss        !total prey wght lost for day (g prey / g wwt indiv / day)
REAL(8) totpreyener !total prey energy summed across all prey types (Joules; J/g * g * g/g = J)
INTEGER xx
INTEGER ind, ifwsp

 totpreyener = 0.D0        !zero out running total for ea indiv (Joules)
 DO xx = 1, preytypes !loop over all potential prey (1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)
  IF (preyv(ifwsp,xx) .EQ. 1) THEN !if prey type vulnerable
   IF (xsensce(ind) .EQ. 1) THEN !if indiv senescent (i.e., max age AND gonads too small, max reprod invest, all batches spawned, end of spawn season)
	IF (fitsearch .EQ. 0) THEN !if actually consuming,
	 IF (totcons(ind) .GT. 0.D0) preypropn(ind,xx) = realcons(ind,xx) / totcons(ind) !if consumed, calc contribn of prey type xx to 'totcons' (proportion)
	 IF (totcons(ind) .LE. 0.D0) preypropn(ind,xx) = 0.D0 !if did not consume, set contribn of prey type xx to zero (needed for estim of maint cons cmax during senescence)
	END IF !fitsearch
   END IF !batches and jday
   IF (fitsearch .EQ. 0) spspcons = realcons(ind,xx) !if actually consuming, store realized cons as local shorthand (g prey / g wwt indiv)
   IF (fitsearch .EQ. 1) spspcons = expcons(xx) !if evaluating fitness 'nhood', store expected cons as local shorthand (g prey / g wwt indiv)
   IF (speciesid(ifwsp) .NE. 4) egestion = egscalar(ifwsp) * spspcons !if not anchovy (BA), use sp-specific cons to calc that day's egestion (g prey / g wwt of indiv / day)
   IF (speciesid(ifwsp) .EQ. 4) egestion = egscalar(ifwsp) * spspcons * (wtemp ** egexpo(ifwsp)) !if anchovy (BA), use sp-specific cons to calc temp-dep egestion (g prey / g wwt of indiv / day)
   IF (fitsearch .EQ. 1) egestion = egestion / 24.D0 !if egestion calc part of fitness search, convert daily egestion to daily (g prey / g wwt of indiv / hour)
   excretion = exscalar(ifwsp) * (spspcons - egestion) !exretion (g prey / g wwt of indiv / day) (/ hour if fitness search)
   sda = sdascalar(ifwsp) * (spspcons - egestion) !sda (g prey / g wwt of indiv / day) (/ hour if fitness search)
   loss = egestion + excretion + sda !calc day's total loss (excluding respiration; g prey / g wwt indiv / day) (/ hour if fitness search)
   totpreyener = totpreyener + preyenerden(xx) * xtotwght(ind) * (spspcons - loss) !sum energy (Joules) across all prey types (J/g * g * g/g = J)
   IF (fitsearch .EQ. 1) expcons(xx) = 0.D0 !if evaluating fitness 'nhood', zero expected cons (g prey / g wwt indiv)
  END IF !prey type vulnerable
 END DO !prey field
 enerchng = totpreyener - (calorcoeff * xtotwght(ind) * resp) !convert prey energy to pred energy [(J - J/O2 * g * O2/g) = J]

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE weightchange(ifwsp) !at end of day (last hr), convert ener change (J) to wght change (g wwt)
USE time_etc            !then add or subtract wght as needed. If immature, all wght change through
USE indivattributes     !somatic tissue. If mature and wght loss, deficit is met first via somatic
USE bioenergetics       !tissue (if protecting gonad and batches left) or gonad (if protecting somatic
USE reproduction        !tissue). For former, indiv uses gonad to avoid starvation. If mature and wght
USE species
USE movement
IMPLICIT NONE           !gain, add to gonad wght to a maximum determined by target reprod invest
						!(g wwt; a function of adult M). If target reached or all batches spawned,
						!add to somatic tissue. After wght change allocated, determine if indiv
						!starved or length increased. All calc assume that som and gon tissues have same energy density
REAL(8) wghtchng     !positive or negative change in wght as a result of change in energy (g wwt)
REAL(8) expsomwght   !expected som wght (g wwt) given length (mm)
REAL(8) actualinvest !actual seasonal invest in reprod, expressed as prop'n of somatic g wwt
REAL(8) maxinvest    !maximum g wwt that indiv can invest in reprod w/o exceeding 'xtrgtinvst' (g wwt)
REAL(8) oldlength    !stores length at start of day so that positive length changes can be calculated (mm)
REAL(8) length       !a FUNCTION that accepts wght (g wwt), sp-specific coeffs, and returns length (mm)
REAL(8) totchngbuffer
INTEGER z            !local index for error do loop
INTEGER zz
INTEGER c, r, ifwsp


c= i_col(indiv); r=j_row(indiv)
wghtchng = enerchng / predenerden(fwsp)                    !convert total energy change to total wght change (J / J/g = g wwt)
expsomwght = lw_scalar(fwsp) * xlen(indiv) ** lw_expo(fwsp)!calc expected som wght (g wwt) given current length (mm)
xstrvwght(indiv) = expsomwght / 2.D0                       !establish starvation threshold (g wwt)

IF (xmat(indiv) .EQ. 0) THEN                               !if indiv immature
 xsomwght(indiv) = xsomwght(indiv) + wghtchng              !apply wght change to somatic tissue (g wwt)
 xsomchng(indiv) = xsomchng(indiv) + wghtchng              !update somatic wght change (g wwt)
ELSE                                                       !otherwise, indiv mature
 IF (wghtchng .LT. 0.D0) THEN                              !if negative wght change (g wwt)
  IF (xfecflag(indiv) .EQ. 1) THEN !if can spawn (b/c in spawn season and batches left)
   xsomwght(indiv) = xsomwght(indiv) + wghtchng            !apply wght change to somatic tissue to protect gonad (g wwt)
   xsomchng(indiv) = xsomchng(indiv) + wghtchng            !update somatic wght change (g wwt)

   IF (xsomwght(indiv) .LT. xstrvwght(indiv)) THEN         !if new somatic wght < starvation wght (g wwt)
	IF (xgonwght(indiv) .GE. xstrvwght(indiv)-xsomwght(indiv)) THEN !if gonad wght > deficit (g wwt)
	 xgonwght(indiv) = xgonwght(indiv) - (xstrvwght(indiv) - xsomwght(indiv)) !transfer deficit to gonad wght to prevent starvation (g wwt)
	 xgonchng(indiv) = xsomwght(indiv) - xstrvwght(indiv)  !set change in gonad wght to deficit (g wwt)
	 xsomwght(indiv) = xsomwght(indiv) - xgonchng(indiv)   !increment somatic wght by deficit (g wwt)
	 xsomchng(indiv) = xsomchng(indiv) - xgonchng(indiv)   !increment somatic wght change by deficit (g wwt)
	ELSE                                                   !otherwise, gonad wght < deficit (g wwt)
	 xsomwght(indiv) = xsomwght(indiv) + xgonwght(indiv)   !transfer all of gonad wght to somatic wght (g wwt)
	 xsomchng(indiv) = xsomchng(indiv) + xgonwght(indiv)   !update somatic wght change (g wwt)
	 xgonchng(indiv) = xgonchng(indiv) - xgonwght(indiv)   !update gonad wght change (g wwt)
	 xgonwght(indiv) = 0.D0                                !zero gonad wght (g wwt)
	END IF !gonad wght vs deficit betw som and starve wghts
   END IF !som wght vs starve wght

  ELSE                                                     !otherwise, can't spawn (b/c out of spawn season or all batches spawned)
   IF (xgonwght(indiv) .GT. ABS(wghtchng)) THEN            !if gonad can accomodate all of negative wght change (g wwt)
	xgonwght(indiv) = xgonwght(indiv) + wghtchng           !decrement gonad wght by negative wght change (g wwt)
	xgonchng(indiv) = xgonchng(indiv) + wghtchng           !add negative wght change to gonad wght change (g wwt)
   ELSE                                                    !otherwise, gonad wght does not exceed negative wght change (g wwt)
	wghtchng = wghtchng + xgonwght(indiv)                  !decrement negative wght change by gonad wght, if any (g wwt)
	xgonchng(indiv) = xgonchng(indiv) - xgonwght(indiv)    !add existing gonad wght (if any) to change in gonad wght (g wwt)
	xgonwght(indiv) = 0.D0                                 !set (or re-set) gonad wght to zero (g wwt)
	xsomchng(indiv) = xsomchng(indiv) + wghtchng           !add remainder of negative wght change to change in somatic wght (g wwt)
	xsomwght(indiv) = xsomwght(indiv) + wghtchng           !decrement somatic wght by remainder of wght change (g wwt)

	IF (xsomwght(indiv) .LT. 0.D0) THEN                    !if new somatic wght negative (g wwt)
	 xsomchng(indiv) = xsomchng(indiv) + xsomwght(indiv)   !add deficit to change in somatic wght (g wwt)
	 xsomwght(indiv) = 0.D0                                !set somatic wght to zero (g wwt)
	END IF

   END IF !gonad wght vs. negative wght change
  END IF !target investment and batch count

  IF (breedtype(fwsp) .EQ. 1 .AND. xfecflag(indiv) .EQ. 1 .AND. xageyrs(indiv) .EQ. ageclasses(fwsp) & !if capital spawner AND spawning AND in last year of life
	.AND. xgonwght(indiv) .LT. xbatchwght(indiv)) xsensce(indiv) = 1                                   !AND not enough gonad for a batch, trigger senescence (indiv can no longer spawn)

 ELSE                                                      !otherwise, positive wght chng (g wwt)
  actualinvest = 0.D0                                      !assume indiv has not invested in reprod (g wwt invested as a prop'n of som g wwt)
  IF (breedtype(fwsp) .NE. 0 .AND. xfecflag(indiv) .NE. 0) actualinvest = xreprdinvst(indiv) / xbreedsom(indiv) ! !if not income spawner that is out of season, calc actual reprod invest (g wwt invested as a prop'n of som g wwt)
  IF (breedtype(fwsp) .EQ. 1 .AND. xbtchcnt(indiv) .EQ. 1) actualinvest = xtrgtinvst(indiv) + 1.D0 !if capital spawner that has spawned once, force actual invest above target invest (capital breeders cannot invest in reprod once reprod has begun)

  IF (actualinvest .LT. xtrgtinvst(indiv) .AND. &                  !if target invest not realized AND either a
	((xfecflag(indiv) .EQ. 1 .AND. breedtype(fwsp) .EQ. 0) .OR. &  !spawning income breeder (in season and batches left to spawn)
	(xfecflag(indiv) .EQ. 0 .AND. xspecies(indiv) .EQ. 2 .AND. &   !OR non-spawning IS
	(jday .GE. minspwnday(fwsp)+353 .OR. jday .LT. maxspwnday(fwsp))))) THEN !that can invest in reprod (i.e., jday inside of week before spawn season and end of spawn season)
   maxinvest = (xtrgtinvst(indiv) * xbreedsom(indiv)) - xreprdinvst(indiv) !calc max amount that indiv can invest in reprod (g wwt)

   IF (maxinvest .LT. 0.D0) THEN
	DO z = 1, 25
	 CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
	END DO
	PRINT *, "Error 1 in 'weightchange', 'maxinvest' is negative:"
	PRINT *, "maxinvest:", maxinvest
	PRINT *, "xtrgtinvst:", xtrgtinvst(indiv)
	PRINT *, "xbreedsom:", xbreedsom(indiv)
	PRINT *, "xreprdinvst:", xreprdinvst(indiv)
	PRINT *, "Error year, day, indiv", year, jday, indiv
	PRINT *, "Simulation stopped."
	CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
	STOP
   END IF

   IF (maxinvest .GT. wghtchng) THEN                       !if indiv can invest all of positive wght change into reprod (g wwt)
	xgonwght(indiv) = xgonwght(indiv) + wghtchng           !invest all of positive wght change in gonad wght (g wwt)
	xgonchng(indiv) = xgonchng(indiv) + wghtchng           !update gonad wght change (g wwt)
	xreprdinvst(indiv) = xreprdinvst(indiv) + wghtchng     !add to seasonal running total of investment in reprod (g wwt)
   ELSE                                                    !otherwise, indiv can invest only part of positive wght change in reprod (g wwt)
	xgonwght(indiv) = xgonwght(indiv) + maxinvest          !invest all of 'maxinvest' in gonad wght (g wwt)
	xgonchng(indiv) = xgonchng(indiv) + maxinvest          !update gonad wght change (g wwt)
	xreprdinvst(indiv) = xreprdinvst(indiv) + maxinvest    !add to seasonal running total of investment in reprod (g wwt)
	wghtchng = wghtchng - maxinvest                        !decrement positive wght change by amount that went into gonad wght (g wwt)
	xsomwght(indiv) = xsomwght(indiv) + wghtchng           !invest remained of positive wght change in somatic wght (g wwt)
	xsomchng(indiv) = xsomchng(indiv) + wghtchng           !update somatic wght change (g wwt)
   END IF !allocation of positive wght change

  ELSE                                                     !otherwise, target investment already realized AND (spawning income OR non-spawning IS 7 d before spawn season)
   xsomwght(indiv) = xsomwght(indiv) + wghtchng            !apply all of positive wght change to somatic tissue (g wwt)
   xsomchng(indiv) = xsomchng(indiv) + wghtchng            !update somatic wght change (g wwt)
   IF (breedtype(fwsp) .EQ. 0 .AND. xageyrs(indiv) .EQ. ageclasses(fwsp) .AND. actualinvest .GE. xtrgtinvst(indiv) & !if income spawner AND in last year of life AND target invest realized
	 .AND. xgonwght(indiv) .LT. xbatchwght(indiv)) xsensce(indiv) = 1 !AND not enough gonad for a batch, trigger senescence (indiv can no longer spawn)

  END IF !target investment and batch count
 END IF !sign of wght change
END IF !maturity

IF (xgonwght(indiv) .LT. 0.D0 .OR. xsomwght(indiv) .LT. 0.D0) THEN
 DO z = 1, 25
  CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
 END DO
 PRINT *, "Error 2 in 'weightchange', weight is negative:"
 PRINT *, "xsomwght:", xsomwght(indiv)
 PRINT *, "xgonwght:", xgonwght(indiv)
 PRINT *, "Error year, day, indiv", year, jday, indiv
 PRINT *, "Simulation stopped."
 CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
 STOP
END IF

totchngbuffer = xtotchng(indiv)+xspawnloss(indiv)
xtotwght(indiv) = xsomwght(indiv) + xgonwght(indiv)     !update total wght (g wwt)
xtotchng(indiv) = xsomchng(indiv) + xgonchng(indiv)     !update total wght change (g wwt)
xavgwghtdiff(indiv)=xavgwghtdiff(indiv)*REAL((xagedays(indiv)-1)/xagedays(indiv))+(xtotwght(indiv)-xprevwght(indiv))/REAL(xagedays(indiv))
xavgwghtandworthdiff(indiv)=xavgwghtandworthdiff(indiv)*REAL((xagedays(indiv)-1)/xagedays(indiv))+(xworth(indiv)*(xtotwght(indiv)-xprevwght(indiv)))/REAL(xagedays(indiv))
speciesweightdiff(xspecies(indiv))=speciesweightdiff(xspecies(indiv))+(xtotwght(indiv)-xprevwght(indiv))
!DEC$ IF DEFINED(REGISTER_ONLY_LAST_WORTH)
speciesweightandworthdiff(xspecies(indiv))=speciesweightandworthdiff(xspecies(indiv))+(xworth(indiv)*(wghtchng))
!DEC$ ELSE
speciesweightandworthdiff(xspecies(indiv))=speciesweightandworthdiff(xspecies(indiv))+(xworthdailysumming(indiv)*(wghtchng))
!DEC$ ENDIF
speciesweightdiffnumber(xspecies(indiv))=speciesweightdiffnumber(xspecies(indiv))+1
IF (xsensce(indiv) .EQ. 0 .AND. xsomwght(indiv) .GT. xstrvwght(indiv)) THEN !if indiv not senescent AND not going to starve, add total weight change to annual running total (g wwt)
 !DEC$ IF DEFINED(TRACE_INDIVIDUALS)
  IF(isselectedindividual(indiv) .EQ. 1) THEN
    WRITE (310,*) "Using spawn loss value for individual: ", indiv, " of species: ", xspecies(indiv), " on year: ", year, " day: ", jday, " and hour: ", hour, " with batch weight: ", xbatchwght(indiv), " and spawn loss: ", xspawnloss(indiv)
  END IF
 !DEC$ ENDIF
 netproductivity(ifwsp,c,r) = netproductivity(ifwsp,c,r) + ((xtotchng(indiv)+xspawnloss(indiv)) * xworth(indiv))
 productivityvalues(ifwsp,c,r) = productivityvalues(ifwsp,c,r) + ((xtotchng(indiv)+xspawnloss(indiv)) * xworth(indiv))
END IF
IF(xsomwght(indiv) .LT. xstrvwght(indiv)) THEN
 IF (xsensce(indiv) .EQ. 0 ) THEN !if indiv not senescent AND going to starve, subtract INITIAL total weight from annual running total (g wwt)
  netproductivity(ifwsp,c,r) = netproductivity(ifwsp,c,r) - ((xtotwght(indiv)+xtotchng(indiv)+xspawnloss(indiv)) * xworth(indiv))
     UPDATE_STARVATION_TROPHIC_TRANSFER_NOT_SENESCENT(indiv)
  ELSE
    UPDATE_STARVATION_TROPHIC_TRANSFER_SENESCENT(indiv)
 END IF
END IF
IF (xsomwght(indiv) .LT. expsomwght) THEN               !if new som wght < expected wght (g wwt)
 IF (xsomwght(indiv) .LT. xstrvwght(indiv)) THEN        !if new som wght < starvation wght (g wwt)
  IF (xsensce(indiv) .EQ. 0) THEN                         !if not senescent
   mortloss(fwsp,2) = mortloss(fwsp,2) + xworth(indiv)  !add change in worth to daily running tot of worth lost to starvation
   cummortloss(2) = cummortloss(2) + xworth(indiv)      !add change in worth to annual running tot of worth lost to starvation
   xalive(indiv) = 3                                    !remove indiv from population (0=unassigned, 1=alive, 2=dead(nat mort incl. predation), 3=dead(starved), 4=dead(stranded), 5=dead(old))
  ELSE                                                  !otherwise senescent
   mortloss(fwsp,5) = mortloss(fwsp,5) + xworth(indiv)  !add change in worth to daily running tot of worth lost to senescence
   cummortloss(5) = cummortloss(5) + xworth(indiv)      !add change in worth to annual running tot of worth lost to senescence
   xalive(indiv) = 5                                    !remove indiv from population (0=unassigned, 1=alive, 2=dead(nat mort incl. predation), 3=dead(starved), 4=dead(stranded), 5=dead(old))
  END IF
  
  IF (fileflag(11) .EQ. 1 .AND. (fwsp .EQ. isspeciesout .OR. isspeciesout .EQ. 0)) THEN !if output desired AND (want specific species OR all sp)
   outflag = 12; CALL output(0,0,0,0,0,0,0)             !output attributes of dead indiv (12=file 120)
  END IF !output desired (dead indivs)
  xworth(indiv)=0.D0; xsomwght(indiv)=0.D0; xgonwght(indiv)=0.D0; xtotwght(indiv)=0.D0; xlenchng(indiv)=0.D0; xprevwght(indiv)=0.D0; xavgwghtdiff(indiv)=0.D0 !zero worth, set som, gon, and total wghts to zero (g wwt), record a zero change in length (mm)
  xprevwghtandworth(indiv)=0.D0; xavgwghtandworthdiff(indiv)=0.D0
 ELSE                                                   !otherwise, wght loss not severe
  xlenchng(indiv) = 0.D0                                !but length does not change (b/c cannot shrink in length)
 END IF !new total wght vs starvation wght
ELSE                                                    !otherwise, new wght => expected wght (g wwt)
 oldlength = xlen(indiv)                                !store old length (mm)
 xlen(indiv) = length(xsomwght(indiv),lw_scalar(fwsp),lw_expo(fwsp)) !grow indiv in length via a FUNCTION that converts som wght (g wwt) to length (mm)
 xlenchng = xlen(indiv) - oldlength                     !calc length change (mm)
 xmindep(indiv) = xlen(indiv) / 3000.D0                 !use new length to update min depth (m)
 mortflag = 3                                           !set mort flag to 1 (1=assign(init), 2=assign(recruit), 3=weightchange)
 CALL mortcalc(0)                                       !use new length to calc new hourly inst nat mort (/hr)
END IF !new total wght vs expected wght

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE mortcalc(zz) !each time that length changes, convert to exptected weight
USE time_etc        !and then use equation 1 and table 1 in Lorenzen (1996) to
USE species         !calculate mortality rate. Use Lorenzen's tank parameters (b/c
USE indivattributes !other sources of mort are built into model).
USE reproduction    ! For croaker, use tank parameters plu
IMPLICIT NONE       !pred mort, calculated from Lorenzen's eq'n 3 assuming a
					!non-predator mort rate of 67% of nat mort.
INTEGER ind, sp !local shorthand for 'indiv' and species
REAL(8) wt      !expected weight given lenght (g wwt)
INTEGER zz

IF (mortflag .EQ. 1) ind = zz !if called when init indivs, set 'ind' to 'zz'
IF (mortflag .EQ. 2) ind = oldindivs(spaceavail) !if called when recruit indivs, set 'ind' to 'oldindivs(spaceavail)'
IF (mortflag .EQ. 3) ind = indiv !if called from weightchange, set 'ind' to 'indiv'
sp = xspecies(ind) !store 'xspecies(indiv)' as local shorthand
wt = lw_scalar(spposn(sp)) * xlen(ind) ** lw_expo(spposn(sp)) !calc expected wght (g wwt) given length (mm)
xmort(ind) = (0.91D0 * wt**-0.427D0) / REAL((ndays * nhours)) !Lorenzen's tank parameters

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE sumstages  !each hour, sum indiv worths, lengths, and weights across ages
USE time_etc          !for ea sp. Calc mean and max length (m) and mean weigth (g wwt)
USE physicalenv       !for output file 44 (out_abund_by_stage.out) and outpute file 140
					  !(out_mean_by_stage.out). Also keep track of worth entering and
USE species           !leaving stage for lifetable (file 180, 'out_lifetable.out').
USE indivattributes
USE bioenergetics
USE reproduction
IMPLICIT NONE
INTEGER age   !local shorthand for age in years

age = xageyrs(indiv)                                          !store year as local shorthand
ageabund(fwsp,age) = ageabund(fwsp,age) + xworth(indiv)       !add to running total of worth
IF (hour .EQ. maxfeedhr) agemeanp(fwsp,age) = agep(fwsp,age) / ageabund(fwsp,age) !if last hour to feed, update age- and sp-specific mean p (propn of cmax)
IF (hour .EQ. nhours) THEN                                    !if last hour of day (when length and weight updated, feeding over)
 agelen(fwsp,age) = agelen(fwsp,age) + (xlen(indiv) * xworth(indiv)) !add to running total of length (mm)
 agemeanlen(fwsp,age) = agelen(fwsp,age) / ageabund(fwsp,age) !update age- and sp-specific mean length (mm)
 agewgt(fwsp,age) = agewgt(fwsp,age) + (xtotwght(indiv) * xworth(indiv))!add to running total of weight (g wwt)
 agemeanwgt(fwsp,age) = agewgt(fwsp,age) / ageabund(fwsp,age) !update age- and sp-specific mean wght (g wwt)
 IF (xlen(indiv) .GT. maxlen(fwsp)) maxlen(fwsp) = xlen(indiv)!if indiv's length > max length so far observed, store indiv's max length (mm)
 IF (jday .EQ. ndays) THEN                                    !if last day of year
  lftblout(3+age,fwsp) = lftblout(3+age,fwsp) + xworth(indiv) !number leaving current (st)age = number in (st)age at end of year
  lftbloutflag(3+age,fwsp) = 1                                !flag that worth has left (st)age
  IF (age .EQ. 1) lftbldurn(3+age,fwsp) = lftbldurn(3+age,fwsp) + (xagedays(indiv) * xworth(indiv)) !if in first yr, add to sum of (st)age durations weighted by worth (aved at end of yr)
  IF (age .GT. 1) lftbldurn(3+age,fwsp) = lftbldurn(3+age,fwsp) + (ndays * xworth(indiv)) !if not in first yr, add to fixed (st)age duration weighted by worth (aved at end of yr)
 END IF !last day of year
END IF !last hour of day

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE extinctcheck  !at end of year, use results of subroutine 'sumstages'
USE time_etc             !to check for extinct species. Sums are based on worth.
USE species              !Because indiv worth is set to zero when it drops below
USE indivattributes      !'minworth', 'minworth' is a reliable indicator of whether
USE reproduction         !or not at least one indivs of sp x is present (alive).
IMPLICIT NONE            !If no indivs of any sp are present, sim is stopped.
INTEGER z    !local index for error do loop
INTEGER xx

DO xx = 1, nfwsp                             !loop over food web sp, print error if sum of worth across stages < minimum
 IF (extinctquery(xx) .EQ. 0) THEN
  IF (SUM(ageabund(xx,:)) .LT. minworth) THEN 
   PRINT *, "Food web species", xx, "extinct at year ", year, ", day ", jday, " and hour ",hour
   extinctquery(xx) = 1
  END IF
 END IF
END DO !species

IF (SUM(ageabund) .LT. minworth) THEN      !if all species extinct
 DO z = 1, 25
  CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
 END DO
 PRINT *, "All food web sp extinct at end of year", year    !end sim
 PRINT *, "Simulation stopped."
 CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
 STOP
END IF

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE age_years   !at end of year, increase age by 1, change YOYs to
USE time_etc           !juveniles (if immature), record IDs of indivs older
USE indivattributes    !than max age (as defind by 'ageclasses')
USE species
USE reproduction
IMPLICIT NONE

 oldcount=0; oldindivs=0           !zero out

 DO indiv = 1, totindivs           !loop over indivs
  fwsp = spposn(xspecies(indiv))   !store indiv's sp-specific posn in 'nfwsp'-dimensioned arrays as global shorthand
  xageyrs(indiv) = xageyrs(indiv) + 1 !regardless of 'alive', increase age of all indivs by 1 year
  IF (xageyrs(indiv) .EQ. 2 .AND. xmat(indiv) .EQ. 0) THEN !if no longer YOY but still immature
   xstage(indiv) = 2               !change late life stage to juvenile (1=YOY, 2=juvenile, 3=adult)
  END IF !no longer YOY but still immature
  IF (xageyrs(indiv) .GT. ageclasses(fwsp)) THEN !if indiv older than max age
   mortloss(fwsp,5) = mortloss(fwsp,5) + xworth(indiv) !add worth to daily running tot of worth lost to senescence
   cummortloss(5) = cummortloss(5) + xworth(indiv)     !add worth to annual running tot of worth lost to senescence
   assignflag = 4                  !(1=initialize, 2=recruit, 3=splitworth, 4=zero old)
   CALL assignattributes(0,0,0)    !xx,yy,zz not used with assignflag=4
   oldcount = oldcount + 1         !increment counter
   oldindivs(oldcount) = indiv     !store ID number (position) of that individual
  END IF !older than max age
 END DO !indivs

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE assignattributes(xx,yy,zz) !assing indiv attributes according to needs at specific times in code
USE physicalenv             !(assignflag 1=initialize, 2=recruit, 3=split worth, 4=zero old)
USE species
USE indivattributes
USE movement
USE reproduction
IMPLICIT NONE
REAL(8) length   !a FUNCTION that accepts wght (g wwt), sp-specific coeffs, and returns length (mm)
REAL(8) pran    !a random number, even dist, 0-1
REAL(8) mxwrt    !highest recorded yoy worth (used when flag=3: split worth)
INTEGER ind      !local shorthand for 'indiv'
INTEGER hab      !local shorthand for sp-specific hab type in which to deposit new (init and recruit) indivs
INTEGER maskposn !a randomly-generated position in a hab type mask. Max value = number of cells in mask
INTEGER fails    !number of times subroutine failed to find wet opt hab type in which to init/recruit indiv
INTEGER maxind   !ID of YOY indiv w highest recorded worth (used when flag=3: split worth)
INTEGER lowsplitwrth(nfwsp) !sp-specific flag indicating "low split worth" warning was printed
INTEGER c, r     !local shorthand for 'i_col(ind)' and 'j_row(ind)'
INTEGER x, y     !local shorthand for 'x_dist(ind)' and 'y_dist(ind)'
INTEGER a, b, z  !local index loops (z is for error beepq)
INTEGER xx,yy,zz

IF (assignflag .EQ. 1 .OR. assignflag .EQ. 2) THEN !assign attributes (1 called from 'initindiv', 2 from 'recruitment')

 IF (assignflag .EQ. 1) THEN         !initialize attributes at start of simulation (called from 'initindiv')
  ind = zz                           !store 'indiv' as local shorthand

  xspecies(ind) = speciesid(xx)      !'xx' is index of 'nfwsp' loop in 'initindiv' (equivalent to 'fwsp')
  xworth(ind) = ageclasspopsize(xx) / REAL(ageclassspace(xx))
  !DEC$ IF (.NOT. DEFINED(REGISTER_ONLY_LAST_WORTH))
  worthannualsumming(xspecies(ind))=worthannualsumming(xspecies(ind))+xworth(ind)
  !DEC$ ENDIF
  speciesworthdailysumming(xspecies(ind))=speciesworthdailysumming(xspecies(ind))+xworth(ind)
  xworthdailysumming(ind)=xworthdailysumming(ind)+xworth(ind)
  xageyrs(ind) = yy + 1              !these individuals just had their birthday ('yy' = age class in 'initindiv')
  xsomwght(ind) = initwt(xx,xageyrs(ind)) !set somatic wght to sp-specific wght-at-age (g wwt)
  xstage(ind) = 2                    !assume juvenile (1=YOY, 2=juvenile, 3=adult)
  mortflag = 1                       !for call to 'mortcalc' below, set mort flag to 1 (1=assign(init), 2=assign(recruit), 3=weightchange)
 END IF !assignflag 1

 IF (assignflag .EQ. 2) THEN         !assign attributes to new recuits (called from 'recruitment')
  ind = oldindivs(spaceavail)        !store 'indiv' as local shorthand

  xspecies(ind) = speciesid(xx)      !'xx' is index of 'nfwsp' loop in 'recruitment' (equivalent to 'fwsp')
  xworth(ind) = newyoy(xx) / REAL(modrecdist(xx,jday))
  !DEC$ IF (.NOT. DEFINED(REGISTER_ONLY_LAST_WORTH))
  worthannualsumming(xspecies(ind))=worthannualsumming(xspecies(ind))+xworth(ind)
  !DEC$ ENDIF
  speciesworthdailysumming(xspecies(ind))=speciesworthdailysumming(xspecies(ind))+xworth(ind)
  xworthdailysumming(ind)=xworthdailysumming(ind)+xworth(ind)
  xageyrs(ind) = 1
  xstage(ind) = 1                    !1=YOY, 2=juvenile, 3=adult
  xsomwght(ind) = metmorphwt(xx)
  mortflag = 2                       !for call to 'mortcalc' below, set mort flag to 1 (1=assign(init), 2=assign(recruit), 3=weightchange)
  !DEC$ IF DEFINED(TRACE_INDIVIDUALS)
   CALL registerindividual(ind)
  !DEC$ ENDIF
 END IF ! assignflag 2

 xalive(ind) = 1                     !assign remaining attributes identically for flags 1 and 2
 xagedays(ind) = 1
 xgonwght(ind) = 0.D0                !g wwt
 xprevwght(ind)=0.D0; xavgwghtdiff(ind)=0.D0
 xprevwghtandworth(ind)=0.D0; xavgwghtandworthdiff(ind)=0.D0  
 xtotwght(ind) = xsomwght(ind) + xgonwght(ind) !calc total wght as sum of somatic and gonadal wghts (g wwt)
 xsomchng(ind)=0.D0; xgonchng(ind)=0.D0; xtotchng(ind) = 0.D0   !g wwt
 xworthchng(ind)=0.D0
 xspawnloss(ind)=0.D0
 xlen(ind) = length(xsomwght(ind),lw_scalar(xx),lw_expo(xx)) !use a FUNCTION to convert wght to length (xx = xspecies)
 xlenchng(ind) = 0.D0                !mm
 xmindep(ind) = xlen(ind) / 3000.D0  !define min depth as 1/3 of indiv's length (m)
 CALL mortcalc(zz)                   !use length to calc new hourly inst nat mort (/hr)
 xmat(ind) = 0                       !assume immature (if long enough, will change in subroutine 'maturity')
 IF (xspecies(ind) .EQ. 2) THEN      !if new indiv is sp IS
  xbreedsom(ind) = xsomwght(ind)     !set breedsom (for older age classes not init, will be set 1 week before start of spawn season, which is Mar 1)
  xtrgtinvst(ind) = 1.18D0 * (1.D0 - EXP(REAL(-ndays * nhours) * xmort(ind))) !calc target annual reprod invest (g wwt invested as a prop'n of g wwt somatic wght)
 END IF
 xreprdinvst(ind) = 0.D0             !g wwt
 xfecflag(ind) = 0
 xfec(ind) = 0.D0                    !eggs
 xbatchwght(ind) = 0.D0              !g wwt
 xbtchcnt(ind) = 0
 xsensce(ind) = 0

 !Now assign position on grid by randomly selecting among cells that are sp-specific optimal hab AND deep enough (> 'xmindep')
 !(1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh))

 DO a = 1, 2                     !loop over init hab types 1 (deep water) and 2 (SAV)
  hab = inithab(xx) - (a - 1)    !store init hab type as local shorthand

  IF (habcount(hab) .EQ. 0) THEN !if no cells of desired hab type
   DO z = 1, 25
	CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
   END DO
   PRINT *, "Error 1 in 'assignattributes' during assignflag:", assignflag
   PRINT *, "(1=initialize, 2=recruit, 3=split worth, 4=zero old)."
   PRINT *, "There are no cells of hab type", hab
   PRINT *, "Cannot assign cells to indivs of fwsp", xx
   IF (assignflag .EQ. 2) PRINT *, "at the end of day", jday
   PRINT *, "Simulation stopped."
   CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
   STOP
  END IF !no cells of desired hab type

  maskposn = INT(pran(1) * habcount(hab)) + 1 !choose random mask position associated with hab type
  IF (maskposn .GT. habcount(hab)) maskposn = habcount(hab) !if 'ran11(idum)' exactly 1, set 'maskposn' to max mask posn
  c = habmask(hab,1,maskposn)             !extract coord of randomly selected cell
  r = habmask(hab,2,maskposn)

  IF (waterdepth(c,r) .LT. xmindep(ind)) THEN !if random cell shallower than min depth (m)
   fails = 1                              !record first failure to find wet cell

   DO b = 1, habcount(hab)-1              !loop over remaining cells of hab type 'hab'
	maskposn = maskposn + 1               !increment position in mask by 1
	IF (maskposn .GT. habcount(hab)) maskposn = 1 !if position > max posn, reset to 1
	c = habmask(hab,1,maskposn)           !extract cell coord
	r = habmask(hab,2,maskposn)
	IF (waterdepth(c,r) .GE. xmindep(ind)) EXIT !if cell as or deeper than min depth (m), exit loop early
	fails = fails + 1                      !otherwise, record another fail, continue looping

	IF (fails .EQ. habcount(hab) .AND. a .EQ. 3) THEN  !if even deep water hab too shallow, print error
	 DO z = 1, 25
	  CALL beepqq(200,50) !beep (frequency in Hz, duration in milliseconds)
	 END DO
	 PRINT *, "Error 2 in 'assignattributes' during assignflag:", assignflag
	 PRINT *, "(1=initialize, 2=recruit, 3=split worth, 4=zero old)."
	 PRINT *, "There are no wet cells of hab types (opt, next deepest)", inithab(xx), inithab(xx)-1
	 PRINT *, "Cannot assign cells to indivs of fwsp", xx
	 IF (assignflag .EQ. 2) PRINT *, "at the end of day", jday
	 PRINT *, "Simulation stopped."
	 CALL timecounter     !reprint start date and time, calc and output run-time in days, hours, mintues, and seconds
	 STOP
	END IF !next deepest completely dry
   END DO !cells of hap type 'hab'
  END IF !random cell dry

  IF (waterdepth(c,r) .GE. xmindep(ind)) EXIT !if cell as or deeper than min depth (m), exit loop

 END DO ! exit loop via wet cell or error

 x = c * cellsize - cellsize / 2.D0  !set x and y distances from origin to center of selected cell (m)
 y = r * cellsize - cellsize / 2.D0

 i_col(ind) = c; j_row(ind) = r      !convert shorthand back to longhand
 x_dist(ind) = x; y_dist(ind) = y
 hoursdry(ind) = 0                   !zero counter that keeps track of number of hours that indiv has been in a dry cell

END IF !assignflag 1 or 2

IF (assignflag .EQ. 3) THEN          !if unused space at end of year, split high-worth indivs (called from 'recruitment')
 ind = oldindivs(spaceavail)         !store ID of new indiv
 mxwrt = 0.D0                        !zero out highest recorded worth
 maxind = 0                          !zero 'maxind'; if remains zero, no indivs of sp xx are alive
 IF (yy .EQ. 1) lowsplitwrth(xx) = 0 !if first indiv to be split, zero flag indicating that "low split worth" warning was printed for sp xx

 DO zz = 1, totindivs                !loop over indivs
  IF (xalive(zz) .EQ. 1 .AND. xageyrs(zz) .EQ. 1 &                          !if indiv 'zz' alive AND yoy
	.AND. xspecies(zz) .EQ. speciesid(xx) .AND. xworth(zz) .GT. mxwrt) THEN !AND same species AND worth highest so far recorded,
   mxwrt = xworth(zz)                !store indiv's worth
   maxind = zz                       !and ID
  END IF !alive yoy of same species
 END DO !indivs

 IF (maxind .NE. 0) THEN             !if at least one indiv of sp xx alive to split
  split(spposn(xspecies(maxind))) = split(spposn(xspecies(maxind))) + 1 !add to sp-specific split counter (for output file 33 'out_allocatedspace.out')
  xworth(maxind) = xworth(maxind) / 2.D0  !render highest worth in twain
  IF (xworth(maxind) .LT. minworth .AND. lowsplitwrth(spposn(xspecies(maxind))) .EQ. 0) THEN !if twain worth not too low
   PRINT *, "Warning: split worth < survial threshold for fwsp", spposn(xspecies(maxind)) !print warning once for sp xx
   lowsplitwrth(spposn(xspecies(maxind))) = 1 !flag that "low split worth" warning was printed for sp xx
  END IF

  xspecies(ind) = xspecies(maxind)   !set attributes of new indiv to existing (zz) indiv
  xageyrs(ind) = xageyrs(maxind)
  xagedays(ind) = xagedays(maxind)
  xworth(ind) = xworth(maxind)
  xalive(ind) = xalive(maxind)
  xstage(ind) = xstage(maxind)
  xmat(ind) = xmat(maxind)
  xspwnday(ind) = xspwnday(maxind)   !give same spawn jday
  xfecflag(ind) = xfecflag(maxind)
  xfec(ind) = xfec(maxind)           !eggs
  xbatchwght(ind) = xbatchwght(maxind) !g wwt
  xbtchcnt(ind) = xbtchcnt(maxind)
  xsomwght(ind) = xsomwght(maxind)   !the following are likely to diverge through last day due to bioen and movement
  xgonwght(ind) = xgonwght(maxind)   !g wwt
  xprevwght(ind)=xprevwght(maxind)
  xavgwghtdiff(ind)=xavgwghtdiff(maxind)
  xprevwghtandworth(ind)=xprevwghtandworth(maxind)
  xavgwghtandworthdiff(ind)=xavgwghtandworthdiff(maxind)
  xtotwght(ind) = xtotwght(maxind)   !g wwt
  xbreedsom(ind) = xbreedsom(maxind) !g wwt
  xtrgtinvst(ind) = xtrgtinvst(maxind) !(g wwt invested as a prop'n of g wwt somatic wght)
  xreprdinvst(ind) = xreprdinvst(maxind) !g wwt
  xlen(ind) = xlen(maxind)           !mm
  xmindep(ind) = xmindep(maxind)     !m
  xmort(ind) = xmort(maxind)         !/hr
  i_col(ind) = i_col(maxind)
  j_row(ind) = j_row(maxind)
  x_dist(ind) = x_dist(maxind)       !m
  y_dist(ind) = y_dist(maxind)       !m

  xsomchng(ind)=0.D0; xgonchng(ind)=0.D0; xtotchng(ind)=0.D0; xspawnloss(ind)=0.D0 !these will be defined by bioenergetics (g wwt)
  xworthchng(ind)=0.D0
  xlenchng(ind) = 0.D0               !mm

 ELSE                                !otherwise, no indivs available to split. Leftover space will be wasted. Set basic parameters so that subroutine 'age_years' turns over unused model indivs when they are > max age
  xspecies(ind) = speciesid(xx)      !define species
  xageyrs(ind) = 1                   !set age in years
  xalive(ind) = 0                    !0=unassigned, 1=alive, 2=dead(nat mort incl. predation), 3=dead(starved), 4=dead(stranded), 5=dead(old)
 END IF !indivs to split
END IF !assignflag 3

IF (assignflag .EQ. 4) THEN          !zero out old indivs (called from 'age_years')
 ind = indiv

 xspecies(ind) = xspecies(ind)       !needed for subroutine 'age_years' to turn over old, unused model indivs
 xageyrs(ind) = 1                    !needed for subroutine 'age_years' to turn over old, unused model indivs
 xagedays(ind) = 0                   !zero out all other attributes
 xworth(ind) = 0.D0
 xalive(ind) = 6                     !0=unassigned, 1=alive, 2=dead(nat mort incl. predation), 3=dead(starved), 4=dead(stranded), 5=dead(old)
 xstage(ind) = 0
 xmat(ind) = 0
 xavgwghtdiff(ind)=0.D0; xprevwght(ind)=0.D0; xavgwghtandworthdiff(ind)=0.D0; xprevwghtandworth(ind)=0.D0;
 xsomwght(ind)=0.D0; xgonwght(ind)=0.D0; xtotwght(ind) = 0.D0  !g wwt
 xsomchng(ind)=0.D0; xgonchng(ind)=0.D0; xtotchng(ind) = 0.D0  !g wwt
 xworthchng(ind)=0.D0
 xspawnloss(ind) = 0.D0
 xreprdinvst(ind) = 0.D0             !g wwt
 xlen(ind) = 0.D0                    !mm
 xlenchng(ind) = 0.D0                !mm
 xmindep(ind) = 0.D0                 !m
 xmort(ind) = 0.D0                   !/hr
 xfecflag(ind) = 0
 xfec(ind) = 0.D0                    !eggs
 xbatchwght(ind) = 0.D0              !g wwt
 xbtchcnt(ind) = 0
 xsensce(ind) = 0
 i_col(ind) = 0
 j_row(ind) = 0
 x_dist(ind) = 0.D0
 y_dist(ind) = 0.D0

END IF !assignflag 4

END SUBROUTINE

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

!----------------------------------------------------------------------
SUBROUTINE output(i, j, ii, jj, xx, yy, zz)     !write to output files according to flags, where
USE time_etc           !flag 2  = file 22 (attributes of alive indivs)
USE physicalenv        !flag 3  = file 33 (space allocation)
USE preybase           !flag 4  = file 44 (abundance-by-stage)
USE indivattributes    !flag 5  = file 55 (relativized cell elevations)
USE bioenergetics      !flag 6  = file 66 (water depths)
USE species            !flag 7  = file 77 (inundation frequency)
USE movement           !flag 9  = file 99 (fish-by-cell)
USE reproduction       !flag 10 = file 100 (habitat types)
IMPLICIT NONE          !flag 11 = file 110 (intial attributes of new indivs)
					   !flag 12 = file 120 (attributes of dead indivs)
					   !flag 13 = file 130 (detailed movement info of user-defined indivs)
					   !flag 14 = file 140 (mean length, weight, and density by stage)
					   !flag 15 = file 150 (indiv diets)
					   !flag 16 = file 160 (prey-by-cell)
					   !flag 17 = file 170 (prey averages by habitat and grid)
					   !flag 18 = file 180 (life table)
					   !flag 19 = file 190 (mean diet)
					   !flag 20 = file 200 (abund-based habitat ditribution)
					   !flag 21 = file 210 (highest density (worth/m2) on grid by sp)
REAL(8) zmort   !mortality rate (outflag 18)
REAL(8) area    !cell area (m^2) (outflag 21)
INTEGER z       !local variable for indexing do loop (outflag 13)
INTEGER c, r    !local variables for indexing column and row loops
INTEGER cc, rr  !local shorthand for colmn and row (outflags 2, 11, 12, 13)
INTEGER ind, sp !local shorthand for 'indiv' (outflag 11) and species id (outflag 19)
INTEGER xx, yy, zz
INTEGER ii, jj
INTEGER i, j
INTEGER inundationtime

CALL file_lock(outflag-1)

IF (outflag .EQ. 2) THEN  !output alive indiv attributes (file 22) !222222222222222222222222
    
     IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 &     !but limit to MOD = 0
       .AND. MOD(hour,iswhourout) .EQ. 0 .AND. MOD(indiv,iswindivout) .EQ. 0) THEN
      cc = i_col(indiv); rr = j_row(indiv) !store column and row as local shorthand
      IF (MOD(jday, 30) .EQ. 15) THEN
      IF (MOD(xagedays(indiv), 30) .EQ. 15) THEN
      !DEC$ IF DEFINED(TRACE_INDIVIDUALS)
     IF (isselectedindividual(indiv) .EQ. 1) THEN
      CALL checkandclearindividual(indiv)
      !DEC$ ENDIF
      WRITE (22, 223) year, jday, cumday, hour, cumhr, indiv, fwsp, xspecies(indiv), xageyrs(indiv), &
	    xagedays(indiv), xworth(indiv), xalive(indiv), xstage(indiv), xcmax(indiv), totcons(indiv)/xcmax(indiv), &
	    xsomwght(indiv), xsomchng(indiv), xgonwght(indiv), xgonchng(indiv), xtotwght(indiv), &
	    xtotchng(indiv), xlen(indiv), xlenchng(indiv), xmort(indiv), xmat(indiv), xspwnday(indiv), xfecflag(indiv), &
	    xbreedsom(indiv), xtrgtinvst(indiv), xreprdinvst(indiv), xfec(indiv)/REAL(batches(spposn(xspecies(indiv)))), xbatchwght(indiv), &
	    xeggprdn(indiv), xbtchcnt(indiv), cc, rr, x_dist(indiv), y_dist(indiv), waterdepth(cc,rr), habitat(cc,rr), xsensce(indiv), &
	    xmindep(indiv), xstrvwght(indiv), xspawnloss(indiv)
      223 FORMAT (1X, I3, 1X, I3, 4X, I4, 3X, I2, 1X, I6, 1X, I5, 5X, I1, 5X, I1, 6X, I2, &
			    4X, I4, 1X, ES10.3, 5X, I1, 5X, I1, 1X, ES10.3, 1X, F6.4, &
			    1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
			    1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 6X, I1, 8X, I3, 8X, I1, &
			    1X, ES10.3, 1X, ES10.3, 3X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
			    1X, ES10.3, 7X, I2, 1X, I3, 1X, I3, 1X, F6.2, 1X, F6.2, 1X, F5.2, 7X, I1, &
			    6X, I1, 2X, F5.2, 1X, ES10.3, 1X, ES10.3)
		!DEC$ IF DEFINED(TRACE_INDIVIDUALS)
	    END IF
	    !DEC$ ENDIF
      END IF
     END IF !output limits
 END IF
END IF !flag 2

IF (outflag .EQ. 3 .AND. hour .eq. nhours) THEN  !output space allocation (e.g., used, available, unused) (file 33)
 WRITE (33, 334) year, jday, hour, totindivs, totestab, &
			 totnew, totavail, totsplit, totunused, wtemp
 334 FORMAT (1X, I3, 1X, I3, 3X, I2, 4X, I6, 3X, I10, &
			 6X, I5, 7X, I5, 7X, I5, 8X, I5, 8X, ES10.3)
END IF !flag 3

IF (outflag .EQ. 4) THEN  !output abundance-by-stage (file 44) !444444444444444444444444444444
 IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 &     !but limit to MOD = 0
   .AND. MOD(hour,iswhourout) .EQ. 0) THEN
  WRITE (44, 446) year, cumday, hour, i, speciesid(i), dayeggprdn(i), cohorttot(1,i), &  !where i = fwsp
	  cohorttot(2,i), cohorttot(3,i), ageabund(i,1), ageabund(i,2), ageabund(i,3), &
	  SUM(ageabund(i,:)), avenewrth(i), senescentcount(i), senescentvalue(i), notcalculatedspawningdays(i)
  446 FORMAT (1X, I3, 3X, I5, 3X, I2, 5X, I1, 5X, I1, 1X, ES10.3, 1X, ES10.3, 1X, &
			  ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, &
			  ES10.3, 1X, ES10.3, 1X, I3, 13X, ES10.3, 5X, I3)
 END IF !output limits
END IF !flag 4

IF (outflag .EQ. 5) THEN  !output relativized cell elevations (file 55) !555555555555555555555
 DO r = 1, rows                             !loop over rows in sub-grid
  WRITE (55, 555) (cellelev(c,r),c=1,cols)  !output as a grid (implied DO loop)
  555 FORMAT (2200(F8.5,1X))                !(where 2200 is some number > rows)
 END DO !sub-grid rows
END IF !flag 5

IF (outflag .EQ. 6) THEN  !output water depths (file 66) !666666666666666666666666666666666666
 IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 &   !but limit to MOD = 0
   .AND. MOD(hour,iswhourout) .EQ. 0) THEN
  DO r = 1, rows                              !loop over rows in sub-grid
   WRITE (66, 666) (waterdepth(c,r),c=1,cols) !output as a grid (implied DO loop)
   666 FORMAT (2200(F5.2,1X))                 !(where 2200 is some number > rows)
  END DO !sub-grid rows
 END IF !output limits
END IF !flag 6

IF (outflag .EQ. 7) THEN  !output hours inundated (file 77) !777777777777777777777777777777777
 DO r = 1, rows                             !loop over rows in sub-grid
  WRITE (77, 777) (inund(c,r),c=1,cols)     !output as a grid (implied DO loop)
  777 FORMAT (2200(I4,1X))                  !(where 2200 is some number > 'rows')
 END DO !sub-grid rows
END IF !flag 7

IF (outflag .EQ. 9) THEN  !output fish-by-cell (file 99) !999999999999999999999999999999999999
! IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 &   !but limit to MOD = 0
!   .AND. MOD(hour,iswhourout) .EQ. 0) THEN
   IF ( (year .EQ. 3) .AND.  (inundationtime() .EQ. 1)) THEN
  DO c = 1, cols                                        !loop over columns
   DO r = 1, rows                                       !loop over rows
	WRITE (99, 990) year, jday, hour, i, speciesid(i), c, r, & !where i, c, r = fwsp, column, row
		habitat(c,r), indivbycell(i,c,r), worthbycell(i,c,r), bmassbycell(i,c,r)
	990 FORMAT (1X, I3, 1X, I3, 3X, I2, 5X, I1, 5X, I1, 1X, I4, 1X, I4, 7X, &
				I1, 1X, I6, 1X, ES10.3, 1X, ES10.3)
   END DO !rows
  END DO !columns
  END IF
! END IF !time-specific output limiters
END IF !flag 9

IF (outflag .EQ. 10) THEN  !output habitat types (file 100) !1010101010101010101010101010101
 DO r = 1, rows                             !loop over rows in sub-grid
  WRITE (100, 1001) (habitat(c,r),c=1,cols) !output as a grid (implied DO loop)
  1001 FORMAT (2200(I1,1X))                 !(where 2200 is some number > 'rows')
 END DO !sub-grid rows
END IF !flag 10

IF (outflag .EQ. 11) THEN  !output initial attributes (file 110) !11_11_11_11_11_11_11_11_11
 IF (assignflag .EQ. 1) ind = zz                     !if init, store 'indiv' from subroutine 'initindiv' as local shorthand
 IF (assignflag .GT. 1) ind = oldindivs(spaceavail)  !if recruit (2) or split (3), store 'indiv' from subroutine 'recruitment' as local shorthand
 cc = i_col(ind); rr = j_row(ind)                    !store column and row as local shorthand
 IF (year .LE. 1 .OR. (year .GE. 2 .AND. MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0)) THEN !if year 0 (start of sim) or 1, OR any other year AND output desired, output ALL new indivs on all days
  IF (xalive(ind) .EQ. 1) THEN                       !if alive                                              !(note that limiter 'isspeciesout' is applied in subroutine 'recruitment')
   WRITE (110, 1101) year, jday, hour, assignflag, ind, spposn(xspecies(ind)), xspecies(ind), &
	   xageyrs(ind), xagedays(ind), xworth(ind), xalive(ind), xstage(ind), xmat(ind), &
	   xsomwght(ind), xgonwght(ind), xtotwght(ind), xlen(ind), xmort(ind), &
	   cc, rr, x_dist(ind), y_dist(ind), habitat(cc,rr), waterdepth(cc,rr)
   1101 FORMAT (1X, I3, 1X, I3, 3X, I2, 8X, I1, 1X, I5, 5X, I1, 5X, I1, &
				7X, I1, 4X, I4, 1X, ES10.3, 5X, I1, 5X, I1, 6X, I1, &
				1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
				1X, I3, 1X, I3, 1X, F6.2, 1X, F6.2, 7X, I1, 1X, F5.2)
  ELSE                                               !otherwise, alive = 0 (indiv unassigned b/c unused space)
   WRITE (110, 1102) year, jday, hour, assignflag, ind, spposn(xspecies(ind)), xspecies(ind), &
	   xageyrs(ind), xagedays(ind), xworth(ind), xalive(ind), xstage(ind), xmat(ind), &
	   xsomwght(ind), xgonwght(ind), xtotwght(ind), xlen(ind), &
	   cc, rr, 0.D0, 0.D0, 0, 0.D0
   1102 FORMAT (1X, I3, 1X, I3, 3X, I2, 8X, I1, 1X, I5, 5X, I1, 5X, I1, &
				7X, I1, 4X, I4, 1X, ES10.3, 5X, I1, 5X, I1, 6X, I1, &
				1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
				1X, I3, 1X, I3, 1X, F6.2, 1X, F6.2, 7X, I1, 1X, F5.2)
  END IF !alive
 END IF !year and output desired
END IF !flag 11

IF (outflag .EQ. 12) THEN  !output dead indiv attributes (file 120) !12_12_12_12_12_12_12_12
 cc = i_col(indiv); rr = j_row(indiv) !store column and row as local shorthand
 WRITE (120, 2203) year, jday, cumday, hour, cumhr, indiv, spposn(xspecies(indiv)), xspecies(indiv), xageyrs(indiv), &
   xagedays(indiv), xworth(indiv), xalive(indiv), xstage(indiv), xcmax(indiv), totcons(indiv)/xcmax(indiv), &
   xsomwght(indiv), xsomchng(indiv), xgonwght(indiv), xgonchng(indiv), xtotwght(indiv), &
   xtotchng(indiv), xlen(indiv), xlenchng(indiv), xmort(indiv), xmat(indiv), xspwnday(indiv), xfecflag(indiv), &
   xbreedsom(indiv), xtrgtinvst(indiv), xreprdinvst(indiv), xfec(indiv)/REAL(batches(spposn(xspecies(indiv)))), xbatchwght(indiv), &
   xeggprdn(indiv), xbtchcnt(indiv), cc, rr, x_dist(indiv), y_dist(indiv), waterdepth(cc,rr), habitat(cc,rr)
 2203 FORMAT (1X, I3, 1X, I3, 4X, I4, 3X, I2, 1X, I6, 1X, I5, 5X, I1, 5X, I1, 6X, I2, &
		   4X, I4, 1X, ES10.3, 5X, I1, 5X, I1, 1X, ES10.3, 1X, F6.4, &
		   1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
		   1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 6X, I1, 8X, I3, 8X, I1, &
		   1X, ES10.3, 1X, ES10.3, 3X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
		   1X, ES10.3, 7X, I2, 1X, I3, 1X, I3, 1X, F6.2, 1X, F6.2, 1X, F5.2, 7X, I1)
END IF !flag 12

IF (outflag .EQ. 13) THEN  !output move info for up to 5 indivs (file 130) !13_13_13_13_13_13
 DO z = 1, 5                       !loop over id list
  IF (indiv .EQ. trackid(z)) THEN  !if indiv is on list of id's to track
   cc = i_col(indiv); rr = j_row(indiv) !store column and row as local shorthand
   WRITE (130, 3304) year, jday, cumday, hour, cumhr, timesteps, &
	 j, indiv, fwsp, xspecies(indiv), xageyrs(indiv), xagedays(indiv), &
	 cc, rr, x_dist(indiv), y_dist(indiv), waterdepth(cc,rr), xmindep(indiv), habitat(cc,rr), &
	 xlen(indiv), nhood(fwsp), nhdflag, maxfit, neerest, &
	 deepest, newcol, newrow
   3304 FORMAT (1X, I3, 1X, I3, 4X, I4, 3X, I2, 1X, I6, 7X, I3, &
	 6X, I3, 1X, I5, 5X, I1, 5X, I1, 7X, I1, 4X, I4, &
	 1X, I3, 1X, I3, 1X, F6.2, 1X, F6.2, 1X, F5.2, 5X, F5.2, 3X, I1, &
	 1X, ES10.3, 4X, I2, 8X, I1, 1X, ES10.3, 1X, F7.3, &
	 3X, F5.2, 6X, I3, 6X, I3)
  END IF !indiv to be output
 END DO !list of indivs to output
END IF !flag 13

IF (outflag .EQ. 14 &  !output mean-by-stage (file 140) !14_14_14_14_14_14_14_14_14_14_14_14
  .AND. hour .EQ. nhours) THEN                                       !but only if last hour of day
 IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0) THEN !and limit to MOD = 0
  WRITE (140, 4405) year, cumday, hour, i, speciesid(i), &      !where i = fwsp
	agemeanp(i,1), agemeanp(i,2), agemeanp(i,3), &
	agemeanlen(i,1), agemeanlen(i,2), agemeanlen(i,3), maxlen(i), &
	agemeanwgt(i,1), agemeanwgt(i,2), agemeanwgt(i,3)
  4405 FORMAT (1X, I3, 3X, I5, 3X, I2, 5X, I1, 5X, I1, 1X, &
	ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, &
	ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, &
	ES10.3, 1X, ES10.3, 1X, ES10.3)
 END IF !output limits
END IF !flag 14

IF (outflag .EQ. 15) THEN !output indiv diets (file 150) !15_15_15_15_15_15_15_15_15_15_15_15
 IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 &     !but limit to MOD = 0
   .AND. MOD(indiv,iswindivout) .EQ. 0) THEN
  WRITE (150, 5506) year, jday, cumday, hour, cumhr, indiv, &
	fwsp, xspecies(indiv), xstage(indiv), xworth(indiv), xtotwght(indiv), xlen(indiv), &
	dietbase(indiv,1), dietbase(indiv,2), dietbase(indiv,3), dietbase(indiv,4), dietbase(indiv,5)
  5506 FORMAT (1X, I3, 1X, I3, 4X, I4, 3X, I2, 1X, I6, 1X, I5, &
	5X, I1, 5X, I1, 5X, I1, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
	1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3)
 END IF !output limits
END IF !flag 15

IF (outflag .EQ. 16) THEN  !output prey-by-cell (file 160) !16_16_16_16_16_16_16_16_16_16_16
  WRITE (160, 6607) year, jday, hour, xx, ii, jj, habitat(ii,jj), &  !where xx, ii, jj = prey type, col, row
	unitpreynum(xx,ii,jj), unitpreybmss(xx,ii,jj)
  6607 FORMAT (1X, I3, 1X, I3, 3X, I2, 8X, I1, 1X, I3, 1X, I3, 7X, I1, &
	1X, ES10.3, 1X, ES10.3)
END IF !flag 16

IF (outflag .EQ. 17) THEN  !output mean prey by habitat (file 170) !17_17_17_17_17_17_17_17_17
  WRITE (170, 7708) year, jday, hour, xx, yy, &  !where xx and yy = prey type, hab type
	aveunitnum(yy,xx), aveunitbmss(yy,xx)
  7708 FORMAT (1X, I3, 1X, I3, 3X, I2, 8X, I1, 14X, I1, &
	2X, ES10.3, 2X, ES10.3)
END IF !flag 17

IF (outflag .EQ. 18) THEN  !output life table (file 180) !18_18_18_18_18_18_18_18_18_18_18_18
 IF (j .GE. ageclasses(i)+3 .OR. j .GE. year+4 .OR. & !if stage exceeds max stage for sp OR too early in sim for legit stage data
   (speciesid(i) .EQ. 1 .AND. j .EQ. 2)) THEN         !OR if shrimp AND yolk stage (which shrimp d.n. have)
  WRITE (180, 8809) year, i, speciesid(i), j, 0.D0, & !output zeros (i and j = food web sp, stage (egg, yolk, feed + indiv year ages))
	0.D0, 0.D0, 0.D0, 0.D0
 ELSE !otherwise, j <= age classes OR not shrimp yolk
  IF (lftbloutflag(j,i) .EQ. 1) THEN                  !if at least one cohort or indiv left stage
   lftbldurn(j,i) = lftbldurn(j,i) / lftblout(j,i)    !calc mean stage duration
   zmort = (-LOG(lftblout(j,i) / lftblin(j,i))) / lftbldurn(j,i) !calc zmort
  ELSE                                                !otherwise, complete mortality (all worth lost)
   lftbldurn(j,i) = 0.D0                              !mean stage duration = 0
   zmort = 999.D0                                     !set zmort to an arbitrarily large number
  END IF !stage out flag
  WRITE (180, 8809) year, i, speciesid(i), j, lftbldurn(j,i), & !where i and j = food web sp, stage (egg, yolk, feed + indiv year ages)
	lftblin(j,i), lftblout(j,i), lftblout(j,i)/lftblin(j,i), zmort
 END IF !age classes or yolk shrimp
 8809 FORMAT (1X, I3, 5X, I1, 5X, I1, 4X, I2, 1X, ES10.3, &
   1X, ES10.3, 1X, ES10.3, 2X, ES10.3, 1X, ES10.3)
END IF !flag 18

IF (outflag .EQ. 19 .AND. hour .EQ. nhours) THEN !output mean diet at end of day (prop'n by prey type; file 190) !19_19_19
 IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0) THEN !and limit to MOD = 0
  sp = speciesid(i)                                 !store species id of fwsp i as local shorthand
  IF (SUM(avediet(sp,:)) .LE. 0.D0) THEN            !if indiv did not consume prey
   WRITE (190, 9910) year, cumday, hour, i, sp, 0.D0, &
	0.D0, 0.D0, 0.D0, 0.D0, &
	 0.D0, 0.D0
   9910 FORMAT (1X, I3, 3X, I5, 3X, I2, 5X, I1, 5X, I1, 1X, ES10.3, &
	 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
	 1X, ES10.3, 1X, ES10.3)
  ELSE                                              !otherwise, indiv consumed at least one prey type
   WRITE (190, 9911) year, cumday, hour, i, sp, SUM(avediet(sp,:)), &
	 avediet(sp,1)/SUM(avediet(sp,:)),avediet(sp,2)/SUM(avediet(sp,:)),avediet(sp,3)/SUM(avediet(sp,:)), &
	 avediet(sp,4)/SUM(avediet(sp,:)),avediet(sp,5)/SUM(avediet(sp,:))
   9911 FORMAT (1X, I3, 3X, I5, 3X, I2, 5X, I1, 5X, I1, 1X, ES10.3, &
	 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
	 1X, ES10.3, 1X, ES10.3)
  END IF !diet
 END IF !output limits
END IF !flag 19

IF (outflag .EQ. 20) THEN !output distribn of abund by sp and hab type (file 200) !20_20_20_20
 IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 &  !and limit to MOD = 0
   .AND. MOD(hour,iswhourout) .EQ. 0) THEN
   IF (hour .EQ. nhours) THEN
  WRITE (200, 2223) year, cumday, hour, i, speciesid(i), SUM(waterdepth)/REAL(cellnumber), &
	habdist(i,1)/SUM(habdist(i,:)), habdist(i,2)/SUM(habdist(i,:)), habdist(i,3)/SUM(habdist(i,:)), &
	habdist(i,4)/SUM(habdist(i,:)), habdist(i,5)/SUM(habdist(i,:)), habdist(i,6)/SUM(habdist(i,:))
  2223 FORMAT (1X, I3, 3X, I5, 3X, I2, 5X, I1, 5X, I1, 1X, ES10.3, &
	1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
	1X, ES10.3, 1X, ES10.3, 1X, ES10.3)
  END IF
 END IF !output limits
END IF !flag 20

IF (outflag .EQ. 21) THEN !output highest density (worth) on grid by sp (file 210) !21_21_21_21
 IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0 & !but limit to MOD = 0
   .AND. MOD(hour,iswhourout) .EQ. 0 .AND. MOD(indiv,iswindivout) .EQ. 0) THEN
  area = REAL(cellsize*cellsize)                          !store cell area as local shorthand
  c = maxcoord(i,1); r = maxcoord(i,2)
  IF (c .GT. 0 .AND. r .GT. 0) THEN !if col and row data available (i.e., if sp i produced at least one recruit)
   WRITE (210, 2121) year, jday, hour, i, speciesid(i), c, r, &
	 waterdepth(c,r), indivbycell(i,c,r), worthbycell(i,c,r)/area, bmassbycell(i,c,r)/area
   2121 FORMAT (1X, I3, 1X, I3, 3X, I2, 5X, I1, 3X, I3, 1X, I3, 1X, I3, &
	 1X, F5.2, 2X, I5, 1X, ES10.3, 1X, ES10.3)
  END IF !c and r
 END IF !output limits
END IF !flag 21

IF (outflag .EQ. 22 .AND. hour .EQ. nhours) THEN !output daily worth lost to nat mort, starvation, stranding, predation, senescence (file 220) !22_22_22
 !IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0) THEN !and limit to MOD = 0
   WRITE (220, 2202) year, jday, hour, i, speciesid(i), mortloss(i,1), mortloss(i,2), &
	 mortloss(i,3), mortloss(i,4), mortloss(i,5)
   2202 FORMAT (1X, I3, 1X, I3, 3X, I2, 5X, I1, 3X, I3, 1X, ES10.3, 1X, ES10.3, &
	 1X, ES10.3, 1X, ES10.3, 1X, ES10.3)
 !END IF !output limits
END IF !flag 22

IF (outflag .EQ. 23) THEN !output daily worth lost to nat mort, starvation, stranding, predation, senescence (file 220) !22_22_22
 !IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0) THEN !and limit to MOD = 0
   WRITE (230, 2302) year, jday, hour, i, profilingtime
   2302 FORMAT (1X, I3, 1X, I3, 3X, I2, 5X, I2, 5X, ES10.3)
 !END IF !output limits
END IF !flag 22

		IF (outflag .EQ. 24) THEN !output cummulative annaul net change in productivity by species and cell (file 240) !23_23_23
		IF (MOD(year,iswyearout) .EQ. 0) THEN !and limit to MOD = 0
			DO c = 1, cols !loop over columns
				DO r = 1, rows !loop over rows
					WRITE (240, 2402) year, i, c, r, netproductivity(i,c,r), productivityvalues(i,c,r)
2402                FORMAT (1X, I3, 5X, I1, 1X, I3, 1X, I3, 1X, ES10.3, 7X, ES10.3)
				END DO !rows
			END DO !columns
		END IF !output limits
	END IF !flag 23
	IF (outflag .EQ. 25) THEN !output cummulative annaul net change in productivity by species and cell (file 250) !25_25_25
		IF (MOD(year,iswyearout) .EQ. 0) THEN !and limit to MOD = 0
			DO c = 1, cols !loop over columns
			    DO r = 1, rows !loop over rows
					WRITE (250, 2502) year, c,r, inund(c, r), drytowet(c, r), wettodry(c,r)
2502                FORMAT (3X, I1, 1X, I3, 1X, I3, 2X, I4, 5X, I4, 5X, I4)
				END DO !rows
			END DO !columns
		END IF !output limits
	END IF !flag 23
	IF (outflag .EQ. 26) THEN !output cummulative annaul net change in productivity by species and cell (file 260) !23_23_23
		IF (MOD(year,iswyearout) .EQ. 0) THEN !and limit to MOD = 0
			WRITE (260, 2602) year, i, speciesid(i), annualbmass(i), speciesspawnloss(i), &
			sumaverageweightdifference(i), sumaverageweightandworthdifference(i), speciesweightdiff(i), growthproductivity(i), &
			trophictransfer%withworth%starvation%notsenescent(i), trophictransfer%withworth%starvation%senescent(i), trophictransfer%withworth%stranding%notsenescent(i), trophictransfer%withworth%stranding%senescent(i), trophictransfer%withworth%naturalmortality%notsenescent(i), trophictransfer%withworth%naturalmortality%senescent(i), countlivingindividuals(i), speciesweightdiffnumber(i) 
!			WRITE (260, 2602) year, i, speciesid(i), annualbmass(i) productivityvalues(i,1), productivityvalues(i,3), productivityvalues(i,2), productivityvalues(i,4)
2602        FORMAT (3X, I1, 5X, I1, 5X, I1, 1X, ES10.3, 1X, ES10.3, 7X, ES10.3, 15X, ES10.3, 1X, ES10.3, 9X, ES10.3, 29X, ES10.3, 26X, ES10.3, 28X, ES10.3, 25X, ES10.3, 35X, ES10.3, 32X, ES10.3, 8X, I6, 9X, I6)
!2602        FORMAT (3X, I1, 5X, I1, 5X, I1, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 3X, ES10.3)
		END IF !output limits
	END IF !flag 23
	!DEC$ IF DEFINED(TRACE_INDIVIDUALS)
	IF (outflag .EQ. 27) THEN !output cummulative annaul net change in productivity by species and cell (file 260) !23_23_23
		IF (MOD(year,iswyearout) .EQ. 0) THEN !and limit to MOD = 0
		    IF(isselectedindividual(indiv) .EQ. 1) THEN
		        CALL checkandclearindividual(indiv)
			    WRITE (300, 3002) year, jday, cumday, hour, cumhr, indiv, fwsp, xspecies(indiv), xspawnloss(indiv), xbatchwght(indiv)
        3002    FORMAT (1X, I3, 1X, I3, 4X, I4, 3X, I2, 1X, I6, 1X, I5, 5X, I1, 5X, I1, 1X, ES10.3, 1X, ES10.3)
            END IF
		END IF !output limits
	END IF !flag 23
	!DEC$ ENDIF
	
CALL file_unlock(outflag-1)

END SUBROUTINE

!----------------------------------------------------------------------
SUBROUTINE inputecho  !echo all input / parameter values
USE time_etc
USE physicalenv
USE preybase
USE species
USE indivattributes
USE bioenergetics
USE movement
USE reproduction
IMPLICIT NONE
INTEGER vulnflag !=0 when no fwsp sp are vulnerable to predation, =1 when at least one is vuln
INTEGER z, a     !local indices for do loops
INTEGER i

WRITE (12, *) "OUTPUT CONTROL"
WRITE (12, *) "data output every nth hour, where n =", iswhourout
WRITE (12, *) "data output every nth day, where n =", iswdayout
WRITE (12, *) "data output every nth year, where n =", iswyearout
WRITE (12, *) "data output for species (0=all, 1=GS, 2=IS, 3=GK, 4=BA) =", isspeciesout
IF (SUM(fileflag(9:13)) .GT. 0) WRITE (12, *) "data output for every nth indiv, where n =", iswindivout
WRITE (12, *) ""
WRITE (12, *) "Output includes file(s)"
IF (fileflag(1) .EQ. 1) WRITE (12, *) " out_allocatedspace.out"
IF (fileflag(2) .EQ. 1) WRITE (12, *) " out_griddepth.m"
IF (fileflag(3) .EQ. 1) WRITE (12, *) " out_gridelevation.m"
IF (fileflag(4) .EQ. 1) WRITE (12, *) " out_gridfishbycell.out"
IF (fileflag(5) .EQ. 1) WRITE (12, *) " out_gridhabitat.m"
IF (fileflag(6) .EQ. 1) WRITE (12, *) " out_gridinundhrs.m"
IF (fileflag(7) .EQ. 1) WRITE (12, *) " out_gridpreybycell.out"
IF (fileflag(8) .EQ. 1) WRITE (12, *) " out_gridpreymeans.out"
IF (fileflag(9) .EQ. 1) WRITE (12, *) " out_indivs_added.out"
IF (fileflag(10) .EQ. 1) WRITE (12, *) " out_indivs_alive.out"
IF (fileflag(11) .EQ. 1) WRITE (12, *) " out_indivs_dead.out"
IF (fileflag(12) .EQ. 1) WRITE (12, *) " out_indivs_diet.out"
IF (fileflag(13) .EQ. 1) THEN
 WRITE (12, *) " out_indivs_move.out"
 DO i = 1, 5
  IF (trackid(i) .GT. 1) WRITE (12, *) "  for indiv", trackid(i)
 END DO
END IF
IF (fileflag(14) .EQ. 1) WRITE (12, *) " out_input_echo.out"
IF (fileflag(15) .EQ. 1) WRITE (12, *) " out_stage_abunds.out"
IF (fileflag(16) .EQ. 1) WRITE (12, *) " out_stage_means.out"
IF (fileflag(17) .EQ. 1) WRITE (12, *) " out_summary_diet.out"
IF (fileflag(18) .EQ. 1) WRITE (12, *) " out_summary_habdist.out"
IF (fileflag(19) .EQ. 1) WRITE (12, *) " out_summary_lifetable.out"
IF (fileflag(20) .EQ. 1) WRITE (12, *) " out_summary_maxden.out"
IF (fileflag(21) .EQ. 1) WRITE (12, *) " out_summary_worthlost.out"
IF (fileflag(22) .EQ. 1) WRITE (12, *) " out_summary_profiling.out"
IF (fileflag(23) .EQ. 1) WRITE (12, *) " out_gridproductivity.out"
IF (fileflag(24) .EQ. 1) WRITE (12, *) " out_waterprofile.out"
IF (fileflag(25) .EQ. 1) WRITE (12, *) " out_annualbiomass.out"
IF (fileflag(26) .EQ. 1) WRITE (12, *) " out_spawnloss.out"

WRITE (12, *) ""
WRITE (12, *) "TIME, ETC"
WRITE (12, *) "random number seed =", idum
WRITE (12, *) "number of years in a sim =", nyears
WRITE (12, *) "number of days in a year =", ndays
WRITE (12, *) "number of hours in a day =", nhours
WRITE (12, *) ""
WRITE (12, *) "GRID PARAMETERS"
WRITE (12, *) "length (m) of one side of square cell =", cellsize
WRITE (12, *) "max number of cols and rows in any interpolated DELFT grid =", delftcols, delftrows
WRITE (12, *) "DELT topo file (1=topo1, 2=topo2, 3=topo3) =", topo
WRITE (12, *) "minimum and maximum column numbers of sub-grid =", subgrid(1), subgrid(2)
WRITE (12, *) "minimum and maximum rows numbers of sub-grid =", subgrid(3), subgrid(4)
WRITE (12, *) "total number of columns and rows in sub-grid =", cols, rows
WRITE (12, *) "total number of cells in sub-grid =", cellnumber
WRITE (12, *) "sub-grid area (m^2) =", cellsize*cellsize*cellnumber
WRITE (12, *) "sub-grid area (km^2) =", cellsize*cellsize*cellnumber/REAL(1000000.D0)
WRITE (12, *) "sub-grid area (ha) =", (cellsize*cellsize*cellnumber)/REAL(10000.D0)
WRITE (12, *) ""
WRITE (12, *) "HABITAT PARAMETERS (1=deep water, 2=SAV, 3=lo_emergent, 4=hi emergent, 5=woody, 6=bare (unvegetated marsh))"
WRITE (12, *) "water level that defines marsh edge (m) =", zerolevel
WRITE (12, *) "added water so that marsh sp can access marsh (m) =", addwater
WRITE (12, *) "inund threshold above which hab type is 1 or 2 =", inundmax
WRITE (12, *) "number of habitats =", habtype
WRITE (12, *) "probty below which occurrence of hab type assumed zero =", minprob
WRITE (12, *) "max mean water depth at which SAV occur (m) =", maxSAV
DO z = 1, habtype
 WRITE (12, *) "parameters a-e of the beta distribn of hab type", z
 WRITE (12, *) pdfa(z)
 WRITE (12, *) pdfb(z)
 WRITE (12, *) pdfc(z)
 WRITE (12, *) pdfd(z)
 WRITE (12, *) pdfe(z)
END DO
WRITE (12, *) ""
WRITE (12, *) "PREY PARAMETERS (1=small benth, 2=medium benth, 3=large benth, 4=small zoop, 5=large zoop)"
WRITE (12, *) "number of prey types =", preytypes
DO z = 1, preytypes
 WRITE (12, *) "PARAMETERS FOR PREY TYPE", z
 WRITE (12, *) "indiv wght (g wwt) =", indpreywght(z)
 WRITE (12, *) "energy density (J/ g wwt) =", preyenerden(z)
 IF (z .LE. 3) WRITE (12, *) "init number (#/m^2) =", initpreynum(z)
 IF (z .GE. 4) WRITE (12, *) "init number (#/m^3) =", initpreynum(z)
 WRITE (12, *) "turnover rate parameters a-d:"
 WRITE (12, *) preya(z)
 WRITE (12, *) preyb(z)
 WRITE (12, *) preyc(z)
 WRITE (12, *) preyd(z)
 WRITE (12, *) "hab multiplier of eqbm den (init number):"
 DO a = 1, habtype
  WRITE (12, *) preymult(z,a)
 END DO
END DO
WRITE (12, *) ""
WRITE (12, *) "SPECIES PARAMETERS"
WRITE (12, *) "number of sp in sp pool =", speciespool
WRITE (12, *) "number of food web sp =", nfwsp
WRITE (12, *) "maximum age across all sp =", maxacctage
WRITE (12, *) "total number of model individuals =", totindivs
WRITE (12, *) "total number of model indivs allowed in a cell =", cellmax
WRITE (12, *) "feeding hours (min, max) =", minfeedhr, maxfeedhr
WRITE (12, *) "calorific coeff (energy conv rate, J/g O2) =", calorcoeff
WRITE (12, *) "minimum worth threshold below which indiv dies =", minworth
DO z = 1, nfwsp
 WRITE (12, *) "PARAMETERS FOR EACH FOOD WEB SPECIES", z
 WRITE (12, *) "species id", speciesid(z)
 WRITE (12, *) "posn of sp in food web array =", spposn(speciesid(z))
 WRITE (12, *) "number of age classes =", ageclasses(z)
 WRITE (12, *) "number of model indivs in ea age class =", ageclassspace(z)
 WRITE (12, *) "population size of ea age class =", ageclasspopsize(z)
 WRITE (12, *) "total number of model indivs =", indivs(z)
 WRITE (12, *) "egg mortality (/hour) =", earlymort(1,z)
 WRITE (12, *) "yolk mortality (/hour) =", earlymort(2,z)
 WRITE (12, *) "feed mortality (/hour) =", earlymort(3,z)
 WRITE (12, *) "hab-specific multipliers of nat mort (for movement):"
 DO a = 1, habtype
  WRITE (12, *) mortmult(z,a)
 END DO
 WRITE (12, *) "initialization hab type =", inithab(z)
 WRITE (12, *) "cmax scalar =", cmaxscalar(z)
 WRITE (12, *) "cmax exponent =", cmaxexpo(z)
 WRITE (12, *) "cmax optimum temperature (oC) =", cmaxoptt(z)
 WRITE (12, *) "cmax maximum temperature (oC) =", cmaxmaxt(z)
 WRITE (12, *) "cmax theta =", cmaxtheta(z)
 WRITE (12, *) "prop'n of cmax (if force-feeding) =", pvalue(z)
 WRITE (12, *) "vulnerability of benth (1-3) and zoop (4,5):"
 DO a = 1, preytypes
  WRITE (12, *) preyv(z,a)
 END DO
 WRITE (12, *) "half saturation constants for vuln benth and zoop:"
 DO a = 1, preytypes
  IF (preyv(z,a) .EQ. 1) WRITE (12, *) preyk(z,a)
 END DO
 WRITE (12, *) "egestion scalar =", egscalar(z)
 WRITE (12, *) "excretion scalar =", exscalar(z)
 WRITE (12, *) "sda =", sdascalar(z)
 WRITE (12, *) "respiration scalar =", respscalar(z)
 WRITE (12, *) "respiration expo =", respexpo(z)
 WRITE (12, *) "respiration optimum temperature (oC) =", respoptt(z)
 WRITE (12, *) "respiration maximum temperatuer (oC) =", respmaxt(z)
 WRITE (12, *) "respiration theta =", resptheta(z)
 WRITE (12, *) "activity constant =", act(z)
 WRITE (12, *) "species energy density (Joules/g wwt) =", predenerden(z)
 WRITE (12, *) "initialization wght for all age classes (g wwt):"
 DO a = 1, ageclasses(z)
  WRITE (12, *) initwt(z,a)
 END DO
 WRITE (12, *) "starting wght of metamorphs (feed larv, g wwt) =",  metmorphwt(z)
 WRITE (12, *) "scalar of length-wght relation =", lw_scalar(z)
 WRITE (12, *) "exponent of length-wght relation =", lw_expo(z)
 WRITE (12, *) "lenght-at-maturity threshold (mm) =", matlen(z)
 WRITE (12, *) "breeder type =", breedtype(z)
 WRITE (12, *) "minimum spawning jday =", minspwnday(z)
 WRITE (12, *) "maximum spawning jday =", maxspwnday(z)
 WRITE (12, *) "parameter a of fecund-tot wght relation =", feca(z)
 WRITE (12, *) "parameter b of fecund-tot wght relation =", fecb(z)
 WRITE (12, *) "parameter c of fecund-tot wght relation =", fecc(z)
 WRITE (12, *) "number of batches =", batches(z)
 WRITE (12, *) "egg weight (g wwt) =", eggwght(z)
 WRITE (12, *) "parameter 'Da' of fractional development for eggs, yolk, feed larv:"
 DO a = 1, 3
  WRITE (12, *) fracta(z,a)
 END DO
 WRITE (12, *) "parameter 'Dc' of fractional development for eggs, yolk, feed larv:"
 DO a = 1, 3
  WRITE (12, *) fractc(z,a)
 END DO
 WRITE (12, *) "neighborhood (cell radius) under fitness movement =", nhood(z)
 WRITE (12, *) "multiplier of neighborhood for emergency movement =", emermult(z)
 WRITE (12, *) "error rate in fitness calc (0.01=1%) =", fiterror(z)
END DO !loop over fwsp
END SUBROUTINE

SUBROUTINE countsenescentindividuals()
USE indivattributes
USE species
INTEGER i
senescentcount=0
senescentvalue=0.D0
DO i=1, totindivs
	IF (xsensce(i)) THEN
		senescentcount(spposn(xspecies(i)))=senescentcount(spposn(xspecies(i)))+1
		senescentvalue(spposn(xspecies(i)))=senescentvalue(spposn(xspecies(i)))+xworth(i)
	END IF
END DO
END SUBROUTINE

!-FUNCTIONS------------------------------------------------------------
!----------------------------------------------------------------------
!----------------------------------------------------------------------
FUNCTION pdf(ind,a,b,c,d,e) !calculate Beta probability of a veg type given cell inund (hours)
USE time_etc                !and hab-specific coefficients (calculation is done in 6 parts)
USE physicalenv
IMPLICIT NONE
INTEGER ind                         !inundation (hours)
REAL(8) pdf                         !Beta probability
REAL(8) a, b, c, d, e               !Beta coefficients (equivalent to pdfa, pdfb, etc.)
REAL(8) part1, part2, part3, part4, part5  !parts of equation for performing calculation

part1 = (c * (d - 1)) / (d + e - 2)
part2 = a * ((REAL(ind) - b + part1) / c) ** (d - 1)
part3 = (1 - (REAL(ind) - b + part1) / c) ** (e - 1)
part4 = ((d - 1) / (d + e - 2)) ** (d - 1)
part5 = ((e - 1) / (d + e - 2)) ** (e - 1)

pdf = (part2 * part3) / (part4 * part5)

END FUNCTION

!----------------------------------------------------------------------
FUNCTION length(weight,scalar,expo) !convert som wght (g wwt) to length (mm)
USE indivattributes
IMPLICIT NONE
REAL(8) length, weight, scalar, expo  !length (mm), som wght (g wwt), conversion parameters

length = EXP((LOG(weight) - LOG(scalar)) / expo)

END FUNCTION

FUNCTION inundationtime()
USE physicalenv
USE time_etc
INTEGER inundationtime,i
inundationtime=0
DO i=1, inundationtimestamps
	IF ((jday .EQ. inundationdays(i)).AND. (hour .EQ. inundationhours(i))) THEN
		inundationtime =1
	END IF
END DO
END FUNCTION


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


INCLUDE 'readparameters.f90'

