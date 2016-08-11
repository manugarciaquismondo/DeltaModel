!-----------------------------------------------------------------------
MODULE physicalenv      !the physical environment (grid, elevation, temperature, habitat, etc)
USE time_etc
IMPLICIT NONE

!GRID(S)
INTEGER:: cellsize = 2     ! size of one side of ea square cell (m)
INTEGER, PARAMETER :: delftcols = 2495 ! max number of columns in any interpolated DELFT grid
INTEGER, PARAMETER :: delftrows = 3745 ! max number of rows in any interpolated DELFT grid
INTEGER, PARAMETER :: topo = 2         !select DELFT 'topo' (i.e., elevation) input file (1=topo1(early), 2=topo2(middle), 3=topo3(late))
INTEGER, PARAMETER :: waterschedyrs = 9, numwaterprofiles=5         !select DELFT 'topo' (i.e., elevation) input file (1=topo1(early), 2=topo2(middle), 3=topo3(late))
!INTEGER, PARAMETER :: subgrid(4) = (/ 250, 1220, 820, 2600 /) ! topo1 full grid (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 250, 1750, 600, 2750 /) ! topo2 full grid (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 250, 1950, 700, 2750 /) ! topo3 full grid (1=min_col, 2=max_col, 3=min_row, 4=max_row)
INTEGER, PARAMETER :: subgrid(4) = (/ 570, 709, 1800, 1939 /) ! topo1 and topo2 main subgrid, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 557, 696, 1770, 1909 /) ! topo3 main subgrid, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 727, 866, 1932, 2071 /)  ! topo2 subgrid 1, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 885, 1024, 2065, 2204 /) ! topo2 subgrid 2, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 674, 813, 1625, 1764 /)  ! topo2 subgrid 3, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)
!INTEGER, PARAMETER :: subgrid(4) = (/ 778, 917, 1450, 1589 /)  ! topo2 subgrid 4, 140x140 cells (1=min_col, 2=max_col, 3=min_row, 4=max_row)


INTEGER, PARAMETER :: cols = subgrid(2)-subgrid(1)+1 ! total number of columns in sub-grid
INTEGER, PARAMETER :: rows = subgrid(4)-subgrid(3)+1 ! total number of rows in sub-grid
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
INTEGER :: starvationAndStranding
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
INTEGER, PARAMETER:: morttypes = 5
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
REAL(8) mortloss(nfwsp,morttypes)              ! total sp-specific worth lost to nat mort, starvation, stranding, predation and senescence ea day
REAL(8) cummortloss(morttypes)                 ! total worth lost to nat mort, starvation, stranding, predation and senescence ea year
INTEGER spwndayscounter(nfwsp)
INTEGER longindivscounter(nfwsp)
INTEGER aliveindivs(nfwsp)
INTEGER senescentcount(nfwsp)
REAL(8) senescentvalue(nfwsp)
INTEGER notcalculatedspawningdays(nfwsp)
REAL(8) netproductivity(nfwsp,cols,rows) !net productivity (g wwt = weight gained or lost * worth) of each species in each cell
!REAL(8) productivityvalues(nfwsp,morttypes-1)

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

INTEGER assignflag             ! a flag for assigning attributes in different ways. See subroutine 'assingattributes'
							   ! (1=initialize, 2=recruit, 3=split worth, 4=zero old)

!$OMP THREADPRIVATE(mortflag)
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