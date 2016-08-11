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