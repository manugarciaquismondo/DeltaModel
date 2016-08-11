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
!  IF (MOD(jday, 30) .EQ. 15) THEN
  IF (MOD(xagedays(indiv), 30) .EQ. 15) THEN
  WRITE (22, 223) year, jday, cumday, hour, cumhr, indiv, fwsp, xspecies(indiv), xageyrs(indiv), &
	xagedays(indiv), xworth(indiv), xalive(indiv), xstage(indiv), xcmax(indiv), totcons(indiv)/xcmax(indiv), &
	xsomwght(indiv), xsomchng(indiv), xgonwght(indiv), xgonchng(indiv), xtotwght(indiv), &
	xtotchng(indiv), xlen(indiv), xlenchng(indiv), xmort(indiv), xmat(indiv), xspwnday(indiv), xfecflag(indiv), &
	xbreedsom(indiv), xtrgtinvst(indiv), xreprdinvst(indiv), xfec(indiv)/REAL(batches(spposn(xspecies(indiv)))), xbatchwght(indiv), &
	xeggprdn(indiv), xbtchcnt(indiv), cc, rr, x_dist(indiv), y_dist(indiv), waterdepth(cc,rr), habitat(cc,rr)
  223 FORMAT (1X, I3, 1X, I3, 4X, I4, 3X, I2, 1X, I6, 1X, I5, 5X, I1, 5X, I1, 6X, I2, &
			4X, I4, 1X, ES10.3, 5X, I1, 5X, I1, 1X, ES10.3, 1X, F6.4, &
			1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
			1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 6X, I1, 8X, I3, 8X, I1, &
			1X, ES10.3, 1X, ES10.3, 3X, ES10.3, 1X, ES10.3, 1X, ES10.3, &
			1X, ES10.3, 7X, I2, 1X, I3, 1X, I3, 1X, F6.2, 1X, F6.2, 1X, F5.2, 7X, I1)
	END IF
!  END IF
 END IF !output limits
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

IF (outflag .EQ. 23) THEN !output daily worth lost to nat mort, starvation, stranding, predation, senescence (file 230) !22_22_22
 !IF (MOD(year,iswyearout) .EQ. 0 .AND. MOD(jday,iswdayout) .EQ. 0) THEN !and limit to MOD = 0
   WRITE (230, 2302) year, jday, hour, i, profilingtime
   2302 FORMAT (1X, I3, 1X, I3, 3X, I2, 5X, I2, 5X, ES10.3)
 !END IF !output limits
END IF !flag 22

	IF (outflag .EQ. 24) THEN !output cummulative annaul net change in productivity by species and cell (file 240) !23_23_23
		IF (MOD(year,iswyearout) .EQ. 0) THEN !and limit to MOD = 0
			DO c = 1, cols !loop over columns
				DO r = 1, rows !loop over rows
					WRITE (240, 2402) year, i, c, r, netproductivity(i,c,r)
2402                FORMAT (1X, I3, 5X, I1, 1X, I3, 1X, I3, 1X, ES10.3)
				END DO !rows
			END DO !columns
		END IF !output limits
	END IF !flag 23
	IF (outflag .EQ. 25) THEN !output cummulative annaul dry to wet (file 250) !25_25_25
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
			WRITE (260, 2602) year, i, speciesid(i), annualbmass(i)
!			WRITE (260, 2602) year, i, speciesid(i), annualbmass(i) productivityvalues(i,1), productivityvalues(i,3), productivityvalues(i,2), productivityvalues(i,4)
2602        FORMAT (3X, I1, 5X, I1, 5X, I1, 1X, ES10.3)
!2602        FORMAT (3X, I1, 5X, I1, 5X, I1, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 1X, ES10.3, 3X, ES10.3)
		END IF !output limits
	END IF !flag 23
	
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
