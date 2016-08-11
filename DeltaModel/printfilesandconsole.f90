PRINT "(A,I1,A)"," Execution using ",num_threads," processors."
narg=COMMAND_ARGUMENT_COUNT()
nyears=2
argCounter=1
IF (narg .ge. argCounter) THEN
	call GET_COMMAND_ARGUMENT(argCounter, fileRoute)
	fileRoute= fileRoute(:LEN_TRIM(fileRoute))
	argCounter=argCounter+1
	if(narg .ge. argCounter) then
	    call GET_COMMAND_ARGUMENT(argCounter, waterDir)
	    waterDir= waterDir(:LEN_TRIM(waterDir))
	    waterDir=trim(waterDir)//'/'
	    argCounter=argCounter+1
	    if(narg .ge. argCounter) then
	        call GET_COMMAND_ARGUMENT(argCounter, outputDir)
	        outputDir= outputDir(:LEN_TRIM(outputDir))
	        outputDir=trim(outputDir)//'/'
	        argCounter=argCounter+1
	        if(narg .ge. argCounter) then
	            call GET_COMMAND_ARGUMENT(argCounter, inputYear)
	            read(inputYear,*) nyears
	            argCounter=argCounter+1
	            if(narg .ge. argCounter) then
	                call GET_COMMAND_ARGUMENT(argCounter, addStarvationAndStranding)
	                read(addStarvationAndStranding,*) starvationAndStranding
	                 argCounter=argCounter+1
	            end if
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
fileflag(10) = 1 !do (1) or do not (0) output 'out_indivs_alive.out' (file 22, traits of alive indivs)
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
fileflag(22) = 1 !do (1) or do not (0) output 'out_summary_profiling.out' (file 230, worth lost by sp to nat mort, starvation, stranding, predation)
fileflag(23) = 1 !do (1) or do not (0) output 'out_gridproductivity.out' (file 240, net productivity (g wwt = weight gained or lost *worth) by species and cell)
fileflag(24) = 1 !do (1) or do not (0) output 'out_waterprofile.out' (file 250, water profile (cell inundated or not))
fileflag(25) = 1 !do (1) or do not (0) output 'out_annualbiomass.out' (file 260, annual biomass (summing of all biomass per species and cell throughout the year))

iswhourout = 24  !1 = every hour, 2 = every 2nd, 3 = 3rd, ..., nhours = last (daily)
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
OPEN (260, file = trim(outputDir)//'out_annualbiomass.out', STATUS = 'UNKNOWN')
!--write output headers------------------------------------------------
WRITE (22, 222)
222 FORMAT ('year', 1X, 'day', 1X, 'cum_day', 1X, 'hour', 1X, 'cum_hr', 1X, 'indiv', 1X, 'fw_sp', 1X, 'sp_id', 1X, 'age_yrs', 1X, &
			'age_dys', 6X, 'worth', 1X, 'alive', 1X, 'stage', 7X, 'cmax', 1X, 'p_cmax', 3X, &
			'som_wght', 3X, 'som_chng', 3X, 'gon_wght', 3X, 'gon_chng', 3X, 'tot_wght', 3X, &
			'tot_chng', 5X, 'length', 3X, 'len_chng', 7X, 'M/hr', 1X, 'mature', 1X, 'spawn_jday', 1X, 'fec_flag', 2X, &
			'breed_som', 1X, 'trgt_invst', 1X, 'reprod_invst', 3X, 'btch_fec', 2X, 'btch_wght', 2X, &
			'egg_prodn', 1X, 'btch_cnt', 1X, 'col', 1X, 'row', 1X, 'x_dist', 1X, 'y_dist', 1X, 'depth', 1X, 'habitat')
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
2401 FORMAT ('year', 1X, 'fw_sp', 1X, 'col', 1X, 'row', 3X, 'net_prod')
WRITE (250, 2501)
2501 FORMAT ('year', 1X, 'col', 1X, 'row', 1X 'depth', 1X, 'drytowet', 1X, 'wettodry')
WRITE (260, 2601)
!2601 FORMAT ('year', 1X, 'fw_sp', 1X, 'sp_id', 4X, 'biomass', 4X, 'natural', 2X, 'stranding', 1X, 'starvation', 1X, 'weightchange')
2601 FORMAT ('year', 1X, 'fw_sp', 1X, 'sp_id', 4X, 'biomass')