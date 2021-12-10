*** 3. Additional plots Quantification 2021

	** append data from all the runs 
	cd "/Users/hannahkoenker/Dropbox/R Directory/Quantification"

	clear
	
	foreach x of numlist 105/107 {
		append using "output/runs/`x'"
	}
	
	foreach x of numlist 207/230 {
		append using "output/runs/`x'"
	}
	foreach x of numlist 307/330 {
		append using "output/runs/`x'"
	}
	foreach x of numlist 410/420 {
		append using "output/runs/`x'"
	}
	foreach x of numlist 510/520 {
		append using "output/runs/`x'"
	}
	
	label var year "year"
	label var scenario "scenario"
	
	drop crop* npclb npcub accrkub acc_npclb accub_npclb acclb_npcub acc_npcub npclbcentile acclb_npclb npcubcentile k accrklb
	drop if iso3==""
	
	gen group=floor(scenario/100)
	label var group "ITN strategy"
	
	
	encode iso3, gen(cty)
	
	kountry iso3, from(iso3c) to(iso2c)
	rename _ISO2C_ iso_a2 
	
	* twoway scatter accrk percpop, by(group) ms(oh) mcolor(%50)
	
	* twoway scatter accrk percpop if accrk>70, by(group, ixaxes note("")) ms(oh) mcolor(%50) xlabel(0(10)100)
	
	* twoway scatter accrk percpop if accrk>70 & scenario==207 & year>2021, ms(oh) mcolor(%50) xlabel(0(10)100) || scatter accrk percpop if accrk>70 & scenario==225 & year>2021,  ms(oh) mcolor(%50)
	
	save output/quant_runs, replace 
	
	
	* li iso_a2 percpop if accrk>70 & iso3!=""
	
	* su percpop if group==2 & iso3=="TZA" & accrk>70 & year>2021, d
	
	* bysort iso_a2: su percpop if group==2 & accrk>70 & year>2021
	
	** We want to identify which NPP are associated with targeted access of 80-85%, and 90-95% (to obtain 80% use)
		bysort iso_a2: su percpop if group==2 & accrk>=80 & accrk<=85
		
		bysort iso_a2: su percpop if group==2 & accrk>=90 & accrk<=95

			/*
					bysort iso_a2: egen mymode=mode(percpop) if group==2 & year>2021 & accrk>60 & accrk<99, minmode
					bysort iso_a2: egen maxmode=mode(percpop) if group==2 & year>2021 & accrk>60 & accrk<99, maxmode

					bysort iso_a2: egen med2 = median(percpop) if group==2 & year>2021 & accrk>60 & accrk<99 
					bysort iso_a2: egen mean2 = mean(percpop) if group==2 & year>2021 & accrk>60 & accrk<99
				
					twoway scatter med2 mean2 
						
						twoway scatter percpop cty, xlabel(1(1)40, angle(45) labsize(tiny) valuelabel)

			*/	
			** they are similar, let's use the mean
			* collapse (median) percpop if group==2 & accrk>60 & year>2021, by(iso_a2)
	
	*** SCENARIO 2 RECOMMENDED QUANTIFIERS
	
	** lowest of most frequent scenario where band criteria are met 
	tab scenario if iso3=="AGO" & accrk>=80 & accrk<=85 & group==2 // some countries, incl CAM, don't fall in this band! only 31 countries.
		tab scenario if iso3=="LBR" & accrk>=80 & accrk<=85 & group==2 // some countries, incl CAM, don't fall in this band! only 31 countries.
		
		
			*** Max accrk that can be reached with Scenario 2 in each country:
			
				preserve 
					collapse (max) accrk if group==2 & year>2021, by(iso_a2 scenario)
					bysort iso_a2: egen maxacc = max(accrk)
					collapse max, by(iso_a2)
					save data/s2_max_acc, replace
				restore 
				
		
			* target of 70	
		preserve 
			collapse (count) group if group==2 & accrk>=70 & accrk<80 & year>2021, by(iso_a2 scenario)
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max
			collapse q, by(iso_a2)
			replace q=q-200
			gen target=70
			
			tempfile s2t70
			save "`s2t70'", replace 
		restore
		
			* target of 80
		preserve 
			collapse (count) group if group==2 & accrk>=80 & accrk<90 & year>2021, by(iso_a2 scenario)
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max
			collapse q, by(iso_a2)
			replace q=q-200
			gen target=80
			
			tempfile s2t80
			save "`s2t80'", replace 
		restore
		
			* target of 90
		preserve
			collapse (count) group if group==2 & accrk>=90 & accrk<100 & year>2021, by(iso_a2 scenario)
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max
			collapse q, by(iso_a2)
			replace q=q-200
			gen target=90
			
			append using "`s2t80'"
			append using "`s2t70'"
			save data/s2_min_npp, replace 
		restore
	
	** Mean NPP within the targeted band 
	
		* collapse band of 70% 
			preserve
				collapse (mean) percpop if group==2 & accrk>=70 & accrk<80 & year>2021, by(iso_a2)
				*encode iso_a2, gen(cty)
				gen target=70
				rename percpop q
				replace q=floor(q)-6
				
				tempfile target70
				save "`target70'", replace
				
			restore
			
		* collapse band of 90% 
			preserve
				collapse (mean) percpop if group==2 & accrk>=90 & accrk<=95 & year>2021, by(iso_a2)
				*encode iso_a2, gen(cty)
				gen target=90
				rename percpop q
				replace q=floor(q)-6
				
				tempfile target90
				save "`target90'", replace
				
			restore
		
		* collapse band of 80% and append to 90
			preserve
				collapse (mean) percpop if group==2 & accrk>=80 & accrk<=85 & year>2021, by(iso_a2)
				*encode iso_a2, gen(cty)
				gen target=80
				rename percpop q
				replace q=floor(q)-6

			append using "`target90'"
			append using "`target70'"
			save data/s2_mean_npp, replace
			export delimited using data/s2mean_npp.csv, replace 

			restore
	
	
	*** SCENARIO 3 RECOMMENDED QUANTIFIERS
	* should I be doing which scenario has the most accrk over 80?
	
	tab scenario if iso3=="AGO" & accrk>=80 & accrk<=85 & group==3 // some countries, incl CAM, don't fall in this band! only 31 countries.
		tab scenario if iso3=="LBR" & accrk>=80 & accrk<=85 & group==3 // some countries, incl CAM, don't fall in this band! only 31 countries.

	* then take the lowest scenario number from the highest frequency group 
	
		preserve
			collapse (count) group if group==3 & accrk>=70 & accrk<100 & year>2021 & percpop<61, by(iso_a2 scenario) // exclude years of the mass campaigns; band expanded to 70-100 for Scenario 3, as some btw years are always above 80
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max
			collapse q, by(iso_a2)
			replace q=q-300
			gen target=70
			
			tempfile s3t70
			save "`s3t70'", replace 
		restore
		
		preserve
			collapse (count) group if group==3 & accrk>=80 & accrk<90 & year>2021 & percpop<61, by(iso_a2 scenario)
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max
			collapse q, by(iso_a2)
			replace q=q-300
			gen target=80
			
			tempfile s3t80
			save "`s3t80'", replace 
		restore
		
		preserve
			collapse (count) group if group==3 & accrk>=90 & accrk<100 & year>2021 & percpop<61, by(iso_a2 scenario)
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max
			collapse q, by(iso_a2)
			replace q=q-300
			gen target=90
			
			append using "`s3t80'"
			append using "`s3t70'"
			save data/s3_min_npp, replace 
		restore
	
	
	
	*** Mean NPP within the band // should not exclude any countries
	
			* collapse band of 70% 
			preserve
				collapse (mean) percpop if group==3 & accrk>=70 & accrk<100 & year>2021 & percpop<61, by(iso_a2) // band expanded to 70-100 for Scenario 3, as some btw years are always above 80
			*	encode iso_a2, gen(cty)
				gen target=70
				rename percpop q
				replace q=floor(q)-6 // take away the ANC nets since we're using percpop here and not the quantifier itself
				
				tempfile target70
				save "`target70'", replace
				
			restore
			
			* collapse band of 90% 
			preserve
				collapse (mean) percpop if group==3 & accrk>=90 & accrk<100 & year>2021 & percpop<61, by(iso_a2)
			*	encode iso_a2, gen(cty)
				gen target=90
				rename percpop q
				replace q=floor(q)-6 // take away the ANC nets since we're using percpop here and not the quantifier itself
				
				tempfile target90
				save "`target90'", replace
				
			restore
		
		* collapse band of 80% and append to 90
			preserve
				collapse (mean) percpop if group==3 & accrk>=80 & accrk<90 & year>2021 & percpop<61, by(iso_a2)
			*	encode iso_a2, gen(cty)
				gen target=80
				rename percpop q
				replace q=floor(q)-6
				
			append using "`target90'"
			append using "`target70'"
			save data/s3_mean_npp, replace
			export delimited using data/s3_mean_npp.csv, replace 

			restore
			
			
			
	*** SCENARIO 4 - 3y UCC - how far should ITN access drop between campaigns?
	
		* what's the lowest that ITN access drops to between campaigns?
		
			
				preserve 
					collapse (min) accrk if group==4 & year>2021, by(iso_a2 scenario)
					reshape wide accrk, i(i) j(scenario)
					save data/s4_min_acc, replace
				restore 
		
		/* 
			preserve 
				collapse (count) group  if group==4 & accrk>=70 & accrk<100 & percpop<61, by(iso_a2 scenario)
				sort iso_a2 scenario
				gen target=70
				bysort iso_a2: egen max = max(group)
				bysort iso_a2: egen q = min(scenario) if group==max
		*/
		
		*** SCENARIO 5 - 2y UCC - how far should ITN access drop between campaigns?
	
		* what's the lowest that ITN access drops to between campaigns?
		
			
				preserve 
					collapse (min) accrk if group==5 & year>2021, by(iso_a2 scenario)
					reshape wide accrk, i(i) j(scenario)
					save data/s5_min_acc, replace
				restore 
		
