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
		
		
*** Total nets needed - 80% minimum + status quo

cd output/runs
	
	use 219, clear
	keep if iso3=="TZA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop. MORE nets than 3y campaigns...
	
	use 307, clear
	keep if iso3=="TZA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.

	use 412, clear
	keep if iso3=="TZA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.


	use 418, clear
	keep if iso3=="TZA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.

*** Total nets needed for same scenario at different targets
	* for a few different countries: Liberia, Mali, Nigeria - except they all have same pop, so same total nets for a given scenario
	
	* need total nets for 70, for 80, for 90 for each scenario 2, 3, and 4/5
	* so 3 x 3 x 3 countries 

	cd output/runs

	* set year to keep records from:
	local y=2021
	
	putexcel set quant_totalnets_MLNGLB.xlsx, replace
	putexcel A1="country" B1="cd70" C1="cd80" D1="cd90" E1="ucc70" F1="ucc80" G1="ucc90" H1="combo70" I1="combo80" J1="combo90" K1="statusquo" L1="accstatusquo" A2="1.03 yrs - Liberia" A3="2.81 yrs - Mali" A4="2.22 yrs - Nigeria"
	
	
	
	use 418, clear // status quo - lowest is 20
		keep if iso3=="LBR" 
		bysort iso3: egen grandtot=total(totalnets)
		replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop. MORE nets than 3y campaigns...
		collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		mean accrk 
		scalar pacc=r(table)[1,1]
		putexcel K2=pp L2=pacc
	
	/*
	use 325, clear // cd70 Liberia -- can use either 226 or 325 to maintain at 70 but lets' use 226
	keep if iso3=="LBR"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>2021, by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		mean accrk 
		scalar pacc=r(table)[1,1]
		putexcel B2=pp 
	*/
	
	use 324, clear // cd70 Liberia
	keep if iso3=="LBR"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		mean accrk 
		scalar pacc=r(table)[1,1]
		putexcel H2=pp 
		
		use 330, clear // cd80 Liberia
	keep if iso3=="LBR"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		mean accrk 
		scalar pacc=r(table)[1,1]
		putexcel I2=pp 
		
		
	use 510, clear // lowest is 81; 2 yr campaign at 1.0, OMG UCC80
	keep if iso3=="LBR"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel F2=pp 
		
	use 513, clear // UCC70
	keep if iso3=="LBR"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel E2=pp 	

	/*
	use 230, clear // 230 only gives y ou 63% minimum , so no cd70 for scen 2 Liberia
	keep if iso3=="LBR"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel B2=pp 
	*/ 
	
	
*** Mali scenarios	
	* status quo - 1.8
	use 418, clear // lowest is 78
	keep if iso3=="MLI" 
	bysort iso3: egen grandtot=total(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop. MORE nets than 3y campaigns...
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
	mean grandtot 
		scalar pp=r(table)[1,1]
		mean accrk 
		scalar pacc=r(table)[1,1]
		putexcel K3=pp L3=pacc
		
		
	* 70% for CD
	use 209, clear
	keep if iso3=="MLI"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel B3=pp 	
		
	* 80% for CD
	use 212, clear // lowest is 80
	keep if iso3=="MLI"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel C3=pp
		
	* 90% for CD
	use 221, clear
	keep if iso3=="MLI"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel D3=pp 	
	
	* UCC 80
	use 417, clear
	keep if iso3=="MLI"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel F3=pp 	
		
	* UCC 90
	use 411, clear
	keep if iso3=="MLI"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel G3=pp 	
		
	* Combo 90
	use 307, clear
	keep if iso3=="MLI"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel J3=pp 	

*** Nigeria scenarios	
	* status quo - 1.8
	use 418, clear // lowest is 78
	keep if iso3=="NGA" 
	bysort iso3: egen grandtot=total(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop. MORE nets than 3y campaigns...
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
	mean grandtot 
		scalar pp=r(table)[1,1]
		mean accrk 
		scalar pacc=r(table)[1,1]
		putexcel K4=pp L4=pacc
		
		
	* 70% for CD
	use 212, clear
	keep if iso3=="NGA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel B4=pp 	
		
	* 80% for CD
	use 218, clear // lowest is 80
	keep if iso3=="NGA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel C4=pp
		
	* 90% for CD
	use 227, clear
	keep if iso3=="NGA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel D4=pp 	
	
	* UCC 70
	use 417, clear
	keep if iso3=="NGA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel E4=pp 	
		
	* UCC 80
	use 413, clear
	keep if iso3=="NGA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel F4=pp 	
		
	* UCC 90
	use 518, clear
	keep if iso3=="NGA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel G4=pp 	
		
	* Combo 80
	use 307, clear
	keep if iso3=="NGA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel I4=pp 
		
	* Combo 90
	use 315, clear
	keep if iso3=="NGA"
	bysort iso3: egen grandtot=sum(totalnets)
	replace grandtot=grandtot/1000000 // remember this is still in a 10m population, not real pop.
	collapse grandtot scenario (min) accrk if year>`y', by(iso3)
		mean grandtot 
		scalar pp=r(table)[1,1]
		putexcel J4=pp 	

	
	import excel quant_totalnets_MLNGLB.xlsx, firstrow case(l) clear 
	
	grstyle set plain, hor compact
	grstyle set legend, nobox
	grstyle set color ptol, n(9)

	*graph bar statusquo cd70 cd80 cd90 ucc70 ucc80 ucc90 combo70 combo80 combo90, by(country, row(1))
	
	
	** replace with percent diff from status quo
	 
	 
		foreach var of varlist cd70 cd80 cd90 ucc70 ucc80 ucc90 combo70 combo80 combo90 statusquo {
			replace `var'=(`var'/47.559505*100)-100
			format `var' %9.2f
		}
		
		graph bar cd70 cd80 cd90 ucc70 ucc80 ucc90 combo70 combo80 combo90, by(country, row(1) note("") ) blabel(total, format(%9.0f) size(vsmall)) legend(col(3) label(1 "CD 70%") label(2 "CD 80%") label(3 "CD 90%") label(4 "Campaigns 70%") label(5 "Campaigns 80%") label (6 "Campaigns 90%") label(7 "Both 70%") label(8 "Both 80%") label(9 "Both 90%"))  bar(1, color(eltblue)) bar(2, color(ebblue)) bar(3, color(edkblue)) bar(4, color(eltgreen)) bar(5, color(teal)) bar(6, color(dkgreen)) bar(7, color(gs13)) bar(8, color(gs10)) bar(9, color(gs6)) ytitle("Percent difference from 3-year campaigns using pop/1.8", size(small)) bargap(25) // title("% difference in nets needed compared to 3-year mass campaigns using population/1.8")
		
	 *	graph bar cd70 cd80 cd90 ucc70 ucc80 ucc90 combo70 combo80 combo90 accstatusquo, by(country, row(1) note("") ) blabel(total, format(%9.0f) size(vsmall)) legend(col(3) label(1 "CD 70%") label(2 "CD 80%") label(3 "CD 90%") label(4 "Campaigns 70%") label(5 "Campaigns 80%") label (6 "Campaigns 90%") label(7 "Both 70%") label(8 "Both 80%") label(9 "Both 90%") label(10 "Lowest ITN access between campaigns for status quo"))  bar(1, color(eltblue)) bar(2, color(ebblue)) bar(3, color(edkblue)) bar(4, color(eltgreen)) bar(5, color(forest_green)) bar(6, color(dkgreen)) bar(7, color(gs13)) bar(8, color(gs10)) bar(9, color(gs6)) bar(10, color(red)) // title("% difference in nets needed compared to 3-year mass campaigns using population/1.8")
		
	 