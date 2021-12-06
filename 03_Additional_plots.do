*** 3. Additional plots Quantification 2021

	** append data from all the runs 
	cd "/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Quantification 2021"

	clear
	
	foreach x of numlist 105/107 {
		append using "output/runs/`x'"
	}
	
	foreach x of numlist 207/225 {
		append using "output/runs/`x'"
	}
	foreach x of numlist 307/325 {
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
	save "/Users/hannahkoenker/Dropbox/R Directory/Quantification/data/quant_runs.dta", replace
	
	li iso_a2 percpop if accrk>70 & iso3!=""
	
	su percpop if group==2 & iso3=="TZA" & accrk>70 & year>2021, d
	
	bysort iso_a2: su percpop if group==2 & accrk>70 & year>2021
	
	bysort iso_a2: egen mymode=mode(percpop) if group==2 & year>2021 & accrk>60, minmode
	bysort iso_a2: egen maxmode=mode(percpop) if group==2 & year>2021 & accrk>60, maxmode

	bysort iso_a2: egen med2 = median(percpop) if group==2 & year>2021 & accrk>60 
	bysort iso_a2: egen mean2 = mean(percpop) if group==2 & year>2021 & accrk>60 
	
	twoway scatter med2 mean2 
	
	** they are similar, let's use the mean
	* collapse (median) percpop if group==2 & accrk>60 & year>2021, by(iso_a2)
	
	 collapse (mean) percpop if group==2 & accrk>60 & year>2021, by(iso_a2)
	
	
	* collapse (firstnm) mymode, by(rep78)
	
	encode iso_a2, gen(cty)

	twoway scatter percpop cty, xlabel(1(1)40, angle(45) labsize(tiny) valuelabel)
	
	save output/mean_npp, replace
	export delimited using "/Users/hannahkoenker/Dropbox/R Directory/Quantification/data/mean_npp.csv", replace 
	save "/Users/hannahkoenker/Dropbox/R Directory/Quantification/data/mean_npp.dta", replace
