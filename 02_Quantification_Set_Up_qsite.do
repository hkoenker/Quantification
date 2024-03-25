****************************************************************************************************

**** Multi country projections of ITN access across different retention times and ITN strategies
**** Author: Hannah Koenker
**** Created: Jan 2020

**** This file loads ITN retention times for different countries from Bertozzi-Villa et al, then calculates 
**** ITN access across each country under five major ITN distribution strategies and within each strategy,
**** several different quantification approaches.

**** The file calls locals from "local.do" and runs a separate do file to produce the net crops within each loop.

**** An earlier version graphed results from each iteration and used putpdf to produce a pdf file 
**** with all of the plots for each scenario. I manually recreated the geo_facet grid used by Amelia for these.
**** This code has been commented out as R is now used to produce the full suite of the same plots. 

****************************************************************************************************
clear
cd "/Users/hannahkoenker/Dropbox/R Directory/Quantification"

grstyle init
grstyle set plain, hor compact
grstyle set legend, nobox
grstyle set color Set2

* set graphics off // toggle if you need to check graphs are looking ok 

* STEP 1: LOAD DATA and tidy the retention times. Our dataset has a row for each country and we will build out years from there (wide format) 

	import excel data/retentiontime.xlsx, clear firstrow case(l) // load data from Bertozzi-Villa et al 2021 in Nature Comms
	destring retention, replace
	destring retlb, replace
	destring retub, replace
	

	save output/retentiontime, replace
	
* create population databases for each country 

	* lots of work - get real population projections from all countries
	* less work - use illustrative population of 10m for all countries, with country-specific growth rates
	* even less work - 10m population, fixed growth rate, since net inputs based on pop and NPC, will cancel out 
	

**** import and save RKoenker's gridded points transforming nets-per-capita into ITN access with bands. We need three separate files for each hhsize because they all have their own lb and ubs. But we'll merge on the npc variable and hhsize variable.

	**** Four different hh sizes: 0 is small, 1 is med1, 2 is med2, 3 is large
	
	
		import delimited "data/npc_access/itnpers_into_access_transformation_hh.csv", clear case(l) // get this from running R Script 01
		
		** save it a first time as the regular access estimates with CIs
		rename npc npccentile
		replace npccentile=round(npccentile/100,.01)
		rename v3 accrk
		rename v2 accrklb
		rename v4 accrkub
		
		save data/npc_access/itnpers_into_access_transformation_hh, replace 
		
		** save it a second time as the lower bound estimate and CI (has to match variables later)
		rename npccentile npclbcentile
		rename accrk acc_npclb
		rename accrklb acclb_npclb
		rename accrkub accub_npclb
		
		save data/npc_access/itnpers_into_access_transformation_lb_hh, replace
		
		** save it a third time as the upper bound estimate and CI (has to match variables later)
		rename npclbcentile npcubcentile
		rename acc_npclb acc_npcub
		rename acclb_npclb acclb_npcub
		rename accub_npclb accub_npcub
		
		save data/npc_access/itnpers_into_access_transformation_ub_hh, replace
	

/* *** The full dataset fitted for transformation - ignores household size:
 
		import delimited "../data/itnpers_into_access_transformation.csv", clear case(l)
		rename v1 npccentile
		replace npccentile=round(npccentile/100,.01)
		rename v3 accrk
		rename v2 accrklb
		rename v4 accrkub
			
		save data/npc_access/itnpers_into_access_transformation, replace 
		
		rename npccentile npclbcentile
		rename accrk acc_npclb
		rename accrklb acclb_npclb
		rename accrkub accub_npclb
		
		save data/npc_access/itnpers_into_access_transformation_lb, replace
		
		rename npclbcentile npcubcentile
		rename acc_npclb acc_npcub
		rename acclb_npclb acclb_npcub
		rename accub_npclb accub_npcub
		
		save data/npc_access/itnpers_into_access_transformation_ub, replace
*/		
		

	
** Merge in the average hh size for each country 

	use "/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Analysis ITN Indicators compiled/national_itn_indicators.dta", clear
	bysort country: egen maxy=max(year)
	keep if year==maxy 
	drop if meanhh==. // 8 dropped 
	kountry country, from(other) stuck m
	rename _ISO3N_ iso3n
	kountry iso3n, from(iso3n) to(iso3c)
	rename _ISO3C_ iso3
	drop if dataset=="ZMPR18"
	
	keep country iso3 meanhh 
	
	replace iso3="CIV" if country=="Cote"
	replace iso3="COD" if country=="DRC"
	* replace iso3="STP" if country=="Sao" // not including Sao Tome & Principe
	replace iso3="SLE" if country=="Sierra"
	drop if iso3=="" // drop PNG and East Timor
	
	merge 1:1 iso3 using output/retentiontime
	* _mer==1 is countries outside SSA that we don't need to include
	* _mer==2 is countries I don't have mean hh size for, that we do need to include // https://globaldatalab.org/areadata/table/hhsize/CAF+DJI+GNQ+ERI+ETH+GNB+SOM+SSD+SDN/?levels=1
	replace meanhh=5.59 if iso3=="CAF"
	replace meanhh=5.89 if iso3=="DJI"
	replace meanhh=6.15 if iso3=="ERI"
	replace meanhh=5.83 if iso3=="ETH"
	replace meanhh=5.98 if iso3=="SSD"
	replace meanhh=5.98 if iso3=="SDN"
	replace meanhh=5.81 if iso3=="GNQ"
	replace meanhh=6.81 if iso3=="GNB"
	replace meanhh=6.16 if iso3=="SOM"
	
	drop if _merge==1
	drop _merge country 
	
	** create the groupings 
	egen hhsize=cut(meanhh), at(0,4,5,6,14) label
	
	
** Call the local parameters from the file locals.do in the folder - this is the start and end year and others based on those:

	include locals.do 

* create the population (10 million for all countries) in the start year. Below we will project population forward through the end year. 13 years.

	gen pop`starty'=10000000
	format pop`starty' %15.0f
	
	local n=1
	foreach i of numlist `secondy'/`endy' {
		gen pop`i'=pop`starty'*1.03^`n'
		local n=`n'+1
		format pop`i' %15.0fc
	}
	
	reshape long pop, i(iso3) j(year) // yassssss
	
	label var pop "Population size"
	
* derive L from retention times 

	gen k=20 // k is fixed at 20 in Amelia's paper
	
	gen L=retention/(sqrt(1-(k/(k-log(.5))))) // woo hoo! confirmed with TZ result.
	gen Llb=retlb/(sqrt(1-(k/(k-log(.5)))))
	gen Lub=retub/(sqrt(1-(k/(k-log(.5)))))
	
	label var k "k fixed at 20 for loss function"
	label var L "L parameter of loss function"
	label var Llb "L (lower bound) parameter of loss function"
	label var Lub "L (upper bound) parameter of loss function"
	
	gen totalnets=. // create an empty variable to fill below with ITN distributions
	label var totalnets "Number of ITNs distributed in current year"
	format totalnets %15.0fc
	
	gen percpop=. // empty, fill later
	label var percpop "ITNs delivered as a percent of the population"
	
	/*
	*******************************************************************
	********** TEST SECTION REMOVE WHEN RUNNING REAL THING ***********
	
	replace totalnets=pop/1.8 if year==`starty' // do a mass campaign in 2020, then wait two years before organizing the school/community channels. 
		
		replace totalnets=pop*0.06 if year==`secondy' // just do RCH in 2021, while waiting for school/community to start.
			
		replace totalnets=pop*0.18 if year>=2022 // distribute nets (routine + school/community are combined here)
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		sort iso3 year // this is crucial
		
		run "/Users/hannahkoenker/Dropbox/R Directory/Quantification/03_Internal_crop_access_calcs.do"
		
		di "`max'"
		
		sort iso3 year 
	*******************************************************************
	*******************************************************************
	*/
	
	* putpdf clear
	* putpdf begin 
	
***** SCENARIO 1 -- distribute nets in a mass campaign with RCH between
	
	* putpdf paragraph, font(,20) halign(center)
	* putpdf text ("1. Projected ITN access from 3 year mass campaigns with varying ANC-EPI distribution")

	foreach x of numlist 0.05 0.06 0.07 {
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031 2034
			
		replace totalnets=pop*`x' if year>=`starty' // issue the RCH nets starting from the start year
		replace totalnets=totalnets+(pop/1.8) if year==2022 | year==2025 | year==2028 | year==2031 | year==2034 // add onto  the MRC nets 
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		local X=`x'*100 // whole number for putting into the file/graph names for saving, later
		local X: di %2.0f `X' // this should make it 7, 8, etc. 
		
		sort iso3 year // this is crucial
		
		run "03_Internal_crop_access_calcs.do"
		
		gen scenario=100+`X'
		local tag = 100+`X'
		save "output/runs/`tag'", replace 
		
	** GRAPH **
/*

	gen geo=""
	tokenize "MRT ERI GMB SEN GNB MLI NER TCD SDN DJI SLE GIN GHA BFA BEN CAF SSD ETH SOM LBR CIV TGO NGA CMR RWA KEN GAB COG GNQ BDI UGA COM AGO COD ZMB TZA ZWE MWI MOZ MDG"

		foreach i of numlist 1/9 {
			replace geo="0`i'_`1'" if iso3=="`1'"
			mac shift
		}
		foreach i of numlist 10/40 {
			replace geo="`i'_`1'" if iso3=="`1'"
			mac shift
		}
		
			twoway connected percpop year if year>=`starty', by(geo, compact col(10) holes(1 3 4 5 6 7 9 10 19 20 30 31 39 40 41 42 43 49 51 52 53 54 59 60 61 62 63 64 68 69)  note("3-year mass campaigns with ANC-EPI at `X'%"))  msymbol(oh) mcolor(%50) mlabel(percpop) mlabpos(6) mlabsize(tiny) xtitle("") note("") || ///
				connected accrk year if year>=`starty', by(geo) msymbol(x) mcolor(%50) mlabel(accrk) mlabpos(12) mlabsize(tiny)  || ///
				rarea acclb_npclb accub_npcub year, lcolor(gs9%20) fcolor(gs9%30)  ///
				ytitle("Percent") xlabel(`starty'(1)`endy', labsize(tiny) angle(45)) ylabel(0(20)100) legend(order(1 2) size(vsmall) row(1)) ysize(*2) xsize(*1.5)
					* title("`X'%")
			graph export "figs/quant_ucc_rch_`X'.png", replace
			putpdf paragraph
			putpdf image "figs/quant_ucc_rch_`X'.png"
			putpdf pagebreak
*/			
			
		restore 	
	}	
	
	
* SCENARIO 2 - distribute nets in large-scale school/community distribution, but do a mass campaign in 2020 two years prior.

	* putpdf paragraph, font(,20) halign(center)
	* putpdf text ("2. Projected ITN access from large-scale annual distributions")
	
	** numlist indicates CD quantification, SEPARATE FROM the RCH quantification, which is assumed at 6% in line 263
	
	* go through pop x X% quantifiers ranging from 0.01 to 0.50. 50 is fine for targets 70, 80, 90 - no NAs.
		
		foreach x of numlist .000001 .01(0.01).50 {
			
		preserve 
		
		** distribute nets = start with an MRC in the start year, then wait two years. Since most countries will need some time after last mass campaign to get organized.
			include locals.do 

		replace totalnets=pop*0.06 if year>=`starty' // just do RCH at 6% in 2020 and forward, while waiting for school/community to start. Start with this to fill in all the years with something (not NA)
		
		replace totalnets=totalnets+(pop/1.8) if year==2022 // do a mass campaign in 2022, then start CD in 2023 (for the paper, it was mass in 2020, then wait two years before organizing the school/community channels.) 
		
		* local thirdy = `secondy'+1 // not used in qsite approach.
		
		replace totalnets=totalnets+(pop*`x') if year>=2023 // ADD ON the school/community nets to the RCH nets in 2023 (was 2021 for the paper)
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years - this is now total nets from BOTH SCHOOL AND RCH 
		
		local X=`x'*100 // whole number for putting into the file/graph names for saving, later
		local X: di %2.0f `X' // this should make it 0, 7, 8, etc. THIS IS THE CD QUANT FACTOR ONLY.
		
		sort iso3 year // this is crucial
		
		run "03_Internal_crop_access_calcs.do"
		
		gen scenario=200+`X' // LABEL IS CD QUANT FACTOR ONLY 
		local tag = 200+`X'
		save "output/runs/`tag'", replace 

	** GRAPH **
/*

	gen geo=""
	tokenize "MRT ERI GMB SEN GNB MLI NER TCD SDN DJI SLE GIN GHA BFA BEN CAF SSD ETH SOM LBR CIV TGO NGA CMR RWA KEN GAB COG GNQ BDI UGA COM AGO COD ZMB TZA ZWE MWI MOZ MDG"

		foreach i of numlist 1/9 {
			replace geo="0`i'_`1'" if iso3=="`1'"
			mac shift
		}
		foreach i of numlist 10/40 {
			replace geo="`i'_`1'" if iso3=="`1'"
			mac shift
		}
		
			twoway connected percpop year if year>=`starty', by(geo, compact col(10) holes(1 3 4 5 6 7 9 10 19 20 30 31 39 40 41 42 43 49 51 52 53 54 59 60 61 62 63 64 68 69) note("Annual distribution at `X'%"))  msymbol(oh) mcolor(%50) mlabel(percpop) mlabpos(6) mlabsize(tiny) xtitle("") note("") || ///
				connected accrk year if year>=`starty', by(geo) msymbol(x) mcolor(%50) mlabel(accrk) mlabpos(12) mlabsize(tiny)  || ///
				rarea acclb_npclb accub_npcub year, lcolor(gs9%20) fcolor(gs9%30)  ///
				ytitle("Percent") xlabel(`starty'(1)`endy', labsize(tiny) angle(45)) ylabel(0(20)100) legend(order(1 2) size(vsmall) row(1)) ysize(*2) xsize(*1.5)
					* title("`X'%")
			graph export "figs/quant_annual_`X'.png", replace
			putpdf paragraph
			putpdf image "figs/quant_annual_`X'.png"
			putpdf pagebreak
			
*/
		restore 	
	}	

* SCENARIO 3 - distribute nets in mass campaigns with RCH and varying between-campaign distribution
	
	* putpdf paragraph, font(,20) halign(center)
	* putpdf text ("3. Projected ITN access from 3 year mass campaigns with 6% ANC-EPI distribution and varying between-campaign annual distributions")
	
	local y=0.06 // assume 6% RCH 
	
	** we can go to 40% in Scenario 3; maxes out at 39 for 80% target. Liberia needs >40 for scen 3 at 90% target.
	
	foreach x of numlist .000001 .01(0.01).50 {
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031
			
		replace totalnets=pop*`y' if year>=`starty' // issue the RCH nets starting from 2020 
		replace totalnets=totalnets+(pop/1.8) if year==2022 | year==2025 | year==2028 | year==2031 | year==2034 // ADD ON  the MRC nets 
		replace totalnets=totalnets+(pop*`x') if year==2023 | year==2024 | year==2026 | year==2027 | year==2029 | year==2030 | year==2032 | year==2033 | year==2035 // ADD ON the community/school nets between campaigns
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years -- is TOTAL CHANNEL PERCPOP
		
		local X=`x'*100 // whole number for putting into the file/graph names for saving, later
		local X: di %2.0f `X' // this should make it 0, 1, 7, 8, etc. 
		
		sort iso3 year // this is crucial
		
		run "03_Internal_crop_access_calcs.do"
		
		gen scenario=300+`X'
		local tag = 300+`X'
		save "output/runs/`tag'", replace 
		
	** GRAPH **
/*
	
	gen geo=""
	tokenize "MRT ERI GMB SEN GNB MLI NER TCD SDN DJI SLE GIN GHA BFA BEN CAF SSD ETH SOM LBR CIV TGO NGA CMR RWA KEN GAB COG GNQ BDI UGA COM AGO COD ZMB TZA ZWE MWI MOZ MDG"

		foreach i of numlist 1/9 {
			replace geo="0`i'_`1'" if iso3=="`1'"
			mac shift
		}
		foreach i of numlist 10/40 {
			replace geo="`i'_`1'" if iso3=="`1'"
			mac shift
		}
		
			twoway connected percpop year if year>=`starty', by(geo, compact col(10) holes(1 3 4 5 6 7 9 10 19 20 30 31 39 40 41 42 43 49 51 52 53 54 59 60 61 62 63 64 68 69) note("3-year mass campaigns with annual ANC-EPI at 6% and between-campaign distribution at `X'%", size(vsmall)))  msymbol(oh) mcolor(%50) mlabel(percpop) mlabpos(6) mlabsize(tiny) xtitle("") note("") || ///
				connected accrk year if year>=`starty', by(geo) msymbol(x) mcolor(%50) mlabel(accrk) mlabpos(12) mlabsize(tiny)  || ///
				rarea acclb_npclb accub_npcub year, lcolor(gs9%20) fcolor(gs9%30)  ///
				ytitle("Percent") xlabel(`starty'(1)`endy', labsize(tiny) angle(45)) ylabel(0(20)100) legend(order(1 2) size(vsmall) row(1)) ysize(*2) xsize(*1.5)
					* title("`X'%")
			graph export "figs/quant_UCC_btw_`X'.png", replace
			putpdf paragraph
			putpdf image "figs/quant_UCC_btw_`X'.png"
			putpdf pagebreak
			
*/
		restore 	
	}	
	
* SCENARIO 4 - distribute nets in 3-year mass campaigns with varying population/x 
	
	* putpdf paragraph, font(,20) halign(center)
	* putpdf text ("4. Projected ITN access from 3 year mass campaigns with varying quantifiers and 6% ANC-EPI distribution")
	
	local y=0.06 // assume 6% RCH 
	
	tokenize "0_1 0_2 0_3 0_4 0_5 0_6 0_7 0_8 0_9 1_0 1_1 1_2 1_3 1_4 1_5 1_6 1_7 1_8 1_9 2_0" // for saving the graph
	
	foreach x of numlist 0.1(0.1)2.0 {
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031
			
		replace totalnets=pop*`y' if year>=`starty' // issue the RCH nets
		replace totalnets=totalnets+(pop/`x') if year==2022 | year==2025 | year==2028 | year==2031 | year==2034 // add onto  the MRC nets 
		
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		
		
		sort iso3 year // this is crucial
		
		run "03_Internal_crop_access_calcs.do"
		
		gen scenario=400+(`x'*10)
		local tag = 400+(`x'*10)
		save "output/runs/`tag'", replace 
		
	** GRAPH **
/*

	gen geo=""
tokenize "MRT ERI GMB SEN GNB MLI NER TCD SDN DJI SLE GIN GHA BFA BEN CAF SSD ETH SOM LBR CIV TGO NGA CMR RWA KEN GAB COG GNQ BDI UGA COM AGO COD ZMB TZA ZWE MWI MOZ MDG"

	foreach i of numlist 1/9 {
		replace geo="0`i'_`1'" if iso3=="`1'"
		mac shift
	}
	foreach i of numlist 10/40 {
		replace geo="`i'_`1'" if iso3=="`1'"
		mac shift
	}
	
	
	
	
			twoway connected percpop year if year>=`starty', by(geo, compact col(10) holes(1 3 4 5 6 7 9 10 19 20 30 31 39 40 41 42 43 49 51 52 53 54 59 60 61 62 63 64 68 69) note("3-year mass campaigns with annual ANC-EPI at 6%; population/`x'"))  msymbol(oh) mcolor(%50) mlabel(percpop) mlabpos(6) mlabsize(tiny) xtitle("") note("") || ///
				connected accrk year if year>=`starty', by(geo) msymbol(x) mcolor(%50) mlabel(accrk) mlabpos(12) mlabsize(tiny)  || ///
				rarea acclb_npclb accub_npcub year, lcolor(gs9%20) fcolor(gs9%30)  ///
				ytitle("Percent") xlabel(`starty'(1)`endy', labsize(tiny) angle(45)) ylabel(0(20)100) legend(order(1 2) size(vsmall) row(1)) ysize(*2) xsize(*1.5)
					* title("`X'%")
					
			graph export "figs/quant_UCC_pop_over_`1'.png", replace
			putpdf paragraph
			putpdf image "figs/quant_UCC_pop_over_`1'.png"
			putpdf pagebreak
			mac shift
			
*/
		restore 	
	}	
	
	

* SCENARIO 5 - distribute nets in 2-year mass campaigns with varying population/x 
	
	* putpdf paragraph, font(,20) halign(center)
	* putpdf text ("5.Projected ITN access from 2 year mass campaigns with varying quantifiers and 6% ANC-EPI distribution")
	
	local y=0.06 // assume 6% RCH 
	
	tokenize "0_5 0_6 0_7 0_8 0_9 1_0 1_1 1_2 1_3 1_4 1_5 1_6 1_7 1_8 1_9 2_0" // for saving the graph
	
	foreach x of numlist 0.5(0.1)2.0 {
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031
			
		replace totalnets=pop*`y' if year>=`starty' // issue the RCH nets
		foreach i of numlist `starty'(2)`endy' {
			replace totalnets=totalnets+(pop/`x') if year==`i' // add on the MRC nets every 2 years
		}
		 
		
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		
		
		sort iso3 year // this is crucial
		
		run "03_Internal_crop_access_calcs.do"
		
		gen scenario=500+(`x'*10)
		local tag = 500+(`x'*10)
		save "output/runs/`tag'", replace 
		
	** GRAPH **
/*

	gen geo=""
tokenize "MRT ERI GMB SEN GNB MLI NER TCD SDN DJI SLE GIN GHA BFA BEN CAF SSD ETH SOM LBR CIV TGO NGA CMR RWA KEN GAB COG GNQ BDI UGA COM AGO COD ZMB TZA ZWE MWI MOZ MDG"

	foreach i of numlist 1/9 {
		replace geo="0`i'_`1'" if iso3=="`1'"
		mac shift
	}
	foreach i of numlist 10/40 {
		replace geo="`i'_`1'" if iso3=="`1'"
		mac shift
	}
	

			twoway connected percpop year if year>=`starty', by(geo, compact col(10) holes(1 3 4 5 6 7 9 10 19 20 30 31 39 40 41 42 43 49 51 52 53 54 59 60 61 62 63 64 68 69) note("2-year mass campaigns with annual ANC-EPI at 6%; population/`x'"))  msymbol(oh) mcolor(%50) mlabel(percpop) mlabpos(6) mlabsize(tiny) xtitle("") note("") || ///
				connected accrk year if year>=`starty', by(geo) msymbol(x) mcolor(%50) mlabel(accrk) mlabpos(12) mlabsize(tiny)  || ///
				rarea acclb_npclb accub_npcub year, lcolor(gs9%20) fcolor(gs9%30)  ///
				ytitle("Percent") xlabel(`starty'(1)`endy', labsize(tiny) angle(45)) ylabel(0(20)100) legend(order(1 2) size(vsmall) row(1)) ysize(*2) xsize(*1.5)
					* title("`X'%")
					
			graph export "figs/quant_UCC_2y_pop_over_`1'.png", replace
			putpdf paragraph
			putpdf image "figs/quant_UCC_2y_pop_over_`1'.png"
			putpdf pagebreak
			mac shift
			
*/
		restore 	
	}	
	
	*** Scenario 6 - 2 year campaigns at pop/1.8
	
	* putpdf paragraph, font(,20) halign(center)
	* putpdf text ("6. Projected ITN access from 2 year mass campaigns with varying ANC-EPI distribution")

	foreach x of numlist 0.05 0.06 0.07 {
		preserve 
		
		** distribute nets = MRC every two years
			
		replace totalnets=pop*`x' if year>=`starty' // issue the RCH nets starting from the start year
		foreach i of numlist `starty'(2)`endy' {
			replace totalnets=totalnets+(pop/1.8) if year==`i' // add on the MRC nets every 2 years
		}
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		local X=`x'*100 // whole number for putting into the file/graph names for saving, later
		local X: di %2.0f `X' // this should make it 7, 8, etc. 
		
		sort iso3 year // this is crucial
		
		run "03_Internal_crop_access_calcs.do"
		
		gen scenario=600+`X'
		local tag = 600+`X'
		
		
		save "output/runs/`tag'", replace 
		
	** GRAPH **
/*

	gen geo=""
	tokenize "MRT ERI GMB SEN GNB MLI NER TCD SDN DJI SLE GIN GHA BFA BEN CAF SSD ETH SOM LBR CIV TGO NGA CMR RWA KEN GAB COG GNQ BDI UGA COM AGO COD ZMB TZA ZWE MWI MOZ MDG"

		foreach i of numlist 1/9 {
			replace geo="0`i'_`1'" if iso3=="`1'"
			mac shift
		}
		foreach i of numlist 10/40 {
			replace geo="`i'_`1'" if iso3=="`1'"
			mac shift
		}
		
			twoway connected percpop year if year>=`starty', by(geo, compact col(10) holes(1 3 4 5 6 7 9 10 19 20 30 31 39 40 41 42 43 49 51 52 53 54 59 60 61 62 63 64 68 69)  note("3-year mass campaigns with ANC-EPI at `X'%"))  msymbol(oh) mcolor(%50) mlabel(percpop) mlabpos(6) mlabsize(tiny) xtitle("") note("") || ///
				connected accrk year if year>=`starty', by(geo) msymbol(x) mcolor(%50) mlabel(accrk) mlabpos(12) mlabsize(tiny)  || ///
				rarea acclb_npclb accub_npcub year, lcolor(gs9%20) fcolor(gs9%30)  ///
				ytitle("Percent") xlabel(`starty'(1)`endy', labsize(tiny) angle(45)) ylabel(0(20)100) legend(order(1 2) size(vsmall) row(1)) ysize(*2) xsize(*1.5)
					* title("`X'%")
			graph export "figs/quant_ucc2_rch_`X'.png", replace
			putpdf paragraph
			putpdf image "figs/quant_ucc2_rch_`X'.png"
			putpdf pagebreak
			
*/
		restore 	
	}	
	
	* putpdf save output/quant_summary_stata.pdf, replace 
	
