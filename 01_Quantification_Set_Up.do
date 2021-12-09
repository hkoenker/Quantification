**** Multi country projections of coverage at different retention times and ITN strategies

cd "/Users/hannahkoenker/Dropbox/R Directory/Quantification"

grstyle init
grstyle set plain, hor compact
grstyle set legend, nobox
grstyle set color Set2

* load and tidy the retention times. Our dataset has a row for each country and we will build out years from there (wide format) 

	import excel data/retentiontime.xlsx, clear firstrow case(l)
	destring retention, replace
	destring retlb, replace
	destring retub, replace
	

	save output/retentiontime, replace
	
* create population databases for each country 

	* lots of work - get real population projections from all countries
	* less work - use illustrative population of 10m for all countries, with country-specific growth rates
	* even less work - 10m population, fixed growth rate, since net inputs based on pop and NPC, will cancel out 
	

** Call the local parameters from the file locals.do in the folder - this is the start and end year and others based on those:

	include locals.do 

* create the population in the start year. Below we will project population forward through the end year. 13 years.

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
		
		run "/Users/hannahkoenker/Dropbox/A DHS MIS Datasets/Analysis/Quantification 2021/2. Internal crop and access calcs.do"
		
		di "`max'"
		
		sort iso3 year 
	*******************************************************************
	*******************************************************************
	*/
	
	putpdf clear
	putpdf begin 
	
***** SCENARIO 1 -- distribute nets in a mass campaign with RCH between
	
	putpdf paragraph, font(,20) halign(center)
	putpdf text ("1. Projected ITN access from 3 year mass campaigns with varying ANC-EPI distribution")

	foreach x of numlist 0.05 0.06 0.07 {
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031 2034
			
		replace totalnets=pop*`x' if year>=`starty' // issue the RCH nets starting from the start year
		replace totalnets=totalnets+(pop/1.8) if year==2022 | year==2025 | year==2028 | year==2031 | year==2034 // add onto  the MRC nets 
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		local X=`x'*100 // whole number for putting into the file/graph names for saving, later
		local X: di %2.0f `X' // this should make it 7, 8, etc. 
		
		sort iso3 year // this is crucial
		
		run "02_Internal_crop_access_calcs.do"
		
		gen scenario=100+`X'
		local tag = 100+`X'
		save "output/runs/`tag'", replace 
		
	** GRAPH **


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
		restore 	
	}	
	
* SCENARIO 2 - distribute nets in large-scale school/community distribution, but do a mass campaign in 2020 two years prior.

	putpdf paragraph, font(,20) halign(center)
	putpdf text ("2. Projected ITN access from large-scale annual distributions")
	
	foreach x of numlist .07 .08 .09 .10 .11 .12 .13 .14 .15 .16 .17 .18 .19 .20 .21 .22 .23 .24 .25 .26 .27 .28 .29 .30 {
		
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031
			include locals.do 

		replace totalnets=pop/1.8 if year==`starty' // do a mass campaign in 2020, then wait two years before organizing the school/community channels. 
		
		replace totalnets=pop*0.06 if year>=`secondy' // just do RCH at 6% in 2021 and forward, while waiting for school/community to start.
			
		local thirdy = `secondy'+1
		
		replace totalnets=totalnets+(pop*`x') if year>=`thirdy' // add on the school/community nets
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		local X=`x'*100 // whole number for putting into the file/graph names for saving, later
		local X: di %2.0f `X' // this should make it 7, 8, etc. 
		
		sort iso3 year // this is crucial
		
		run "02_Internal_crop_access_calcs.do"
		
		gen scenario=200+`X'
		local tag = 200+`X'
		save "output/runs/`tag'", replace 

	** GRAPH **


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
		restore 	
	}	
	
* SCENARIO 3 - distribute nets in mass campaigns with RCH and varying between-campaign distribution
	
	putpdf paragraph, font(,20) halign(center)
	putpdf text ("3. Projected ITN access from 3 year mass campaigns with 6% ANC-EPI distribution and varying between-campaign annual distributions")
	
	local y=0.06 // assume 6% RCH 
	
	foreach x of numlist .07 .08 .09 .10 .11 .12 .13 .14 .15 .16 .17 .18 .19 .20 .21 .22 .23 .24 .25 .26 .27 .28 .29 .30 {
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031
			
		replace totalnets=pop*`y' if year>=`starty' // issue the RCH nets starting from 2020 
		replace totalnets=totalnets+(pop/1.8) if year==2022 | year==2025 | year==2028 | year==2031 | year==2034 // add on  the MRC nets 
		replace totalnets=totalnets+(pop*`x') if year==2023 | year==2024 | year==2026 | year==2027 | year==2029 | year==2030 | year==2032 | year==2033 | year==2035 // issue the community/school nets between campaigns
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		local X=`x'*100 // whole number for putting into the file/graph names for saving, later
		local X: di %2.0f `X' // this should make it 7, 8, etc. 
		
		sort iso3 year // this is crucial
		
		run "02_Internal_crop_access_calcs.do"
		
		gen scenario=300+`X'
		local tag = 300+`X'
		save "output/runs/`tag'", replace 
		
	** GRAPH **

	
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
		restore 	
	}	
	
* SCENARIO 4 - distribute nets in 3-year mass campaigns with varying population/x 
	
	putpdf paragraph, font(,20) halign(center)
	putpdf text ("4. Projected ITN access from 3 year mass campaigns with varying quantifiers and 6% ANC-EPI distribution")
	
	local y=0.06 // assume 6% RCH 
	
	tokenize "1_0 1_1 1_2 1_3 1_4 1_5 1_6 1_7 1_8 1_9 2_0" // for saving the graph
	
	foreach x of numlist 1.0(0.1)2.0 {
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031
			
		replace totalnets=pop*`y' if year>=`starty' // issue the RCH nets
		replace totalnets=totalnets+(pop/`x') if year==2022 | year==2025 | year==2028 | year==2031 | year==2034 // add onto  the MRC nets 
		
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		
		
		sort iso3 year // this is crucial
		
		run "02_Internal_crop_access_calcs.do"
		
		gen scenario=400+(`x'*10)
		local tag = 400+(`x'*10)
		save "output/runs/`tag'", replace 
		
	** GRAPH **


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
		restore 	
	}	
	
	
	* more useful maybe to run graphs by x, for a given country...

* SCENARIO 5 - distribute nets in 2-year mass campaigns with varying population/x 
	
	putpdf paragraph, font(,20) halign(center)
	putpdf text ("5.Projected ITN access from 2 year mass campaigns with varying quantifiers and 6% ANC-EPI distribution")
	
	local y=0.06 // assume 6% RCH 
	
	tokenize "1_0 1_1 1_2 1_3 1_4 1_5 1_6 1_7 1_8 1_9 2_0" // for saving the graph
	
	foreach x of numlist 1.0(0.1)2.0 {
		preserve 
		
		** distribute nets = MRC in 2022 2025 2028 2031
			
		replace totalnets=pop*`y' if year>=`starty' // issue the RCH nets
		foreach i of numlist `starty'(2)`endy' {
			replace totalnets=totalnets+(pop/`x') if year==`i' // add on the MRC nets every 2 years
		}
		 
		
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years
		
		
		
		sort iso3 year // this is crucial
		
		run "02_Internal_crop_access_calcs.do"
		
		gen scenario=500+(`x'*10)
		local tag = 500+(`x'*10)
		save "output/runs/`tag'", replace 
		
	** GRAPH **


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
		restore 	
	}	
	
	putpdf save output/quant_summary_`max'.pdf, replace 
	
	* !pdftk quant_UCC_y2_pop_over_1_0.pdf quant_UCC_y2_pop_over_1_1.pdf quant_UCC_y2_pop_over_1_2.pdf cat output combined_abc.pdf
