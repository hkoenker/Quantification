****************************************************************************************************

**** Multi country projections of ITN access across INDICATIVE retention times and ITN strategies
**** Author: Hannah Koenker
**** Created: July 18 2023, based on 02_Quantification_Set_Up from published ITN quantification paper

**** This file DOES NOT load ITN retention times for different countries from Bertozzi-Villa et al.
**** INSTEAD, it uses retention times ranging from 1.0-3.5 in half-year intervals, and THEN then calculates 
**** ITN access across each country under five major ITN distribution strategies and within each strategy,
**** several different quantification approaches.

**** The file calls locals from "local.do" and runs a separate do file to produce the net crops within each loop.

****************************************************************************************************
clear
cd "/Users/hannahkoenker/Dropbox/R Directory/Quantification"

grstyle init
grstyle set plain, hor compact
grstyle set legend, nobox
grstyle set color Set2

set graphics off // toggle if you need to check graphs are looking ok 

* STEP 1: INVENT the INDICATIVE retention times. Our dataset has a row for each country and we will build out years from there (wide format) 

	* import excel data/retentiontime.xlsx, clear firstrow case(l) // load data from Bertozzi-Villa et al 2021 in Nature Comms
	* destring retention, replace
	* destring retlb, replace
	* destring retub, replace
	

	
*** LOOP FOR VARYING RETENTION TIMES

	foreach g of numlist 1 15 2 25 3 35 {
		

	
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
	
	* merge 1:1 iso3 using output/retentiontime
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
	
	
	drop country 
	
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
	
	gen r1=1.0
	gen r1lb=.9
	gen r1ub=1.1
	gen r15=1.5
	gen r15lb=1.4
	gen r15ub=1.6
	gen r2=2
	gen r2lb=1.8
	gen r2ub=2.2
	gen r25=2.5
	gen r25lb=2.3
	gen r25ub=2.7
	gen r3=3
	gen r3lb=2.8
	gen r3ub=3.2
	gen r35=3.5
	gen r35lb=3.3
	gen r35ub=3.7
	
	

	
	** call the loop for retention times to set the retention time at the level desired
		gen retention=r`g'
		gen retlb=r`g'lb 
		gen retub=r`g'ub 
		
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
		save "output/gruns/`tag'_`g'", replace 
		

			
		restore 	
	}	
	
	
* SCENARIO 2 - distribute nets in large-scale school/community distribution, but do a mass campaign in 2020 two years prior.

	* putpdf paragraph, font(,20) halign(center)
	* putpdf text ("2. Projected ITN access from large-scale annual distributions")
	
	** numlist indicates CD quantification, SEPARATE FROM the RCH quantification, which is assumed at 6% in line 263
	
	*foreach x of numlist .000001 .01 .02 .03 .04 .05 .06 .07 .08 .09 .10 .11 .12 .13 .14 .15 .16 .17 .18 .19 .20 .21 .22 .23 .24 .25 .26 .27 .28 .29 .30 .31 .32 .33 .34 .35 .36 .37 .38 .39 .40 {
		
		foreach x of numlist .000001 .01(0.01).50 {
			
		preserve 
		
		** distribute nets = start with an MRC in the start year, then wait two years. 
			include locals.do 

		replace totalnets=pop*0.06 if year>=`starty' // just do RCH at 6% in 2020 and forward, while waiting for school/community to start. Start with this to fill in all the years with something (not NA)
		
		replace totalnets=totalnets+(pop/1.8) if year==`starty' // do a mass campaign in 2020, then wait two years before organizing the school/community channels. 
		
		local thirdy = `secondy'+1
		
		replace totalnets=totalnets+(pop*`x') if year>=`thirdy' // ADD ON the school/community nets to the RCH nets
		
		replace percpop=totalnets/pop*100 if year>=`starty' // fill in percent pop for the new years - this is now total nets from BOTH SCHOOL AND RCH 
		
		local X=`x'*100 // whole number for putting into the file/graph names for saving, later
		local X: di %2.0f `X' // this should make it 0, 7, 8, etc. THIS IS THE CD QUANT FACTOR ONLY.
		
		sort iso3 year // this is crucial
		
		run "03_Internal_crop_access_calcs.do"
		
		gen scenario=200+`X' // LABEL IS CD QUANT FACTOR ONLY 
		local tag = 200+`X'
		save "output/gruns/`tag'_`g'", replace 


		restore 	
	}	

* SCENARIO 3 - distribute nets in mass campaigns with RCH and varying between-campaign distribution
	
	* putpdf paragraph, font(,20) halign(center)
	* putpdf text ("3. Projected ITN access from 3 year mass campaigns with 6% ANC-EPI distribution and varying between-campaign annual distributions")
	
	local y=0.06 // assume 6% RCH 
	
	** we can go to 40% in Scenario 3; maxes out at 39 
	
	foreach x of numlist .000001 .01(0.01).40 {
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
		save "output/gruns/`tag'_`g'", replace 
		

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
		save "output/gruns/`tag'_`g'", replace 
		

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
		save "output/gruns/`tag'_`g'", replace 
		

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
		
		
		save "output/gruns/`tag'_`g'", replace 
		

		restore 	
	} // end scenario 6 loop 	
	
	} // end indicative retention time loop
	
