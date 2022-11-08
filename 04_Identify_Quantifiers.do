*** 3. Append all iterations and identify best quantification factor for each of the 6 scenarios
***		Calculate total person-years of ITN access for each iteration



	** append data from all the runs -- MAKE SURE NUMBER LISTS BELOW ARE UPDATED TO REFLECT THE SCENARIOS USED IN 01 do file
	
	cd "/Users/hannahkoenker/Dropbox/R Directory/Quantification"

	clear
	
	foreach x of numlist 105/107 {
		append using "output/runs/`x'"
	}
	
	foreach x of numlist 200/250 {
		append using "output/runs/`x'"
	}
	foreach x of numlist 300/340 {
		append using "output/runs/`x'"
	}
	foreach x of numlist 401/420 {
		append using "output/runs/`x'"
	}
	foreach x of numlist 505/520 {
		append using "output/runs/`x'"
	}
	foreach x of numlist 605/607 {
		append using "output/runs/`x'"
	}
	
	label var year "year"
	label var scenario "scenario"
	
	
		
		
	drop crop* npclb npcub accrkub acc_npclb accub_npclb acclb_npcub acc_npcub npclbcentile npcubcentile k accrklb
	drop if iso3==""
	
	gen group=floor(scenario/100)
	label var group "ITN strategy"
	
	
	** person-years of ITN access
		
		sort iso3 scenario year // important to sort by year here
		
		gen double pyp = accrk/100*pop 
		bysort iso3 scenario: gen pypsum = sum(pyp) // gives running sum
		
		gen double pyplb = acclb_npclb/100*pop 
		bysort iso3 scenario: gen pyplbsum = sum(pyplb)
		
		gen double pypub = accub_npcub/100*pop 
		bysort iso3 scenario: gen pypubsum = sum(pypub)
		
		
	** naming of countries
	
		encode iso3, gen(cty)
		
		kountry iso3, from(iso3c) to(iso2c)
		rename _ISO2C_ iso_a2 
	
	
	
	save output/quant_runs, replace 
	
	** Calculate total person-years of ITN access and the quantifiers that provide the max PYP for the various scenarios
	
	preserve 
	
			keep iso3 year group scenario totalnets pyp*
			
			collapse (sum) totalnets pyp pyplb pypub, by(iso3 group scenario) 
			
			bysort iso3 group: egen double maxpyp=max(pyp)
			gen ismaxpyp=maxpyp==pyp
			gen best=0 // start from zero
			
			** Find the lowest quantifier that provides the max PYP for the CD scenarios (1, 2, 3, 6):
				bysort iso3 group: egen minbest=min(scenario) if ismaxpyp==1 & (group<4 | group==6)
				bysort iso3 group: replace best=1 if scenario==minbest & (group<4 | group==6)
			
			** Find the highest quantifier that provides the max pyp for the UCC scenarios (4, 5)
				bysort iso3 group: egen maxbest=max(scenario) if ismaxpyp==1 & (group==4 | group==5)
				bysort iso3 group: replace best=1 if scenario==maxbest & (group==4 | group==5)
			
			** These q's do not align AT ALL with the recommended Qs in the rest of the paper. it maxes out PYP at 100's in every year and identifies the lowest Q that achieves that. So not linked to the 80 or 90% targets. 
			
			** However, file can still be used in R to merge in the Qs from table 2 etc and then label those as 'best' and plot them in the graph. 
			
			save output/totalnetspyp, replace
		
	
	restore 
	
	*** Set max number of years at which we want ITN access to be at or above the threshold for Scenario 2 and 3:
	local s23max=5
	
	
	*** SCENARIO 2 RECOMMENDED QUANTIFIERS
	
	** find lowest of most frequent scenario where band criteria are met 
		tab scenario if iso3=="AGO" & accrk>=80 & accrk<=85 & group==2 // some countries, incl CAM, don't fall in this band! only 31 countries.
		tab scenario if iso3=="LBR" & accrk>=80 & accrk<=85 & group==2 // some countries, incl CAM, don't fall in this band! only 31 countries.
		
		
			
				
		
			* target of 70; was pre-Oct 27th 70-79, 80-89, 90-99 bands. 
			* post Nov 8 I set limit to <=100 for each band. This seriously increased some of the q factors; so I put it back at the narrow bands so that we're getting just over the threshold rather than lots of high-ass values in a wider band.
			
			** number of scenarios that meet the target is generally 13-14 of 15 years in this scenario. For Scenario 3 (below) it's different. 
			
		preserve 
			collapse (count) group if group==2 & accrk>=70 & accrk<80 & year>2021, by(iso_a2 scenario)
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max  & max>=`s23max'
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
			bysort iso_a2: egen q = min(scenario) if group==max & max>=`s23max'
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
			bysort iso_a2: egen q = min(scenario) if group==max  & max>=`s23max'
			collapse q, by(iso_a2)
			replace q=q-200
			gen target=90
			
			append using "`s2t80'"
			append using "`s2t70'"
			save data/s2_min_npp, replace 
		restore
	
	
	
	
	*** SCENARIO 3 RECOMMENDED QUANTIFIERS
	* identifying which scenario has the most years with accrk over 80, and less than 100 (pre Oct 27 2022). After Nov 8 we set <=100 otherwise Cameroon, Congo don't show any quantification factors for 90% in scenario 3. This doesn't change the q's in the other target levels for Scen 3. Earlier (up to Oct 27 2022) we didn't set a minimum number of years within the band that access should be at or above threshold. Some times it was only 3 or 5 years out of the 15 year period.
	
	* For most countries at most targets they can get 9 years between target and up to and including 100. 
	
	
	
	tab scenario if iso3=="AGO" & accrk>=80 & accrk<=85 & group==3 // some countries, incl CAM, don't fall in this band! only 31 countries.
		tab scenario if iso3=="LBR" & accrk>=80 & accrk<=85 & group==3 // some countries, incl CAM, don't fall in this band! only 31 countries.

	* then take the lowest scenario number from the highest frequency group 
	* the "q" is the CD quantification factor only, it DOES NOT INCLUDE THE 6% RCH 
	
		preserve
			collapse (count) group if group==3 & accrk>=70 & accrk<100 & year>2021 & percpop<61, by(iso_a2 scenario) // exclude years of the mass campaigns; band expanded to 70-100 for Scenario 3, as some btw years are always above 80
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max  & max>=`s23max'
			collapse q, by(iso_a2)
			replace q=q-300
			gen target=70
			
			tempfile s3t70
			save "`s3t70'", replace 
		restore
		
		preserve
			collapse (count) group if group==3 & accrk>=80 & accrk<100 & year>2021 & percpop<61, by(iso_a2 scenario) // count # of years where access is at least 80%, excluding 2021 and any campaign years from the count.
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max  & max>=`s23max'
			collapse q, by(iso_a2)
			replace q=q-300
			gen target=80
			
			tempfile s3t80
			save "`s3t80'", replace 
		restore
		
		preserve
			collapse (count) group if group==3 & accrk>=90 & accrk<=100 & year>2021 & percpop<61, by(iso_a2 scenario) // here we do include 100, otherwise CMR and COG don't appear in table.
			bysort iso_a2: egen max = max(group)
			bysort iso_a2: egen q = min(scenario) if group==max & max>=`s23max'
			collapse q, by(iso_a2)
			replace q=q-300
			gen target=90
			
			append using "`s3t80'"
			append using "`s3t70'"
			save data/s3_min_npp, replace 
		restore
	
	
	
	
			
			
			
	*** SCENARIO 4 - 3y UCC - how far should ITN access drop between campaigns?
	
		* what's the lowest that ITN access drops to between campaigns?
		
		use output/quant_runs, clear 
			
				preserve 
					collapse (min) accrk if group==4 & year>2021, by(iso_a2 scenario)
					reshape wide accrk, i(i) j(scenario)
					
					
					
					save data/s4_min_acc, replace
					
			
					** grab the q for keeping ITN access at or above 80
					
						reshape long accrk,  i(iso_a2) j(q) 
						drop if accrk<80
						bysort iso_a2: egen best=max(q)
						
						collapse best, by(iso_a2)
						
						save data/s4q, replace
				restore 
				
			
		
		
		*** SCENARIO 5 - 2y UCC - how far should ITN access drop between campaigns?
	
		* what's the lowest that ITN access drops to between campaigns?
		
			
				preserve 
					collapse (min) accrk if group==5 & year>2021, by(iso_a2 scenario)
					reshape wide accrk, i(i) j(scenario)
					
		
					save data/s5_min_acc, replace
					
					** grab the q for keeping ITN access at or above 80
					
						reshape long accrk,  i(iso_a2) j(q) 
						drop if accrk<80
						bysort iso_a2: egen best=max(q)
						
						collapse best, by(iso_a2)
						
						save data/s5q, replace
				restore 
		
		
