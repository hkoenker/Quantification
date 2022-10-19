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
			
			save output/totalnetspyp, replace
		
	
	restore 
	
	
	
	
	*** SCENARIO 2 RECOMMENDED QUANTIFIERS
	
	** find lowest of most frequent scenario where band criteria are met 
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
	
	
	
	
	*** SCENARIO 3 RECOMMENDED QUANTIFIERS
	* should I be doing which scenario has the most accrk over 80?
	
	tab scenario if iso3=="AGO" & accrk>=80 & accrk<=85 & group==3 // some countries, incl CAM, don't fall in this band! only 31 countries.
		tab scenario if iso3=="LBR" & accrk>=80 & accrk<=85 & group==3 // some countries, incl CAM, don't fall in this band! only 31 countries.

	* then take the lowest scenario number from the highest frequency group 
	* the "q" is the CD quantification factor only, it DOES NOT INCLUDE THE 6% RCH 
	
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
			collapse (count) group if group==3 & accrk>=80 & accrk<100 & year>2021 & percpop<61, by(iso_a2 scenario) // count # of years where access is at least 80%, excluding 2021 and any campaign years from the count.
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
		
		
