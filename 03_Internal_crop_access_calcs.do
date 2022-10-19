**** Internal do file to perform net decay functions, works with 1. Quantification Set Up


** Call the local parameters from the file locals.do in the working folder:

	include locals.do 


* build the net crop 

	
	
	
	* years start from 2020 and go through  - should we say 2035? That is 16 years
	
	gen cropn=year-`priory' // variable to number years 1-n where start year (2020) is year 1
	label var cropn "Year 1-n starting `starty'"
	
	foreach i of numlist 1/`max' {
			gen crop`i'=.
			replace crop`i'=totalnets if cropn==`i' // create crop variables to fill next loop
			format crop`i' %15.0fc
			
			gen croplb`i'=.
			replace croplb`i'=totalnets if cropn==`i' // create lower bound crop variables to fill next loop
			format croplb`i' %15.0fc
			
			gen cropub`i'=.
			replace cropub`i'=totalnets if cropn==`i' // create upper bound crop variables to fill next loop
			format cropub`i' %15.0fc
		}
	
	foreach j of numlist 1/`max' {
			foreach i of numlist 1/`max' {
				local tdist=`i'-`j'
				if `i'==`j' {
					// do nothing, it's a distribution, leave net quantity in place
				}
				if `i'<`j' {
					// tdist is a negative number, is before distribution, leave it as missing, will replace with 0 later
				}
				if `tdist'>L {
				*	replace crop`j'=0 if crop`j'[_n-`i']==totalnets[_n-`i'] & crop`j'[_n-`i']!=.
				* replace crop`j'=0 if crop`j'[_n-`i']!=totalnets[_n-`i'] & crop`j'[_n-`i']!=.
					* if 13-7 is more than 11.75, make crop2=0
				}
				if `tdist'>0 & `tdist'<L {
					replace crop`j'=crop`j'[_n-`tdist']*exp(k-(k/(1-((`tdist'/L)*(`tdist'/L))))) if crop`j'[_n-`tdist']==totalnets[_n-`tdist'] & crop`j'[_n-`tdist']!=. & totalnets[_n-`tdist']!=0 & cropn==`i'
				}
			} // go to next i (years since distribution)
		} // go to next j (crop column)
		
		** LOWER BOUND CROPS 
		
			foreach j of numlist 1/`max' {
			foreach i of numlist 1/`max' {
				local tdist=`i'-`j'
				if `i'==`j' {
					// do nothing, it's a distribution, leave net quantity in place
				}
				if `i'<`j' {
					// tdist is a negative number, is before distribution, leave it as missing, will replace with 0 later
				}
				if `tdist'>Llb {
				*	replace croplb`j'=0 if croplb`j'[_n-`i']==totalnets[_n-`i'] & croplb`j'[_n-`i']!=.
				* replace croplb`j'=0 if croplb`j'[_n-`i']!=totalnets[_n-`i'] & croplb`j'[_n-`i']!=.
					* if 13-7 is more than 10.2, make crop2=0
				}
				if `tdist'>0 & `tdist'<L {
					replace croplb`j'=croplb`j'[_n-`tdist']*exp(k-(k/(1-((`tdist'/Llb)*(`tdist'/Llb))))) if croplb`j'[_n-`tdist']==totalnets[_n-`tdist'] & croplb`j'[_n-`tdist']!=. & totalnets[_n-`tdist']!=0 & cropn==`i'
				}
			} // go to next i (years since distribution)
		} // go to next j (crop column)
		
		** UPPER BOUND CROPS 
		
			foreach j of numlist 1/`max' {
			foreach i of numlist 1/`max' {
				local tdist=`i'-`j'
				if `i'==`j' {
					// do nothing, it's a distribution, leave net quantity in place
				}
				if `i'<`j' {
					// tdist is a negative number, is before distribution, leave it as missing, will replace with 0 later
				}
				if `tdist'>Lub {
				*	replace cropub`j'=0 if cropub`j'[_n-`i']==totalnets[_n-`i'] & cropub`j'[_n-`i']!=.
				* replace cropub`j'=0 if cropub`j'[_n-`i']!=totalnets[_n-`i'] & cropub`j'[_n-`i']!=.
					* if 13-7 is more than 13.2, make crop2=0
				}
				if `tdist'>0 & `tdist'<L {
					replace cropub`j'=cropub`j'[_n-`tdist']*exp(k-(k/(1-((`tdist'/Lub)*(`tdist'/Lub))))) if cropub`j'[_n-`tdist']==totalnets[_n-`tdist'] & cropub`j'[_n-`tdist']!=. & totalnets[_n-`tdist']!=0 & cropn==`i'
				}
			} // go to next i (years since distribution)
		} // go to next j (crop column)
		
	foreach j of numlist 1/`max' {
		replace crop`j'=0 if crop`j'==.
		replace croplb`j'=0 if croplb`j'==.
		replace cropub`j'=0 if cropub`j'==.
		*replace crop`j'=0 if crop`j'<1 // replace crop less than 1 with 0...not crucial, since we're just adding. 
	}
	** sum crop by year 
	
		** first order the variables to facilitate rowtotal:
		
			order crop1, after(cropn)
				foreach n of numlist 2/`max' {
					local m = `n'-1
					order crop`n', after(crop`m')
				}
				
			order croplb1, after(crop`max')
				foreach n of numlist 2/`max' {
					local m = `n'-1
					order croplb`n', after(croplb`m')
				}	
				
			order cropub1, after(croplb`max')
				foreach n of numlist 2/`max' {
					local m = `n'-1
					order cropub`n', after(cropub`m')
				}		

		* order crop1 crop2 crop3 crop4 crop5 crop6 crop7 crop8 crop9 crop10 crop11 crop12 crop13, after(cropn) // weren't allowing for other max parameters
		* order croplb1 croplb2 croplb3 croplb4 croplb5 croplb6 croplb7 croplb8 croplb9 croplb10 croplb11 croplb12 croplb13, after(crop13)
		* order cropub1 cropub2 cropub3 cropub4 cropub5 cropub6 cropub7 cropub8 cropub9 cropub10 cropub11 cropub12 cropub13, after(croplb13)
	
	egen crop=rowtotal(crop1-crop`max')
	label var crop "net crop with median lifespan"
	replace crop=0 if crop<1 
	
	egen croplb=rowtotal(croplb1-croplb`max')
	label var croplb "net crop with lower bound median lifespan (lb)"
	replace croplb=0 if croplb<1 
	
	egen cropub=rowtotal(cropub1-cropub`max')
	label var cropub "net crop with upper bound median lifespan (ub)"
	replace cropub=0 if cropub<1 
	
	gen npc=crop/pop // generate nets per capita
		label var npc "net crop per capita"
		gen npclb=croplb/pop // generate npc lower bound
		label var npclb "lower bound net crop per capita"
		gen npcub=cropub/pop // generate npc upper bound 
		label var npcub "upper bound net crop per capita"

	*** merge the appropriate fit into the dataset, based on mean hh size 
		
		
			
				
				gen npccentile=round(npc,.01) // create variable for merging with RKoenker's dataset of gridded points, rounded to 2 decimal places 
				merge m:1 npccentile hhsize using data/itnpers_into_access_transformation_hh
				drop if _m==2 // drop 3 levels of npccentile grid that don't match to any npc 
				drop _merge // 1561 obs that have npc<0.01 that 'don't match', which is fine, see next lines 
				
				** merge again (same data; new variable names) for the lower and upper bounds
				gen npclbcentile=round(npclb,.01)
				merge m:1 npclbcentile hhsize using data/itnpers_into_access_transformation_lb_hh
				drop _merge 
				
				gen npcubcentile=round(npcub,.01)
				merge m:1 npcubcentile hhsize using data/itnpers_into_access_transformation_ub_hh
				drop _merge
		
		
	 
	 
		** clean up:
		** make very small access results 0 where the corresponding npccenter is less than 0.01
			foreach var of varlist accrk accrklb accrkub  { 
				replace `var'=0 if npccentile<0.01
			}
			foreach var of varlist acc_npclb  acclb_npclb accub_npclb {
				replace `var'=0 if npclbcentile<0.01
			}
			foreach var of varlist acc_npcub  accub_npclb accub_npcub {
				replace `var'=0 if npcubcentile<0.01
			}
		
		**  accrk is missing if npc is >1.00, so make access 100. bounds are also missing. 
			replace accrk=100 if npccentile>=1 
			replace acc_npclb=100 if npclbcentile>=1
			replace acc_npcub=100 if npcubcentile>=1
			
		*	replace acclb_npclb=100 if npc
		
		foreach var of varlist accrk accrklb accrkub acc_npclb acclb_npclb accub_npclb acc_npcub accub_npclb accub_npcub {
			format `var' %9.0f
		}
		
		label var accrk "Predicted Population ITN Access"
		label var npccentile "Rounded nets per capita (2 dec places)"
		label var npclbcentile "Rounded nets per capita lower bound of crop lifespan"
		label var npcubcentile "Rounded nets per capita upper bound of crop lifespan"
		
		label var acclb_npclb "lower bound of access and of net lifespan"
		label var accub_npcub "upper bound of access and of net lifespan"
		
		
	sort year
