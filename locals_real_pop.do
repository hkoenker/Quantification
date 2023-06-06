**** Locals to include for 12. Quantification set up and 13. Internal crop and access calcs for REAL POPULATIONS

local starty = 2020 
local endy = 2035
local secondy = `starty'+1

local max=`endy'-`starty'+1 // number of years to run through, inclusive of start year and end year (hence adding 1). 
 
 * This parameter makes a HUGE HUGE difference in results, need to figure out why. 

** When it's 2020 and 2033, 13, we get 14 changes, and things work maybe?
	
local priory = `starty'-1 // the year prior to the start year
