
clear
set more off 

global root "GitHub_2104"
global dd "$root\1_data\1_source"
global dc "$root\2_code"
global do "$root\3_output" //need to create
global dt "$root\4_temp" //need to create


*-----------------------------plotid level-------------------------------------*
use "$dd\estim_plotid_10", clear

gen pgt=punit_gt_statefips/def_factor
replace pgt=punit_gt/def_factor if pgt==.

gen T=year-2009

local chem "glyp atra acet meto comp"
foreach i of local chem {
	gen `i'_rate=ai_`i'_plotid/basearea //in lb/acre
	replace `i'_rate=0 if `i'_rate==.
} //generate the dependent variables

replace comp_rate=prod_comp_plotid/basearea //in gallon/acre
	replace comp_rate=0 if comp_rate==.
	
foreach i of local chem {
	ivreghdfe `i'_rate i.year#c.gt (gt=pgt bt), absorb(fe=i.year i.idnum i.scrd#c.T) cluster(scrd)
	est store `i'
	drop fe
	
}



cd "$do"
coefplot atra || acet || meto || comp, drop(_cons gt) xline(0) 
graph save figS5.gph, replace
//manually change the labels








