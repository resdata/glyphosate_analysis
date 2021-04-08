
clear
set more off 

global root "GitHub_2104"
global dd "$root\1_data\1_source"
global dc "$root\2_code"
global do "$root\3_output" //need to create
global dt "$root\4_temp" //need to create

********************************************************************************
*							Fig. 1											   *
********************************************************************************

*---------------------------fig1a----------------------------------------------*
use "$dd\consp_ai_98.dta", clear //herbicide consumption data at active ingredient level, 1998-2016: year-farm-plot-product-active ingredient 

gen ai= productamtused* lbsai

*national level
local chem "GLYPHOSATE ATRAZINE ACETOCHLOR METOLACHLOR-S"
	foreach i of local chem {
		preserve
		keep if activeingredient=="`i'"
			
		local abbr=lower(substr("`i'",1,4))
		collapse (sum) ai, by(year)
		
		rename ai ai_`abbr'
		
		save "$dt\temp_`abbr'.dta",replace
		restore
	} 


preserve
drop if activeingredient=="GLYPHOSATE"|activeingredient=="GLYPHOSATE"|activeingredient=="ATRAZINE"|activeingredient=="ACETOCHLOR"|activeingredient=="METOLACHLOR-S"
collapse (sum) ai, by(year)
rename ai ai_others
save "$dt\temp_others.dta",replace
restore

use "$dd\estim_plotid_98.dta",clear
collapse (sum) basearea, by (year)


foreach i of local chem{
	local abbr=lower(substr("`i'",1,4))
	merge 1:1 year using "$dt\temp_`abbr'.dta",nogenerate
}
merge 1:1 year using "$dt\temp_others.dta",nogenerate

//create rate_* for R.
foreach i of local chem{
	local abbr=lower(substr("`i'",1,4))
	gen rate_`abbr'=ai_`abbr'/basearea
}
gen rate_others=ai_others/basearea


keep rate_* year

reshape long rate_, i(year) j(variable,string)
rename rate_ value

sort variable year

export delimited using "$root\Github\8_Rfigure\fig1a.csv", datafmt replace



*---------------------------fig1b----------------------------------------------*
use "$dd\consp_ai_98.dta", clear //herbicide consumption data at active ingredient level, 1998-2016: year-farm-plot-product-active ingredient 

gen ai= productamtused* lbsai

preserve
keep if activeingredient=="ATRAZINE"|activeingredient=="ACETOCHLOR"|activeingredient=="METOLACHLOR-S"
collapse (sum) ai, by(year)
rename ai ai_comp
save "$dt\temp_comp.dta",replace

restore

use "$dd\estim_plotid_98.dta",clear
gen gtarea=gt*basearea
gen convtarea=convt*basearea
collapse (sum) basearea (sum) gtarea convtarea , by(year)
merge 1:1 year using "$dt\temp_glyp.dta", nogenerate
merge 1:1 year using "$dt\temp_comp.dta", nogenerate

gen shr_gt=gtarea/basearea
gen shr_convt=convt/basearea
gen rate_glyp=1.12*ai_glyp/basearea //multiply 1.12 to convert from lbs/ac to kg/ha
gen rate_comp=1.12*ai_comp/basearea

keep year shr_* rate_*
export delimited using "$root\Github\8_Rfigure\fig1b.csv", datafmt replace

*---------------------------fig1c----------------------------------------------*
use "$dt\wcount_use", clear 
collapse (sum) wcount_cum_*, by(year)
export delimited using "$root\Github\8_Rfigure\fig1c.csv", datafmt replace

*---------------------------fig1d----------------------------------------------*
use "$dd\estim_idnum_10", clear
gen lnp=ln(Findex_glyp/Findex_comp)

collapse (mean) lnp Findex*, by(year)

//rescale
gen d_glyp=134.90303
gen d_comp=104.96597

gen d_Findex_glyp=Findex_glyp/d_glyp
gen d_Findex_comp=Findex_comp/d_comp

graph twoway (line d_Findex_glyp year)(line d_Findex_comp year)

keep year d_Findex* 

export delimited using "$root\Github\8_Rfigure\fig1d.csv", datafmt replace









