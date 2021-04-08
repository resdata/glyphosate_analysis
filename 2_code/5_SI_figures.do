
clear
set more off 

global root "GitHub_2104"
global dd "$root\1_data\1_source"
global dc "$root\2_code"
global do "$root\3_output" //need to create
global dt "$root\4_temp" //need to create


********************************************************************************
*							Fig. S1											   *
********************************************************************************
use "$dd\estim_idnum_10.dta", clear
***generate vars
*number of firm-year observations
bys statefips year: gen nobs=_N

*cost share
local chem "glyp comp"
foreach i of local chem{
	replace exp_`i'_idnum=0 if exp_`i'_idnum==.
}

gen exp_total_idnum=exp_glyp_idnum+exp_comp_idnum

foreach i of local chem{
	gen s_`i'=exp_`i'_idnum/exp_total_idnum
}

drop if exp_total_idnum==0 //drop farms that use neither herbicide; only 641 out of 30，362 obs.

*lnp, price index
gen lnp=ln(Findex_glyp/Findex_comp)


*wcount difference
gen wcount_cum_d=wcount_cum_gly-wcount_cum_comp
gen wcount_add_d=wcount_add_gly-wcount_add_comp 


***choose alternative measures 
gen till=convt
gen wcount=wcount_cum_d
gen pfuel=pfuel_paddl1/def_factor
gen pgt=punit_gt_statefips/def_factor
replace pgt=punit_gt/def_factor if pgt==.

***clean
local basevar "lnp wcount gt till pgt bt pfuel hel8"
nmissing s_gly `basevar'
egen miss=rowmiss(s_glyp `basevar')
drop if miss

***state-level quantities, averaged over years
preserve
collapse (sum) exp_glyp_idnum exp_comp_idnum (mean) nobs lnp wcount, by(state)
save "$dt\temp.dta", replace
restore

collapse (mean) gt till [aw=basearea], by(state)
merge 1:1 state using "$dt\temp.dta", nogen

gen share=exp_glyp_idnum/(exp_glyp_idnum+exp_comp_idnum)
replace share=0 if share==.
drop exp*

***export to R to make figures
format lnp wcount gt till share %4.3f

tokenize "nobs share lnp wcount gt till"
forval i=1/6{
	preserve
	keep state ``i''
	rename ``i'' value
	export delimited using "$root\Github_new\8_Rfigure\map_var`i'.csv", datafmt replace
	restore
}

********************************************************************************
*							Fig. S2											   *
********************************************************************************
use "$dd\estim_plotid_98.dta",clear

gen gtonly=0
replace gtonly=1 if gt==1 & bt==0 & ht==0

gen gtbtonly=0
replace gtbtonly=1 if gt==1 & bt==1 &ht==0

gen gtbtht=0
replace gtbtht=1 if gt==1 & bt==1 &ht==1

foreach v of varlist gtonly gtbtonly gtbtht{
	gen area_`v'=`v'*basearea
}

collapse (sum) basearea area_*, by(year)

foreach v of varlist area_* {
	local abbr=substr("`v'",6,.)
	gen rate_`abbr'=`v'/basearea
}

keep year rate_*
reshape long rate_, i(year) j(group,string)
rename rate_ value
replace group="A" if group=="gtbtht"
replace group="B" if group=="gtbtonly"
replace group="C" if group=="gtonly"

export delimited using "$dc\Rfigure\figs2.csv", datafmt replace



********************************************************************************
*							Fig. S4											   *
********************************************************************************
//residualized variables, by subtracting the field mean from observation

***get the dataset for estimation
use "$dd\estim_idnum_10", clear

*cost share
local chem "glyp comp"
foreach i of local chem{
	replace exp_`i'_idnum=0 if exp_`i'_idnum==.
}

gen exp_total_idnum=exp_glyp_idnum+exp_comp_idnum

foreach i of local chem{
	gen s_`i'=exp_`i'_idnum/exp_total_idnum
}

drop if exp_total_idnum==0 //drop farms that use neither herbicide; only 641 out of 30，362 obs.

*lnp, price index
gen lnp=ln(Findex_glyp/Findex_comp)


*wcount difference
gen wcount_cum_d=wcount_cum_gly-wcount_cum_comp
gen wcount_add_d=wcount_add_gly-wcount_add_comp 

*year dummies
tab year, gen(yr)
gen T=year-2009

***choose alternative measures 
gen till=convt
gen wcount=wcount_cum_d
gen pfuel=pfuel_paddl1/def_factor
gen pgt=punit_gt_statefips/def_factor
replace pgt=punit_gt/def_factor if pgt==.

***clean
local basevar "lnp gt till wcount pgt bt pfuel hel8"
nmissing s_gly `basevar'
egen miss=rowmiss(s_gly `basevar')
drop if miss

***create residualized variables
foreach v of varlist s_gly lnp wcount till gt{
	bysort idnum: egen `v'_m=mean(`v')
	gen `v'_r=`v'-`v'_m
}


***export
keep s_gly* lnp* gt* till* wcount*
export delimited using "$root\Github_new\8_Rfigure\figs4.csv", datafmt replace




