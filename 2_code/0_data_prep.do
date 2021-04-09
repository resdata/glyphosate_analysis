********************************************************************************
*				program: prepare estimation data							   *			
*				written by: Ziwei Ye										   *
* 				date: 10/13/2020											   *
*				update:											   	   	       *
********************************************************************************

global root "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\0_submission"

global dd "$root\data"
global dc "$root\code"
global do "$root\output"
global dt "$root\temp"

cd "$do"

*------------------------herbicide consumption data (AgroTrak)-----------------*
import delimited "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\S drive_Raw data\ISUAgroTrak_ConsumptionData_NEW.csv", clear

***keep corn and herbicide herbicide observations
keep if crop=="Corn"
keep if pesttype=="Herbicide"


***generate catogory variables
*GT trait
tab seedtraitcollapsed
gen gt=0
replace gt=1 if strpos(seedtraitcollapsed,"Gly Tol")>0
//whether contains GT trait

gen bt=0
replace bt=1 if strpos(seedtraitcollapsed,"CB")>0 |strpos(seedtraitcollapsed,"RW")>0
//whether contains Bt traits

gen ht=0
replace ht=1 if strpos(seedtraitcollapsed,"LL")>0 | strpos(seedtraitcollapsed,"HT other")>0
//whether contains other HT traits


*tillage
tab tillagetype
label define till 1 "No-Till" 2 "Conservation" 3 "Conventional"
encode tillagetype, gen(till)
label list till

***keep only relevant variables
keep year idnum seedtraitcollapsed product countyfips basearea expenditures avgproductrate productamtused totalareatreated projectedsample avgproductprice gt bt ht till seedtrait tillagetype seedcompany sequence 

***save cleaned consumption data
save "$dt\consumption_cleaned.dta", replace

***identify unique plots
use "$dt\consumption_cleaned.dta", clear

preserve
duplicates drop year idnum seedtrait tillagetype seedcompany sequence  basearea,force //the set of variables that identify unique plots
sort year idnum
gen plotid=_n //211,856 unique plots over 1998-2016
save "$dt\plot_identify.dta", replace
restore 

merge m:1 year idnum seedtrait tillagetype seedcompany sequence  basearea using "$dt\plot_identify.dta", nogenerate //merge with plotid
order plotid, before(year)

merge m:1 countyfips using "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\Administrative region relationship\state_crd_county.dta", nogenerate keep(match master) //merge with region variables: county, crd, state. 
drop currentlybeingused2histori

save, replace 



*------------------------prepare field and farm level dataset------------------*
use "$dt\consumption_cleaned.dta", clear

keep plotid year idnum countyfips seedtraitcollapsed basearea gt bt ht till //keep only plot-level variables (plotid seedtraitcollapsed basearea gt till) and farm-level variables (year idnum countyfips ), drop the product-level ones.

duplicates drop plotid, force

gen convt=0 //conventional tillage dummy
replace convt=1 if till==3
gen notill=0
replace notill=1 if till==1 //no-till dummy

label data "estimation dataset, plot-level, 1998-2016"

save "$dt\temp_estim_plotid",replace

use "$dt\temp_estim_plotid",clear
sort year idnum plotid
gen weight=basearea
collapse (sum) basearea (mean) countyfips (mean) gt bt ht convt notill [aw=weight], by(year idnum) // countyfips are the same across plots for the same farm

label var basearea "total basearea at farm-level"
label var gt "proportion of GT acres at farm-level"
label var bt "proportion of Bt acres at farm-level"
label var ht "proportion of HT acres at farm-level"
label var convt "proportion of conventional tillage acres at farm-level"
label var notill "proportion of no-till acres at farm-level"

label data "estimation dataset, farm-level, 1998-2016"
save "$dt\temp_estim_idnum",replace 



*----------------------consumption data at ingredient level--------------------*
import delimited "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\S drive_Raw data\ISUAgroTrak_AISet_NEW.csv", clear

save "$dt\ai.dta", replace 

use "$dt\consumption_cleaned.dta", clear

joinby product using "$dt\ai.dta" //merge with active ingredient data
tab pesttype
keep if pesttype=="Herbicide"  //glyphosate is being labeled as both "Herbicide" and "Growth Regulator" at the same time, hence the duplication

sort year idnum plotid 
save "$dd\consp_ai_98.dta", replace //herbicide consumption data at active ingredient level, 1998-2016: year-farm-plot-product-active ingredient (but we don't have plot-level panel, only farm level panel, because we can't know which plots are the same ones across years)
label data "herbicide consumption data at active ingredient level, 1998-2016: year-farm-plot-product-active ingredient (each row)"




*-----------prepare datasets for consumption and control variables-------------*

***prepare data for consumption at plotid/idnum/scrd/countyfips level

use "$dd\consp_ai_98.dta", clear

gen ai= productamtused* lbsai

*regional level
local geo "plotid idnum scrd countyfips statefips" //individual chemical
foreach j of local geo {
	
	local chem "GLYPHOSATE ATRAZINE ACETOCHLOR METOLACHLOR-S"
	foreach i of local chem {
		preserve
		keep if activeingredient=="`i'"
			
		local abbr=lower(substr("`i'",1,4))
		collapse (sum) expenditure productamtused ai, by(year `j')
		gen qprice_`abbr'_`j'=expenditure/productamtused //product price
		gen aprice_`abbr'_`j'=expenditure/ai //ai price
		rename expenditure exp_`abbr'_`j'
		rename productamtused prod_`abbr'_`j'
		rename ai ai_`abbr'_`j'
		
		save "$dt\consp_`j'_`abbr'.dta",replace
		restore
	} 

	
	local chem "comp" //composite herbicide
	preserve
	keep if activeingredient=="ATRAZINE"|activeingredient=="ACETOCHLOR"|activeingredient=="METOLACHLOR-S"
	duplicates drop plotid product productamtused expenditure, force //to avoid double accounting of the same product that contains more than one of the three chemicals
	foreach i of local chem{
		
		local abbr=lower(substr("`i'",1,4))
		collapse (sum) expenditure productamtused ai, by(year `j')
		gen qprice_`abbr'_`j'=expenditure/productamtused //product price
		gen aprice_`abbr'_`j'=expenditure/ai //ai price
		rename expenditure exp_`abbr'_`j'
		rename productamtused prod_`abbr'_`j'
		
		save "$dt\consp_`j'_`abbr'.dta",replace
	}
	restore
	
}

*national level
local chem "GLYPHOSATE ATRAZINE ACETOCHLOR METOLACHLOR-S"
	foreach i of local chem {
		preserve
		keep if activeingredient=="`i'"
			
		local abbr=lower(substr("`i'",1,4))
		collapse (sum) expenditure productamtused ai, by(year)
		gen qprice_`abbr'_na=expenditure/productamtused //product price
		gen aprice_`abbr'_na=expenditure/ai //ai price
		rename expenditure exp_`abbr'_na
		rename productamtused prod_`abbr'_na
		rename ai ai_`abbr'_na
		
		save "$dt\consp_na_`abbr'.dta",replace
		restore
	} 

	
local chem "comp" //composite herbicide
	preserve
	keep if activeingredient=="ATRAZINE"|activeingredient=="ACETOCHLOR"|activeingredient=="METOLACHLOR-S"
	duplicates drop plotid product productamtused expenditure, force //to avoid double accounting of the same product that contains more than one of the three chemicals
	foreach i of local chem{
		
		local abbr=lower(substr("`i'",1,4))
		collapse (sum) expenditure productamtused ai, by(year)
		gen qprice_`abbr'_na=expenditure/productamtused //product price
		gen aprice_`abbr'_na=expenditure/ai //ai price
		rename expenditure exp_`abbr'_na
		rename productamtused prod_`abbr'_na
		
		save "$dt\consp_na_`abbr'.dta",replace
	}
restore
	


***prepare data for control variables 
*zndx_lastsepmay
use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\ClimateDivisionDataset\exported dta\ClimDivVar_ZNDX_lastSepMay.dta", clear

rename *,lower
rename zndx_lastsepmay zndx
keep year cd countyfips zndx //match with countyfips

save "$dt\zndx.dta", replace

*fuel price
use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\Diesel Fuel Price\exported dta\pfuel_na_lastSepMay.dta", clear
rename *,lower
rename pfuel_na_lastsepmay pfuel_na
save "$dt\pfuel_na.dta", replace //match with year

local padd "paddl1 paddl2"
foreach i of local padd {
	use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\Diesel Fuel Price\exported dta\pfuel_`i'_lastSepMay.dta", clear
	rename *,lower
	rename pfuel_`i'_lastsepmay pfuel_`i'
	drop state stateabbr
	save "$dt\pfuel_`i'.dta", replace
} //match with year/year+statefips

*corn futures price
use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\Futures prices\Corn, December\pfcorn,DecContract.dta", clear
rename *,lower
save "$dt\pfcorn.dta", replace //match with year

*resistant weeds
use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\Resistant Weeds\wcount.dta"
keep Year StateFIPS wcount_cum_gly wcount_cum_comp wcount_add_gly wcount_add_comp
rename *,lower
save "$dt\wcount_use", replace //year+statefips

*GDP price deflator
use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\GDP_price_deflator\0_export_dta\def_fac_98.dta", clear
rename *,lower
save "$dt\def_98.dta", replace //match with year

use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\GDP_price_deflator\0_export_dta\def_fac.dta", clear
rename *,lower
save "$dt\def_10.dta", replace //match with year

*hel from ei data (nri)
use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\nri10\nri_ei\exported dta\ei_county_wm.dta", clear
rename *, lower
generate hel8=0
replace hel8=1 if ei_county_wm>=8
keep countyfips hel8 
save "$dt\hel8,dta", replace //match with countyfips

*seed price
use "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\Data for use\dta\Corn\joinby 2(use this)\0_Basic Data\price_seed.dta", clear
rename *, lower
preserve 
keep year pacre_gt punit_gt pacre_ngt punit_ngt
duplicates drop year, force
save "$dt\price_seed_year.dta", replace //match with year 
restore

preserve 
keep year countyfips *countyfips *scrd *statefips
save "$dt\price_seed.dta", replace //match with year and countyfips 
restore

*--------merge dataset and variables for field and farm level data-------------*
***merge with control variables
local name "plotid idnum"

foreach j of local name { //control variables
	use "$dt\temp_estim_`j'", clear

	merge m:1 countyfips using "C:\Users\yeziw\Michigan State University\AFRE - Ziwei\External source data\Administrative region relationship\state_crd_county.dta", nogenerate keep(match master) //merge with region variables: county, crd, state. 
	drop currentlybeingused2histori

	merge m:1 year countyfips using "$dt\zndx.dta", nogenerate keep(master match) //merge with zndx data; with missing values 

	merge m:1 year using "$dt\pfuel_na.dta", nogenerate keep(master match) 
	merge m:1 year statefips using "$dt\pfuel_paddl1.dta", nogenerate keep(master match) 
	merge m:1 year statefips using "$dt\pfuel_paddl2.dta", nogenerate keep(master match) 
	//merge with fuel price data

	merge m:1 year using "$dt\pfcorn.dta", nogenerate keep(master match) //corn futures price

	merge m:1 year statefips using "$dt\wcount_use.dta", nogenerate keep(master match) //resistant weed count for gly and composite; missing for some year-state
	merge m:1 year using "$dt\def_10.dta", nogenerate keep(master match) //deflation factor, 2010 as base year, missing for previous years 
	
	merge m:1 countyfips using "$dt\hel8,dta", nogenerate keep(master match) //HEL8, highly erodible land
	
	merge m:1 year countyfips using "$dt\price_seed. dta", nogenerate keep(master match) //seed price, GT seed v.s. non-GT seed.
	merge m:1 year using "$dt\price_seed_year. dta", nogenerate keep(master match) //seed price, GT seed v.s. non-GT seed.

	//add lables later
	
	save, replace

}

***merge with consumption variables: exp, quantity, prices.
local abbr "glyp atra acet meto comp"

use "$dt\temp_estim_plotid", clear
foreach i of local abbr {
	merge 1:1 plotid using "$dt\consp_plotid_`i'",  nogenerate keep(master match)
	merge m:1 year using "$dt\consp_na_`i'", nogenerate keep(master match)
	
	local l1 "idnum scrd countyfips statefips"
	foreach j of local l1{
		merge m:1 year `j' using "$dt\consp_`j'_`i'", nogenerate keep(master match)
	}
}
save,replace

use "$dt\temp_estim_idnum",clear
foreach i of local abbr{
	merge 1:1 year idnum using "$dt\consp_idnum_`i'", nogenerate keep(master match)
	merge m:1 year using "$dt\consp_na_`i'", nogenerate keep(master match)

	local l2 "scrd countyfips statefips"
	foreach j of local l2{
		merge m:1 year `j' using "$dt\consp_`j'_`i'", nogenerate keep(master match)
	}
}	
save,replace

nmissing qprice_*_scrd if year>2009 //check if there are missing values of crd-level prices over 2010-2016 --very few

***merge with fisher price index
merge m:1 year scrd using "$dt\fisher_glyp_CRDmean_deflated.dta",keep(matched master) nogenerate 
merge m:1 year scrd using "$dt\fisher_comp_CRDmean_deflated.dta",keep(matched master) nogenerate 

save,replace
*------------------------export data for estimation----------------------------*

local name "plotid idnum"
foreach i of local name {
	use "$dt\temp_estim_`i'", clear
	for var exp_*: replace X = 0 if X==.
	for var wcount_*: replace X = 0 if X==.
	
	save "$dd\estim_`i'_98.dta", replace
	
	keep if year>2009
	save "$dd\estim_`i'_10.dta", replace

}









