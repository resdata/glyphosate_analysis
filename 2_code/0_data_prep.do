
clear
set more off 

global root "GitHub_2104"
global dd "$root\1_data\1_source"
global dc "$root\2_code"
global do "$root\3_output" //need to create
global dt "$root\4_temp" //need to create

cd "$dd"

*------------------------prep--------------------------------------------------*
*------deflator-------*
import delimited "other\GDPDEF.csv", clear
gen year=real(substr(date,1,4))

bysort year : egen gdpdef_year=mean(gdpdef) //use year average as the GDP deflator for that year.
duplicates drop year,force

preserve
gen def_factor=gdpdef_year/96.10675 //use 2010 as baseline.
keep if year>2009 & year<2017
keep year def_factor
save "$dt\def_fac_10.dta",replace
restore

preserve 
gen def_factor=gdpdef_year/75.279251 //use 1998 as baseline.
keep if year>1998 & year<2017
keep year def_factor
save "$dt\def_fac_98.dta",replace
restore

*------administrative regions-------*
import excel "other\padd_statefips.xls", firstrow clear
save "$dt\padd_statefips.dta", replace

import excel "other\state_crd_county.xls", firstrow clear
save "$dt\state_crd_county.dta", replace

import excel "other\statefips.xls", firstrow clear
save "$dt\statefips.dta", replace


*------------------------herbicide consumption---------------------------------*
import delimited "ISUAgroTrak_ConsumptionData_NEW.csv", clear

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

merge m:1 countyfips using "$dt\state_crd_county.dta", nogenerate keep(match master) //merge with region variables: county, crd, state. 
drop currentlybeingused2histori

save, replace 



*----prepare field and farm level dataset-------*
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



*-------consumption data at ingredient level--------*
import delimited "ISUAgroTrak_AISet_NEW.csv", clear

save "$dt\ai.dta", replace 

use "$dt\consumption_cleaned.dta", clear

joinby product using "$dt\ai.dta" //merge with active ingredient data
tab pesttype
keep if pesttype=="Herbicide"  //glyphosate is being labeled as both "Herbicide" and "Growth Regulator" at the same time, hence the duplication

sort year idnum plotid 
save "consp_ai_98.dta", replace //herbicide consumption data at active ingredient level, 1998-2016: year-farm-plot-product-active ingredient (but we don't have plot-level panel, only farm level panel, because we can't know which plots are the same ones across years)
label data "herbicide consumption data at active ingredient level, 1998-2016: year-farm-plot-product-active ingredient (each row)"




*-----herbicide consumption variables-------*

***prepare data for consumption at plotid/idnum/scrd/countyfips level

use "consp_ai_98.dta", clear

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
		collapse (sum) expenditure productamtused, by(year `j')
		gen qprice_`abbr'_`j'=expenditure/productamtused //product price
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
		collapse (sum) expenditure productamtused, by(year)
		gen qprice_`abbr'_na=expenditure/productamtused //product price
		rename expenditure exp_`abbr'_na
		rename productamtused prod_`abbr'_na
		
		save "$dt\consp_na_`abbr'.dta",replace
	}
restore
	

*-----choose basket product----------*
*-----glyphosate-----*
use "$dd\consp_ai_98.dta", clear

keep if year>2009
keep if activeingredient=="GLYPHOSATE"

merge m:1 year using "$dt\def_10.dta",nogenerate
replace expenditures=expenditures/def_factor

collapse (sum) productamtused expenditures (mean) lbsai, by(product)

rename lbsai lbsai_flag

joinby product using "$dt\ai.dta" //merge with active ingredient data
drop if pesttype=="Growth Regulator"
gen flag1=1 if lbsai_flag!=lbsai
bysort product: egen flag2=max(flag1)
drop if activeingredient!="GLYPHOSATE"

drop if flag2==1
egen amtsum=total(productamtused)
egen expsum=total(expenditures)
gen amtperc=productamtused/amtsum
gen expperc=expenditures/expsum
gsort -expperc


//select the basket products who account for more than 1% and not pre-mixed.
replace expperc=round(expperc, 0.001) 
keep if expperc>=0.05
gen indexpr=_n
keep product indexpr
save "$dt\indexpr_glyp.dta", replace


*-----the composite-----*
*individual chemical
use "$dd\consp_ai_98.dta", clear
keep if year>2009
gen ai=productamtused*lbsai

merge m:1 year using "$dt\def_10.dta",nogenerate
replace expenditures=expenditures/def_factor


local chem "ATRAZINE METOLACHLOR-S ACETOCHLOR"
foreach i of local chem{
	preserve
	keep if activeingredient=="`i'"
	
	collapse (sum) ai productamtused expenditures (mean) lbsai, by(product)
	rename lbsai lbsai_flag
	joinby product using "$dt\ai.dta" //merge with active ingredient data
	gen flag1=1 if activeingredient!="ATRAZINE"&activeingredient!="METOLACHLOR-S"&activeingredient!="ACETOCHLOR"
	bysort product: egen flag2=max(flag1)
	drop if activeingredient!="`i'"
	gsort -ai //descending order

	egen aisum=total(ai)
	egen amtsum=total(productamtused)
	gen aiperc=ai/aisum
	gen amtperc=productamtused/amtsum
	gen qprice_na=expenditures/productamtused
	gen aprice_na=expenditures/ai
	
	replace flag2=0 if flag2!=1
	ttest qprice_na, by(flag2)
	
	keep product productamtused expenditures flag2
	replace expenditures=round(expenditures,1)
	save "$dt\mix_`i'.dta", replace
	restore
}

*composite herbicide product
use "$dt\mix_ATRAZINE.dta", clear
rename flag2 newflag1
merge 1:1 product productamtused expenditures using "$dt\mix_METOLACHLOR-S.dta",
drop _merge 
rename flag2 newflag2
merge 1:1 product productamtused expenditures using "$dt\mix_ACETOCHLOR.dta",
drop _merge 
rename flag2 newflag3
egen flag2=rowmax(newflag1 newflag2 newflag3)

drop if flag==1
gen Atrazine=1 if newflag1==0
gen MetolachlorS=1 if newflag2==0
gen Acetochlor=1 if newflag3==0
 
egen amtsum=total(productamtused)
gen amtperc=productamtused/amtsum
egen expsum=total(expenditures)
gen expperc=expenditures/expsum
gen qprice_comp=expenditures/productamtused


//store the product names if it is not pre-mixed and accounts for more than 5% expenditure among the non mixed products 
replace expperc=round(expperc, 0.001) 
keep if expperc>0.05
gen indexpr=_n
keep product indexpr
save "$dt\indexpr_comp.dta", replace


*----------------------------price index: prepare----------------------------*
***total area, for approximating quantities
use "$dd\consp_ai_98.dta", clear
keep if year>2009
duplicates drop year idnum seedtrait tillagetype seedcompany sequence basearea,force //the set of variables that identify unique plots
foreach a of global admin{
	preserve
	collapse (sum) basearea, by(year `a')
	save "$dt\area_`a'.dta", replace
	restore
}

collapse (sum) basearea, by(year)
save "$dt\area_na.dta", replace


*-------price and quantity of basket products-------*

***CRD/state level amount-weighted average price, for each basket product defined in indexpr
		
foreach h of global herb{
	foreach a of global admin{
		use "$dd\consp_ai_98.dta", clear
		keep if year>2009
		keep product ai productamtused expenditures activeingredient `a' year

		merge m:1 product using "$dt\indexpr_`h'.dta", nogenerate keep(matched) //keep only the observations of basket products from indexpr_* file
		collapse (sum) expenditure productamtused, by(year `a' indexpr)
		merge m:1 year `a' using "$dt\area_`a'.dta", keep(matched) nogenerate
		merge m:1 year using "$dt\def_10.dta",nogenerate

		gen qprice_`a'=(expenditure/productamtused)/def_factor
		gen rate_`a'=productamtused/basearea
		rename expenditure exp
		rename productamtused amt
		drop basearea
		reshape wide exp amt qprice_`a' rate_`a', i(year `a') j(indexpr)
		
		preserve
		drop qprice* rate*
		save "$dt\exp_amt_`h'_`a'_indexpr.dta", replace
		restore
		preserve
		drop exp* amt*
		save "$dt\qprice_rate_`h'_`a'_indexpr.dta", replace
		restore
	}
}
		

	
***national level amount-weighted average price, for each basket product defined in indexpr
foreach h of global herb{
		use "$dd\consp_ai_98.dta", clear
		keep if year>2009
		keep product ai productamtused expenditures activeingredient year

		merge m:1 product using "$dt\indexpr_`h'.dta", nogenerate keep(matched) //keep only the observations of basket products from indexpr_* file

		collapse (sum) expenditure productamtused, by(year indexpr)
		merge m:1 year using "$dt\area_na.dta", keep(matched) nogenerate
		merge m:1 year using "$dt\def_10.dta",nogenerate

		gen qprice_na=(expenditure/productamtused)/def_factor
		gen rate_na=productamtused/basearea

		rename expenditure exp
		rename productamtused amt
		drop basearea
		reshape wide exp amt qprice_na rate_na, i(year) j(indexpr)

		preserve
		drop qprice* rate*
		save "$dt\exp_amt_`h'_na_indexpr.dta", replace
		restore
		preserve
		drop exp* amt* 
		save "$dt\qprice_rate_`h'_na_indexpr.dta", replace
		restore
}

***match with adjacent states and calculate average price for each product
foreach h of global herb{
		use "$dd\consp_ai_98.dta", clear
		keep if year>2009
		keep year statefips
		duplicates drop year statefips, force //complete set of county-year in our sample

		joinby statefips using "$dt\adj_statefips"

		rename statefips main_statefips
		rename adj_statefips statefips
		merge m:1 year statefips using "$dt\exp_amt_`h'_statefips_indexpr.dta", keep(matched master) nogenerate
		merge m:1 year statefips using "$dt\area_statefips.dta"

		rename statefips adj_statefips
		collapse (sum) exp* amt* basearea*, by (year main_statefips)
		merge m:1 year using "$dt\def_10.dta",nogenerate

		reshape long exp amt, i(year main_statefips) j(indexpr)
		gen qprice_statefips_adj=(exp/amt)/def_factor
		gen rate_statefips_adj=amt/basearea
		replace rate_statefips_adj=. if rate_statefips_adj==0
		
		reshape wide exp amt qprice_statefips_adj rate_statefips_adj, i(year main_statefips) j(indexpr)
		
		rename main_statefips statefips
		keep year statefips qprice* rate*	
		
		save "$dt\qprice_rate_`h'_statefips_adj.dta", replace

}

		
***replace missing county-level prices with adjacent county average, state average, adjacent state average, and national average
***replace missing county-level quantities with county area * per area quantity of adjacent county average, state average, adjacent state average, and national average
foreach h of global herb{
use "$dd\consp_ai_98.dta", clear
keep if year>2009
keep year scrd statefips
duplicates drop year scrd, force //complete set of county-year in our sample

merge 1:1 year scrd using "$dt\qprice_rate_`h'_scrd_indexpr.dta", nogenerate keep(master matched)
merge m:1 year statefips using "$dt\qprice_rate_`h'_statefips_indexpr.dta", nogenerate keep(master matched)
merge m:1 year statefips using "$dt\qprice_rate_`h'_statefips_adj.dta", nogenerate keep(master matched)
merge m:1 year using "$dt\qprice_rate_`h'_na_indexpr.dta", nogenerate keep(master matched)
merge 1:1 year scrd using "$dt\area_scrd.dta", nogenerate keep(master matched)

reshape long qprice_scrd  qprice_statefips qprice_statefips_adj qprice_na rate_scrd rate_statefips rate_statefips_adj rate_na,i(year scrd) j(indexpr)

gen qprice=qprice_scrd
replace qprice=qprice_statefips if qprice==.
replace qprice=qprice_statefips_adj if qprice==.
replace qprice=qprice_na if qprice==.

gen amt=basearea*rate_scrd
replace amt=basearea*rate_statefips if amt==.
replace amt=basearea*rate_statefips_adj if amt==.
replace amt=basearea*rate_na if amt==.

drop rate_* qprice_* basearea

reshape wide qprice amt, i(year scrd) j(indexpr)

keep year scrd amt* qprice*

save "$dt\fisher_prep_`h'.dta",replace

nmissing _all

}


*----------------------------price index: construct----------------------------*
***base=average of the entire period, for each CRD
foreach h of global herb{
	use "$dt\fisher_prep_`h'.dta", clear 
	reshape long amt qprice, i(year scrd) j(indexpr)

	preserve
	collapse (mean) amt , by(indexpr scrd) 
	save "$dt\fisher_`h'_baseCRDmean_amt.dta", replace
	restore

	collapse (mean) qprice [weight=amt] , by(indexpr scrd) 
	save "$dt\fisher_`h'_baseCRDmean_qprice.dta", replace

	merge 1:1 indexpr scrd using "$dt\fisher_`h'_baseCRDmean_amt.dta", nogenerate

	reshape wide amt qprice, i(scrd) j(indexpr)
	for var amt* qprice*: rename X X_base
	save "$dt\fisher_`h'_baseCRDmean.dta", replace
}


	
***calculate price index using CRD sample mean as the base.
*glyphosate
use "$dt\fisher_prep_glyp.dta", clear  
merge m:1 scrd using "$dt\fisher_glyp_baseCRDmean.dta", nogenerate 

gen Pindex=100*(qprice1*amt1+qprice2*amt2+qprice3*amt3+qprice4*amt4+qprice5*amt5)/(qprice1_base*amt1+qprice2_base*amt2+qprice3_base*amt3+qprice4_base*amt4+qprice5_base*amt5)
gen Lindex=100*(qprice1*amt1_base+qprice2*amt2_base+qprice3*amt3_base+qprice4*amt4_base+qprice5*amt5_base)/(qprice1_base*amt1_base+qprice2_base*amt2_base+qprice3_base*amt3_base+qprice4_base*amt4_base+qprice5_base*amt5_base)
gen Findex=sqrt(Pindex*Lindex)

keep year scrd *index
for var *index: rename X X_glyp

save "$dt\fisher_glyp_CRDmean_deflated.dta", replace

*composite
use "$dt\fisher_prep_comp.dta", clear  
merge m:1 scrd using "$dt\fisher_comp_baseCRDmean.dta", nogenerate keep (matched) //drop the observations for which Fisher is not obtainable.

gen Pindex=100*(qprice1*amt1+qprice2*amt2+qprice3*amt3+qprice4*amt4+qprice5*amt5+qprice6*amt6)/(qprice1_base*amt1+qprice2_base*amt2+qprice3_base*amt3+qprice4_base*amt4+qprice5_base*amt5+qprice6_base*amt6)
gen Lindex=100*(qprice1*amt1_base+qprice2*amt2_base+qprice3*amt3_base+qprice4*amt4_base+qprice5*amt5_base+qprice6*amt6_base)/(qprice1_base*amt1_base+qprice2_base*amt2_base+qprice3_base*amt3_base+qprice4_base*amt4_base+qprice5_base*amt5_base+qprice6_base*amt6_base)
gen Findex=sqrt(Pindex*Lindex)

keep year scrd *index
for var *index: rename X X_comp

save "$dt\fisher_comp_CRDmean_deflated.dta", replace	
	

*-----------------------------------soil erodibility---------------------------*
import delimited "hel8\nri10_extract_061914.csv", clear
egen ei=rowmean(ei82-ei10)
order ei, before (ei82)

bysort fips: asgen ei_county_wm=ei, w(xfact) //calculate soil erodibility at county level as a weighted average of erodibility at field level, using xfact as weight.
order ei_county_wm, after(ei)
duplicates drop fips, force

rename fips countyfips
keep countyfips ei_county_wm

generate hel8=0
replace hel8=1 if ei_county_wm>=8
keep countyfips hel8 
save "$dt\hel8,dta", replace //match with countyfips

*--------------------------------pfuel-----------------------------------------*
import excel "pfuel\PET_PRI_GND_A_EPD2D_PTE_DPGAL_M.xls", sheet("Data 2") cellrange(A3:J309) firstrow clear
gen day = string(mofd(Date), "%tmCYN")
gen year=real(substr(day, 1,4))
gen month=real(substr(day, 5,6))
drop Date day NewEnglandPADD1ANo2Diese CentralAtlanticPADD1BNo2 LowerAtlanticPADD1CNo2Di WestCoastPADD5ExceptCalif
rename EastCoastNo2DieselRetailPr pfuel_padd10
rename MidwestNo2DieselRetailPrice pfuel_padd20
rename GulfCoastNo2DieselRetailPr pfuel_padd30
rename RockyMountainNo2DieselRetai pfuel_padd40
rename WestCoastNo2DieselRetailPr pfuel_padd50

reshape long pfuel_padd, i(year month) j(padd_l1)

rename year FuelYear

generate year=0

order year, before (FuelYear)

sort FuelYear month

forvalues i=2010(1)2016{
  replace year=`i' if FuelYear==`i'-1&month>=9
  replace year=`i' if FuelYear==`i'&month<=5
  }
  
drop if year==0

keep if padd_l1==10|padd_l1==20|padd_l1==30|padd_l1==40|padd_l1==50
bysort padd_l1 year:egen pfuel_paddl1_lastSepMay=mean(pfuel_padd)
keep padd_l1 year  pfuel_paddl1_lastSepMay

duplicates drop padd_l1 year, force
rename pfuel_paddl1_lastSepMay pfuel

reshape wide pfuel, i(padd_l1) j(year)

merge 1:m padd_l1 using "$dt\padd_statefips.dta", nogen
drop state stateabbr padd_*

reshape long pfuel, i(statefips) j(year)

save "$dt\pfuel_paddl1_lastSepMay.dta",replace
//data structure: Year-PADD level;the data is at PADD level 1.Year here corresponds to the Year in Herbicide Consumption dataset.



*--------------------------------resistant weeds-------------------------------*
import excel "wcount\Resistant_Weeds.xlsx", sheet("Sheet1") firstrow clear
keep if HerbicideSiteofAction=="Photosystem II inhibitors (C1/5)" ///atrazine & metolachlor
	|HerbicideSiteofAction=="Long chain fatty acid inhibitors (K3/15)" 	///acetochlor
	|HerbicideSiteofAction=="EPSP synthase inhibitors (G/9)" //glyphosate
gen herb="glyp" if HerbicideSiteofAction=="EPSP synthase inhibitors (G/9)"
replace herb="comp" if HerbicideSiteofAction=="Photosystem II inhibitors (C1/5)"|HerbicideSiteofAction=="Long chain fatty acid inhibitors (K3/15)" 
gen wcount_add=1
collapse (count) wcount_add, by(FirstYearIdentified herb State)

rename State state
rename FirstYearIdentified year
reshape wide wcount_add, i(year state) j(herb) string
merge m:1 state using "$dt\statefips.dta", nogen keep(matched master)

tsset statefips year
tsfill, full
drop state stateabbr

for var wcount_add*: replace X=0 if X==.

local herb "glyp comp"
foreach i of local herb{
	bysort statefips (year) : gen wcount_cum_`i' = sum(wcount_add`i')
}

save "$dt\wcount_use", replace //year+statefips

*--------------------------------------seed price------------------------------*
import delimited "ISUTraitTrakData_NEW.csv", clear

*****prepare data
keep if year>2009
gen 	GlyTol=0
replace GlyTol=1 if strmatch(seedtraitcollapsed, "*Gly Tol*") 
order GlyTol seedtraitcollapsed

rename 	county countyfips
rename 	yearcode year
merge m:1 countyfips using "$dt\state_crd_county.dta"
keep 	if _merge==3
drop 	_merge
gen na=1

local admin "statefips na"
foreach a of local admin{
	forval i=0/1{
	preserve
	keep 	if GlyTol==`i'
	collapse (sum) expenditures units, by(year `a')
	gen 	punit_`i'_`a'=expenditures/units
	
	drop expenditures units

	save "$dt\punit_`i'_`a'.dta",replace
	restore
	}
}


use "$dt\punit_1_statefips.dta",clear
merge 1:1 year statefips using "$dt\punit_0_statefips.dta", nogen
merge m:1 year using "$dt\punit_1_na.dta", nogen
merge m:1 year using "$dt\punit_0_na.dta", nogen
drop na
rename punit_1_statefips punit_gt_statefips
rename punit_0_statefips punit_ngt_statefips
rename punit_1_na punit_gt_na
rename punit_0_na punit_ngt_na

save "$dt\price_seed.dta", replace //year-statefips



*------------------merge dataset and variables for farm level data-------------*
local name "plotid idnum"

foreach j of local name { //control variables
	use "$dt\temp_estim_`j'", clear

	merge m:1 countyfips using "$dt\state_crd_county.dta", nogenerate keep(match master) //merge with region variables: county, crd, state. 
	drop currentlybeingused2histori
	merge m:1 year statefips using "$dt\pfuel_paddl1_lastSepMay.dta", nogen keep(match master) //merge with padd variables.
	merge m:1 year statefips using "$dt\wcount_use.dta", nogenerate keep(master match) //wcount, resistant weed count for gly and composite
	merge m:1 year using "$dt\def_fac_10.dta", nogenerate keep(master match) //def_factor, deflation factor, 2010 as base year	
	merge m:1 countyfips using "$dt\hel8,dta", nogenerate keep(master match) //hel8, highly erodible land
	merge m:1 year statefips using "$dt\price_seed.dta", nogenerate keep(master match) //pgt, seed price
	merge 1:1 year idnum using "$dt\consp_idnum_idnum", nogenerate keep(master match)
	merge m:1 year scrd using "$dt\fisher_glyp_CRDmean_deflated.dta",keep(matched master) nogenerate 
	merge m:1 year scrd using "$dt\fisher_comp_CRDmean_deflated.dta",keep(matched master) nogenerate 

	save,replace
}

*------------------------export data for estimation----------------------------*
use "$dt\temp_estim_idnum", clear //estimate using farm level dataset
keep if year>2009

for var exp_*: replace X = 0 if X==.
for var wcount_*: replace X = 0 if X==.

gen lnp=ln(Findex_glyp/Findex_comp)

local chem "glyp comp"
foreach i of local chem{
	replace exp_`i'_idnum=0 if exp_`i'_idnum==.
	}

gen exp_total_idnum=exp_glyp_idnum+exp_comp_idnum

foreach i of local chem{
	gen s_`i'=exp_`i'_idnum/exp_total_idnum
	}

drop if exp_total_idnum==0 //drop farms that use neither herbicide; only 641 out of 30，362 obs.
gen till=convt
gen wcount=wcount_cum_gly-wcount_cum_comp
gen pfuel=pfuel/def_factor
gen pgt=punit_gt_statefips/def_factor
replace pgt=punit_gt_na/def_factor if pgt==.

local basevar "lnp gt till wcount pgt bt pfuel hel8"
nmissing s_gly `basevar'
egen miss=rowmiss(s_gly `basevar')
drop if miss

save "estim_idnum_10.dta", replace










