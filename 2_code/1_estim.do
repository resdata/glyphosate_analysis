
clear
set more off 

global root "GitHub_2104"
global dd "$root\1_data\1_source"
global dc "$root\2_code"
global do "$root\3_output" //need to create
global dt "$root\4_temp" //need to create

********************************************************************************
*							prepare dataset for estimation					   *
********************************************************************************
use "$dd\estim_idnum_10", clear


********************************************************************************
*							estimation: fracglm								   *
********************************************************************************
***prep
*tobs
bys id: gen tobs=_N //number of time periods for each id
tab tobs


*---------------------------model 1: endo gt and till--------------------------*

local z1var "lnp wcount"
local z2var "pgt bt pfuel hel8"
local y1 "gt"
local y2 "till"

forvalues i=2/7{
	gen lam`i'=tobs==`i'
} //number of time periods dummy indicator

foreach v of varlist `z1var' `z2var' yr* st* {

	egen `v'bar=mean(`v'),by(idnum) //zbar
	
	forvalues i=2/7{
		gen lam`i'_`v'bar=lam`i'*`v'bar //g*zbar
	}
}

//step 1
reg `y1' `z2var' `z1var' ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	vce(cluster scrd)
est store m1y1
test `z2var'
local f1=r(F)
predict v1hat, resid


reg `y2' `z2var' `z1var' ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	vce(cluster scrd)
est store m1y2
test `z2var'
local f2=r(F)
predict v2hat, resid


//step 2
fhetprob s_glyp `z1var' `y1' `y2' v1hat v2hat ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) ///
	vce(cluster scrd)
	
predict xbhat, xb

test v1hat v2hat



//ASF and APE
	predictnl double scale=normalden(xb(#1)/exp(xb(#2)))*exp(-xb(#2)) if e(sample) 
	sum scale 
	local scale=r(mean)
	
	foreach v of varlist lnp wcount till gt  {
		gen pe`v'=scale*[#1]`v'
	}
	
	//elas
	
	gen pr_s=normal(xbhat)
	gen elas11=(pelnp/pr_s)+pr_s-1
	gen elas22=pelnp/(1-pr_s)-pr_s
	gen elas12=-elas11
	gen elas21=-elas22
	gen eos=1-pelnp/(pr_s*(1-pr_s))

	local elas "elas11 elas22 elas12 elas21 eos"
	foreach i of local elas{
		sum `i'
		local `i'=r(mean)
	}

cd "$do"

outreg2 using second, word replace ///Second stage result table (main manuscript)
	keep (`z1var' `y1' `y2' v1hat v2hat) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes, ///
			"Own elas. of Gly.", `elas11', ///
			"Own elas. of Comp.", `elas22', ///
			"AES", `eos') ///
	addstat("F-statistic (GT)", `f1', ///
			"F-statistic (Till)", `f2', ///
			"N", e(N)) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2)
//Table 1, standard errors need to be adjusted using bootstrapping

	
outreg2 using app_second, word replace ///Second stage result table (supplementary material)
	keep (`z1var' `y1' `y2' v1hat v2hat  *_lnpbar *_wcountbar *_pgtbar *_btbar *_pfuelbar *_hel8bar lam2 lam3 lam4 lam5 lam6) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 	
//Table S5
	
drop v1hat v2hat lam* *bar xbhat scale pe* pr_s elas* eos 



*---------------------------model 2: endo gt --------------------------*

local z1var "lnp wcount till"
local z2var "pgt bt"
local y1 "gt"


forvalues i=2/7{
	gen lam`i'=tobs==`i'
} //number of time periods dummy indicator

foreach v of varlist `z1var' `z2var' yr* st* {

	egen `v'bar=mean(`v'),by(idnum) //zbar
	
	forvalues i=2/7{
		gen lam`i'_`v'bar=lam`i'*`v'bar //g*zbar
	}
}

//step 1
reg `y1' `z2var' `z1var' ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	vce(cluster scrd)
est store m2y1
test `z2var'
local f1=r(F)
predict v1hat, resid

	//overidentification test
	ivregress 2sls s_glyp `z1var'  ///
	lam* ib(last).year i.statefips#c.T i.statefips (`y1'=`z2var'), ///
	robust
	estat overid
	local score1=r(score)
	local pscore1=r(p_score)

//step 2
fhetprob s_glyp `z1var' `y1' v1hat ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) ///
	vce(cluster scrd)

	
predict xbhat, xb

test v1hat 



//ASF and APE
	predictnl double scale=normalden(xb(#1)/exp(xb(#2)))*exp(-xb(#2)) if e(sample) 
	sum scale 
	local scale=r(mean)
	
	foreach v of varlist lnp wcount till gt  {
		gen pe`v'=scale*[#1]`v'
	}
	
	//elas
	
	gen pr_s=normal(xbhat)
	gen elas11=(pelnp/pr_s)+pr_s-1
	gen elas22=pelnp/(1-pr_s)-pr_s
	gen elas12=-elas11
	gen elas21=-elas22
	gen eos=1-pelnp/(pr_s*(1-pr_s))

	local elas "elas11 elas22 elas12 elas21 eos"
	foreach i of local elas{
		sum `i'
		local `i'=r(mean)
	}
	
cd "$do"
outreg2 using second, word append ///
	keep (`z1var' `y1' v1hat) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes, ///
			"Own elas. of Gly.", `elas11', ///
			"Own elas. of Comp.", `elas22', ///
			"AES", `eos') ///
	addstat("F-statistic (GT)", `f1', ///
			"score chi-squared (GT)", `score1', ///
			"p-value for score (GT)", `pscore1', ///
			"N", e(N)) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 

outreg2 using app_second, word append ///Second stage result table (supplementary material)
	keep (`z1var' `y1' v1hat *_lnpbar *_wcountbar *_tillbar *_pgtbar *_btbar lam2 lam3 lam4 lam5 lam6) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 



drop v1hat lam* *bar  xbhat scale pe* pr_s elas* eos 



*---------------------------model 3: endo till --------------------------*

local z1var "lnp wcount gt"
local z2var "pfuel hel8"
local y2 "till"

forvalues i=2/7{
	gen lam`i'=tobs==`i'
} //number of time periods dummy indicator

foreach v of varlist `z1var' `z2var' yr* st* {

	egen `v'bar=mean(`v'),by(idnum) //zbar
	
	forvalues i=2/7{
		gen lam`i'_`v'bar=lam`i'*`v'bar //g*zbar
	}
}

//step 1
reg `y2' `z2var' `z1var' ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	vce(cluster scrd)
est store m3y2
test `z2var'
local f2=r(F)
predict v2hat, resid

	//overidentification test
	ivregress 2sls s_glyp `z1var'  ///
	lam* ib(last).year i.statefips i.statefips#c.T i.statefips (`y2'=`z2var'), ///
	robust
	estat overid
	local score2=r(score)
	local pscore2=r(p_score)

//step 2
fhetprob s_glyp `z1var' `y2' v2hat ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) ///
	vce(cluster scrd)
	
predict xbhat, xb

test v2hat 

//ASF and APE
	predictnl double scale=normalden(xb(#1)/exp(xb(#2)))*exp(-xb(#2)) if e(sample) 
	sum scale 
	local scale=r(mean)
	
	foreach v of varlist lnp wcount till gt  {
		gen pe`v'=scale*[#1]`v'
	}
	
	//elas
	
	gen pr_s=normal(xbhat)
	gen elas11=(pelnp/pr_s)+pr_s-1
	gen elas22=pelnp/(1-pr_s)-pr_s
	gen elas12=-elas11
	gen elas21=-elas22
	gen eos=1-pelnp/(pr_s*(1-pr_s))

	local elas "elas11 elas22 elas12 elas21 eos"
	foreach i of local elas{
		sum `i'
		local `i'=r(mean)
	}

	
cd "$do"
outreg2 using second, word append ///
	keep (`z1var' `y2' v2hat) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes, ///
			"Own elas. of Gly.", `elas11', ///
			"Own elas. of Comp.", `elas22', ///
			"AES", `eos') ///
	addstat("F-statistic (Till)", `f2', ///
			"score chi-squared (Till)", `score2', ///
			"p-value for score (Till)", `pscore2', ///
			"N", e(N)) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 

outreg2 using app_second, word append ///Second stage result table (supplementary material)
	keep (`z1var' `y2' v2hat  *_lnpbar *_wcountbar *_gtbar *_pfuelbar *_hel8bar lam2 lam3 lam4 lam5 lam6) noobs ///
	addtext("CRE", Yes, ///
			"CF", YES, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 
	
drop v2hat lam* *bar xbhat scale pe* pr_s elas* eos 


*------------------------model 4: endo none------------------------------------*
local z1var "lnp wcount gt till"

forvalues i=2/7{
	gen lam`i'=tobs==`i'
} //number of time periods dummy indicator

foreach v of varlist `z1var' yr* st* {

	egen `v'bar=mean(`v'),by(idnum) //zbar
	
	forvalues i=2/7{
		gen lam`i'_`v'bar=lam`i'*`v'bar //g*zbar
	}
}

//step 2
fhetprob s_glyp `z1var' ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) ///
	vce(cluster scrd)

predict xbhat, xb



//ASF and APE
	predictnl double scale=normalden(xb(#1)/exp(xb(#2)))*exp(-xb(#2)) if e(sample) 
	sum scale 
	local scale=r(mean)
	
	foreach v of varlist lnp wcount till gt  {
		gen pe`v'=scale*[#1]`v'
	}
	
	//elas
	
	gen pr_s=normal(xbhat)
	gen elas11=(pelnp/pr_s)+pr_s-1
	gen elas22=pelnp/(1-pr_s)-pr_s
	gen elas12=-elas11
	gen elas21=-elas22
	gen eos=1-pelnp/(pr_s*(1-pr_s))

	local elas "elas11 elas22 elas12 elas21 eos"
	foreach i of local elas{
		sum `i'
		local `i'=r(mean)

	}
	

cd "$do"
outreg2 using second, word append ///
	keep (`z1var') noobs ///
	addtext("CRE", Yes, ///
			"CF", No, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes, ///
			"Own elas. of Gly.", `elas11', ///
			"Own elas. of Comp.", `elas22', ///
			"AES", `eos') ///	addstat("N", e(N)) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 

outreg2 using app_second, word append ///Second stage result table (supplementary material)
	keep (`z1var' *_lnpbar *_wcountbar *_gtbar *_tillbar lam2 lam3 lam4 lam5 lam6) noobs ///
	addtext("CRE", Yes, ///
			"CF", No, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 	

margins, dydx(`z1var') post
outreg2 using second, append ///
	keep(`z1var') noobs ///
	eqkeep(APEs) ct("APEs") stats(coef tstat) bdec(3) 


drop lam* *bar xbhat scale pe* pr_s elas* eos 
 
*--------------------------model 5: no control variables-----------------------*
local z1var "lnp"

forvalues i=2/7{
	gen lam`i'=tobs==`i'
} //number of time periods dummy indicator

foreach v of varlist `z1var' yr* st*{

	egen `v'bar=mean(`v'),by(idnum) //zbar
	
	forvalues i=2/7{
		gen lam`i'_`v'bar=lam`i'*`v'bar //g*zbar
	}
}

//step 2
fhetprob s_glyp `z1var' ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) ///
	vce(cluster scrd)

predict xbhat, xb



//ASF and APE
	predictnl double scale=normalden(xb(#1)/exp(xb(#2)))*exp(-xb(#2)) if e(sample) 
	sum scale 
	local scale=r(mean)
	
	foreach v of varlist lnp   {
		gen pe`v'=scale*[#1]`v'
	}
	
	//elas
	
	gen pr_s=normal(xbhat)
	gen elas11=(pelnp/pr_s)+pr_s-1
	gen elas22=pelnp/(1-pr_s)-pr_s
	gen elas12=-elas11
	gen elas21=-elas22
	gen eos=1-pelnp/(pr_s*(1-pr_s))

	local elas "elas11 elas22 elas12 elas21 eos"
	foreach i of local elas{
		sum `i'
		local `i'=r(mean)

	}
	

cd "$do"
outreg2 using second, word append ///
	keep (`z1var') noobs ///
	addtext("CRE", Yes, ///
			"CF", No, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes, ///
			"Own elas. of Gly.", `elas11', ///
			"Own elas. of Comp.", `elas22', ///
			"AES", `eos') ///	addstat("N", e(N)) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 

outreg2 using app_second, word append ///Second stage result table (supplementary material)
	keep (`z1var' *_lnpbar lam2 lam3 lam4 lam5 lam6) noobs ///
	addtext("CRE", Yes, ///
			"CF", No, ///
			"Time dummies", Yes, ///
			"State dummies", Yes, ///
			"State-specific trends", Yes) ///
	nocons eq(auto) ct("coefficients") stats(coef tstat) bdec(3) tdec(2) 	
	

margins, dydx(`z1var') post
outreg2 using second, append ///
	keep(`z1var') noobs ///
	eqkeep(APEs) ct("APEs") stats(coef tstat) bdec(3) 


drop lam* *bar xbhat pe* pr_s elas* eos scale




*--------------------------export first-stage results--------------------------*
esttab m1y1 m1y2 m2y1 m3y2 using first.rtf, ///
	keep(lnp wcount gt till pgt bt pfuel hel8) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	mtitles("model 1: gt" "model 1: till" "model 2: gt" "model 3: till") ///
	title ("Table S4. First-stage regression results") ///
	b(%9.4f) ///
	replace
//Table S4
