
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

*---------------------------final dataset for estimation-----------------------*

********************************************************************************
*							estimation: fracglm								   *
********************************************************************************
***prep
*tobs
bys id: gen tobs=_N //number of time periods for each id
tab tobs


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

cap program drop estim_boot
program estim_boot, rclass
version 15.0


	***first stage
	reg gt pgt bt lnp wcount ///
		lam* ib(last).year i.statefips i.statefips#c.T, ///
		vce(cluster scrd)
	predict v1hat, resid


	***second stage
	fhetprob s_glyp lnp wcount gt till v1hat ///
	lam* ib(last).year i.statefips i.statefips#c.T, ///
	het(lam2-lam6) iter(100) ///
	vce(cluster scrd) 
	predict xbhat, xb


	***post estimation
	//return estimates
	return scalar blnp=[#1]lnp
	return scalar bwcount=[#1]wcount
	return scalar bgt=[#1]gt
	return scalar btill=[#1]till
	return scalar bv1hat=[#1]v1hat

	//ASF and APE
	predictnl double scale=normalden(xb(#1)/exp(xb(#2)))*exp(-xb(#2)) if e(sample) 
	sum scale 
	local scale=r(mean)
	
	foreach v of varlist lnp wcount till gt  {
		gen pe`v'=scale*[#1]`v'
		return scalar ape`v'=`scale'*[#1]`v'
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
		return scalar m`i'=r(mean)
	}

drop v1hat xbhat scale pe* pr_s elas* eos 

end

bootstrap r(blnp) r(bwcount) r(bgt) r(btill) r(bv1hat) ///
	r(apelnp) r(apewcount) r(apegt) r(apetill) ///
	r(melas11) r(melas22) r(melas12) r(melas21) r(meos), cluster(scrd) ///
	reject(e(converged)!=1) reps(1000) seed(0234) idcluster(newid):estim_boot
est store boot
test _bs_5 

estadd scalar chi2=r(chi2)

cd "$dt"

esttab boot using boot2.rtf, ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	b(%9.4f) ///
	stats(chi2) ///
	replace
	
drop newid

