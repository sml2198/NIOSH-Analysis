/********************************************************************************
********************************************************************************

NIOSH Project 2014-N-15776
Designing a Statistical Algorithm for Strategic Identification and Development 
of New Mine Safety Technologies and Technological Applications

Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

8 - prepare_stage_3_stata
 # Generate lagged violations and other variables

Coded by Sarah Levine, sarah.michael.levine@gmail.com
Last edit 1/19/17

********************************************************************************
********************************************************************************/

* include "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"
include "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"

/****** SETTINGS **************************/
pause off
set matsize 11000, perm

/********************************************************************************
********************************************************************************/	
local injury_types "MR PS"

*+- loop through injury types 
foreach inj_type in `injury_types' {	
		
	*+- load injury-specific datasets 
	use "$PROJECT_ROOT/data/5_prepared/prepared_stage_3_`inj_type'_part_1.dta", clear
	pause "`inj_type' data loaded"
			
	*+- rename injury variable of interest and create list of relevant parts
	if "`inj_type'" == "PS" {
		rename PS dv
	}
	if "`inj_type'" == "MR" {
		rename MR dv
	}
			
	*+- format time
	tostring(year), replace
	encode year, gen(time)
	
	*+- count orphan years
	sort mineid year
	qui gen mine_year = regexs(0) if(regexm(year, "(1[0-5]$)|([0-9]$)"))
	destring mine_year, replace
	qui bys mineid: gen orphan = (mine_year[_n] - mine_year[_n - 1]) // orphan equals the number of years between _n and _n-1 (should be 1 if no missing years between observations)
	qui bys mineid: replace orphan = 1 if (_n == 1) // set the first year at a given mine artificially equal to one (these lags aren't generated anyway)  

	*+- generate violation and injury lags for t-1, t-2, t-3, t-4, t-5, and cumulative t-3, t-4, t-5 (3 and 5 are robustness checks)
	sort mineid year
	local varlist "sp* dv inspectionhours totalviolations total_injuries" 
	foreach var of varlist `varlist' {
		* first generate lags 1 through 5 (non-cumulative)
		local numlist "1/5"
		foreach x of numlist `numlist' {
			qui by mineid: gen `var'_`x'lag = `var'[_n - `x']
			qui replace `var'_`x'lag = . if orphan != 1 // lags can only exist if there is a previous year of data 
			* by lag, pipe in missings where orphan is greater than 1 for previous years
			if `x' == 1 qui replace `var'_`x'lag = . if orphan != 1
			if `x' == 2 qui replace `var'_`x'lag = . if (orphan[_n] + orphan[_n-1]) != 2
			if `x' == 3 qui replace `var'_`x'lag = . if (orphan[_n] + orphan[_n-1] + orphan[_n-2]) != 3
			if `x' == 4 qui replace `var'_`x'lag = . if (orphan[_n] + orphan[_n-1] + orphan[_n-2] + orphan[_n-3]) != 4
			if `x' == 5 qui replace `var'_`x'lag = . if (orphan[_n] + orphan[_n-1] + orphan[_n-2] + orphan[_n-3] + orphan[_n-4]) != 5
		}
		pause "created missings in data to reflect orphan years"
		
		* now sum lags to produce cumulative lag 3, 4, 5 and then drop non-cumulative vars
		by mineid: gen `var'_c3lag = (`var'_1lag + `var'_2lag + `var'_3lag) if (!missing(`var'_1lag) & !missing(`var'_2lag) & !missing(`var'_3lag))
		by mineid: gen `var'_c4lag = (`var'_1lag + `var'_2lag + `var'_3lag + `var'_4lag) if (!missing(`var'_1lag) & !missing(`var'_2lag) & !missing(`var'_3lag) & !missing(`var'_4lag))
		by mineid: gen `var'_c5lag = (`var'_1lag + `var'_2lag + `var'_3lag + `var'_4lag + `var'_5lag) if (!missing(`var'_1lag) & !missing(`var'_2lag) & !missing(`var'_3lag) & !missing(`var'_4lag) & !missing(`var'_5lag))
		drop `var'_2lag `var'_3lag `var'_4lag `var'_5lag 
		pause "cumulative lags produced"
	}
	pause "all lags generated"
	
	*+- format hours and binary depvar, and take the log of covariates that are highly skewed
	gen dv_indicator = dv >= 1
	gen lnhours = log(hours)
	gen lnemployment = log(employment)
	gen lncoal_prod = log(prod)
	gen lnoperator_time = log(operatortime)
	
	*+- prepare total violations for strong null model (violations/hours, lagged once)
	gen total_violations_hours_1lag = (totalviolations_1lag/inspectionhours_1lag)
	
	*+- remove all var labels and re-label (we tell eststo command to grab label names after estimation because they're prettier than variable names)
	foreach var of varlist * {
		label var `var' ""
	}
	label var hours "Total annual production hours"
	label var safetycommittee "Safety committee indicator"
	label var appalachia "Appalachian state indicator"
	label var lncoal_prod "Log total annual coal production (tons)"
	label var lnemployment "Log mean annual employment at given mine"
	label var lnoperator_time "Log operator tenure at given mine"	
	label var time "Year"
	label var dv_1lag "Number of injuries in previous year"
	label var totalviolations_1lag "Number of violations in previous year (any type)"
	label var total_violations_hours_1lag "Number of violations per inspection hour in previous year (any type)"
	if "`inj_type'" == "MR" label var dv "Number of MR injuries"
	if "`inj_type'" == "MR" label var dv_indicator "MR injury indicator"
	if "`inj_type'" == "PS" label var dv "Number of PS injuries"
	if "`inj_type'" == "PS" label var dv_indicator "PS injury indicator"
	
	*+- drop unnecessary stuff
	drop orphan employment prod operatortime mine_year
	
	save "$PROJECT_ROOT/data/5_prepared/prepared_stage_3_`inj_type'_part_2.dta", replace
}
*end*
