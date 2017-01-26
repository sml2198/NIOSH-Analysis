/********************************************************************************
********************************************************************************

NIOSH Project 2014-N-15776
Designing a Statistical Algorithm for Strategic Identification and Development 
of New Mine Safety Technologies and Technological Applications

Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

11 - Randomization Inference Method 1
	This file applies a randomization inference techniques to our data at the 
	mine-year level. Here, we randomize ALL subparts and then re-estimate all
	models 1000 times (method 1) to derive a distribution of coefficients on
	each randomized violation subpart.
	
Coded by Sarah Levine, sarah.michael.levine@gmail.com
Last edit 1/19/17

********************************************************************************
********************************************************************************/

* include "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"
include "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"

* check for/install packages if missing 
capture which shufflevar
if _rc==111 ssc install shufflevar

/********************************************************************************
********************************************************************************/

*+- SETTINGS
pause off
set seed 625
set matsize 11000, perm

/********************************************************************************
********************************************************************************/

*+- LOCALS THAT HAVE TO BE SET FOR ROBUSTNESS ANALYSES

/****** LAG FORMS *************************/
local lag_levels "1 4" // preferred models 
* local lag_levels "3 5" // robustness check

/****** ITERATIONS ************************/
local num_iterations = 1000
local max_iterations = 1200 // the file will keep running until it has # convergences equal to num_iterations, until this limit

/*** UNION/LONGWALL SPECIFICATION TEST ****/
* local specification_check "on" // includes "longwall" and "union" indicators 
local specification_check "off"

/********************************************************************************
********************************************************************************/

*+- LOCALS THAT NEVER CHANGE (even for robustness tests) 

/****** INJURY TYPES **********************/
local injury_types "MR PS"

/****** OUTCOME FORMS *********************/
local outcome_forms "C B"

/****** RATE VS. COUNTS *******************/
local violation_forms "count rate"

/****** COVARIATES ************************/
* time is included (unlike in fit models) because no prediction is done here
local nonfactor_vars "dv_1lag lnoperator_time lnemployment lncoal_prod" 
local covariates "ib(freq).district ib(freq).time i.appalachia i.safetycommittee `nonfactor_vars'"
if "`specification_check'" == "on" {
	local covariates "`covariates' union longwall"
	local sub_folder "ulw/"
	local ulw_ext "_ulw"
}

/********************************************************************************
********************************************************************************/

*+- MAKE DIRECTORIES
cap mkdir "$PROJECT_ROOT/results/"
cap mkdir "$PROJECT_ROOT/results/csv/"
cap mkdir "$PROJECT_ROOT/results/csv/ulw/"
cap mkdir "$PROJECT_ROOT/results/dta/"
cap mkdir "$PROJECT_ROOT/results/dta/lag_3/"
cap mkdir "$PROJECT_ROOT/results/dta/lag_5/"

/*******************************************************************************
********************************************************************************

// MODEL LABEL KEY

Model W.X.Y.Z
  W
	MR: Maintenance and repair injuries
	PS: Pinning and striking injuries
  X
    C: response variable is count of injuries
    B: response variable is binary of injuries 
  Z
    1: inj_t ~ viol_(t_-1)
	4: inj_t ~ viol_(t_-1 + t_-2 + t_-3 + t_-4)

*******************************************************************************
********************************************************************************/

foreach inj_type in `injury_types' {
	foreach viol_form in `violation_forms' {
	
		*+- set file extension for non-rate models
		local violform_ext ""
		if "`viol_form'" != "rate" local violform_ext "non-rate_"
		
		foreach outcome in `outcome_forms' {	
		
			*+- load injury-specific datasets 
			use "$PROJECT_ROOT/data/5_prepared/prepared_stage_3_`inj_type'_part_2.dta", clear
			pause "`inj_type' data loaded"
			
			*+- rename injury variable of interest and create list of relevant parts
			if "`inj_type'" == "PS" {
				local relevant_parts "48 75"
			}
			if "`inj_type'" == "MR" {
				local relevant_parts "47 48 71 72 75 77"
			}

			*+- format dependent variable as a rate (violations per inspection hour)
			if "`viol_form'" == "rate"  {
				* rename the denominator so that it isn't part of the loops below
				foreach var of varlist inspectionhours_1lag inspectionhours_c3lag inspectionhours_c4lag inspectionhours_c5lag {
					rename `var' `var'_x
				}
				* replace variables with rates (divided by inspection hours x 1000)
				foreach var of varlist *_1lag {
					replace `var' = (`var'/inspectionhours_1lag_x)*1000
				}
				foreach var of varlist *_c3lag {
					replace `var' = (`var'/inspectionhours_c3lag_x)*1000
				}
				foreach var of varlist *_c4lag {
					replace `var' = (`var'/inspectionhours_c4lag_x)*1000
				}
				foreach var of varlist *_c5lag {
					replace `var' = (`var'/inspectionhours_c5lag_x)*1000
				}
				pause "complete: rate variables formatted"
			}
			
			*+- set locals for models, dependent variables, exposure terms, and irrs
			if "`outcome'" == "C" {
				local model "nbreg"
				local depvar "dv"
				local suffix "exposure(hours) irr"		
			}
			else {
				local model "probit"
				local depvar "dv_indicator"
				local suffix "offset(lnhours)"
			}
			
			pause "before randomization procedure method 1"
			/****** BEGIN RANDOMIZATION PROCEDURE (METHOD 1) **************/
					
			foreach lag in `lag_levels' {
				
				pause "about to preserve data"
				*+- preserve data (we only restore once an entire RI process is complete & .dta is stored)
				preserve
			
				*+- group violation variables by lag
				foreach x in 1 2 3 4 5 6 7 8 9 0 {
					foreach var of varlist sp*`x'_1lag  {
						local lag_1_vars `lag_1_vars' `var'
					}
					foreach var of varlist sp*`x'_c3lag  {
						local lag_3_vars `lag_3_vars' `var'
					}
					foreach var of varlist sp*`x'_c4lag  {
						local lag_4_vars `lag_4_vars' `var'
					}
					foreach var of varlist sp*`x'_c5lag  {
						local lag_5_vars `lag_5_vars' `var'
					}
				}
				
				*+- assign "covars" (local containing covariates of interest) based on current lag level
				if "`lag'" == "1" local covars "`lag_1_vars'"	
				if "`lag'" == "3" local covars "`lag_3_vars'"
				if "`lag'" == "4" local covars "`lag_4_vars'"	
				if "`lag'" == "5" local covars "`lag_5_vars'"

				*+- drop existing shuffled variables (if this is not the first instance of the lag-level loop)
				cap drop *_shuffled
				
				*+- create empty variables that will store the coefficients from each iteration (for each covariate of interest)
				foreach var of varlist `covars' {
					gen x_`var'_shuffled_c = . 
				}
				pause "coefficient storage vars created"
				
				*+- run preferred models once so we can capture the sample 
					*+- the sample is injury, violation type, outcome type, and lag specific at this point
				local cmd "`model' `depvar' `covars' `covariates', vce(cl mineid) `suffix' iter(500)"
				cap qui `cmd'
				keep if e(sample)
				pause "sample captured, non-sample observations dropped, ready to iterate"
				
				/****** LOOP THAT ITERATES! ********/	
				local desired_iters = `num_iterations' 
				forval x = 1/`num_iterations' {	
					
					pause "beginning iteration number `x' of `num_iterations'"
					
					*+- reset locals 
					local converged ""
					local cov_of_interest ""
					
					*+- drop existing shuffled variables (if this is not the first instance of the loop that iterates)
					capture drop *shuffled

					*+- shuffle all covariates of interest (i.e. randomly sample without replacement)
					foreach var of varlist `covars' {
						qui shufflevar `var'
					}
					
					*+- set local for covariates of interest (now we grab the SHUFFLED variables, instead of the unshuffled variables contained in "covars")
					foreach var of varlist *_shuffled {
						local cov_of_interest "`cov_of_interest' `var'"
					}
					pause "complete: variables-of-interest local assigned"
						
					/****** THE MODEL! ********/	
					local cmd "`model' `depvar' `cov_of_interest' `covariates', vce(cl mineid) `suffix' iter(500)"
					cap noi `cmd'
					pause "model number `x' run"
					
					*+- create a local macro containing a binary value indicating whether or not the regression converged
					local converged = e(converged)
							
					*+- count the numver of covariates/subparts of interest (so we know how many positions in the matrix to look for)
					qui describe `cov_of_interest'
					local count: word count `cov_of_interest' 
					
					*+- grab the coefficient on each shuffled variable of interest & replace each corresponding _c var with the proper coefficient
					sort mineid year
					if "`converged'" == "1" {
						foreach var of varlist `cov_of_interest' {
							qui replace x_`var'_c = _b[`var'] in `x'
						}
						pause "coefficients should now be stored in variables"
						local convergence_count = `convergence_count' + 1
					}
					
					*+- if the number of iterations has been achieved, but not enough converged iterations have been achieved, keep adding 1 to the number of iterations
					if ("`x'" >= "`desired_iters'") & ("`convergence_count'" < "`desired_iters'") & ("`x'" < "`max_iterations'") {
						local num_iterations = `num_iterations' + 1
						pause "added one to iterations: `num_iterations'"
					}
					if ("`x'" == "`max_iterations'") {
						noi di "`x' (max) iterations completed, convergence not achieved"
						keep in 1/`x'
						keep *shuffled_c
					}
					if "`convergence_count'" != "`num_iterations'" noi di "not finished! on iteration `x' with `convergence_count' converged"
					if "`convergence_count'" == "`num_iterations'" {
						noi di "finished! in `x' iterations"
						keep in 1/`num_iterations'
						keep *shuffled_c
					}
					
				} // num iterations
				pause "iterations complete for model `inj_type' `viol_form' `outcome' `lag'"
				
				*+- name sub-folder (if a ulw, lag 3, or lag 5 specification test)
				if "`lag'" == "3" local sub_folder "lag_3/"
				if "`lag'" == "5" local sub_folder "lag_5/"
				
				*+- save a dta with the distribution of coefficients attained for each variable
				saveold "$PROJECT_ROOT/results/dta/`sub_folder'`inj_type'_`outcome'_`lag'_`violform_ext'ri.dta", replace version(12)
				
				*+- restore all data, reset some locals, and begin this whole shabang again
				local lag_1_vars ""
				local lag_3_vars ""
				local lag_4_vars ""
				local lag_5_vars ""
				local covars ""
				local cov_of_interest ""
				local convergence_count ""
				local num_iterations = `desired_iters'
				restore 
				pause "data restored"
					
			} // lag levels
			pause "all `inj_type' done"
		} // outcome form  
		local covars ""
		local lag_1_vars ""
		local lag_2_vars ""
		local lag_4_vars "" 
		local lag_5_vars ""
	} // violation form (rate or count)
	local covars ""
	local lag_1_vars ""
	local lag_2_vars ""
	local lag_4_vars "" 
	local lag_5_vars ""
} // inj type

*end*
