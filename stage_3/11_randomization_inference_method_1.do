
/********************************************************************************
********************************************************************************

NIOSH Project 2014-N-15776
Designing a Statistical Algorithm for Strategic Identification and Development 
of New Mine Safety Technologies and Technological Applications

Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

11 - Randomization Inference Method 1
	* inputs
		* defined dynamically in file 
			* injury-specific datasets (line 118)
	* outputs
		* defined dynamically in file 
			* model-specific datasets containing one variable for each 
			* violation and one row for each iteration of the RI procedure, 
			* with each observation representing the coefficient attained 
			* for that variable on that iteration (line 261)
	* helper file
		* 11_helper_file_RI_method_1.do, called on line 69 
		* enables parallelization, as this file sends that .do file
		* ("the job") to multiple clusters on the machine
	
Coded by Sarah Levine, sarah.michael.levine@gmail.com

Last edit 2/15/17

********************************************************************************
********************************************************************************/

*+- seed note
	* the seed needs to be set INSIDE all loops that define the preferred models
	* this way, if you run several models at once, they will produce the same
	* output as if you ran one at a time

	* a further seed is set for each parallel "job", so that no matter what cluster 
	* the job is on, it will produce the same output)

*+- preferences
clear all
set more off
timer clear
pause off
set matsize 11000, perm
set trace off

*+- header
* include "/NIOSH-Analysis/data"
* include "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"
include "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"

*+- check for/install packages if missing 
capture which shufflevar
if _rc==111 ssc install shufflevar
capture which parallel
if _rc==111 {
	ssc install parallel
	net install parallel, from(https://raw.github.com/gvegayon/parallel/master/) replace
	mata mata mlib index
}
*+- make directories
cap mkdir "$PROJECT_ROOT/results/"
cap mkdir "$PROJECT_ROOT/results/dta/"
cap mkdir "$PROJECT_ROOT/results/dta/test/"

*+- define function to call randomization
program define fit_models
args var
	do "$PROJECT_ROOT/programs/stage_3/parallelize/11_helper_file_RI_method_1.do" `var'	
end

/****** ITERATIONS ************************/
global num_iterations = 1000
global max_iterations = 2000

/****** LAG FORMS *************************/
local lag_levels "1 4" 

/*** UNION/LONGWALL SPECIFICATION TEST ****/
local specification_check "off"

/****** INJURY TYPES **********************/
local injury_types "MR PS"

/****** OUTCOME FORMS *********************/
local outcome_forms "B C"

/****** RATE VS. COUNTS *******************/
local violation_forms "count rate"

/****** COVARIATES ************************/
*+- time is included (unlike in fit models) because no prediction is done here
local nonfactor_vars "dv_1lag lnoperator_time lnemployment lncoal_prod" 
global covariates "ib(freq).district ib(freq).time i.appalachia i.safetycommittee `nonfactor_vars'"
if "`specification_check'" == "on" {
	global covariates "$covariates union longwall"
	local sub_folder "ulw/"
	local ulw_ext "_ulw"
}

*+- timed_iteration let's us compare performance of serial vs. parallel computing - 2 is parallel (preferred)
local timed_iteration = 2

*+- if you are parallelizing your code, each cluster will perform num_iterations
	*+- so you must divide the desired number of coefficients by the number of clusters
if `timed_iteration' == 2 {
	local num_clusters = 4
	global num_iterations = ( $num_iterations / `num_clusters' )
	global max_iterations = ( $max_iterations / `num_clusters' )
}

foreach inj_type in `injury_types' {
	foreach viol_form in `violation_forms' {
	
		*+- set file extension for non-rate models
		local violform_ext ""
		if "`viol_form'" == "rate" local violform_ext "VR_"
		if "`viol_form'" != "rate" local violform_ext "VC_"
		
		foreach outcome in `outcome_forms' {	
		
			*+- load injury-specific datasets 
			if "`specification_check'" == "off" use "$PROJECT_ROOT/data/5_prepared/prepared_stage_3_`inj_type'_part_2.dta", clear
			if "`specification_check'" == "on" use "$PROJECT_ROOT/data/5_prepared/prepared_stage_3_`inj_type'_part_2_ulw.dta", clear
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
				foreach var of varlist dv_1lag inspectionhours_1lag inspectionhours_c3lag inspectionhours_c4lag inspectionhours_c5lag totalviolations_1lag total_violations_hours_1lag {
					qui rename `var' `var'_x
				}
				* replace vars with rates (x 1000)
				foreach var of varlist *_1lag {
					qui replace `var' = (`var'/inspectionhours_1lag_x)*1000
				}
				foreach var of varlist *_c3lag {
					qui replace `var' = (`var'/inspectionhours_c3lag_x)*1000
				}
				foreach var of varlist *_c4lag {
					qui replace `var' = (`var'/inspectionhours_c4lag_x)*1000
				}
				foreach var of varlist *_c5lag {
					qui replace `var' = (`var'/inspectionhours_c5lag_x)*1000
				}
				rename total_violations_hours_1lag_x total_violations_hours_1lag
				rename dv_1lag_x dv_1lag
				rename totalviolations_1lag_x totalviolations_1lag
				pause "complete: rate variables formatted"
			}
			
			*+- set locals for models, dependent variables, exposure terms, and irrs
			if "`outcome'" == "C" {
				global model "nbreg"
				global depvar "dv"
				global suffix "exposure(hours) irr"		
			}
			else {
				global model "probit"
				global depvar "dv_indicator"
				global suffix "offset(lnhours)"
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
				if "`lag'" == "1" global covars "`lag_1_vars'"	
				if "`lag'" == "3" global covars "`lag_3_vars'"
				if "`lag'" == "4" global covars "`lag_4_vars'"	
				if "`lag'" == "5" global covars "`lag_5_vars'"

				*+- drop existing shuffled variables (if this is not the first instance of the lag-level loop)
				cap drop *_shuffled
				pause here
				
				*+- create empty variables that will store the coefficients from each iteration (for each covariate of interest)
				foreach var of varlist $covars {
					gen x_`var'_shuffled_c = . 
				}
				pause "coefficient storage vars created"
				
				*+- run preferred models once so we can capture the sample 
					*+- the sample is injury, violation type, outcome type, and lag specific at this point
				local cmd "$model $depvar $covars $covariates, vce(cl mineid) $suffix iter(200)"
				cap qui `cmd'
				keep if e(sample)
				pause "sample captured, non-sample observations dropped, ready to iterate"
				
				/****** LOOP THAT ITERATES! ********/	
				
				*+- call file in serial
				if `timed_iteration' == 1 {
					timer on 1
					fit_models `inj_type'
					timer off 1
					
					*+- capture time
					cap log close
					log using "$PROJECT_ROOT/results/stage 3/dta/run_time_method_1.txt", text replace
					timer list
					noi di "serial duration: `r(t1)'"
					cap log close
				}
				
				*+-  call file in parallel
				if `timed_iteration' == 2 {
					parallel setclusters `num_clusters'
					timer on 2
					
					*+- set seeds (one for each cluster, so each cluster produces different but reproducible results)
					parallel, prog(fit_models): fit_models `inj_type' seeds(1 2 3 4)
					parallel clean, all force
					timer off 2
					
					*+- display performance, relative to first thang
					/*cap log close
					log using "$PROJECT_ROOT/results/stage 3/dta/run_time_method_1.txt", text append
					timer list
					noi di "parallel duration: `r(t2)'"
					pause "parallel is `=round(r(t2)/r(t1),.1)' times faster"
					cap log close*/
				}
				
				pause "iterations complete for model `inj_type' `viol_form' `outcome' `lag'"
				
				*+- find and drop rows with missing values(non-convergent observations) 
				keep *shuffled_c
				foreach var of varlist *shuffled_c {
					drop if missing(`var')
				}
				
				*+- name sub-folder (if a ulw, lag 3, or lag 5 specification test)
				if "`lag'" == "3" local sub_folder "lag_3/"
				if "`lag'" == "5" local sub_folder "lag_5/"
				
				*+- save a dta with the distribution of coefficients attained for each variable
				saveold "$PROJECT_ROOT/results/stage 3/dta/`sub_folder'`inj_type'_`outcome'_`lag'_`violform_ext'ri.dta", replace version(12)
				
				*+- restore all data, reset some locals, and begin this whole shabang again
				local lag_1_vars ""
				local lag_3_vars ""
				local lag_4_vars ""
				local lag_5_vars ""
				restore 
				pause "data restored"
					
			} // lag levels
			pause "all `inj_type' done"
		} // outcome form  
		local lag_1_vars ""
		local lag_2_vars ""
		local lag_4_vars "" 
		local lag_5_vars ""
	} // violation form (rate or count)
	local lag_1_vars ""
	local lag_2_vars ""
	local lag_4_vars "" 
	local lag_5_vars ""
} // inj type

* end*
