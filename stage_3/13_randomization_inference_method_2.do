/********************************************************************************
********************************************************************************

NIOSH Project 2014-N-15776
Designing a Statistical Algorithm for Strategic Identification and Development 
of New Mine Safety Technologies and Technological Applications

Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

13 - Randomization Inference Method 2
	This file applies two randomization inference techniques to our data at the 
	mine-year level. Here, we randomize each subpart found to be significant in 
	the first (method 1) randimization inference procedure 1000 times ONE AT A TIME.
	
Coded by Sarah Levine, sarah.michael.levine@gmail.com
Last edit 1/25/17

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
local num_iterations = 500
local max_iterations = 600 // the file will keep running until it has # convergences equal to num_iterations, until this limit

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
cap mkdir "$PROJECT_ROOT/results/dta/method_2/"
cap mkdir "$PROJECT_ROOT/results/dta/ulw/method_2/"
cap mkdir "$PROJECT_ROOT/results/dta/lag_3/method_2/"
cap mkdir "$PROJECT_ROOT/results/dta/lag_5/method_2/"

/*******************************************************************************
********************************************************************************/

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
				foreach var of varlist dv_1lag inspectionhours_1lag inspectionhours_c3lag inspectionhours_c4lag inspectionhours_c5lag {
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
			
			* set locals for models, dependent variables, exposure terms, and irrs
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
			
			/****** BEGIN RANDOMIZATION PROCEDURE (METHOD 2) **************/
			
			foreach lag in `lag_levels' {
			
				*+- name sub-folder (if a lag 3 or lag 5 specification test - ulw subfolder is defined in header)
				if "`lag'" == "3" local sub_folder "lag_3/"
				if "`lag'" == "5" local sub_folder "lag_5/"
				
				/******* load in significant subparts *******/
				*+- this must be done before the *official* preserve/restore
				preserve
				
				*+- load in relevant results (produced in method 1)
					*+- transpose data to make subpart/violation variable names the column names 
				import delimited "$PROJECT_ROOT/results/csv/`sub_folder'`inj_type'_`outcome'_`lag'_`violform_ext'method_2_input`ulw_ext'", clear 
				rename subpart _varname
				xpose, clear varname
				drop _varname
				
				*+- grab names of significant subparts from column names
				foreach var of varlist * {
					local significant_subparts "`significant_subparts' `var'"
				}	
				pause "significant subparts: `significant_subparts'"	
				
				*+- bring back MR/PS data (now significant subparts are captured in a local)
				restore
			
				*+- make lag-specific locals for covariates of interest
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
				if "`lag'" == "1" local covars "`lag_1_vars'"
				if "`lag'" == "3" local covars "`lag_3_vars'"
				if "`lag'" == "4" local covars "`lag_4_vars'"
				if "`lag'" == "5" local covars "`lag_5_vars'"
				
				*+- run preferred models once so we can capture the sample
				local cmd "`model' `depvar' `covars' `covariates', vce(cl mineid) `suffix' iter(50)"
				cap qui `cmd'
				keep if e(sample)
				pause "sample captured, non-sample observations dropped, ready to iterate"

				/****** LOOP THAT GOES THROUGH EACH SIGNIFICANT SUBPART! ********/
				
				*+- go through each significant supbart captured above
				foreach sig_subpart in `significant_subparts' {
				
					*+- preserve data as-is (we only restore the data once an entire RI process is complete & dta is stored)
					preserve
					pause "beginning `sig_subpart' iterations"
					
					*+- create a copy of the violation/subpart of interest variable (will be used to reset this variable after each randomized iteration) 
						*+- and then create an empty variable to store the coefficient on that same violation variable from each iteration
					gen copy_`sig_subpart' = `sig_subpart'
					gen x_`sig_subpart'_c = . 
					pause "coefficient storage variables created"
					
					/****** LOOP THAT ITERATES! ********/	
					
					/*+- "desired iterations" is equal to how many coefficients (from converged iterations) we want
						*+- we create this because we will be adding to the num_iterations local whenever we have a non-convergent iteration
					local desired_iters = `num_iterations' 
					forval x = 1/`num_iterations' {	*/
					
					local x = 0 // current number in loop
					local convergence_count = 0  // number of successful/convergent iterations
				
					while (`convergence_count' < `num_iterations' & `x' < `max_iterations') {
					
						pause "beginning iteration number `x' of `num_iterations'"
						
						*+- first, add one to x (to keep track of number of completed loops/iterations, regardless of outcome - this starts at zero)
						local x = (`x' + 1)
						
						*+- reset the subpart/violation of interest (set it equal to its real value)
						replace `sig_subpart' = copy_`sig_subpart'
						local converged ""

						*+- shuffle covariate of interest
						cap drop *_shuffled
						qui shufflevar `sig_subpart'
						replace `sig_subpart' = `sig_subpart'_shuffled
							
						/****** THE MODEL! ********/	
						
						local cmd "`model' `depvar' `covars' `covariates', vce(cl mineid) `suffix' iter(200)"
						noi di "`cmd'"
						cap noi `cmd'
						pause "model number `x' run"
						
						*+- create a local macro containing a binary value indicating whether or not the regression converged
						local converged = e(converged)
						
						*+- sort (stably) on mineid and year so storing the coefficients can be done according to iteration number
						sort mineid year
					
						*+- grab the coefficient on each shuffled variable of interest & replace each corresponding _c var with the proper coefficient
						if "`converged'" == "1" {
							qui replace x_`sig_subpart'_c = _b[`sig_subpart'] in `x'
							pause "coefficients should now be stored in vars"
							local convergence_count = `convergence_count' + 1
						}
						
						*+- if still in progress
						if "`convergence_count'" != "`num_iterations'" noi di "not finished! on iteration `x' with `convergence_count' converged"

						*+- if iteration limit has been reached
						if ("`x'" == "`max_iterations'") {
							noi di "`x' (max) iterations completed, convergence NOT achieved `num_iterations' times"
							keep in 1/`x'
							keep *_c
						}
						
						*+- if finished in less than the maximum number of iterations
						if "`convergence_count'" == "`num_iterations'" {
							noi di "finished! in `x' iterations"
							keep in 1/`x'
							keep *_c
						}
						
					} // while statement - loop that iterates

					*+- find and drop rows with missing values(non-convergent observations) 
					keep *_c
					foreach var of varlist *_c {
						drop if missing(`var')
					}				
						
					*+- save a dta with the distribution of coefficients for the significant subpart of interest
					saveold "$PROJECT_ROOT/results/dta/`sub_folder'method_2/`inj_type'_`outcome'_`lag'_`violform_ext'`sig_subpart'.dta", replace version(12)

					*+- restore data and reset all subpart specific things/counters
					restore 
					pause "data and locals restored"
					
				} // sig subparts
			
				* reset everything
				local covars ""
				local lag_1_vars ""
				local lag_2_vars ""
				local lag_4_vars "" 
				local lag_5_vars ""
				
				*+- append all of the subpart files into one
				foreach sig_subpart in `significant_subparts' {
					append using "$PROJECT_ROOT/results/dta/`sub_folder'method_2/`inj_type'_`outcome'_`lag'_`violform_ext'`sig_subpart'.dta"
				}
				local significant_subparts ""
				
			} // lag levels
			local covars ""
			local lag_1_vars ""
			local lag_2_vars ""
			local lag_4_vars "" 
			local lag_5_vars ""
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
