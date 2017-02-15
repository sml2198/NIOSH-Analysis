
/********************************************************************************
********************************************************************************

NIOSH Project 2014-N-15776
Designing a Statistical Algorithm for Strategic Identification and Development 
of New Mine Safety Technologies and Technological Applications

Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

13 - Randomization Inference Method 2
	* inputs
		* defined dynamically in file 
			* injury-specific datasets (line 126)
			* csv results from RI method 1 containing violations to test in method 2:
				* produced in 12_analyze_randomization_1.R (line 188)
	* outputs
		* defined dynamically in file (line 261) 
			* model-specific datasets containing one variable for each violation and 
			* one row for each iteration of the RI procedure, with each observation 
			* representing the coefficient attained for that variable on that iteration
	* helper file
		* 13_helper_file_RI_method_2.do, called on line 75 
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
cap mkdir "$PROJECT_ROOT/results/dta/method_2/"
cap mkdir "$PROJECT_ROOT/results/dta/method_2/test/"
cap mkdir "$PROJECT_ROOT/results/dta/ulw/method_2/"
cap mkdir "$PROJECT_ROOT/results/dta/lag_3/method_2/"
cap mkdir "$PROJECT_ROOT/results/dta/lag_5/method_2/"

*+- define function to call randomization
program define fit_models
args var
	do "$PROJECT_ROOT/programs/stage_3/parallelize/13_helper_file_RI_method_2.do" `var'	
end

/****** LAG FORMS *************************/break

local lag_levels "1 4" // preferred models 

/****** ITERATIONS ************************/
global num_iterations = 500
global max_iterations = 1600

/*** UNION/LONGWALL SPECIFICATION TEST ****/
local specification_check "off"

/****** INJURY TYPES **********************/
local injury_types "MR PS"

/****** OUTCOME FORMS *********************/
local outcome_forms "B C"

/****** RATE VS. COUNTS *******************/
local violation_forms "rate count"

/****** COVARIATES ************************/
* time is included (unlike in fit models) because no prediction is done here
local nonfactor_vars "dv_1lag lnoperator_time lnemployment lncoal_prod" 
global covariates "ib(freq).district ib(freq).time i.appalachia i.safetycommittee `nonfactor_vars'"
if "`specification_check'" == "on" {
	global covariates "$covariates union longwall"
	local sub_folder "ulw/"
	local ulw_ext "_ulw"
}

*+- timed_iteration let's us compare performance of serial vs. parallel computing - 2 is parallel (preferred)
local timed_iteration = 2
global significant_subparts ""

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
			
			* set locals for models, dependent variables, exposure terms, and irrs
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
			
			/****** BEGIN RANDOMIZATION PROCEDURE (METHOD 2) **************/
			
			foreach lag in `lag_levels' {
			
				*+- name sub-folder (if a lag 3 or lag 5 specification test - ulw subfolder is defined in header)
				if "`lag'" == "3" local sub_folder "lag_3/"
				if "`lag'" == "5" local sub_folder "lag_5/"
				
				/******* load in significant subparts *******/
				*+- this must be done before the *official* preserve/restore
				preserve
				
				*+- load in relevant results (produced in method 1)
				import delimited "$PROJECT_ROOT/results/csv/`sub_folder'`inj_type'_`outcome'_`lag'_`violform_ext'method_2_input`ulw_ext'", clear 

				*+- if there are zero observations (i.e. no significant subparts), go onto next item in loop
				if _N == 0 restore
				if _N == 0 continue
				
				*+- if not zero observations, transpose data to make subpart/violation variable names the column names 
				rename subpart _varname
				xpose, clear varname
				drop _varname
				
				*+- grab names of significant subparts from column names
				foreach var of varlist * {
					global significant_subparts "$significant_subparts `var'"
				}	
				pause "significant subparts: $significant_subparts"	

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
				if "`lag'" == "1" global covars "`lag_1_vars'"
				if "`lag'" == "3" global covars "`lag_3_vars'"
				if "`lag'" == "4" global covars "`lag_4_vars'"
				if "`lag'" == "5" global covars "`lag_5_vars'"
				
				*+- run preferred models once so we can capture the sample
				local cmd "$model $depvar $covars $covariates, vce(cl mineid) $suffix iter(100)"
				cap qui `cmd'
				keep if e(sample)
				pause "sample captured, non-sample observations dropped, ready to iterate"

				/****** LOOP THAT GOES THROUGH EACH SIGNIFICANT SUBPART! ********/
				
				*+- go through each significant supbart captured above
				foreach significant_subpart in $significant_subparts {
				
					*+- preserve data as-is (we only restore the data once an entire RI process is complete & dta is stored)
					preserve
					pause "beginning `significant_subpart' iterations"
					
					*+- create a copy of the violation/subpart of interest variable (will be used to reset this variable after each randomized iteration) 
						*+- and then create an empty variable to store the coefficient on that same violation variable from each iteration
					gen copy_`significant_subpart' = `significant_subpart'
					gen x_`significant_subpart'_c = . 
					pause "coefficient storage variables created"
					
					/****** LOOP THAT ITERATES! ********/
					
					*+- create a global for the subpart (so that it can be passed to the next file)
						*+- will be renamed as a local in the next file 
					global global_sig_subpart `significant_subpart'
					pause "$global_sig_subpart"
					
					*+- call file in serial
					if `timed_iteration' == 1 {
						timer on 1
						set seed 625
						fit_models `inj_type'
						timer off 1
						
						*+- capture time and log performance
						cap log close
						log using "$PROJECT_ROOT/results/dta/method_2/run_time_method_2_`significant_subpart'_parallel.txt", text replace
						timer list
						noi di "serial duration: `r(t1)'"
						cap log close
					}
					
					*+- call file in parallel
					if `timed_iteration' == 2 {
						parallel setclusters `num_clusters'
						timer on 2
						
						*+- set seeds (one for each cluster, so each cluster produces different but reproducible results)
						parallel, prog(fit_models): fit_models `inj_type' seeds (1 2 3 4)
						timer off 2
						parallel clean, force
						
						*+- display performance, relative to first thang, and log it
						/*cap log close
						log using "$PROJECT_ROOT/results/dta/method_2/run_time_method_2_`significant_subpart'_parallel.txt", text append
						timer list
						noi di "parallel duration: `r(t2)'"
						pause "parallel is `=round(r(t2)/r(t1),.1)' times faster"
						cap log close */
					}

					*+- find and drop rows with missing values(non-convergent observations) 
					keep *_c
					foreach var of varlist *_c {
						drop if missing(`var')
					}				
						
					*+- save a dta with the distribution of coefficients for the significant subpart of interest
					saveold "$PROJECT_ROOT/results/dta/`sub_folder'method_2/`inj_type'_`outcome'_`lag'_`violform_ext'`significant_subpart'.dta", replace version(12)

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
				global significant_subparts ""
				
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
