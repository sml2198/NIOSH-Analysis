/********************************************************************************
********************************************************************************

NIOSH Project 2014-N-15776
Designing a Statistical Algorithm for Strategic Identification and Development 
of New Mine Safety Technologies and Technological Applications

Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

10 - Fit Models
	* fits preferred models and robustness checks
	* fits weak and strong null models
	* fits targeting (predictive) algorithms and specification tests
		* inputs
			* defined dynamically in file 
				* injury-specific datasets (line 109)
		* outputs
			* defined dynamically in file 
				* model-specific .csvs and .tex files containing the significant 
					* violations (if any) from prefered models
				* model-specific .dtas containing results (predictions) generated
					* by targeting algorithms

Coded by Sarah Levine, sarah.michael.levine@gmail.com
Last edit 1/19/17

********************************************************************************
********************************************************************************/

* include "/NIOSH-Analysis/data"
* include "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"
include "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"

/****** SETTINGS **************************/
pause off
set matsize 11000, perm

/********************************************************************************
********************************************************************************/
	
*+- LOCALS THAT HAVE TO BE SET FOR ROBUSTNESS ANALYSES

/****** LAG FORMS *************************/
local lag_levels "1 4" // preferred models 
*local lag_levels "3 5" //  preferred model robustness check 

/****** TRAIN/TEST SPLIT ******************/
*local train_test_split "2010" // targeting algorithms robustness check 
*local train_test_split "2011" // targeting algorithms robustness check 
local train_test_split "2012" // targeting algorithms
*local train_test_split "2013" // targeting algorithms robustness check 
*local train_test_split "2014" // targeting algorithms robustness check 

/*** UNION/LONGWALL SPECIFICATION TEST ****/
/* Includes "longwall" and "union" indicators  - you MUST  have access to EIA and
 NIOSH data for this test to work! This will also turn off the option to run the
 targeting algorithms, since we don't do this robustness assessment on those analyses. */
* local specification_check "on" 
local specification_check "off"

/* PRODUCE TABLES WITH ADDITIONAL COVARIATES? */
local report_add_covars "off" // preferred models
* local report_add_covars "on" // if you want to produce tables reports all model covariates EXCEPT significant subparts

/*********** RUN NULL MODELS? *************/
local targeting_algorithms "on" // if you want to run the nulls
*local targeting_algorithms "off" // if you do NOT want to run null models (if conducting a robustness assessment)
if "`specification_check'" == "on" local targeting_algorithms "off" // never do this with the union/longwall covariates
if "`lag_levels'" == "3 5" local targeting_algorithms "off" // never do this with the union/longwall covariates

/********************************************************************************
********************************************************************************/

*+- LOCALS THAT NEVER CHANGE (even for robustness tests) 

/****** INJURY TYPES **********************/
local injury_types "PS MR"

/****** OUTCOME FORMS *********************/
local outcome_form "B C"

/****** RATE VS. COUNTS *******************/
local violation_form "rate count"

/****** COVARIATES ************************/
* time not included here, added directly into models because it is excluded from predictions model
local nonfactor_vars "dv_1lag lnoperator_time lnemployment lncoal_prod"  
local covariates "ib(freq).district i.appalachia i.safetycommittee `nonfactor_vars'"
if "`specification_check'" == "on" {
	local covariates "`covariates' union longwall"
}

/*******************************************************************************
********************************************************************************/

*+- MAKE DIRECTORIES

cap mkdir "$PROJECT_ROOT/results/stage 3/"
cap mkdir "$PROJECT_ROOT/results/stage 3/tex/"
cap mkdir "$PROJECT_ROOT/results/stage 3/csv/"
cap mkdir "$PROJECT_ROOT/results/stage 3/dta/"

/********************************************************************************
********************************************************************************/

foreach inj_type in `injury_types' {
	foreach viol_form in `violation_form' {
		
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
	
		*+- if doing the union/longwall specification test, create "ulw" subfolder, and file name extension ("_ulw")
		if "`specification_check'" != "on" local sub_folder ""
		if "`specification_check'" == "on" local sub_folder "ulw/"
		if "`specification_check'" == "on" cap mkdir "$PROJECT_ROOT/results/stage 3/tex/`sub_folder'"
		if "`specification_check'" == "on" cap mkdir "$PROJECT_ROOT/results/stage 3/csv/`sub_folder'"
		if "`specification_check'" == "on" cap mkdir "$PROJECT_ROOT/results/stage 3/dta/`sub_folder'"
		if "`specification_check'" == "on" local spec_file_ext "_ulw"
		if "`specification_check'" == "on" local title_options " (Specification Test: Union and Longwall Indicators)"

		*+- identify training (0) & test (1) set based on specified cutoff year - used for robustness assessments
		if "`train_test_split'" == "2010" local cutoff "inlist(year, "2015", "2014", "2013", "2012", "2011", "2010")"
		if "`train_test_split'" == "2011" local cutoff "inlist(year, "2015", "2014", "2013", "2012", "2011")"
		if "`train_test_split'" == "2012" local cutoff "inlist(year, "2015", "2014", "2013", "2012")"
		if "`train_test_split'" == "2013" local cutoff "inlist(year, "2015", "2014", "2013")"
		if "`train_test_split'" == "2014" local cutoff "inlist(year, "2015", "2014")"
		local cutoff_ext "_`train_test_split'"
		* set variable flags TESTING set (if set = 0, then the observation is in the training set)
		qui gen set = 1 if `cutoff'
		qui replace set = 0 if missing(set)
		
			/****** PREPARE INDEPENDENT VARIABLE LOCALS **************/
			foreach x in 1 2 3 4 5 6 7 8 9 0 {
				foreach var of varlist sp*`x'_1lag  {
					local sp_count_lag_1_vars `sp_count_lag_1_vars' `var'
				}
				foreach var of varlist sp*`x'_c3lag  {
					local sp_count_lag_3_vars `sp_count_lag_3_vars' `var'
				}
				foreach var of varlist sp*`x'_c4lag  {
					local sp_count_lag_4_vars `sp_count_lag_4_vars' `var'
				}
				foreach var of varlist sp*`x'_c5lag  {
					local sp_count_lag_5_vars `sp_count_lag_5_vars' `var'
				}
			}
			pause "complete: violation variable groups/locals assigned"
			
			*+- format violations variables as rates (violations per onsite inspection hour)
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

			/****** LOOP THROUGH LAGS LEVELS AND BINARY/COUNT OUTCOMES FOR PREFERRED AND NULL MODELS ********/	
			pause "beginning outcome and lag loops"
			foreach outcome in `outcome_form' {	
				foreach lag in `lag_levels' {
							
					*+- set locals for models, dependent variables, and exposure terms
					if "`outcome'" == "C" {
						local model "nbreg"
						local depvar "dv"
						local suffix "exposure(hours) irr" // exposure(var) = offset(ln(var)) 
						local outcome_label "Count Outcome"
					}
					else {
						local model "probit"
						local depvar "dv_indicator"
						local suffix "offset(lnhours)" // exposure(var) = offset(ln(var)) 
						local outcome_label "Binary Outcome"
					}
					
					*+- set locals for covariates of interest
					if "`lag'" == "1" local cov_of_interest "`sp_count_lag_1_vars'"	
					if "`lag'" == "3" local cov_of_interest "`sp_count_lag_3_vars'"
					if "`lag'" == "4" local cov_of_interest "`sp_count_lag_4_vars'"	
					if "`lag'" == "5" local cov_of_interest "`sp_count_lag_5_vars'"
					
					*+- set locals & file extensions for covariates of interest if doing a lag 3/5 robustness assessment
					if "`lag'" == "3" local sub_folder "lag_3/"
					if "`lag'" == "5" local sub_folder "lag_5/"
					if ("`lag'" == "3" | "`lag'" == "5") cap mkdir "$PROJECT_ROOT/results/stage 3/tex/`sub_folder'"
					if ("`lag'" == "3" | "`lag'" == "5") cap mkdir "$PROJECT_ROOT/results/stage 3/csv/`sub_folder'"
					if ("`lag'" == "3" | "`lag'" == "5") cap mkdir "$PROJECT_ROOT/results/stage 3/dta/`sub_folder'"
					
					*+- set file and table title options if using violation counts (not a rate)
					if "`viol_form'" == "rate" local file_ext "_VR"
					if "`viol_form'" == "count" local file_ext "_VC"
					if "`viol_form'" == "rate" local title_options " (Rate)"
					if "`viol_form'" == "count" local title_options " (Not a Rate)"
					
					*+- set locals for table titles 
					if "`lag'" == "1" local title_lag_level "1 Yr"
					if "`lag'" == "3" local title_lag_level "3 Cumulative Yrs"
					if "`lag'" == "4" local title_lag_level "4 Cumulative Yrs"
					if "`lag'" == "5" local title_lag_level "5 Cumulative Yrs"
					
					*+- set locals for table notes in preferred models
					local add_space " "
						*+- note 1
					if "`outcome'" == "C" local note_1 "\textbf{Dependent variable}: Total number of `inj_type' injuries per mine-year"
					if "`outcome'" == "B" local note_1 "\textbf{Dependent variable}: Indicator as to whether any `inj_type' injuries occured in a given mine-year"
						*+- note 2
					if "`outcome'" == "C" local note_2 "\textbf{Model}: The model is a negative binomial. Standard errors are clustered on mines"
					if "`outcome'" == "B" local note_2 "\textbf{Model}: The model is a logit. Standard errors are clustered on mines"
						*+- note 3
					if "`viol_form'" == "count" & "`lag'" == "1" local note_3 "\textbf{Covariates of interest}: Violation variables are lagged by one-year"
					if "`viol_form'" == "count" & "`lag'" != "1" local note_3 "\textbf{Covariates of interest}: Violation variables represent the sum of all violations over the past `lag' years"
					if "`viol_form'" == "rate" & "`lag'" == "1" local note_3 "\textbf{Covariates of interest}: Violation rate variables are lagged by one-year"
					if "`viol_form'" == "rate" & "`lag'" != "1" local note_3 "\textbf{Covariates of interest}: Violation rate variables represent the sum of all violations over the past `lag' years"
						*+- note 4
					local note_4 "\textbf{Unit of analysis}: Mine-years"
						*+- note 5
					if ("`specification_check'" == "off" & "`null_plus'" == "off") local note_5 "\textbf{Additional covariates}: See Appendix A: Model Covariates"
					if ("`specification_check'" == "off" & "`null_plus'" == "on") local note_5 "\textbf{Additional covariates}: See Appendix A: Model Covariates. Total annual violations are also included."						
						*+- note 6
					if "`specification_check'" == "on" local note_6 "\textbf{Additional covariates}: See Appendix A: Model Covariates. Union and longwall indicators also included."
						*+- note 7
					local note_7 "\textbf{Exposure term}: Total hours worked per mine-year"
						*+- note 8
					if "`lag'" == "1" local note_8 "\textbf{Sample}: Mine-years for which we do not have data from the previous year are excluded"
					if "`lag'" != "1" local note_8 "\textbf{Sample}: Mine-years for which we do not have data from the previous `lag' years are excluded"
						*+- note 9
					local note_9 "\textbf{Notes}: Only `inj_type' relevant subparts with significant coefficients are reported"
						*+- note 10
					if "`outcome'" == "C" local note_10 "\textbf{Interpretation:} Coefficients are presented as IRRs. * p \textless  .05, ** p \textless  .01, *** p \textless  .001"
					if "`outcome'" == "B" local note_10 "\textbf{Interpretation:} Odds ratios are presented. * p \textless  .05, ** p \textless  .01, *** p \textless  .001"
						
					*+- now pull it all together!
					if "`specification_check'" == "off" local table_notes "addnote("`add_space'" "`note_1'" "`note_2'" "`note_3'" "`note_4'" "`note_5'" "`note_7'" "`note_8'" "`note_9'" "`note_10'")"
					if "`specification_check'" == "on" local table_notes "addnote("`add_space'" "`note_1'" "`note_2'" "`note_3'" "`note_4'" "`note_6'" "`note_7'" "`note_8'" "`note_9'" "`note_10'")"
					local null_table_notes "addnote("`add_space'" "`note_1'" "`note_2'" "`note_4'" "`note_5'" "`note_7'" "`note_10'")"
					local null_plus_table_notes "addnote("`add_space'" "`note_1'" "`note_2'" "`note_4'" "`note_5'" "`note_7'" "`note_10'")"
					
					/****** BEGINNING PREFERRED MODELS ********/
					
					eststo clear
					local cmd ""
					local cmd "`model' `depvar' `cov_of_interest' `covariates' ib(freq).time, vce(cl mineid) `suffix' iter(500)" // for inference (on all data)
					local cmd_pred "`model' `depvar' `cov_of_interest' `covariates' if set == 0, vce(cl mineid) `suffix' iter(500)" // for prediction (on just test data - no time) 
					cap noi eststo: `cmd'
					
					*+- display whether or not model converged
					if e(rc) noi di "`inj_type' `outcome' lag `lag' (`viol_form') - failed"
					else pause "after `inj_type' `outcome' lag `lag' (`viol_form') - converged"
					
					*+- if model did not converge, capture it's name in a local so we can print at the end
					local converge = e(converged)
					if "`converge'" == "0" {
						pause on
						pause "MODEL DID NOT CONVERGE: `inj_type' `outcome' lag `lag' (`viol_form')"
						local not_converged "`not_converged'; `inj_type' `outcome' lag `lag' (`viol_form')"
						pause off
					}
					
					*+- if model did converge, create a local storing a list of all significant subparts from the last model
					if "`converge'" == "1" { 
						local sig_vars ""
						local x = 1 // create a counter (used to access location of covariates in covariance matrix - covariates are returned in the order in which they enter the model)
						matrix V = e(V) // create matrix storing variance/covariance matrix
						foreach var of varlist `cov_of_interest' {
							local abs_z_score = abs(_b[`var']/sqrt(V[`x', `x']))
								if `abs_z_score' < 1.96 local sig = 0
								if `abs_z_score' >= 1.96 local sig = 1
								if V[`x', `x'] == 0 local sig = 0 // in case this covariate was omitted, we don't want it to appear as significant
								if "`sig'" == "1" local sig_vars "`var' `sig_vars'" // add variable to significant variable list if appropriate
							local x = `x' + 1 // add 1 to the counter so we progress to the next entry in the variance/covariance matrix
						}
						pause "significant variable list: `sig_vars'"
						
						*+- set locals for which variables you want to report in the latex/csv tables (does not affect null models at all)
						if "`report_add_covars'" != "on" local keep_statement "keep (`sig_vars')"
						if "`report_add_covars'" == "on" {
							local keep_statement "keep (1.appalachia 1.safetycommittee *time* *district* `nonfactor_vars')"
							local add_covars_ext "_covars" // for appendix table
						}
						
						if "`train_test_split'" == "2012" {
							*+- create tex file with estimates and csv file with only significant variable estimates (used for randomization inference)
								*+- we only do this for preferred models (not rpediction robustness assessments with different year cutoffs
							esttab using "$PROJECT_ROOT/results/stage 3/tex/`sub_folder'`inj_type'_`outcome'_`lag'`file_ext'`cutoff_ext'`spec_file_ext'`add_covars_ext'.tex", replace ///
								mlabels() label eform b(3) not booktabs longtable /// use labels, report irrs, grab coefficients and no se's
								`table_notes' noomitted noconstant nonote /// add table notes and suppress automated notes
								`keep_statement' title(`inj_type' Injuries, `outcome_label', `title_lag_level'`title_options') 
							esttab using "$PROJECT_ROOT/results/stage 3/csv/`sub_folder'`inj_type'_`outcome'_`lag'`file_ext'_sig`cutoff_ext'`spec_file_ext'`add_covars_ext'.csv", replace ///
								`keep_statement' plain p noobs wide star // no eform - we want coefficients
						}
					} // converge == 1 
					
					*+-  run model again (just on training set) to generate predictions (just on test set) and store in new variable
					if "`targeting_algorithms'" != "off" {
						noi di "`cmd_pred'"
						qui eststo: `cmd_pred'
						qui predict `inj_type'_`outcome'_`lag'_pred if set == 1
						
						*+- if a count model, capture the max number of injuries in the training period, and enforce that predictions cannot be larger than this 
						if "`outcome'" == "C" {
							qui summ dv if set == 0
							local prediction_maximum = `r(max)'
							pause "prediction maximum = `prediction_maximum'"
							replace `inj_type'_`outcome'_`lag'_pred = `prediction_maximum' if `inj_type'_`outcome'_`lag'_pred > `prediction_maximum' & !missing(`inj_type'_`outcome'_`lag'_pred)
						}
						pause "complete: `inj_type' `outcome' lag `lag' (`viol_form')"
					}
				
			local keep_statement ""
			local sig_vars ""
			local cov_of_interest ""
			} // lag level		
			
			pause "complete: `inj_type' `outcome' all lags (`viol_form')"		
			
				pause "beginning null models"
				/****** BEGINNING NULL MODELS ********/
				if "`targeting_algorithms'" != "off" {
				
				*+- run null models (on training set) to generate predictions (on test set) and store predictions in new variable
					local null_1 ""
					local null_2 ""
					local null_3 ""
				
					* WEAK NULL MODEL (1) 
						* PREFERRED SPECIFICATION, WITH NO SUBSECTION-SPECIFIC COVARIATES ON INTEREST
					eststo clear
					local null_1 "`model' `depvar' `covariates' if set == 0, vce(cl mineid) `suffix' iter(500)"
					noi di "`null_1'"
					noi eststo: `null_1'			
					qui predict `inj_type'_`outcome'_null_1 if set == 1
					if "`outcome'" == "C" {
						replace `inj_type'_`outcome'_null_1 = `prediction_maximum' if `inj_type'_`outcome'_null_1 > `prediction_maximum' & !missing(`inj_type'_`outcome'_null_1)
					}
					
					* FIRST STRONG NULL MODEL (2) 
						* THESE ALSO CONTAIN A TOTAL VIOLATIONS COVARIATE (LAGGED ONCE)
					eststo clear
					local null_2 "`model' `depvar' `covariates' totalviolations_1lag if set == 0, vce(cl mineid) `suffix' iter(500)"
					noi di "`null_2'"
					noi eststo: `null_2'			
					predict `inj_type'_`outcome'_null_2 if set == 1
						if "`outcome'" == "C" {
						replace `inj_type'_`outcome'_null_2 = `prediction_maximum' if `inj_type'_`outcome'_null_2 > `prediction_maximum' & !missing(`inj_type'_`outcome'_null_2)
					}
					
					* SECOND STRONG NULL MODEL (3) 
						* THESE ALSO CONTAIN A TOTAL VIOLATIONS/ONSITE INSPEC HOURS COVARIATE (LAGGED ONCE)
					eststo clear
					local null_3 "`model' `depvar' `covariates' total_violations_hours_1lag if set == 0, vce(cl mineid) `suffix' iter(500)"
					noi di "`null_3'"
					noi eststo: `null_3'			
					predict `inj_type'_`outcome'_null_3 if set == 1
					if "`outcome'" == "C" {
						replace `inj_type'_`outcome'_null_3 = `prediction_maximum' if `inj_type'_`outcome'_null_3 > `prediction_maximum' & !missing(`inj_type'_`outcome'_null_3)
					}
					pause "all `inj_type' `outcome' complete"	
				
				} // targeting_algoritghms
			} // outcome form (C or B)
		
		* save new data (with produced predictions as new vars) - needs to happen outside outcome loop
		if "`targeting_algorithms'" != "off" {
			drop p* sp*
			export delimited using "$PROJECT_ROOT/results/stage 3/csv/`sub_folder'`inj_type'`file_ext'`cutoff_ext'_predictions.csv", replace
		}
		
		* reset locals
		local relevant_parts "" 
		local cov_of_interest "" 
		local file_ext ""
		local sub_folder ""
		local count_vars ""
		local sp_count_vars ""
		local sp_count_lag_1_vars ""
		local sp_count_lag_3_vars ""
		local sp_count_lag_4_vars ""
		local sp_count_lag_5_vars ""
		pause "after all `inj_type' models with violation form: `viol_form'"
		
	} // violation form (rate or count)		
		
	* reset locals
	local sub_folder ""
	local file_ext ""
	local relevant_parts "" 
	local cov_of_interest "" 
	local count_vars ""
	local sp_count_vars ""
	local sp_count_lag_1_vars ""
	local sp_count_lag_3_vars ""
	local sp_count_lag_4_vars ""
	local sp_count_lag_5_vars ""
	local prediction_maximum ""
	
} // inj type

noi di "FOLLOWING MODEL(S) DID NOT CONVERGE: `not_converged'"

*end*
