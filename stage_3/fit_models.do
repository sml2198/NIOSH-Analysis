/********************************************************************************
********************************************************************************

NIOSH Project 2014-N-15776
Designing a Statistical Algorithm for Strategic Identification and Development 
of New Mine Safety Technologies and Technological Applications

Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

X - Fit Models
 # 

Coded by Sarah Levine, sarah.michael.levine@gmail.com
Last edit 1/19/17

********************************************************************************
********************************************************************************/

* include "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"
include "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/header.do"

/****** SETTINGS **************************/
pause on
set more off
set matsize 11000, perm
local date "1-19"

/********************************************************************************
********************************************************************************/

*+- LOCALS THAT HAVE TO BE SET FOR ROBUSTNESS ANALYSES

/****** LAG FORMS *************************/
local lag_levels "1 4" // preferred models 
* local lag_levels "3 5" // robustness check 

/****** MINE SAMPLE ***********************/
local mine_sample "some" // preferred models - only years with 4 non-missing quarters included
*local mine_sample "all" // robustness check - all data (including incomplete years) - NOT CODED YET!!!!!

/****** TRAIN/TEST SPLIT ******************/
*local train_test_split "2010" // robustness check 
*local train_test_split "2011" // robustness check 
local train_test_split "2012" // preferred models - only relevant for the predictive stage (not inference)
*local train_test_split "2013" // robustness check 
*local train_test_split "2014" // robustness check 

/*** UNION/LONGWALL SPECIFICATION TEST ****/
*local specification_check "on" // includes "longwall" and "union" indicators 
local specification_check "off"

/*********** RUN NULL MODELS? *************/
local run_nulls "on" // if you want to run the nulls (preferred)
*local run_nulls "off" // if you do NOT want to run null models  (if conducting a robustness assessment)

/* PRODUCE TABLES WITH ADDITIONAL COVARS? */
local report_add_covars "off" // preferred
* local report_add_covars "on" // if you want to produce tables reports all model covariates EXCEPT significant subparts

/********************************************************************************
********************************************************************************/

*+- MAKE DIRECTORIES

cap mkdir "$PROJECT_ROOT/results/"
cap mkdir "$PROJECT_ROOT/results/tex/"
cap mkdir "$PROJECT_ROOT/results/tex/`date'/"
cap mkdir "$PROJECT_ROOT/results/csv/"
cap mkdir "$PROJECT_ROOT/results/csv/`date'/"
cap mkdir "$PROJECT_ROOT/results/dta/"
cap mkdir "$PROJECT_ROOT/results/dta/`date'/"

/********************************************************************************
********************************************************************************/

*+- LOCALS THAT NEVER CHANGE (even for robustness tests) 

/****** INJURY TYPES **********************/
local injury_types "MR PS"

/****** OUTCOME FORMS *********************/
local outcome_form "B C"

/****** RATE VS. COUNTS *******************/
local violation_form "rate count"

/****** COVARIATES ************************/
local nonfactor_vars "dv_1lag lnoperator_time lnemployment lncoal_prod"  // time not included here, added below because excluded from predictions model
local covariates "ib(freq).district i.apalachia i.safetycommittee `nonfactor_vars'"
if "`specification_check'" == "on" {
	local covariates "`covariates' union longwall"
}

/****** LAG DEFINITIONS *******************/
* There are two ways (at least) that we could construct lagged variables, considering
* that we have "orphan years" in our data, i.e. years in the middle of a mine's 
* production history for which we have no data. In the "strict" definition of lags, we
* force lags to be NA for year (t) when there is no data in year (t-1). In the "lax" 
* definition of lags, we allow lag (t) to be equal to lag (t-2) when there is no data
* for (t-1). However, we don't allow the lag to be equal to any quarter earlier than
* (t-2). We report strict results, and use lax results as a robustness assessment. 
local lag_definitions "strict" // preferred models
*local lag_definitions "lax" // robustness assessment

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
  Y
	P: part
    SP: subpart
  Z
    1: inj_t ~ viol_(t_-1)
	4: inj_t ~ viol_(t_-1 + t_-2 + t_-3 + t_-4)

*******************************************************************************
********************************************************************************/

foreach inj_type in `injury_types' {
	foreach lag_def in `lag_definitions' {
		foreach viol_form in `violation_form' {
			
			use "$PROJECT_ROOT/data/5_prepped/prepped_`inj_type'_prediction_data.dta", clear
			pause "`inj_type' data loaded"
			
			*+- drop unnecessary variables, rename injury, and create list of relevant parts
			if "`inj_type'" == "PS" {
				drop MR
				rename PS dv
				local relevant_parts "48 75"
			}
			if "`inj_type'" == "MR" {
				drop PS
				rename MR dv
				local relevant_parts "47 48 71 72 75 77"
			}
			
			*+- format time
			tostring(year), replace
			encode year, gen(time)
			
			*+- count orphan years (will be dropped whn we produce lags)
			sort mineid year
			qui gen mine_year = regexs(0) if(regexm(year, "(1[0-5]$)|([0-9]$)"))
			destring mine_year, replace
			qui bys mineid: gen orphan = (mine_year[_n] - mine_year[_n - 1]) // orphan equals the number of years between _n and _n-1 (should be 1 if no missing years between observations)
			qui bys mineid: replace orphan = 1 if (_n == 1) // makes the first year of operations at a given mine artificially be one (these lags aren't generated anyway)
			drop mine_year   

			*+- generate violation and injury lags for t-1, t-2, t-3, t-4, t-5, and cumulative t-3, t-4, t-5 (3 and 5 are robustness checks)
			sort mineid year
			local varlist "sp* dv inspectionhours totalviolations total_injuries" 
			foreach var of varlist `varlist' {
				* gen lags 1 through 5 (non-cumulative)
				local numlist "1/5"
				foreach x of numlist `numlist' {
					qui by mineid: gen `var'_`x'lag = `var'[_n - `x']
					qui if "`lag_def'" == "strict" replace `var'_`x'lag = . if orphan != 1 // by strict definition, lags can only exist if there is a previous year of data 
					qui if "`lag_def'" == "lax" replace `var'_`x'lag = . if orphan > 2 // there are only 99 mine-years this affects 
				}
				
				* sum lags to produce cumulative lag 3, 4, 5 (sum of last 3/4/5 years) if strict lag definitions & drop non-cumulative vars
				if "`lag_def'" == "strict" by mineid: gen `var'_c3lag = (`var'_1lag + `var'_2lag + `var'_3lag)
				if "`lag_def'" == "strict" by mineid: gen `var'_c4lag = (`var'_1lag + `var'_2lag + `var'_3lag + `var'_4lag)
				if "`lag_def'" == "strict" by mineid: gen `var'_c5lag = (`var'_1lag + `var'_2lag + `var'_3lag + `var'_4lag + `var'_5lag)
				if "`lag_def'" == "strict" drop `var'_2lag `var'_3lag `var'_4lag `var'_5lag 
				
				* if using lax lag definition, generate cumulative lag 4 (sum of last four years) ONLY if all four lags are non-missing
				if "`lag_def'" == "lax" {
					qui by mineid: gen `var'_c4lag = (`var'_1lag + `var'_2lag + `var'_3lag + `var'_4lag) ///
						if !missing(`var'_1lag) & !missing(`var'_2lag) & !missing(`var'_3lag) & !missing(`var'_4lag)
					* now generate cumulative lag 4 as the sum of the past FIVE years ONLY if 4lag is already missing, AND ONLY one year is missing
					qui by mineid: replace `var'_c4lag = (`var'_1lag + `var'_2lag + `var'_3lag + `var'_4lag + `var'_5lag) ///
						if !missing(`var'_c4lag) & ///
						(missing(`var'_1lag) & (!missing(`var'_2lag) & !missing(`var'_3lag) & !missing(`var'_4lag) & !missing(`var'_5lag)) | ///
						missing(`var'_2lag) & (!missing(`var'_1lag) & !missing(`var'_3lag) & !missing(`var'_4lag) & !missing(`var'_5lag)) | ///
						missing(`var'_3lag) & (!missing(`var'_1lag) & !missing(`var'_2lag) & !missing(`var'_4lag) & !missing(`var'_5lag)) | ///
						missing(`var'_4lag) & (!missing(`var'_1lag) & !missing(`var'_2lag) & !missing(`var'_3lag) & !missing(`var'_5lag)) )
					qui drop `var'_2lag `var'_3lag `var'_4lag `var'_5lag
				}
			}
			pause "all lags generated"
			
			*+- prepare total violations for strong null model
			gen total_violations_hours = (totalviolations_1lag/inspectionhours_1lag)
			
			*+- format hours and dependent variables for binary outcome models
			gen lnhours = log(hours)
			gen dv_indicator = dv >= 1
			
			*+- take the log of covariates that are highly skewed
			gen lnemployment = log(employment)
			gen lncoal_prod = log(prod)
			gen lnoperator_time = log(operatortime)
			
			
			*+- if using the strict definition of lags, create file name extension ("_lax") - there is no extension for strict (the preferred)
			if "`lag_def'" == "lax" local file_ext "_lax"
			if "`lag_def'" == "lax" local title_options " (Lax)"
			
			*+- if doing the union/longwall specification test, create "ulw" subfolder, and file name extension ("_ulw")
			if "`specification_check'" == "on" local sub_folder "ulw/"
			if "`specification_check'" == "on" cap mkdir "$PROJECT_ROOT/results/tex/`date'/`sub_folder'"
			if "`specification_check'" == "on" cap mkdir "$PROJECT_ROOT/results/csv/`date'/`sub_folder'"
			if "`specification_check'" == "on" cap mkdir "$PROJECT_ROOT/results/dta/`date'/`sub_folder'"
			if "`specification_check'" != "on" local sub_folder ""
			if "`specification_check'" == "on" local spec_file_ext "_ulw"
			if "`specification_check'" == "on" local spec_null_file_ext "_ulw"
			if "`specification_check'" == "on" local title_options " (Specification Test: Added Union and Longwall Indicators)"
			
			*+- remove all var labels and re-label (we tell eststo to grab label names after estimation because they're prettier than var names)
			foreach var of varlist * {
				label var `var' ""
			}
			label var hours "Total annual production hours"
			label var safetycommittee "Safety committee indicator"
			label var apalachia "Appalachian state indicator"
			label var lncoal_prod "Log total annual coal production (tons)"
			label var lnemployment "Log mean annual employment at given mine"
			label var lnoperator_time "Log operator tenure at given mine"	
			label var time "Year"
			label var dv_1lag "Number of injuries in previous year"
			label var total_violations_1lag "Number of violations in previous year (any type)"
			label var total_violations_hours "Number of violations per inspection hour in previous year (any type)"
			if "`inj_type'" == "MR" label var dv "Number of M.R. injuries"
			if "`inj_type'" == "MR" label var dv_indicator "M.R. injury indicator"
			if "`inj_type'" == "PS" label var dv "Number of P.S. injuries"
			if "`inj_type'" == "PS" label var dv_indicator "P.S. injury indicator"

			*+- identify training (0) & test (1) set based on specified cutoff year
			if "`train_test_split'" == "2010" local cutoff "inlist(year, "2015", "2014", "2013", "2012", "2011", "2010")"
			if "`train_test_split'" == "2011" local cutoff "inlist(year, "2015", "2014", "2013", "2012", "2011")"
			if "`train_test_split'" == "2012" local cutoff "inlist(year, "2015", "2014", "2013", "2012")"
			if "`train_test_split'" == "2013" local cutoff "inlist(year, "2015", "2014", "2013")"
			if "`train_test_split'" == "2014" local cutoff "inlist(year, "2015", "2014")"
			if "`train_test_split'" != "2012" local cutoff_ext "_`train_test_split'"
			gen set = 1 if `cutoff'
			replace set = 0 if missing(set)
			
				/****** PREPARE INDEPENDENT VARIABLE LOCALS **************/
				foreach x in 1 2 3 4 5 6 7 8 9 0 {
					foreach var of varlist sp*`x' {
						local sp_count_vars `sp_count_vars' `var'
					}
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
				pause "complete: variable groups"
				
				*+- format dependent variable as a rate (violations per onsite inspection hour)
				if "`viol_form'" == "rate"  {
					* rename the denominator so it isn't part of the loop
					rename inspectionhours_1lag inspectionhours_1lag_x
					rename inspectionhours_c3lag inspectionhours_c3lag_x
					rename inspectionhours_c4lag inspectionhours_c4lag_x
					rename inspectionhours_c5lag inspectionhours_c5lag_x
					* replace vars with rates (x1000)
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
				}
				pause "rate vars formatted (if rate) - current violation form: `viol_form'"
				
				/****** RUN THE PREFERRED MODELS ON ALL DATA ********/	
				foreach outcome in `outcome_form' {	
					foreach viol_level in `violation_levels' {
						foreach lag in `lag_levels' {
														
								* set locals for models, depvars, exposure, and irrs
								if "`outcome'" == "C" {
									local model "nbreg"
									local depvar "dv"
									local suffix "exposure(hours) irr"
									local outcome_label "Count Outcome"
								}
								else {
									local model "probit"
									local depvar "dv_indicator"
									local suffix "offset(lnhours)"
									local outcome_label "Binary Outcome"
								}
								
								* set locals for covariates of interest
								if "`lag'" == "0" local cov_of_interest "`sp_count_vars'"
								if "`lag'" == "1" local cov_of_interest "`sp_count_lag_1_vars'"	
								if "`lag'" == "3" local cov_of_interest "`sp_count_lag_3_vars'"
								if "`lag'" == "4" local cov_of_interest "`sp_count_lag_4_vars'"	
								if "`lag'" == "5" local cov_of_interest "`sp_count_lag_5_vars'"
								
								* set locals & file extension for covariates of interest if lag 3/5 robustness asessments 
								if "`lag'" == "3" local sub_folder "lag_3/"
								if "`lag'" == "5" local sub_folder "lag_5/"
								if ("`lag'" == "3" | "`lag'" == "5") cap mkdir "$PROJECT_ROOT/results/tex/`date'/`sub_folder'"
								if ("`lag'" == "3" | "`lag'" == "5") cap mkdir "$PROJECT_ROOT/results/csv/`date'/`sub_folder'"
								if ("`lag'" == "3" | "`lag'" == "5") cap mkdir "$PROJECT_ROOT/results/dta/`date'/`sub_folder'"
								
								* set file and file options if NOT using a rate
								if "`viol_form'" == "count" local file_ext "_non-rate"
								if "`viol_form'" == "count" local title_options " (Not a Rate)"
								
								* set locals for table titles 
								local title_viol_level "Subpart-level"
								if "`lag'" == "1" local title_lag_level "1 Yr"
								if "`lag'" == "3" local title_lag_level "3 Cumulative Yrs"
								if "`lag'" == "4" local title_lag_level "4 Cumulative Yrs"
								if "`lag'" == "5" local title_lag_level "5 Cumulative Yrs"
								
								*+- set locals for table notes in preferred models
								local add_space ""
								if "`outcome'" == "C" local note_1 "\textbf{Dependent variable}: Total number of `inj_type' injuries per mine-year"
								if "`outcome'" == "B" local note_1 "\textbf{Dependent variable}: Indicator as to whether any `inj_type' injuries occured in a given mine-year"
								if "`outcome'" == "C" local note_2 "\textbf{Model}: The model is a negative binomial. Standard errors are clustered on mines"
								if "`outcome'" == "B" local note_2 "\textbf{Model}: The model is a logit. Standard errors are clustered on mines"
								if "`viol_form'" == "count" & "`lag'" == "1" local note_3 "\textbf{Covariates of interest}: Violation variables are lagged by one-year"
								if "`viol_form'" == "count" & "`lag'" == "3" local note_3 "\textbf{Covariates of interest}: Violation variables represent the sum of all violations over the past three years"
								if "`viol_form'" == "count" & "`lag'" == "4" local note_3 "\textbf{Covariates of interest}: Violation variables represent the sum of all violations over the past four years"
								if "`viol_form'" == "count" & "`lag'" == "5" local note_3 "\textbf{Covariates of interest}: Violation variables represent the sum of all violations over the past five years"
								if "`viol_form'" == "rate" & "`lag'" == "1" local note_3 "\textbf{Covariates of interest}: Violation rate variables are lagged by one-year"
								if "`viol_form'" == "rate" & "`lag'" == "3" local note_3 "\textbf{Covariates of interest}: Violation rate variables represent the sum of all violations over the past three years"
								if "`viol_form'" == "rate" & "`lag'" == "4" local note_3 "\textbf{Covariates of interest}: Violation rate variables represent the sum of all violations over the past four years"
								if "`viol_form'" == "rate" & "`lag'" == "5" local note_3 "\textbf{Covariates of interest}: Violation rate variables represent the sum of all violations over the past five years"
								local note_4 "\textbf{Unit of analysis}: Mine-years"
								if ("`specification_check'" == "off" & "`null_plus'" == "off") local note_5 "\textbf{Additional covariates}: See Appendix A: Model Covariates"
								if ("`specification_check'" == "off" & "`null_plus'" == "on") local note_5 "\textbf{Additional covariates}: See Appendix A: Model Covariates. Total annual violations are also included."						
								if "`specification_check'" == "on" local note_6 "\textbf{Additional covariates}: See Appendix A: Model Covariates. Union and longwall indicators also included."
								* I guess seven is bad luck because I forgot it. Oops. - Sarah.
								local note_8 "\textbf{Exposure term}: Total hours worked per mine-year"
								if "`lag'" == "1" local note_9 "\textbf{Sample}: Mine-years for which we do not have data from the previous year are excluded"
								if "`lag'" == "3" local note_9 "\textbf{Sample}: Mine-years for which we do not have data from the previous three years are excluded"
								if "`lag'" == "4" local note_9 "\textbf{Sample}: Mine-years for which we do not have data from the previous four years are excluded"
								if "`lag'" == "5" local note_9 "\textbf{Sample}: Mine-years for which we do not have data from the previous five years are excluded"
								local note_10 "\textbf{Notes}: Only `inj_type' relevant subparts with significant coefficients are reported"
								if "`outcome'" == "C" local note_11 "\textbf{Interpretation:} Coefficients are presented as IRRs. * p \textless  .05, ** p \textless  .01, *** p \textless  .001"
								if "`outcome'" == "B" local note_11 "\textbf{Interpretation:} Odds ratios are presented. * p \textless  .05, ** p \textless  .01, *** p \textless  .001"
								if "`specification_check'" == "off" local table_notes "addnote("`add_space'" "`note_1'" "`note_2'" "`note_3'" "`note_4'" "`note_5'" "`note_8'" "`note_9'" "`note_10'" "`note_11'")"
								if "`specification_check'" == "on" local table_notes "addnote("`add_space'" "`note_1'" "`note_2'" "`note_3'" "`note_4'" "`note_6'" "`note_8'" "`note_9'" "`note_10'" "`note_11'")"
								
								*+- set locals for which vars you want to report in the latex/csv tables (does not affect null models at all)
								if "`report_add_covars'" != "on" local tex_covars "keep (`sig_vars' 1.apalachia `nonfactor_vars') noomitted noconstant" // preferred (tex)
								if "`report_add_covars'" != "on" local csv_covars "keep (`sig_vars')" // preferred (csv)
								if "`report_add_covars'" == "on" {
									local tex_covars "keep (1.apalachia 1.safetycommittee *time* *district* `nonfactor_vars') noomitted noconstant" // for appendix table (tex)
									local csv_covars "keep (1.apalachia 1.safetycommittee *time* *district* `nonfactor_vars') " // for appendix table (csv)
									local add_covars_ext "_covars" // for appendix table (file extension for csv's and tex's)
								}
								
								*+- set locals for table notes in null models
								local null_table_notes "addnote("`add_space'" "`note_1'" "`note_2'" "`note_4'" "`note_5'" "`note_8'" "`note_11'")"
								local null_plus_table_notes "addnote("`add_space'" "`note_1'" "`note_2'" "`note_4'" "`note_5'" "`note_8'" "`note_11'")"
					
								pause "before model `inj_type' `outcome' `viol_level' lag `lag' (`viol_form')"
								
								* THE MODEL
								eststo clear
								local cmd "`model' `depvar' `cov_of_interest' `covariates' ib(freq).time, vce(cl mineid) `suffix' iter(500)" // for inference (on all data)
								local cmd_pred "`model' `depvar' `cov_of_interest' `covariates' if set == 0, vce(cl mineid) `suffix' iter(500)" // for prediction (on just test data - no time) 
								cap noi eststo: `cmd'
								
								if e(rc) noi di "`inj_type' `outcome' `viol_level' lag `lag' (`viol_form') - failed"
								local converge = e(converged)
								if "`converge'" == "0" {
									pause on
									pause "MODEL DID NOT CONVERGE: `inj_type' `outcome' `viol_level' lag `lag' (`viol_form')"
									local not_converged "`not_converged'; `inj_type' `outcome' `viol_level' lag `lag' (`viol_form')"
									pause off
								}
								if "`converge'" == "1" { 
									* create local storing a list of significant subparts (only if at subpart-level, not enough part levels to bother us)
									if "`viol_level'" == "sp" {
										local x = 1 // create counter (used to access location of covs in covariance matrix - covariates are returned in the order they enter the model)
										local sig_vars "" // make sure list is empty after last iteration
										matrix V = e(V) // create matrix storing variance/covariance matrix
										foreach var of varlist `cov_of_interest' {
											local abs_z_score = abs(_b[`var']/sqrt(V[`x', `x']))
												if `abs_z_score' < 1.96 local sig = 0
												if `abs_z_score' >= 1.96 local sig = 1
												if V[`x', `x'] == 0 local sig = 0 // in case this covariate was omitted, in which case it will be a 0
												if "`sig'" == "1" local sig_vars "`var' `sig_vars'" // add var to significant variable list
											local x = `x' + 1 // add 1 to the counter so we progress to the next entry in the var/cov matrix
										}
									noi di "significant variable list: `sig_vars'"
									}
									else local sig_vars "`cov_of_interest'"
									
									* create tex file with estimates
									esttab using "$PROJECT_ROOT/results/tex/`date'/`sub_folder'`inj_type'_`outcome'_`viol_level'_`lag'`file_ext'`cutoff_ext'`spec_file_ext'`sample_ext'`add_covars_ext'.tex", replace ///
										mlabels() label eform /// use labels, report irrs
										b(3) not booktabs longtable /// grab coefficients (3 decimal places) and no se's
										`table_notes' nonote /// add table notes and suppress automated notes
										`tex_covars' title(`inj_type' Injuries, `outcome_label', `title_viol_level', `title_lag_level'`title_options'\label{tab1})
									
									* create csv file with only significant var estimates (used for randomization inference)
									esttab using "$PROJECT_ROOT/results/csv/`date'/`sub_folder'`inj_type'_`outcome'_`viol_level'_`lag'`file_ext'_sig`cutoff_ext'`spec_file_ext'`sample_ext'`add_covars_ext'.csv", replace ///
										`csv_covars' plain p noobs wide star // no eform - we want coefficients
									
								} // else 
								
								* run model again (just on training set) to generate predictions (just on test set) and store in new variable
								noi di "`cmd_pred'"
								cap qui eststo: `cmd_pred'
								qui predict `inj_type'_`outcome'_`viol_level'_`lag'_pred if set == 1
								pause "`inj_type' `outcome' `viol_level' lag `lag' complete (`viol_form')"	
								
					} // lag level		
					pause "all `inj_type' `outcome' `viol_level' complete (`viol_form')"	
				} // violation level		
				
				if "`run_nulls'" != "off" {
					
					* NOW RUN WEAK NULL MODEL (1)
					eststo clear
					local null_1 "`model' `depvar' `covariates' ib(freq).time, vce(cl mineid) `suffix' iter(200)" // for inference (on all data)
					local null_1_pred "`model' `depvar' `covariates' if set == 0, vce(cl mineid) `suffix' iter(200)" // for prediction (on just test data) - time excluded
					cap noi eststo: `null_1'
					if e(rc) pause on
						pause "`inj_type' `outcome' null model 1 - failed"
					else { 
						* create tex file with estimates
						esttab using "$PROJECT_ROOT/results/tex/`date'/`sub_folder'`inj_type'_`outcome'_null_1`cutoff_ext'`spec_file_ext'`sample_ext'.tex", replace ///
							b(3) not label nonote `null_table_notes' /// 
							booktabs longtable noconstant noomitted eform /// 
							title(`inj_type' Injuries, `outcome_label', Null Models `title_options'\label{tab1})
					}
					* run null model again (just on training set) to generate predictions (just on test set) and store in new variable
					cap eststo: `null_1_pred'			
					qui predict `inj_type'_`outcome'_null_1 if set == 1
					
					* NOW RUN STRONG NULL MODEL (2) - THESE ALSO CONTAIN A TOTAL VIOLATIONS COVARIATE (LAGGED ONCE)
					eststo clear
					local null_2 "`model' `depvar' `covariates' ib(freq).time totalviolations_1lag, vce(cl mineid) `suffix' iter(500)" // for inference (on all data)
					local null_2_pred "`model' `depvar' `covariates' totalviolations_1lag if set == 0, vce(cl mineid) `suffix' iter(500)" // for prediction (on just test data) - time excluded
					cap noi eststo: `null_2'
					if e(rc) pause on
						pause "`inj_type' `outcome' null model 2 - failed"
					else { 
						* create tex file with estimates
						esttab using "$PROJECT_ROOT/results/tex/`date'/`sub_folder'`inj_type'_`outcome'_null_2`cutoff_ext'`spec_file_ext'`sample_ext'.tex", replace ///
							b(3) not label `null_2_table_notes' nonote ///
							booktabs eform longtable noconstant noomitted /// 
							title(`inj_type' Injuries, `outcome_label', Null Model (with Total Injuries) `title_options'\label{tab1})
					}
						
					* run null model again (just on training set) to generate predictions (just on test set) and store in new variable
					cap eststo: `null_2_pred'			
					predict `inj_type'_`outcome'_null_2 if set == 1
					
					* NOW RUN STRONG NULL MODEL (3) - THESE ALSO CONTAIN A TOTAL VIOLATIONS/ONSITE INSPEC HOURS COVARIATE (LAGGED ONCE)
					eststo clear
					local null_3 "`model' `depvar' `covariates' ib(freq).time total_violations_hours, vce(cl mineid) `suffix' iter(500)" // for inference (on all data)
					local null_3_pred "`model' `depvar' `covariates' total_violations_hours if set == 0, vce(cl mineid) `suffix' iter(500)" // for prediction (on just test data) - time excluded
					cap noi eststo: `null_3'
					if e(rc) pause on
						pause "`inj_type' `outcome' null model 3 - failed"
					else { 
						* create tex file with estimates
						esttab using "$PROJECT_ROOT/results/tex/`date'/`sub_folder'`inj_type'_`outcome'_null_3`cutoff_ext'`spec_file_ext'`sample_ext'.tex", replace ///
							b(3) not label `null_plus_table_notes' nonote ///
							booktabs eform longtable noconstant noomitted /// 
							title(`inj_type' Injuries, `outcome_label', Null Model (with Total Injuries) `title_options'\label{tab1})
					}
						
					* run null model again (just on training set) to generate predictions (just on test set) and store in new variable
					cap eststo: `null_3_pred'			
					predict `inj_type'_`outcome'_null_3 if set == 1
					pause "all `inj_type' `outcome' complete"	
					
				} // run null models
			} // outcome form 
			
			* save new data (with produced predictions as new vars)
			drop p* sp*
			save "$PROJECT_ROOT/results/dta/`date'/`sub_folder'`inj_type'_with_predictions`file_ext'`cutoff_ext'`spec_file_ext'`sample_ext'.dta", replace
			export delimited using "$PROJECT_ROOT/results/csv/`date'/`sub_folder'`inj_type'_with_predictions`file_ext'`cutoff_ext'`spec_file_ext'`sample_ext'.csv", replace
			
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
		if "`lag_def'" != "strict" pause "after all `inj_type' models with `lag_def' lag definition"		
		
	} // lag definition (strict or lax)
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
	
} // inj type
* report all non-converged models
noi di "NOT CONVERGED: `not_converged'"

*end*
