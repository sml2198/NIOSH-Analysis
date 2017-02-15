# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 7 - Prepare Stage 3 Data (R)
  # Collapses merged violations data (produced in 6_merge_violations) 
    # and cleaned inspections data (produced in 3_clean_inspections) to the mine-year-level
  # Merges these datasets with mine-year-level data used in Stage 2 (produced in 5_prepare_mines)
  # Outputs data prepared for Stage 3

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com

# Last edit 2/9/2017

################################################################################

library(foreign)
library(plyr)
library(psych)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/data/2_merged", collapse = NULL) 
prepared.path = paste0(root, "/data/5_prepared", collapse = NULL)
                     
# inputs
  # merged violations data
    # produced in 6_merge_violations
violations.in.file.name = paste0(merged.path, "/merged_violations.rds", collapse = NULL)
  # cleaned inspections data
    # produced in 3_clean_inspections
inspections.in.file.name = paste0(cleaned.path, "/clean_inspections.rds", collapse = NULL)
  # mine-year-level data
    # produced in 5_prepare_mines
mines.in.file.name = paste0(prepared.path, "/prepared_mine_years.rds", collapse = NULL)

# outputs
  # MR data for stage 3 (rds)
MR.data.out.rds.file.name = paste0(prepared.path, "/prepared_stage_3_MR_part_1.rds", collapse = NULL)
  # MR data for stage 3 (dta)
MR.data.out.dta.file.name = paste0(prepared.path, "/prepared_stage_3_MR_part_1.dta", collapse = NULL)
  # PS data for stage 3 (rds)
PS.data.out.rds.file.name = paste0(prepared.path, "/prepared_stage_3_PS_part_1.rds", collapse = NULL)
  # PS data for stage 3 (dta)
PS.data.out.dta.file.name = paste0(prepared.path, "/prepared_stage_3_PS_part_1.dta", collapse = NULL)

# generate file paths
dir.create(prepared.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, cleaned.path, merged.path, prepared.path)

################################################################################

# READ DATA

# read cleaned violations data
  # 843760 rows; 23 columns; unique on violationno
all.violations = readRDS(violations.in.file.name)

# read cleaned inspections data
  # 192141 rows; 8 columns; unique on eventno
inspections = readRDS(inspections.in.file.name)

# read mine-year-level data
  # 6253 rows; 13 columns; unique on mineid-year
mine.years = readRDS(mines.in.file.name)

# bye
rm(violations.in.file.name, inspections.in.file.name, mines.in.file.name)

################################################################################

# COLLAPSE INSPECTIONS DATA TO THE MINE-YEAR LEVEL

# collapse inspections to the mine-year level
  # 11580 rows; 4 columns; unique on mineid-year
inspections = ddply(inspections[, c("inspectionhours", "numinspections", "mineid", "year")], 
                    c("mineid", "year"), 
                    function(x) colSums(x[, c("inspectionhours", "numinspections")], na.rm = TRUE))

################################################################################

for (injury in c("MR", "PS")) {

  # GET LISTS OF RELEVANT VIOLATIONS
  
  # copy violations (will become MR/PS-specific)
  violations = all.violations
  
  # UNCOMMENT IF YOU WANT TO PRODUCE SIG AND SUB VARIABLES
  # format sig and sub variables
  #as.numeric.factor = function(x) {as.numeric(levels(x))[x]}
  #names(violations)[names(violations) == "sigandsubdesignation"] = "sigandsub"
  #levels(violations$sigandsub) = c(0, 1)
  #violations$sigandsub = as.numeric.factor(violations$sigandsub)
  
  # create list of relevant CFR parts/subsections
  if (injury == "MR") {
    relevantparts = as.list(levels(factor(violations[violations$MRrelevant == 1 | 
                                                       violations$MRmayberelevant == 1, "cfr_part_code"])))
    relevantsubsects = as.list(levels(factor(violations[violations$MRrelevant == 1 | 
                                                              violations$MRmayberelevant == 1, "subsectioncode"])))
  }
  if (injury == "PS") {
    relevantparts = as.list(levels(factor(violations[violations$PSrelevant == 1 | 
                                                       violations$PSmayberelevant == 1, "cfr_part_code"])))
    relevantsubsects = as.list(levels(factor(violations[violations$PSrelevant == 1 | 
                                                              violations$PSmayberelevant == 1, "subsectioncode"])))
  }
  
  # remove subsections that have < 15 violations
  removesubcodes = list()
  for (code in relevantsubsects) {
    if (nrow(violations[violations$subsectioncode == code, ]) < 15) {
      removesubcodes = c(removesubcodes, code)
    }
  }
  relevantsubsects = setdiff(relevantsubsects, removesubcodes)
  
  # bye 
  rm(removesubcodes, code)
  
  ##############################################################################
  
  # GENERATE CFR PART AND SUBSECTION SPECIFIC VARIABLES
  
  # create part dummies
    # UNCOMMENT IF YOU WANT TO PRODUCE PENALTY POINT AND SIG AND SUB VARIABLES
  relevantparts = unlist(relevantparts)
  for (i in 1:length(relevantparts)) {
    violations[, relevantparts[i]] = ifelse(violations$cfr_part_code == relevantparts[i], 1, 0)
    # violations[, paste(relevantparts[i], "penaltypoints", sep = ".")] = apply(cbind(violations[, "penaltypoints"], violations[, relevantparts[i]]), 1, prod)
    # violations[, paste(relevantparts[i], "sigandsub", sep = ".")] = apply(cbind(violations[, "sigandsub"], violations[, relevantparts[i]]), 1, prod)
  }
  
  # create subsection dummies
    # UNCOMMENT IF YOU WANT TO PRODUCE PENALTY POINT AND SIG AND SUB VARIABLES
  relevantsubsects = unlist(relevantsubsects)
  for (i in 1:length(relevantsubsects)) {
    violations[, relevantsubsects[i]] = ifelse(violations$subsectioncode == relevantsubsects[i], 1, 0)
    # violations[, paste(relevantsubsects[i], "penaltypoints", sep = ".")] = apply(cbind(violations[, "penaltypoints"], violations[, relevantsubsects[i]]), 1, prod)
    # violations[, paste(relevantsubsects[i], "sigandsub", sep = ".")] = apply(cbind(violations[, "sigandsub"], violations[, relevantsubsects[i]]), 1, prod)
  }
  
  # part dummies contain information about all violations to any subsection within 
    # a part that contains any relevant subsection; we only want part dummies to 
    # contain information about relevant subsections within the part; we remedy this
  varlist = names(violations[, grep("^[0-9][0-9]((.[a-z])|($))", names(violations))])
  
  if (injury == "MR") {
    violations$relevant = ifelse((violations$MRrelevant == 1 | violations$MRmayberelevant == 1), 1, 0)
  }
  if (injury == "PS") {
    violations$relevant = ifelse((violations$PSrelevant == 1 | violations$PSmayberelevant == 1), 1, 0)
  }
  
  for (i in 1:length(varlist)) {
    violations[, varlist[i]] = ifelse(violations$relevant == 1, 0, violations[, varlist[i]])
  }
  
  # bye
  rm(i, varlist, relevantparts, relevantsubsects)
  
  ##############################################################################
  
  # COLLAPSE VIOLATIONS DATA TO THE MINE-YEAR LEVEL
  
  # create variable to sum total violations
  violations$totalviolations = ifelse(!is.na(violations$violationno), 1, 0)
  
  # collapse violations data
    # MR: 9660 rows; 340 columns; unique on mineid-year
    # PS: 9660 rows; 96 columns; unique on mineid-year
  violations = ddply(violations[, c(grep("^[0-9][0-9]", names(violations)), 
                                    match("totalviolations", names(violations)),
                                    match("mineid", names(violations)),
                                    match("year", names(violations)))], 
                     c("mineid", "year"), 
                     function(x) colSums(x[, c(grep("^[0-9][0-9]", names(x)), 
                                               match("totalviolations", names(x)))], na.rm = TRUE))
  
  ############################################################################## 
  
  # MERGE MINES, VIOLATIONS, AND INSPECTIONS
  
  # merge mine-years and violations data
    # MR: 9711 rows; 351 columns
    # PS: 9711 rows; 107 columns
  data = merge(mine.years, violations, by = c("mineid", "year"), all = TRUE)
  
  # drop non-merging observations
    # MR: 6253 rows; 351 columns; unique on mineid-year
    # PS: 6253 rows; 107 columns; unique on mineid-year
  data = data[complete.cases(data$hours), ]
  
  # merge inspections data onto mine-years/violations data
    # MR: 11582 rows; 353 columns
    # PS: 11582 rows; 109 columns
  data = merge(data, inspections, by = c("mineid", "year"), all = TRUE)
  
  # drop non-merging observations
    # MR: 6253 rows; 353 columns; unique on mineid-year
    # PS: 6253 rows; 107 columns; unique on mineid-year
  data = data[complete.cases(data$hours), ]
 
  # bye
  rm(violations)
  
  ##############################################################################
  
  # EDIT DATA
  
  # replace NAs in inspections with 0 (mine years that had no inspections)
  data$numinspections = ifelse(is.na(data$numinspections), 0, data$numinspections)
  
  # replace missings violations with 0 if there were inspections 
  data$totalviolations = ifelse((is.na(data$totalviolations) & data$numinspections > 0), 0, data$totalviolations)
  
  # order by mine-year
  data = data[order(data$mineid, data$year),]
  
  # create binary injury variables
  if (injury == "MR"){
    data$MR.indicator = ifelse(data$MR > 0, 1, 0)
  }
  if (injury == "PS"){
    data$PS.indicator = ifelse(data$PS > 0, 1, 0)
  }
  
  # drop variables with SE = 0
    # MR: 6253 rows; 350 columns; unique on mineid-year
    # PS: 6253 rows; 110 columns; unique on mineid-year
  
  keep = c("appalachia", "district", "employment",
           "hours", "mineid", "MR",
           "operatorid", "operatortime", "prod", 
           "PS", "safetycommittee", "total_injuries",
           "year")
  
  var.stats = describe(data[, !(names(data) %in% keep)])
  nontriv.vars = rownames(var.stats[var.stats$sd > 0, ])
  triv.vars = setdiff(names(data), nontriv.vars)
  
  data = data[, c(nontriv.vars, keep)]
  
  # rename variable with "p" prefix for parts and "sp" prefix for subparts
  names(data)[grep("^[0-9]", names(data))] = paste("p", names(data)[grep("^[0-9]", names(data))], sep = "")
  names(data)[grep("[0-9].[0-9]", names(data))] = paste("s", names(data)[grep("[0-9].[0-9]", names(data))], sep = "")
  
  # format variables
  data$mineid = as.character(data$mineid)
  data$year = as.numeric(data$year)
  if (injury == "MR"){
    data$MR.indicator = as.factor(data$MR.indicator)
  }
  if (injury == "PS"){
    data$PS.indicator = as.factor(data$PS.indicator)
  }
  
  # bye
  rm(var.stats, nontriv.vars, triv.vars, keep)
  
  ##############################################################################
  
  # OUTPUT DATA
  
  # grab correct file paths
  if (injury == "MR") {
    r.file.name = MR.data.out.rds.file.name
    stata.file.name = MR.data.out.dta.file.name
  }
  if (injury == "PS") {
    r.file.name = PS.data.out.rds.file.name
    stata.file.name = PS.data.out.dta.file.name
  }
  
  # output data for stage 3 (rds)
    # MR: 6253 rows; 350 columns; unique on mineid-year
    # PS: 6253 rows; 110 columns; unique on mineid-year
  saveRDS(data, file = r.file.name)
  
  # make data stata-friendly
  stata.names = names(data)
  stata.names = gsub("\\.", "_", stata.names)
  stata.names = gsub("-", "_", stata.names)
  stata.data = data
  names(stata.data) = stata.names
  
  # output output data for stage 3 (dta)
    # MR: 6253 rows; 350 columns; unique on mineid-year
    # PS: 6253 rows; 110 columns; unique on mineid-year
  write.dta(stata.data, file = stata.file.name)
  
  # bye
  rm(stata.names, stata.data, r.file.name, stata.file.name)

}

################################################################################

# bye
rm(list = ls())

################################################################################
