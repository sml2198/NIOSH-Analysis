# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 7 - Prepare Stage 3 Data (R)
  # 

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

library(plyr)
library(foreign)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/2_merged", collapse = NULL) 
prepped.path = paste0(root, "/5_prepared", collapse = NULL)
                     
# inputs
  # violations data
violations.in.file.name = paste0(merged.path, "/merged_violations.rds", collapse = NULL)
  # inspections data
inspections.in.file.name = paste0(clean.path, "/clean_inspections.rds", collapse = NULL)
  # mine-year data
mines.in.file.name = paste0(prepped.path, "/prepared_mine_years.rds", collapse = NULL)

# outputs
  # prediction-ready MR data
MR.data.out.file.name = paste0(prepped.path, "/prepared_stage_3_MR_part_1", collapse = NULL)
  # prediction-ready PS data
PS.data.out.file.name = paste0(prepped.path, "/prepared_stage_3_PS_part_1", collapse = NULL)

################################################################################

# LOAD DATASETS THAT ONLY ONLY NEED TO BE LOADED ONCE 

# load inspections
  # 192141 rows; 12 columns; unique on eventno
inspections = readRDS(inspections.in.file.name)

# load mine-years
  # 6253 rows; 13 columns; unique on mineid-year
mine.years = readRDS(mines.in.file.name)

# load violations
  # 843760 rows; 34 columns; unique on violationno-mineid
all.violations = readRDS(violations.in.file.name)

################################################################################

# COLLAPSE INSPECTIONS TO THE MINE-YEAR LEVEL

# collapse inspections to the mine-year level (sums)
  # 11580 rows; 340 columns; unique on eventno
inspections = ddply(inspections[, c(match("inspectionhours", names(inspections)), 
                                    match("numinspections", names(inspections)), 
                                    match("mineid", names(inspections)), 
                                    match("year", names(inspections)))], c("mineid", "year"), 
                    function(x) colSums(x[, c(match("inspectionhours", names(x)), 
                                              match("numinspections", names(x)))], na.rm = T))

# bye
rm(root, clean.path, merged.path, prepped.path, inspections.in.file.name, 
   mines.in.file.name, violations.in.file.name, datevars, i)

################################################################################

for (injury in c("MR", "PS")) { # create separate datasets for MR and PS injuries

  ##############################################################################

  # FORMAT VIOLATIONS AND PREPARE TO GENERATE PART AND SUBSECTION SPECIFIC VARIABLES
  
  # create copy of violations data (will become MR/PS specific soon)
  violations = all.violations
  
  # format sig and sub var - only necessary if you want to produce sigandsub variables
  as.numeric.factor = function(x) {as.numeric(levels(x))[x]}
  names(violations)[names(violations) == "sigandsubdesignation"] = "sigandsub"
  levels(violations$sigandsub) = c(0,1)
  violations$sigandsub = as.numeric.factor(violations$sigandsub)
  
  # create lists of relevant cfr parts/subsections for violation specific variable creation
  if (injury == "MR") {
    relevantparts = as.list(levels(factor(violations[violations$MRrelevant == 1 | 
                                                       violations$MRmayberelevant == 1, ]$cfr_part_code)))
    relevantsubsects = as.list(levels(factor(violations[violations$MRrelevant == 1 | 
                                                              violations$MRmayberelevant == 1, ]$subsectioncode)))
  }
  if (injury == "PS") {
    relevantparts = as.list(levels(factor(violations[violations$PSrelevant == 1 | 
                                                       violations$PSmayberelevant == 1, ]$cfr_part_code)))
    relevantsubsects = as.list(levels(factor(violations[violations$PSrelevant == 1 | 
                                                              violations$PSmayberelevant == 1, ]$subsectioncode)))
  }
  
  # if the subsectioncode is relevant, AND there are < 15 violations from that 
  # subsectioncode, remove that subsectioncode from the relevant lists 
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
  
  # create part- and variable-specific variables/dummies
    # uncomment the two commented lines if you want to produce penaltypoints and/or sigandsub specific variables
  relevantparts = unlist(relevantparts)
  for (i in 1:length(relevantparts)) {
    violations[, relevantparts[i]] = ifelse(violations$cfr_part_code == relevantparts[i], 1, 0)
    # violations[, paste(relevantparts[i], "penaltypoints", sep = ".")] = apply(cbind(violations[, "penaltypoints"], violations[, relevantparts[i]]), 1, prod)
    # violations[, paste(relevantparts[i], "sigandsub", sep = ".")] = apply(cbind(violations[, "sigandsub"], violations[, relevantparts[i]]), 1, prod)
  }
  
  # create subpart- and variable-specific variables/dummies
  relevantsubsects = unlist(relevantsubsects)
  for (i in 1:length(relevantsubsects)) {
    violations[, relevantsubsects[i]] = ifelse(violations$subsectioncode == relevantsubsects[i], 1, 0)
    # violations[, paste(relevantsubsects[i], "penaltypoints", sep = ".")] = apply(cbind(violations[, "penaltypoints"], violations[, relevantsubsects[i]]), 1, prod)
    # violations[, paste(relevantsubsects[i], "sigandsub", sep = ".")] = apply(cbind(violations[, "sigandsub"], violations[, relevantsubsects[i]]), 1, prod)
  }
  
  # At this point, part-level vars contain information for all observations in any part with any relevant
  # subsections. We only want our part-level vars to reflect information about observations that are
  # relevant at the subsection level, before we collapse.
  varlist = names(violations[, grep("^[0-9][0-9]((.[a-z])|($))", names(violations))])
  
  if (injury == "MR") {
    violations$relevant = ifelse((violations$MRrelevant == 1 | violations$MRmayberelevant == 1), 1, 0)
  }
  
  if (injury == "PS") {
    violations$relevant = ifelse((violations$PSrelevant == 1 | violations$PSmayberelevant == 1), 1, 0)
  }
  
  # this problem only affects the part-level data preparation. 
    # subsection-specific data is obviously already subsection specific.
  for (j in 1:length(varlist)) {
    violations[, varlist[j]] = ifelse(violations$relevant == 1, 0, violations[, varlist[j]])
  }
  
  # bye
  rm(i, j, varlist, relevantparts, relevantsubsects)
  
  ##############################################################################
  
  # COLLAPSE VIOLATIONS DATA TO THE MINE-YEAR LEVEL
  
  # create variables to sum for total violation counts (always 1)
  violations$totalviolations = ifelse(!is.na(violations$violationno), 1, 0)
  
  # collapse the violations variables that we need to sum
    # MR: 9660 rows; 340 columns; unique on mineid-year
    # PS: 9660 rows; 96 columns; unique on mineid-year
  violations = ddply(violations[, c(grep("^[0-9][0-9]", names(violations)), 
                                    match("totalviolations", names(violations)),
                                    match("mineid", names(violations)),
                                    match("year", names(violations)))], c("mineid", "year"), 
                     function(x) colSums(x[, c(grep("^[0-9][0-9]", names(x)), 
                                               match("totalviolations", names(x)))], na.rm = T))
  
  ############################################################################## 
  
  # MERGE MINES, VIOLATIONS, AND INSPECTIONS (ALL AT MINE-YEAR LEVEL NOW)
  
  # merge mine-years and violations data
    # MR: 6253 rows; 351 columns; unique on mineid-year
    # PS: 6253 rows; 107 columns; unique on mineid-year
  prediction.data = merge(mine.years, violations, by = c("mineid", "year"), all = TRUE)
  prediction.data = prediction.data[complete.cases(prediction.data$hours),]
  
  # merge inspections data onto mine-years/violations data
    # MR: 6253 rows; 353 columns; unique on mineid-year
    # PS: 6253 rows; 109 columns; unique on mineid-year
  prediction.data = merge(prediction.data, inspections, by = c("mineid", "year"), all = TRUE)
  prediction.data = prediction.data[complete.cases(prediction.data$hours),]
 
  # replace NAs in inspection variables with zeroes (mine years that had no inspections)
  prediction.data$numinspections = ifelse(is.na(prediction.data$numinspections), 
                                          0, prediction.data$numinspections)
  
  # replace missings (mine years without violations) with zeroes 
    # BUT only when there were are inspections!
  prediction.data$totalviolations = ifelse((is.na(prediction.data$totalviolations) &
                                              prediction.data$numinspections > 0), 
                                           0, prediction.data$totalviolations)
  
  ##############################################################################
  
  # FINAL VARIABLE CLEANING AND PREP
  
  # order the data by mine-year
  prediction.data = prediction.data[order(prediction.data[,"mineid"], prediction.data[,"year"]),]
  
  # add variables for binary dependent vars
  if (injury == "MR"){
    prediction.data$MR.indicator = ifelse(prediction.data$MR > 0, 1, 0)
  }
  if (injury == "PS"){
    prediction.data$PS.indicator = ifelse(prediction.data$PS > 0, 1, 0)
  }
  
  # pare away variables with zero variation before model selection and prediction stages
  var.stats = describe(prediction.data[, c(-match("mineid", names(prediction.data)), 
                                           -match("appalachia", names(prediction.data)),
                                           -match("district", names(prediction.data)),
                                           -match("operatorid", names(prediction.data)),
                                           -match("operatortime", names(prediction.data)), 
                                           -match("safetycommittee", names(prediction.data)), 
                                           -match("year", names(prediction.data)))])
  
  # variables are nontrivial (worth keeping) if their standard deviation is greater than zero 
  nontriv.vars = rownames(var.stats[var.stats$sd > 0,])
  triv.vars = setdiff(names(prediction.data), nontriv.vars)
  
  # keeps only nontrivial variables
    # MR: 6253 rows; 350 columns; unique on mineid-year
    # PS: 6253 rows; 110 columns; unique on mineid-year
  prediction.data = prediction.data[, c(nontriv.vars, "mineid", "appalachia", 
                                        "district", "operatorid", "operatortime", 
                                        "safetycommittee", "year")]
  
  # bye
  rm(var.stats, nontriv.vars, triv.vars)
  
  ##############################################################################
  
  # FINAL VARIABLE FORMATTING
  
  # rename variable with "p" prefix for parts and "sp" prefix for subparts
  names(prediction.data)[grep("^[0-9]", names(prediction.data))] = paste("p", names(prediction.data)[grep("^[0-9]", names(prediction.data))], sep = "")
  names(prediction.data)[grep("[0-9].[0-9]", names(prediction.data))] = paste("s", names(prediction.data)[grep("[0-9].[0-9]", names(prediction.data))], sep = "")
  
  # reformat variables
  prediction.data$mineid = as.character(prediction.data$mineid)
  prediction.data$year = as.numeric(prediction.data$year)
  
  if (injury == "MR"){
    prediction.data$MR.indicator = as.factor(prediction.data$MR.indicator)
  }
  if (injury == "PS"){
    prediction.data$PS.indicator = as.factor(prediction.data$PS.indicator)
  }
  
  ##############################################################################
  
  # OUTPUT DATA
  
  # create file names
  if (injury == "MR") {
    r.file.name = paste0(MR.data.out.file.name, ".rds")
    stata.file.name = paste0(MR.data.out.file.name, ".dta")
  }

  if (injury == "PS") {
    r.file.name = paste0(PS.data.out.file.name, ".rds")
    stata.file.name = paste0(PS.data.out.file.name, ".dta")
  }
  
  # output prediction-ready mine-year data as an R dataset
    # MR: 6253 rows; 350 columns; unique on mineid-year
    # PS: 6253 rows; 110 columns; unique on mineid-year
  saveRDS(prediction.data, file = r.file.name)
  
  # remove special characters from data names so it's stata-friendly
  stata.names = names(prediction.data)
  stata.names = gsub("\\.", "_", stata.names)
  stata.names = gsub("-", "_", stata.names)
  stata.data = prediction.data
  names(stata.data) = stata.names
  
  # output prediction-ready mine-year data as a dta for stata
    # MR: 6253 rows; 350 columns; unique on mineid-year
    # PS: 6253 rows; 110 columns; unique on mineid-year
  write.dta(stata.data, file = stata.file.name)
  
  # bye
  rm(stata.names, stata.data, r.file.name, stata.file.name)
  
  ################################################################################

} # end of the PS/MR loop
  
rm(list = ls())

################################################################################
