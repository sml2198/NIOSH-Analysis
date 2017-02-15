# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Merge Violations
  # Merges violations data (produced in 1_clean_violations) 
    # and assessments data (produced in 2_clean_assessments) 
    # and CFR key (4_clean_cfr_key)

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com

# Last edit 2/8/2017

################################################################################

library(stringr)
library(zoo)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/data/2_merged", collapse = NULL) 

# inputs
  # cleaned violations data
    # produced in 1_clean_violations
violations.in.file.name = paste0(cleaned.path, "/clean_violations.rds", collapse = NULL)
  # cleaned assessments data
    # produced in 2_clean_assessments
assessments.in.file.name = paste0(cleaned.path, "/clean_assessments.rds", collapse = NULL)
  # cleaned CFR key
    # produced in 4_clean_cfr_key
cfr.key.in.file.name = paste0(cleaned.path, "/clean_cfr_key.rds", collapse = NULL)

# outputs
  # merged violations
violations.out.file.name = paste0(merged.path, "/merged_violations.rds", collapse = NULL)

# generate file paths
dir.create(merged.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, cleaned.path, merged.path)

################################################################################

# READ DATA

# read violations data
  # 868722 rows; 9 columns; unique on violationno
violations = readRDS(violations.in.file.name)

# read assessments data
  # 843818 rows; 5 columns; unique on violationno
assessments = readRDS(assessments.in.file.name)

# read cfr key
  # 2026 rows; 11 columns; unique on subsectioncode
cfr.key = readRDS(cfr.key.in.file.name)

# bye
rm(violations.in.file.name, assessments.in.file.name, cfr.key.in.file.name)

################################################################################

# MERGE ASSESSMENTS ON VIOLATIONS

# format violationno
violations$violationno = str_pad(violations$violationno, 7, pad = "0")
assessments$violationno = str_pad(assessments$violationno, 7, pad = "0")

# merge assessments data with violations data
  # 843787 rows; 12 columns; unique on violationno-mineid
violations = merge(violations, assessments, by = c("mineid", "violationno"), all = FALSE)

# bye
rm(assessments)

################################################################################

# EDIT DATA

# format subsectioncode
violations$subsectioncode = gsub("(\\(([0-9]|[a-z]|-|[A-Z])+\\))+", "", violations$subsectioncode)
violations$subsectioncode = gsub("(-([a-z]+)\\))+(\\([0-9])*", "",  violations$subsectioncode)
violations$subsectioncodemarker = paste("S", violations$subsectioncode, sep = "")

# in 2 cases where subsection is missing, partsection can be subbed in 
violations$partsection2 = violations$partsection
violations$partsection2 = gsub("\\([a-z]+\\)", "", violations$partsection2)
violations$partsection2 = gsub("\\([0-9]+\\)", "", violations$partsection2)
violations$subsectioncode = ifelse((is.na(violations$subsectioncode) & !is.na(violations$partsection2)), 
                                   violations$partsection2, violations$subsectioncode)
violations$partsection2 = NULL

# As detailed on page 7 of the Citation and Order Writing Handbook for Coal Mines 
  # and Metal and Non-metal Mines (MSHA Handbook Series, December 2013, Chapter 3: 
  # Violations Description & Issuing Form), which can be retrieved here: 
    # http://arlweb.msha.gov/READROOM/HANDBOOK/PH13-I-1.pdf,
  # a violation can be issued EITHER for a violation of a specific regulation 
  # (a part/subsection of Title 30 - Chapter 1 - Suchapter P - Part 100 of the Code 
  # of Federal Regulations(CFR)) OR for a violation of the Mine Act. 
# We are only interested in which parts/subsections of the CFR code are related 
  # to injuries. 
# Therefore, we preserve violations that are missing "subsectioncode" but not 
  # missing "sectionofact" (these are violations of the Mine Act and not the CFR), 
  # and we make sure they are reflected in our final "totalviolations"
# The statement below yields 0, so there are no improperly missing subsections. 
  # In every violation without a subsectioncode, there is a non-missing sectionofact.
#sum(is.na(violations$sectionofact) & is.na(violations$subsectioncode)) # 0

################################################################################

# MERGE VIOLATIONS DATA AND CFR KEY

# merge violations data and CFR key
  # 845021 rows; 23 columns
violations = merge(violations, cfr.key, by = "subsectioncode", all = TRUE)

# drop non-merging observations
  # 843787 rows; 23 columns
violations = violations[which(!is.na(violations$violationno)), ]

# bye
rm(cfr.key)

################################################################################

# EDIT DATA

# Some observations do not have relevant/maybe relevant designations mapped onto them.
# This happens in two cases: 
  # (1) If there is a subsection that we have NOT marked in terms of relevance in the CFR key
    # This only happens for subsections codes lower than 40 because in our meeting with NIOSH, 
      # we strictly considered parts 40 and above
  # (2) If subsectioncode is code, which happens in cases of violations of the Mine Act. 
    # These observations are never missing mineactsectioncode
    # Here, we make all relevance vars equal to zero if missing
varlist = names(violations[, grep("relevant", names(violations))])
for (i in 1:length(varlist)) {
  violations[, varlist[i]] = ifelse(is.na(violations[, varlist[i]]), 0, violations[, varlist[i]])
}

# format date variables
violations$dateissued = as.Date(as.character(violations$dateissued), "%m/%d/%Y")
violations$quarter = as.yearqtr(violations$dateissued)
violations$year = as.numeric(format(violations$quarter, "%Y"))

# drop data outside study period
  # 843760 rows; 34 columns; unique on violationno-mineid
violations = violations[(violations$year > "1999"), ]

# bye
rm(i, varlist)

################################################################################

# OUTPUT DATA

# output violations-level data
  # 843760 rows; 34 columns; unique on violationno-mineid
saveRDS(violations, file = violations.out.file.name)

# bye
rm(list = ls())

################################################################################
