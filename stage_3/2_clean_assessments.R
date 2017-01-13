# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Clean Assessments
  # Cleans assessments

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library(stringr)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL) 
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # assessments data
assessments.in.file.name = paste0(originals.path, "/AssessedViolations.txt", collapse = NULL)
  # mine types data (used to remove underground observations)
mine.types.in.file.name = paste0(clean.path, "/clean_mine_types.rds", collapse = NULL)

# outputs
  # cleaned assessments data
assessments.out.file.name = paste0(clean.path, "/clean_assessments.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(clean.path, recursive = TRUE)

################################################################################

# load data
  # 2137348 rows; 58 columns; unique on violation_no
assessments = read.table(assessments.in.file.name, header = T, sep = "|", na.strings = c("", "NA"))

# rename variables
names(assessments)[names(assessments) == "VIOLATION_NO"] = "violationno"
names(assessments)[names(assessments) == "EVENT_NO"] = "eventno"
names(assessments)[names(assessments) == "MINE_ID"] = "mineid"
names(assessments)[names(assessments) == "ASSESS_CASE_NO"] = "assesscaseno"
names(assessments)[names(assessments) == "SIG_SUB_IND"] = "sigandsubindicator"
names(assessments)[names(assessments) == "MINE_ACT_SECTION_CD"] = "mineactsectioncode"
names(assessments)[names(assessments) == "CFR_STANDARD_CD"] = "cfrstandardcode"
names(assessments)[names(assessments) == "CITATION_TYPE_CD"] = "violationtypecode"
names(assessments)[names(assessments) == "COAL_METAL_IND"] = "coalcormetalm"
names(assessments)[names(assessments) == "VIOLATOR_TYPE_CD"] = "violatortypecode"
names(assessments)[names(assessments) == "PROPOSED_PENALTY_AMT"] = "proposedpenaltyamount"
names(assessments)[names(assessments) == "CURRENT_ASSESSMENT_AMT"] = "currentassessmentamount"
names(assessments)[names(assessments) == "PENALTY_POINTS"] = "penaltypoints"
names(assessments)[names(assessments) == "ISSUE_DT"] = "issuedate"
names(assessments) = tolower(names(assessments))

# format variables
assessments$violationno = as.character(assessments$violationno)
assessments$violationno = str_pad(assessments$violationno, 7, pad = "0")
assessments$eventno = as.character(assessments$eventno)
assessments$eventno = str_pad(assessments$eventno, 7, pad = "0")
assessments$mineid = as.character(assessments$mineid)
assessments$mineid = str_pad(assessments$mineid, 7, pad = "0")

################################################################################

# REMOVE DATA FROM ENVIRONMENTS NOT OF INTEREST

# drop data from environments not of interest
  # 1160380 rows; 58 columns; unique on violation_no
assessments = assessments[assessments$coalcormetalm == "C", ] 

# read mine types data (assessments data does NOT contain "minetype" field so we need
# to merge on mine type information by mineid)
  # 86362 rows; 3 columns; unique on mineid
mine.types = readRDS(mine.types.in.file.name)

# merge assessments with mine types & drop non-merged observations
  # 1160380 rows; 60 columns; unique on violationno
assessments = merge(assessments, mine.types, by = c("mineid"), all = T)
assessments = assessments[!is.na(assessments$eventno), ]
rm(mine.types)

# drop data from environments not of interest
  # 843818 rows; 60 columns; unique on violationno
assessments = assessments[assessments$minetype == "Underground", ]

################################################################################

# keep only useful variables
  # 843818 rows; 13 columns; unique on violationno
keep = c("violationno", "mineid", "eventno", 
         "assesscaseno", "sigandsubindicator", "mineactsectioncode",
         "cfrstandardcode", "violationtypecode", "violatortypecode",
         "proposedpenaltyamount", "currentassessmentamount", "penaltypoints", 
         "issuedate")
assessments = assessments[, (names(assessments) %in% keep)] 

################################################################################

# output assessment-level data
  # 843818 rows; 13 columns; unique on violationno
saveRDS(assessments, file = assessments.out.file.name)

################################################################################

rm(list = ls())

################################################################################
