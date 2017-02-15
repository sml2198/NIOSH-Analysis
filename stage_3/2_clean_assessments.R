# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Clean Assessments
  # Cleans assessments data from the MSHA open data portal

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/8/2017

################################################################################

library(stringr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
originals.path = paste0(root, "/data/0_originals", collapse = NULL) 
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 

# inputs
  # assessments data from the MSHA open data portal
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
assessments.in.file.name = paste0(originals.path, "/AssessedViolations.txt", collapse = NULL)
  # mine type data
    # produced in 1_clean_mines
mine.types.in.file.name = paste0(cleaned.path, "/clean_mine_types.rds", collapse = NULL)

# outputs
  # cleaned assessments data
assessments.out.file.name = paste0(cleaned.path, "/clean_assessments.rds", collapse = NULL)

# generate file paths 
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read assessments data
  # 2137348 rows; 58 columns; unique on violationno
assessments = read.table(assessments.in.file.name, header = TRUE, sep = "|", na.strings = c("", "NA"))

# read cleaned mine types data
  # 86362 rows; 3 columns; unique on mineid
mine.types = readRDS(mine.types.in.file.name)

# bye
rm(assessments.in.file.name, mine.types.in.file.name)

################################################################################

# CLEAN DATA

# drop unnecessary variables
  # 2137348 rows; 7 columns; unique on violationno
assessments = assessments[, c("CFR_STANDARD_CD", "COAL_METAL_IND", "EVENT_NO",
                              "MINE_ACT_SECTION_CD", "MINE_ID", "PENALTY_POINTS",
                              "VIOLATION_NO")]

# rename variables
names(assessments)[names(assessments) == "CFR_STANDARD_CD"] = "subsectioncode"
names(assessments)[names(assessments) == "COAL_METAL_IND"] = "coalcormetalm"
names(assessments)[names(assessments) == "EVENT_NO"] = "eventno"
names(assessments)[names(assessments) == "MINE_ACT_SECTION_CD"] = "mineactsectioncode"
names(assessments)[names(assessments) == "MINE_ID"] = "mineid"
names(assessments)[names(assessments) == "PENALTY_POINTS"] = "penaltypoints"
names(assessments)[names(assessments) == "VIOLATION_NO"] = "violationno"

# format variables
assessments$violationno = str_pad(assessments$violationno, 7, pad = "0")
assessments$mineid = str_pad(assessments$mineid, 7, pad = "0")

# drop data from environments not of interest
  # 1160380 rows; 7 columns; unique on violationno
assessments = assessments[assessments$coalcormetalm == "C", ] 

# bye
  # 1160380 rows; 6 columns; unique on violationno
assessments$coalcormetalm = NULL

################################################################################

# MERGE ASSESSMENTS AND MINE TYPE
  # assessments data does not include information about mine type, which we need
    # to drop data from environments not of interest

# merge assessments and mine type data
  # 1241904 rows; 8 columns
assessments = merge(assessments, mine.types, by = c("mineid"), all = TRUE)

# drop non-merging observations
  # 1160380 rows; 8 columns
assessments = assessments[!is.na(assessments$eventno), ]

# bye
rm(mine.types)

################################################################################

# CLEAN DATA

# drop data from environments not of interest
  # 843818 rows; 8 columns; unique on violationno
assessments = assessments[assessments$minetype == "Underground", ]

# drop unnecessary variables
  # 843818 rows; 5 columns; unique on violationno
assessments$coalcormetalmmine = 
  assessments$eventno = 
  assessments$minetype = NULL

################################################################################

# OUTPUT DATA

# output cleaned assessment data
  # 843818 rows; 5 columns; unique on violationno
saveRDS(assessments, file = assessments.out.file.name)

# bye
rm(list = ls())

################################################################################
