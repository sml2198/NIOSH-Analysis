# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 1 - Clean Violations
  # Cleans violations data from the MSHA open data portal

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
  # violations data from the MSHA open data portal
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
violations.in.file.name = paste0(originals.path, "/Violations.txt", collapse = NULL)

# outputs
  # cleaned violations data
violations.out.file.name = paste0(cleaned.path, "/clean_violations.rds", collapse = NULL)

# generate file paths 
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read violations data
  # 2193591 rows; 61 columns
violations = read.table(violations.in.file.name, header = TRUE, sep = "|", na.strings = c("", "NA"))

# bye 
rm(violations.in.file.name)

################################################################################

# CLEAN DATA

# drop data from environments not of interest
  # 868757 rows; 61 columns
violations = violations[which(violations$COAL_METAL_IND == "C" &
                                violations$MINE_TYPE == "Underground"), ]

# drop duplicated observations
  # 868722 rows; 14 columns; unique on violationno
violations = violations[!duplicated(violations$VIOLATION_NO), ]

# drop unnecessary variables
  # 868757 rows; 9 columns; violationno
violations = violations[, c("CAL_QTR", "CAL_YR", "EVENT_NO", 
                            "MINE_ID", "PART_SECTION", "SECTION_OF_ACT", 
                            "SIG_SUB", "VIOLATION_ISSUE_DT", "VIOLATION_NO")]

# rename variables
names(violations)[names(violations) == "CAL_QTR"] = "quarter"
names(violations)[names(violations) == "CAL_YR"] = "year"
names(violations)[names(violations) == "EVENT_NO"] = "eventno"
names(violations)[names(violations) == "MINE_ID"] = "mineid"
names(violations)[names(violations) == "PART_SECTION"] = "partsection"
names(violations)[names(violations) == "SECTION_OF_ACT"] = "sectionofact"
names(violations)[names(violations) == "SIG_SUB"] = "sigandsubdesignation"
names(violations)[names(violations) == "VIOLATION_ISSUE_DT"] = "dateissued"
names(violations)[names(violations) == "VIOLATION_NO"] = "violationno"

# format variables
violations$eventno = str_pad(violations$eventno, 7, pad = "0")
violations$mineid = str_pad(violations$mineid, 7, pad = "0")
violations$violationno = str_pad(violations$violationno, 7, pad = "0")

################################################################################

# OUTPUT DATA

# output cleaned violations data
  # 868722 rows; 9 columns; unique on violationno
saveRDS(violations, file = violations.out.file.name)

# bye
rm(list = ls())

################################################################################
