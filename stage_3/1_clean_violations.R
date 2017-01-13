# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 1 - Clean Violations
  # Cleans violations

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
  # violations data
violations.in.file.name = paste0(originals.path, "/Violations.txt", collapse = NULL)

# outputs
  # cleaned violations data
violations.out.file.name = paste0(clean.path, "/clean_violations.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(clean.path, recursive = TRUE)

################################################################################

# load data
  # 2193591 rows; 61 columns; unique on violation_no
violations = read.table(violations.in.file.name, header = T, sep = "|", na.strings = c("", "NA"))

# drop data from environments not of interest
  # 868757 rows; 61 columns; unique on violation_no
violations = violations[violations$COAL_METAL_IND == "C", ] 
violations = violations[violations$MINE_TYPE == "Underground", ]

# rename variables
names(violations)[names(violations) == "VIOLATION_NO"] = "violationno"
names(violations)[names(violations) == "VIOLATION_ID"] = "violationid"
names(violations)[names(violations) == "MINE_ID"] = "mineid"
names(violations)[names(violations) == "EVENT_NO"] = "eventno"
names(violations)[names(violations) == "CAL_QTR"] = "quarter"
names(violations)[names(violations) == "CAL_YR"] = "year"
names(violations)[names(violations) == "TERMINATION_DT"] = "dateterminated"
names(violations)[names(violations) == "SECTION_OF_ACT_1"] = "typeaction1"
names(violations)[names(violations) == "SECTION_OF_ACT_2"] = "typeaction2"
names(violations)[names(violations) == "VIOLATION_ISSUE_DT"] = "dateissued"
names(violations)[names(violations) == "VACATE_DT"] = "datevacated"
names(violations)[names(violations) == "VACATE_DT"] = "datevacated"
names(violations)[names(violations) == "TERMINATION_DT"] = "dateterminated"
names(violations)[names(violations) == "PART_SECTION"] = "partsection"
names(violations)[names(violations) == "SECTION_OF_ACT"] = "sectionofact"
names(violations)[names(violations) == "SIG_SUB"] = "sigandsubdesignation"
names(violations)[names(violations) == "VIOLATOR_TYPE_CD"] = "violatortypecode"

# format variables
violations$violationno = as.character(violations$violationno)
violations$violationno = str_pad(violations$violationno, 7, pad = "0")
violations$eventno = as.character(violations$eventno)
violations$eventno = str_pad(violations$eventno, 7, pad = "0")
violations$mineid = as.character(violations$mineid)
violations$mineid = str_pad(violations$mineid, 7, pad = "0")

# flag and drop duplicates on violationno (only 35)
  # 868722 rows; 61 columns; unique on violation_no
violations[, "dup"] = duplicated(violations$violationno) 
violations = violations[violations$dup == F, ]

################################################################################

# keep only useful variables
keep = c("violationno", "violationid", "mineid", 
         "eventno", "quarter", "year",
         "dateterminated", "typeaction1", "typeaction2",
         "dateissued", "datevacated", "dateterminated",
         "partsection", "sectionofact", "sigandsubdesignation", "violatortypecode")
violations = violations[, (names(violations) %in% keep)] 

################################################################################

# output violation-level data
  # 868722 rows; 14 columns; unique on violationno
saveRDS(violations, file = violations.out.file.name)

################################################################################

rm(list = ls())

################################################################################
