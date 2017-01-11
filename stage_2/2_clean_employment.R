# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Clean Employment Data
  # Cleans employment data from the MSHA open data portal

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/4/17

################################################################################

library(stringr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
input.path = paste0(root, "/0_originals", collapse = NULL)
output.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # quarterly employment/production data from the MSHA open data portal 
employment.in.file.name = paste0(input.path, "/MinesProdQuarterly.txt", collapse = NULL)

# outputs
  # clean quarterly employment/production data
employment.out.file.name = paste0(output.path, "/clean_employment.rds", collapse = NULL)

# generate file paths 
dir.create(output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

################################################################################

# READ AND CLEAN QUARTERLY EMPLOYMENT/PRODUCTION DATA

# read quarterly employment/production data
  # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
  # 1803837 rows; 13 columns; unique on mine-year-quarter
employment = read.table(employment.in.file.name, header = TRUE, sep = "|", na.strings = c("", "NA"))

# drop unnecessary variables
employment$COAL_METAL_IND = 
  employment$STATE = 
  employment$CURR_MINE_NM = 
  employment$FISCAL_YR = 
  employment$FISCAL_QTR = 
  employment$SUBUNIT = 
  employment$SUBUNIT_CD = NULL

# rename variables
names(employment)[names(employment) == "CAL_YR"] = "year"
names(employment)[names(employment) == "CAL_QTR"] = "quarter"
names(employment)[names(employment) == "MINE_ID"] = "mineid"
names(employment)[names(employment) == "HOURS_WORKED"] = "hours_qtr"
names(employment)[names(employment) == "AVG_EMPLOYEE_CNT"] = "avg_employee_cnt_qtr"
names(employment)[names(employment) == "COAL_PRODUCTION"] = "coal_prod_qtr"

# format mineid
employment$mineid = str_pad(employment$mineid, 7, pad = "0")

################################################################################

# OUPUT CLEAN QUARTERLY EMPLOYMENT/PRODUCTION DATA

# output clean employment data
  # 1803837 rows; 6 variables; unique on mine-year-quarter
saveRDS(employment, file = employment.out.file.name)

################################################################################

rm(list = ls())

################################################################################
