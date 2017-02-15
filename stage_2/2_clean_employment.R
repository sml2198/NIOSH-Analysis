# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Clean Employment Data
  # Cleans employment data from the MSHA open data portal

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/7/2017

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
  # employment/production data from the MSHA open data portal
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
employment.in.file.name = paste0(originals.path, "/MinesProdQuarterly.txt", collapse = NULL)

# outputs
  # cleaned employment/production data
employment.out.file.name = paste0(cleaned.path, "/clean_employment.rds", collapse = NULL)

# generate file paths 
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read employment data
  # 1803837 rows; 13 columns
employment = read.table(employment.in.file.name, header = TRUE, sep = "|", na.strings = c("", "NA"))

# bye
rm(employment.in.file.name)

################################################################################

# CLEAN DATA

# drop data from environments not of interest
  # 42019 rows; 13 columns; unique on mine-year-quarter
employment = employment[which(employment$COAL_METAL_IND == "C" & employment$SUBUNIT == "UNDERGROUND"), ]

# drop unnecessary variables
  # 42019 rows; 6 columns; unique on mine-year-quarter
employment = employment[, c("AVG_EMPLOYEE_CNT", "CAL_QTR", "CAL_YR",
                            "COAL_PRODUCTION", "HOURS_WORKED", "MINE_ID")]

# rename variables
names(employment)[names(employment) == "AVG_EMPLOYEE_CNT"] = "employment_qtr"
names(employment)[names(employment) == "CAL_QTR"] = "quarter"
names(employment)[names(employment) == "CAL_YR"] = "year"
names(employment)[names(employment) == "COAL_PRODUCTION"] = "prod_qtr"
names(employment)[names(employment) == "HOURS_WORKED"] = "hours_qtr"
names(employment)[names(employment) == "MINE_ID"] = "mineid"

# format variables
employment$mineid = str_pad(employment$mineid, 7, pad = "0")

################################################################################

# OUPUT DATA

# output cleaned employment/production data
  # 42019 rows; 6 variables; unique on mine-year-quarter
saveRDS(employment, file = employment.out.file.name)

# bye
rm(list = ls())

################################################################################
