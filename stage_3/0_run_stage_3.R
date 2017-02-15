# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 0 - Run Stage 3
  # Runs files to complete analyses for Stage 3
    # 1_clean_violations
    # 2_clean_assessments
    # 3_clean_inspections
    # 4_clean_cfr_key 
    # 5_optional_clean_union_longwall
    # 6_merge_violations
    # 7_prepare_stage_3_data
    # 8_optional_merge_union_longwall

# NOTE:
  # Unlike the run files for stage 1 and 2, this file cannot be run in full
  # Because stage 3 of the project requires Stata code, you will need to switch 
    # to Stata to run some do-files in the middle of this file
  # Follow instructions given in this file CAREFULLY

# Coded by: Julia Bodson, juliabodson@gmail.com

# Last edit 2/9/2017

################################################################################

# SETTINGS

# NOTE:
# Some files in stage 3 rely on data that not all users will have access to
  # Select "ULW = ON" if:
    # You have access to BOTH the "EIA-data" and "NIOSH-data" data sub-folders
    # You intend to run the union/longwall specification test
  # Otherwise select "ULW = OFF"

ULW = "ON"
# ULW = "OFF"

################################################################################

# LOAD PACKAGES

install.packages("foreign")
install.packages("plyr")
install.packages("psych")
install.packages("stringr")
install.packages("zoo")

library(foreign)
library(plyr)
library(psych)
library(stringr)
library(zoo)

################################################################################

# setwd("/NIOSH-Analysis/programs/stage_3/")
# setwd("C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/")
setwd("C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_3/")

################################################################################

# RUN FILES - 1

if (ULW == "ON") {
  source("1_clean_violations.R")
  source("2_clean_assessments.R")
  source("3_clean_inspections.R")
  source("4_clean_cfr_key.R")
  source("5_optional_clean_union_longwall.R")
  source("6_merge_violations.R")
  source("7_prepare_stage_3_data.R")
  source("8_optional_merge_union_longwall.R")
} else {
  source("1_clean_violations.R")
  source("2_clean_assessments.R")
  source("3_clean_inspections.R")
  source("4_clean_cfr_key.R")
  source("6_merge_violations.R")
  source("7_prepare_stage_3_data.R")
}

################################################################################

################################################################################

################################################################################

################################################################################

