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
      # NOTE: Files are run in Stata at this point
    # 12_analyze_randomization_1
      # NOTE: Files are run in Stata at this point
    # 14_analyze_randomization_2
    # 15_analyze_prediction

# NOTES:
  # (1) Unlike the run files for stage 1 and 2, this file cannot be run in full
    # Stage 3 of the project requires Stata code and so you will need to switch 
    # to Stata to run some do-files in the middle of this file
    # Follow instructions, beginning line 38, CAREFULLY
  # (2) This project was generated for R 3.2.3 and Stata 14 MP
    # Older versions of these programs may not be compatible with all commands
    # utilized in these files
  # (3) The randomization inference procedures which are conducted in Stata
    # (see 11_randomization_inference_method_1.do and 1
    # 3_randomization_inference_method_2.do) are designed to parallelize the
    # the analysis (run on four clusters on one machine) to expedite the analysis
    # These files will take an extremely long time to run (several days) and
    # will use ~80-90% of your computer's processing power during that time

# INSTRUCTIONS:
  # (1) Read notes on line 23, then read all instructions below
  # (2) See note on line 63
  # (3) Set working directory on line 91
  # (4) Set working directories for all files in "/NIOSH-Analysis/programs/stage_3/"
  # (5) Run this file from lines 70-114
  # (6) Run the following files in the following order in Stata MP (version 14):
    # (a) 9_prepare_stage_3_stata.do
    # (b) 10_fit_models.do
    # (c) 11_randomization_inference_method_1.do
      # NOTE: This file calls 11_helper_file_RI_method_1.do
  # (7) Run this file on line 127
  # (8) Run the following files in the following order in Stata MP (version 14)
    # (a) 13_randomization_inference_method_2.do
      # NOTE: This file calls 13_helper_file_RI_method_2.do
  # (9) Run this file from lines 140-146

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

setwd("/NIOSH-Analysis/programs/stage_3/")

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

# RUN THE FOLLOWING FILES IN STATA:
  # 9_prepare_stage_3_stata.do
  # 10_fit_models.do
  # 11_randomization_inference_method_1.do
    # NOTE: This file calls 11_helper_file_RI_method_1.do

################################################################################

# RUN FILES - 2

source("12_analyze_randomization_1.R")

################################################################################

# RUN THE FOLLOWING FILES IN STATA:
  
  # 13_randomization_inference_method_2.do
   # NOTE: This file calls 13_helper_file_RI_method_2.do

################################################################################

# RUN FILES - 3

source("14_analyze_randomization_2.R")
source("15_analyze_prediction.R")

################################################################################

# bye
rm(list = ls())

################################################################################
