# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Merge Violations
  # Merges assessments and inspection information onto violations, and then merges on cfr key

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/2_merged", collapse = NULL) 

# inputs
  # violations data
violations.in.file.name = paste0(clean.path, "/clean_violations.rds", collapse = NULL)
  # inspections data
inspections.in.file.name = paste0(clean.path, "/clean_inspections.rds", collapse = NULL)
  # assessments data
assessments.in.file.name = paste0(clean.path, "/clean_assessments.rds", collapse = NULL)
  # cfr key
cfr.key.in.file.name = paste0(clean.path, "/clean_cfr_key.rds", collapse = NULL)

# outputs
  # merged violations
violations.out.file.name = paste0(merged.path, "/merged_violations.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(merged.path, recursive = TRUE)

################################################################################

# load violations
  # 868722 rows; 14 columns; unique on violationno
violations = readRDS(violations.in.file.name)

# load inspections
  # 192652 rows; 10 columns; unique on eventno
inspections = readRDS(inspections.in.file.name)

# load assessments
  # 843818 rows; 13 columns; unique on violationno
assessments = readRDS(assessments.in.file.name)

# load cfr key
  # 2026 rows; 11 columns; unique on subsection_code
cfr.key = readRDS(cfr.key.in.file.name)

################################################################################

# output violations-level data
saveRDS(violations, file = violations.out.file.name)

################################################################################

rm(list = ls())

################################################################################
