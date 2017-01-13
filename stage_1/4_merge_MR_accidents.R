# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Merge MR (Maintenance and Repair) Accidents
  # Train/Test:

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

library(stringr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.input.path = paste0(root, "/0_originals", collapse = NULL)
cleaned.output.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # master MR dataset
    # coded by NIOSH representatives and sent to the Morantz team on 8/2/2015
training.set.in.file.name = paste0(originals.input.path, "/training-sets/Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", collapse = NULL)

# outputs
  # clean MR training set
training.set.out.file.name = paste0(cleaned.output.path, "/clean_MR_training_set.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################




################################################################################

# output clean training set
  # 1000 rows; 104 columns; unique on documentno
saveRDS(ps.data, file = training.set.out.file.name)

################################################################################

rm(list = ls())

################################################################################
