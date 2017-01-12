# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Clean Inspections
  # Cleans inspections

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library()

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "/Users/Sarah/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL) 
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs

# outputs

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(clean.path, recursive = TRUE)

################################################################################

################################################################################

# output mine-level data
saveRDS(, file = )

################################################################################

rm(list = ls())

################################################################################
