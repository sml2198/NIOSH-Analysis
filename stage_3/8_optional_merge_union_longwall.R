# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 8 - OPTIONAL Merge Union/Longwall Data
  # Merges union and longwall indicators onto prepared stage 3 data

### NOTE ###
# Only run this file if you have access to BOTH the "EIA-data" and "NIOSH-data"
# data sub-folders, and intend to run the union/longwall specification test.

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/26/17

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/2_merged", collapse = NULL) 
prepped.path = paste0(root, "/5_prepared", collapse = NULL)

# inputs
  # prediction-ready MR data
MR.data.in.file.name = paste0(prepped.path, "/prepared_stage_3_MR_part_1", collapse = NULL)
  # prediction-ready PS data
PS.data.in.file.name = paste0(prepped.path, "/prepared_stage_3_PS_part_1", collapse = NULL)


# outputs
  # prediction-ready MR data with union and longwall indicators
MR.data.out.file.name = paste0(prepped.path, "/prepared_stage_3_MR_part_1_ulw", collapse = NULL)
  # prediction-ready PS data with union and longwall indicators
PS.data.out.file.name = paste0(prepped.path, "/prepared_stage_3_PS_part_1_ulw", collapse = NULL)

################################################################################

for (injury in c("MR", "PS")) { # create separate datasets for MR and PS injuries
  
  ##############################################################################

} # end of the PS/MR loop

rm(list = ls())

################################################################################
