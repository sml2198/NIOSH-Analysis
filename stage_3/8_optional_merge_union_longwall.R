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

library(foreign)

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
  # prediction-ready MR data (part 1)
MR.data.in.file.name = paste0(prepped.path, "/prepared_stage_3_MR_part_1.rds", collapse = NULL)
  # prediction-ready PS data
PS.data.in.file.name = paste0(prepped.path, "/prepared_stage_3_PS_part_1.rds", collapse = NULL)
  # cleaned union/longwall data
union.longwall.in.file.name = paste0(clean.path, "/clean_union_longwall.rds", collapse = NULL)

# outputs
  # prediction-ready MR data with union and longwall indicators (part 1)
MR.data.out.file.name = paste0(prepped.path, "/prepared_stage_3_MR_part_1_ulw", collapse = NULL)
  # prediction-ready PS data with union and longwall indicators (part 1)
PS.data.out.file.name = paste0(prepped.path, "/prepared_stage_3_PS_part_1_ulw", collapse = NULL)

################################################################################

# LOAD DATA

  # 24403 observations, 4 variables, unique on mineid-year
ulw = readRDS(union.longwall.in.file.name)

for (injury in c("MR", "PS")) { # create separate datasets for MR and PS injuries
  
  ##############################################################################

  # load prepped injury-specific datasets, unique at mineid-year
  if (injury == "MR") {
      # 6253 observations, 350 variables, unique on mineid-year
    data = readRDS(MR.data.in.file.name)
  }
  
  if (injury == "PS") {
      # 6253 observations, 110 variables, unique on mineid-year
    data = readRDS(PS.data.in.file.name)
  }
  
  # merge on union and longwall fields
  merged.data = merge(data, ulw, by = c("mineid", "year"), all = TRUE)
  
  # drop non-merging fields
    # MR: 6253 observations, 352 variables, unique on mineid-year
    # PS: 6253 observations, 112 variables, unique on mineid-year
  merged.data = merged.data[complete.cases(merged.data$hours),]
  
  # replace longwall with zero if it's not a 1 and the year is one for which we have data (2000-2015)
  merged.data$longwall = ifelse(is.na(merged.data$longwall) & merged.data$year < 2016, 0, merged.data$longwall)
  
  # replace union with zero if it's not a 1 and the year is one for which we have data (2000-2013)
  merged.data$union = ifelse(is.na(merged.data$union) & merged.data$year < 2014, 0, merged.data$union)
  
  ##############################################################################
  
  # OUTPUT DATA
  
  # create file names
  if (injury == "MR") {
    r.file.name = paste0(MR.data.out.file.name, ".rds")
    stata.file.name = paste0(MR.data.out.file.name, ".dta")
  }
  
  if (injury == "PS") {
    r.file.name = paste0(PS.data.out.file.name, ".rds")
    stata.file.name = paste0(PS.data.out.file.name, ".dta")
  }
  
  # output prediction-ready mine-year data as an R dataset
    # MR: 6253 rows; 352 columns; unique on mineid-year
    # PS: 6253 rows; 112 columns; unique on mineid-year
  saveRDS(merged.data, file = r.file.name)
  
  # remove special characters from data names so it's stata-friendly
  stata.names = names(merged.data)
  stata.names = gsub("\\.", "_", stata.names)
  stata.names = gsub("-", "_", stata.names)
  stata.data = merged.data
  names(stata.data) = stata.names
  
  # output prediction-ready mine-year data as a dta for stata
    # MR: 6253 rows; 352 columns; unique on mineid-year
    # PS: 6253 rows; 112 columns; unique on mineid-year
  write.dta(stata.data, file = stata.file.name)
  
  # bye
  rm(stata.names, stata.data, r.file.name, stata.file.name)
  
  ################################################################################
  
} # end of the PS/MR loop

################################################################################

rm(list = ls())

################################################################################
