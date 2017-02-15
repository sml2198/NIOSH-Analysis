# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 8 - OPTIONAL Merge Union/Longwall Data
  # Merges union/longwall data (produced in 5_optional_clean_union_longwall)
    # and MR and PS data for stage 3 (produced in 7_prepare_stage_3_data)

# ONLY RUN THIS FILE IF
  # you have access to BOTH the "EIA-data" and "NIOSH-data" data sub-folders
  # you have run 5_optional_clean_union_longwall
  # you intend to run the union/longwall specification test

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com

# Last edit 2/9/2017

################################################################################

library(foreign)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 
prepared.path = paste0(root, "/data/5_prepared", collapse = NULL)

# inputs
  # MR data for stage 3
    # produced in 7_prepare_stage_3_data
MR.data.in.file.name = paste0(prepared.path, "/prepared_stage_3_MR_part_1.rds", collapse = NULL)
  # PS data for stage 3
    # produced in 7_prepare_stage_3_data
PS.data.in.file.name = paste0(prepared.path, "/prepared_stage_3_PS_part_1.rds", collapse = NULL)
  # cleaned union/longwall data
    # produced in 5_optional_clean_union_longwall 
union.longwall.in.file.name = paste0(cleaned.path, "/clean_union_longwall.rds", collapse = NULL)

# outputs
  # MR data for stage 3 with union and longwall indicators (rds)
MR.data.out.rds.file.name = paste0(prepared.path, "/prepared_stage_3_MR_part_1_ulw.rds", collapse = NULL)
  # MR data for stage 3 with union and longwall indicators (dta)
MR.data.out.dta.file.name = paste0(prepared.path, "/prepared_stage_3_MR_part_1_ulw.dta", collapse = NULL)
  # PS data for stage 3 with union and longwall indicators (rds)
PS.data.out.rds.file.name = paste0(prepared.path, "/prepared_stage_3_PS_part_1_ulw.rds", collapse = NULL)
  # PS data for stage 3 with union and longwall indicators (dta)
PS.data.out.dta.file.name = paste0(prepared.path, "/prepared_stage_3_PS_part_1_ulw.dta", collapse = NULL)

# generate file paths
dir.create(prepared.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, cleaned.path, prepared.path)

################################################################################

# READ DATA

# read cleaned union/longwall data
  # 24403 rows; 4 columns; unique on mineid-year
ulw = readRDS(union.longwall.in.file.name)

# bye
rm(union.longwall.in.file.name)

################################################################################

for (injury in c("MR", "PS")) {
  
  # READ DATA

  # read MR/PS data for stage 3
    # MR: 6253 rows; 350 columns; unique on mineid-year
    # PS: 6253 rows; 110 columns; unique on mineid-year
  if (injury == "MR") {
    data = readRDS(MR.data.in.file.name)
    rm(MR.data.in.file.name)
  }
  if (injury == "PS") {
    data = readRDS(PS.data.in.file.name)
    rm(PS.data.in.file.name)
  }
  
  ##############################################################################
  
  # MERGE UNION/LONGWALL DATA AND PREPARED DATA

  # merge union/longwall data and prepared data
    # MR: 25185 rows; 352 columns
    # PS: 25185 rows; 112 columns; unique on mineid-year
  merged.data = merge(data, ulw, by = c("mineid", "year"), all = TRUE)
  
  # drop non-merging observations
    # MR: 6253 rows; 352 columns; unique on mineid-year
    # PS: 6253 rows; 112 columns; unique on mineid-year
  merged.data = merged.data[complete.cases(merged.data$hours),]
  
  # replace longwall with 0 if it is not 1 and the year is one for which we have data (2000-2015)
  merged.data$longwall = ifelse(is.na(merged.data$longwall) & merged.data$year < 2016, 0, merged.data$longwall)
  
  # replace union with 0 if it is not 1 and the year is one for which we have data (2000-2013)
  merged.data$union = ifelse(is.na(merged.data$union) & merged.data$year < 2014, 0, merged.data$union)
  
  ##############################################################################
  
  # OUTPUT DATA
  
  # grab correct file paths
  if (injury == "MR") {
    r.file.name = MR.data.out.rds.file.name
    stata.file.name = MR.data.out.dta.file.name
  }
  if (injury == "PS") {
    r.file.name = PS.data.out.rds.file.name
    stata.file.name = PS.data.out.dta.file.name
  }
  
  # output data for stage 3 with union and longwall indicators (rds)
    # MR: 6253 rows; 352 columns; unique on mineid-year
    # PS: 6253 rows; 112 columns; unique on mineid-year
  saveRDS(merged.data, file = r.file.name)
  
  # make data stata-friendly
  stata.names = names(merged.data)
  stata.names = gsub("\\.", "_", stata.names)
  stata.names = gsub("-", "_", stata.names)
  stata.data = merged.data
  names(stata.data) = stata.names
  
  # output data for stage 3 with union and longwall indicators (dta)
    # MR: 6253 rows; 352 columns; unique on mineid-year
    # PS: 6253 rows; 112 columns; unique on mineid-year
  write.dta(stata.data, file = stata.file.name)
  
  # bye
  rm(stata.names, stata.data, r.file.name, stata.file.name)
  
}

################################################################################

# bye
rm(list = ls())

################################################################################
