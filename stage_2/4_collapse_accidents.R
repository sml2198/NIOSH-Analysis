# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Collapse Accidents Data
  # Merges mine type data (produced in 1_clean_mines) and classified accidents 
    # data (produced in 9_classify_MR and 10_classify_PS) to produce mine-quarter-level data
  # Collapses data to mine-year level

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/7/2017

################################################################################

library(plyr)
library(stringr)
library(zoo)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL)
classified.path = paste0(root, "/data/3_coded", collapse = NULL) 
collapsed.path = paste0(root, "/data/4_collapsed", collapse = NULL) 

# inputs 
  # cleaned mine types data
    # produced in 1_clean_mines
mine.types.file.name = paste0(cleaned.path, "/clean_mine_types.rds", collapse = NULL)
  # classified MR accidents data 
    # produced in 9_classify_MR
MR.accidents.coded.in.file.name = paste0(classified.path, "/classified_accidents_MR.rds", collapse = NULL)
  # classified PS accidents data 
    # produced in 10_classify_PS
PS.accidents.coded.in.file.name = paste0(classified.path, "/classified_accidents_PS.rds", collapse = NULL)

# outputs
  # collapsed MR accidents data 
MR.accidents.coded.out.file.name = paste0(collapsed.path, "/collapsed_MR_accidents.rds", collapse = NULL)
  # collapsed PS accidents data 
PS.accidents.coded.out.file.name = paste0(collapsed.path, "/collapsed_PS_accidents.rds", collapse = NULL)

# generate file paths
dir.create(collapsed.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, cleaned.path, classified.path, collapsed.path)

################################################################################

# READ DATA

# read mine type data
  # 86362 rows; 3 columns; unique on mineid
mine.types = readRDS(mine.types.file.name)

# bye
rm(mine.types.file.name)

################################################################################

for (injury in c("MR", "PS")) {

  # READ DATA
  
  if (injury == "MR") {
    in.file.name = MR.accidents.coded.in.file.name
    out.file.name = MR.accidents.coded.out.file.name
  }
  if (injury == "PS") {
    in.file.name = PS.accidents.coded.in.file.name
    out.file.name = PS.accidents.coded.out.file.name
  }
  
  # read classified accidents data
    # 75016 rows; 4 columns; unique on documentno
  classified.accidents = readRDS(in.file.name)
  
  # bye
  rm(in.file.name)
  
  ##############################################################################
  
  # FORMAT DATA
  
  # format MR/PS (0 = no, 1 = yes)
  classified.accidents[, injury] = as.numeric(as.character(classified.accidents[, injury]))
  
  # drop unnecessary variables
  classified.accidents$documentno = NULL

  # format mineid
  classified.accidents$mineid = str_pad(classified.accidents$mineid, 7, pad = "0")

  # format date variables
  classified.accidents$accidentdate = as.Date(as.character(classified.accidents$accidentdate), "%m/%d/%Y")
  classified.accidents$quarter = as.yearqtr(classified.accidents$accidentdate)
  classified.accidents$year = format(as.Date(classified.accidents$accidentdate, format = "%d/%m/%Y"),"%Y")
  
  # generate total injuries variable
  classified.accidents$total_injuries = 1
  
  ##############################################################################
  
  # MERGE DATA
  
  # merge mine types data and classified accidents data
    # 159909 rows; 8 columns
  classified.accidents = merge(classified.accidents, mine.types, by = c("mineid"), all = TRUE)
  
  # drop non-merging observations
    # 75016 rows; 8 columns
  classified.accidents = classified.accidents[!is.na(classified.accidents[, injury]), ]
  
  # drop data from environments not of interest
    # 64989 rows; 8 columns
  classified.accidents = classified.accidents[which(classified.accidents$minetype == "Underground" & 
                                                        classified.accidents$coalcormetalmmine == "C"), ]

  ##############################################################################
  
  # COLLAPSE DATA
  
  # collapse data to mine-year level
    # 6597 rows; 4 columns; unique on mineid-year
  collapsed.accidents = ddply(classified.accidents[, c("mineid", "year", "total_injuries", injury)], 
                              c("mineid", "year"), 
                              function(x) colSums(x[, c("total_injuries", injury)], na.rm = TRUE))
  
  # bye
  rm(classified.accidents)
  
  ##############################################################################
  
  # OUTPUT DATA
  
  # output accidents data
    # 6597 rows; 4 columns; unique on mineid-year
  saveRDS(collapsed.accidents, out.file.name)
  
  # bye
  rm(out.file.name, collapsed.accidents)

}

################################################################################
  
# bye
rm(list = ls())

################################################################################
