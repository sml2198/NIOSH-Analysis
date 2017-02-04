# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Collapse Accidents Data
  # Merges mine type data (produced in 1_clean_mines) and classified accidents 
    # data (produced in 10_classify_MR and 11_classify_PS) to produce mine-quarter-level data
  # Collapses data to mine-year level

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library(plyr)
library(stringr)
library(zoo)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
cleaned.path = paste0(root, "/1_cleaned", collapse = NULL)
coded.path = paste0(root, "/3_coded", collapse = NULL) 
collapsed.path = paste0(root, "/4_collapsed", collapse = NULL) 

# inputs 
  # cleaned mine types data
    # produced in 1_clean_mines
mine.types.file.name = paste0(cleaned.path, "/clean_mine_types.rds", collapse = NULL)
  # classified MR accidents data 
    # produced in 10_classify_MR
MR.accidents.coded.in.file.name = paste0(coded.path, "/classified_accidents_MR.rds", collapse = NULL)
  # classified PS accidents data 
    # produced in 11_classify_PS
PS.accidents.coded.in.file.name = paste0(coded.path, "/classified_accidents_PS.rds", collapse = NULL)

# outputs
  # collapsed MR accidents data 
MR.accidents.coded.out.file.name = paste0(collapsed.path, "/collapsed_MR_accidents.rds", collapse = NULL)
  # collapsed PS accidents data 
PS.accidents.coded.out.file.name = paste0(collapsed.path, "/collapsed_PS_accidents.rds", collapse = NULL)

# generate file paths
dir.create(collapsed.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, cleaned.path, coded.path, collapsed.path)

################################################################################

# READ MINE TYPE DATA

# read mine type data
  # 86362 rows; 3 columns; unique on mineid
mine.types = readRDS(mine.types.file.name)

# bye
rm(mine.types.file.name)

################################################################################

for (injury in c("MR", "PS")) { # collapse data for MR and PS injuries

  ##############################################################################
  
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
    # MR: 75016 rows; 4 columns; unique on documentno
  mines.accidents.coded = readRDS(in.file.name)
  
  ##############################################################################
  
  # FORMAT DATA
  
  # format MR/PS (0 = no, 1 = yes)
  mines.accidents.coded[, injury] = as.numeric(as.character(mines.accidents.coded[, injury]))
  
  # drop unnecessary variable
  mines.accidents.coded$documentno = NULL

  # format mineid
  mines.accidents.coded$mineid = str_pad(mines.accidents.coded$mineid, 7, pad = "0")

  # format date variables
  mines.accidents.coded$accidentdate = as.Date(as.character(mines.accidents.coded$accidentdate), "%m/%d/%Y")
  mines.accidents.coded$quarter = as.yearqtr(mines.accidents.coded$accidentdate)
  mines.accidents.coded$year = format(as.Date(mines.accidents.coded$accidentdate, format = "%d/%m/%Y"),"%Y")
  
  # generate total injuries variable
  mines.accidents.coded$total_injuries = 1
  
  ################################################################################
  
  # MERGE DATA
  
  # merge mine type data and classified accidents data
    # 159909 rows; 8 columns; unique on documentno
  mines.accidents.coded = merge(mines.accidents.coded, mine.types, by = c("mineid"), all = TRUE)
  
  # drop non-merging observations
    # 75016 rows; 8 columns; unique on documentno
  mines.accidents.coded = mines.accidents.coded[!is.na(mines.accidents.coded[, injury]), ]
  
  # drop data from environments not of interest
    # 64989 rows; 8 columns; unique on documentno
  mines.accidents.coded = mines.accidents.coded[which(mines.accidents.coded$minetype == "Underground" & 
                                                        mines.accidents.coded$coalcormetalmmine == "C"), ]

  ################################################################################
  
  # COLLAPSE AND OUTPUT DATA
  
  # collapse data to mine-year level
    # 6597 rows; 4 columns; unique on mineid-year
  collapsed.accidents = ddply(mines.accidents.coded[, c("mineid", "year", "total_injuries", injury)], 
                              c("mineid", "year"), 
                              function(x) colSums(x[, c("total_injuries", injury)], na.rm = TRUE))
  
  # output mine-year-level dataset
    # 6597 rows; 4 columns; unique on mineid-year
  saveRDS(collapsed.accidents, out.file.name)
  
  # bye
  rm(in.file.name, out.file.name, mines.accidents.coded, collapsed.accidents)

}

################################################################################
  
# bye
rm(list = ls())

################################################################################
