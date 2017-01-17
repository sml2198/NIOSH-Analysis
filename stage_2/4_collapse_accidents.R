# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Collapse Accidents Data
  # Loads in coded accidents data from classify MR/PS and formats variables for merge
  # Loads and merges minetype data and drops observations not relevant to the study environment
  # Collapses to the mine-quarter level, the outputs

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
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
  # cleaned mine-types key produced in produced in 1_clean_mines.R
mine.types.file.name = paste0(cleaned.path, "/clean_mine_types.rds", collapse = NULL)
  # classified MR accidents data produced in 5_analyze_MR.R
MR.accidents.coded.in.file.name = paste0(coded.path, "/classified_accidents_MR.rds", collapse = NULL)
  # classified PS accidents data produced in 4_analyze_PS.R
PS.accidents.coded.in.file.name = paste0(coded.path, "/classified_accidents_PS.rds", collapse = NULL)

# outputs
  # collapsed coded accidents data 
MR.accidents.coded.out.file.name = paste0(collapsed.path, "/collapsed_MR_accidents.rds", collapse = NULL)
  # collapsed coded accidents data 
PS.accidents.coded.out.file.name = paste0(collapsed.path, "/collapsed_PS_accidents.rds", collapse = NULL)

# generate file paths
dir.create(collapsed.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# DEFINE LOOP THAT WILL ITERATE THROUGH PS AND MR INJURIES

for (injury.type in c("PS", "MR")) { # make datasets for MR/PS INJURIES

  ################################################################################
  
  # LOAD IN CODED ACCIDENTS DATA AND FORMAT VARIABLES FOR MERGE
  
  # Read in data
  mine.types = readRDS(mine.types.file.name)
  
  if (injury.type == "PS"){
    # import data
      # 75016 rows; 4 columns; unique on documentno
    mines.accidents.coded = readRDS(PS.accidents.coded.in.file.name)
  
    # format PS (0 = no, 1 = yes)
    mines.accidents.coded$PS = as.numeric(mines.accidents.coded$PS)
  }
  if (injury.type == "MR"){
    # import data
      # 75016 rows; 4 columns; unique on documentno
    mines.accidents.coded = readRDS(MR.accidents.coded.in.file.name)
    
    # format MR (0 = no, 1 = yes)
    mines.accidents.coded$MR = as.numeric(mines.accidents.coded$MR)
  }
  
  # remove documentno (not necessary anymore)
  mines.accidents.coded = mines.accidents.coded[, c(-match("documentno", names(mines.accidents.coded)))]
  
  # format mineid by padding it with zeroes to make it 7 digits, so we have a clean merge
  mines.accidents.coded$mineid = str_pad(mines.accidents.coded$mineid, 7, pad = "0")
  mines.accidents.coded$mineid = withr::with_options(c(scipen = 999), str_pad(mines.accidents.coded$mineid, 7, pad = "0"))
  
  # format date vars
  mines.accidents.coded$accidentdate = as.Date(as.character(mines.accidents.coded$accidentdate), "%m/%d/%Y")
  mines.accidents.coded$quarter = as.yearqtr(mines.accidents.coded$accidentdate)
  mines.accidents.coded$year = format(as.Date(mines.accidents.coded$accidentdate, format = "%d/%m/%Y"),"%Y")
  
  ################################################################################
  
  # COLLAPSE TO THE MINE-YEAR LEVEL, THEN OUTPUT
  
  # create injury indicator so that we can collapse & sum total injuries per mine quarter
  mines.accidents.coded$total_injuries = 1
  
  # merge on minetypes to drop non-coal and non-underground observations before saving
  mines.accidents.coded = merge(mines.accidents.coded, mine.types, by = c("mineid"), all = T)
  
  # drop non-merging observations (not accidents)
    # 75016 rows; 8 columns; unique on documentno
  if (injury.type == "PS"){
    mines.accidents.coded = mines.accidents.coded[!is.na(mines.accidents.coded$PS),]
  }
  if (injury.type == "MR"){
    mines.accidents.coded = mines.accidents.coded[!is.na(mines.accidents.coded$MR),]
  }
  rm(mine.types)
  
  # only keep observations from environment we care about (we already did this based on the accidents 
  # data in 1_clean.accidents.R but doing it based on mines data is more thorough, so we do that now here)
    # 64989 rows; 8 columns; unique on documentno
  mines.accidents.coded = mines.accidents.coded[mines.accidents.coded$minetype == "Underground",]
  mines.accidents.coded = mines.accidents.coded[mines.accidents.coded$coalcormetalmmine == "C",]
  
  ################################################################################
  
  # collapse accidents data here to the year level, and then save
  if (injury.type == "PS"){
    # 6597 rows; 4 columns; unique on mineid-year
    summed.accidents = ddply(mines.accidents.coded[, c(match("total_injuries", names(mines.accidents.coded)), 
                                                       match("PS", names(mines.accidents.coded)),
                                                       match("mineid", names(mines.accidents.coded)), 
                                                       match("year", names(mines.accidents.coded)))], c("mineid", "year"), 
                                   function(x) colSums(x[, c(grep("total_injuries", names(x)), grep("PS", names(x)))], na.rm = T))
    
    saveRDS(summed.accidents, PS.accidents.coded.out.file.name)
  }
  if (injury.type == "MR"){
      # 6597 rows; 4 columns; unique on mineid-year
    summed.accidents = ddply(mines.accidents.coded[, c(grep("total_injuries", names(mines.accidents.coded)), 
                                                       grep("MR", names(mines.accidents.coded)),
                                                       match("mineid", names(mines.accidents.coded)), 
                                                       match("year", names(mines.accidents.coded)))], c("mineid", "year"), 
                                   function(x) colSums(x[, c(grep("total_injuries", names(x)), grep("MR", names(x)))], na.rm = T))
    
    saveRDS(summed.accidents, MR.accidents.coded.out.file.name)
  }
  
  ################################################################################

} # end of main MR/PS loop  

################################################################################
  
rm(list = ls())
gc()

################################################################################
