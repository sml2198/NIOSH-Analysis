# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Collapse Accidents Data
  # Loads in coded accidents data from 5_analyze_MR_R.R and formats variables for merge
  # Loads and merges minetype data and drops observations not relevant to the study environment
  # Collapses to the mine-quarter level, the outputs

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/4/17

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
cleaned.path = paste0(root, "/0_originals", collapse = NULL)
coded.path = paste0(root, "/3_coded", collapse = NULL) 
collapsed.path = paste0(root, "/4_collapsed", collapse = NULL) 

#inputs 
  # cleaned mine-types key produced in produced in 1_clean_mines.R
mine.types.file.name = paste0(cleaned.path, "/mine_types.rds", collapse = NULL)
  # coded MR accidents data produced in 5_analyze_MR.R
MR.accidents.coded.in.file.name = paste0(coded.path, "/MR_accidents_with_predictions.rds", collapse = NULL)
  # coded PS accidents data produced in 4_analyze_PS.R
PS.accidents.coded.in.file.name = paste0(coded.path, "/PS_accidents_with_predictions.rds", collapse = NULL)

# outputs
  # collapsed coded accidents data 
MR.accidents.coded.out.file.name = paste0(collapsed.path, "/collapsed_MR_accidents.rds", collapse = NULL)
  # collapsed coded accidents data 
PS.accidents.coded_out.file.name = paste0(collapsed.path, "/collapsed_PS_accidents.rds", collapse = NULL)

# generate file paths
dir.create(collapsed.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# Set preferences 

# injury.type = "PS"
injury.type = "MR"

################################################################################

# LOAD IN CODED ACCIDENTS DATA AND FORMAT VARIABLES FOR MERGE

# Read in data
mine_types = readRDS(mine_types_file_name)

if (injury.type == "PS"){
  mines.accidents.coded = readRDS(PS.accidents.coded.in.file.name)
  
  # remove unnecessary vars
  mines.accidents.coded = mines.accidents.coded[, c(match("mineid", names(mines.accidents.coded)),
                                                    match("accidentdate", names(mines.accidents.coded)),
                                                    match("PS", names(mines.accidents.coded)))]
  # format PS ( 0 = no, 1 = yes)
  mines.accidents.coded$PS = ifelse(mines.accidents.coded$PS == 1, 0, 1)
}
if (injury.type == "MR"){
  mines.accidents.coded = readRDS(MR.accidents.coded.in.file.name)
  
  # remove unnecessary vars
  mines.accidents.coded = mines.accidents.coded[, c(match("mineid", names(mines.accidents.coded)),
                                                    match("accidentdate", names(mines.accidents.coded)),
                                                    match("MR", names(mines.accidents.coded)))]
}

# format mineid by padding it with zeroes to make it 7 digits, so we have a clean merge
mines.accidents.coded$mineid = str_pad(mines.accidents.coded$mineid, 7, pad = "0")
mines.accidents.coded$mineid = withr::with_options(c(scipen = 999), str_pad(mines.accidents.coded$mineid, 7, pad = "0"))

# format date vars
mines.accidents.coded$accidentdate = as.Date(as.character(mines.accidents.coded$accidentdate), "%m/%d/%Y")
mines.accidents.coded$quarter = as.yearqtr(mines.accidents.coded$accidentdate)

################################################################################

# COLLAPSE TO THE MINE-QUARTER LEVEL, THEN OUTPUT

# drop accidents from too early 
mines.accidents.coded = mines.accidents.coded[mines.accidents.coded$quarter > "1999 Q4" & 
                                                mines.accidents.coded$quarter < "2016 Q2",]

# create injury indicator so that we can collapse & sum total injuries per mine quarter
mines.accidents.coded$total_injuries = 1

# merge on minetypes to drop non-coal and non-underground observations before saving
mines.accidents.coded = merge(mines.accidents.coded, mine_types, by = c("mineid"), all = T)

# drop non-merging observations
if (injury.type == "PS"){
  mines.accidents.coded = mines.accidents.coded[!is.na(mines.accidents.coded$PS),]
}
if (injury.type == "MR"){
  mines.accidents.coded = mines.accidents.coded[!is.na(mines.accidents.coded$MR),]
}
rm(mine_types)

# only keep observations from environment we care about
mines.accidents.coded = mines.accidents.coded[mines.accidents.coded$minetype == "Underground",]
mines.accidents.coded = mines.accidents.coded[mines.accidents.coded$coalcormetalmmine == "C",]

################################################################################

# collapse mines_accidents data here, and then save and clear workspace
if (injury.type == "PS"){
  summed_coded_accidents = ddply(mines.accidents.coded[, c(grep("total_injuries", names(mines.accidents.coded)), 
                                                           grep("PS", names(mines.accidents.coded)),
                                                           match("mineid", names(mines.accidents.coded)), 
                                                           match("quarter", names(mines.accidents.coded)))], c("mineid", "quarter"), 
                                 function(x) colSums(x[, c(grep("total_injuries", names(x)), grep("PS", names(x)))], na.rm = T))
  
  saveRDS(summed_coded_accidents, PS.accidents.coded.out.file.name)
}
if (injury.type == "MR"){
  summed_coded_accidents = ddply(mines.accidents.coded[, c(grep("total_injuries", names(mines.accidents.coded)), 
                                                           grep("MR", names(mines.accidents.coded)),
                                                           match("mineid", names(mines.accidents.coded)), 
                                                           match("quarter", names(mines.accidents.coded)))], c("mineid", "quarter"), 
                                 function(x) colSums(x[, c(grep("total_injuries", names(x)), grep("MR", names(x)))], na.rm = T))
  
  saveRDS(summed_coded_accidents, MR.accidents.coded.out.file.name)
}

################################################################################

rm(list = ls())
gc()

################################################################################
