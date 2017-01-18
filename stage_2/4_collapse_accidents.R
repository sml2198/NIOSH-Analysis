# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Collapse Accidents Data
  # Loads in coded accidents data from classify MR/PS and formats variables for merge
  # Loads and merges minetype data and drops observations not relevant to the study environment
  # Collapses to the mine-quarter level, the outputs

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

#library(plyr)
#library(stringr)
#library(zoo)

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
  # cleaned mine types data produced in 1_clean_mines
mine.types.file.name = paste0(cleaned.path, "/clean_mine_types.rds", collapse = NULL)
  # classified MR accidents data produced in 5_analyze_MR
MR.accidents.coded.in.file.name = paste0(coded.path, "/classified_accidents_MR.rds", collapse = NULL)
  # classified PS accidents data produced in 4_analyze_PS
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
  
  if (injury == "PS") {
    file.name = PS.accidents.coded.in.file.name
  }
  if (injury == "MR") {
    file.name = MR.accidents.coded.in.file.name
  }
  
  # read classified accidents data
    # 75016 rows; 4 columns; unique on documentno
  mines.accidents.coded = readRDS(file.name)
  
  # bye
  rm(file.name)
  
  ##############################################################################
  
  # FORMAT DATA
  
  # format MR/PS (0 = no, 1 = yes)
  mines.accidents.coded[, injury] = as.numeric(mines.accidents.coded[, injury])
  
  # remove documentno
  mines.accidents.coded = mines.accidents.coded[, c(-match("documentno", names(mines.accidents.coded)))]
  
  # format mineid
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
  if (injury == "PS"){
    mines.accidents.coded = mines.accidents.coded[!is.na(mines.accidents.coded$PS),]
  }
  if (injury == "MR"){
    mines.accidents.coded = mines.accidents.coded[!is.na(mines.accidents.coded$MR),]
  }
  
  # only keep observations from environment we care about (we already did this based on the accidents 
  # data in 1_clean.accidents.R but doing it based on mines data is more thorough, so we do that now here)
    # 64989 rows; 8 columns; unique on documentno
  mines.accidents.coded = mines.accidents.coded[mines.accidents.coded$minetype == "Underground",]
  mines.accidents.coded = mines.accidents.coded[mines.accidents.coded$coalcormetalmmine == "C",]
  
  ################################################################################
  
  # collapse accidents data here to the year level, and then save
  if (injury == "PS"){
    # 6597 rows; 4 columns; unique on mineid-year
    summed.accidents = ddply(mines.accidents.coded[, c(match("total_injuries", names(mines.accidents.coded)), 
                                                       match("PS", names(mines.accidents.coded)),
                                                       match("mineid", names(mines.accidents.coded)), 
                                                       match("year", names(mines.accidents.coded)))], c("mineid", "year"), 
                                   function(x) colSums(x[, c(grep("total_injuries", names(x)), grep("PS", names(x)))], na.rm = T))
    
    saveRDS(summed.accidents, PS.accidents.coded.out.file.name)
  }
  if (injury == "MR"){
      # 6597 rows; 4 columns; unique on mineid-year
    summed.accidents = ddply(mines.accidents.coded[, c(grep("total_injuries", names(mines.accidents.coded)), 
                                                       grep("MR", names(mines.accidents.coded)),
                                                       match("mineid", names(mines.accidents.coded)), 
                                                       match("year", names(mines.accidents.coded)))], c("mineid", "year"), 
                                   function(x) colSums(x[, c(grep("total_injuries", names(x)), grep("MR", names(x)))], na.rm = T))
    
    saveRDS(summed.accidents, MR.accidents.coded.out.file.name)
  }
  
  ################################################################################

}

################################################################################
  
rm(list = ls())

################################################################################
