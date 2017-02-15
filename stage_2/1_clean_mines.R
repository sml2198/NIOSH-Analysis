# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 1 - Clean Mines Data
  # Cleans mines data from the MSHA open data portal

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/7/2017

################################################################################

library(stringr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
originals.path = paste0(root, "/data/0_originals", collapse = NULL)
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 

# inputs
  # mines data from the MSHA open data portal
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
mines.in.file.name = paste0(originals.path, "/Mines.txt", collapse = NULL)

# outputs
  # cleaned mine type data
mine.types.out.file.name = paste0(cleaned.path, "/clean_mine_types.rds", collapse = NULL)
  # cleaned mines data
mines.out.file.name = paste0(cleaned.path, "/clean_mines.rds", collapse = NULL)

# generate file paths 
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) )

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA 

# read mines data
  # 86362 rows; 59 columns; unique on mineid
mines = read.table(mines.in.file.name, header = TRUE, sep = "|", na.strings = c("", "NA"))

# bye
rm(mines.in.file.name)

################################################################################

# CLEAN DATA

# drop unnecessary variables
  # 86362 rows; 8 columns; unique on mineid
mines = mines[, c("COAL_METAL_IND", "CURRENT_MINE_STATUS", "CURRENT_STATUS_DT",
                  "CURRENT_MINE_TYPE", "DISTRICT", "MINE_ID",
                  "SAFETY_COMMITTEE_IND","STATE")]

# rename variables
names(mines)[names(mines) == "COAL_METAL_IND"] = "coalcormetalmmine"
names(mines)[names(mines) == "CURRENT_MINE_STATUS"] = "minestatus"
names(mines)[names(mines) == "CURRENT_STATUS_DT"] = "minestatusdate"
names(mines)[names(mines) == "CURRENT_MINE_TYPE"] = "minetype"
names(mines)[names(mines) == "DISTRICT"] = "district"
names(mines)[names(mines) == "MINE_ID"] = "mineid"
names(mines)[names(mines) == "SAFETY_COMMITTEE_IND"] = "safetycommittee"
names(mines)[names(mines) == "STATE"] = "stateabbreviation"

# format variables
mines$mineid = str_pad(mines$mineid, 7, pad = "0")
mines$district = as.numeric(mines$district)
mines$safetycommittee = ifelse(mines$safetycommittee  == "Y", 1, 0)

# generate appalachia indicator
mines$appalachia = ifelse((mines$stateabbreviation == "VA" |
                             mines$stateabbreviation == "WV" |
                             mines$stateabbreviation == "KY" |
                             mines$stateabbreviation == "PA"), 1, 0)

################################################################################

# OUPUT DATA

# output mine type data
  # 86362 rows; 3 columns; unique on mineid
saveRDS(mines[, c("mineid", "minetype", "coalcormetalmmine")], file = mine.types.out.file.name) 

# output cleaned mines data
  # 86362 rows; 9 columns; unique on mineid
saveRDS(mines, file = mines.out.file.name)

# bye
rm(list = ls())

################################################################################
