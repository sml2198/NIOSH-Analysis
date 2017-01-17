# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 1 - Clean Mines Data
  # Cleans mines data from the MSHA open data portal

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/4/17

################################################################################

library(stringr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
input.path = paste0(root, "/0_originals", collapse = NULL)
output.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # mines data from the MSHA open data portal
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
mines.in.file.name = paste0(input.path, "/Mines.txt", collapse = NULL)

# outputs
  # clean mine type data (mine id, mine type, and coal status)
mine.types.out.file.name = paste0(output.path, "/clean_mine_types.rds", collapse = NULL)
  # clean mines data
mines.out.file.name = paste0(output.path, "/clean_mines.rds", collapse = NULL)

# generate file paths 
dir.create(output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, input.path, output.path)

################################################################################

# READ DATA 

# read mines data
  # 86362 rows; 59 columns; unique on mineid
mines = read.table(mines.in.file.name, header = T, sep = "|", na.strings = c("", "NA"))

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

# format mineid
mines$mineid = str_pad(mines$mineid, 7, pad = "0")

# encode district
mines$district = as.numeric(mines$district)

# clean up safetycommittee
mines$safetycommittee = ifelse(mines$safetycommittee  == "Y", 1, 0)

# generate appalachia indicator
mines$appalachia = ifelse((mines$stateabbreviation == "VA" |
                             mines$stateabbreviation == "WV" |
                             mines$stateabbreviation == "KY" |
                             mines$stateabbreviation == "PA"), 1, 0)

################################################################################

# OUPUT CLEAN DATA

# output mine type data
  # this file is smaller than the full mines data, so it is easier to load in subsequent files
  # 86362 rows; 3 columns; unique on mineid
mine.types = mines[, c("mineid", "minetype", "coalcormetalmmine")]
saveRDS(mine.types, file = mine.types.out.file.name) 

# output clean mines data
  # 86362 rows; 9 columns; unique on mineid
saveRDS(mines, file = mines.out.file.name)

# bye
rm(list = ls())

################################################################################
