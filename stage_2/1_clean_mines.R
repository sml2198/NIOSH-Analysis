# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 1 - Clean Mines Data
  # Cleans mines data from the MSHA open data portal

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/4/17

################################################################################

library(stringr)

################################################################################

# define root directory

# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
input.path = paste0(root, "/0_originals", collapse = NULL)
output.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # mines data from the MSHA open data portal
open.data.mines.file.name = paste0(input.path, "/Mines.txt", collapse = NULL)

# outputs
  # clean mine type data (mine id, mine type, and coal status)
mine.types.file.name = paste0(output.path, "/clean_mine_types.rds", collapse = NULL)
  # clean mines data
clean.mines.file.name = paste0(output.path, "/clean_mines.rds", collapse = NULL)

# generate file paths 
dir.create(output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

################################################################################

# READ AND CLEAN MINES DATA

# read mines data
  # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
  # 86362 rows; 59 columns; unique on mineid
mines = read.table(open.data.mines.file.name, header = T, sep = "|", na.strings = c("", "NA"))

# rename variables
names(mines)[names(mines) == "MINE_ID"] = "mineid"
names(mines)[names(mines) == "MINE_NAME"] = "minename"
names(mines)[names(mines) == "MINE_TYPE"] = "minetype"
names(mines)[names(mines) == "COAL_METAL_IND"] = "coalcormetalmmine"
names(mines)[names(mines) == "CURRENT_MINE_TYPE"] = "minetype"
names(mines)[names(mines) == "CURRENT_MINE_NAME"] = "minename"
names(mines)[names(mines) == "CURRENT_MINE_STATUS"] = "minestatus"
names(mines)[names(mines) == "CURRENT_STATUS_DT"] = "minestatusdate"
names(mines)[names(mines) == "CURRENT_CONTROLLER_ID"] = "controllerid"
names(mines)[names(mines) == "CURRENT_CONTROLLER_NAME"] = "controllername"
names(mines)[names(mines) == "CURRENT_OPERATOR_ID"] = "operatorid"
names(mines)[names(mines) == "CURRENT_OPERATOR_NAME"] = "operatorname"
names(mines)[names(mines) == "STATE"] = "stateabbreviation"
names(mines)[names(mines) == "BOM_STATE_CD"] = "bomstatecode"
names(mines)[names(mines) == "FIPS_CNTY_CD"] = "fipscountycode"
names(mines)[names(mines) == "FIPS_CNTY_NM"] = "fipscountyname"
names(mines)[names(mines) == "CURRENT_CONTROLLER_BEGIN_DT"] = "controllerbegindate"
names(mines)[names(mines) == "OFFICE_CD"] = "officecode"
names(mines)[names(mines) == "OFFICE_NAME"] = "officename"
names(mines)[names(mines) == "PRIMARY_SIC"] = "primarysicdesc"
names(mines)[names(mines) == "PRIMARY_SIC_CD"] = "primarysiccode"
names(mines)[names(mines) == "PRIMARY_SIC_CD_1"] = "primarysiccodegroup"
names(mines)[names(mines) == "PRIMARY_SIC_CD_SFX"] = "primarysiccodesuffix"
names(mines)[names(mines) == "PRIMARY_CANVASS_CD"] = "primarycanvasscode"
names(mines)[names(mines) == "PRIMARY_CANVASS"] = "primarycanvasscodedesc"
names(mines)[names(mines) == "SECONDARY_CANVASS_CD"] = "secondarycanvasscode"
names(mines)[names(mines) == "SECONDARY_CANVASS"] = "secondarycanvasscodedesc"
names(mines)[names(mines) == "CURRENT_103I"] = "idesc"
names(mines)[names(mines) == "CURRENT_103I_DT"] = "idate"
names(mines)[names(mines) == "PORTABLE_OPERATION"] = "portableoperationindicator"
names(mines)[names(mines) == "PORTABLE_FIPS_ST_CD"] = "portablefipsstatecode"
names(mines)[names(mines) == "DAYS_PER_WEEK"] = "daysperweek"
names(mines)[names(mines) == "HOURS_PER_SHIFT"] = "hourspershift"
names(mines)[names(mines) == "PART48_TRAINING"] = "part48training"
names(mines)[names(mines) == "PROD_SHIFTS_PER_DAY"] = "productionshiftsperday"
names(mines)[names(mines) == "MAINT_SHIFTS_PER_DAY"] = "maintenanceshiftsperday"
names(mines)[names(mines) == "NO_EMPLOYEES"] = "numberofemployees"
names(mines)[names(mines) == "AVG_MINE_HEIGHT"] = "averagemineheight"
names(mines)[names(mines) == "MINE_GAS_CATEGORY_CD"] = "minegascategorycode"
names(mines)[names(mines) == "METHANE_LIBERATION"] = "methaneliberation"
names(mines)[names(mines) == "NO_PRODUCING_PITS"] = "noofproducingpits"
names(mines)[names(mines) == "NO_NONPRODUCING_PITS"] = "noofnonproducingpits"
names(mines)[names(mines) == "NO_TAILING_PONDS"] = "nooftailingponds"
names(mines)[names(mines) == "PILLAR_RECOVERY_USED"] = "roomandpillarindicator"
names(mines)[names(mines) == "HIGHWALL_MINER_USED"] = "highwallminerindicator"
names(mines)[names(mines) == "MULTIPLE_PITS"] = "multiplepitsindicator"
names(mines)[names(mines) == "MINERS_REP_IND"] = "minersrepindicator"
names(mines)[names(mines) == "SAFETY_COMMITTEE_IND"] = "safetycommittee"
names(mines)[names(mines) == "MILES_FROM_OFFICE"] = "milesfromoffice"
names(mines)[names(mines) == "DIRECTIONS_TO_MINE"] = "directionstominemodified"
names(mines)[names(mines) == "NEAREST_TOWN"] = "nearesttown"
names(mines) = tolower(names(mines))

# format mineid
mines$mineid = str_pad(mines$mineid, 7, pad = "0")

################################################################################

# OUPUT CLEAN MINE TYPE DATA AND CLEAN MINES DATA

# output mine type data
  # this file is smaller than the full mines data, so it is easier to load in subsequent files
  # 86362 rows; 3 columns; unique on mineid
mine.types = mines[, c("mineid", "minetype", "coalcormetalmmine")]
saveRDS(mine.types, file = mine.types.file.name) 

# output clean mines data
  # 86362 rows; 59 columns; unique on mineid
saveRDS(mines, file = clean.mines.file.name)

################################################################################

rm(list = ls())

################################################################################
