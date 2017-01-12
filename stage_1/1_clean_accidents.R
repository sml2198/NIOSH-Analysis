# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 1 - Clean Accidents Data
  # Cleans accidents data from the MSHA open data portal

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/4/17

################################################################################

# install.packages("stringr")
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
  # accidents data from the MSHA open data portal 
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
accidents.in.file.name = paste0(input.path, "/Accidents.txt", collapse = NULL)

# outputs
  # clean accidents data
accidents.out.file.name = paste0(output.path, "/clean_accidents.rds", collapse = NULL)

# generate file paths
dir.create(output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ AND CLEAN ACCIDENTS DATA 

# read accidents data
  # 212611 rows; 57 columns; unique on documentno
accidents = read.table(accidents.in.file.name, header = TRUE, sep = "|", na.strings = c("","NA"))

# rename variables
names(accidents)[names(accidents) == "MINE_ID"] = "mineid"
names(accidents)[names(accidents) == "ACCIDENT_DT"] = "accidentdate"
names(accidents)[names(accidents) == "ACCIDENT_TIME"] = "accidenttime"
names(accidents)[names(accidents) == "ACCIDENT_TYPE"] = "accidenttype"
names(accidents)[names(accidents) == "ACCIDENT_TYPE_CD"] = "accidenttypecode"
names(accidents)[names(accidents) == "ACTIVITY"] = "mineractivity"
names(accidents)[names(accidents) == "ACTIVITY_CD"] = "activitycode"
names(accidents)[names(accidents) == "CAL_QTR"] = "calendarquarter"
names(accidents)[names(accidents) == "CAL_YR"] = "calendaryear"
names(accidents)[names(accidents) == "CLASSIFICATION"] = "accidentclassification"
names(accidents)[names(accidents) == "CLASSIFICATION_CD"] = "classificationcode"
names(accidents)[names(accidents) == "CONTRACTOR_ID"] = "contractorid"
names(accidents)[names(accidents) == "CONTROLLER_ID"] = "controllerid"
names(accidents)[names(accidents) == "CONTROLLER_NAME"] = "controllername"
names(accidents)[names(accidents) == "DAYS_LOST"] = "dayslost"
names(accidents)[names(accidents) == "DAYS_RESTRICT"] = "daysrestrictedduty"
names(accidents)[names(accidents) == "DEGREE_INJURY"] = "degreeofinjury"
names(accidents)[names(accidents) == "DEGREE_INJURY_CD"] = "degreeofinjurycode"
names(accidents)[names(accidents) == "DOCUMENT_NO"] = "documentno"
names(accidents)[names(accidents) == "EQUIP_MFR_CD"] = "equipmanufacturercode"
names(accidents)[names(accidents) == "EQUIP_MFR_NAME"] = "equipmanufacturer"
names(accidents)[names(accidents) == "EQUIP_MODEL_NO"] = "equipmentmodelno"
names(accidents)[names(accidents) == "FISCAL_QTR"] = "fiscalquarter"
names(accidents)[names(accidents) == "FISCAL_YR"] = "fiscalyear"
names(accidents)[names(accidents) == "FIPS_STATE_CD"] = "fipsstatecode"
names(accidents)[names(accidents) == "IMMED_NOTIFY"] = "immediatenotificationclass"
names(accidents)[names(accidents) == "IMMED_NOTIFY_CD"] = "immediatenotificationcode"
names(accidents)[names(accidents) == "INJ_BODY_PART"] = "bodypart"
names(accidents)[names(accidents) == "INJ_BODY_PART_CD"] = "bodypartcode"
names(accidents)[names(accidents) == "INJURY_SOURCE"] = "sourceofinjury"
names(accidents)[names(accidents) == "INJURY_SOURCE_CD"] = "injurysourcecode"
names(accidents)[names(accidents) == "INVEST_BEGIN_DT"] = "investigationbegindate"
names(accidents)[names(accidents) == "JOB_EXPER"] = "jobexperience"
names(accidents)[names(accidents) == "MINE_EXPER"] = "mineexperience"
names(accidents)[names(accidents) == "MINING_EQUIP"] = "typeofequipment"
names(accidents)[names(accidents) == "MINING_EQUIP_CD"] = "equiptypecode"
names(accidents)[names(accidents) == "NARRATIVE"] = "narrative"
names(accidents)[names(accidents) == "NATURE_INJURY"] = "natureofinjury"
names(accidents)[names(accidents) == "NATURE_INJURY_CD"] = "natureofinjurycode"
names(accidents)[names(accidents) == "NO_INJURIES"] = "numberofinjuries"
names(accidents)[names(accidents) == "OCCUPATION"] = "occupation"
names(accidents)[names(accidents) == "OCCUPATION_CD"] = "occupcode3digit"
names(accidents)[names(accidents) == "OPERATOR_ID"] = "operatorid"
names(accidents)[names(accidents) == "OPERATOR_NAME"] = "operatorname"
names(accidents)[names(accidents) == "RETURN_TO_WORK_DT"] = "returntoworkdate"
names(accidents)[names(accidents) == "SCHEDULE_CHARGE"] = "schedulechargedays"
names(accidents)[names(accidents) == "SHIFT_BEGIN_TIME"] = "shiftbeginningtime"
names(accidents)[names(accidents) == "SUBUNIT"] = "subunit"
names(accidents)[names(accidents) == "SUBUNIT_CD"] = "subunitcode"
names(accidents)[names(accidents) == "TOT_EXPER"] = "totalexperience"
names(accidents)[names(accidents) == "TRANS_TERM"] = "transferredorterminated"
names(accidents)[names(accidents) == "UG_LOCATION"] = "uglocation"
names(accidents)[names(accidents) == "UG_LOCATION_CD"] = "uglocationcode"
names(accidents)[names(accidents) == "UG_MINING_METHOD"] = "ugminingmethod"
names(accidents)[names(accidents) == "UG_MINING_METHOD_CD"] = "ugminingmethodcode"

# format narrative
  # must remove encoded characters (otherwise tolower won't work)
accidents$narrative = iconv(accidents$narrative,"WINDOWS-1252","UTF-8")

# format variables
names(accidents) = tolower(names(accidents))

# format mineid
accidents$mineid = as.character(as.numeric(accidents$mineid))
accidents$mineid = str_pad(accidents$mineid, 7, pad = "0")

# format documentno
accidents$documentno = as.character(as.numeric(accidents$documentno))
accidents$documentno = str_pad(accidents$documentno, 12, pad = "0")

# drop data from environments not of interest 
  # 75672 rows; 57 columns; unique on documentno
accidents = accidents[accidents$coal_metal_ind == "C", ]
accidents = accidents[accidents$subunit == "UNDERGROUND", ]

# drop data after 2016 Q1
  # 75016 rows; 58 columns; unique on documentno
accidents$drop = ifelse((accidents$calendaryear == 2016 & accidents$calendarquarter > 1), 1, 0)
accidents = accidents[accidents$drop == 0,] 

# drop unnecessary variables
  # 75016 rows; 56 columns; unique on documentno
accidents$coal_metal_ind = NULL
accidents$drop = NULL

################################################################################

# OUPUT CLEAN ACCIDENTS DATA

# output clean accidents data
  # 75016 rows; 56 columns; unique on documentno
saveRDS(accidents, file = accidents.out.file.name)

################################################################################

rm(list = ls())

################################################################################
