# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Clean Inspections
  # Cleans inspections

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library()

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL) 
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # inspections data
inspections.in.file.name = paste0(originals.path, "/Inspections.txt", collapse = NULL)
  # mine types data (used to remove underground observations)
mine.types.in.file.name = paste0(clean.path, "/clean_mine_types.rds", collapse = NULL)

# outputs
  # cleaned inspections data
inspections.out.file.name = paste0(clean.path, "/clean_inspections.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(clean.path, recursive = TRUE)

################################################################################

# load data
  # 2193591 rows; 61 columns; unique on violation_no
inspections = read.table(inspections.in.file.name, header = T, sep = "|", na.strings = c("", "NA"))

# drop observations for environments not of interest
inspections = inspections[inspections$COAL_METAL_IND == "C", ] # now we have 327,777 obs

# rename variables
names(inspections)[names(inspections) == "EVENT_NO"] = "eventno"
names(inspections)[names(inspections) == "MINE_ID"] = "mineid"
names(inspections)[names(inspections) == "INSPECTION_BEGIN_DT"] = "beginningdate"
names(inspections)[names(inspections) == "INSPECTION_END_DT"] = "endingdate"
names(inspections)[names(inspections) == "CAL_YR"] = "calendaryear"
names(inspections)[names(inspections) == "CAL_QTR"] = "calendarquarter"
names(inspections)[names(inspections) == "FISCAL_YR"] = "fiscalyear"
names(inspections)[names(inspections) == "FISCAL_QTR"] = "fiscalquarter"
names(inspections)[names(inspections) == "INSPECT_OFFICE_CD"] = "inspectingofficecode"
names(inspections)[names(inspections) == "ACTIVITY_CODE"] = "inspactycode"
names(inspections)[names(inspections) == "ACTIVITY"] = "inspacty"
names(inspections)[names(inspections) == "ACTIVE_SECTIONS"] = "activesectionsinspected"
names(inspections)[names(inspections) == "IDLE_SECTIONS"] = "idlesectionsinspected"
names(inspections)[names(inspections) == "SHAFT_SLOPE_SINK"] = "shaftslopesinkingconstinspected"
names(inspections)[names(inspections) == "IMPOUND_CONSTR"] = "impoundconstinspected"
names(inspections)[names(inspections) == "BLDG_CONSTR_SITES"] = "buildingconstinspected"
names(inspections)[names(inspections) == "DRAGLINES"] = "draglineconstinspected"
names(inspections)[names(inspections) == "UNCLASSIFIED_CONSTR"] = "otherconstsitesinspected"
names(inspections)[names(inspections) == "CO_RECORDS"] = "companyrecords"
names(inspections)[names(inspections) == "SURF_UG_MINE"] = "surfaceareasugmines"
names(inspections)[names(inspections) == "SURF_FACILITY_MINE"] = "surfaceworkings"
names(inspections)[names(inspections) == "REFUSE_PILES"] = "refusepiles"
names(inspections)[names(inspections) == "EXPLOSIVE_STORAGE"] = "explosivestorage"
names(inspections)[names(inspections) == "OUTBY_AREAS"] = "outbyareas"
names(inspections)[names(inspections) == "MAJOR_CONSTR"] = "majorconstruction"
names(inspections)[names(inspections) == "SHAFTS_SLOPES"] = "shafts"
names(inspections)[names(inspections) == "MISC_AREA"] = "miscellaneous"
names(inspections)[names(inspections) == "SUM.SAMPLE_CNT_AIR."] = "airsamples"
names(inspections)[names(inspections) == "SUM.SAMPLE_CNT_DUSTSPOT."] = "spotdustsamples"
names(inspections)[names(inspections) == "SUM.SAMPLE_CNT_DUSTSURVEY."] = "surveydustsamples"
names(inspections)[names(inspections) == "SUM.SAMPLE_CNT_RESPDUST."] = "respdustsamples"
names(inspections)[names(inspections) == "SUM.SAMPLE_CNT_NOISE."] = "noisesamples"
names(inspections)[names(inspections) == "SUM.SAMPLE_CNT_OTHER."] = "othersamples"
names(inspections)[names(inspections) == "SUM.TOTAL_INSP_HOURS."] = "sumtotal_insp_hours"
names(inspections)[names(inspections) == "SUM.TOTAL_ON_SITE_HOURS."] = "sumtotal_on_site_hours"
names(inspections)[names(inspections) == "SUM.TOTAL_INSP_HRS_SPVR_TRAINEE."] = "sumtotal_insp_hrs_spvr_trainee"
names(inspections)[names(inspections) == "SUM.TOTAL_ON_SITE_HRS_SPVR_TRAINEE."] = "sumtotal_on_site_hrs_spvr_traine"
names(inspections)[names(inspections) == "CONTROLLER_ID"] = "controllerid"
names(inspections)[names(inspections) == "OPERATOR_ID"] = "operatorid"
names(inspections)[names(inspections) == "CONTROLLER_NAME"] = "controllername"
names(inspections)[names(inspections) == "OPERATOR_NAME"] = "operatorname"
names(inspections) = tolower(names(inspections))

# format variables
inspections$eventno = as.character(inspections$eventno)
inspections$beginningdate = as.character(inspections$beginningdate)
inspections$endingdate = as.character(inspections$endingdate)
inspections$mineid = str_pad(inspections$mineid, 7, pad = "0")
inspections$eventno = str_pad(inspections$eventno, 7, pad = "0")

# one obs is not unique on eventno - remove these two rows (with the same eventno)
#inspections[, "dup"] = duplicated(inspections$eventno)
#inspections = inspections[,-grep("dup", names(inspections))]
inspections = inspections[inspections$eventno != 4165469,]

################################################################################

# READ CLEAN MINE TYPE DATA, MERGE WITH INSPECTIONS DATA, THEN CLEAN RESULTING DATA

# read clean mine type data (1_clean_mines) - 86,362 obs, 3 vars
mine_types = readRDS(mine_types_file_name)

# merge open source inspections data with mine type data - 408,375 obs, 47 vars
inspections = merge(inspections, mine_types, by = c("mineid"), all = T)

# drop problematic merge observations
inspections = inspections[!is.na(inspections$eventno), ] # back to 327775 obs

# drop observations from environments not of interest
# facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16
inspections = inspections[inspections$minetype == "Underground", ] # now we have 195,719 obs

inspections$too_new = ifelse(inspections$calendaryear == 2016 & inspections$calendarquarter > 1, 1, 0)
inspections = inspections[inspections$too_new == 0,]
inspections = inspections[, c(-match("too_new", names(inspections)))] 

################################################################################

# output inspection-level data
saveRDS(inspections, file = inspections.out.file.name)

################################################################################

rm(list = ls())

################################################################################
