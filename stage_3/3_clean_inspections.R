# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Clean Inspections
  # Cleans inspections data from the MSHA open data portal

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/8/2017

################################################################################

library(stringr)
library(zoo)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
originals.path = paste0(root, "/data/0_originals", collapse = NULL) 
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 

# inputs
  # inspections data from the MSHA open data portal
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
inspections.in.file.name = paste0(originals.path, "/Inspections.txt", collapse = NULL)
  # mine type data
    # produced in 1_clean_mines
mine.types.in.file.name = paste0(cleaned.path, "/clean_mine_types.rds", collapse = NULL)

# outputs
  # cleaned inspections data
inspections.out.file.name = paste0(cleaned.path, "/clean_inspections.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read inspections data
  # 790234 rows; 45 columns
inspections = read.table(inspections.in.file.name, header = TRUE, sep = "|", na.strings = c("", "NA"))

# read cleaned mine types data
  # 86362 rows; 3 columns; unique on mineid
mine.types = readRDS(mine.types.in.file.name)

# bye
rm(inspections.in.file.name, mine.types.in.file.name)

################################################################################

# EDIT DATA

# drop duplicated data
  # 790232 rows; 45 columns; unique on eventno
inspections = inspections[inspections$EVENT_NO != 4165469, ]

# drop unnecessary variables
  # 790232 rows; 8 columns; unique on eventno
inspections = inspections[, c("CAL_QTR", "CAL_YR", "COAL_METAL_IND", 
                              "EVENT_NO", "INSPECTION_BEGIN_DT", "INSPECTION_END_DT", 
                              "MINE_ID", "SUM.TOTAL_INSP_HOURS.")]

# rename variables
names(inspections)[names(inspections) == "CAL_QTR"] = "quarter"
names(inspections)[names(inspections) == "CAL_YR"] = "year"
names(inspections)[names(inspections) == "COAL_METAL_IND"] = "coal_metal_ind"
names(inspections)[names(inspections) == "EVENT_NO"] = "eventno"
names(inspections)[names(inspections) == "INSPECTION_BEGIN_DT"] = "beginningdate"
names(inspections)[names(inspections) == "INSPECTION_END_DT"] = "endingdate"
names(inspections)[names(inspections) == "MINE_ID"] = "mineid"
names(inspections)[names(inspections) == "SUM.TOTAL_INSP_HOURS."] = "inspectionhours"

# format variables
inspections$mineid = str_pad(inspections$mineid, 7, pad = "0")
inspections$eventno = str_pad(inspections$eventno, 7, pad = "0")

# drop observations from environments not of interest
  # 327775 rows; 8 columns; unique on eventno
inspections = inspections[inspections$coal_metal_ind == "C", ]

# bye
  # 327775 rows; 7 columns; unique on eventno
inspections$coal_metal_ind = NULL

################################################################################

# MERGE INSPECTIONS AND MINE TYPE
  # inspections data does not include information about mine type, which we need
    # to drop data from environments not of interest

# merge inspections and mine type data
  # 408375 rows; 9 columns
inspections = merge(inspections, mine.types, by = c("mineid"), all = TRUE)

# drop non-merging observations
  # 327775 rows; 9 columns; unique on eventno
inspections = inspections[!is.na(inspections$eventno), ]

# bye
rm(mine.types)

################################################################################

# EDIT DATA

# drop observations from environments not of interest
  # 195732 rows; 9 columns; unique on eventno
inspections = inspections[inspections$minetype == "Underground", ]

# drop observations from time periods not of interest
  # 192652 rows; 9 columns; unique on eventno
inspections$too.new = ifelse(inspections$year == 2016 & inspections$quarter > 1, 1, 0)
inspections = inspections[inspections$too.new == 0,]
inspections$too.new = NULL

# drop observations missing eventno
  # 192141 rows; 9 columns; unique on eventno
inspections = inspections[complete.cases(inspections$eventno), ]

# drop unnecessary variables
  # 192141 rows; 7 columns; unique on eventno
inspections$coalcormetalmmine = 
  inspections$minetype = NULL

# create an indicator for inspections
inspections$numinspections = 1

# check that there are no inspections that begin in one year and end in another year
#datevars = names(inspections)[grep("date", names(inspections))]
#for (i in 1:length(datevars)) {
#  inspections[, datevars[i]] = as.Date(as.character(inspections[, datevars[i]]), "%m/%d/%Y")
#}
#inspections$endyear = as.numeric(format(inspections$endingdate, "%Y"))
#sum((inspections$endyear - inspections$year) != 0) # 0
#rm(datevars, i)

################################################################################

# OUTPUT DATA

# output cleaned inspections data
  # 192141 rows; 8 columns; unique on eventno
saveRDS(inspections, file = inspections.out.file.name)

# bye
rm(list = ls())

################################################################################
