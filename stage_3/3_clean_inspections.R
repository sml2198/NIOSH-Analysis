# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Clean Inspections
  # Cleans inspections

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library(stringr)

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
  # 790234 rows; 45 columns; unique on event_no
inspections = read.table(inspections.in.file.name, header = T, sep = "|", na.strings = c("", "NA"))

# rename variables
names(inspections)[names(inspections) == "EVENT_NO"] = "eventno"
names(inspections)[names(inspections) == "MINE_ID"] = "mineid"
names(inspections)[names(inspections) == "INSPECTION_BEGIN_DT"] = "beginningdate"
names(inspections)[names(inspections) == "INSPECTION_END_DT"] = "endingdate"
names(inspections)[names(inspections) == "CAL_YR"] = "year"
names(inspections)[names(inspections) == "CAL_QTR"] = "quarter"
names(inspections)[names(inspections) == "ACTIVITY_CODE"] = "inspactycode"
names(inspections)[names(inspections) == "ACTIVITY"] = "inspacty"
names(inspections)[names(inspections) == "SUM.TOTAL_INSP_HOURS."] = "inspectionhours"
names(inspections)[names(inspections) == "SUM.TOTAL_ON_SITE_HOURS."] = "onsitehours"
names(inspections)[names(inspections) == "OPERATOR_ID"] = "operatorid"
names(inspections) = tolower(names(inspections))

# format variables
inspections$eventno = as.character(inspections$eventno)
inspections$beginningdate = as.character(inspections$beginningdate)
inspections$endingdate = as.character(inspections$endingdate)
inspections$mineid = str_pad(inspections$mineid, 7, pad = "0")
inspections$eventno = str_pad(inspections$eventno, 7, pad = "0")

# one obs is not unique on eventno - remove these two rows (with the same eventno)
  # 790232 rows; 45 columns; unique on eventno
inspections = inspections[inspections$eventno != 4165469,]

################################################################################

# REMOVE DATA FROM ENVIRONMENTS NOT OF INTEREST

# drop observations for environments not of interest
  # 327775 rows; 45 columns; unique on eventno
inspections = inspections[inspections$coal_metal_ind == "C", ]

# read mine types data (assessments data does NOT contain "minetype" field so we need
# to merge on mine type information by mineid)
  # 86362 rows; 3 columns; unique on eventno
mine.types = readRDS(mine.types.in.file.name)

# merge inspections with mine types & drop non-merged observations 
  # 327775 rows; 47 columns; unique on eventno
inspections = merge(inspections, mine.types, by = c("mineid"), all = T)
inspections = inspections[!is.na(inspections$eventno), ]
rm(mine.types)

# drop observations from environments not of interest
  # 195732 rows; 47 columns; unique on eventno
inspections = inspections[inspections$minetype == "Underground", ]

# drop observations from time periods not of interest
  # 192652 rows; 48 columns; unique on eventno
inspections$too_new = ifelse(inspections$year == 2016 & inspections$quarter > 1, 1, 0)
inspections = inspections[inspections$too_new == 0,]

# remove observations missing eventno
  # 192141 rows; 48 columns; unique on eventno
inspections = inspections[complete.cases(inspections$eventno),]

################################################################################

# keep only useful variables
  # 192141 rows; 10 columns; unique on eventno
keep = c("mineid", "eventno", "beginningdate",
         "endingdate", "year", "quarter", 
         "inspactycode", "inspectionhours", "onsitehours",
         "operatorid")
inspections = inspections[, (names(inspections) %in% keep)] 

################################################################################

# create an indicator for inspections, so we can sum inspections per year
  # in the prepare_stage_3
inspections$numinspections = 1

# While in theory inspections are quarterly, they do not always happen once per quarter or even 
# four times per year, and some inspections might even last a whole year. As a check, we not only 
# count the total number of inspections per year, but we also confirm that there are no inspections 
# that begin in one calendar year and end in another calendar year. Fortunately, there are none.
# See lines 133 and 134.

# create and format endyear variable
datevars = names(inspections)[grep("date", names(inspections))]
for (i in 1:length(datevars)) {
  inspections[, datevars[i]] = as.Date(as.character(inspections[, datevars[i]]), "%m/%d/%Y")
}
inspections$endyear = as.numeric(format(inspections$endingdate, "%Y"))

# count number of inspections lasting more than one calendar year - all zero, good!
# sum((inspections$endyear - inspections$year) != 0)

################################################################################

# output inspection-level data
  # 192141 rows; 12 columns; unique on eventno
saveRDS(inspections, file = inspections.out.file.name)

################################################################################

rm(list = ls())

################################################################################
