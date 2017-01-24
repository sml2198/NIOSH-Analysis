# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 1 - Clean Accidents Data
  # Cleans accidents data from the MSHA open data portal

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 1/23/2017

################################################################################

library(stringr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL)
cleaned.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # accidents data
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
accidents.in.file.name = paste0(originals.path, "/Accidents.txt", collapse = NULL)

# outputs
  # cleaned accidents data
accidents.out.file.name = paste0(cleaned.path, "/clean_accidents.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA 

# read accidents data
  # 212611 rows; 57 columns; unique on DOCUMENT_NO
accidents = read.table(accidents.in.file.name, header = TRUE, sep = "|", na.strings = c("", "NA"))

# bye
rm(accidents.in.file.name)

################################################################################

# CLEAN DATA 

# drop unnecessary variables
  # 212611 rows; 21 columns; unique on DOCUMENT_NO
accidents = accidents[, c("ACCIDENT_DT", "ACCIDENT_TYPE", "ACCIDENT_TYPE_CD", 
                          "ACTIVITY", "ACTIVITY_CD", "CAL_QTR", 
                          "CAL_YR", "CLASSIFICATION", "COAL_METAL_IND", 
                          "DEGREE_INJURY", "DOCUMENT_NO", "INJ_BODY_PART_CD", 
                          "INJURY_SOURCE", "INJURY_SOURCE_CD", "MINE_ID", 
                          "MINING_EQUIP_CD", "NARRATIVE", "NATURE_INJURY", 
                          "OCCUPATION", "OCCUPATION_CD", "SUBUNIT")]

# rename variables
names(accidents)[names(accidents) == "ACCIDENT_DT"] = "accidentdate"
names(accidents)[names(accidents) == "ACCIDENT_TYPE"] = "accidenttype"
names(accidents)[names(accidents) == "ACCIDENT_TYPE_CD"] = "accidenttypecode"
names(accidents)[names(accidents) == "ACTIVITY"] = "mineractivity"
names(accidents)[names(accidents) == "ACTIVITY_CD"] = "activitycode"
names(accidents)[names(accidents) == "CAL_QTR"] = "calendarquarter"
names(accidents)[names(accidents) == "CAL_YR"] = "calendaryear"
names(accidents)[names(accidents) == "CLASSIFICATION"] = "accidentclassification"
names(accidents)[names(accidents) == "COAL_METAL_IND"] = "coalmetalind"
names(accidents)[names(accidents) == "DEGREE_INJURY"] = "degreeofinjury"
names(accidents)[names(accidents) == "DOCUMENT_NO"] = "documentno"
names(accidents)[names(accidents) == "INJ_BODY_PART_CD"] = "bodypartcode"
names(accidents)[names(accidents) == "INJURY_SOURCE"] = "sourceofinjury"
names(accidents)[names(accidents) == "INJURY_SOURCE_CD"] = "injurysourcecode"
names(accidents)[names(accidents) == "MINE_ID"] = "mineid"
names(accidents)[names(accidents) == "MINING_EQUIP_CD"] = "equiptypecode"
names(accidents)[names(accidents) == "NARRATIVE"] = "narrative"
names(accidents)[names(accidents) == "NATURE_INJURY"] = "natureofinjury"
names(accidents)[names(accidents) == "OCCUPATION"] = "occupation"
names(accidents)[names(accidents) == "OCCUPATION_CD"] = "occupcode3digit"
names(accidents)[names(accidents) == "SUBUNIT"] = "subunit"

# format variables
accidents$documentno = str_pad(accidents$documentno, 12, pad = "0")
accidents$mineid = str_pad(accidents$mineid, 7, pad = "0")
accidents$accidentclassification = tolower(accidents$accidentclassification)
accidents$accidenttype = tolower(accidents$accidenttype)
accidents$degreeofinjury = tolower(accidents$degreeofinjury)
accidents$mineractivity = tolower(accidents$mineractivity)
accidents$narrative = iconv(accidents$narrative,"WINDOWS-1252","UTF-8") # remove encoded characters
accidents$narrative = tolower(accidents$narrative)
accidents$natureofinjury = tolower(accidents$natureofinjury)
accidents$occupation = tolower(accidents$occupation)
accidents$sourceofinjury = tolower(accidents$sourceofinjury)

# drop data from environments not of interest 
  # 75672 rows; 21 columns; unique on documentno
accidents = accidents[which(accidents$coalmetalind == "C" & 
                              accidents$subunit == "UNDERGROUND"), ]

# drop data after 2016 Q1
  # 75016 rows; 21 columns; unique on documentno
accidents = accidents[which(!(accidents$calendaryear == 2016 & 
                                 accidents$calendarquarter > 1)), ]

# drop unnecessary variables
  # 75016 rows; 17 columns; unique on documentno
accidents$calendarquarter = 
  accidents$calendaryear =
  accidents$coalmetalind = 
  accidents$subunit = NULL

################################################################################

# OUPUT DATA

# output cleaned accidents data
  # 75016 rows; 17 columns; unique on documentno
saveRDS(accidents, file = accidents.out.file.name)

# bye
rm(list = ls())

################################################################################
