# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Clean MR (Maintenance and Repair) Training Set
  # Train/Test:
    # Inputs master MR dataset
    # Conducts narrative analysis on description of accidents, generates flags for key words and phrases, 
    # and groups existing categorical variables based on utility in classifying accidents
    # Trains and tests various classification algorithms to classify accidents (PS/non-PS)
    # Results printed to screen

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

library(stringr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "/Users/Sarah/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.input.path = paste0(root, "/0_originals", collapse = NULL)
cleaned.output.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # master MR dataset
    # coded by NIOSH representatives and sent to the Morantz team on 1/29/2016
training.set.in.file.name = paste0(originals.input.path, "/training-sets/Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", collapse = NULL)
  # extra MR accidents (all fatalities) 
    # collected by Sarah Levine following correspondence with John H. from NIOSH on 4/14/2016
fatalities.in.file.name = paste0(originals.input.path, "/coded_MR_fatalities.csv", collapse = NULL)

# outputs
  # clean MR training set
training.set.out.file.name = paste0(cleaned.output.path, "/clean_MR_training_set.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# read master MR dataset
  # 1000 rows; 111 columns; unique on documentno
mr.data = read.csv(training.set.in.file.name, header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE)

# read extra MR injuries
  # 23 rows; 110 columns; unique on documentno
mr.fatalities = read.csv(fatalities.in.file.name, header = TRUE, sep = ",", nrows = 24, stringsAsFactors = FALSE)

# bye
rm(root, originals.input.path, cleaned.output.path, training.set.in.file.name, fatalities.in.file.name)

################################################################################

# CLEAN DATASET

# drop unnecessary variables
drop = c("narrativemodified", "degreeofinjury", "accidentclassification", 
         "accidenttype", "natureofinjury", "mineractivity")
mr.data = mr.data[, !(names(mr.data) %in% drop)]

# rename variables
names(mr.data)[names(mr.data) == "narrativemodified.1"] = "narrative"
names(mr.data)[names(mr.data) == "degreeofinjury.1"] = "degreeofinjury"
names(mr.data)[names(mr.data) == "accidentclassification.1"] = "accidentclassification"
names(mr.data)[names(mr.data) == "accidenttype.1"] = "accidenttype"
names(mr.data)[names(mr.data) == "natureofinjury.1"] = "natureofinjury"
names(mr.data)[names(mr.data) == "mineractivity.1"] = "mineractivity"

# format narrative field
  # must remove encoded characters (otherwise tolower won't work)
mr.data$narrative = iconv(mr.data$narrative,"WINDOWS-1252","UTF-8")

# format variables
mr.data$MR = as.factor(mr.data$M.R.)
mr.data$M.R. = NULL
mr.data$narrative = tolower(mr.data$narrative)
mr.data$degreeofinjury = tolower(mr.data$degreeofinjury)
mr.data$accidentclassification = tolower(mr.data$accidentclassification)
mr.data$accidenttype = tolower(mr.data$accidenttype)
mr.data$natureofinjury = tolower(mr.data$natureofinjury)
mr.data$mineractivity = tolower(mr.data$mineractivity)
mr.data$occupation = tolower(mr.data$occupation)
mr.data$typeofequipment = tolower(mr.data$typeofequipment)
mr.data$sourceofinjury = tolower(mr.data$sourceofinjury)
mr.data$bodypart = tolower(mr.data$bodypart)
mr.data$equipmanufacturer = tolower(mr.data$equipmanufacturer)
mr.data$immediatenotificationclass = tolower(mr.data$immediatenotificationclass)
mr.data$uglocation = tolower(mr.data$uglocation)

# generate death variable
mr.data$death = ifelse(grepl("fatality", mr.data$degreeofinjury), 1, 0)

# bye
rm(drop)

################################################################################

# CLEAN EXTRA FATAL MR ACCIDENTS

# rename variable
mr.fatalities$MR = as.factor(mr.fatalities$MR_fatality)

# drop unnecessary variables
drop = c("MR_fatality", "v56", "v57", "v58", "v59")
mr.fatalities = mr.fatalities[, !(names(mr.fatalities) %in% drop)]

# these four fatalgrams are considered MR and were included in a study by John H. from NIOSH as MR
# however, it's only evident from the fatalgrams that these were sustained during larger group MR activities
# nothing from the narrative field/occupation indicates that MR was the activity at the time
# training on these observations will not help; drop them
mr.fatalities = mr.fatalities[!(mr.fatalities$documentno == "220030290001") & 
                                !(mr.fatalities$documentno == "220030290002") &
                                !(mr.fatalities$documentno == "220030290003") & 
                                !(mr.fatalities$documentno == "220030130149"), ]

# bye
rm(drop)

################################################################################

# COMBINE MR MASTER DATASET AND EXTRA FATAL MR ACCIDENTS

# merge datasts
  # 1019 rows; 106 columns; unique on documentno
mr.data = rbind(mr.data, mr.fatalities) 

# drop redundant observations (in terms of documentno)
  # 1018 rows; 106 columns; unique on documentno
mr.data = mr.data[!duplicated(mr.data$documentno), ]

# format variable
mr.data$MR = factor(ifelse(mr.data$MR == 1, "YES", "NO"))

# mark data as training data
mr.data$type = "classified" 

# bye
rm(mr.fatalities)

################################################################################

# RECODE ACCIDENTS

# Based on J. Heberger's email 5/2/2016
# "even though mine worker activity is MR, installing roof bolts is not considered MR"

mr.data[mr.data$documentno == "219932950056", "MR"] = "NO"

################################################################################

# output clean training set
  # 1018 rows; 107 columns; unique on documentno
saveRDS(mr.data, file = training.set.out.file.name)

################################################################################

rm(list = ls())

################################################################################