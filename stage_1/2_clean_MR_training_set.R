# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Clean MR (Maintenance and Repair) Training/Testing Set
  # Cleans master MR training/testing set from NIOSH
  # Adds extra MR accidents to training/testing set (all fatalities)

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com
# Last edit 1/13/17

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL)
cleaned.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # master MR training/testing set
    # coded by NIOSH representatives and sent to the Morantz team on 1/29/2016
training.set.in.file.name = paste0(originals.path, "/training-sets/Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", collapse = NULL)
  # extra MR accidents (all fatalities) 
    # collected by Sarah Levine following correspondence with John H. from NIOSH on 4/14/2016
fatalities.in.file.name = paste0(originals.path, "/coded_MR_fatalities.csv", collapse = NULL)

# outputs
  # clean MR training/testing set
training.set.out.file.name = paste0(cleaned.path, "/clean_MR_training_set.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read master MR training/testing set
  # 1000 rows; 111 columns; unique on documentno
mr.data = read.csv(training.set.in.file.name, header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE)

# read extra MR accidents 
  # 23 rows; 110 columns; unique on documentno
mr.fatalities = read.csv(fatalities.in.file.name, header = TRUE, sep = ",", nrows = 24, stringsAsFactors = FALSE)

# bye
rm(training.set.in.file.name, fatalities.in.file.name)

################################################################################

# CLEAN MASTER MR TRAINING/TESTING SET

# drop unnecessary variables
  # 1000 rows; 20 columns; unique on documentno
mr.data = mr.data[, c("accidentclassification.1", "accidentdate", "accidenttype.1", 
                      "accidenttypecode", "activitycode", "bodypartcode", 
                      "degreeofinjury.1", "documentno", "equipmanufacturer", 
                      "equiptypecode", "immediatenotificationclass", "injurysourcecode", 
                      "M.R.", "mineid", "mineractivity.1", 
                      "narrativemodified.1", "natureofinjury.1", "occupation", 
                      "occupcode3digit", "sourceofinjury")]

# rename variables
names(mr.data)[names(mr.data) == "narrativemodified.1"] = "narrative"
names(mr.data)[names(mr.data) == "degreeofinjury.1"] = "degreeofinjury"
names(mr.data)[names(mr.data) == "accidentclassification.1"] = "accidentclassification"
names(mr.data)[names(mr.data) == "accidenttype.1"] = "accidenttype"
names(mr.data)[names(mr.data) == "natureofinjury.1"] = "natureofinjury"
names(mr.data)[names(mr.data) == "mineractivity.1"] = "mineractivity"
mr.data$MR = as.factor(mr.data$M.R.)
mr.data$M.R. = NULL

# format variables
mr.data$accidentclassification = tolower(mr.data$accidentclassification)
mr.data$accidenttype = tolower(mr.data$accidenttype)
mr.data$degreeofinjury = tolower(mr.data$degreeofinjury)
mr.data$equipmanufacturer = tolower(mr.data$equipmanufacturer)
mr.data$immediatenotificationclass = tolower(mr.data$immediatenotificationclass)
mr.data$mineractivity = tolower(mr.data$mineractivity)
mr.data$narrative = iconv(mr.data$narrative,"WINDOWS-1252","UTF-8") # remove encoded characters
mr.data$narrative = tolower(mr.data$narrative)
mr.data$natureofinjury = tolower(mr.data$natureofinjury)
mr.data$occupation = tolower(mr.data$occupation)
mr.data$sourceofinjury = tolower(mr.data$sourceofinjury)

################################################################################

# CLEAN EXTRA MR ACCIDENTS

# drop unnecessary variables
  # 23 rows; 20 columns; unique on documentno
mr.fatalities = mr.fatalities[, c("accidentclassification", "accidentdate", "accidenttype", 
                                  "accidenttypecode", "activitycode", "bodypartcode", 
                                  "degreeofinjury", "documentno", "equipmanufacturer", 
                                  "equiptypecode", "immediatenotificationclass", "injurysourcecode", 
                                  "mineid", "mineractivity", "MR_fatality",
                                  "narrative", "natureofinjury", "occupation", 
                                  "occupcode3digit", "sourceofinjury")]

# rename variables
mr.fatalities$MR = as.factor(mr.fatalities$MR_fatality)
mr.fatalities$MR_fatality = NULL

# these four fatalgrams are considered MR and were included in a study by John H. from NIOSH as MR
  # however, it's only evident from the fatalgrams that these were sustained during larger group MR activities
  # nothing from the narrative field/occupation indicates that MR was the activity at the time
  # training on these observations will not help; drop them
mr.fatalities = mr.fatalities[!(mr.fatalities$documentno == "220030290001") & 
                                !(mr.fatalities$documentno == "220030290002") &
                                !(mr.fatalities$documentno == "220030290003") & 
                                !(mr.fatalities$documentno == "220030130149"), ]

################################################################################

# COMBINE MASTER MR TRAINING/TESTING SET AND EXTRA MR ACCIDENTS

# combine master MR training/testing set and extra MR accidents
  # 1019 rows; 20 columns
mr.data = rbind(mr.data, mr.fatalities) 

# drop duplicated observations
  # 1018 rows; 20 columns; unique on documentno
mr.data = mr.data[!duplicated(mr.data$documentno), ]

# format variables
mr.data$MR = factor(ifelse(mr.data$MR == 1, "YES", "NO"))
mr.data$documentno = as.character(mr.data$documentno)

# bye
rm(mr.fatalities)

################################################################################

# RECODE ACCIDENTS

# Based on J. Heberger's email 5/2/2016
  # "even though mine worker activity is MR, installing roof bolts is not considered MR"
mr.data[mr.data$documentno == "219932950056", "MR"] = "NO"

################################################################################

# OUTPUT CLEAN MR TRAINING/TESTING SET

# output clean MR training/testing set
  # 1018 rows; 20 columns; unique on documentno
saveRDS(mr.data, file = training.set.out.file.name)

# bye
rm(list = ls())

################################################################################