# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Clean MR (Maintenance and Repair) Training/Testing Set
  # Cleans master MR dataset coded by NIOSH
  # Cleans and adds extra MR accidents to master MR dataset

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 1/23/2017

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL)
cleaned.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # master MR dataset
    # coded by NIOSH representatives and sent to the Morantz team on 1/29/2016
master.set.in.file.name = paste0(originals.path, "/training-sets/Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", collapse = NULL)
  # extra MR accidents (all fatalities) 
    # collected by Sarah Levine following correspondence with John H. from NIOSH on 4/14/2016
extra.fatalities.in.file.name = paste0(originals.path, "/coded_MR_fatalities.csv", collapse = NULL)

# outputs
  # cleaned MR training/testing set
train.test.set.out.file.name = paste0(cleaned.path, "/clean_MR_train_test_set.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read master MR dataset
  # 1000 rows; 111 columns; unique on documentno
master.data = read.csv(master.set.in.file.name, header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE)

# read extra MR accidents 
  # 23 rows; 110 columns; unique on documentno
extra.fatalities = read.csv(extra.fatalities.in.file.name, header = TRUE, sep = ",", nrows = 24, stringsAsFactors = FALSE)

# bye
rm(master.set.in.file.name, extra.fatalities.in.file.name)

################################################################################

# CLEAN MASTER MR DATASET

# drop unnecessary variables
  # 1000 rows; 18 columns; unique on documentno
master.data = master.data[, c("accidentclassification.1", "accidentdate", "accidenttype.1", 
                              "accidenttypecode", "activitycode", "bodypartcode", 
                              "degreeofinjury.1", "documentno", "equiptypecode", 
                              "injurysourcecode", "M.R.", "mineid", "mineractivity.1", 
                              "narrativemodified.1", "natureofinjury.1", "occupation", 
                              "occupcode3digit", "sourceofinjury")]

# rename variables
names(master.data)[names(master.data) == "narrativemodified.1"] = "narrative"
names(master.data)[names(master.data) == "degreeofinjury.1"] = "degreeofinjury"
names(master.data)[names(master.data) == "accidentclassification.1"] = "accidentclassification"
names(master.data)[names(master.data) == "accidenttype.1"] = "accidenttype"
names(master.data)[names(master.data) == "natureofinjury.1"] = "natureofinjury"
names(master.data)[names(master.data) == "mineractivity.1"] = "mineractivity"
master.data$MR = as.factor(master.data$M.R.)
master.data$M.R. = NULL

# format variables
master.data$accidentclassification = tolower(master.data$accidentclassification)
master.data$accidenttype = tolower(master.data$accidenttype)
master.data$degreeofinjury = tolower(master.data$degreeofinjury)
master.data$mineractivity = tolower(master.data$mineractivity)
master.data$narrative = iconv(master.data$narrative,"WINDOWS-1252","UTF-8") # remove encoded characters
master.data$narrative = tolower(master.data$narrative)
master.data$natureofinjury = tolower(master.data$natureofinjury)
master.data$occupation = tolower(master.data$occupation)
master.data$sourceofinjury = tolower(master.data$sourceofinjury)

################################################################################

# CLEAN EXTRA MR ACCIDENTS

# drop unnecessary variables
  # 23 rows; 18 columns; unique on documentno
extra.fatalities = extra.fatalities[, c("accidentclassification", "accidentdate", "accidenttype", 
                                        "accidenttypecode", "activitycode", "bodypartcode", 
                                        "degreeofinjury", "documentno", "equiptypecode", 
                                        "injurysourcecode", "mineid", "mineractivity", 
                                        "MR_fatality", "narrative", "natureofinjury", 
                                        "occupation", "occupcode3digit", "sourceofinjury")]

# rename variables
extra.fatalities$MR = as.factor(extra.fatalities$MR_fatality)
extra.fatalities$MR_fatality = NULL

# these four fatalgrams are considered MR and were included in a study by John H. from NIOSH as MR
  # however, it's only evident from the fatalgrams that these were sustained during larger group MR activities
  # nothing from the narrative field/occupation indicates that MR was the activity at the time
  # training on these observations will not help; drop them
    # 9 rows; 18 columns; unique on documentno
extra.fatalities = extra.fatalities[!(extra.fatalities$documentno == "220030290001") & 
                                      !(extra.fatalities$documentno == "220030290002") &
                                      !(extra.fatalities$documentno == "220030290003") & 
                                      !(extra.fatalities$documentno == "220030130149"), ]

################################################################################

# COMBINE MASTER MR DATASET AND EXTRA MR ACCIDENTS

# combine master MR dataset and extra MR accidents
  # 1019 rows; 18 columns; unique on documentno after dropping duplicates
train.test.data = rbind(master.data, extra.fatalities) 

# drop duplicated observations
  # 1018 rows; 18 columns; unique on documentno
train.test.data = train.test.data[!duplicated(train.test.data$documentno), ]

# format variables
train.test.data$MR = factor(ifelse(train.test.data$MR == 1, "YES", "NO"))
train.test.data$documentno = as.character(train.test.data$documentno)

# bye
rm(master.data, extra.fatalities)

################################################################################

# RECODE MISCLASSIFIED ACCIDENTS

# based on J. Heberger's email 5/2/2016
  # "even though mine worker activity is MR, installing roof bolts is not considered MR"
train.test.data[train.test.data$documentno == "219932950056", "MR"] = "NO"

################################################################################

# CLEAN COMBINED MR TRAINING/TESTING DATA

# some narrative fields are polluted with other columns; split and replace these  
train.test.data$messy = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", train.test.data$narrative), 1, 0)
narrative.split = strsplit(train.test.data[train.test.data$messy == 1, "narrative"], "|", fixed = TRUE)
messy.rows = row.names(train.test.data[train.test.data$messy == 1, ])
for (i in 1:length(messy.rows)) {
  train.test.data[messy.rows[i], "narrative"] = unlist(narrative.split[i])[1]
  train.test.data[messy.rows[i], "occupcode3digit"] = unlist(narrative.split[i])[2]
  train.test.data[messy.rows[i], "occupation"] = unlist(narrative.split[i])[3]
}

# bye
train.test.data$messy = NULL
rm(i, messy.rows, narrative.split)

################################################################################

# OUTPUT DATA

# output MR training/testing set
  # 1018 rows; 18 columns; unique on documentno
saveRDS(train.test.data, file = train.test.set.out.file.name)

# bye
rm(list = ls())

################################################################################
