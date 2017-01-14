# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Clean PS (Pinning and Striking) Training Set 
  # Train/Test:
    # Inputs master PS dataset
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
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.input.path = paste0(root, "/0_originals", collapse = NULL)
cleaned.output.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # master PS dataset
    # coded by NIOSH representatives and sent to the Morantz team on 1/29/2016
training.set.in.file.name = paste0(originals.input.path, "/training-sets/Training_Set_Pinning_And_Striking_Accidents-January-29-2016.csv", collapse = NULL)

# outputs
  # clean PS training set
training.set.out.file.name = paste0(cleaned.output.path, "/clean_PS_training_set.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# read master PS dataset
  # 1002 rows; 104 columns; unique on documentno 
ps.data = read.csv(training.set.in.file.name)

rm(root, originals.input.path, cleaned.output.path, training.set.in.file.name)

################################################################################

# CLEAN MASTER DATASET

# drop data without mineid 
  # 1000 rows; 104 columns; unique on documentno
ps.data = ps.data[!is.na(ps.data$mineid), ]

# rename variables
names(ps.data)[names(ps.data) == "narrativemodified"] = "narrative"

# format variables
ps.data$narrative = as.character(ps.data$narrative)
ps.data$occupcode3digit = as.character(ps.data$occupcode3digit)
ps.data$occupation = as.character(ps.data$occupation)
ps.data$returntoworkdate = as.character(ps.data$returntoworkdate)

# make variables lowercase
ps.data$narrative = tolower(ps.data$narrative)
ps.data$mineractivity = tolower(ps.data$mineractivity)
ps.data$natureofinjury = tolower(ps.data$natureofinjury)
ps.data$degreeofinjury = tolower(ps.data$degreeofinjury)
ps.data$sourceofinjury = tolower(ps.data$sourceofinjury)
ps.data$accidenttype = tolower(ps.data$accidenttype)
ps.data$accidentclassification = tolower(ps.data$accidentclassification)
ps.data$bodypart = tolower(ps.data$bodypart)
ps.data$typeofequipment = tolower(ps.data$typeofequipment)
ps.data$occupation = tolower(ps.data$occupation)

# format PS indicator
ps.data$X = factor(ifelse(ps.data$X == 1, "YES", "NO"))
names(ps.data)[names(ps.data) == "X"] = "PS"

# some narrative fields are polluted with other columns; split and replace these  
ps.data$messy = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", ps.data$narrative), 1, 0)
narrative.split = strsplit(ps.data[ps.data$messy == 1, "narrative"], "|", fixed = T) # 23
messy.rows = row.names(ps.data[ps.data$messy == 1, ])
for (i in 1:length(messy.rows)) {
  ps.data[messy.rows[i], "narrative"] = unlist(narrative.split[i])[1]
  ps.data[messy.rows[i], "occupcode3digit"] = unlist(narrative.split[i])[2]
  ps.data[messy.rows[i], "occupation"] = unlist(narrative.split[i])[3]
  ps.data[messy.rows[i], "returntoworkdate"] = unlist(narrative.split[i])[4]
}
ps.data$messy = NULL

# mark data as training data
ps.data$type = "classified" 

# bye
rm (i, messy.rows, narrative.split)

################################################################################


# RECODE MISCLASSIFIED ACCIDENTS

# recoded in light of Miguel's 5/27/2016 response to our questions
ps.data[ps.data$documentno == "219891280164", "PS"] = "YES"
ps.data[ps.data$documentno == "219852170075", "PS"] = "YES"
ps.data[ps.data$documentno == "219901620109", "PS"] = "YES"
ps.data[ps.data$documentno == "220011070020", "PS"] = "NO"
ps.data[ps.data$documentno == "219892570061", "PS"] = "NO"
ps.data[ps.data$documentno == "219893100251", "PS"] = "NO"
ps.data[ps.data$documentno == "219872990054", "PS"] = "NO"
ps.data[ps.data$documentno == "219983280016", "PS"] = "NO"
ps.data[ps.data$documentno == "220082800043", "PS"] = "NO"
ps.data[ps.data$documentno == "219830320021", "PS"] = "NO"
ps.data[ps.data$documentno == "219912970040", "PS"] = "NO"
ps.data[ps.data$documentno == "219942900032", "PS"] = "NO"
ps.data[ps.data$documentno == "219982380025", "PS"] = "NO"

# recoded in light of Miguel's 6/7/2016 response to our questions
ps.data[ps.data$documentno == "219912970040", "PS"] = "YES"
ps.data[ps.data$documentno == "219871460076", "PS"] = "NO"
ps.data[ps.data$documentno == "219861280065", "PS"] = "NO"
ps.data[ps.data$documentno == "220000310115", "PS"] = "NO"
ps.data[ps.data$documentno == "220001180052", "PS"] = "NO"
ps.data[ps.data$documentno == "219831430047", "PS"] = "NO"
ps.data[ps.data$documentno == "219943180016", "PS"] = "NO"
ps.data[ps.data$documentno == "220112090013", "PS"] = "NO"

# recoded on 9/13/2016 in light of Miguel's lack of response to our questions
ps.data[ps.data$documentno == "220090630033", "PS"] = "YES"
ps.data[ps.data$documentno == "220050800006", "PS"] = "YES"
ps.data[ps.data$documentno == "219892210062", "PS"] = "YES"
ps.data[ps.data$documentno == "219950870035", "PS"] = "YES"
ps.data[ps.data$documentno == "219972890025", "PS"] = "YES"
ps.data[ps.data$documentno == "219930390025", "PS"] = "NO"
ps.data[ps.data$documentno == "219992320012", "PS"] = "NO"
ps.data[ps.data$documentno == "219853190080", "PS"] = "NO"
ps.data[ps.data$documentno == "219973490121", "PS"] = "NO"
ps.data[ps.data$documentno == "219852050003", "PS"] = "NO"
ps.data[ps.data$documentno == "219891140147", "PS"] = "NO"
ps.data[ps.data$documentno == "220020100051", "PS"] = "NO"

################################################################################

# output clean training set
  # 1000 rows; 105 columns; unique on documentno
saveRDS(ps.data, file = training.set.out.file.name)

################################################################################

rm(list = ls())

################################################################################