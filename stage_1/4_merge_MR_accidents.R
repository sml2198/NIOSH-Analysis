# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Merge MR (Maintenance and Repair) Accidents
  # Train/Test:

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
cleaned.input.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.output.path = paste0(root, "/2_merged", collapse = NULL) 

# inputs
  # clean PS training set
training.set.in.file.name = paste0(cleaned.input.path, "/clean_MR_training_set.rds", collapse = NULL)
  # clean accidents data
accidents.in.file.name = paste0(cleaned.input.path, "/clean_accidents.rds", collapse = NULL)

# outputs
  # merged PS accidents 
merged.out.file.name = paste0(merged.output.path, "/merged_mR_accidents.rds", collapse = NULL)

# generate file paths
dir.create(merged.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# cleaned MR training set
  # 1018 rows; 107 columns; unique on documentno
mr.data = readRDS(training.set.in.file.name)

# cleaned accidents
  # 75016 rows; 56 columns; unique on documentno
accidents.data = readRDS(accidents.in.file.name)

################################################################################

# COMBINE MASTER DATASET AND ACCIDENTS DATA
  
# distinguish classified/unclassified accidents (mr.data was marked in clean stage)
accidents.data$type = "unclassified"
  
# make master dataset and accidents data compatible
mr.data$datasource = "training"  
mr.data$investigationbegindate = "" 
accidents.data$contractor_accident = "" 
accidents.data$MR = "" 

# drop variables not common to the datasets
drop = c("assesscontrolno", "part48training", "controllerbegindate", 
         "fiscalquarter", "fiscalyear", "year", "closed_doc_no",
         "quarter", "avg_hours_qtr", "avg_employment_qtr", 
         "avg_coal_prod_qtr")
accidents.data = accidents.data[, !(names(accidents.data) %in% drop)]
  
drop = c("death", "i", "fiscalquarter", "fiscalyear", "year", "closed_doc_no")
mr.data = mr.data[, !(names(mr.data) %in% drop)]  

accident.names = names(accidents.data)
mr.data = mr.data[, names(mr.data) %in% accident.names]
  
# format variables
accidents.data$narrative = tolower(accidents.data$narrative)
accidents.data$degreeofinjury = tolower(accidents.data$degreeofinjury)
accidents.data$accidentclassification = tolower(accidents.data$accidentclassification)
accidents.data$accidenttype = tolower(accidents.data$accidenttype)
accidents.data$natureofinjury = tolower(accidents.data$natureofinjury)
accidents.data$mineractivity = tolower(accidents.data$mineractivity)
accidents.data$occupation = tolower(accidents.data$occupation)
accidents.data$typeofequipment = tolower(accidents.data$typeofequipment)
accidents.data$sourceofinjury = tolower(accidents.data$sourceofinjury)
accidents.data$bodypart = tolower(accidents.data$bodypart)
accidents.data$equipmanufacturer = tolower(accidents.data$equipmanufacturer)
accidents.data$immediatenotificationclass = tolower(accidents.data$immediatenotificationclass)
accidents.data$uglocation = tolower(accidents.data$uglocation)
  
# deal with duplicated documentnos between datasets
# create lists of document numbers from each dataset 
mr.docnos = as.character(mr.data$documentno)
accident.docnos = as.character(accidents.data$documentno)
  
# identify common document numbers
keep.docnos = setdiff(accident.docnos, mr.docnos)  
  
# remove observations from accidents data present in the mr.data (training set)
accidents.data = accidents.data[which(accidents.data$documentno %in% keep.docnos), ]
  
# combine master MR dataset and accidents dataset
  # 75700 rows; 56 columns; unique on documentno  
mr.data = rbind(mr.data, accidents.data) 
  
# bye
rm(drop, accident.names, accident.docnos, mr.docnos, keep.docnos, accidents.data)

################################################################################

# output clean training set
  # 75700 rows; 56 columns; unique on documentno
saveRDS(mr.data, file = merged.out.file.name)

################################################################################

rm(list = ls())

################################################################################
