# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Merge MR (Maintenance and Repair) Accidents
  # Merges accidents data (produced in 1_clean_accidents) and MR training/testing 
    # set (produced in 2_clean_MR_training_set)

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com
# Last edit 1/13/17

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
cleaned.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/2_merged", collapse = NULL) 

# inputs
  # cleaned accidents data
    # produced in 1_clean_accidents
accidents.in.file.name = paste0(cleaned.path, "/clean_accidents.rds", collapse = NULL)
  # cleaned MR training/testing set
    # produced in 2_clean_MR_training_set
training.set.in.file.name = paste0(cleaned.path, "/clean_MR_training_set.rds", collapse = NULL)

# outputs
  # merged MR accidents data
merged.out.file.name = paste0(merged.path, "/merged_MR_accidents.rds", collapse = NULL)

# generate file paths
dir.create(merged.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, cleaned.path, merged.path)

################################################################################

# READ DATA

# cleaned accidents data
  # 75016 rows; 19 columns; unique on documentno
accidents.data = readRDS(accidents.in.file.name)

# cleaned MR training/testing set
  # 1018 rows; 20 columns; unique on documentno
mr.data = readRDS(training.set.in.file.name)

# bye
rm(accidents.in.file.name, training.set.in.file.name)

################################################################################

# COMBINE ACCIDENTS DATA AND MR TRAINING/TESTING SET
  
# distinguish classified/unclassified accidents
accidents.data$type = "unclassified"
mr.data$type = "classified"

# make accidents data and MR training/testing set compatible
accidents.data$MR = "" 

# deal with duplicated documentnos between datasets
  # create lists of document numbers from each dataset 
mr.docnos = as.character(mr.data$documentno)
accident.docnos = as.character(accidents.data$documentno)
  
  # identify non-common document numbers
keep.docnos = setdiff(accident.docnos, mr.docnos)  
  
  # remove observations from accidents data that are in the MR training/testing set
    # 74682 rows; 21 columns; unique on documentno
accidents.data = accidents.data[which(accidents.data$documentno %in% keep.docnos), ]
  
# combine master MR dataset and accidents dataset
  # 75700 rows; 21 columns; unique on documentno  
mr.data = rbind(mr.data, accidents.data) 
  
# bye
rm(accident.docnos, mr.docnos, keep.docnos, accidents.data)

################################################################################

# OUTPUT MERGED DATA

# output merged data
  # 75700 rows; 21 columns; unique on documentno
saveRDS(mr.data, file = merged.out.file.name)

# bye
rm(list = ls())

################################################################################
