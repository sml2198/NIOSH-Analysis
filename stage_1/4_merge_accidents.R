# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Merge Accidents
  # Merges accidents data (produced in 1_clean_accidents) 
    # and MR training/testing set (produced in 2_clean_MR_training_set) 
    # or PS training/testing set (3_clean_PS_training_set)

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com
      # and Julia Bodson, juliabodson@gmail.com

# Last edit 1/23/2017

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
MR.train.test.set.in.file.name = paste0(cleaned.path, "/clean_MR_train_test_set.rds", collapse = NULL)
  # cleaned PS training/testing set
    # produced in 3_clean_PS_training_set
PS.train.test.set.in.file.name = paste0(cleaned.path, "/clean_PS_train_test_set.rds", collapse = NULL)

# outputs
  # merged MR accidents data
merged.MR.out.file.name = paste0(merged.path, "/merged_MR_accidents.rds", collapse = NULL)
  # merged PS accidents data
merged.PS.out.file.name = paste0(merged.path, "/merged_PS_accidents.rds", collapse = NULL)

# generate file paths
dir.create(merged.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, cleaned.path, merged.path)

################################################################################

for (injury in c("MR", "PS")) { # merge accidents with MR and PS training/testing sets
  
  # READ DATA
  
  if (injury == "MR") {
    train.test.set.in.file.name = MR.train.test.set.in.file.name
  }
  if (injury == "PS") {
    train.test.set.in.file.name = PS.train.test.set.in.file.name
  }
  
  # read cleaned accidents data
    # 75016 rows; 17 columns; unique on documentno
  accidents = readRDS(accidents.in.file.name)
  
  # read cleaned MR or PS training/testing set
    # MR: 1018 rows; 18 columns; unique on documentno
    # PS: 1000 rowsl 18 columns; unique on documentno
  train.test = readRDS(train.test.set.in.file.name)
  
  # bye
  rm(train.test.set.in.file.name)

  ##############################################################################
  
  # COMBINE ACCIDENTS DATA AND MR/PS TRAINING/TESTING SET
  
  # distinguish classified/unclassified accidents
  accidents$type = "unclassified"
  train.test$type = "classified"
  
  # make accidents data and MR/PS training/testing set compatible
  accidents[, injury] = "" 
  
  # deal with duplicated documentnos between datasets
    # create lists of document numbers from each dataset 
  train.test.docnos = as.character(train.test$documentno)
  accidents.docnos = as.character(accidents$documentno)
    
    # identify non-common document numbers
  keep.docnos = setdiff(accidents.docnos, train.test.docnos)  
    
    # remove observations from accidents data that are in the MR/PS training/testing set
      # MR: 74682 rows; 19 columns; unique on documentno
      # PS: 74743 rows; 19 columns; unique on documentno
  accidents = accidents[which(accidents$documentno %in% keep.docnos), ]
    
  # combine accidents data and MR/PS training/testing set 
    # MR: 75700 rows; 19 columns; unique on documentno 
    # PS: 75743 rows; 19 columns; unique on documentno
  merged.accidents = rbind(train.test, accidents) 
    
  # bye
  rm(accidents.docnos, train.test.docnos, keep.docnos, 
     accidents, train.test)
  
  ##############################################################################
  
  # OUTPUT DATA
  
  if (injury == "MR") {
    merged.out.file.name = merged.MR.out.file.name
  }
  if (injury == "PS") {
    merged.out.file.name = merged.PS.out.file.name
  }
  
  # output merged data
    # MR: 75700 rows; 19 columns; unique on documentno
    # PS: 75743 rows; 19 columns; unique on documentno
  saveRDS(merged.accidents, file = merged.out.file.name)
  
  # bye
  rm(merged.accidents, merged.out.file.name)
  
}

################################################################################

# bye
rm(list = ls())

################################################################################
