# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 5 - Merge PS (Pinning and Striking) Accidents
  # Train/Test:

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
cleaned.input.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.output.path = paste0(root, "/2_merged", collapse = NULL) 

# inputs
  # clean PS training set
training.set.in.file.name = paste0(cleaned.input.path, "/clean_PS_training_set.rds", collapse = NULL)
  # clean accidents data
accidents.in.file.name = paste0(cleaned.input.path, "/clean_accidents.rds", collapse = NULL)

# outputs
  # merged PS accidents 
merged.out.file.name = paste0(merged.output.path, "/merged_PS_accidents.rds", collapse = NULL)

# generate file paths
dir.create(merged.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# read unclassified accidents data 
  # 75016 rows; 56 columns; unique on documentno 
accidents.data = readRDS(accidents.in.file.name)

# read clean master PS dataset
  # 1000 rows; 105 columns; unique on documentno 
ps.data = readRDS(training.set.in.file.name)

# bye 
rm(root, cleaned.input.path, merged.output.path, training.set.in.file.name, accidents.in.file.name)

################################################################################

# COMBINE MASTER DATASET AND ACCIDENTS DATA

# distinguish classified/unclassified accidents (ps.data already has this field)
accidents.data$type = "unclassified"

# make master dataset and accidents data compatible
accidents.data$closed_doc_no = 
  accidents.data$fiscalyear = 
  accidents.data$fiscalquarter = 
  accidents.data$investigationbegindate = NULL

accidents.data$PS = ""

accident.names = names(accidents.data)
ps.data = ps.data[, names(ps.data) %in% accident.names]

# deal with duplicated documentnos between datasets
# create lists of document numbers from each dataset 
ps.docnos = as.character(ps.data$documentno)
accident.docnos = as.character(accidents.data$documentno)

# identify common document numbers
keep.docnos = setdiff(accident.docnos, ps.docnos) 

# remove observations from accidents data present in the ps.data (training set)
  # 74743 rows; 54 columns; unique on documentno
accidents.data = accidents.data[which(accidents.data$documentno %in% keep.docnos), ]

# combine master PS dataset and accidents dataset
  # 75743 rows; 54 columns; unique on documentno
ps.data = rbind(ps.data, accidents.data)

# bye
rm(accidents.data, accident.docnos, accident.names, keep.docnos, ps.docnos)

################################################################################

# output merged PS data
  # 75743 rows; 54 columns; unique on documentno
saveRDS(ps.data, file = merged.out.file.name)

################################################################################

rm(list = ls())

################################################################################
