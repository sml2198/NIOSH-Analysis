# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 11 - Classify PS (Pinning and Striking) 
  # Classifies all accidents from MSHA open data portal as PS/non-PS

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

library(adabag)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
prepped.input.path = paste0(root, "/5_prepped", collapse = NULL) 
coded.output.path = paste0(root, "/3_coded", collapse = NULL)

# inputs
  # prepped PS data for classification
prepped.classify.in.file.name = paste0(prepped.input.path, "/prepped_PS_classify.rds", collapse = NULL)

# outputs
  # accidents data, classified as PS/non-PS (R dataset)
classified.accidents.file.name = paste0(coded.output.path, "/classified_accidents_PS.rds", collapse = NULL)
  # accidents data, classified as PS/non-PS (csv)
classified.accidents.file.name.csv = paste0(coded.output.path, "/classified_accidents_PS.csv", collapse = NULL)

# generate file paths
dir.create(prepped.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# set seed to enable reproducible results
set.seed(625)

# read cleaned PS training set data and remive "type" field (it's all the same)
# 1000 rows; 100 columns; unique on documentno 
simple.ps = readRDS(prepped.train.set.in.file.name)
simple.ps = simple.ps[, -c(match("type", names(simple.ps)))]

# print PS indicator column number
which(colnames(simple.ps) == "PS") 

# bye
rm(root, prepped.input.path, prepped.train.set.in.file.name)

################################################################################
