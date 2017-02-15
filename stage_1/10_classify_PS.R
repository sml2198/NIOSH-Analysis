# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 11 - Classify PS (Pinning and Striking) Accidents
  # Classifies all accidents from MSHA open data portal as PS/non-PS

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/7/2017

################################################################################

library(adabag)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
#root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 
prepared.path = paste0(root, "/data/5_prepared", collapse = NULL) 
classified.path = paste0(root, "/data/3_coded", collapse = NULL)

# inputs
  # prepared merged PS accidents data
    # produced in 6_prepare_PS
prepared.classify.in.file.name = paste0(prepared.path, "/prepared_PS_classify.rds", collapse = NULL)
  # cleaned accidents data
    # produced in 1_clean_accidents
accidents.in.file.name = paste0(cleaned.path, "/clean_accidents.rds", collapse = NULL)

# outputs
  # accidents data, classified as PS/non-PS (rds)
classified.accidents.file.name = paste0(classified.path, "/classified_accidents_PS.rds", collapse = NULL)
  # accidents data, classified as PS/non-PS (csv)
classified.accidents.file.name.csv = paste0(classified.path, "/classified_accidents_PS.csv", collapse = NULL)

# generate file paths
dir.create(classified.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, cleaned.path, prepared.path, classified.path)

################################################################################

# READ DATA

# read prepared merged PS accidents data
  # 75743 rows; 103 columns; unique on documentno 
pre.classify = readRDS(prepared.classify.in.file.name)

# read prepared merged MR accidents data
  # 75016 rows; 17 columns; unique on documentno
all.accidents = readRDS(accidents.in.file.name)

# bye
rm(prepared.classify.in.file.name, accidents.in.file.name)

################################################################################

# TRAIN ADAPTIVE BOOSTING ALGORITHM AND USE TO CLASSIFY ACCIDENTS

# set seed
set.seed(625)

# train algorithm
ps.adaboost = boosting(PS ~ ., data = pre.classify[pre.classify$type != "unclassified", 
                                                   !(names(pre.classify) %in% c("documentno", "type", "mineid", "accidentdate"))], 
                       boos = TRUE, mfinal = 300, coeflearn = "Freund")

# generate classifications
adaboost.pred = predict.boosting(ps.adaboost, newdata = pre.classify[pre.classify$type == "unclassified", 
                                                                     !(names(pre.classify) %in% c("documentno", "type", "mineid", "accidentdate"))])


# apply predictions to unclassified injuries
classified = cbind(pre.classify[pre.classify$type == "unclassified", ], adaboost.pred$class)
names(classified)[names(classified) == "adaboost.pred$class"] = "adaboost"
classified$adaboost = ifelse(!is.na(classified$adaboost) & classified$adaboost == "YES", 1, 0)

# bye
rm(adaboost.pred, ps.adaboost)

################################################################################

# POST-PROCESSING

# remove false positives
classified$adaboost = ifelse(classified$entrapment == 1, 0, classified$adaboost) 
classified$adaboost = ifelse(classified$brokensteel == 1, 0, classified$adaboost)
classified$adaboost = ifelse(classified$headroof == 1, 0, classified$adaboost)
classified$adaboost = ifelse(classified$headcanopy == 1, 0, classified$adaboost)
classified$adaboost = ifelse(classified$hole == 1, 0, classified$adaboost)
classified$adaboost = ifelse(classified$jarring == 1 |
                                classified$rock == 1 |
                                classified$bodyseat == 1, 0, classified$adaboost)
classified$adaboost = ifelse(classified$accident.only == 1, 0, classified$adaboost)
classified$adaboost = ifelse(classified$falling.accident == 1, 0, classified$adaboost)

################################################################################

# EDIT DATA

# drop unnecessary variables
  # 74743 rows; 2 columns; unique on documentno
classified = classified[, c("adaboost", "documentno")]

# merge on training observations
  # 75743 rows; 4 columns; unique on documentno
classified = merge(pre.classify[, c("documentno", "PS", "type")], classified, by = "documentno", all = TRUE)
classified$PS = ifelse(classified$type == "classified" & classified$PS == "YES", 1, 
                       ifelse(classified$type == "classified" & classified$PS == "NO", 0, NA))
classified$PS = ifelse(classified$type == "unclassified" & classified$adaboost == 1, 1, 
                       ifelse(classified$type == "unclassified" & classified$adaboost == 0, 0, classified$PS))

# check
table(classified$PS)
# non-PS    PS
# 73541     2202 

# bye
classified$adaboost = classified$type = NULL
rm(pre.classify)

################################################################################

# MERGE PREDICTIONS ONTO ACCIDENTS DATA

# merge
  # 75016 rows; 4 columns; unique on documentno
accidents = merge(all.accidents[, c("documentno", "accidentdate", "mineid")], 
                  classified, by = "documentno", all = FALSE)

# format variables
accidents$PS = factor(accidents$PS)

# check
table(accidents$PS)
# non-PS   PS 
# 73006  2010 

# bye
rm(all.accidents, classified)

################################################################################

# OUTPUT CLASSIFIED DATA

# output classified accidents data (csv)
  # 75016 rows; 4 columns; unique on documentno 
write.csv(accidents, file = classified.accidents.file.name.csv)

# output classified accidents data (rds)
  # 75016 rows; 4 columns; unique on documentno 
saveRDS(accidents, file = classified.accidents.file.name)

# bye
rm(list = ls())

################################################################################
