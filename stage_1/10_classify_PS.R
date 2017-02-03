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
cleaned.input.path = paste0(root, "/1_cleaned", collapse = NULL) 
prepped.input.path = paste0(root, "/5_prepared", collapse = NULL) 
coded.output.path = paste0(root, "/3_coded", collapse = NULL)

# inputs
  # prepped PS data for classification
prepped.classify.in.file.name = paste0(prepped.input.path, "/prepared_PS_classify.rds", collapse = NULL)
  # clean accidents data
accidents.in.file.name = paste0(cleaned.input.path, "/clean_accidents.rds", collapse = NULL)

# outputs
  # accidents data, classified as PS/non-PS (R dataset)
classified.accidents.file.name = paste0(coded.output.path, "/classified_accidents_PS.rds", collapse = NULL)
  # accidents data, classified as PS/non-PS (csv)
classified.accidents.file.name.csv = paste0(coded.output.path, "/classified_accidents_PS.csv", collapse = NULL)

# generate file paths
dir.create(coded.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# set seed to enable reproducible results
set.seed(625)

# prepped PS data for classification
  # 75743 rows; 103 columns; unique on documentno 
simple.ps = readRDS(prepped.classify.in.file.name)

# print PS indicator column number
which(colnames(simple.ps) == "PS") 

# bye
rm(root, prepped.input.path, coded.output.path, prepped.classify.in.file.name)

################################################################################

classify.vars = names(simple.ps)

# run boosting on master dataset
ps.adaboost = boosting(PS ~ ., 
                       data = simple.ps[simple.ps$type == "classified", !(names(simple.ps) %in% c("documentno", "type",  "accidentdate", "mineid"))], 
                       boos = T, mfinal = 300, coeflearn = "Freund")

# predict PS for unclassified accidetns
adaboost.pred = predict.boosting(ps.adaboost, 
                                 newdata = simple.ps[simple.ps$type == "unclassified", !(names(simple.ps) %in% c("documentno", "type",  "accidentdate"))])

# generate variable with predictions
adaboost.pred$class = as.factor(adaboost.pred$class)
accidents = cbind(simple.ps[simple.ps$type == "unclassified", ], adaboost.pred$class)
names(accidents)[names(accidents) == "adaboost.pred$class"] = "prediction"
accidents$PS = NULL

# re-code common false positives
accidents$prediction = ifelse(accidents$entrapment == 1, 1, accidents$prediction) 
accidents$prediction = ifelse(accidents$brokensteel == 1, 1, accidents$prediction)
accidents$prediction = ifelse(accidents$headroof == 1, 1, accidents$prediction)
accidents$prediction = ifelse(accidents$headcanopy == 1, 1, accidents$prediction)
accidents$prediction = ifelse(accidents$hole == 1, 1, accidents$prediction)
accidents$prediction = ifelse(accidents$jarring == 1 |
                                accidents$rock == 1 |
                                accidents$bodyseat == 1, 1, accidents$prediction)
accidents$prediction = ifelse(accidents$accident.only == 1, 1, accidents$prediction)
accidents$prediction = ifelse(accidents$falling.accident == 1, 1, accidents$prediction)
accidents$prediction = as.factor(accidents$prediction)

# merge NIOSH-classified accidents onto data
accidents = accidents[, c("prediction", "documentno")]
accidents = merge(simple.ps, accidents, by = "documentno", all = T)
accidents$PS = ifelse(accidents$PS == "YES", 2, accidents$prediction)
accidents$PS = ifelse(is.na(accidents$PS), 1, accidents$PS)
accidents = accidents[, c("PS", "documentno")]

################################################################################

# MERGE BACK ON ALL ACCIDENTS TO SELECT SAMPLE (AND GRAB MINEID/ACCIDENTDATE)

# load cleaned accidents
  # 75016 rows; 56 columns; unique on documentno
clean.accidents = readRDS(accidents.in.file.name)

# merge
  # 75016 rows; 4 columns; unique on documentno
accidents = merge(clean.accidents[, c("documentno", "accidentdate", "mineid")], 
                       accidents, by = "documentno", all = F)

# format PS
accidents$PS = ifelse(accidents$PS == 2, 1, 0)
accidents$PS = factor(accidents$PS)

################################################################################

# OUTPUT CLASSIFIED DATA

# output CSV
  # 75016 rows; 4 columns; unique on documentno 
write.csv(accidents, file = classified.accidents.file.name.csv)

# output R dataset
saveRDS(accidents, file = classified.accidents.file.name)

################################################################################

rm(list = ls())

################################################################################