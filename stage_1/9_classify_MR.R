# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 10 - Classify MR (Maintenance and Repair) 
  # Classifies all accidents from MSHA open data portal as MR/non-MR

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
  # prepped MR data for classification
prepped.classify.in.file.name = paste0(prepped.input.path, "/prepared_MR_classify.rds", collapse = NULL)
  # clean accidents data
accidents.in.file.name = paste0(cleaned.input.path, "/clean_accidents.rds", collapse = NULL)

# outputs
  # accidents data, classified as MR/non-MR (R dataset)
classified.accidents.file.name = paste0(coded.output.path, "/classified_accidents_MR.rds", collapse = NULL)
  # accidents data, classified as MR/non-MR (csv)
classified.accidents.file.name.csv = paste0(coded.output.path, "/classified_accidents_MR.csv", collapse = NULL)

# generate file paths
dir.create(coded.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# set seed to enable reproducible results
set.seed(625)

# prepped MR data for classification
  # 75700 rows; 73 columns; unique on documentno 
simple = readRDS(prepped.classify.in.file.name)

# print PS indicator column number - 2
which(colnames(simple) == "MR") 

# bye
rm(root, prepped.input.path, coded.output.path, prepped.classify.in.file.name)

################################################################################

# USE BOOSTING TO CLASSIFY REAL ACCIDENTS DATA WITH UNKNOWN "MR" STATUS

post.algorithm = c("flashburn", "carpal.tunnel", "cumulative", 
                   "hearingloss", "exposure", "heartattack", 
                   "unrelated", "working.on", "barring",
                   "otherverb", "othernoun", "other.keyword")

# implement Adaptive Boosting
mr.adaboost = boosting(MR ~ . , data = simple[simple$type!="unclassified", 
                                              !(names(simple) %in% c('documentno','type', 'mineid', 'accidentdate', post.algorithm))], 
                       boos = T, mfinal = 300, coeflearn = 'Freund')

# generate predictions
adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[simple$type == "unclassified", 
                                                               !(names(simple) %in% c('documentno','type', 'mineid', 'accidentdate', post.algorithm))])

# apply predictions to unclassified injuries
accidents.data = cbind(simple[simple$type == "unclassified",], adaboost.pred$class)
names(accidents.data)[names(accidents.data) == 'adaboost.pred$class'] = 'adaboost'

################################################################################

# POST-PROCESSING

# now manually weed out false positives and negatives that could not have been foreseen in the training data 
accidents.data$manual.predict = ifelse(((accidents.data$likely.activy == 1 & 
                                           accidents.data$likely.class == 1 & 
                                           accidents.data$false.keyword == 0) |
                                          (accidents.data$likely.occup == 1 & 
                                             (accidents.data$maybe.activy == 1 | 
                                                accidents.data$likely.activy == 1 | 
                                                accidents.data$likely.class == 1 | 
                                                accidents.data$maybe.keyword == 1)) |
                                          (accidents.data$likely.activy == 1 & 
                                             (accidents.data$maybe.occup == 1 | 
                                                accidents.data$likely.class == 1 | 
                                                accidents.data$maybe.keyword == 1)) |
                                          (accidents.data$maybe.occup == 1 & 
                                             ((accidents.data$maybe.activy == 1 & 
                                                 accidents.data$likely.class == 1) | 
                                                (accidents.data$likely.class == 1 & 
                                                   accidents.data$maybe.keyword == 1) | 
                                                (accidents.data$maybe.activy == 1 & 
                                                   accidents.data$maybe.keyword == 1))) |
                                          accidents.data$likely.keyword == 1) & 
                                         accidents.data$accident.only == 0, 1, 0)

# flag false negatives
accidents.data$false.neg = ifelse(accidents.data$flashburn == 1 & accidents.data$adaboost == "NO", 1, 0)

# flag definitely and likely false positives (including accident-only observations)
accidents.data$false.pos = ifelse((accidents.data$carpal.tunnel == 1 | 
                                     accidents.data$cumulative == 1 | 
                                     accidents.data$heartattack == 1 |
                                accidents.data$hearingloss == 1 | 
                                  accidents.data$exposure == 1 |
                                  accidents.data$unrelated == 1 | 
                                  accidents.data$accident.only == 1) & 
                                  accidents.data$adaboos == "YES", 1, 0)
accidents.data$false.pos = ifelse(accidents.data$adaboos == "YES" & 
                             accidents.data$likely.keyword == 0 &
                             accidents.data$maybe.keyword == 0 & 
                             accidents.data$other.keyword == 0, 1, accidents.data$false.pos)  

# format classifications
accidents.data$MR  = ifelse((accidents.data$adaboost == "YES" & 
                               accidents.data$false.pos == 0) | 
                              accidents.data$false.neg == 1, 1, 0)

# remove unessential variables
accidents.data = accidents.data[, c(match("adaboost", names(accidents.data)),
                                    match("documentno", names(accidents.data)))]

# merge on predictions from training obs
accidents.data = merge(simple[, c("documentno", "MR")], accidents.data, by = "documentno", all = TRUE)
accidents.data$MR = ifelse(accidents.data$MR == "YES", "1", "0")
accidents.data$MR = ifelse(!is.na(accidents.data$adaboost) & accidents.data$adaboost == "YES", 1, accidents.data$MR)

# remove unessential variables
accidents.data = accidents.data[, c(-grep("adaboost", names(accidents.data)))]

################################################################################

# MERGE BACK ON ALL ACCIDENTS TO SELECT SAMPLE (AND GRAB MINEID/ACCIDENTDATE)

# load cleaned accidents
  # 75016 rows; 56 columns; unique on documentno
accidents = readRDS(accidents.in.file.name)

# merge
  # 75016 rows; 4 columns; unique on documentno
accidents.data = merge(accidents[, c("documentno", "accidentdate", "mineid")], 
                       accidents.data, by = "documentno", all = F)

# format MR
accidents.data$MR = factor(accidents.data$MR)

# bye
rm(simple, accidents)

################################################################################

# OUTPUT CLASSIFIED DATA

# save a CSV file
  # 75016 rows; 4 columns; unique on documentno 
write.csv(accidents.data, file = classified.accidents.file.name.csv)

# save R dataset
saveRDS(accidents.data, file = classified.accidents.file.name)

################################################################################

rm(list = ls())

################################################################################