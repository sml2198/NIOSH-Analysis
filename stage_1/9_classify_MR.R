# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 10 - Classify MR (Maintenance and Repair) 
  # Classifies all accidents from MSHA open data portal as MR/non-MR

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/3/2017

################################################################################

library(adabag)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
cleaned.path = paste0(root, "/1_cleaned", collapse = NULL) 
prepared.path = paste0(root, "/5_prepared", collapse = NULL) 
classified.path = paste0(root, "/3_coded", collapse = NULL)

# inputs
  # prepared merged MR accidents data
    # produced in 7_train_test_MR
prepared.classify.in.file.name = paste0(prepared.path, "/prepared_MR_classify.rds", collapse = NULL)
  # cleaned accidents data
    # produced in 1_clean_accidents
accidents.in.file.name = paste0(cleaned.path, "/clean_accidents.rds", collapse = NULL)

# outputs
  # accidents data, classified as MR/non-MR (rds)
classified.accidents.file.name = paste0(classified.path, "/classified_accidents_MR.rds", collapse = NULL)
  # accidents data, classified as MR/non-MR (csv)
classified.accidents.file.name.csv = paste0(classified.path, "/classified_accidents_MR.csv", collapse = NULL)

# generate file paths
dir.create(classified.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)  

# bye
rm(root, cleaned.path, prepared.path, classified.path)

################################################################################

# READ DATA

# read prepared merged MR accidents data
  # 75700 rows; 69 columns; unique on documentno 
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

# define post-classification variables (to exclude from algorithm)
post.classification = c("barring", "carpal.tunnel", "cumulative",
                        "exposure", "flashburn", "hearingloss", 
                        "heartattack", "other.keyword", "othernoun",
                        "otherverb", "unrelated", "working.on")

# train algorithm
mr.adaboost = boosting(MR ~ . , data = pre.classify[pre.classify$type != "unclassified", 
                                            !(names(pre.classify) %in% c("documentno", "type", "mineid", "accidentdate", post.classification))], 
                       boos = TRUE, mfinal = 300, coeflearn = "Freund")

# generate classifications
adaboost.pred = predict.boosting(mr.adaboost, newdata = pre.classify[pre.classify$type == "unclassified", 
                                                               !(names(pre.classify) %in% c("documentno", "type", "mineid", "accidentdate", post.classification))])

# apply predictions to unclassified injuries
classified = cbind(pre.classify[pre.classify$type == "unclassified", ], adaboost.pred$class)
names(classified)[names(classified) == "adaboost.pred$class"] = "adaboost"

# bye
rm(post.classification)

################################################################################

# POST-PROCESSING

# remove false positives/negatives that could not have been forseen in training
classified$manual.predict = ifelse(((classified$likely.activity == 1 & classified$false.keyword == 0) |
                                      (classified$likely.occup == 1 & 
                                         (classified$likely.activity == 1 | classified$maybe.keyword == 1)) |
                                          (classified$likely.activity == 1 & 
                                             (classified$maybe.occup == 1 | classified$maybe.keyword == 1)) |
                                      (classified$maybe.occup == 1 & 
                                         ((classified$maybe.keyword == 1) | (classified$maybe.keyword == 1))) |
                                          classified$likely.keyword == 1) & 
                                     classified$accident.only == 0, 1, 0)

classified$false.neg = ifelse(classified$flashburn == 1 & classified$adaboost == "NO", 1, 0)

classified$false.pos = ifelse((classified$carpal.tunnel == 1 | 
                                 classified$cumulative == 1 | 
                                 classified$heartattack == 1 |
                                 classified$hearingloss == 1 | 
                                 classified$exposure == 1 |
                                 classified$unrelated == 1 | 
                                 classified$accident.only == 1) & 
                                classified$adaboost == "YES", 1, 0)

classified$false.pos = ifelse(classified$adaboost == "YES" & 
                                classified$likely.keyword == 0 &
                                classified$maybe.keyword == 0 & 
                                classified$other.keyword == 0, 1, classified$false.pos)  

################################################################################

# EDIT DATA

# format classifications
classified$adaboost = ifelse((classified$adaboost == "YES" & 
                                classified$false.pos == 0) | 
                               classified$false.neg == 1, 1, 0)

# drop unnecessary variables
classified = classified[, c("adaboost", "documentno")]

# merge on training observations
classified = merge(pre.classify[, c("documentno", "MR")], classified, by = "documentno", all = TRUE)
classified$MR = ifelse(classified$MR == "YES", 1, 0)
classified$MR = ifelse(!is.na(classified$adaboost) & classified$adaboost == 1, 1, classified$MR)

# check
table(classified$MR)
# non-MR    MR 
# 61231     14469 

# bye
classified$adaboost = NULL

################################################################################

# MERGE BACK ON ALL ACCIDENTS TO SELECT SAMPLE (AND GRAB MINEID/ACCIDENTDATE)


# merge
  # 75016 rows; 4 columns; unique on documentno
classified = merge(all.accidents[, c("documentno", "accidentdate", "mineid")], 
                       classified, by = "documentno", all = F)

# format MR
classified$MR = factor(classified$MR)

# bye
rm(data, all.accidents)

################################################################################

# OUTPUT CLASSIFIED DATA

# save a CSV file
  # 75016 rows; 4 columns; unique on documentno 
write.csv(classified, file = classified.accidents.file.name.csv)

# save R dataset
saveRDS(classified, file = classified.accidents.file.name)

table(classified$MR)
# non-MR   MR 
# 60815 14201 

################################################################################

rm(list = ls())

################################################################################
