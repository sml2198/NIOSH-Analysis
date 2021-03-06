# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Clean Operator History
  # Cleans controller/operator history data from the MSHA open data portal

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 2/7/2017

################################################################################

library(stringr)
library(zoo)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
 
# define file paths
originals.path = paste0(root, "/data/0_originals", collapse = NULL) 
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 

# inputs
  # controller/operator history data from the MSHA open data portal
    # downloaded on 4/20/16 from http://arlweb.msha.gov/OpenGovernmentData/OGIMSHA.asp
history.in.file.name = paste0(originals.path, "/ControllerOperatorHistory.txt", collapse = NULL)

# outputs
  # cleaned controller/operator history data
history.out.file.name = paste0(cleaned.path, "/clean_operator_history.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read controller/operator history data 
  # 144065 rows; 13 columns
history = read.table(history.in.file.name, header = TRUE, sep = "|")

# bye
rm(history.in.file.name)

################################################################################

# CLEAN DATA

# drop data from environments not of interest
  # 63143 rows; 13 columns
history = history[which(history$COAL_METAL_IND == "C"), ]

# drop unnecessary variables
  # 63143 rows; 4 columns
history = history[, c("MINE_ID", "OPERATOR_END_DT", "OPERATOR_ID", "OPERATOR_START_DT")]

# rename variables
names(history)[names(history) == "MINE_ID"] = "mineid"
names(history)[names(history) == "OPERATOR_END_DT"] = "operatorenddt"
names(history)[names(history) == "OPERATOR_ID"] = "operatorid"
names(history)[names(history) == "OPERATOR_START_DT"] = "operatorstartdt"

# format variables
history$mineid = str_pad(history$mineid, 7, pad = "0")

# replace end date with 2016 Q1 if missing
history[, "operatorenddt"] = as.character(history[, "operatorenddt"])
history[, "operatorenddt"] = ifelse(history[, "operatorenddt"] == "", NA, history[, "operatorenddt"])
history[, "operatorenddt"] = ifelse(is.na(history[, "operatorenddt"]), "01/01/2016", history[, "operatorenddt"]) 

# format start/end date  
datevars = c("operatorstartdt", "operatorenddt")
for (i in 1:length(datevars)) {
  history[, datevars[i]] = as.Date(as.character(history[, datevars[i]]), "%m/%d/%Y")
  history[, datevars[i]] = as.yearqtr(history[, datevars[i]])
  history[, datevars[i]] = as.numeric(format(history[, datevars[i]], "%Y"))
}

# drop duplicates
  # 55770 rows; 4 columns; unique on minied-operatorid-operatorstartdt
history = history[order(history$mineid), ]
history$operatorid = as.character(history$operatorid)
history = history[!duplicated(history), ]

# bye
rm(i, datevars)

################################################################################

# OUTPUT DATA

# output cleaned controller/operator history data 
  # 55770 rows; 4 columns; unique on minied-operatorid-operatorstartdt
saveRDS(history, file = history.out.file.name)

# bye
rm(list = ls())

################################################################################
