# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Clean Operator History
  # Cleans controller/operator history data

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library(stringr)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL) 
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # prepped mine-years
history.in.file.name = paste0(originals.path, "/ControllerOperatorHistory.txt", collapse = NULL)

# outputs
  # merged and prepped mine-years data
history.out.file.name = paste0(clean.path, "/clean_operator_history.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(clean.path, recursive = TRUE)

################################################################################

# MERGE MINES AND EMPLOYMENT/PRODUCTION DATA, THEN FORMAT VARIABLES

# read controller/operator history data 
  # 144065 rows; 13 columns; unique on mineid-year
history = read.table(history.in.file.name, header = T, sep = "|")

# drop data from environments not of interest
  # 63143 rows; 13 columns; unique on mineid-year
history = history[(history$COAL_METAL_IND == "C"), ]

# rename variables
names(history)[names(history) == "MINE_ID"] = "mineid"
names(history)[names(history) == "OPERATOR_ID"] = "operatorid"
names(history)[names(history) == "OPERATOR_START_DT"] = "operatorstartdt"
names(history)[names(history) == "OPERATOR_END_DT"] = "operatorenddt"
history = history[, c("operatorid", "mineid", "operatorstartdt", "operatorenddt")]

# format mineid
history$mineid = as.character(history$mineid)
history$mineid = str_pad(history$mineid, 7, pad = "0")

# convert start/end dates into quarters & replace end quarter with 2016 if missing
enddtvars = "operatorenddt"
for (i in 1:length(enddtvars)) {
  history[, enddtvars[i]] = as.character(history[, enddtvars[i]])
  history[, enddtvars[i]] = ifelse(history[, enddtvars[i]] == "", NA, history[, enddtvars[i]])
  history[, enddtvars[i]] = ifelse(is.na(history[, enddtvars[i]]), "01/01/2016", history[, enddtvars[i]]) # Q1 2016
}
datevars = c("operatorstartdt", "operatorenddt")
for (i in 1:length(datevars)) {
  history[, datevars[i]] = as.Date(as.character(history[, datevars[i]]), "%m/%d/%Y")
  history[, datevars[i]] = as.yearqtr(history[, datevars[i]])
}

################################################################################

# output mine-level data
saveRDS(history, file = history.out.file.name)

################################################################################

rm(list = ls())

################################################################################
