# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development of New Mine Safety Technologies and Technological Applications

# 3 - Merge Mines Data
  # Merges cleaned mines and cleaned employment data

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu
# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/4/17

################################################################################

library(zoo)

################################################################################

# set root directory
# root = "/Injury-Classification/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/Injury-Classification/data"

# define file paths
clean.input.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.output.path = paste0(root, "/2_merged", collapse = NULL) 

# input 1: cleaned employment data
employment.in.file.name = paste0(clean.input.path, "/clean_employment.rds", collapse = NULL)
# input 2: cleaned mines data
mines.in.file.name = paste0(clean.input.path, "/clean_mines.rds", collapse = NULL)

# output 1: clean merged mines data
merged.mines.out.file.name = paste0(merged.output.path, "/merged_mines.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(merged.output.path, recursive = TRUE)

################################################################################

# MERGE MINES AND EMPLOYMENT/PRODUCTION DATA, THEN CLEAN

# load in datasets
employment = readRDS(employment.in.file.name)
mines = readRDS(mines.in.file.name)

# merge mines and employment/production data by mineid - wind up with 1,859,206 observations
mine.quarters = merge(employment, mines, by = c("mineid"), all = T)
rm(employment, mines)

# drop data from environments not of interest, wind up with 107,928 obs
# (facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16)
mine.quarters = subset(mine.quarters, minetype == "Underground")
mine.quarters = subset(mine.quarters, coalcormetalmmine == "C")
mine.quarters = mine.quarters[order(mine.quarters$mineid, mine.quarters$year, mine.quarters$quarter),]

# format date issued so that we can drop mines that were abandoned before our study period
datevars = names(mine.quarters)[grep("date", names(mine.quarters))]
for (i in 1:length(datevars)) {
  mine.quarters[, datevars[i]] = as.Date(as.character(mine.quarters[, datevars[i]]), "%m/%d/%Y")
}
mine.quarters$statusyear = as.yearqtr(mine.quarters$minestatusdate)
mine.quarters$statusyear = as.numeric(format(mine.quarters$statusyear, "%Y"))
mine.quarters$statusquarter = as.yearqtr(mine.quarters$minestatusdate)

# drop mines that were abandoned before our study period 
mine.quarters$too_early = ifelse((mine.quarters$minestatus == "Abandoned" | 
                                  mine.quarters$minestatus == "Abandoned and Sealed" |
                                  mine.quarters$minestatus == "NonProducing") & 
                                  mine.quarters$statusyear < 2000, 1, 0)
mine.quarters = mine.quarters[mine.quarters$too_early == 0, ]
mine.quarters$too_early = NULL

# there are some mines that didn't merge on any hours/employment data. We confirmed using the Mine Data 
# retrieval System that there is really no employment information on these mines. We drop them here. Now 95,109 obs
mine.quarters = mine.quarters[!is.na(mine.quarters$hours_qtr),]

# format date variables so quarter contains date-formatted year and quarter info (now observations will be unique on mineid-quarter)
mine.quarters$quarter = paste(mine.quarters$year, mine.quarters$quarter, sep = "-")
mine.quarters$quarter = ifelse(mine.quarters$quarter == "NA-NA", NA, mine.quarters$quarter)
mine.quarters$quarter = as.yearqtr(mine.quarters$quarter)

# re-name employment for ease of use
names(mine.quarters)[names(mine.quarters) == "avg_employee_cnt_qtr"] = "employment_qtr"

# output mines data
saveRDS(mine.quarters, file = merged.mines.out.file.name)

################################################################################

rm(list = ls())

################################################################################
