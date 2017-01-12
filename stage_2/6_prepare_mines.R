# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Prepare Mines Data
  # Merges cleaned mines and cleaned employment data and collapses
  # Then merges collapsed accidents data

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library(plyr)
library(zoo)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "/Users/Sarah/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 
collapsed.path = paste0(root, "/4_collapsed", collapse = NULL) 
prepped.path = paste0(root, "/5_prepped", collapse = NULL) 

# inputs
  # cleaned employment data
employment.in.file.name = paste0(clean.path, "/clean_employment.rds", collapse = NULL)
  # cleaned mines data
mines.in.file.name = paste0(clean.path, "/clean_mines.rds", collapse = NULL)
  # cleaned MR data
MR.in.file.name = paste0(collapsed.path, "/collapsed_MR_accidents.rds", collapse = NULL)
  # cleaned PS data
PS.in.file.name = paste0(collapsed.path, "/collapsed_PS_accidents.rds", collapse = NULL)

# outputs
  # merged and prepped mine-years data
mine.years.out.file.name = paste0(prepped.path, "/prepped_mine_years.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(prepped.path, recursive = TRUE)

################################################################################

# MERGE MINES AND EMPLOYMENT/PRODUCTION DATA, THEN FORMAT VARIABLES

# load in datasets
  # 42019 rows; 6 columns; unique on mineid-year-quarter
employment = readRDS(employment.in.file.name)
  # 86362 rows; 59 columns; unique on mineid
mines = readRDS(mines.in.file.name)

# merge mines and employment/production data by mineid to produce mine-quarter data
  # 126313 rows; 64 columns; unique on mine-year-quarter
mine.quarters = merge(employment, mines, by = c("mineid"), all = T)
rm(employment, mines)

# format mine status
mine.quarters$minestatus = as.character(mine.quarters$minestatus)

# format date issued so that we can drop mines that were abandoned before our study period
datevars = names(mine.quarters)[grep("date", names(mine.quarters))]
for (i in 1:length(datevars)) {
  mine.quarters[, datevars[i]] = as.Date(as.character(mine.quarters[, datevars[i]]), "%m/%d/%Y")
}
mine.quarters$statusyear = as.yearqtr(mine.quarters$minestatusdate)
mine.quarters$statusyear = as.numeric(format(mine.quarters$statusyear, "%Y"))
mine.quarters$statusquarter = as.yearqtr(mine.quarters$minestatusdate)

################################################################################

# SELECT SAMPLE

# drop data from environments not of interest
# (facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16)
  # 50993 rows; 64 columns; unique on mine-year-quarter
mine.quarters = subset(mine.quarters, minetype == "Underground")
mine.quarters = subset(mine.quarters, coalcormetalmmine == "C")
mine.quarters = mine.quarters[order(mine.quarters$mineid, mine.quarters$year, mine.quarters$quarter),]

# drop mines that were abandoned before our study period 
  # 38653 rows; 66 columns; unique on mine-year-quarter
mine.quarters$too_early = ifelse((mine.quarters$minestatus == "Abandoned" | 
                                  mine.quarters$minestatus == "Abandoned and Sealed" |
                                  mine.quarters$minestatus == "NonProducing") & 
                                  mine.quarters$statusyear < 2000, 1, 0)
mine.quarters = mine.quarters[mine.quarters$too_early == 0, ]
mine.quarters$too_early = NULL

# There are some mines that didn't merge on any hours/employment data. We confirmed using the Mine Data 
# retrieval System that there is really no employment information on these mines. This is usually because
# they were abandoned and sealed during the first or second quarter of our study period, so no hours
# data was ever recorded.
  # 38295 rows; 66 columns; unique on mine-year-quarter
mine.quarters = mine.quarters[!is.na(mine.quarters$hours_qtr),]

# format date variables so quarter contains date-formatted year and quarter info (now observations will be unique on mineid-quarter)
mine.quarters$quarter = paste(mine.quarters$year, mine.quarters$quarter, sep = "-")
mine.quarters$quarter = as.yearqtr(mine.quarters$quarter)

# drop observations from after 2016 Q2
  # 37925 rows; 66 columns; unique on mine-year-quarter
mine.quarters = mine.quarters[which(mine.quarters$quarter <= "2016 Q1"),]

# drop observations for mines that are abandoned or sealed when their status date comes before the current quarter
  # 36940 rows; 67 columns; unique on mine-year-quarter
mine.quarters$drop = ifelse((mine.quarters$minestatus == "Abandoned" | 
                             mine.quarters$minestatus == "Abandoned and Sealed") &
                            (mine.quarters$statusquarter <= mine.quarters$quarter), 1, 0)
mine.quarters = mine.quarters[mine.quarters$drop == 0, ]

# edit minestatus based on minestatusdate and quarter. for a given mine-quarter:
  # if the minestatus date is LESS than the current quarter then the observation should take on that minestatus
  # the minestatus date is GREATER than the current quarter AND the minestatus is abandoned, 
  # then the observation should take on some other minestatus
mine.quarters$minestatus = ifelse((mine.quarters$statusquarter >= mine.quarters$quarter) 
                                & (mine.quarters$minestatus == "Abandoned" | 
                                   mine.quarters$minestatus == "Abandoned and Sealed" | 
                                   mine.quarters$minestatus == "Temporarily Idled" | 
                                   mine.quarters$minestatus == "NonProducing" ), "Unknown", mine.quarters$minestatus)

# remove mine-quarters with zero hours
  # 30289 rows; 67 columns; unique on mine-year-quarter
mine.quarters = mine.quarters[( mine.quarters$hours_qtr != 0), ]

# generate central appalachia indicator
mine.quarters$appalachia = ifelse((mine.quarters$stateabbreviation == "VA" |
                                   mine.quarters$stateabbreviation == "WV" |
                                   mine.quarters$stateabbreviation == "KY" |
                                   mine.quarters$stateabbreviation == "PA"), 1, 0)

# clean up safetycommittee
mine.quarters$safetycommittee = ifelse(mine.quarters$safetycommittee  == "Y", 1, 0)

################################################################################

# COLLAPSE DATA TO THE MINE-YEAR LEVEL AND CLEAN

# preserve variables that don't need to be summed in the collapse (1582 unique mines)
temp = mine.quarters[, c("appalachia", "district", "mineid", "safetycommittee")]
temp = unique(temp)

# generate a marker for each quarter so we can sum and count number of quarters 
# for which we have data in each mine-year
mine.quarters$num_quarts = 1

# collapse data to mine-year level to make sure nothing weird has happened
  # 9023 rows; 9 columns; unique on mine-year-quarter
mine.years = ddply(mine.quarters[, c("hours_qtr", "employment_qtr", "num_quarts", 
                                     "prod_qtr", "mineid", "year")], c("mineid", "year"), 
                       function(x) colSums(x[, c(match("hours_qtr", names(x)), 
                                                 match("num_quarts", names(x)), 
                                                 match("employment_qtr", names(x)), 
                                                 match("prod_qtr", names(x)))], na.rm = TRUE))
mine.years = merge(mine.years, temp, by = c("mineid"), all = T)
rm(temp)

# remove mine-years that are missing any quarters worth of data
  # 6253 rows; 9 columns; unique on mine-year
mine.years = mine.years[which(mine.years$num_quarts == 4),]

# rename vars that are no longer quarterly
names(mine.years)[names(mine.years) == "hours_qtr"] = "hours"
names(mine.years)[names(mine.years) == "employment_qtr"] = "employment"
names(mine.years)[names(mine.years) == "prod_qtr"] = "prod"

################################################################################

# MERGE IN PS AND MR DATA

# load in datasets
  # 6597 rows (each); 4 columns (each); unique on mineid-year
MR = readRDS(MR.in.file.name)
PS = readRDS(PS.in.file.name)

# merge in MR
mine.years = merge(mine.years, MR, by = c("mineid", "year"), all = TRUE)

# merge in PS (we don't need total_injuries again)
mine.years = merge(mine.years, PS[, c("mineid", "year", "PS")], by = c("mineid", "year"), all = TRUE)

# this just drops all non-merging observations
  # 6253 rows; 13 columns; unique on mineid-year
mine.years = mine.years[!is.na(mine.years$appalachia), ]

# replace all NA's in PS, MR, and total_injuries with 0's
mine.years$MR = ifelse(is.na(mine.years$MR), 0, mine.years$MR)
mine.years$PS = ifelse(is.na(mine.years$PS), 0, mine.years$PS)
mine.years$total_injuries = ifelse(is.na(mine.years$total_injuries), 0, mine.years$total_injuries)

################################################################################

# output mine-year data
  # 6253 rows; 13 columns; unique on mineid-year
saveRDS(mine.years, file = mine.years.out.file.name)

################################################################################

rm(list = ls())

################################################################################
