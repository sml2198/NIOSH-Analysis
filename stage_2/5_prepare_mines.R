# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 5 - Prepare Mines Data
  # Merges mines data (produced in 1_clean_mines) and employment data (produced in 
    # 2_clean_employment) to produce mine-quarter-level data
  # Collapses data to mine-year level
  # Uses operator history data (produced in 3_clean_operator_history) to create
    # operator time variable
  # Merges mine-year-level data and MR and PS accidents data (produced in 
    # 4_collapse_accidents)

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Julia Bodson, juliabodson@gmail.com

# Last edit 2/8/2017

################################################################################

library(zoo)
library(plyr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 
collapsed.path = paste0(root, "/data/4_collapsed", collapse = NULL) 
prepared.path = paste0(root, "/data/5_prepared", collapse = NULL) 

# inputs
  # cleaned mines data
    # produced in 1_clean_mines
mines.in.file.name = paste0(cleaned.path, "/clean_mines.rds", collapse = NULL)
  # cleaned employment/production data
    # produced in 2_clean_employment
employment.in.file.name = paste0(cleaned.path, "/clean_employment.rds", collapse = NULL)
  # cleaned operator history data
    # produced in 3_clean_operator_history
history.in.file.name = paste0(cleaned.path, "/clean_operator_history.rds", collapse = NULL)
  # collapsed MR accidents data
    # produced in 4_collapse_accidents
MR.in.file.name = paste0(collapsed.path, "/collapsed_MR_accidents.rds", collapse = NULL)
  # collapsed PS accidents data
    # produced in 4_collapse_accidents
PS.in.file.name = paste0(collapsed.path, "/collapsed_PS_accidents.rds", collapse = NULL)

# outputs
  # mine-year-level data
mine.years.out.file.name = paste0(prepared.path, "/prepared_mine_years.rds", collapse = NULL)

# create file paths 
dir.create(prepared.path, recursive = TRUE) # (recursive = TRUE will create this file structure if it does not exist)

# bye
rm(root, cleaned.path, collapsed.path, prepared.path)

################################################################################

# READ DATA

# read cleaned mines data
  # 86362 rows; 9 columns; unique on mineid
mines = readRDS(mines.in.file.name)

# read cleaned employment data
  # 42019 rows; 6 columns; unique on mineid-year-quarter
employment = readRDS(employment.in.file.name)

# read cleaned operator history data
  # 55770 rows; 4 columns; unique on minied-operatorid-operatorstartdt-operatorenddt
history = readRDS(history.in.file.name)

# read MR accidents data
  # 6597 rows; 4 columns; unique on mineid-year
MR = readRDS(MR.in.file.name)

# read PS accidents data
  # 6597 rows; 4 columns; unique on mineid-year
PS = readRDS(PS.in.file.name)

# bye
rm(mines.in.file.name, employment.in.file.name, history.in.file.name, MR.in.file.name, PS.in.file.name)

################################################################################

# MERGE MINES AND EMPLOYMENT DATA

# merge mines and employment data by mineid (produce mine-quarter data)
  # 126313 rows; 14 columns; unique on mineid-year-quarter
mine.quarters = merge(employment, mines, by = c("mineid"), all = TRUE)
mine.quarters = mine.quarters[order(mine.quarters$mineid, mine.quarters$year, mine.quarters$quarter), ]

# bye
rm(mines, employment)

################################################################################

# CLEAN DATA

# some mines did not merge on any employment data
  # Mine Data Retrieval System confirmed that there is no employment data on these mines
    # usually, this is because the mines were abandoned and sealed during the first or 
    # second quarter of the study period
  # 42019 rows; 14 columns; unique on mineid-year-quarter
mine.quarters = mine.quarters[!is.na(mine.quarters$hours_qtr), ]

# drop data for mine-year-quarters with no hours
  # 33262 rows; 14 columns; unique on mineid-year-quarter
mine.quarters = mine.quarters[( mine.quarters$hours_qtr != 0), ]

# drop data from environments not of interest
  # facility means a mill/processing location, always above ground, according to April Ramirez @ DOL on 6/6/16
  # 30847 rows; 14 columns; unique on mineid-year-quarter
mine.quarters = mine.quarters[which(mine.quarters$minetype == "Underground" &
                                      mine.quarters$coalcormetalmmine == "C"), ]

# format and generate date variables
mine.quarters$minestatus = as.character(mine.quarters$minestatus)
mine.quarters$minestatusdate = as.Date(as.character(mine.quarters$minestatusdate), "%m/%d/%Y")
mine.quarters$statusquarter = as.yearqtr(mine.quarters$minestatusdate)
mine.quarters$statusyear = as.numeric(format(mine.quarters$statusquarter, "%Y"))
mine.quarters$quarter = paste(mine.quarters$year, mine.quarters$quarter, sep = "-")
mine.quarters$quarter = as.yearqtr(mine.quarters$quarter) # now data is unique on mineid-quarter

# drop data from mines that are abandoned or sealed before study period
  # 30779 rows; 16 columns; unique on mineid-quarter
mine.quarters$drop = ifelse((mine.quarters$minestatus == "Abandoned" | 
                               mine.quarters$minestatus == "Abandoned and Sealed" |
                               mine.quarters$minestatus == "NonProducing") & 
                              mine.quarters$statusyear < 2000, 1, 0)
mine.quarters = mine.quarters[which(mine.quarters$drop == 0), ]
mine.quarters$drop = NULL

# drop data for mines that are abandoned or sealed when status date < current quarter
  # 30551 rows; 16 columns; unique on mineid-quarter
mine.quarters$drop = ifelse((mine.quarters$minestatus == "Abandoned" | 
                               mine.quarters$minestatus == "Abandoned and Sealed") &
                              (mine.quarters$statusquarter <= mine.quarters$quarter), 1, 0)
mine.quarters = mine.quarters[which(mine.quarters$drop == 0), ]
mine.quarters$drop = NULL

# drop observations from after 2016 Q2
  # 30289 rows; 16 columns; unique on mineid-quarter
mine.quarters = mine.quarters[which(mine.quarters$quarter <= "2016 Q1"), ]

# edit minestatus based on minestatusdate and quarter
  # for a given mine-quarter:
    # if minestatus date < current quarter, then mine-quarter takes on that minestatus
    # if minestatus date > current quarter AND the minestatus is abandoned, then mine-quarter takes on other minestatus
mine.quarters$minestatus = ifelse((mine.quarters$statusquarter >= mine.quarters$quarter) 
                                  & (mine.quarters$minestatus == "Abandoned" | 
                                       mine.quarters$minestatus == "Abandoned and Sealed" | 
                                       mine.quarters$minestatus == "Temporarily Idled" | 
                                       mine.quarters$minestatus == "NonProducing"), "Unknown", mine.quarters$minestatus)

################################################################################

# COLLAPSE DATA TO THE MINE-YEAR-LEVEL

# preserve variables that don't need to be summed in the collapse
  # 1582 rows; 4 columns; unique on mineid
temp = mine.quarters[, c("appalachia", "district", "mineid", "safetycommittee")]
temp = unique(temp)

# generate a marker for each quarter to count number of quarters for which we have data in each mine-year
mine.quarters$numquarters = 1

# collapse data to mine-year-level
  # 9023 rows; 9 columns; unique on mine-year
mine.years = ddply(mine.quarters[, c("employment_qtr", "hours_qtr", "mineid", 
                                     "numquarters", "prod_qtr", "year")], c("mineid", "year"), 
                   function(x) colSums(x[, c("employment_qtr", "hours_qtr", "numquarters", "prod_qtr")], na.rm = TRUE))
mine.years = merge(mine.years, temp, by = c("mineid"), all = TRUE)
rm(temp)

# drop mine-years that are missing any quarters of data
  # 6253 rows; 8 columns; unique on mine-year
mine.years = mine.years[which(mine.years$numquarters == 4), ]
mine.years$numquarters = NULL

# rename variables
names(mine.years)[names(mine.years) == "hours_qtr"] = "hours"
names(mine.years)[names(mine.years) == "employment_qtr"] = "employment"
names(mine.years)[names(mine.years) == "prod_qtr"] = "prod"

# format variables
mine.years$district = as.numeric(mine.years$district)
mine.years$appalachia = as.numeric(mine.years$appalachia)

# bye
rm(mine.quarters)

################################################################################

# CALCULATE OPERATOR TIME

# create operatorid and operatortime variables
mine.years$operatorid = NA
mine.years$operatortime = NA

# fill in operator id for all mine-quarters based on operator start/end dates from history
fill.in.operator = function(mine.data) {
  
  mine = unique(mine.data$mineid)[1]
  t.history = history[history$mineid == mine, ]
  
  if (nrow(t.history) != 0) {
    
    for (i in 1:nrow(mine.data)) {
      for (j in 1:nrow(t.history)) {
        
        mine.data$operatorid[i] = 
          ifelse(mine.data$year[i] >= t.history$operatorstartdt[j] 
                 & mine.data$year[i] <= t.history$operatorenddt[j], 
                 t.history$operatorid[j], mine.data$operatorid[i])
      }
    }
  }
  
  return(mine.data)
}
mine.years = ddply(mine.years, "mineid", fill.in.operator)

# fill in missing mine-quarters 
fill.in.ts = function(mine.data) {
  times = data.frame(year = seq(min(mine.data$year), max(mine.data$year), by = 1))
  full.ts = merge(times, mine.data, by = c("year"), all.x = TRUE)
  full.ts$mineid[is.na(full.ts$mineid)] = unique(full.ts$mineid)[1]
  return(full.ts)
}
  # 6705 rows; 10 columns; unique on mineid-year
mine.years = ddply(mine.years, "mineid", fill.in.ts)

# calculate operator time
make.op.time = function(mine.data) {
  
  mine = unique(mine.data$mineid)[1]
  t.history = history[history$mineid == mine, ]
  
  for (i in 1:nrow(mine.data)) {
    
    # operatorid missing
    if (is.na(mine.data$operatorid[i])) {
      mine.data$operatortime[i] = NA
    }
    
    # operatorid not missing
    else { 
      
      start.date = t.history[which(t.history$operatorid == mine.data$operatorid[i] &
                                     t.history$operatorstartdt <= mine.data$year[i] &
                                     t.history$operatorenddt >= mine.data$year[i]), "operatorstartdt"]
      
      # first year of history
      if (i == 1) {
        mine.data$operatortime[i] = mine.data$year[i] - start.date
      }
      
      # second+ year of history
      if (i > 1) {
        
        # missing data previous year
        if (is.na(mine.data$operatortime[i - 1])) {
          mine.data$operatortime[i] = mine.data$year[i] - start.date
        }
        
        # open and producing previous year
        else {
          
          # same operator
          if (mine.data$operatorid[i] == mine.data$operatorid[i - 1]) {
            mine.data$operatortime[i] = mine.data$operatortime[i - 1] + 1
          }
          
          # different operator
          else {
            mine.data$operatortime[i] = mine.data$year[i] - start.date
          }
        }
      }
    }
  }
  
  return(mine.data)
}

mine.years = ddply(mine.years, "mineid", make.op.time)

# drop "orphan" observations
  # 6253 rows; 10 columns; unique on mineid-year
mine.years = mine.years[!is.na(mine.years$operatorid), ]

# bye
rm(history, fill.in.operator, fill.in.ts, make.op.time)

################################################################################

# MERGE MINE-YEAR DATA WITH MR AND PS ACCIDENTS DATA

# merge mine-year-level data with MR accidents data
  # 7645 rows; 12 columns; unique on mineid-year
mine.years = merge(mine.years, MR, by = c("mineid", "year"), all = TRUE)

# merge mine-year-level data with PS accidents data
  # 7645 rows; 13 columns; unique on mineid-year
mine.years = merge(mine.years, PS[, c("mineid", "year", "PS")], by = c("mineid", "year"), all = TRUE)

# drop non-merging observations
  # 6253 rows; 13 columns; unique on mineid-year
mine.years = mine.years[!is.na(mine.years$appalachia), ]

# replace NAs in PS, MR, and total_injuries with 0
mine.years$MR = ifelse(is.na(mine.years$MR), 0, mine.years$MR)
mine.years$PS = ifelse(is.na(mine.years$PS), 0, mine.years$PS)
mine.years$total_injuries = ifelse(is.na(mine.years$total_injuries), 0, mine.years$total_injuries)

# bye 
rm(MR, PS)

################################################################################

# OUTPUT DATA

# output mine-year-level data
  # 6253 rows; 13 columns; unique on mineid-year
saveRDS(mine.years, file = mine.years.out.file.name)

# bye
#rm(list = ls())

################################################################################
