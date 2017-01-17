# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 5 - Prepare Mines Data
  # Merges cleaned mines and cleaned employment data and collapses
  # Then merges collapsed accidents data

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library(plyr)
library(stringr)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 
collapsed.path = paste0(root, "/4_collapsed", collapse = NULL) 
prepped.path = paste0(root, "/5_prepped", collapse = NULL) 

# inputs
  # cleaned employment data
employment.in.file.name = paste0(clean.path, "/clean_employment.rds", collapse = NULL)
  # cleaned operator history data
history.in.file.name = paste0(clean.path, "/clean_operator_history.rds", collapse = NULL)
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



################################################################################

# MERGE IN OPERATOR HISTORY DATA

# load in operator history data
  # 63143 rows; 4 columns; unique on operatorid-operatorstartdt
history = readRDS(history.in.file.name)

# remove duplicates and order history
  # 55776 rows; 4 columns; unique on operatorid-operatorstartdt
history = history[order(history$mineid),]
history$operatorid = as.character(history$operatorid)
history = history[!duplicated(history), ]

# create empty operatorid and oeprator time variables in mines data
mine.quarters$operatorid = NA
mine.quarters$operatortime = NA

# fill in operator id for all mine-quarters based on operator start/end dates from history
fill.in.mines = function(t.mines) {
  mine = unique(t.mines$mineid)[1]
  t.history = history[history$mineid == mine, ]
  if (nrow(t.history) != 0) {
    t.history.o = t.history[, c("operatorid", "operatorstartdt", "operatorenddt")]
    t.history.o = t.history.o[!duplicated(t.history.o), ]
    t.history.o = t.history.o[order(t.history.o$operatorstartdt), ]
    for (i in 1:nrow(t.mines)) {
      for (j in 1:nrow(t.history.o)) {
        t.mines$operatorid[i] = 
          ifelse(t.mines$quarter[i] >= t.history.o$operatorstartdt[j] 
                 & t.mines$quarter[i] <= t.history.o$operatorenddt[j], 
                 t.history.o$operatorid[j], t.mines$operatorid[i])
      }
    }
  }
  return(t.mines)
}
mine.quarters1 = ddply(mine.quarters, "mineid", fill.in.mines)

# fill in missing mine-quarters (those for which we have no data) using bounds from history file
fill.in.ts = function(mine.df) {
  times = data.frame(quarter = seq(min(mine.df$quarter), max(mine.df$quarter), by = 0.25))
  full.ts = merge(times, mine.df, by = c("quarter"), all.x = TRUE)
  full.ts$mineid[is.na(full.ts$mineid)] = unique(full.ts$mineid)[1]
  return(full.ts)
}
mine.quarters1 = ddply(mine.quarters, "mineid", fill.in.ts)

# calculate operator time for each mine-quarter
make.op.time = function(mine.data) {
  for (i in 1:nrow(mine.data)) {
    if (is.na(mine.data$operatorid[i])) {
      mine.data$operatortime[i] = NA
    }
    else { 
      if (i <= 4) {
        temp = mine.data[1:(i - 1), "operatortime"]
        if (length(temp[!is.na(temp)]) == 0) {
          mine.data$operatortime[i] = 1
        }
        else {
          mine.data$operatortime[i] = temp[!is.na(temp)][length(temp[!is.na(temp)])] + 1
        }
      }
      else {
        if (sum(is.na(mine.data[(i - 4):(i - 1), "operatorid"])) == 4) {
          mine.data$operatortime[i] = NA
        }
        else {
          temp = mine.data[(i - 4):(i - 1), "operatortime"]
          if (length(temp[!is.na(temp)]) == 0) {
            mine.data$operatortime[i] = 1
          }
          else {
            mine.data$operatortime[i] = temp[!is.na(temp)][length(temp[!is.na(temp)])] + 1
          }
        }
      }
    }
  }
  return(mine.data)
}
mine.quarters = ddply(mine.quarters, "mineid", make.op.time)
mine.quarters = mine.quarters[mine.quarters$quarter >= 2000, ]

# there is one mine-quarter for which we don't have information on the operator (we've spot
# checked this mine - 4406239 - in the history file). Here we force operatimetime to be 0.
mine.quarters$operatortime = ifelse(is.na(mine.quarters$operatortime), 0, mine.quarters$operatortime)

################################################################################

# COLLAPSE DATA TO THE MINE-YEAR LEVEL AND CLEAN

# preserve variables that don't need to be summed in the collapse (1582 unique mines)
temp = mine.quarters[, c("appalachia", "district", "mineid", "safetycommittee")]
temp = unique(temp)

# grab operatortime in the first quarter of the year (this is what we'll use)
# we'll merge this back on in a hot second
time = mine.quarters[, c("mineid", "year", "quarter", "operatortime")]
time$q = ifelse(grepl("Q1$", time$quarter), 1, 0)
time = time[which(time$q == 1),]

# generate a marker for each quarter so we can sum and count number of quarters 
# for which we have data in each mine-year
mine.quarters$numquarters = 1

# collapse data to mine-year level to make sure nothing weird has happened
  # 9023 rows; 9 columns; unique on mine-year-quarter
mine.years = ddply(mine.quarters[, c("hours_qtr", "employment_qtr", "numquarters", 
                                     "prod_qtr", "mineid", "year")], c("mineid", "year"), 
                       function(x) colSums(x[, c(match("hours_qtr", names(x)), 
                                                 match("numquarters", names(x)), 
                                                 match("employment_qtr", names(x)), 
                                                 match("prod_qtr", names(x)))], na.rm = TRUE))
mine.years = merge(mine.years, temp, by = c("mineid"), all = T)
rm(temp)

# remove mine-years that are missing any quarters worth of data
  # 6253 rows; 9 columns; unique on mine-year
mine.years = mine.years[which(mine.years$numquarters == 4),]

# merge operator time back on to the now mine-year data
  # 6253 rows; 10 columns; unique on mine-year
mine.years = merge(mine.years, time[,c("mineid", "year", "operatortime")], by = c("mineid", "year"))
rm(time)

# rename vars that are no longer quarterly
names(mine.years)[names(mine.years) == "hours_qtr"] = "hours"
names(mine.years)[names(mine.years) == "employment_qtr"] = "employment"
names(mine.years)[names(mine.years) == "prod_qtr"] = "prod"

# final formatting to make generating figures easy
mine.years$district = as.numeric(mine.years$district)
mine.years$appalachia = as.numeric(mine.years$appalachia)

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
