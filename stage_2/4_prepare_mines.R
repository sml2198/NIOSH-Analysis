# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 4 - Prepare Mines Data
  # Merges cleaned mines and cleaned employment data and collapses
  # Then merges collapsed accidents data

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library(zoo)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
clean.input.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.output.path = paste0(root, "/2_merged", collapse = NULL) 

# inputs
  # cleaned employment data
employment.in.file.name = paste0(clean.input.path, "/clean_employment.rds", collapse = NULL)
  # cleaned mines data
mines.in.file.name = paste0(clean.input.path, "/clean_mines.rds", collapse = NULL)

# outputs
  # merged mines data
merged.mines.out.file.name = paste0(merged.output.path, "/merged_mines.rds", collapse = NULL)
  # merged mine-quarters data
merged.mines.quarters.out.file.name = paste0(merged.output.path, "/merged_mine_quarters.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(merged.output.path, recursive = TRUE)

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
  # 38259 rows; 66 columns; unique on mine-year-quarter
mine.quarters = mine.quarters[!is.na(mine.quarters$hours_qtr),]

# format date variables so quarter contains date-formatted year and quarter info (now observations will be unique on mineid-quarter)
mine.quarters$quarter = paste(mine.quarters$year, mine.quarters$quarter, sep = "-")
mine.quarters$quarter = as.yearqtr(mine.quarters$quarter)

# drop observations from after 2016 Q2
  # 94187 rows; 66 columns; unique on mine-year-quarter
mine.quarters = mine.quarters[which(mine.quarters$quarter <= "2016 Q1"),]

# drop observations for mines that are abandoned or sealed when their status date comes before the current quarter
  # 37297 rows; 67 columns; unique on mine-year-quarter
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

# remove mine-quarters with minestatus "Temporarily Idled" and zero hours/employment/production (these are nonproducing) 
  # 36916 rows; 67 columns; unique on mine-year-quarter
mine.quarters = mine.quarters[(mine.quarters$minestatus != "Temporarily Idled" | 
                               mine.quarters$hours_qtr != 0), ]

################################################################################

# drop unnecessary variables
mine.quarters$statusyear = 
  mine.quarters$statusquarter = 
  mine.quarters$drop = NULL

################################################################################

# COLLAPSE MERGED DATA TO THE MINE-YEAR LEVEL

# preserve variables that don't need to be summed in the collapse (1850 unique mines)
temp = mine.quarters[,c("stateabbreviation", "district", "mineid")]
temp = unique(temp)

# collapse data to mine-year level to make sure nothing weird has happened
# we wind up with 36,916 obs which is the same as before, as it should be, wahoo!
mine.quarters = ddply(mine.quarters[, c("hours_qtr",
                                        "employment_qtr", 
                                        "prod_qtr", 
                                        "mineid", 
                                        "year", 
                                        "quarter")], c("mineid", "quarter"), 
                       function(x) colMeans(x[, c(match("hours_qtr", names(x)), 
                                                  match("employment_qtr", names(x)), 
                                                  match("prod_qtr", names(x)))], na.rm = TRUE))

mine.quarters = merge(mine.quarters, temp, by = c("mineid"), all = T)
rm(temp)

################################################################################

# output mine-level data
saveRDS(mines.quarters, file = mines_quarters_file_name)

################################################################################

rm(list = ls())

################################################################################
