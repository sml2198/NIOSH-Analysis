# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Merge Mines Data
  # Merges cleaned mines and cleaned employment data

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/4/17

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

# There are some mines that didn't merge on any hours/employment data. We confirmed using the Mine Data 
# retrieval System that there is really no employment information on these mines. We drop them here. Now 95,109 obs
mine.quarters = mine.quarters[!is.na(mine.quarters$hours_qtr),]

# format date variables so quarter contains date-formatted year and quarter info (now observations will be unique on mineid-quarter)
mine.quarters$quarter = paste(mine.quarters$year, mine.quarters$quarter, sep = "-")
mine.quarters$quarter = ifelse(mine.quarters$quarter == "NA-NA", NA, mine.quarters$quarter)
mine.quarters$quarter = as.yearqtr(mine.quarters$quarter)

# re-name employment for ease of use
names(mine.quarters)[names(mine.quarters) == "avg_employee_cnt_qtr"] = "avg_employee_qtr"

# output mines data
saveRDS(mine.quarters, file = merged.mines.quarters.out.file.name)

################################################################################


# HERE WE DROP OBSERVATIONS THAT DON'T BELONG 

# now have 38,295 obs unique on mineid-quarter, 1895 unique mines, 66 vars

# drop observations for mines that are abandoned or sealed when their status date comes before the current quarter - now 37,297 obs, 1855 unique mineids
mines_quarters$minestatus = as.character(mines_quarters$minestatus)
mines_quarters$drop = ifelse((mines_quarters$minestatus == "Abandoned" | mines_quarters$minestatus == "Abandoned and Sealed") &
                               (mines_quarters$statusquarter <= mines_quarters$quarter), 1, 0)
mines_quarters = mines_quarters[mines_quarters$drop == 0, ]

# edit mine status date:
# if for a given mine-quarter the minestatus date is LESS than the quarter then the observation should take on that minestatus
# if for a given mine-quarter the minestatus date is GREATER than that quarter AND the minestatus is abandoned, then the observation should take on some other minestatus
mines_quarters$minestatus = ifelse((mines_quarters$statusquarter >= mines_quarters$quarter) 
                                   & (mines_quarters$minestatus == "Abandoned" | mines_quarters$minestatus == "Abandoned and Sealed" | 
                                        mines_quarters$minestatus == "Temporarily Idled" | mines_quarters$minestatus == "NonProducing" ), "Unknown", mines_quarters$minestatus)

# remove mine-quarters with minestatus "Temporarily Idled" and zero hours/employment/production (these are nonproducing) - wind up with 36,916 obs, 67 vars, 1850 unique mineids
mines_quarters = mines_quarters[(mines_quarters$minestatus != "Temporarily Idled" | mines_quarters$hours_qtr != 0), ]

# drop unnecessary variables - wind up with 36,916 obs, 64 vars
mines_quarters$statusyear = 
  mines_quarters$statusquarter = 
  mines_quarters$drop = NULL

######################################################################################################

# COLLAPSE MERGED DATA TO THE MINE-QUARTER LEVEL

# collapse data to mine-quarter level to make sure nothing weird has happened
# we wind up with 36,916 obs which is the same as before, as it should be, wahoo!
temp = mines_quarters[, c("mineid", 
                          "year",
                          "quarter",
                          "district",
                          "minetype",
                          "minename",
                          "minestatus",
                          "minestatusdate",
                          "coalcormetalmmine",
                          "stateabbreviation",
                          "safetycommittee",
                          "idate",
                          "idesc",
                          "daysperweek",
                          "productionshiftsperday")]

mines_quarters = ddply(mines_quarters[, c("hours_qtr",
                                          "employment_qtr", 
                                          "coal_prod_qtr", 
                                          "mineid", 
                                          "year", 
                                          "quarter")], c("mineid", "quarter"), 
                       function(x) colMeans(x[, c(match("hours_qtr", names(x)), 
                                                  match("employment_qtr", names(x)), 
                                                  match("coal_prod_qtr", names(x)))], na.rm = TRUE))
mines_quarters = merge(mines_quarters, temp, by = c("mineid", "quarter"), all = T)
rm(temp)

######################################################################################################

# drop observations with no hours - wind up with 30,551 obs unique on mineid-quarter, 18 vars, 1583 unique mines
mines_quarters = mines_quarters[(mines_quarters$hours_qtr != 0), ]

# output mine-level data
saveRDS(mines_quarters, file = mines_quarters_file_name)

################################################################################

rm(list = ls())

################################################################################
