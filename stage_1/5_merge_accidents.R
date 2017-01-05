# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development of New Mine Safety Technologies and Technological Applications

# 5 - Merge Accidents Data
  # Merges cleaned accidents and cleaned mines/employment data

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
merged.path = paste0(root, "/2_merged", collapse = NULL)

# input 1: cleaned accidents data
accidents.in.file.name = paste0(clean.input.path, "/clean_accidents.rds", collapse = NULL)
# input 2: merged mines data data
mines.in.file.name = paste0(merged.path, "/merged_mines.rds", collapse = NULL)

# output 1: clean merged accidents/mines data
merged.accidents.out.file.name = paste0(merged.path, "/merged_accidents.rds", collapse = NULL)

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(clean.output.path, recursive = TRUE)

################################################################################

# READ DATASETS AND MAKE THEM COMPATIBLE FOR MERGE

# read accidents data
accidents = readRDS(accidents.in.file.name)

# drop data from times and environments not of interest
accidents = accidents[accidents$COAL_METAL_IND == "C", ]
accidents = accidents[accidents$SUBUNIT == "UNDERGROUND", ]

# format variables to facilitate merging
accidents$oldoccupationcode = ""
accidents$subunit = tolower(accidents$subunit)

# remove observations after Q1 2016
accidents$drop = ifelse((accidents$calendaryear == 2016 & accidents$calendarquarter > 1), 1, 0)
accidents = accidents[accidents$drop == 0,] 
accidents = accidents[,-match("drop", names(accidents))]

# read mines data
mine.quarters = readRDS(mines.in.file.name)

################################################################################

# MERGE MINES AND ACCIDENTS DATA, THEN OUTPUT

# collapse mines data to the mine level (no quarters required) - these are already mine-level vars
temp = mine.quarters[, c("mineid", 
                          "year",
                          "quarter",
                          "minetype",
                          "minename",
                          "minestatus",
                          "minestatusdate",
                          "operatorid",
                          "operatorname",
                          "coalcormetalmmine",
                          "stateabbreviation",
                          "idate",
                          "idesc",
                          "daysperweek",
                          "productionshiftsperday")]

# average variables that are quarter-specific 
vars.to.avrg = ddply(mine.quarters[, c("hours_qtr",
                                        "employment_qtr", 
                                        "coal_prod_qtr", 
                                        "mineid")], c("mineid"), 
                     function(x) colMeans(x[, c(match("hours_qtr", names(x)), 
                                                match("employment_qtr", names(x)), 
                                                match("coal_prod_qtr", names(x)))], na.rm = TRUE))

# rename quarter specific variables so we don't get confused 
names(vars.to.avrg)[names(vars.to.avrg) == "hours_qtr"] = "avg_hours_qtr"
names(vars.to.avrg)[names(vars.to.avrg) == "employment_qtr"] = "avg_employment_qtr"
names(vars.to.avrg)[names(vars.to.avrg) == "coal_prod_qtr"] = "avg_coal_prod_qtr"

# merge the collapsed data with mine-level data
mines = merge(vars.to.avrg, temp, by = "mineid")

# keep unique mine info
mines = mines[!duplicated(mines$mineid), ]

# now remove useless datasets
rm(mines_quarters, temp, vars.to.avrg)

# merge mines and accidents data
mines.accidents = merge(accidents, mines, by = "mineid", all = T)

# remove observations only from mines data
mines.accidents = mines.accidents[!is.na(mines.accidents$documentno),]

# keep mine-level information from mines data (.y)
mines.accidents = mines.accidents[, c(-grep("\\.x", names(mines.accidents)))]
names(mines.accidents) = gsub("\\.[x|y]", "", names(mines.accidents))

# output merged mines and accidents data
saveRDS(mines.accidents, file = merged.accidents.out.file.name)

################################################################################

rm(list = ls())

################################################################################
