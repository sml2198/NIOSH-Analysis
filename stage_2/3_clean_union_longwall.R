# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 5 - Clean Union/Longwall Data
# Merges cleaned mines and cleaned employment data and collapses
# Then merges collapsed accidents data

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

################################################################################

library(plyr)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL) 
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # original eia data (covers 1999-2013)
employment.in.file.name = paste0(originals.path, "/EIA-data/Book1.csv", collapse = NULL)

  # original longwall data 1: 1992-2008 (from Chris Mark at NIOSH to Nate Atkinson and Brian Karfunkel on 6/20/11)
longwall.1.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Chris Mark/longwallID1992_2008.csv", collapse = NULL)
  # original longwall data 2: 2009 (from Deno Pappas at NIOSH to Nate Atkinson and Brian Karfunkel on 12/13/11)
longwall.2.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Deno Pappas/LW2009.csv", collapse = NULL)
  # original longwall data 3: 2010 (from Deno Pappas at NIOSH to Nate Atkinson and Brian Karfunkel on 12/13/11)
longwall.3.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Deno Pappas/LW2010.csv", collapse = NULL)
  # original longwall data 4: 2011 (from Deno Pappas at NIOSH to Kristen Altenburger and Ted Westling on 10/26/12)
longwall.4.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Deno Pappas/LW2011.csv", collapse = NULL)
  # original longwall data 5: 2012 (from Deno Pappas at NIOSH to Ted Westling on 9/8/13)
longwall.5.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Deno Pappas/LW2012.csv", collapse = NULL)
  # original longwall data 6: 2013 (from  Linda McWilliams on  10/17/16)
longwall.6.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Linda McWilliams/LW2013.csv", collapse = NULL)
  # original longwall data 7: 2014 (from  Linda McWilliams on  10/17/16)
longwall.7.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Linda McWilliams/LW2014.csv", collapse = NULL)
  # original longwall data 8: 2015 (from  Linda McWilliams on  10/17/16)
longwall.8.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Linda McWilliams/LW2015.csv", collapse = NULL)

# output
  # clean data with mine-year specific union and longwall information
ulw.out.file.name = "X:/Projects/Mining/NIOSH/analysis/data/2_cleaned/clean_union_longwall.rds"

# create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(clean.path, recursive = TRUE)

################################################################################


# BRING IN EIA DATA USED FOR UNDERREPORTING (LATEST DATA PULL FROM JOE CONKLIN)

# Import
eia = read.csv(eia_5_in_file_name, header = T, na.strings = c("","NA"))

# Clean var names
names(eia)[names(eia) == "MSHA_ID"] = "mineid"
names(eia) = tolower(names(eia))
eia = eia[, c(match("mineid", names(eia)),
              match("year", names(eia)),
              match("mine_type_code", names(eia)),
              match("company_type", names(eia)),
              match("operation_type", names(eia)),
              match("longwall_pct", names(eia)),
              match("union_id", names(eia)))]

# Format mineid
eia$mineid = str_pad(eia$mineid, 7, pad = "0")

# Drop years before study period
eia = eia[eia$year > 1999, ]

# Rename company-type categories
eia$company_type = ifelse(eia$mine_type == 1, "independent", eia$mine_type)
eia$company_type = ifelse(eia$mine_type == 2, "subsidiary", eia$mine_type)
eia$company_type = ifelse(eia$mine_type == 3, "contractor", eia$mine_type)

# Rename mine_type categories
eia$mine_type = ifelse(eia$mine_type == 0, "prep plant", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 1, "underground", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 2, "strip", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 4, "auger", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 6, "strip/auger combo ", eia$mine_type)
eia$mine_type = ifelse(eia$mine_type == 8, "refuse", eia$mine_type)

# Rename operation_type categories
eia$operation_type = ifelse(eia$operation_type == 1, "mine", eia$operation_type)
eia$operation_type = ifelse(eia$operation_type == 2, "prep plant", eia$operation_type)
eia$operation_type = ifelse(eia$operation_type == 3, "mine and prep plant", eia$operation_type)

# Replace union ID with "None" with missing & create union indicator
eia$union_id = ifelse(is.na(eia$union_id), 0, eia$union_id)
eia$union = ifelse(eia$union_id != 0, 1, 0)

# Collapse to the mine_year level - longwall_pct is just because ddply needs two arguments
eia = ddply(eia[, c(match("union", names(eia)),
                    match("longwall_pct", names(eia)),
                    match("year", names(eia)),
                    match("mineid", names(eia)))], c("mineid", "year"), 
            function(x) colMeans(x[, c(match("union", names(x)),
                                       match("longwall_pct", names(x)))], na.rm = T))
eia = eia[order(eia$mineid, eia$year), c(-grep("longwall_pct", names(eia)))]

######################################################################################################

# BRING IN LONGWALL DATA

# Import, clean & keep only mineid, year, and longwall indicator for all three longwall sheets
longwall.1 = read.csv(longwall_1_in_file_name, header = T, na.strings = c("", "NA"))
names(longwall.1) = tolower(names(longwall.1))
# keep only years after 2000
longwall.1 = longwall.1[longwall.1$year > 1999, ]
names(longwall.1)[names(longwall.1) == "lw.1"] = "longwall"
longwall.1 = longwall.1[, c("mineid", "year", "longwall")]

# two
longwall.2 = read.csv(longwall_2_in_file_name, header = T, na.strings = c("", "NA"))
names(longwall.2) = tolower(names(longwall.2))
names(longwall.2)[names(longwall.2) == "lw"] = "longwall"
longwall.2 = longwall.2[which(!is.na(longwall.2$year)), c("mineid", "year", "longwall")]

# three
longwall.3 = read.csv(longwall_3_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.3) = tolower(names(longwall.3))
# create longwall indicator - everything in this sheet is a longwall mine
longwall.3$longwall = 1
longwall.3 = longwall.3[which(!is.na(longwall.3$year)), c("mineid", "year", "longwall")]

# four
longwall.4 = read.csv(longwall_4_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.4) = tolower(names(longwall.4))
names(longwall.4)[names(longwall.4) == "lw"] = "longwall"
longwall.4 = longwall.4[, c("mineid", "year", "longwall")]

# five
longwall.5 = read.csv(longwall_5_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.5) = tolower(names(longwall.5))
names(longwall.5)[names(longwall.5) == "lw"] = "longwall"
longwall.5 = longwall.5[, c("mineid", "year", "longwall")]

# six
longwall.6 = read.csv(longwall_6_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.6) = tolower(names(longwall.6))
names(longwall.6)[names(longwall.6) == "lw"] = "longwall"
longwall.6 = longwall.6[, c("mineid", "year", "longwall")]
longwall.6 = longwall.6[which(!is.na(longwall.6$mineid) & !(is.na(longwall.6$year))), ]

# seven
longwall.7 = read.csv(longwall_7_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.7) = tolower(names(longwall.7))
names(longwall.7)[names(longwall.7) == "lw"] = "longwall"
longwall.7 = longwall.7[, c("mineid", "year", "longwall")]
longwall.7 = longwall.7[which(!is.na(longwall.7$mineid) & !(is.na(longwall.7$year))), ]

# eight
longwall.8 = read.csv(longwall_8_in_file_name,header = T, na.strings = c("", "NA"))
names(longwall.8) = tolower(names(longwall.8))
names(longwall.8)[names(longwall.8) == "lw"] = "longwall"
longwall.8 = longwall.8[, c("mineid", "year", "longwall")]
longwall.8 = longwall.8[which(!is.na(longwall.8$mineid)), ]

# append longwall datasets
longwall = rbind(longwall.1, longwall.2, longwall.3, longwall.4, longwall.5, longwall.6, longwall.7, longwall.8)
longwall = longwall[which(!is.na(longwall$mineid) & !is.na(longwall$year)), ] # should be 0
rm(longwall.1, longwall.2, longwall.3, longwall.4, longwall.5, longwall.6, longwall.7, longwall.8)

# format mineid
longwall$mineid = str_pad(longwall$mineid, 7, pad = "0")

# append eia and longwall data
eia = merge(eia, longwall, by = c("mineid", "year"), all = T)

# replace longwall with zero if it's not a 1 and the year is one for which we have data (2000-2015)
eia$longwall = ifelse(is.na(eia$longwall) & eia$year < 2016, 0, eia$longwall)

# replace union with zero if it's not a 1 and the year is one for which we have data (2000-2013)
eia$union = ifelse(is.na(eia$union) & eia$year < 2014, 0, eia$union)

################################################################################

# save
saveRDS(eia, ulw.out.file.name)

################################################################################

rm(list = ls())

################################################################################
