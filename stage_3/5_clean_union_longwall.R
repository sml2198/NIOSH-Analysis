# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 5 - OPTIONAL Clean Union/Longwall Data
  # Cleans union and longwall indicators from NIOSH and EIA datasets

### NOTE ###
  # Only run this file if you have access to BOTH the "EIA-data" and "NIOSH-data"
  # data sub-folders, and intend to run the union/longwall specification test.

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/26/17

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.path = paste0(root, "/0_originals", collapse = NULL) 
clean.path = paste0(root, "/1_cleaned", collapse = NULL) 

# inputs
  # original union data (covers 1999-2013)
union.in.file.name = paste0(originals.path, "/EIA-data/Book1.csv", collapse = NULL)

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

# BRING IN UNION DATA FROM EIA

# load data
  # 26338 rows; 100 columns; unique on mineid-year
union = read.csv(union.in.file.name, header = T, na.strings = c("","NA"))

# clean variable names and drop unnecessary vars
names(union)[names(union) == "MSHA_ID"] = "mineid"
names(union) = tolower(names(union))
union = union[, c(match("mineid", names(union)),
              match("year", names(union)),
              match("mine_type_code", names(union)),
              match("company_type", names(union)),
              match("operation_type", names(union)),
              match("longwall_pct", names(union)),
              match("union_id", names(union)))]

# format mineid
union$mineid = str_pad(union$mineid, 7, pad = "0")

# drop years before study period
  # 24321 rows; 7 columns; unique on mineid-year
union = union[union$year > 1999, ]

# replace union ID with "None" with missing & create union indicator
union$union_id = ifelse(is.na(union$union_id), 0, union$union_id)
union$union = ifelse(union$union_id != 0, 1, 0)

######################################################################################################

# BRING IN LONGWALL DATA

# load first longwall data set
  # 899 rows; 11 columns; unique on mineid-year
longwall.1 = read.csv(longwall.1.in.file.name, header = T, na.strings = c("", "NA"))
names(longwall.1) = tolower(names(longwall.1))

# keep only years after 2000
  # 417 rows; 3 columns; unique on mineid-year
longwall.1 = longwall.1[longwall.1$year > 1999, ]
names(longwall.1)[names(longwall.1) == "lw.1"] = "longwall"
longwall.1 = longwall.1[, c("mineid", "year", "longwall")]

# load second longwall data set
  # 43 rows; 15 columns; unique on mineid-year
longwall.2 = read.csv(longwall.2.in.file.name, header = T, na.strings = c("", "NA"))
names(longwall.2) = tolower(names(longwall.2))
names(longwall.2)[names(longwall.2) == "lw"] = "longwall"
  
# drop one empty row and unnecessary vars
  # 42 rows; 3 columns; unique on mineid-year
longwall.2 = longwall.2[which(!is.na(longwall.2$year)), c("mineid", "year", "longwall")]

# load third longwall data set
  # 46 rows; 31 columns; unique on mineid-year
longwall.3 = read.csv(longwall.3.in.file.name,header = T, na.strings = c("", "NA"))
names(longwall.3) = tolower(names(longwall.3))

# create longwall indicator - everything in this sheet is a longwall mine
longwall.3$longwall = 1
longwall.3 = longwall.3[which(!is.na(longwall.3$year)), c("mineid", "year", "longwall")]

# load fourth longwall data set
  # 40 rows; 5 columns; unique on mineid-year
longwall.4 = read.csv(longwall.4.in.file.name,header = T, na.strings = c("", "NA"))
names(longwall.4) = tolower(names(longwall.4))
names(longwall.4)[names(longwall.4) == "lw"] = "longwall"
longwall.4 = longwall.4[, c("mineid", "year", "longwall")]

# load fifth longwall data set
  # 42 rows; 5 columns; unique on mineid-year
longwall.5 = read.csv(longwall.5.in.file.name,header = T, na.strings = c("", "NA"))
names(longwall.5) = tolower(names(longwall.5))
names(longwall.5)[names(longwall.5) == "lw"] = "longwall"
longwall.5 = longwall.5[, c("mineid", "year", "longwall")]

# load sixth longwall data set
  # 44 rows; 19 columns; unique on mineid-year
longwall.6 = read.csv(longwall.6.in.file.name,header = T, na.strings = c("", "NA"))
names(longwall.6) = tolower(names(longwall.6))
names(longwall.6)[names(longwall.6) == "lw"] = "longwall"
longwall.6 = longwall.6[, c("mineid", "year", "longwall")]
longwall.6 = longwall.6[which(!is.na(longwall.6$mineid) & !(is.na(longwall.6$year))), ]

# load seventh longwall data set
  # 42 rows; 19 columns; unique on mineid-year
longwall.7 = read.csv(longwall.7.in.file.name,header = T, na.strings = c("", "NA"))
names(longwall.7) = tolower(names(longwall.7))
names(longwall.7)[names(longwall.7) == "lw"] = "longwall"
longwall.7 = longwall.7[, c("mineid", "year", "longwall")]
longwall.7 = longwall.7[which(!is.na(longwall.7$mineid) & !(is.na(longwall.7$year))), ]

# load eighth longwall data set
  # 40 rows; 19 columns; unique on mineid-year
longwall.8 = read.csv(longwall.8.in.file.name,header = T, na.strings = c("", "NA"))
names(longwall.8) = tolower(names(longwall.8))
names(longwall.8)[names(longwall.8) == "lw"] = "longwall"
longwall.8 = longwall.8[, c("mineid", "year", "longwall")]
longwall.8 = longwall.8[which(!is.na(longwall.8$mineid)), ]

# append longwall datasets
  # 708 rows; 3 columns; unique on mineid-year
longwall = rbind(longwall.1, longwall.2, longwall.3, longwall.4, longwall.5, longwall.6, longwall.7, longwall.8)
rm(longwall.1, longwall.2, longwall.3, longwall.4, longwall.5, longwall.6, longwall.7, longwall.8)

# format mineid
longwall$mineid = str_pad(longwall$mineid, 7, pad = "0")

# append union and longwall data
 # 24403 rows; 4 columns; unique on mineid-year
union.longwall = merge(union[,c("mineid", "year", "union")], longwall, by = c("mineid", "year"), all = T)

# replace longwall with zero if it's not a 1 and the year is one for which we have data (2000-2015)
union.longwall$longwall = ifelse(is.na(union.longwall$longwall) & union.longwall$year < 2016, 0, union.longwall$longwall)

# replace union with zero if it's not a 1 and the year is one for which we have data (2000-2013)
union.longwall$union = ifelse(is.na(union.longwall$union) & union.longwall$year < 2014, 0, union.longwall$union)

################################################################################

# save
  # 24403 rows; 4 columns; unique on mineid-year
saveRDS(union.longwall, ulw.out.file.name)

################################################################################

rm(list = ls())

################################################################################
