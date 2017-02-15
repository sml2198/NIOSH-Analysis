# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 5 - OPTIONAL Clean Union/Longwall Data
  # Cleans union and longwall indicators from NIOSH and EIA datasets

# ONLY RUN THIS FILE IF
  # you have access to BOTH the "EIA-data" and "NIOSH-data" data sub-folders
  # you intend to run the union/longwall specification test

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com

# Last edit 2/8/2017

################################################################################

library(stringr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
originals.path = paste0(root, "/data/0_originals", collapse = NULL) 
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 

# inputs
  # union data (covers 1999-2013)
union.in.file.name = paste0(originals.path, "/EIA-data/Book1.csv", collapse = NULL)
  # longwall data 1 (1992-2008) 
    # from Chris Mark at NIOSH on 6/20/2011
longwall.1.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Chris Mark/longwallID1992_2008.csv", collapse = NULL)
  # longwall data 2 (2009)
    # from Deno Pappas at NIOSH on 12/13/2011
longwall.2.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Deno Pappas/LW2009.csv", collapse = NULL)
  # longwall data 3 (2010) 
    # from Deno Pappas at NIOSH on 12/13/2011
longwall.3.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Deno Pappas/LW2010.csv", collapse = NULL)
  # longwall data 4 (2011)
    # from Deno Pappas at NIOSH on 10/26/2012
longwall.4.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Deno Pappas/LW2011.csv", collapse = NULL)
  # longwall data 5 (2012) 
    # from Deno Pappas at NIOSH on 9/8/2013
longwall.5.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Deno Pappas/LW2012.csv", collapse = NULL)
  # longwall data 6 (2013)
    # from  Linda McWilliams at NIOSH on  10/17/2016
longwall.6.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Linda McWilliams/LW2013.csv", collapse = NULL)
  # longwall data 7 (2014) 
    # from  Linda McWilliams at NIOSH on  10/17/2016
longwall.7.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Linda McWilliams/LW2014.csv", collapse = NULL)
  # longwall data 8 (2015)
    # from  Linda McWilliams at NIOSH on  10/17/2016
longwall.8.in.file.name = paste0(originals.path, "/NIOSH-data/Data from Linda McWilliams/LW2015.csv", collapse = NULL)

# output
  # cleaned union and longwall data
ulw.out.file.name = paste0(cleaned.path, "/clean_union_longwall.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read union data
  # 26338 rows; 100 columns; unique on mshaid-year
union = read.csv(union.in.file.name, header = TRUE, na.strings = c("", "NA"))

# read longwall data 1
  # 899 rows; 11 columns; unique on mineid-year
longwall.1 = read.csv(longwall.1.in.file.name, header = TRUE, na.strings = c("", "NA"))

# read longwall data 2
  # 43 rows; 15 columns; unique on mineid-year
longwall.2 = read.csv(longwall.2.in.file.name, header = TRUE, na.strings = c("", "NA"))

# read longwall data 3
  # 46 rows; 31 columns
longwall.3 = read.csv(longwall.3.in.file.name, header = TRUE, na.strings = c("", "NA"))

# read longwall data 4
  # 40 rows; 5 columns; unique on mineid-year
longwall.4 = read.csv(longwall.4.in.file.name, header = TRUE, na.strings = c("", "NA"))

# read longwall data 5
  # 42 rows; 5 columns; unique on mineid-year
longwall.5 = read.csv(longwall.5.in.file.name, header = TRUE, na.strings = c("", "NA"))

# read longwall data 6
  # 44 rows; 19 columns; unique on mineid-year
longwall.6 = read.csv(longwall.6.in.file.name, header = TRUE, na.strings = c("", "NA"))

# read longwall data 7 
  # 42 rows; 19 columns; unique on mineid-year
longwall.7 = read.csv(longwall.7.in.file.name, header = TRUE, na.strings = c("", "NA"))

# read longwall data 8
  # 40 rows; 19 columns; unique on mineid-year
longwall.8 = read.csv(longwall.8.in.file.name, header = TRUE, na.strings = c("", "NA"))

# bye
rm(union.in.file.name, longwall.1.in.file.name, longwall.2.in.file.name,
   longwall.3.in.file.name, longwall.4.in.file.name, longwall.5.in.file.name, 
   longwall.6.in.file.name, longwall.7.in.file.name, longwall.8.in.file.name)

################################################################################

# EDIT UNION DATA

# rename variables
names(union)[names(union) == "MSHA_ID"] = "mineid"
names(union) = tolower(names(union))

# drop unnecessary variables
union = union[, c("company_type", "longwall_pct", "mineid", 
                  "mine_type_code", "operation_type", "union_id", 
                  "year")]

# format variables
union$mineid = str_pad(union$mineid, 7, pad = "0")
union$union_id = ifelse(is.na(union$union_id), 0, union$union_id)
union$union = ifelse(union$union_id != 0, 1, 0)

# drop data from outside study period
  # 24321 rows; 7 columns; unique on mineid-year
union = union[union$year > 1999, ]

# bye
union$union_id = NULL

################################################################################

# EDIT LONGWALL DATA

# rename variables
names(longwall.1) = tolower(names(longwall.1))
names(longwall.2) = tolower(names(longwall.2))
names(longwall.3) = tolower(names(longwall.3))
names(longwall.4) = tolower(names(longwall.4))
names(longwall.5) = tolower(names(longwall.5))
names(longwall.6) = tolower(names(longwall.6))
names(longwall.7) = tolower(names(longwall.7))
names(longwall.8) = tolower(names(longwall.8))
names(longwall.1)[names(longwall.1) == "lw.1"] = "longwall"
names(longwall.2)[names(longwall.2) == "lw"] = "longwall"
longwall.3$longwall = 1 # everything in this sheet is a longwall mine
names(longwall.4)[names(longwall.4) == "lw"] = "longwall"
names(longwall.5)[names(longwall.5) == "lw"] = "longwall"
names(longwall.6)[names(longwall.6) == "lw"] = "longwall"
names(longwall.7)[names(longwall.7) == "lw"] = "longwall"
names(longwall.8)[names(longwall.8) == "lw"] = "longwall"

# drop observations from time periods not of interest
  # 417 rows; 11 columns; unique on mineid-year
longwall.1 = longwall.1[longwall.1$year > 1999, ]

# drop missing rows and unnecessary variables
  # 417 rows; 3 columns; unique on mineid-year
longwall.1 = longwall.1[which(!is.na(longwall.1$mineid) & !(is.na(longwall.1$year))), c("mineid", "year", "longwall")]
  # 42 rows; 3 columns; unique on mineid-year
longwall.2 = longwall.2[which(!is.na(longwall.2$mineid) & !(is.na(longwall.2$year))), c("mineid", "year", "longwall")]
  # 42 rows; 3 columns; unique on mineid-year
longwall.3 = longwall.3[which(!is.na(longwall.3$mineid) & !(is.na(longwall.3$year))), c("mineid", "year", "longwall")]
  # 40 rows; 3 columns; unique on mineid-year
longwall.4 = longwall.4[which(!is.na(longwall.4$mineid) & !(is.na(longwall.4$year))), c("mineid", "year", "longwall")]
  # 42 rows; 3 columns; unique on mineid-year
longwall.5 = longwall.5[which(!is.na(longwall.5$mineid) & !(is.na(longwall.5$year))), c("mineid", "year", "longwall")]
  # 43 rows; 3 columns; unique on mineid-year
longwall.6 = longwall.6[which(!is.na(longwall.6$mineid) & !(is.na(longwall.6$year))), c("mineid", "year", "longwall")]
  # 42 rows; 3 columns; unique on mineid-year
longwall.7 = longwall.7[which(!is.na(longwall.7$mineid) & !(is.na(longwall.7$year))), c("mineid", "year", "longwall")]
  # 40 rows; 3 columns; unique on mineid-year
longwall.8 = longwall.8[which(!is.na(longwall.8$mineid) & !(is.na(longwall.8$year))), c("mineid", "year", "longwall")]

# append longwall datasets
  # 708 rows; 3 columns; unique on mineid-year
longwall = rbind(longwall.1, longwall.2, longwall.3, longwall.4, longwall.5, longwall.6, longwall.7, longwall.8)

# format variables
longwall$mineid = str_pad(longwall$mineid, 7, pad = "0")

# bye
rm(longwall.1, longwall.2, longwall.3, longwall.4, 
   longwall.5, longwall.6, longwall.7, longwall.8)

################################################################################

# APPEND UNION AND LONGWALL DATA

# append union and longwall data
  # 24403 rows; 4 columns; unique on mineid-year
union.longwall = merge(union[, c("mineid", "year", "union")], longwall, by = c("mineid", "year"), all = TRUE)

# replace longwall with 0 if it is not a 1 and the year is one for which we have data (2000-2015)
union.longwall$longwall = ifelse(is.na(union.longwall$longwall) & union.longwall$year < 2016, 0, union.longwall$longwall)

# replace union with 0 if it is not a 1 and the year is one for which we have data (2000-2013)
union.longwall$union = ifelse(is.na(union.longwall$union) & union.longwall$year < 2014, 0, union.longwall$union)

# bye
rm(union, longwall)

################################################################################

# OUTPUT DATA

# output cleaned union longwall data
  # 24403 rows; 4 columns; unique on mineid-year
saveRDS(union.longwall, ulw.out.file.name)

# bye
rm(list = ls())

################################################################################
