# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Clean PS (Pinning and Striking) Training/Testing Set
  # Cleans master PS dataset coded by NIOSH

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 1/23/2017

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
originals.path = paste0(root, "/data/0_originals", collapse = NULL)
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 

# inputs
  # master PS dataset
    # coded by NIOSH representatives and sent to the Morantz team on 1/29/2016
master.data.file.name = paste0(originals.path, "/training-sets/Training_Set_Pinning_And_Striking_Accidents-January-29-2016.csv", collapse = NULL)

# outputs
  # cleaned PS training/testing set
train.test.set.out.file.name = paste0(cleaned.path, "/clean_PS_train_test_set.rds", collapse = NULL)

# generate file paths
dir.create(cleaned.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, originals.path, cleaned.path)

################################################################################

# READ DATA

# read master PS dataset
  # 1002 rows; 104 columns; unique on documentno after dropping observations without mineid
master.data = read.csv(master.data.file.name)

# bye
rm(master.data.file.name)

################################################################################

# CLEAN DATA

# drop data without mineid 
  # 1000 rows; 104 columns; unique on documentno
master.data = master.data[!is.na(master.data$mineid), ]

# drop unnecessary variables
  # 1000 rows; 18 columns; unique on documentno
master.data = master.data[, c("accidentclassification", "accidentdate", "accidenttype", 
                              "accidenttypecode", "activitycode", "bodypartcode", 
                              "degreeofinjury", "documentno", "equiptypecode", 
                              "injurysourcecode", "mineid", "mineractivity", 
                              "narrativemodified", "natureofinjury", "occupation", 
                              "occupcode3digit", "sourceofinjury", "X")]

# rename variables
names(master.data)[names(master.data) == "narrativemodified"] = "narrative"
names(master.data)[names(master.data) == "X"] = "PS"

# format variables
master.data$PS = factor(ifelse(master.data$PS == 1, "YES", "NO"))
master.data$documentno = as.character(master.data$documentno)
master.data$narrative = as.character(master.data$narrative)
master.data$occupcode3digit = as.character(master.data$occupcode3digit)
master.data$occupation = as.character(master.data$occupation)
master.data$accidentclassification = tolower(master.data$accidentclassification)
master.data$accidenttype = tolower(master.data$accidenttype)
master.data$degreeofinjury = tolower(master.data$degreeofinjury)
master.data$mineractivity = tolower(master.data$mineractivity)
master.data$narrative = iconv(master.data$narrative,"WINDOWS-1252","UTF-8") # remove encoded characters
master.data$narrative = tolower(master.data$narrative)
master.data$natureofinjury = tolower(master.data$natureofinjury)
master.data$occupation = tolower(master.data$occupation)
master.data$sourceofinjury = tolower(master.data$sourceofinjury)

# some narrative fields are polluted with other columns; split and replace these  
master.data$messy = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", master.data$narrative), 1, 0)
narrative.split = strsplit(master.data[master.data$messy == 1, "narrative"], "|", fixed = TRUE)
messy.rows = row.names(master.data[master.data$messy == 1, ])
for (i in 1:length(messy.rows)) {
  master.data[messy.rows[i], "narrative"] = unlist(narrative.split[i])[1]
  master.data[messy.rows[i], "occupcode3digit"] = unlist(narrative.split[i])[2]
  master.data[messy.rows[i], "occupation"] = unlist(narrative.split[i])[3]
}

# bye
master.data$messy = NULL
rm(i, messy.rows, narrative.split)

################################################################################

# RECODE MISCLASSIFIED ACCIDENTS

# recoded based on Miguel's 5/27/2016 response to our questions
master.data[master.data$documentno == "219891280164", "PS"] = "YES"
master.data[master.data$documentno == "219852170075", "PS"] = "YES"
master.data[master.data$documentno == "219901620109", "PS"] = "YES"
master.data[master.data$documentno == "220011070020", "PS"] = "NO"
master.data[master.data$documentno == "219892570061", "PS"] = "NO"
master.data[master.data$documentno == "219893100251", "PS"] = "NO"
master.data[master.data$documentno == "219872990054", "PS"] = "NO"
master.data[master.data$documentno == "219983280016", "PS"] = "NO"
master.data[master.data$documentno == "220082800043", "PS"] = "NO"
master.data[master.data$documentno == "219830320021", "PS"] = "NO"
master.data[master.data$documentno == "219912970040", "PS"] = "NO"
master.data[master.data$documentno == "219942900032", "PS"] = "NO"
master.data[master.data$documentno == "219982380025", "PS"] = "NO"

# recoded based on Miguel's 6/7/2016 response to our questions
master.data[master.data$documentno == "219912970040", "PS"] = "YES"
master.data[master.data$documentno == "219871460076", "PS"] = "NO"
master.data[master.data$documentno == "219861280065", "PS"] = "NO"
master.data[master.data$documentno == "220000310115", "PS"] = "NO"
master.data[master.data$documentno == "220001180052", "PS"] = "NO"
master.data[master.data$documentno == "219831430047", "PS"] = "NO"
master.data[master.data$documentno == "219943180016", "PS"] = "NO"
master.data[master.data$documentno == "220112090013", "PS"] = "NO"

# recoded on 9/13/2016 in light of Miguel's lack of response to our questions
master.data[master.data$documentno == "220090630033", "PS"] = "YES"
master.data[master.data$documentno == "220050800006", "PS"] = "YES"
master.data[master.data$documentno == "219892210062", "PS"] = "YES"
master.data[master.data$documentno == "219950870035", "PS"] = "YES"
master.data[master.data$documentno == "219972890025", "PS"] = "YES"
master.data[master.data$documentno == "219930390025", "PS"] = "NO"
master.data[master.data$documentno == "219992320012", "PS"] = "NO"
master.data[master.data$documentno == "219853190080", "PS"] = "NO"
master.data[master.data$documentno == "219973490121", "PS"] = "NO"
master.data[master.data$documentno == "219852050003", "PS"] = "NO"
master.data[master.data$documentno == "219891140147", "PS"] = "NO"
master.data[master.data$documentno == "220020100051", "PS"] = "NO"

################################################################################

# OUTPUT DATA

# output PS training/testing set
  # 1000 rows; 18 columns; unique on documentno
saveRDS(master.data, file = train.test.set.out.file.name)

# bye
rm(list = ls())

################################################################################
