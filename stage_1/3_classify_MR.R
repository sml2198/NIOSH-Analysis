# resume line 104

# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 3 - Classify Maintenance and Repair Accidents
  # Train/Test:
    # Inputs master MR dataset
    # Conducts narrative analysis on description of accidents, generates flags for key words and phrases, 
      # and groups existing categorical variables based on utility in classifying accidents
    # Trains and tests various classification algorithms to classify accidents (MR/non-MR)
      # Results printed to screen
  # Classify:
    # Inputs master MR dataset and accidents data from the MSHA open data portal, (cleaned in 1_clean_accidents)
    # Conducts narrative analysis on description of accidents, generates flags for key words and phrases, 
      # and groups existing categorical variables based on utility in classifying accidents
    # Trains classification algorithm using best method determined in train/test phase (Adaptive Boosting)
    # Classifies all accidents from MSHA open data portal as MR/non-MR

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com
# Last edit 1/11/17

################################################################################

#library(zoo)
#library(tree)
#library(randomForest)
#library(ggplot2)
#library(reshape2)
#library(pROC)
#library(ROSE)
#library(rpart)
#library(rpart.plot)
#library(adabag)
#library(DMwR)
#library(caret)

################################################################################

# SETTINGS
  # file can be used to train and test various algorithms OR to train the best 
  # algorithm and use it to classify all accidents (MR/non-MR)

purpose = "train.test" # trains and test various algorithms
# purpose = "classify" # trains best algorithm and classifies accidents as MR/non-MR

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
originals.input.path = paste0(root, "/0_originals", collapse = NULL) 
cleaned.input.path = paste0(root, "/1_cleaned", collapse = NULL)
coded.output.path = paste0(root, "/3_coded", collapse = NULL)

# inputs
  # master MR dataset
    # coded by NIOSH representatives and sent to the Morantz team on 8/28/2015
training.set.file.name = paste0(originals.input.path, "/training-sets/Training_Set_Maintenance_And_Repair_Accidents_August_2015_2.csv", collapse = NULL)
  # extra MR accidents (all fatalities) 
    # collected by Sarah Levine following correspondence with John H. from NIOSH on 4/14/2016
fatalities.data.file.name = paste0(originals.input.path, "/coded_MR_fatalities.csv", collapse = NULL)
  # accidents data from the MSHA open data portal , cleaned in 1_clean_accidents
accidents.data.file.name = paste0(cleaned.input.path, "/clean_accidents.rds", collapse = NULL)

# outputs
  # accidents data, classified as MR/non-MR (R dataset)
classified.accidents.file.name = paste0(coded.output.path, "/classified_accidents_MR.rds", collapse = NULL)
  # accidents data, classified as MR/non-MR (R dataset)
classified.accidents.file.name.csv = paste0(coded.output.path, "/classified_accidents_MR.csv", collapse = NULL)

# generate file paths
dir.create(coded.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)  

################################################################################

# READ DATA

# read master MR dataset
  # 1000 rows; 111 columns; unique on documentno
mr.data = read.csv(training.set.file.name, header = TRUE, sep = ",", nrows = 1001, stringsAsFactors = FALSE)

# read extra MR injuries
  # 23 rows; 110 columns; unique on documentno
mr.fatalities = read.csv(fatalities.data.file.name, header = TRUE, sep = ",", nrows = 24, stringsAsFactors = FALSE)

# read unclassified accidents data 
  # 75016 rows; 56 columns; unique on documentno
if (purpose == "classify") {
  accidents.data = readRDS(accidents.data.file.name) 
}

# bye
rm(root, cleaned.input.path, coded.output.path, originals.input.path,
   training.set.file.name, accidents.data.file.name, fatalities.data.file.name)

################################################################################

# MAKE SURE TRAINING SET AND FATALITIES DATASETS HAVE ALL THE SAME VARIABLES NAMES BEFORE APPENDING - IF USING TRAINING DATA

mr.data$MR = as.factor(mr.data$M.R.)
mr.data$M.R. = NULL
mr.data$death = ifelse(grepl("fatality", mr.data$degreeofinjury), 1, 0)

# Clean up fatalities variables - drop variables not present in training set before appending
mr.fatalities$MR = as.factor(mr.fatalities$MR_fatality)
mr.fatalities = mr.fatalities[, c(-grep("MR_fatality", names(mr.fatalities)), 
                                  -grep("v56", names(mr.fatalities)),
                                  -grep("v57", names(mr.fatalities)), 
                                  -grep("v58", names(mr.fatalities)), 
                                  -grep("v59", names(mr.fatalities)))]

# These four fatalgrams are considered MR and were included in a study by John H. from NIOSH as MR. 
# However, it's really only evident from the fatalgrams (see open data folder) that these were sustained
# during larger group MR activities. Nothing from the narrative field/occupation indicates that MR was the
# activity at the time. Essentially, training on these observations will stack the deck against us.
# We delete them and wind up with 19 additional fatality observations to append.
mr.fatalities = mr.fatalities[!(mr.fatalities$documentno=="220030290001") & 
                              !(mr.fatalities$documentno=="220030290002") &
                              !(mr.fatalities$documentno=="220030290003") & 
                              !(mr.fatalities$documentno=="220030130149"),]

# Clean up narrative fields: drop redundant variables and keep the lowercase versions
drops = c("narrativemodified", "degreeofinjury", "accidentclassification", "accidenttype", "natureofinjury", "mineractivity")
mr.data = mr.data[, !(names(mr.data) %in% drops)]
names(mr.data)[names(mr.data) == "narrativemodified.1"] = "narrative"
mr.data$narrative = tolower(mr.data$narrative)
names(mr.data)[names(mr.data) == "degreeofinjury.1"] = "degreeofinjury"
mr.data$degreeofinjury = tolower(mr.data$degreeofinjury)
names(mr.data)[names(mr.data) == "accidentclassification.1"] = "accidentclassification"
mr.data$accidentclassification = tolower(mr.data$accidentclassification)
names(mr.data)[names(mr.data) == "accidenttype.1"] = "accidenttype"
mr.data$accidenttype = tolower(mr.data$accidenttype)
names(mr.data)[names(mr.data) == "natureofinjury.1"] = "natureofinjury"
mr.data$natureofinjury = tolower(mr.data$natureofinjury)
names(mr.data)[names(mr.data) == "mineractivity.1"] = "mineractivity"
mr.data$mineractivity = tolower(mr.data$mineractivity)
mr.data$occupation = tolower(mr.data$occupation)
mr.data$typeofequipment = tolower(mr.data$typeofequipment)
mr.data$sourceofinjury = tolower(mr.data$sourceofinjury)
mr.data$bodypart = tolower(mr.data$bodypart)
mr.data$equipmanufacturer = tolower(mr.data$equipmanufacturer)
mr.data$immediatenotificationclass = tolower(mr.data$immediatenotificationclass)
mr.data$uglocation = tolower(mr.data$uglocation)

# Append dataset of additional fatality observations for training set
mr.data = rbind(mr.data, mr.fatalities) 

# One of these was redundant (same document number) so we drop this
mr.data = mr.data[!duplicated(mr.data$documentno), ]

# make MR a factor variable
mr.data[, "MR"] = factor(ifelse(mr.data[, "MR"] == 1, "YES", "NO"))
names(mr.data)[names(mr.data) == "MR"] = "MR"

# RECODE MISCODED INJURIES AS NON-M&R. 
# See email with J. Heberger from NIOSH on May 2, 2016. About the following injury, he explains "even though 
# mine worker activity is MR, installing roof bolts is not considered MR. Should be coded 2." Here we manually 
# recode this one observation. 
mr.data$MR[mr.data$documentno=="219932950056"] = "NO"

if (data.type == "training data") {
  mr.data$type = "training"
}

################################################################################

# THIS CODE IS RUN IF USING THE REAL ACCIDENTS DATA (NOT THE TRAINING SET FOR ALGORITHM TESTING)

if (data.type == "real accidents data") {
  
  # First make a flag to identify the training observations
  mr.data[, "type"] = "training"  
  mr.data[, "datasource"] = "training"  
  mr.data[, "investigationbegindate"] = "" 
  accidents.data[, "type"] = "unclassified" 
  accidents.data[, "contractor_accident"] = "" 
  accidents.data[, "MR"] = "" 
  
  # Clean up injury narrative fields: drop redundant variables and keep the lowercase versions
  accidents.data$narrative = tolower(accidents.data$narrative)
  accidents.data$degreeofinjury = tolower(accidents.data$degreeofinjury)
  accidents.data$accidentclassification = tolower(accidents.data$accidentclassification)
  accidents.data$accidenttype = tolower(accidents.data$accidenttype)
  accidents.data$natureofinjury = tolower(accidents.data$natureofinjury)
  accidents.data$mineractivity = tolower(accidents.data$mineractivity)
  accidents.data$occupation = tolower(accidents.data$occupation)
  accidents.data$typeofequipment = tolower(accidents.data$typeofequipment)
  accidents.data$sourceofinjury = tolower(accidents.data$sourceofinjury)
  accidents.data$bodypart = tolower(accidents.data$bodypart)
  accidents.data$equipmanufacturer = tolower(accidents.data$equipmanufacturer)
  accidents.data$immediatenotificationclass = tolower(accidents.data$immediatenotificationclass)
  accidents.data$uglocation = tolower(accidents.data$uglocation)
  
  # Drop variables not common to the accidents data set and the mr data (training set)
  drops = c("death", "i" )
  mr.data = mr.data[, !(names(mr.data) %in% drops)]  
  drops = c("assesscontrolno", "part48training", "controllerbegindate", 
            "fiscalquarter", "fiscalyear", "year", "closed_doc_no",
            "quarter", "avg_hours_qtr", "avg_employment_qtr", 
            "avg_coal_prod_qtr")
  accidents.data = accidents.data[, !(names(accidents.data) %in% drops)]
  
  # Drop any remaining variables not common to the real accidents data and mr.data (training set)
  accident.names = names(accidents.data)
  mr.data = mr.data[, names(mr.data) %in% accident.names]
  
  # Create lists of document numbers from each dataset (training data and real accidents data)
  mr.docnos = mr.data$documentno
  mr.docnos = as.character(mr.docnos)
  accident.docnos = accidents.data$documentno
  
  # Identify common document numbers (the real accidents data should contain all document numbers from the training data)
  keep.docnos = setdiff(accident.docnos, mr.docnos)  
  
  # Remove observations from accidents data present in the mr.data (training set) - should now be unique on document number
  accidents.data = accidents.data[which(accidents.data$documentno %in% keep.docnos), ]
  rm(mr.names, accident.names, mr.docnos, accident.docnos, keep.docnos)
  
  # Append the training data and the real accidents dataset for classification
  mr.data = rbind(mr.data, accidents.data) 
}

################################################################################

# CLEAN UP ALL REMAINING VARIABLES 

# Destring variables
mr.data[,grep("numberofemployees", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("numberofemployees", names(mr.data))])
mr.data[,grep("numberofemployees", names(mr.data))] = as.numeric(mr.data[,grep("numberofemployees", names(mr.data))])
mr.data[,grep("methaneliberation", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("methaneliberation", names(mr.data))])
mr.data[,grep("methaneliberation", names(mr.data))] = as.numeric(mr.data[,grep("methaneliberation", names(mr.data))])
mr.data[,grep("averagemineheight", names(mr.data))] = gsub(pattern = ",",replacement =  "", mr.data[,grep("averagemineheight", names(mr.data))])
mr.data[,grep("averagemineheight", names(mr.data))] = as.numeric(mr.data[,grep("averagemineheight", names(mr.data))])

# Merge redundant "no value found" fields in factor variables
mr.data[, "uglocation"] = ifelse(mr.data[, "uglocation"] == "not marked", "no value found", mr.data[, "uglocation"])
mr.data[, "immediatenotificationclass"] = ifelse(mr.data[, "immediatenotificationclass"] == "not marked", "no value found", mr.data[, "immediatenotificationclass"])
mr.data[, "natureofinjury"] = ifelse(mr.data[, "natureofinjury"] == "unclassified,not determed", "no value found", mr.data[, "natureofinjury"])
mr.data[, "equipmanufacturer"] = ifelse(mr.data[, "equipmanufacturer"] == "not reported", "no value found", mr.data[, "equipmanufacturer"])

# We decided to recode three observations in our data that were coded as MR, but are apparently non-injury accidents. 
# It's apparent that whoever did the coding didn't look at this field. We don't want our algorithm to classify an 
# accident-only observation as positive for MR, so we enforce this change.  
mr.data$accident.only = ifelse(mr.data$degreeofinjury == "accident only" | 
                                 mr.data$accidenttype == "acc type, without injuries", 1, 0)
mr.data$MR = ifelse(mr.data$MR == "YES" & 
                      mr.data$accident.only == 0, 1, 0)
mr.data$MR[mr.data$MR == "YES" & mr.data$accident.only == 1] = 0

# Make sure that MR is still a factor variable
mr.data[, "MR"] = factor(ifelse(mr.data[, "MR"] == 1, "YES", "NO"))
names(mr.data)[names(mr.data) == "MR"] = "MR"

################################################################################

# 60 NARRATIVE FIELDS ARE POLLUTED WITH OTHER COLUMNS - SPLIT AND REPLACE THESE 

# Where the data pull was messy, there can be some pipe delimiters - "|" - strewn within the narrative fields
mr.data[, "messy"] = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", mr.data[,"narrative"]), 1, 0)
narrative.split = strsplit(mr.data[mr.data$messy == 1, "narrative"], "|", fixed = T)
messy.rows = row.names(mr.data[mr.data$messy == 1, ])
for (i in 1:length(messy.rows)) {
  mr.data[messy.rows[i], "narrative"] = unlist(narrative.split[i])[1]
  mr.data[messy.rows[i], "occupcode3digit"] = unlist(narrative.split[i])[2]
  mr.data[messy.rows[i], "occupation"] = unlist(narrative.split[i])[3]
  mr.data[messy.rows[i], "returntoworkdate"] = unlist(narrative.split[i])[4]
}
mr.data = mr.data[, c(-match("messy", names(mr.data)))]

# Deal with messy number typos - random numbers that have been dropped into narratives 
mr.data[, "numbertypo"] = ifelse(grepl("[a-z][0-9][a-z]", mr.data[,"narrative"]), 1, 0)
for (i in 0:9) {
  mr.data[mr.data$numbertypo == 1,]$narrative = gsub(i, "", mr.data[mr.data$numbertypo == 1,]$narrative)
}

# Convert dates
indices_with_date = grep("date", names(mr.data))
for (i in indices_with_date) {
  mr.data[,i] = as.Date(mr.data[,i], "%m/%d/%Y")
}

################################################################################

# CREATE KEY WORD VARIABLES FROM NARRATIVE FIELD

# GENERATE LIKELY-POSITIVELY PREDICTIVE KEY WORDS (LIKELY TO INDICATE MR INJURIES) 

# *REPAIR* 
mr.data[, "repair"] = ifelse(grepl("(^| )r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data[,"narrative"]) &
                               !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,20}hernia", mr.data[,"narrative"]) &
                               !grepl("hernia.{1,10}r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data[,"narrative"]) &
                               !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,10}wound", mr.data[,"narrative"]) &
                               !grepl("wound.{1,20}r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data[,"narrative"]), 1, 0)
mr.data[, "rplace"] = ifelse(grepl("(^| )replac(e|i)[a-z]*", mr.data[,"narrative"]), 1, 0)
# We don't want to see the noun "service" because that often refers to hoist service, but "serviced" and "servicing" are good indicators
mr.data[, "service"] = ifelse(grepl("serviced", mr.data[,"narrative"]) | 
                                grepl("servicing", mr.data[,"narrative"]), 1, 0)
mr.data[, "fix"] = ifelse(grepl("(^| )fix[a-z]*", mr.data[,"narrative"]) & 
                            !grepl("(^| )fixture", mr.data[,"narrative"]), 1, 0) 
mr.data[, "changing"] = ifelse(grepl("chang(e|ing|ed)( |-)*out", mr.data[,"narrative"]) |
                                 (grepl("chang(e|ing|ed)", mr.data[,"narrative"]) & 
                                    !grepl("chang(e|ing|ed).{1,10}(shift|place|positi)", mr.data[,"narrative"])), 1, 0)
mr.data[, "retrack"] = ifelse(grepl("re(rail|track|trakc)(ed|ing)", mr.data[,"narrative"]) |
                                grepl("pull(ing|ed)*.{1,5}track", mr.data[,"narrative"]), 1, 0)
mr.data[, "pullbelt"] = ifelse(grepl("pull( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", mr.data[,"narrative"]) |
                                 grepl("(belt|rope|spool|tube|tubing).{1,15}pull( |ing|ed|s)*", mr.data[,"narrative"]) |
                                 grepl("(belt|rope|spool|tube|tubing).{1,15}load( |ing|ed|s)*", mr.data[,"narrative"]) |
                                 grepl("load( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", mr.data[,"narrative"]), 1, 0)
mr.data[, "reposition"] = ifelse(grepl("re( |-)*pos(i|t)(i|t)(i|o)(i|o)n", mr.data[,"narrative"]), 1, 0) 
# Sometimes even when the occupation field isn't MR, the narrative field refers to the job of the injured, or to the injured employee helping an MR worker
mr.data[, "mrworker"] = ifelse(grepl("(mechanic|electrician|repairm(a|e)n)", mr.data[,"narrative"]), 1, 0) 
mr.data[, "cover"] = ifelse((grepl("(replac|l(i|e)ft).{1,20}(panel|cover| lid|hood)", mr.data[,"narrative"]) |
                               grepl("(panel|cover| lid|hood){1,5}fell", mr.data[,"narrative"]) | 
                               grepl("drop.{1,10}(panel|cover| lid|hood)", mr.data[,"narrative"])) &
                              !grepl("eye.{1,5}lid", mr.data[,"narrative"]), 1, 0) 
mr.data[, "toolbox"] = ifelse(grepl("( |^)tool", mr.data[,"narrative"]), 1, 0)

# *MAINTENANCE* 

# "Cleaning the rib" refers to using a scoop to grab extra coal, but this is in fact a production activity (not an MR activity)
mr.data[, "cleaning"] = ifelse(grepl("cl(ean|(e)*aning)", mr.data[,"narrative"]) & 
                                 !grepl("clean.{1,10} rib", mr.data[,"narrative"]), 1, 0) 
mr.data[, "maintain"] = ifelse(grepl("(^| )maint(ain|en|ean)[a-z]*", mr.data[,"narrative"]) | 
                                 grepl("maint.{1,9}work", mr.data[,"narrative"]), 1, 0)
# Try to avoid inspection/inspector (the noun) and just grab mentions of "inspect" (the verb) 
mr.data[, "inspect"] = ifelse(grepl("inspect( |ed|s|ing|\\.|,|$)", mr.data[,"narrative"]), 1, 0)
mr.data[, "shovel"] = ifelse(grepl("shovel(ing|ed).{1,5}coal)", mr.data[,"narrative"]) |
                               grepl("coal.{1,15}shovel(ing|ed)", mr.data[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}belt", mr.data[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}convey(e|o)r", mr.data[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}tail( |-)*p(e|i)(e|i)ce", mr.data[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}(head|drive|guide|bend|lagged|tail)*( |-)*pull(y|ey|ies|ys)", mr.data[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}(roller|idler)", mr.data[,"narrative"]) |
                               grepl("shovel(ing|ed).{1,20}(west|header|drive)", mr.data[,"narrative"]), 1, 0)
# We don't want the noun "hose" (or "whose"), just the verb - we also don't want "bullhose" 
mr.data[, "washingdown"] = ifelse(grepl("( |^|\\.|,)(wash|hose)(d|ed|ing| )", mr.data[,"narrative"]), 1, 0) 
mr.data[, "grease"] = ifelse(grepl("greas(ed|ing|e|er)", mr.data[,"narrative"]), 1, 0) 
# Try to avoid "doctor checked out injury"
mr.data[, "check"] = ifelse(grepl("che(c|k)(c|k)", mr.data[,"narrative"]) &
                              !grepl("doctor", mr.data[,"narrative"]) &
                              !grepl("hospital", mr.data[,"narrative"]) &
                              !grepl("emergency", mr.data[,"narrative"]) &
                              !grepl("clinic", mr.data[,"narrative"]), 1, 0) 
# "Tests" usually means doctors test, and we also want to avoid "testicles"
mr.data[, "tests"] = ifelse(grepl("test(ing|ed)", mr.data[,"narrative"]) &
                              !grepl("doctor", mr.data[,"narrative"]) &
                              !grepl("hospital", mr.data[,"narrative"]) &
                              !grepl("emergency", mr.data[,"narrative"]) &
                              !grepl("clinic", mr.data[,"narrative"]), 1, 0) 
# Oil in mention of can/drum/barrel often means something is being greased (MR). Otherwise it usually apears in some other context (being slipped on, lit, etc.)
mr.data[, "oil"] = ifelse(grepl("(^| )(oil).{1,25}(can|drum|barrel|container)", mr.data[,"narrative"]) |
                            grepl("(can|drum|barrel|container).{1,25}oil", mr.data[,"narrative"]) | 
                            grepl("(chang|add)(e|ing).{1,6}(oil|fuel|equipment)", mr.data[,"narrative"]) |
                            grepl("(( |^)oil|fuel)ing", mr.data[,"narrative"]) | 
                            grepl("to (((change|add) (oil|fuel))|fuel)", mr.data[,"narrative"]), 1, 0) 

# GENERATE POTENTIALLY-POSITIVELY PREDICTIVE KEY WORDS (MAYBE INDICATE MR INJURIES) 

# *REPAIR* 
mr.data[, "dismantl"] = ifelse(grepl("dismant(el|le|al|il|l)", mr.data[,"narrative"]), 1, 0) 
mr.data[, "rethread"] = ifelse(grepl("re( |-)*thr(ea|e)d", mr.data[,"narrative"]), 1, 0)
mr.data[, "remove"] = ifelse(grepl("re(m)*ov(e|ed|ing|al)", mr.data[,"narrative"]) | 
                               grepl("rem(o)*v(e|ed|ing|al)", mr.data[,"narrative"]), 1, 0) 

# *MAINTENANCE* 
mr.data[, "bits"] = ifelse(grepl("set(t)*(ing)*( |-)*bits", mr.data[,"narrative"]), 1, 0)
mr.data[, "conveyor"] = ifelse(grepl("convey(o|e)r", mr.data[,"narrative"]), 1, 0)
# We don't want "help", just the verb "helping" 
mr.data[, "helping"] = ifelse(grepl("help(ed|in(g)*|er)", mr.data[,"narrative"]) |
                                grepl("assis(s)*t(ed|in(g))*", mr.data[,"narrative"]), 1, 0)
mr.data[, "belt"] = ifelse(grepl("belt|spool|tube|tubing", mr.data[,"narrative"]), 1, 0)
mr.data[, "tighten"] = ifelse(grepl("tighten", mr.data[,"narrative"]), 1, 0)
mr.data[, "loosen"] = ifelse(grepl("loos(en|ing)", mr.data[,"narrative"]), 1, 0)
# Most cases of changing batteries are MR, but sometimes someone might just trip on a charger, or
# be operating a "battery personnel carrier", and we want to avoid these
mr.data[, "battery"] = ifelse(grepl("bat(t)*(e)*r(y|ies)", mr.data[,"narrative"]) &
                                !grepl("bat(t)*(e)*r(y|ies).{1,6}charg(er|ing)", mr.data[,"narrative"]) &
                                !grepl("bat(t)*(e)*r(y|ies).{1,8}person(n)*(el|le)", mr.data[,"narrative"]) &
                                !grepl("bat(t)*(e)*r(y|ies).{1,5}car(r)*i(e|o)r", mr.data[,"narrative"]), 1, 0)

# DEAL WITH THE WORD "INSTALLATION". INSTALLS ARE NOT MR IF RELEVANT TO CONSTRUCTION/PRODUCTION, LIKE WITH ROOF BOLTING
mr.data[, "roof.bolt"] = ifelse(grepl("(roof|rib)*( |-)*(bolt)(er|s|ing| |$|\\.|,).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) | 
                                  grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(roof|rib)*( |-)*(bolt)(er|s|ing| |$|\\.|,)", mr.data[,"narrative"]), 1, 0)  
mr.data[, "rib.hole"] = ifelse(grepl("(rib)( |-)*(hole).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) | 
                                 grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(rib)( |-)*(hole)", mr.data[,"narrative"]), 1, 0)  
# Accounts for install, reinstall, uninstall (but not "an installed cable" or something like that)
mr.data[, "install"] = ifelse(grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data[,"narrative"]) &
                                !grepl("(^| )an( |e|n)i(s|n|t)(s|n|t)(s|n|t)alled", mr.data[,"narrative"]) & 
                                (mr.data[, "rib.hole"] != 1 & mr.data[, "roof.bolt"] != 1), 1, 0)

# LIEKLY NEGATIVELY-PREDICTIVE KEY WORDS (LIKELY TO INDICATE NOT-MR) 

# "Pain" and "injured" often indicate surgical "repairs" have been performed
mr.data[, "pain"] = ifelse(grepl("(^| )(pain|hurt)(s)*( |$|\\.|,|:)", mr.data[,"narrative"]), 1, 0)
mr.data[, "injured"] = ifelse(grepl("injur", mr.data[,"narrative"]), 1, 0)
# Make sure "hoisting" or "hoisted" aren't grabbed (we just want to capture elevators)
mr.data[, "hoist"] = ifelse(((grepl("(^| )hoist(s| |$|\\.|,|:)", mr.data[,"narrative"]) |
                                grepl("(^| )el(e|a|i)vat(o|e)r", mr.data[,"narrative"])) & 
                               !grepl("(elevat(o|e)r|hoist).{1,10}(door|cable|hous|cylinder|hook|rope|convey(o|e)r|drum|jack|motor|chain|grip|brake|pedal|slope|cage|frame)", mr.data[,"narrative"]) & 
                               !grepl("(door|cable|hous|cylinder|hook|rope|convey(o|e)r|drum|jack|motor|chain|grip|brake|pedal|slope|cage|frame).{1,10}(elevat(o|e)r|hoist)", mr.data[,"narrative"]) & 
                               !grepl("(^| )us(e|ing).{1,10}(elevat(o|e)r|hoist)", mr.data[,"narrative"]) & 
                               mr.data[, "pain"] == 0 & 
                               mr.data[, "injured"] == 0), 1, 0) 
mr.data[, "surgery"] = ifelse((grepl("surger[a-z]*", mr.data[,"narrative"]) | 
                                 grepl("surgic[a-z]*", mr.data[,"narrative"])) & 
                                mr.data[, "pain"] == 0 & 
                                mr.data[, "injured"] == 0, 1, 0)

################################################################################

# GENERATE ADDITIONAL KEY WORDS ABOUT WHICH WE HAVE NO PRIORS TO FEED INTO RANDOM FOREST 

mr.data[, "power"] = ifelse(grepl("pow(e)*r", mr.data[,"narrative"]), 1, 0)
# These don't add much
mr.data[, "splice"] = ifelse(grepl("splice", mr.data[,"narrative"]) & 
                               (mr.data$occupcode3digit %in% c("004", "418")), 1, 0)
mr.data[, "lug"] = ifelse(grepl("( |^)lug(g)*", mr.data[,"narrative"]) & 
                            (mr.data$occupcode3digit %in% c("004", "418")), 1, 0)
# We only want the noun, not the verb
mr.data[, "wrench"] = ifelse(grepl("wrench", mr.data[,"narrative"]), 1, 0)
mr.data[, "trash"] = ifelse(grepl("(trash|garbage|dumpster)", mr.data[,"narrative"]), 1, 0)
mr.data[, "roller"] = ifelse(grepl("roller", mr.data[,"narrative"]), 1, 0)
mr.data[, "moretools"] = ifelse(grepl("(pry|crow|jack)( |-)*bar", mr.data[,"narrative"]) | 
                                  grepl("(hammer|screw( |-)*driver|shovel( |\\.|$|,|:)|ratchet)", mr.data[,"narrative"]) | 
                                  grepl("com(e)*(-)*(a)*(-)*long", mr.data[,"narrative"]), 1, 0)
mr.data[, "welding"] = ifelse((grepl("(( |^)tank|ac(c)*etyle(ne|en)|weld)", mr.data[,"narrative"]) | 
                                 grepl("(oxygen|o2)( )*(bottle|cylinder)", mr.data[,"narrative"])) &
                                !grepl("chemic.{1,10}tank)", mr.data[,"narrative"]), 1, 0)
mr.data[, "tire"] = ifelse(grepl("(chang|pump)(e|ed|ing).{1,5}tire", mr.data[,"narrative"]), 1, 0)

# GENERATE OTHER USEFUL FLAGS ABOUT THE ACCIDENT (E.G. DID SOMETHING FALL?)
mr.data$falling.class = ifelse(mr.data$accidentclassification == "fall of roof or back", 1, 0)
mr.data[, "falling.word"] = ifelse(grepl("rock( )*fell", mr.data[,"narrative"]) |
                                     grepl("fell.{1,20}roof", mr.data[,"narrative"]) |
                                     grepl("roof( )*f(a|e)ll", mr.data[,"narrative"]), 1, 0)
mr.data$falling.accident = ifelse(mr.data$falling.class == 1 | 
                                    mr.data$falling.word == 1, 1, 0)
mr.data = mr.data[, c(-match("falling.class", names(mr.data)), 
                      -match("falling.word", names(mr.data)))]
mr.data$accident.only = ifelse((mr.data$degreeofinjury == "accident only" | 
                                  mr.data$accidenttype == "acc type, without injuries"), 1, 0)

################################################################################

# CREATE/PREPARE VARIOUS TIME AND DATE VARIABLES

date = strptime(mr.data$calendaryear, "%Y")
format(date, "%Y")
mr.data[, "year"] = format(date, "%Y")
mr.data[, "quarter"] = as.yearqtr(mr.data$accidentdate,"%Y-%m-%d")
mr.data = mr.data[, c(-grep("calendar", names(mr.data)), 
                      -grep("accidentdate", names(mr.data)))]

# Remove irrelevant variables
mr.data = mr.data[, c(-match("roof.bolt", names(mr.data)), 
                      -match("rib.hole", names(mr.data)), 
                      -match("transferredorterminated", names(mr.data)),
                      -match("accidenttime", names(mr.data)))]

# Remove redundant variables (i.e. these with codes and corresponding classes)
mr.data = mr.data[, c(-match("accidenttypecode", names(mr.data)),
                      -match("activitycode", names(mr.data)), 
                      -match("bodypartcode", names(mr.data)), 
                      -match("controllerid", names(mr.data)), 
                      -match("classificationcode", names(mr.data)),
                      -match("degreeofinjurycode", names(mr.data)),
                      -match("equiptypecode", names(mr.data)), 
                      -match("equipmanufacturercode", names(mr.data)),
                      -match("fipsstatecode", names(mr.data)), 
                      -match("injurysourcecode", names(mr.data)), 
                      -match("natureofinjurycode", names(mr.data)),
                      -match("immediatenotificationcode", names(mr.data)), 
                      -match("occupcode3digit", names(mr.data)),
                      -match("operatorid", names(mr.data)), 
                      -match("subunitcode", names(mr.data)), 
                      -match("uglocationcode", names(mr.data)), 
                      -match("ugminingmethodcode", names(mr.data)))]

# Create lists storing the types of variables
var_classes = sapply(mr.data[,names(mr.data)], class)
charac_vars = names(var_classes[c(grep("character", var_classes), grep("factor", var_classes))])
num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])

for (i in 1:length(charac_vars)) {
  mr.data[, charac_vars[i]] = ifelse((mr.data[,charac_vars[i]] == "no value found" | 
                                        mr.data[,charac_vars[i]] == "unknown" | 
                                        mr.data[,charac_vars[i]] == "?" | 
                                        mr.data[,charac_vars[i]] == ""), NA_character_, as.character(mr.data[,charac_vars[i]]))
  mr.data[, charac_vars[i]] = factor(mr.data[, charac_vars[i]])
}

# Define a function that calculates the mode of a variable (for imputation)
modus = function(x) {
  uniqv = unique(x)
  uniqv[which.max(tabulate(match(x, uniqv)))]
}

################################################################################

# CREATE SIMPLE DATA CONTAINING JUST THE VARIABLES USED FOR ANALYSIS

# drop remaining unnecessary vars
drops = c("bodypart", "contractor_accident","contractorid", 
          "controllername", "dayslost", "daysrestrictedduty",
          "equipmanufacturer", "equipmentmodelno", "immediatenotificationclass",
          "injured", "investigationbegindate", "jobexperience", 
          "mineexperience", "natureofinjury", "numberofinjuries", 
          "numbertypo", "operatorname", "quarter", 
          "returntoworkdate", "schedulechargedays", "shiftbeginningtime",       
          "subunit", "totalexperience", "typeofequipment",
          "uglocation", "ugminingmethod", "year")
simple.data = mr.data[, !(names(mr.data) %in% drops)] 
         
################################################################################

# CREATE LIKELY/MAYBE/UNLIKELY GROUPS OF VALUES OF CATEGORICAL VARIABLES

simple.data[, "likely.occup"] = ifelse(grepl("maintenance", simple.data[,"occupation"]) & 
                                         simple.data$accident.only == 0, 1, 0)

simple.data[, "maybe.occup"] = ifelse(grepl("electrician", simple.data[,"occupation"]) & 
                                        simple.data$accident.only == 0, 1, 0)

simple.data[, "likely.activy"] = ifelse(grepl("maintenance", simple.data[,"mineractivity"]) | 
                                          grepl("wet down working place", simple.data[,"mineractivity"]) & 
                                          simple.data$accident.only == 0, 1, 0)

simple.data[, "maybe.activy"] = ifelse(match("handling supplies/materials", simple.data[,"mineractivity"]) |
                                         match("hand tools (not powered)", simple.data[,"mineractivity"]) |
                                         match("no value found", simple.data[,"mineractivity"]) |
                                         match("unknown", simple.data[,"mineractivity"]) | 
                                         match("clean up", simple.data[,"mineractivity"]) | 
                                         match("inspect equipment", simple.data[,"mineractivity"]) & 
                                         simple.data$accident.only == 0, 1, 0)

simple.data[, "likely.class"] = ifelse(match("handtools (nonpowered)", simple.data[,"accidentclassification"]) |
                                         match("machinery", simple.data[,"accidentclassification"]) |
                                         match("electrical", simple.data[,"accidentclassification"]) & 
                                         simple.data$accident.only == 0, 1, 0)

simple.data[, "likely.source"] = ifelse((simple.data$sourceofinjury == "wrench" | 
                                           simple.data$sourceofinjury == "knife" |
                                           simple.data$sourceofinjury == "power saw" | 
                                           simple.data$sourceofinjury == "hand tools,nonpowered,nec" |
                                           simple.data$sourceofinjury == "crowbar,pry bar" | 
                                           simple.data$sourceofinjury == "axe,hammer,sledge") & 
                                          simple.data$accident.only == 0, 1, 0)

# all "surgeries" are false keywords, but only "hoist/elevator" in combo with words that refer to elevator service are false keywords
simple.data$false.keyword = ifelse((simple.data$repair & simple.data$surgery == 1 ) |
                                     (simple.data$fix & simple.data$surgery == 1 ) |
                                     (simple.data$rplace & simple.data$surgery == 1 ) |
                                     (simple.data$repair & simple.data$hoist == 1 ) |
                                     (simple.data$maintain & simple.data$hoist == 1 ) |
                                     (simple.data$service & simple.data$hoist == 1 ) |
                                     (simple.data$fix & simple.data$hoist == 1 ), 1, 0)

simple.data$likely.keyword = ifelse((simple.data$repair == 1 | simple.data$fix == 1 | 
                                       simple.data$maintain == 1 | simple.data$rplace == 1 |
                                       simple.data$install == 1 | simple.data$service == 1 |
                                       simple.data$cleaning == 1 | simple.data$changing == 1 |
                                       simple.data$retrack == 1 | simple.data$inspect == 1 |
                                       simple.data$shovel == 1 | simple.data$reposition == 1 | 
                                       simple.data$pullbelt == 1 | simple.data$grease == 1 |
                                       simple.data$washingdown == 1 | simple.data$check == 1 |
                                       simple.data$oil == 1 | simple.data$mrworker == 1 |                                      
                                       simple.data$cover == 1 | simple.data$tests == 1 |
                                       simple.data$toolbox == 1 ) & simple.data$accident.only == 0 &
                                      simple.data$false.keyword == 0, 1, 0)

simple.data$maybe.keyword = ifelse( (simple.data$remove == 1 | simple.data$dismantl == 1 | 
                                       simple.data$rethread == 1 | simple.data$welding == 1 | 
                                       simple.data$bits == 1 | simple.data$helping == 1 |
                                       simple.data$conveyor == 1 | simple.data$belt == 1 |
                                       simple.data$tighten == 1 | simple.data$battery == 1 ) & simple.data$accident.only == 0 &
                                      simple.data$false.keyword == 0, 1, 0)

# remove all categorical variables - keep narratives and document number for model training
simple.data = simple.data[, c(-grep("accidentclassification", names(simple.data)),
                              -grep("accidenttype", names(simple.data)), 
                              -grep("degreeofinjury", names(simple.data)), 
                              -grep("mineractivity", names(simple.data)),
                              -grep("occupation", names(simple.data)), 
                              -grep("sourceofinjury", names(simple.data)))]

# now drop categorical variables from data
drops = c("sourceofinjury", 
          "equipmentmodelno", 
          "fipscountyname", 
          "controllername", 
          "mineractivity", 
          "minename", 
          "operatorname", 
          "quarter", 
          "occupation")
simple.data = simple.data[, !(names(simple.data) %in% drops)]

################################################################################

# BEGIN ALGORITHM - RANDOMLY SORT DATA 

# Set seed so the randomizations are conducted equally each time this file is run
set.seed(626)
rand = runif(nrow(mr.data))
train = mr.data[order(rand),]
rand2 = runif(nrow(simple.data))
simple = simple.data[order(rand2),]
remove(rand,rand2)

# This code in just to find out which column number the MR indicator is, so we can report accuracy
which(colnames(train)=="MR")
which(colnames(simple)=="MR")

# The code below this line was used in testing various algorithms, and is included for interest alone
if (data.type == "training") {
  
  ################################################################################
  
  # CREATE CART FUNCTION WITH RPART AND EXECUTE ON 1ST 600 OBSERVATIONS
  cart = rpart(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], method="class")
  cart 
  rpart.plot(cart, type=3, extra = 101, fallen.leaves=T)
  printcp(cart) 
  
  ################################################################################
  
  # DEFINE RANDOM FOREST (ON TRUE PROPORTION OF NO'S AND YES'S)
  rf = randomForest(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], mtry = 15, importance=TRUE, type="class",
                    ntree = 1000)
  rf
  rf.oob.predictions = predict(rf, simple[1:700,!(names(simple) %in% c('documentno','narrative'))],type="class")
  table(simple[1:700,4], predicted = rf.oob.predictions)
  
  ################################################################################
  
  # DOWNSAMPLE NEGATIVE OUTCOMES (MR=NO) FOR RANDOM FOREST
  nmin = sum(simple$MR == "YES")
  nmin
  ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
  rf.downsampled = train(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], method = "rf", ntree = 800,
                         tuneLength = 10, metric = "ROC", trControl = ctrl, 
                         strata = simple$MR, sampsize = rep(nmin, 2))
  rf.baseline = train(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], method = "rf", ntree = 800,
                      tuneLength = 10, metric = "ROC", trControl = ctrl)
  
  down.prob = predict(rf.downsampled, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))], type = "prob")[,1]
  down.ROC = roc(response = simple[701:1019,4], predictor = down.prob, levels = rev(levels(simple[701:1019,1])))
  base.prob = predict(rf.baseline, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))], type = "prob")[,1]
  base.ROC = roc(response = simple[701:1019,4], predictor = base.prob, levels = rev(levels(simple[701:1019,1])))
  
  plot(down.ROC, col = rgb(1, 0, 0, .5), lwd = 2)
  plot(base.ROC, col = rgb(0, 0, 1, .5), lwd = 2, add = TRUE)
  legend(.4, .4, c("Down-Sampled", "Normal"), lwd = rep(2, 1), col = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)))
  
  ################################################################################
  
  # OVERSAMPLE POSITIVE OUTCOMES (MR=YES) FOR RANDOM FOREST: GENERATE ARTIFICIALLY BALANCED DATA WITH ROSE PACKAGE
  simple.rosex = ROSE(MR ~ ., data=simple[1:700,!(names(simple) %in% c('documentno','narrative'))])$data
  rand3 = runif(nrow(simple.rosex))
  simple.rose = simple.rosex[order(rand3),]
  remove(simple.rosex)
  rf.rose = randomForest(MR ~ ., data = simple.rose, mtry = 15, ntree = 1000)
  rf.rose
  
  ################################################################################
  
  # OVERSAMPLE POSITIVE OUTCOMES (MR=YES) FOR RANDOM FOREST: GENERATE ARTIFICIALLY BALANCED DATA WITH SMOTE PACKAGE
  smote.trainx = simple[1:700, !(names(simple) %in% c('documentno','narrative'))]
  smote.test = simple[701:1019, !(names(simple) %in% c('documentno','narrative'))]
  smote = SMOTE(MR ~ ., smote.trainx, perc.over = 100, perc.under = 100)
  rf.smo = randomForest(MR ~ ., data = smote, mtry = 10, ntree = 800)
  
  ################################################################################
  
  # USE ADABOOST TO IMPLEMENT BOOSTING ALGORITHM 
  mr.adaboost = boosting(MR ~ . , data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], boos = T, mfinal = 300, coeflearn = 'Freund')
  adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[701:1019,!(names(simple) %in% c('documentno','narrative'))])
  
  ################################################################################
  
  # PRINT ALL PREDICTIONS 
  
  # SMOTE
  rf.smo.pred = predict(rf.smo, smote.test, type = "class")
  table(simple[701:1019,4], predicted = rf.smo.pred)
  
  # ROSE
  rf.rose.pred = predict(rf.rose, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))],type="class")
  table(simple[701:1019,4], predicted = rf.rose.pred)
  
  # SIMPLE CART
  cart.predictions = predict(cart, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))],type="class")
  table(simple[701:1019,4], predicted = cart.predictions)
  
  # RF UNBALANCED 
  rf.predictions = predict(rf, simple[701:1019,!(names(simple) %in% c('documentno','narrative'))],type="class")
  table(simple[701:1019,4], predicted = rf.predictions)
  
  # BOOSTING
  adaboost.pred$confusion
  adaboost_test = cbind(simple[701:1019,], adaboost.pred$class)
  names(adaboost_test)[names(adaboost_test) == 'adaboost.pred$class'] = 'adaboost'
}

################################################################################

# NOW PERFORM THE FINAL ALGORITHM WITH REAL ACCIDENTS DATA FOR CLASSIFICATION

if (data.type == "real accidents data") {
  
  # USE BOOSTING TO CLASSIFY REAL ACCIDENTS DATA WITH UNKNOWN "MR" STATUS
  
  set.seed(625)
  
  # Implement Adaptive Boosting
  mr.adaboost = boosting(MR ~ . , data = simple[simple$type!="unclassified",!(names(simple) %in% c('documentno','narrative','type','mineid'))], 
                         boos = T, mfinal = 300, coeflearn = 'Freund')
  
  # Generate predictions
  adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[simple$type=="unclassified",!(names(simple) %in% c('documentno','narrative','type','mineid'))])
  
  # Apply predictions to unclassified injuries
  accidents.data = cbind(simple[simple$type=="unclassified",], adaboost.pred$class)
  names(accidents.data)[names(accidents.data) == 'adaboost.pred$class'] = 'adaboost'
  
  ################################################################################
  
  # POST-PROCESSING
  
  # Merge back in the rest of the variables from the original accidents data     
  accidents.original = readRDS(accidents.data.file.name)
  accidents.data = merge(accidents.data, accidents.original, by = "documentno", all = TRUE)
  
  # Clean up variable names from the merge
  rm(train, simple, simple.data, mr.fatalities, accidents.original)
  accidents.data = accidents.data[, c(-grep("\\.x", names(accidents.data)))]
  names(accidents.data) = gsub("\\.[x|y]", "", names(accidents.data))
  
  # Now manually weed out false positives and negatives that could not have been foreseen in the training data 
  accidents.data$manual.predict = ifelse(((accidents.data$likely.activy == 1 & 
                                             accidents.data$likely.class == 1 & 
                                             accidents.data$false.keyword == 0) |
                                            (accidents.data$likely.occup == 1 & 
                                               (accidents.data$maybe.activy == 1 | 
                                                  accidents.data$likely.activy == 1 | 
                                                  accidents.data$likely.class == 1 | 
                                                  accidents.data$maybe.keyword == 1)) |
                                            (accidents.data$likely.activy == 1 & 
                                               (accidents.data$maybe.occup == 1 | 
                                                  accidents.data$likely.class == 1 | 
                                                  accidents.data$maybe.keyword == 1)) |
                                            (accidents.data$maybe.occup == 1 & 
                                               ((accidents.data$maybe.activy == 1 & 
                                                   accidents.data$likely.class == 1) | 
                                                  (accidents.data$likely.class == 1 & 
                                                     accidents.data$maybe.keyword == 1) | 
                                                  (accidents.data$maybe.activy == 1 & 
                                                     accidents.data$maybe.keyword == 1))) |
                                            accidents.data$likely.keyword == 1) & 
                                           accidents.data$accident.only == 0, 1, 0)
  
  # Same process for false negatives 
  accidents.data[, "flashburn"] = ifelse(grepl("weld.{1,40}flash( |-)*burn", accidents.data[,"narrative"]) | 
                                           grepl("flash( |-)*burn.{1,40}weld", accidents.data[,"narrative"]), 1, 0)
  accidents.data$false.neg = ifelse(accidents.data$flashburn == 1 & accidents.data$adaboost == "NO", 1, 0)
  
  # Same process for false positives   
  accidents.data[, "carpal.tunnel"] = ifelse((grepl("carp(a|u|e)l( |-)*tun(n)*(e|l)(e|l)", accidents.data[,"narrative"]) | 
                                                grepl("bursitis", accidents.data[,"narrative"])) &
                                               !grepl("fracture", accidents.data[,"narrative"]), 1, 0)
  accidents.data[, "cumulative"] = ifelse(grepl("rep(e|i)t(e|a|i)ti(v|n)e", accidents.data[,"narrative"]) | 
                                            grepl("(cumulative|degenerativ)", accidents.data[,"narrative"]) |
                                            grepl("repeated(ed)*.{1,10}(mo(tion|vement)|trauma|irritation)", accidents.data[,"narrative"]) |
                                            grepl("long( |-)*term", accidents.data[,"narrative"]) | 
                                            grepl("slow( |-)*on( |-)*set", accidents.data[,"narrative"]), 1, 0)
  accidents.data[, "hearingloss"] = ifelse(grepl("hearing.los", accidents.data[,"narrative"]) | 
                                             grepl("los.{1,10}hearing", accidents.data[,"narrative"]) |
                                             grepl("n(o|i)(o|i)se exposur", accidents.data[,"narrative"]) |
                                             grepl("dimini.{1,10}hearing", accidents.data[,"narrative"]) |
                                             grepl("thr(e|i)s(h)*( |-)*hold( |-)*shift", accidents.data[,"narrative"]) |
                                             grepl("shift.{1,4}change.{1,30}hear", accidents.data[,"narrative"]) |
                                             grepl("exposur(e)*.{1,20}noise", accidents.data[,"narrative"]), 1, 0)
  accidents.data[, "exposure"] = ifelse(grepl("((prolon|occupation|long( |-)*term).{1,8}exposur)|(exposur.{1,20}(noise|we(a)*ther))|p(n|h)e(n)*umo(n)*co(nio)*sis", accidents.data[,"narrative"]) | 
                                          accidents.data$natureofinjury == "pneumoconiosis,black lung", 1, 0)
  accidents.data[, "heartattack"] = ifelse(grepl("heart( |-)*at(t)*ac(k|h)", accidents.data[,"narrative"]) | 
                                             accidents.data$natureofinjury == "heart attack", 1, 0)
  accidents.data[, "unrelated"] = ifelse(grepl("not work relat", accidents.data[,"narrative"]) | 
                                           grepl("no.{1,20}(specific|single).{1,5}(accident|injury|indicent|exposure)", accidents.data[,"narrative"]) |  
                                           (grepl("no (accident|incident|injury)", accidents.data[,"narrative"]) & 
                                              !grepl("no (accident|incident|injury).{1,5}report", accidents.data[,"narrative"])), 1, 0)
  
  # Last ditch attempt to find likely verbs and nouns before dropping false positives 
  accidents.data[, "working.on"] = ifelse(grepl("(to work|workin(g)*)( |-)*(on|in|under|out|at)", accidents.data[,"narrative"]), 1, 0)
  accidents.data[, "barring"] = ifelse(grepl("barr(ed|ing).{1,10}(rock|motor)", accidents.data[,"narrative"]), 1, 0)
  accidents.data[, "otherverb"] = ifelse(grepl("( |^)patch", accidents.data[,"narrative"]) | 
                                           grepl("re(-)*(build|pack|fuel|assembl|lin)", accidents.data[,"narrative"]) | 
                                           grepl("(re)*adjust", accidents.data[,"narrative"]) | 
                                           grepl("(secure|unplug)", accidents.data[,"narrative"]) | 
                                           grepl("trouble( |-)*shoot", accidents.data[,"narrative"]) | 
                                           grepl("to drain", accidents.data[,"narrative"]) | 
                                           grepl("mod(i|y)f(y|ication)", accidents.data[,"narrative"]) | 
                                           grepl("(mount|splic|bolt|adjust|digg|drill|cutt|unload|dislodg|pump|lift|jack|lay|haul|spread|position|tap(p)*|air|oil|fuel|drain|hook)(ing|ign|ed)", accidents.data[,"narrative"]) |
                                           grepl("(mov|hang|chan|putt|load|pry|assembl|push|pul(l)*(l)*|swing|trim(m)*|carry|strip(p)*|torqu(e)*|shovel(l)*|plac|pick|dispos)(ing|ign|ed)", accidents.data[,"narrative"]) |
                                           grepl("(grind|tension|clip(p)*|notch|straighten|band|guid(e)*|throw|rotat|saw|apply|align|tear|(un)*screw|attach|latch|goug|clear|restor)(ing|ign|ed)", accidents.data[,"narrative"]) |
                                           grepl("(set(t)*|put(t)*|pump|tak|prim)(ing).{1,10}(tire|wheel|pump|oil|links|fuel)", accidents.data[,"narrative"]) | 
                                           grepl("tr(ied|ying|yign).{1,3}to.{1,3}(free|take( |-)*out|shut( |-)*down|position|start|lift|pry|press|rotate|roll|sep(e|a)rat|releas)", accidents.data[,"narrative"]) | 
                                           grepl("attempt(ed|ing)*.{1,3}to.{1,3}(free|take( |-)*out|position|shut( |-)*down|pry|lift|put|get|unplug|unstop|lay|clear|start|press|rotate|roll|sep(e|a)rat)", accidents.data[,"narrative"]), 1, 0)
  accidents.data[, "othernoun"] = ifelse(grepl("anti( |-)*freeze", accidents.data[,"narrative"]) | 
                                           grepl("sweeper", accidents.data[,"narrative"]), 1, 0)
  
  accidents.data$other.keyword = ifelse((accidents.data$otherverb == 1 | 
                                           accidents.data$othernoun == 1 |
                                           accidents.data$trash == 1 | 
                                           accidents.data$wrench == 1 |
                                           accidents.data$moretools == 1 | 
                                           accidents.data$loosen == 1 |
                                           accidents.data$tire == 1 | 
                                           accidents.data$splice == 1 |
                                           accidents.data$working.on == 1 | 
                                           accidents.data$barring == 1) &
                                          accidents.data$accident.only == 0, 1, 0)
  
  # Flag definitely and likely false positives (including accident-only observations)
  accidents.data$false.pos = ifelse((accidents.data$carpal.tunnel == 1 | 
                                       accidents.data$cumulative == 1 | 
                                       accidents.data$heartattack == 1 |
                                       accidents.data$hearingloss == 1 | 
                                       accidents.data$exposure == 1 |
                                       accidents.data$unrelated == 1 | 
                                       accidents.data$accident.only == 1) & 
                                      accidents.data$adaboos == "YES", 1, 0)
  accidents.data$false.pos = ifelse(accidents.data$adaboos == "YES" & 
                                      accidents.data$likely.keyword == 0 &
                                      accidents.data$maybe.keyword == 0 & 
                                      accidents.data$other.keyword == 0, 1, accidents.data$false.pos)  
  
  # format classifications
  accidents.data$MR  = ifelse((accidents.data$adaboost == "YES" & 
                                 accidents.data$false.pos == 0) | 
                                accidents.data$false.neg == 1, 1, 0)
  
  # remove unessential variables
  accidents.data = accidents.data[, c(match("MR", names(accidents.data)),
                                      match("mineid", names(accidents.data)),
                                      match("accidentdate", names(accidents.data)),
                                      match("documentno", names(accidents.data)))]
  
  # merge on predictions from training obs
  accidents.data = merge(accidents.data, mr.data[,c("documentno", "MR")], by = "documentno", all = F)
  accidents.data$MR.y = ifelse(accidents.data$MR.y == "YES", "1", "0")
  accidents.data$MR = ifelse(!is.na(accidents.data$MR.x), accidents.data$MR.x, accidents.data$MR.y)
  
  # remove unessential variables
  accidents.data = accidents.data[, c(-grep("MR\\.[x|y]", names(accidents.data)))]
  
  # save a CSV file
  write.csv(accidents.data, file = classified.accidents.file.name.csv)
  
  # save R dataset
  saveRDS(accidents.data, file = classified.accidents.file.name)
}

################################################################################

rm(list = ls())

################################################################################

