# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Classify PS (Pinning and Striking)
  # This file loads the PS training set sent to the Morantz team by Miguel Reyes on January 29th, 2016 for use 
  # in constructing a pinning and striking (PS) injury classification algorithm. In this file, we clean and 
  # format the variables in the training set, conduct narrative analysis on the injury description fields, 
  # generate key word flags, and group existing categorical variables by how likely they are to correctly
  # classify injuries as either PS or not-PS. This file contains the code that was used to test the relative
  # classification power of various machine learning algorithms including CaRT, Random Forest decision trees, 
  # and Apadtive Boosting. If this file is run "as-is", it will employ an Adaptive Boosting algorithm to 
  # classify all injuries in the "Accidents Injuries Data Set" as PS/not-PS.

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/11/17

######################################################################################################

library(tree)
library(randomForest)
library(ggplot2)
library(reshape2)
library(pROC)
library(ROSE)
library(rpart)
library(rpart.plot)
library(adabag)
library(DMwR)
library(caret)
library(dummies)
library(stringr)

######################################################################################################

# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# Define file paths
originals.input.path = paste0(root, "/0_originals", collapse = NULL)
clean.input.path = paste0(root, "/1_cleaned", collapse = NULL) 
coded.output.path = paste0(root, "/3_coded", collapse = NULL)

# inputs
  # coded PS training set - sent to the Morantz team by NIOSH on 8/28/2015
training.set.file.name = paste0(originals.input.path, "/training-sets/Training_Set_Pinning_And_Striking_Accidents-January-29-2016.csv", collapse = NULL)
  # all accidents data, unclassified, cleaned in 2_clean_accidents.R and merged on mines in 3_merge_accidents.R
accidents.data.file.name = paste0(clean.input.path, "/clean_accidents.rds", collapse = NULL)

# outputs
  # all accidents, now classified as PS after algorithm (R dataset)
classified.accidents.file.name = paste0(coded.output.path, "/PS_accidents_with_predictions.rds", collapse = NULL)
  # all accidents, now classified as PS after algorithm (csv)
classified.accidents.file.name.csv = paste0(coded.output.path, "/PS_accidents_with_predictions.csv", collapse = NULL)

# Create file paths (recursive = TRUE will create this file structure if it does not exist)
dir.create(coded.output.path, recursive = TRUE)

######################################################################################################

# File preferences (these were used in algorithm testing but have now been hard-coded in, in accordance
# with the algorithm that we determined to be most successful)

# Data type - either "training data" for model selection and testing, or "real accidents data" for classification
data.type = "real accidents data"

##################################################################################################

# LOAD IN DATA

# Load in training set
ps.data = read.csv(training.set.file.name)

# Load in real accidents data (unclassified as PS or not-PS) for classification
if (data.type == "real accidents data") {
  accidents.data = readRDS(accidents.data.file.name)
}

##################################################################################################

# RENAME AND FORMAT VARIABLES

# Drop anything missing mine ID (not a real observation)
ps.data = ps.data[!is.na(ps.data$mineid),]

# Make all string variables lowercase
names(ps.data)[names(ps.data) == 'narrativemodified'] = 'narrative'
ps.data[, "narrative"] = tolower(ps.data[, "narrative"])
ps.data$mineractivity = tolower(ps.data$mineractivity)
ps.data$natureofinjury = tolower(ps.data$natureofinjury)
ps.data$degreeofinjury = tolower(ps.data$degreeofinjury)
ps.data$sourceofinjury = tolower(ps.data$sourceofinjury)
ps.data$accidenttype = tolower(ps.data$accidenttype)
ps.data$accidentclassification = tolower(ps.data$accidentclassification)
ps.data$accidenttype = tolower(ps.data$accidenttype)
ps.data$bodypart = tolower(ps.data$bodypart)
ps.data$typeofequipment = tolower(ps.data$typeofequipment)
ps.data$occupation = tolower(ps.data$occupation)
ps.data$mineractivity = tolower(ps.data$mineractivity)

# Format fields as characters
ps.data[, "narrative"] = as.character(ps.data[, "narrative"])
ps.data[, "occupcode3digit"] = as.character(ps.data[, "occupcode3digit"])
ps.data[, "occupation"] = as.character(ps.data[, "occupation"])
ps.data[, "returntoworkdate"] = as.character(ps.data[, "returntoworkdate"])

# This is used for identifying training from testing observations, or training from real observations 
ps.data[, "type"] = "training" 

# Format PS indicator as a binary factor
ps.data[, "X"] = factor(ifelse(ps.data[, "X"] == 1, "YES", "NO"))
names(ps.data)[names(ps.data) == "X"] = "PS"

##################################################################################################

# DO THIS CODE IF YOU'RE RUNNING ON THE REAL ACCIDENTS DATA (NOT THE TRAINING SET)

if (data.type == "real accidents data") {
  
  # make training set and accidents data compatible to append
  drops = c("closed_doc_no",
            "fiscalyear", 
            "fiscalquarter", 
            "investigationbegindate")
  accidents.data = accidents.data[, !(names(accidents.data) %in% drops)] 
  accidents.data[, "PS"] = ""
  accidents.data[, "type"] = "unclassified" 
  
  # drop any remaining variables not common to the real accidents data and mr.data (training set)
  accident.names = names(accidents.data)
  ps.data = ps.data[, names(ps.data) %in% accident.names]
  
  # create lists of document numbers from each dataset 
  ps.docnos = ps.data$documentno
  ps.docnos = as.character(ps.docnos)
  accident.docnos = accidents.data$documentno
  
  # identify common document numbers
  keep.docnos = setdiff(accident.docnos, ps.docnos) 
  
  # save a dataset of all accidents
  all.accidents = accidents.data 
  
  # remove observations from accidents data present in the ps.data (training set)
  accidents.data = accidents.data[which(accidents.data$documentno %in% keep.docnos),]
  
  # append dataset of training observations and real accidents for classification - should be unique on document number
  ps.data = rbind(ps.data, accidents.data)
  rm(accidents.data)
}

##################################################################################################

# CLEAN UP THE NARRATIVE FIELDS 

# 23 narrative fields are polluted with other columns - split and replace these  
ps.data[, "messy"] = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", ps.data[,"narrative"]), 1, 0)
narrative_split = strsplit(ps.data[ps.data$messy == 1, "narrative"], "|", fixed = T)
messy_rows = row.names(ps.data[ps.data$messy == 1, ])
for (i in 1:length(messy_rows)) {
  ps.data[messy_rows[i], "narrative"] = unlist(narrative_split[i])[1]
  ps.data[messy_rows[i], "occupcode3digit"] = unlist(narrative_split[i])[2]
  ps.data[messy_rows[i], "occupation"] = unlist(narrative_split[i])[3]
  ps.data[messy_rows[i], "returntoworkdate"] = unlist(narrative_split[i])[4]
}
ps.data = ps.data[, c(-match("messy", names(ps.data)))]

# Deal with messy number typos in the narrative fields
ps.data[, "numbertypo"] = ifelse(grepl("[a-z][0-9][a-z]", ps.data[,"narrative"]), 1, 0)
for (i in 0:9) {
  ps.data[ps.data$numbertypo == 1,]$narrative = gsub(i, "", ps.data[ps.data$numbertypo == 1,]$narrative)
}

# Clean up common typos that may affect our keyword searches
ps.data$narrative = gsub("ag(a)*( )*(in)*st", "against", ps.data$narrative)

##################################################################################################

# CLEAN UP OTHER VARIABLES & RECODE MISCLASSIFIED OBSERVATIONS FROM THE TRAINING SET

# Recoded in light of Miguel's 5/27/16 response to our questions
ps.data$PS[ps.data$documentno=="219891280164"] = "YES"
ps.data$PS[ps.data$documentno=="219852170075"] = "YES"
ps.data$PS[ps.data$documentno=="219901620109"] = "YES"
ps.data$PS[ps.data$documentno=="220011070020"] = "NO"
ps.data$PS[ps.data$documentno=="219892570061"] = "NO"
ps.data$PS[ps.data$documentno=="219893100251"] = "NO"
ps.data$PS[ps.data$documentno=="219872990054"] = "NO"
ps.data$PS[ps.data$documentno=="219983280016"] = "NO"
ps.data$PS[ps.data$documentno=="220082800043"] = "NO"
ps.data$PS[ps.data$documentno=="219830320021"] = "NO"
ps.data$PS[ps.data$documentno=="219912970040"] = "NO"
ps.data$PS[ps.data$documentno=="219942900032"] = "NO"
ps.data$PS[ps.data$documentno=="219982380025"] = "NO"

# Recoded in light of Miguel's 6/7/16 response to our questions
ps.data$PS[ps.data$documentno=="219912970040"] = "YES"
ps.data$PS[ps.data$documentno=="219871460076"] = "NO"
ps.data$PS[ps.data$documentno=="219861280065"] = "NO"
ps.data$PS[ps.data$documentno=="220000310115"] = "NO"
ps.data$PS[ps.data$documentno=="220001180052"] = "NO"
ps.data$PS[ps.data$documentno=="219831430047"] = "NO"
ps.data$PS[ps.data$documentno=="219943180016"] = "NO"
ps.data$PS[ps.data$documentno=="220112090013"] = "NO"

# Recoded on 9/13/2016 in light of Miguel's lack of response to our questions
ps.data$PS[ps.data$documentno=="220090630033"] = "YES"
ps.data$PS[ps.data$documentno=="220050800006"] = "YES"
ps.data$PS[ps.data$documentno=="219892210062"] = "YES"
ps.data$PS[ps.data$documentno=="219950870035"] = "YES"
ps.data$PS[ps.data$documentno=="219972890025"] = "YES"
ps.data$PS[ps.data$documentno=="219930390025"] = "NO"
ps.data$PS[ps.data$documentno=="219992320012"] = "NO"
ps.data$PS[ps.data$documentno=="219853190080"] = "NO"
ps.data$PS[ps.data$documentno=="219973490121"] = "NO"
ps.data$PS[ps.data$documentno=="219852050003"] = "NO"
ps.data$PS[ps.data$documentno=="219891140147"] = "NO"
ps.data$PS[ps.data$documentno=="220020100051"] = "NO"

# Destring these numeric variables
ps.data[,grep("numberofemployees", names(ps.data))] = gsub(pattern = ",",replacement =  "", ps.data[,grep("numberofemployees", names(ps.data))])
ps.data[,grep("numberofemployees", names(ps.data))] = as.numeric(ps.data[,grep("numberofemployees", names(ps.data))])
ps.data[,grep("methaneliberation", names(ps.data))] = gsub(pattern = ",",replacement =  "", ps.data[,grep("methaneliberation", names(ps.data))])
ps.data[,grep("methaneliberation", names(ps.data))] = as.numeric(ps.data[,grep("methaneliberation", names(ps.data))])
ps.data[,grep("averagemineheight", names(ps.data))] = gsub(pattern = ",",replacement =  "", ps.data[,grep("averagemineheight", names(ps.data))])
ps.data[,grep("averagemineheight", names(ps.data))] = as.numeric(ps.data[,grep("averagemineheight", names(ps.data))])

# Merge redundant "not-found" values within variables. Note: Values like "Unknown" or "Other" are not funneled into "No Value Found"
ps.data[, "uglocation"] = ifelse(ps.data[, "uglocation"] == "NOT MARKED", "NO VALUE FOUND", ps.data[, "uglocation"])
ps.data[, "accidenttype"] = ifelse(ps.data[, "accidenttype"] == "not elsewhereclassified", "no value found", ps.data[, "accidenttype"])
ps.data[, "immediatenotificationclass"] = ifelse(ps.data[, "immediatenotificationclass"] == "NOT MARKED", "NO VALUE FOUND", 
                                                 ps.data[, "immediatenotificationclass"])
ps.data[, "natureofinjury"] = ifelse(ps.data[, "natureofinjury"] == "unclassified,not determed", "no value found", ps.data[, "natureofinjury"])
ps.data[, "equipmanufacturer"] = ifelse(ps.data[, "equipmanufacturer"] == "Not Reported", "NO VALUE FOUND", ps.data[, "equipmanufacturer"])

# Convert date variables. We drop date variables now, but eventually will make use of them.
indices_with_date = grep("date", names(ps.data))
for (i in indices_with_date) {
  ps.data[,i] = as.Date(ps.data[,i], "%m/%d/%Y")
}

# Convert accident type codes to factor to make this code usable with accidents data
ps.data$accidenttypecode = as.factor(ps.data$accidenttypecode)

##################################################################################################

# GENERATE LIKELY POSITIVELY PREDICTIVE KEY WORDS

# Flag "pin" but not "pinion," "pinner," "pinning top," or "pinned himself"
ps.data[, "pin"] = ifelse(grepl("(^| )pin(n*)(e|i)[a-z]+", ps.data[,"narrative"]) &
                            !grepl("(^| )pinion", ps.data[,"narrative"]) &
                            !grepl("(^| )pinner", ps.data[,"narrative"]) &
                            !grepl("pinn(ing|ed)( ).{1,5}top", ps.data[,"narrative"]) &
                            !grepl("pinn(ing|ed).{1,5}(him|his|her)self", ps.data[,"narrative"]), 1, 0)
ps.data[, "strike"] = ifelse(grepl("str(i|u)(.*)k[a-z]*", ps.data[,"narrative"]) &
                               !grepl("str(i|u)(.*)k[a-z]*.{1,6}head", ps.data[,"narrative"]) &
                               !grepl("head.{1,6}str(i|u)(.*)k[a-z]*", ps.data[,"narrative"]), 1, 0)
ps.data[, "trap"] = ifelse(grepl("( )trap[a-z]*", ps.data[,"narrative"]), 1, 0)
ps.data[, "collided"] = ifelse(grepl("col(l)*i(de|ded|sion|ssion)", ps.data[,"narrative"]), 1, 0)

# Generate maybe likely positively predictive key words
ps.data[, "ranover"] = ifelse(grepl("( |^)r(a|u)n( )*(over|into)", ps.data[,"narrative"]), 1, 0)
ps.data[, "rolled"] = ifelse(grepl("rolled( )*(over|into|onto|on|down)", ps.data[,"narrative"]), 1, 0)
ps.data[, "between"] = ifelse(grepl("between", ps.data[,"narrative"]) | 
                                grepl("btwn", ps.data[,"narrative"]), 1, 0)
ps.data[, "wheel"] = ifelse(grepl("wheel", ps.data[,"narrative"]) & 
                              !grepl("wheeler", ps.data[,"narrative"]), 1, 0)
ps.data[, "by"] = ifelse(grepl("by", ps.data[,"narrative"]), 1, 0)

# Generate likely negatively predictive key words

# jarred, jolted, jostled
ps.data[, "jarring"] = ifelse(grepl("jar(r)*(ed|ing)", ps.data[,"narrative"]) |
                                grepl("jo(lt|stl)(ed|ing)", ps.data[,"narrative"]), 1, 0)
ps.data[, "bounced"] = ifelse(grepl("boun(c)*( )*(e|ing)", ps.data[,"narrative"]), 1, 0)
# avoid sprocket, rockduster, etc
ps.data[, "rock"] = ifelse((grepl("rock( |$|\\.|s|,)", ps.data[,"narrative"]) & 
                              !grepl("rock( )*dust", ps.data[,"narrative"])), 1, 0)
ps.data[, "digit"] = ifelse(grepl("(finger(s)*|pinky|hand(s)*|thumb|hand( |\\.|,|$))", ps.data[,"narrative"]), 1, 0)
ps.data[, "derail"] = ifelse((grepl("(left|off|jumped).{1,15}track", ps.data[,"narrative"]) & 
                                !grepl("(left|off|jumped).{1,15}track.{1,3}switch", ps.data[,"narrative"])) | 
                               grepl("derai", ps.data[,"narrative"]), 1, 0)
ps.data[, "steering"] = ifelse(grepl("ste(e|a)ring( )*wheel.{1,15}sp(u|i)n", ps.data[,"narrative"]), 1, 0)

# Generate less good negative key words
ps.data[, "wrench"] = ifelse(grepl("wrench", ps.data[,"narrative"]), 1, 0)
ps.data[, "controls"] = ifelse(grepl("(lever|stick)", ps.data[,"narrative"]), 1, 0)
ps.data[, "resin"] = ifelse(grepl("resin", ps.data[,"narrative"]), 1, 0)
ps.data[, "atrs"] = ifelse(grepl("a(\\.)*t(\\.)*r(\\.)*s(\\.)*", ps.data[,"narrative"]), 1, 0)
ps.data[, "flew"] = ifelse(grepl("fl(ew|y|ing)", ps.data[,"narrative"]), 1, 0)
ps.data[, "loose"] = ifelse(grepl("loose", ps.data[,"narrative"]), 1, 0)
ps.data[, "broke"] = ifelse(grepl("br(oke|eak)", ps.data[,"narrative"]), 1, 0)
ps.data[, "bent"] = ifelse(grepl("bent", ps.data[,"narrative"]) & 
                             !grepl("bent( )*over", ps.data[,"narrative"]), 1, 0)
ps.data[, "canopy"] = ifelse(grepl("canopy", ps.data[,"narrative"]), 1, 0)

##################################################################################################

# GENERATE KEY WORDS TO IDENTIFY FALSE POSITIVES

# Use body/seat to remove false positive accidents of someone being jostled against the seat
ps.data[, "bodyseat"] = ifelse(grepl("(back|head|neck|shoulder|elbo).{1,10}seat", ps.data[,"narrative"]) &
                                 !grepl("backward.{1,10}seat", ps.data[,"narrative"]) &
                                 !grepl("(bolt|over|drill)( )*head.{1,10}seat", ps.data[,"narrative"]), 1, 0) 
# Hitting head against canopy
ps.data[, "headcanopy"] = ifelse((grepl("(head|neck).{1,5}(on|str(ike|uck)|hit|against).{1,5}(canopy)", ps.data[,"narrative"]) |
                                    grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(canopy)", ps.data[,"narrative"])) &
                                   !grepl("drill( )*head.{1,10}canopy", ps.data[,"narrative"]) &
                                   !grepl("over( )*head.{1,10}canopy", ps.data[,"narrative"]) &
                                   !grepl("head(ing|er|ed).{1,10}canopy", ps.data[,"narrative"]), 1, 0) 
# Going over a bump and operator hitting head 
ps.data[, "hole"] = ifelse(grepl("(hit|str(ike|uck)|r(a|u)n( )*over|(went|go)( )*over).{1,10}(rock|hole|(h|b)ump(s| |$|\\.|,)|dip|depression|uneven|off( |-)*set|((low|bad|rough)( |-)*(spot|patch|place)))", 
                                 ps.data[,"narrative"]), 1, 0)
ps.data[, "unevenbottom"] = ifelse(grepl("(hole|bump(s| |$|\\.|,)|dip|depression|uneven|off( |-)*set|((low|bad|rough)( |-)*(spot|place|patch)))", 
                                         ps.data[,"narrative"]) & 
                                     !grepl("(bolt|drill|steel|cable|test|pin).{1,15}hole", ps.data[,"narrative"]), 1, 0)

# GENERATE KEY WORDS TO IDENTIFY ROOFBOLTING ACCIDENTS AND THEIR FALSE POSITIVES

# roof bolting/drilling steel injuries
ps.data[, "drillsteel"] = ifelse(grepl("drill.{1,5}steel", ps.data[,"narrative"]) & 
                                   grepl("(between|btwn).{1,17}steel.{1,25}(drill|head|roof|guide|canopy|ring)", ps.data[,"narrative"]), 1, 0)
# drill steel breaking/bending during roofbolting and caught an injury is not considered PS
ps.data[, "brokensteel"] = ifelse(grepl("drill.{1,5}steel", ps.data[,"narrative"]) & 
                                    (grepl("(drill|roof).{1,5}(steel|bolt).{1,15}(burst|ben(t|d)|br(eak|oke)|loose|drop(ped|ping)*|c(a|o)*me( )*( )*out|f(a|e)ll|stuck|clog)", ps.data[,"narrative"]) |
                                       grepl("wrench.{1,5}(slip|c(a|o)me).{1,5}off.{1,15}(bolt|drill head)", ps.data[,"narrative"]) |  
                                       grepl("wrench.{1,15}broke", ps.data[,"narrative"])), 1, 0)
ps.data[, "roofbolt"] = ifelse(grepl("(roof|( |^)rib).{1,10}bolt", ps.data[,"narrative"]) | 
                                 grepl("(roof|rib).{1,25}bolting", ps.data[,"narrative"]) |
                                 grepl("roof.{1,35}bolting", ps.data[,"narrative"]) |
                                 grepl("bolt.{1,10}instal", ps.data[,"narrative"]) |
                                 grepl("instal.{1,20}bolt", ps.data[,"narrative"]), 1, 0)
ps.data[, "bolting"] = ifelse(grepl("bolting", ps.data[,"narrative"]) |
                                grepl("put(t)*(ing)*( )*bolt.{1,10}top", ps.data[,"narrative"]), 1, 0)
# gloves getting caught during roof bolting counts as "entrapment" and should not be marked as PS
ps.data[, "entrapment"] = ifelse((grepl("drill.{1,5}steel", ps.data[,"narrative"]) | 
                                    ps.data$roofbolt == 1 |
                                    ps.data$bolting == 1 ) & 
                                   (grepl("(caught|catching|snagg(ed|ing)|grab).{1,10}(glove|shirt|sleeve)", ps.data[,"narrative"]) | 
                                      grepl("(glove|shi(r)*t|sle(e)*ve).{1,10}(entangl|cau( )*ght|catching|snagg(ed|ing))", ps.data[,"narrative"])), 1, 0)
ps.data[ps.data$glove == 1 & ps.data$entrapment == 0, c("narrative","drillsteel", "roofbolt")]

##################################################################################################

# CREATE DUPLICATE NARRATIVE FIELDS AND THEN REPLACE ALL MENTIONS OF VEHICLES WITH "VEHICLE", BODY PARTS WITH "BODY", ETC.

# rename narrative variable
ps.data$old_narrative = ps.data$narrative

# VEHICLE
ps.data$narrative = gsub("(man|ram|s(ch|h)uttle|scoop)( |-|- |v)*(trip|car)( car)*", "VEHICLE1", ps.data$narrative)
ps.data$narrative = gsub("( |^)car( |-|s|\\.|,|$)", " VEHICLE2 ", ps.data$narrative)
ps.data$narrative = gsub("(m|a)(m|a)n( |-|- )*bus", "VEHICLE3", ps.data$narrative)
ps.data$narrative = gsub("vehic(l|e)(l|e)", "VEHICLE4", ps.data$narrative)
ps.data$narrative = gsub("person(n)*(e|a)l carrier", "VEHICLE5", ps.data$narrative)
ps.data$narrative = gsub("wheeler", "VEHICLE6", ps.data$narrative)
ps.data$narrative = gsub("scooter", "VEHICLE7", ps.data$narrative)
ps.data$narrative = gsub("s(ch|h)uttle", "VEHICLE8", ps.data$narrative)
ps.data$narrative = gsub("cricket ", "VEHICLE9", ps.data$narrative)
ps.data$narrative = gsub("(rock|roof)( |-)*bolter", "VEHICLE10", ps.data$narrative)
ps.data$narrative = gsub("( |^)truck", " VEHICLE11", ps.data$narrative)
ps.data$narrative = gsub("buggy", "VEHICLE12", ps.data$narrative)
ps.data$narrative = gsub("stam(m)*ler", "VEHICLE13", ps.data$narrative)
ps.data$narrative = gsub("mac( |-)*(8|eight)", "VEHICLE14", ps.data$narrative)
ps.data$narrative = gsub("(3|three)( |-)*wh(ee)*l(e)*r", "VEHICLE15", ps.data$narrative)
ps.data$narrative = gsub("(c)*ont.{1,10}mi(e)*n(r|er|ing)", "VEHICLE16", ps.data$narrative)
ps.data$narrative = gsub("long( |-)*wall", "VEHICLE17", ps.data$narrative)
ps.data$narrative = gsub("load( |-)*haul(-| )*dump", "VEHICLE18", ps.data$narrative)
ps.data$narrative = gsub("(mining|miner|minr|loading|(roof)*( )*bolt(ing)*)( )*machine", "VEHICLE19", ps.data$narrative)
ps.data$narrative = gsub("tunnel( |-)*borer", "VEHICLE20", ps.data$narrative)
ps.data$narrative = gsub("fork( |-)*lift", "VEHICLE21", ps.data$narrative)
ps.data$narrative = gsub("(front( |-)*end|scraper)( )*loader", "VEHICLE22", ps.data$narrative)
ps.data$narrative = gsub("locomotiv(e)*", "VEHICLE23", ps.data$narrative)
ps.data$narrative = gsub("(road|motor)( |-)*grader", "VEHICLE24", ps.data$narrative)
ps.data$narrative = gsub("motor", "VEHICLE25", ps.data$narrative)
ps.data$narrative = gsub("tractor", "VEHICLE26", ps.data$narrative)
ps.data$narrative = gsub("jeep", "VEHICLE27", ps.data$narrative)
ps.data$narrative = gsub("(ore)*( |-)haul(er|age)", "VEHICLE28", ps.data$narrative)
ps.data$narrative = gsub("rail( |-)*runner", "VEHICLE29", ps.data$narrative)
ps.data$narrative = gsub("feeder", "VEHICLE30", ps.data$narrative)
ps.data$narrative = gsub("s/c", "VEHICLE31", ps.data$narrative)
ps.data$narrative = gsub("shearer", "VEHICLE32", ps.data$narrative)
ps.data$narrative = gsub("mucker", "VEHICLE33", ps.data$narrative)
ps.data$narrative = gsub("eimco", "VEHICLE34", ps.data$narrative)
ps.data$narrative = gsub("jitney", "VEHICLE35", ps.data$narrative)
ps.data$narrative = gsub("bolter", "VEHICLE36", ps.data$narrative)
ps.data$narrative = gsub("rail( |-)*runner", "VEHICLE37", ps.data$narrative)
ps.data$narrative = gsub("mobile", "VEHICLE38", ps.data$narrative)
ps.data$narrative = gsub("porta(l)*( |-)*bus", "VEHICLE39", ps.data$narrative)
ps.data$narrative = gsub("( |^|-)bus(es| |\\.|,|$)", " VEHICLE40 ", ps.data$narrative)
ps.data[!grepl("troll(e)*y( )*pol(e|l)", ps.data[,"narrative"]),]$narrative = gsub("trol(l)*(e)*y", " VEHICLE41 ", ps.data[!grepl("troll(e)*y( )*pol(e|l)", ps.data[,"narrative"]),]$narrative)
ps.data[!grepl("to trip", ps.data[,"narrative"]),]$narrative = gsub("( |^)trip( |$|,|\\.)", " VEHICLE42 ", ps.data[!grepl("to trip", ps.data[,"narrative"]),]$narrative)
ps.data[!grepl("scoop(er|ing)", ps.data[,"narrative"]),]$narrative = gsub("scoop", " VEHICLE43 ", ps.data[!grepl("scoop(er|ing)", ps.data[,"narrative"]),]$narrative)
ps.data[!grepl("to tram", ps.data[,"narrative"]) & 
          !grepl("tram.{1,5}(lever|sprocket|pedal|chain)", ps.data[,"narrative"]),]$narrative = gsub("tram( |$|\\.|,)", " VEHICLE44 ", ps.data[!grepl("to tram", ps.data[,"narrative"]) & 
                                                                                                                                                 !grepl("tram.{1,5}(lever|sprocket|pedal|chain)", ps.data[,"narrative"]),]$narrative)
ps.data$narrative = gsub("mucker", "VEHICLE45", ps.data$narrative)
ps.data[, "shuttlecar_or_rbolter"] = ifelse((grepl("VEHICLE(8|10|36)", ps.data$narrative) | 
                                               grepl("(s(ch|h)uttle).{1,30}( |-|- |v)*(trip|car)( car)*", ps.data$old_narrative)), 1, 0)

# BODY PARTS
ps.data$narrative = gsub("hand(s| |\\.|,|$)", "BODY ", ps.data$narrative)
ps.data$narrative = gsub("finger(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("thumb", "BODY", ps.data$narrative)
ps.data$narrative = gsub("ankle(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("shoulder(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("knee( |s|\\.|,|$)", "BODY ", ps.data$narrative)   	# AVOID "KNEEL"
ps.data$narrative = gsub("wrist(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("cal(f|ve|ves)", "BODY", ps.data$narrative)
ps.data$narrative = gsub("( |^)leg(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("eye(lid|brow|s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("cheek(s|bones)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("bone(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("( |^)lip(s)*", " BODY", ps.data$narrative)
ps.data$narrative = gsub("( |^)ear(s)*", " BODY", ps.data$narrative)
ps.data$narrative = gsub("chin( |$|\\.|,)", "BODY", ps.data$narrative)
ps.data$narrative = gsub("neck", "BODY", ps.data$narrative)
ps.data$narrative = gsub("(^| )(fore|for)*arm", " BODY", ps.data$narrative)
ps.data$narrative = gsub("mouth", "BODY", ps.data$narrative)
ps.data$narrative = gsub("nose( |s|\\.|,|$)", "BODY ", ps.data$narrative)
ps.data$narrative = gsub("pelvis", "BODY", ps.data$narrative)
ps.data$narrative = gsub("chest", "BODY", ps.data$narrative)
ps.data$narrative = gsub("groin", "BODY", ps.data$narrative)
ps.data$narrative = gsub("(t|f)ibia", "BODY", ps.data$narrative)
ps.data$narrative = gsub("ulna", "BODY", ps.data$narrative)
ps.data$narrative = gsub("radia", "BODY", ps.data$narrative)
ps.data$narrative = gsub("rib( |-)*cage", "BODY", ps.data$narrative)
ps.data$narrative = gsub("buttock(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("spine", "BODY", ps.data$narrative)
ps.data$narrative = gsub("elbow(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("testicle(s)*", "BODY", ps.data$narrative)
ps.data$narrative = gsub("t(ee|oo)th", "BODY", ps.data$narrative) # Check to make sure no equipment has teeth/tooth
ps.data$narrative = gsub("(top|bottom) of|(r(igh)*t|l(e)*ft|his|her|both|onto|r\\.)( )*(foot|feet)", "BODY", ps.data$narrative)
ps.data$narrative = gsub("( |^)hip(s)*", " BODY", ps.data$narrative)
ps.data[!grepl("backward", ps.data[,"narrative"]),]$narrative = gsub("(lowe(r)*|upper|PERSON|the|strain).{1,8}back", " BODY", ps.data[!grepl("backward", ps.data[,"narrative"]),]$narrative)
ps.data[!grepl("drill.{1,5}head", ps.data[,"narrative"]) & 
          !grepl("(over|cutter)( )*head", ps.data[,"narrative"]),]$narrative = gsub("(^| )head( |$|\\.|,)", " BODY ", ps.data[!grepl("drill.{1,5}head", ps.data[,"narrative"]) & 
                                                                                                                                !grepl("(over|cutter)( )*head", ps.data[,"narrative"]),]$narrative)
ps.data[!grepl("(coal|the).{1,5}face", ps.data[,"narrative"]) & 
          !grepl("surface", ps.data[,"narrative"]),]$narrative = gsub("face", "BODY", ps.data[!grepl("(coal|the).{1,5}face", ps.data[,"narrative"]) & 
                                                                                                !grepl("surface", ps.data[,"narrative"]),]$narrative)

# Generate some positive keywords using the body parts, before subbing in the pin/strike/trap masks
ps.data[, "bumped"] = ifelse((grepl("bump(ed|ing)( )*(over|into|onto)", ps.data[,"narrative"]) | 
                                grepl("bump(ed|ing).{1,10}BODY", ps.data[,"narrative"])) &
                               !grepl("bump(ed|ing).{1,10}head", ps.data[,"old_narrative"]), 1, 0)
ps.data[, "caught"] = ifelse(grepl("caught.{1,15}(between| in )", ps.data[,"old_narrative"]) |
                               grepl("caught.{1,10}BODY", ps.data[,"narrative"]) |
                               grepl("BODY.{1,6}caught", ps.data[,"narrative"]), 1, 0)
ps.data[, "hit"] = ifelse(grepl("( |^)hit.{1,5}(by|him|his|her|employee|ee)", ps.data[,"old_narrative"]) |
                            grepl("( |^)hit.{1,10}BODY", ps.data[,"narrative"]), 1, 0)
ps.data[, "dropped"] = ifelse(grepl("(lowe(r)*(ing|ed)*|drop(p)*(ing|ed)*).{1,15}(bucket|drill( |-)*head|drill( |-)*pod|pinner( |-)*head).{1,15}BODY", ps.data[,"narrative"]), 1, 0)
ps.data[, "neg_wrench"] = ifelse(ps.data$wrench == 1 & 
                                   (grepl("(burst|ben(t|d)|br(eak|oke)|loose|dislodge|shifted|drop(ped|ping)*|c(a|o)*me( )*( )*(out|off)|f(a|e)ll|stuck|clog|slipped)+", ps.data[, "old_narrative"]) | 
                                      ps.data$flew == 1 | 
                                      ps.data$caught == 1), 1, 0)

# PIN/STRIKE/TRAP
ps.data$narrative = gsub("( |^)pin(n)*(ed|ing)", " PINNED/STRUCK", ps.data$narrative)
ps.data$narrative = gsub("(s)*tr(u|i)(c)*k(e|ing)*", "PINNED/STRUCK", ps.data$narrative)
ps.data$narrative = gsub("r(a|u)n( )*(into|over)", "PINNED/STRUCK", ps.data$narrative)
ps.data$narrative = gsub("col(l)*ided( w| with)*", "PINNED/STRUCK", ps.data$narrative)
ps.data$narrative = gsub("( |^)trap(p)*(ed|ing)", " PINNED/STRUCK", ps.data$narrative)
ps.data$narrative = gsub("rolled (into|onto|over)", "PINNED/STRUCK", ps.data$narrative)
ps.data$narrative = gsub("c(a|u)(a|u)ght", "PINNED/STRUCK", ps.data$narrative)
ps.data$narrative = gsub("catching|to catch", "PINNED/STRUCK", ps.data$narrative)
ps.data[ps.data$hole == 0,]$narrative = gsub("( |^)hit(t)*(ing)*( |$|\\.|,|s)", "PINNED/STRUCK", 
                                             ps.data[ps.data$hole == 0,]$narrative)
ps.data[grepl("VEHICLE.{1,5}got on{1,5}BODY", ps.data[,"narrative"]),]$narrative = gsub("got on", "PINNED/STRUCK", 
                                                                                        ps.data[grepl("VEHICLE.{1,5}got on{1,5}BODY", ps.data[,"narrative"]),]$narrative)

# PERSON FLAGS
ps.data$narrative = gsub("( |^)e(e|mp|mpl|mploye)(e)*( |$|,|\\.)", " PERSON ", ps.data$narrative)
ps.data$narrative = gsub("( |^)i(,|\\.| )*name", " PERSON", ps.data$narrative)
ps.data$narrative = gsub("injured person", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("(him|her)self", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("vi(c)*tim", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("repairm(a|e)n", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("maintenance( )*m(a|e)n", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("( |^)m(a|e)n( |$|,|\\.)", " PERSON ", ps.data$narrative)
ps.data$narrative = gsub("fore(m|a)(a|e|m)n", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("ind(i)*v(idual)*(s)*", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("helper(s)*", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("person(s)*", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("worker(s)*", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("^inj(\\.)*(ured)*", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("the.{1,6}injured", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("injured was", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("( |^)(s)*he(r|rs)*( |$|,|\\.)", " PERSON ", ps.data$narrative)
ps.data$narrative = gsub("( |^)hi(s|m)( |$|,|\\.)", " PERSON ", ps.data$narrative)
ps.data$narrative = gsub("(wo)*m(a|e)n( |$|,|\\.)", "PERSON ", ps.data$narrative)

# These are less likely
ps.data$narrative = gsub("operat(o|e)r", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("passenger", "PERSON", ps.data$narrative)
ps.data$narrative = gsub("driver", "PERSON", ps.data$narrative)

##################################################################################################

# GENERATE VARIABLES TO COUNT NUMBER OF UPPERCASE WORDS PER NARRATIVE AND DISTANCES BETWEEN THEM

# Count the number of capital words in each string
ps.data$num.vehicles = str_count(ps.data$narrative, "VEHICLE")
ps.data$num.pinstrike = str_count(ps.data$narrative, "PINNED/STRUCK")
ps.data$num.person = str_count(ps.data$narrative, "PERSON")
ps.data$num.body = str_count(ps.data$narrative, "BODY")

# Create variable counting the number of unique vehicles mentioned in a narrative
uniq_vehcls = function(x) {
  return(length(unique(substr(unlist(regmatches(x, gregexpr("VEHICLE[0-9][0-9]*", x))), 8, 9))))
}
ps.data$num_unique_vehcl = sapply(ps.data$narrative, uniq_vehcls)
ps.data$mult_vehcl = ifelse(ps.data$num_unique_vehcl > 1, 1, 0)

# If two different types of vehicles are mentioned, it's much more likely to be a V-to-V striking accident
ps.data[, "dif_vehicle"] = ifelse(grepl("(second|another|different).{1,5}VEHICLE", ps.data[,"narrative"]), 1, 0)
ps.data[, "loose_rbolting"] = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", ps.data[,"narrative"]), 1, 0)
ps.data[, "drill_action"] = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", ps.data[,"narrative"]), 1, 0)

##################################################################################################

# CREATE A FEW MORE KEY WORDS ON THE NEW NARRATIVE FIELDS

# Define a few narratives using the vehicle flags - before replacing body parts 
ps.data[, "operating"] = ifelse((grepl("( |^|was|while|had)(tr(a)*m(m)*[^ ]{0,3}|op(e)*r(a)*t[^ ]{0,3}|backin.{1,10}VEHICLE|r(a|u)n|makin(g)*( )*(a)*( )*tu(r)*n|remote.{1,5}control|driv)", ps.data$narrative) &
                                   (!grepl("PERSON.{1,20}(splic(e|ing)|crawl(ing)*|repair|fix)", ps.data$narrative) & 
                                      grepl("(splic(e|ing)|crawl(ing)*|repair|fix).{1,20}PERSON", ps.data$narrative)) &
                                   (grepl("operat", ps.data$mineractivity) | 
                                      (grepl("roof bolt", ps.data$mineractivity) & 
                                         !grepl("help(ing|er|)", ps.data$old_narrative))) &
                                   (!grepl("(side of|right|left|beside).{1,10}VEHICLE", ps.data$narrative) | 
                                      grepl("remote.{1,5}control", ps.data$narrative))), 1, 0)

# Use head/roof to remove driver hitting head against vehicle roof - REQUIRES OPERATING
ps.data[, "headroof"] = ifelse((grepl("(head|neck).{1,5}(on|str(ike|uck)|hit|against).{1,5}(roof|top)", ps.data[,"old_narrative"]) |
                                  grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(roof|top)", ps.data[,"old_narrative"]) | 
                                  (grepl("whip( )*lash", ps.data[,"old_narrative"]) & 
                                     ps.data$operating == 1) | 
                                  grepl("jerked.{1,10}(head|neck)", ps.data[,"old_narrative"])) &
                                 !grepl("drill( )*head.{1,10}roof", ps.data[,"old_narrative"]) &
                                 !grepl("over( )*head.{1,10}roof", ps.data[,"old_narrative"]) &
                                 !grepl("head(ing|er|ed).{1,10}roof", ps.data[,"old_narrative"]) &
                                 !grepl("head.{1,10}roof.{1,5}bolt", ps.data[,"old_narrative"]), 1, 0) 

# Now create positive and negative roof bolting flags 
ps.data[, "pos_roofbolt"] = ifelse(ps.data$roofbolt == 1 & 
                                     grepl("PINNED/STRUCK.{1,15}between.{1,15}(roof( bolt)*|canopy|boom|(bolter|drill)( |-)*(pot|guide|head)|(drill|roof)*( |-)*(steel|guide)|( |^)rib|(bolt(er)*|torque)( |-)*wrench|lead|top).{1,30}(top|roof( bolt)*|canopy|boom|(bolter|drill)( |-)*(pot|guide|(h|l)ead)|(drill|roof)*( |-)*(steel|guide)|( |^)rib|(bolt(er)*|torque)( |-)*wrench|lead)", ps.data[,"narrative"]), 1, 0)
ps.data[, "neg_roofbolt"] = ifelse(ps.data$roofbolt == 1 & 
                                     (ps.data$entrapment == 1 | 
                                        ps.data$brokensteel == 1), 1, 0)
# Identify accidents involving a person who was inside (or hanging outside) of the vehicle
ps.data[, "in_vehicle"] = ifelse(grepl("riding.{1,10}(passenger|driver|operat(o|e)r)", ps.data[,"old_narrative"]) | 
                                   grepl("PERSON.{1,8}riding", ps.data[,"narrative"]) |
                                   grepl("riding.{1,5}outside", ps.data[,"narrative"]) |
                                   !grepl("riding.{1,15}VEHICLE", ps.data[,"narrative"]), 1, 0)
# Operator arm of hand trailing outside vehicle
ps.data[, "outsidevehicle"] = ifelse(((grepl("BODY.{1,15}(resting| hanging).{1,5}(over|out|on)", ps.data[,"narrative"]) & 
                                         grepl("VEHICLE", ps.data[,"narrative"])) |
                                        grepl("BODY.{1,15}out( )*side.{1,30}VEHICLE", ps.data[,"narrative"])) &
                                       !grepl("overhang", ps.data[,"narrative"]), 1, 0)
# In several injuries the miner is struck by a cable which is never PS. However, getting hit by the boom while replacing the cable is common and is PS
ps.data[, "cable"] = ifelse((grepl("cable.{1,30}PINNED/STRUCK", ps.data[,"narrative"]) | 
                               grepl("PINNED/STRUCK.{1,30}cable", ps.data[,"narrative"])) & 
                              (!grepl("boom", ps.data[,"narrative"]) &
                                 !grepl("cable.{1,30}PINNED/STRUCK.{1,15}(against|between)", ps.data[,"narrative"]) &
                                 !grepl("cable( )*bolt", ps.data[,"narrative"])), 1, 0)
ps.data[, "strap"] = ifelse(grepl("strap.{1,20}PINNED/STRUCK", ps.data[,"narrative"]) | 
                              grepl("PINNED/STRUCK.{1,20}strap", ps.data[,"narrative"]), 1, 0)
ps.data[, "trolleypole"] = ifelse(grepl("PINNED/STRUCK.{1,20}troll(e)*y( )*pol(e|l)", ps.data[,"narrative"]) | 
                                    grepl("troll(e)*y( )*pol(e|l).{1,20}PINNED/STRUCK", ps.data[,"narrative"]) |
                                    (grepl(" pol(e|l).{1,20}PINNED/STRUCK", ps.data[,"narrative"]) & 
                                       grepl("troll(e)*y( )*pol(e|l)", ps.data[,"narrative"])) |                                    
                                    (grepl("PINNED/STRUCK.{1,20} pol(e|l)", ps.data[,"narrative"]) & 
                                       grepl("troll(e)*y( )*pol(e|l)", ps.data[,"narrative"])), 1, 0)
ps.data[, "tool_break"] = ifelse(grepl("(wrench|roof bolt).{1,15}(br(eak|oke)|ben(d|t))", ps.data[,"old_narrative"]) & 
                                   !grepl("(wrench|roof bolt).{1,15}(br(eak|oke)|ben(d|t)).{1,20}PINNED/STRUCK.{1,15}between", ps.data[,"narrative"]), 1, 0)
ps.data[, "vcomp_test"] = ifelse(grepl("(seat|rail|canopy|battery|drill|steel|chain|cable)+.{1,20}VEHICLE", ps.data[,"narrative"]) | 
                                   grepl("VEHICLE.{1,20}(seat|rail|canopy|battery|drill|steel|chain|cable)+", ps.data[,"narrative"]), 1, 0)
ps.data[, "psobject_test"] = ifelse(grepl("(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+.{1,20}PINNED/STRUCK", ps.data[,"narrative"]) | 
                                      grepl("PINNED/STRUCK.{1,20}(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+", ps.data[,"narrative"]), 1, 0)
ps.data[, "strikerib"] = ifelse((grepl("PINNED/STRUCK.{0,20}( )rib", ps.data[, "narrative"]) | 
                                   grepl("( )rib.{1,20}PINNED/STRUCK", ps.data[, "narrative"])) &
                                  (!grepl("(lower|upper|side|cage|right|left| in |fractured|bruised).{0,10}( )rib", ps.data[, "old_narrative"]) & 
                                     !grepl("PERSON.{0,10}( )rib", ps.data[, "old_narrative"]) & 
                                     !grepl(" ribs", ps.data[, "old_narrative"])), 1, 0)

##################################################################################################

# GENERATE LIKELY POSITIVELY PREDICTIVE CIRCUMSTANCES FLAGS

# Remove accidents involving falling rock 
ps.data$falling.class = ifelse(ps.data$accidentclassification == "fall of roof or back", 1, 0)
ps.data[, "falling.word"] = ifelse(grepl("rock( )*fell", ps.data[,"narrative"]) |
                                     grepl("fell.{1,20}roof", ps.data[,"narrative"]) |
                                     grepl("roof( )*f(a|e)ll", ps.data[,"narrative"]) |
                                     grepl("(rolled|fell) (from|.ff|out).{0,}( )rib", ps.data[,"narrative"]) |
                                     grepl("( )rib.{0,15}(rolled|fell) (from|.ff|out)", ps.data[,"narrative"]), 1, 0)
ps.data$falling.accident = ifelse(ps.data$falling.class == 1 | 
                                    ps.data$falling.word == 1, 1, 0)
ps.data = ps.data[, c(-match("falling.class", names(ps.data)), 
                      -match("falling.word", names(ps.data)))]

# The last non-missing is key, because otherwise we have one observation that is NA for accident.only
ps.data$accident.only = ifelse((ps.data$degreeofinjury == "accident only" | 
                                  ps.data$accidenttypecode == 44) & 
                                 !is.na(ps.data$accidenttypecode), 1, 0)

# GENERATE KEY WORD FLAGS (POSITIVE KEY WORDS, FALSE POSITIVE KEYWORDS AND MAYBE FALSE POSITIVE KEYWORDS)

ps.data$keyword = ifelse((ps.data$pin == 1 | 
                            ps.data$strike == 1 | 
                            ps.data$pos_roofbolt == 1 | 
                            ps.data$trap == 1 | 
                            ps.data$collided == 1 | 
                            ps.data$hit == 1 | 
                            ps.data$dropped == 1 | 
                            ps.data$ranover == 1 | 
                            ps.data$bumped == 1 | 
                            ps.data$caught == 1 |
                            ps.data$rolled == 1 | 
                            ps.data$between == 1 | 
                            ps.data$wheel == 1) &
                           (ps.data$falling.accident == 0), 1, 0)

ps.data$false_keyword = ifelse((ps.data$jarring == 1 | 
                                  ps.data$outsidevehicle == 1 | 
                                  ps.data$steering == 1 | 
                                  ps.data$neg_roofbolt == 1 |
                                  ps.data$bounced == 1 | 
                                  ps.data$rock == 1 | 
                                  ps.data$derail == 1 | 
                                  ps.data$cable == 1 | 
                                  ps.data$tool_break == 1 | 
                                  ps.data$bodyseat == 1 | 
                                  ps.data$headroof == 1 | 
                                  ps.data$strap == 1 | 
                                  ps.data$trolleypole == 1 | 
                                  ps.data$entrapment == 1 |
                                  ps.data$hole == 1), 1, 0)

ps.data$maybe_false_keyword = ifelse((ps.data$digit == 1 | 
                                        ps.data$operating == 1 |
                                        ps.data$bent == 1 | 
                                        ps.data$strikerib == 1 |
                                        ps.data$wrench == 1 |
                                        ps.data$controls == 1 | 
                                        ps.data$resin == 1 |
                                        ps.data$loose == 1 | 
                                        ps.data$broke == 1 | 
                                        ps.data$canopy == 1 | 
                                        ps.data$flew == 1), 1, 0)

##################################################################################################

# GENERATE LIKELY/MAYBE LIKELY/UNLIKELY CATEGORY INDICATORS

# Generate likely accident class, maybe likely accident ckass, and unlikely accident class indicators
ps.data$likely_class = ifelse((ps.data$accidentclassification == "powered haulage" | 
                                 ps.data$accidentclassification == "machinery" ), 1, 0)
ps.data$unlikely_class = ifelse((ps.data$accidentclassification == "disorders (repeated trauma)" | 
                                   ps.data$accidentclassification == "electrical" |
                                   ps.data$accidentclassification == "explosives and breaking agents" | 
                                   ps.data$accidentclassification == "stepping or kneeling on object" | 
                                   ps.data$accidentclassification == "ignition or explosion of gas or dust"), 1, 0)
ps.data$uncertain_class = ifelse((ps.data$accidentclassification == "fall of roof or back" |
                                    ps.data$accidentclassification == "handling of materials" |
                                    ps.data$accidentclassification == "slip or fall of person" |
                                    ps.data$accidentclassification == "fall of face/rib/pillar/side/highwall" |
                                    ps.data$accidentclassification == "handtools (nonpowered)" |
                                    ps.data$accidentclassification == "no value found" |
                                    ps.data$accidentclassification == "other" |
                                    ps.data$accidentclassification == "striking or bumping"), 1, 0)

# Generate likely accident type, maybe likely accident type, and unlikely accident type indicators
ps.data$likely_type = ifelse((ps.data$accidenttype == "struck by, nec" | 
                                ps.data$accidenttype == "struck by powered moving obj" |
                                ps.data$accidenttype == "struck by rollng or slidng obj" |
                                ps.data$accidenttype == "struck against moving object" |
                                ps.data$accidenttype == "cght i, u, b, rnng, mshng objs" |
                                ps.data$accidenttype == "cght i, u, b, mvng & sttn objs" |
                                ps.data$accidenttype == "caught i, u, b, moving objects" |
                                ps.data$accidenttype == "cght in, under, or btween, nec" |
                                ps.data$accidenttype == "struck against moving object"), 1, 0)
ps.data$unlikely_type = ifelse((ps.data$accidenttype == "fall from ladders" | 
                                  ps.data$accidenttype == "fall to lower level, nec" |
                                  ps.data$accidenttype == "fall to wlkway or wrkng surfc" |
                                  ps.data$accidenttype == "fall onto or against objects" |
                                  ps.data$accidenttype == "rubbed or abraded, nec" |
                                  ps.data$accidenttype == "bodily reaction, nec" |
                                  ps.data$accidenttype == "over-exertion in lifting objs" |
                                  ps.data$accidenttype == "ovr-exrtn in pllng, pshng objs" |
                                  ps.data$accidenttype == "ovrexrtn in wldng, thrwng objs" |
                                  ps.data$accidenttype == "contact with elctric current" |
                                  ps.data$accidenttype == "acc type, without injuries" |
                                  ps.data$accidenttype == "contct w/ hot objs or substanc" |
                                  ps.data$accidenttype == "absrtn rad caust txc & nox sbs" |
                                  ps.data$accidenttype == "flash burns (electric)" |
                                  ps.data$accidenttype == "over-exertion, nec"), 1, 0)
ps.data$uncertain_type = ifelse((ps.data$accidenttype == "struck against stationary obj" |
                                   ps.data$accidenttype == "fall frm mach, vehicle, equip" |
                                   ps.data$accidenttype == "struck by falling object" |
                                   ps.data$accidenttype == "struck by flying object" |
                                   ps.data$accidenttype == "no value found" |
                                   ps.data$accidenttype == "not elsewhere classified"), 1, 0)
ps.data$maybe_type = ifelse((ps.data$accidenttype == "acc type, without injuries" |
                               ps.data$accidenttype == "struck against stationary obj" |
                               ps.data$accidenttype == "fall frm mach, vehicle, equip" |
                               ps.data$accidenttype == "struck by falling object" |
                               ps.data$accidenttype == "struck by flying object" |
                               ps.data$accidenttype == "no value found" |
                               ps.data$accidenttype == "not elsewhere classified") & 
                              ps.data$false_keyword == 0, 1, 0)

# Generate likely equipment indicators
vehcl_equip_codes = c("06", "13", "28", "53", "?")
ps.data[, "moving_vehcl"] = ifelse(!(ps.data$equiptypecode %in% vehcl_equip_codes), 1, 0)
ps.data$likely_equip = ifelse((ps.data$equiptypecode == "12" |  
                                 ps.data$equiptypecode == "23" |
                                 ps.data$equiptypecode == "33" | 
                                 ps.data$equiptypecode == "34" |
                                 ps.data$equiptypecode == "35" | 
                                 ps.data$equiptypecode == "37" |
                                 ps.data$equiptypecode == "41" | 
                                 ps.data$equiptypecode == "61" |
                                 ps.data$equiptypecode == "67") & 
                                ps.data$false_keyword == 0, 1, 0)
ps.data$unlikely_equip = ifelse((ps.data$equiptypecode == "06" |  
                                   ps.data$equiptypecode == "09" |
                                   ps.data$equiptypecode == "15" |  
                                   ps.data$equiptypecode == "16" |
                                   ps.data$equiptypecode == "20" |
                                   ps.data$equiptypecode == "28" |
                                   ps.data$equiptypecode == "29" |
                                   ps.data$equiptypecode == "53" |
                                   ps.data$equiptypecode == "55" | 
                                   ps.data$equiptypecode == "?" ), 1, 0)
ps.data$uncertain_equip = ifelse((ps.data$equiptypecode == "54" |
                                    ps.data$equiptypecode == "71" |  
                                    ps.data$equiptypecode == "25" |
                                    ps.data$equiptypecode == "13" | 
                                    ps.data$equiptypecode == "14"), 1, 0)

# Generate likely source, maybe likely source, and unlikely source indicators
ps.data$likely_source = ifelse((ps.data$injurysourcecode == "074" | 
                                  ps.data$injurysourcecode == "077" | 
                                  ps.data$injurysourcecode == "081" | 
                                  ps.data$injurysourcecode == "087" | 
                                  ps.data$injurysourcecode == "104" | 
                                  ps.data$injurysourcecode == "105" |
                                  ps.data$injurysourcecode == "106" | 
                                  ps.data$injurysourcecode == "107" | 
                                  ps.data$injurysourcecode == "108" | 
                                  ps.data$injurysourcecode == "110") & 
                                 ps.data$false_keyword == 0, 1, 0)
ps.data$unlikely_source = ifelse((ps.data$injurysourcecode == "003" | 
                                    ps.data$injurysourcecode == "004" | 
                                    ps.data$injurysourcecode == "006" | 
                                    ps.data$injurysourcecode == "007" | 
                                    ps.data$injurysourcecode == "008" | 
                                    ps.data$injurysourcecode == "009" |
                                    ps.data$injurysourcecode == "012" | 
                                    ps.data$injurysourcecode == "051" | 
                                    ps.data$injurysourcecode == "089" |
                                    ps.data$injurysourcecode == "067" | 
                                    ps.data$injurysourcecode == "068" |
                                    ps.data$injurysourcecode == "078" | 
                                    ps.data$injurysourcecode == "079" | 
                                    ps.data$injurysourcecode == "080" | 
                                    ps.data$injurysourcecode == "083" |
                                    ps.data$injurysourcecode == "090" |
                                    ps.data$injurysourcecode == "092" | 
                                    ps.data$injurysourcecode == "093" | 
                                    ps.data$injurysourcecode == "096" | 
                                    ps.data$injurysourcecode == "098" | 
                                    ps.data$injurysourcecode == "112" |
                                    ps.data$injurysourcecode == "116" | 
                                    ps.data$injurysourcecode == "125"), 1, 0)
ps.data$uncertain_source = ifelse((ps.data$likely_source == 0 & 
                                     ps.data$unlikely_source == 0), 1, 0)

# Generate likely nature, maybe likely nature, and unlikely nature indicators
ps.data$likely_nature = ifelse(ps.data$natureofinjury == "crushing", 1, 0)
ps.data$unlikely_nature = ifelse((ps.data$natureofinjury == "burn or scald (heat)" |
                                    ps.data$natureofinjury == "burn,chemicl-fume,compoun" |
                                    ps.data$natureofinjury == "elect shock,electrocution" |
                                    ps.data$natureofinjury == "hearing loss or impairmnt" |
                                    ps.data$natureofinjury == "dust in eyes" |
                                    ps.data$natureofinjury == "elect.arc burn-not contac"), 1, 0)
ps.data$uncertain_nature = ifelse((ps.data$natureofinjury == "no value found" |
                                     ps.data$natureofinjury == "sprain,strain rupt disc" |
                                     ps.data$natureofinjury == "cut,lacer,punct-opn wound" |
                                     ps.data$natureofinjury == "contusn,bruise,intac skin" |
                                     ps.data$natureofinjury == "fracture,chip" |
                                     ps.data$natureofinjury == "multiple injuries" |
                                     ps.data$natureofinjury == "amputation or enucleation" |
                                     ps.data$natureofinjury == "dislocation" |
                                     ps.data$natureofinjury == "other injury,nec" |
                                     ps.data$natureofinjury == "scratch,abrasion,superfcl" |
                                     ps.data$natureofinjury == "concussion-brain,cerebral" |
                                     ps.data$natureofinjury == "joint,tendon,muscl inflam"), 1, 0)

# Generate likely activity, maybe likely activity, and unlikely activity indicators
ps.data[, "likely_actvty"] = ifelse((grepl("operate", ps.data$mineractivity) | 
                                       grepl("roof", ps.data$mineractivity)), 1, 0)
ps.data[, "maybe_likely_actvty"] = ifelse(grepl("move/reel", ps.data$mineractivity) | 
                                            grepl("handling supplies/materials", ps.data$mineractivity), 1, 0)
ps.data$unlikely_activity = ifelse((ps.data$activitycode == "009" | 
                                      ps.data$activitycode == "016" | 
                                      ps.data$activitycode == "020" | 
                                      ps.data$activitycode == "022" | 
                                      ps.data$activitycode == "025" | 
                                      ps.data$activitycode == "026" |
                                      ps.data$activitycode == "027" | 
                                      ps.data$activitycode == "029" | 
                                      ps.data$activitycode == "030" | 
                                      ps.data$activitycode == "032" | 
                                      ps.data$activitycode == "034" | 
                                      ps.data$activitycode == "036" |
                                      ps.data$activitycode == "075" | 
                                      ps.data$activitycode == "066" | 
                                      ps.data$activitycode == "065" | 
                                      ps.data$activitycode == "056"), 1, 0)

# Make sure this is mutually exclusive/exhaustive
ps.data$uncertain_activity = ifelse((ps.data$likely_actvty == 0 & 
                                       ps.data$maybe_likely_actvty == 0 & 
                                       ps.data$unlikely_activity == 0), 1, 0)

# Generate likely occupations indicator
ps.data$likely_occup = ifelse((ps.data$occupcode3digit == "050" | 
                                 ps.data$occupcode3digit == "046" | 
                                 ps.data$occupcode3digit == "028" | 
                                 ps.data$occupcode3digit == "016" | 
                                 ps.data$occupcode3digit == "036"), 1, 0)

# Generate likely body parts indicator
ps.data$unlikely_body = ifelse((ps.data$bodypartcode == "200" | 
                                  ps.data$bodypartcode == "340" | 
                                  ps.data$bodypartcode == "420"), 1, 0)

##################################################################################################

# SUM UP LIKELY AND UNLIKELY INDICATORS
ps.data$keyword_pts = rowSums(ps.data[, c('pin', 'strike', 'drillsteel', 
                                          'trap', 'collided', 'hit', 
                                          'dropped', 'ranover', 'bumped', 
                                          'caught', 'rolled', 'between', 
                                          'wheel')], na.rm = TRUE)
ps.data$neg_keyword_pts = rowSums(ps.data[, c('jarring', 'outsidevehicle', 'steering', 
                                              'bounced', 'rock', 'derail', 
                                              'cable', 'strap', 'trolleypole', 
                                              'tool_break', 'bodyseat', 'headroof', 
                                              'hole')], na.rm = TRUE)
ps.data$pos_pts = rowSums(ps.data[, c('likely_class', 'likely_equip', 'likely_nature', 
                                      'likely_source', 'likely_type')], na.rm = TRUE)
ps.data$neg_pts = rowSums(ps.data[, c('unlikely_class', 'unlikely_equip', 'unlikely_source', 
                                      'unlikely_nature', 'unlikely_type', 'uncertain_activity')], na.rm = TRUE)

##################################################################################################

# A FEW MORE KEY WORDS, USING EXISTING KEYWORD FLAGS 

ps.data[, "no_vehcl"] = ifelse(!grepl("VEHICLE", ps.data[, "narrative"]), 1, 0)
ps.data[, "v_to_v"] = ifelse((grepl("(VEHICLE|drill|steel|bolter|shear|cutter|tire).{1,35}PINNED/STRUCK.{1,35}VEHICLE", ps.data[, "narrative"]) |
                                grepl("VEHICLE.{1,35}PINNED/STRUCK.{1,35}(VEHICLE|drill|steel|bolter|shear|cutter|tire)", ps.data[, "narrative"])) & 
                               ps.data$hole == 0, 1, 0)
ps.data[, "v_to_p"] = ifelse((grepl("(VEHICLE).{1,20}PINNED/STRUCK.{1,20}(PERSON|BODY)", ps.data[, "narrative"]) |
                                grepl("(PERSON|BODY).{1,20}PINNED/STRUCK.{1,20}(VEHICLE)", ps.data[, "narrative"])) & 
                               ps.data$false_keyword == 0, 1, 0)
ps.data[, "int_obj_strike"] = ifelse((grepl("( )(block|rock|cho(c)*k|chunk|rail|i-beam)( )", ps.data[, "old_narrative"]) & 
                                        grepl("VEHICLE", ps.data[, "narrative"]) & 
                                        grepl("PINNED/STRUCK", ps.data[, "narrative"]) &
                                        grepl("(tr(a)*m(m)*[^ ]{0,3}|op(e)*r(a)*t[^ ]{0,3}|back(in|ed).{1,10}VEHICLE|VEHICLE.{1,10}back(in|ed)|r(a|u)n|makin(g)*( )*(a)*( )*tu(r)*n|remote.{1,5}control|driv|pull)", ps.data[, "narrative"]) &
                                        grepl("(steering |(hand )*knob).{1,20}(PINNED/STRUCK).{1,20}(BODY|PERSON)", ps.data[, "narrative"]) &
                                        (ps.data$accidenttypecode %in% c(8, 5)) & 
                                        ps.data$falling.accident == 0 & 
                                        ps.data$operating == 0), 1, 0)

##################################################################################################

# VARIOUS SIMPLE ALGORITHMS (JUST USING KEY WORDS AND INDICATORS)

ps.data[, "holistic"] = ifelse((((ps.data$likely_type == 1) | 
                                   (ps.data$maybe_type == 1)) & 
                                  (ps.data$likely_actvty == 1 | 
                                     ps.data$maybe_likely_actvty == 1) & 
                                  (ps.data$likely_class == 1) & 
                                  ps.data$moving_vehcl == 1), 1, 0)

# Generate likely PS accident flag
ps.data$potential_ps = ifelse(ps.data$keyword == 1 | 
                                ps.data$likely_class == 1 | 
                                ps.data$v_to_v == 1 | 
                                ps.data$v_to_p == 1, 1, 0)

# Generate our best simple algorithm PS accident flag
ps.data$likely_ps = ifelse((ps.data$keyword == 1 | 
                              ps.data$likely_class == 1 | 
                              ps.data$v_to_v == 1 | 
                              ps.data$v_to_p == 1) &
                             (ps.data$falling.accident == 0) &
                             (ps.data$bodyseat == 0 & 
                                ps.data$headroof == 0 & 
                                ps.data$hole == 0 &
                                ps.data$cable == 0 & 
                                ps.data$strap == 0 & 
                                ps.data$tool_break == 0 &
                                ps.data$outsidevehicle == 0 & 
                                ps.data$derail == 0 & 
                                ps.data$bounced == 0  & 
                                ps.data$trolleypole == 0 & 
                                ps.data$neg_roofbolt == 0 & 
                                ps.data$unlikely_nature == 0 & 
                                ps.data$unlikely_source == 0) &
                             (ps.data$neg_keyword_pts < 2 & 
                                ps.data$pos_pts > 1 & 
                                ps.data$neg_pts < 3), 1, 0)

##################################################################################################

# Drop variables with redundant or no information while keeping codes used in the algorithms below
all_vars = ps.data
ps.data = ps.data[, c(-match("accidenttime", names(ps.data)), 
                      -match("accidenttypecode", names(ps.data)),
                      -match("bodypartcode", names(ps.data)),
                      -match("classificationcode", names(ps.data)),
                      -match("contractorid", names(ps.data)),
                      -match("daysrestrictedduty", names(ps.data)), 
                      -match("degreeofinjurycode", names(ps.data)), 
                      -match("equipmanufacturercode", names(ps.data)),
                      -match("immediatenotificationcode", names(ps.data)), 
                      -match("immediatenotificationclass", names(ps.data)), 
                      -match("injurysourcecode", names(ps.data)),
                      -match("mineexperience", names(ps.data)), 
                      -match("narrative", names(ps.data)), 
                      -match("natureofinjurycode", names(ps.data)),
                      -match("numbertypo", names(ps.data)),
                      -match("occupation", names(ps.data)),
                      -match("old_narrative", names(ps.data)),
                      -match("operatorid", names(ps.data)),
                      -match("operatorname", names(ps.data)),
                      -match("schedulechargedays", names(ps.data)),  
                      -match("shiftbeginningtime", names(ps.data)),
                      -match("subunitcode", names(ps.data)),
                      -match("transferredorterminated", names(ps.data)),
                      -grep("^ug(l|m)", names(ps.data)))]

# Drop date variables (now irrelevant)
ps.data = ps.data[, c(-grep("date", names(ps.data)))]

##################################################################################################

# PRODUCE DATASETS WITH ONLY VARIABLES OF INTEREST 

drops = c("mineid", "subunit", "calendaryear",
          "calendarquarter", "degreeofinjury", "fipsstatecode", 
          "accidentclassification", "accidenttype", "numberofinjuries", 
          "totalexperience", "jobexperience", "activitycode", 
          "mineractivity", "sourceofinjury", "natureofinjury", 
          "bodypart", "dayslost", "controllerid", 
          "equiptypecode", "typeofequipment", "equipmanufacturer", 
          "equipmentmodelno", "occupcode3digit", "controllername",
          "bolting", "holistic")
simple.data = ps.data[, !(names(ps.data) %in% drops)] 

# Enforce factor storage
vars = names(simple.data)
for (i in 1:length(vars)) {
 simple.data[, vars[i]] = factor(simple.data[, vars[i]])
}

##################################################################################################

# Randomly sort data (in case it was ordered)
set.seed(625)
rand = runif(nrow(simple.data))
simple.ps = simple.data[order(rand),]
remove(rand)

# Print out PS indicator column number
which(colnames(simple.ps)=="PS") 

######################################################################################################

# TO TEST VARIOUS MODELS

# The code below this line was used in testing various algorithms, and is included for interest alone
if (data.type == "training data" ) {
  simple.ps = simple.ps[,c(-grep("type", names(simple.ps)))]
  
  # CART
  cart = rpart(PS ~ ., data = simple.ps[1:700, !(names(simple.ps) %in% c('documentno'))], method = "class")
  cart.predictions = predict(cart, simple.ps[701:1000,], type = "class")
  table(simple.ps[701:1000,74], predicted = cart.predictions)
  
  # RANDOM FOREST
  rf = randomForest(PS ~ . -documentno, data = simple.ps[1:700,], mtry = 3, importance = TRUE, type = "class", ntree = 800)
  rf.predictions = predict(rf, simple.ps[701:1000,], type="class")
  table(simple.ps[701:1000,74], predicted = rf.predictions)
  
  # RANDOM FOREST WITH SMOTE
  smote.trainx = simple.ps[1:700,]
  smote.test = simple.ps[701:1000,]
  smote = SMOTE(PS ~ ., smote.trainx, perc.over = 100, perc.under = 100)
  rf.smo = randomForest(PS ~ . -documentno, data = smote, mtry = 10, ntree = 800)
  rf.smo.pred = predict(rf.smo, smote.test, type = "class")
  table(simple.ps[701:1000,74], predicted = rf.smo.pred)
  
  # RANDOM FOREST WITH ROSE
  simple.rosex = ROSE(PS ~ ., data = simple.ps[1:700,])$data
  rand3 = runif(nrow(simple.rosex))
  simple.rose = simple.rosex[order(rand3),]
  rf.rose = randomForest(PS ~ . -documentno, data = simple.rose, mtry = 15, ntree = 1000)
  rf.rose.pred = predict(rf.rose, simple.ps[701:1000,], type = "class")
  table(simple.ps[701:1000,74], predicted = rf.rose.pred)
  
  # DOWNSAMPLE NEGATIVE OUTCOMES FOR RANDOM FOREST
  nmin = sum(simple.ps$PS == "YES")
  nmin
  ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
  rf.downsampled = train(PS ~ ., data = simple.ps[1:700,!(names(simple.ps) %in% c('documentno','narrative'))], 
                         method = "rf", ntree = 800,
                         tuneLength = 10, metric = "ROC", trControl = ctrl, 
                         strata = simple.ps$PS, sampsize = rep(nmin, 2))
  rf.baseline = train(PS ~ ., data = simple.ps[1:700,!(names(simple.ps) %in% c('documentno','narrative'))], 
                      method = "rf", ntree = 800,
                      tuneLength = 10, metric = "ROC", trControl = ctrl)
  down.prob = predict(rf.downsampled, 
                      simple.ps[701:1000,!(names(simple.ps) %in% c('documentno','narrative'))], type = "prob")[,1]
  down.ROC = roc(response = 
                   simple.ps[701:1000,1], predictor = down.prob, levels = rev(levels(simple.ps[701:1000,1])))
  base.prob = predict(rf.baseline, 
                      simple.ps[701:1000,!(names(simple.ps) %in% c('documentno','narrative'))], type = "prob")[,1]
  base.ROC = roc(response = simple.ps[701:1000,1], 
                 predictor = base.prob, levels = rev(levels(simple.ps[701:1000,1])))
  plot(down.ROC, col = rgb(1, 0, 0, .5), lwd = 2)
  plot(base.ROC, col = rgb(0, 0, 1, .5), lwd = 2, add = TRUE)
  legend(.4, .4, c("Down-Sampled", "Normal"), lwd = rep(2, 1), col = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)))
  
  # BOOSTING
  ps.adaboost = boosting(PS ~ ., 
                         data = simple.ps[1:700, !(names(simple.ps) %in% c('documentno'))], 
                         boos = T, mfinal = 800, coeflearn = 'Freund')
  simple.adaboost.pred = predict.boosting(ps.adaboost, newdata = simple.ps[701:1000,])
  simple.adaboost.pred$confusion
  
  # Generate variable with boosting predictions
  simple.adaboost.pred$class = as.factor(simple.adaboost.pred$class)
  predictions = simple.ps[601:1000,]
  predictions = cbind(predictions, simple.adaboost.pred$class)
  names(predictions)[names(predictions) == 'simple.adaboost.pred$class'] = 'prediction'
  
  # Print variable importance
  pdf("plots.pdf", width = 40, height = 30)
  importanceplot(ps.adaboost)
  dev.off()
  
  # Retrieve narratives of misclassified observations
  predictions = merge(predictions, all_vars[, c("narrative", "old_narrative", "documentno", "mineid")], by = "documentno")
  
  # Re-code common false positives
  predictions$accidents = ifelse(predictions$entrapment == 1, 1, predictions$prediction) 
  predictions$prediction = ifelse(predictions$brokensteel == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$headroof == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$headcanopy == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$hole == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$jarring == 1 |
                                    predictions$bounced == 1 |
                                    predictions$rock == 1 |
                                    predictions$bodyseat == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$accident.only == 1, 1, predictions$prediction)
  predictions$prediction = ifelse(predictions$falling.accident == 1, 1, predictions$prediction)
  predictions$prediction = as.factor(predictions$prediction)
  
  # Now report final predictive accuracy
  table = table(predictions$prediction, predictions$PS)
}

######################################################################################################

# NOW PERFORM THE FINAL ALGORITHM WITH REAL ACCIDENTS DATA FOR CLASSIFICATION

if (data.type == "real accidents data") {
  
  set.seed(625)
  
  # Run boosting on training observations
  ps.adaboost = boosting(PS ~ ., 
                         data = simple.ps[simple.ps$type=="training", !(names(simple.ps) %in% c('documentno', 'type'))], 
                         boos = T, mfinal = 300, coeflearn = 'Freund')
  
  # Predict PS for unclassified observations
  adaboost.pred = predict.boosting(ps.adaboost, 
                                   newdata = simple.ps[simple.ps$type=="unclassified", !(names(simple.ps) %in% c('documentno', 'type'))])
  
  # Generate variable with boosting predictions
  adaboost.pred$class = as.factor(adaboost.pred$class)
  accidents = cbind(simple.ps[simple.ps$type=="unclassified",], adaboost.pred$class)
  names(accidents)[names(accidents) == 'adaboost.pred$class'] = 'prediction'
  accidents = accidents[, c(-match("PS", names(accidents)))]
  
  # Re-code common false positives
  accidents$prediction = ifelse(accidents$entrapment == 1, 1, accidents$prediction) 
  accidents$prediction = ifelse(accidents$brokensteel == 1, 1, accidents$prediction)
  accidents$prediction = ifelse(accidents$headroof == 1, 1, accidents$prediction)
  accidents$prediction = ifelse(accidents$headcanopy == 1, 1, accidents$prediction)
  accidents$prediction = ifelse(accidents$hole == 1, 1, accidents$prediction)
  accidents$prediction = ifelse(accidents$jarring == 1 |
                                  accidents$rock == 1 |
                                  accidents$bodyseat == 1, 1, accidents$prediction)
  accidents$prediction = ifelse(accidents$accident.only == 1, 1, accidents$prediction)
  accidents$prediction = ifelse(accidents$falling.accident == 1, 1, accidents$prediction)
  accidents$prediction = as.factor(accidents$prediction)
  
  # Merge predictions back onto real data (we just need mine IDs and accidentdate for the next stage)
  accidents = accidents[, c(match("prediction", names(accidents)),
                            match("documentno", names(accidents)))]
  accidents = merge(all.accidents, accidents, by = "documentno", all = T)
  accidents$PS = ifelse(!is.na(accidents$prediction), accidents$prediction, accidents$PS)
  accidents = accidents[, c(match("PS", names(accidents)),
                            match("mineid", names(accidents)),
                            match("accidentdate", names(accidents)),
                            match("documentno", names(accidents)))]  
  
  # Merge real classifications onto remaining observations 
  accidents = merge(accidents, simple.ps, by = "documentno", all = F)
  accidents$PS.y = ifelse(accidents$PS.y == "YES", "2", "1")
  accidents$PS = ifelse(accidents$PS.x == "", accidents$PS.y, accidents$PS.x)
  accidents = accidents[, c(match("PS", names(accidents)),
                            match("mineid", names(accidents)),
                            match("accidentdate", names(accidents)),
                            match("documentno", names(accidents)))]  
  
  # Save a CSV file with narrative information 
  write.csv(accidents, file = classified.accidents.file.name.csv)
  
  # Save R dataset
  saveRDS(accidents, file = classified.accidents.file.name)
}

##################################################################################################
