# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 0 - Run Stage 1
  # Runs files to complete analyses for Stage 1:
    # 1_clean_accidents
    # 2_clean_MR_train_test_set
    # 3_clean_PS_train_test_set
    # 4_merge_accidents
    # 5_prepare_MR
    # 6_prepare_PS
    # 7_train_test_MR
    # 8_train_test_PS
    # 9_classify_MR
    # 10_classify_PS

# Coded by: Julia Bodson, juliabodson@gmail.com

# Last edit 2/7/2017

################################################################################

# LOAD PACKAGES

install.packages("adabag")
install.packages("caret")
install.packages("DMwR")
install.packages("ggplot2")
install.packages("lattice")
install.packages("mlbench")
install.packages("randomForest")
install.packages("ROSE")
install.packages("rpart")
install.packages("stringr")

library(adabag)
library(caret)
library(DMwR)
library(ggplot2)
library(lattice)
library(mlbench)
library(randomForest)
library(ROSE)
library(rpart)
library(stringr)

################################################################################

# setwd("/NIOSH-Analysis/programs/stage_1/")
# setwd("C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_1/")
setwd("C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_1/")

################################################################################

# RUN FILES

source("1_clean_accidents.R")
source("2_clean_MR_train_test_set.R")
source("3_clean_PS_train_test_set.R")
source("4_merge_accidents.R")
source("5_prepare_MR.R")
source("6_prepare_PS.R")
source("7_train_test_MR.R")
source("8_train_test_PS.R")
source("9_classify_MR.R")
source("10_classify_PS.R")

################################################################################
