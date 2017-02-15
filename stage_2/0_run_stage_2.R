# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 0 - Run Stage 2
  # Runs files to complete analyses for Stage 2:
    # 1_clean_mines
    # 2_clean_employment
    # 3_clean_operator_history
    # 4_collapse_accidents
    # 5_prepare_mines
    # 6_generate_figures

# Coded by: Julia Bodson, juliabodson@gmail.com

# Last edit 2/8/2017

################################################################################

# LOAD PACKAGES

install.packages("plyr")
install.packages("stringr")
install.packages("zoo")

library(plyr)
library(stringr)
library(zoo)

################################################################################

# setwd("/NIOSH-Analysis/programs/stage_2/")
# setwd("C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_2/")
setwd("C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/programs/stage_2/")

################################################################################

# RUN FILES

source("1_clean_mines.R")
source("2_clean_employment.R")
source("3_clean_operator_history.R")
source("4_collapse_accidents.R")
source("5_prepare_mines.R")
source("6_generate_figures.R")

################################################################################
