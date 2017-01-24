# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 8 - Train/Test MR (Maintenance and Repair)
  # Train/Test:

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

library(psych)
library(rpart)
library(randomForest)
library(ROSE)
library(SMOTE)
library(DMwR)
library(caret)
library(ggplot2)
library(adabag)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
prepped.input.path = paste0(root, "/5_prepped", collapse = NULL) 

# inputs
  # prepped MR training/testing data
prepped.train.set.in.file.name = paste0(prepped.input.path, "/prepped_MR_train_test.rds", collapse = NULL)

# no outputs - all results print to console ()

# generate file paths
dir.create(prepped.input.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# set seed to enable reproducible results
set.seed(625)

# read cleaned MR training set data
  # 1018 rows; 55 columns; unique on documentno 
simple = readRDS(prepped.train.set.in.file.name)

# print PS indicator column number - 2
which(colnames(simple) == "MR") 

# bye
rm(root, prepped.input.path, prepped.train.set.in.file.name)

################################################################################

# CART
  # See Table D.1a: Confusion Matrix for CaRT Algorithm
cart = rpart(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno'))], method = "class")
cart.predictions = predict(cart, simple[701:1018,!(names(simple) %in% c('documentno'))], type = "class")
table(simple[701:1018,2], predicted = cart.predictions)

rm(cart, cart.predictions)

################################################################################

# RANDOM FOREST
  # See Table D.1b: Confusion Matrix for Random Forest (Unbalanced) Algorithm

rf = randomForest(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno'))], mtry = 15, 
                  importance = TRUE, type = "class", ntree = 1000)
rf.predictions = predict(rf, simple[701:1018,!(names(simple) %in% c('documentno'))], type = "class")
table(simple[701:1018,2], predicted = rf.predictions)

rm(rf, rf.predictions)

################################################################################

# RANDOM FOREST WITH ROSE
  # See Table D.1c: Confusion Matrix for Random Forest (ROSE Oversampled) Algorithm

simple.rosex = ROSE(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno'))])$data
rand = runif(nrow(simple.rosex))
simple.rose = simple.rosex[order(rand),]
rf.rose = randomForest(MR ~ ., data = simple.rose, mtry = 15, ntree = 1000)
rf.rose.pred = predict(rf.rose, simple[701:1018,!(names(simple) %in% c('documentno'))], type = "class")
table(simple[701:1018,2], predicted = rf.rose.pred)

remove(simple.rosex, rand, simple.rose, rf.rose, rf.rose.pred)

################################################################################

# RANDOM FOREST WITH SMOTE
  # See Table D.1d: Confusion Matrix for Random Forest (SMOTE Oversampled) Algorithm

smote.trainx = simple[1:700, !(names(simple) %in% c('documentno'))]
smote.test = simple[701:1018, !(names(simple) %in% c('documentno'))]
smote = SMOTE(MR ~ ., smote.trainx, perc.over = 100, perc.under = 100)
rf.smo = randomForest(MR ~ ., data = smote, mtry = 10, ntree = 800)
rf.smo.pred = predict(rf.smo, smote.test, type = "class")
table(simple[701:1018,2], predicted = rf.smo.pred)

rm(smote.trainx, smote.test, smote, rf.smo, rf.smo.pred)

################################################################################

# DOWNSAMPLE NEGATIVE OUTCOMES FOR RANDOM FOREST
  # See Table D.1e: Confusion Matrix for Random Forest (Under-sampled) Algorithm

nmin = sum(simple$MR == "YES")
nmin
ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
rf.downsampled = train(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno', 'lug'))], method = "rf", ntree = 800,
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = simple$MR, sampsize = rep(nmin, 2))
down.prob = predict(rf.downsampled, simple[701:1018,!(names(simple) %in% c('documentno'))], type = "prob")
down.prob = ifelse(down.prob$YES > 0.50, 1, 0)
table(simple[701:1018,2], predicted = down.prob)

rm(nmin, ctrl, down.prob, rf.downsampled)

################################################################################

# ADAPTIVE BOOSTING
  # See Table D.1f: Confusion Matrix for Adaptive Boosting Algorithm

mr.adaboost = boosting(MR ~ . , data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], boos = T, mfinal = 300, coeflearn = 'Freund')
adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[701:1019,!(names(simple) %in% c('documentno','narrative'))])
adaboost.pred$confusion

rm(adaboost.pred, mr.adaboost)

################################################################################

# bye
rm(list = ls())

################################################################################