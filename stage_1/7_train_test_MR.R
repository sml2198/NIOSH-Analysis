# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 7 - Train/Test MR (Maintenance and Repair) Accidents
  # Train and test a variety of algorithms to classify accidents in the MR 
    # training/testing set (produced in 5_prepare_MR) as MR/non-MR

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 1/25/17

################################################################################

library(rpart)
library(randomForest)
library(ROSE)
library(SMOTE)
#library(psych)
#library(DMwR)
#library(caret)
#library(ggplot2)
#library(adabag)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
prepared.path = paste0(root, "/data/5_prepared", collapse = NULL) 

# inputs
  # prepared MR training/testing set
prepared.train.test.in.file.name = paste0(prepared.path, "/prepared_MR_train_test.rds", collapse = NULL)

# no outputs - results print to console

# generate file paths
dir.create(prepared.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, prepared.path)

################################################################################

# set seed to enable reproducible results
set.seed(626)

################################################################################

# READ DATA

# read prepared MR training/testing set
  # 1018 rows; 58 columns; unique on documentno 
data = readRDS(prepared.train.test.in.file.name)

# print PS indicator column number - 2
#which(colnames(data) == "MR") 

# bye
rm(prepared.train.test.in.file.name)

################################################################################

# CART
  # See Table D.1a: Confusion Matrix for CaRT Algorithm
cart = rpart(MR ~ ., data = data[1:700,!(names(data) %in% c('documentno'))], method = "class")
cart.predictions = predict(cart, data[701:1018,!(names(data) %in% c('documentno'))], type = "class")
table(data[701:1018, "MR"], predicted = cart.predictions)

rm(cart, cart.predictions)

################################################################################

# RANDOM FOREST
  # See Table D.1b: Confusion Matrix for Random Forest (Unbalanced) Algorithm

rf = randomForest(MR ~ ., data = data[1:700,!(names(data) %in% c('documentno'))], mtry = 15, 
                  importance = TRUE, type = "class", ntree = 1000)
rf.predictions = predict(rf, data[701:1018,!(names(data) %in% c('documentno'))], type = "class")
table(data[701:1018, "MR"], predicted = rf.predictions)

rm(rf, rf.predictions)

################################################################################

# RANDOM FOREST WITH ROSE
  # See Table D.1c: Confusion Matrix for Random Forest (ROSE Oversampled) Algorithm

# DROP MINE ID AND THIS WORKS

data.rosex = ROSE(MR ~ ., data = data[1:700,!(names(data) %in% c("documentno"))])$data
rand = runif(nrow(data.rosex))
data.rose = data.rosex[order(rand),]
rf.rose = randomForest(MR ~ ., data = data.rose, mtry = 15, ntree = 1000)
rf.rose.pred = predict(rf.rose, data[701:1018,!(names(data) %in% c("documentno"))], type = "class")
table(data[701:1018, "MR"], predicted = rf.rose.pred)

remove(data.rosex, rand, data.rose, rf.rose, rf.rose.pred)

################################################################################

# RANDOM FOREST WITH SMOTE
  # See Table D.1d: Confusion Matrix for Random Forest (SMOTE Oversampled) Algorithm

smote.trainx = data[1:700, !(names(data) %in% c('documentno'))]
smote.test = data[701:1018, !(names(data) %in% c('documentno'))]
smote = SMOTE(MR ~ ., smote.trainx, perc.over = 100, perc.under = 100)
rf.smo = randomForest(MR ~ ., data = smote, mtry = 10, ntree = 800)
rf.smo.pred = predict(rf.smo, smote.test, type = "class")
table(data[701:1018,2], predicted = rf.smo.pred)

rm(smote.trainx, smote.test, smote, rf.smo, rf.smo.pred)

################################################################################

# DOWNSAMPLE NEGATIVE OUTCOMES FOR RANDOM FOREST
  # See Table D.1e: Confusion Matrix for Random Forest (Under-sampled) Algorithm

nmin = sum(data$MR == "YES")
nmin
ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
rf.downsampled = train(MR ~ ., data = data[1:700,!(names(data) %in% c('documentno', 'lug'))], method = "rf", ntree = 800,
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = data$MR, sampsize = rep(nmin, 2))
down.prob = predict(rf.downsampled, data[701:1018,!(names(data) %in% c('documentno'))], type = "prob")
down.prob = ifelse(down.prob$YES > 0.50, 1, 0)
table(data[701:1018,2], predicted = down.prob)

rm(nmin, ctrl, down.prob, rf.downsampled)

################################################################################

# ADAPTIVE BOOSTING
  # See Table D.1f: Confusion Matrix for Adaptive Boosting Algorithm

mr.adaboost = boosting(MR ~ . , data = data[1:700,!(names(data) %in% c('documentno','narrative'))], boos = T, mfinal = 300, coeflearn = 'Freund')
adaboost.pred = predict.boosting(mr.adaboost, newdata = data[701:1019,!(names(data) %in% c('documentno','narrative'))])
adaboost.pred$confusion

rm(adaboost.pred, mr.adaboost)

################################################################################

# bye
rm(list = ls())

################################################################################