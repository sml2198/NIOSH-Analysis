# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 7 - Train/Test MR (Maintenance and Repair) Accidents
  # Train and test a variety of algorithms to classify accidents in the MR 
    # training/testing set (produced in 5_prepare_MR) as MR/non-MR

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com
      # and Julia Bodson, juliabodson@gmail.com

# Last edit 2/6/2017

################################################################################

library(adabag)
library(caret)
library(DMwR)
library(ggplot2)
library(lattice)
library(mlbench)
library(randomForest)
library(ROSE)
library(rpart)

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
prepared.path = paste0(root, "/data/5_prepared", collapse = NULL) 
seed.path = paste0(root, "/data/6_seeds", collapse = NULL) 
results.path = paste0(root, "/results/stage 1", collapse = NULL) 

# inputs
  # prepared MR training/testing set
    # produced in 5_prepare_MR
prepared.train.test.in.file.name = paste0(prepared.path, "/prepared_MR_train_test.rds", collapse = NULL)
  # seeds
seed.file.name =  paste0(seed.path, "/train.test.MR.seed.txt", collapse = NULL)

# outputs
  # CART Confusion Matrix
table.d1a.file.name =  paste0(results.path, "/Table D1a - Confusion Matrix for CaRT Algorithm.csv", collapse = NULL)
  # Random Forest Confusion Matrix
table.d1b.file.name =  paste0(results.path, "/Table D1b - Confusion Matrix for Random Forest (Unbalanced) Algorithm.csv", collapse = NULL)
  # Random Forest (ROSE Oversampled) Confusion Matrix
table.d1c.file.name =  paste0(results.path, "/Table D1c - Confusion Matrix for Random Forest (ROSE Oversampled) Algorithm.csv", collapse = NULL)
  # Random Forest (SMOTE Oversampled) Confusion Matrix
table.d1d.file.name =  paste0(results.path, "/Table D1d - Confusion Matrix for Random Forest (SMOTE Oversampled) Algorithm.csv", collapse = NULL)
  # Random Forest (Under-sampled) Confusion Matrix
table.d1e.file.name =  paste0(results.path, "/Table D1e - Confusion Matrix for Random Forest (Under-sampled) Algorithm.csv", collapse = NULL)
  # Adaptive Boosting Confusion Matrix
table.d1f.file.name =  paste0(results.path, "/Table D1f - Confusion Matrix for Adaptive Boosting Algorithm.csv", collapse = NULL)
  # Summary Statistics
summary.file.name =  paste0(results.path, "/MR Classification Algorithm Summary.csv", collapse = NULL)

# generate file paths
dir.create(results.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, prepared.path, seed.path, results.path)

################################################################################

# DEFINE FUNCTION TO CALCULATE SUMMARY STATISTICS

summarize = function(tab) {
  TP = tab[2, 2]
  TN = tab[1, 1]
  FP = tab[1, 2]
  FN = tab[2, 1]
  
  TPR = round(100 * TP / (TP + FN), 2)
  FPR = round(100 * FP / (FP + TN), 2)
  TNR = round(100 * TN / (TN + FP), 2)
  FNR = round(100 * FN / (FN + TP), 2)
  PPV = round(100 * TP / (TP + FP), 2)
  
  return(c(TPR, FPR, TNR, FNR, PPV))
}

################################################################################

# READ DATA

# read prepared MR training/testing set
  # 1018 rows; 55 columns; unique on documentno 
data = readRDS(prepared.train.test.in.file.name)

# read seeds
seed = read.table(seed.file.name)
seed = seed[, 1]

# bye
rm(prepared.train.test.in.file.name, seed.file.name)

################################################################################

# PREPARE EMPTY DATASET TO STORE SUMMARY STATISTICS

Algorithm = c("CART", 
              "Random Forest", 
              "Random Forest - ROSE", 
              "Random Forest - SMOTE", 
              "Random Forest - Undersampled", 
              "Adaptive Boosting")

summary = data.frame(Algorithm)

summary$TPR = 
  summary$FPR = 
  summary$TNR = 
  summary$FNR = 
  summary$PPV = NA

# bye
rm(Algorithm)

################################################################################

# SET SEED

set.seed(seed)

# bye
rm(seed)

################################################################################

# CART

# run algorithm
cart = rpart(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno", "mineid"))], 
             method = "class")
cart.predictions = predict(cart, data[701:1018, !(names(data) %in% c("documentno", "mineid"))], 
                           type = "class")

# generate confusion matrix and summary statistics
table.d1a = table(data[701:1018, "MR"], predicted = cart.predictions)
sum = summarize(table.d1a)
summary[summary$Algorithm == "CART", 2:6] = sum

# output confusion matrix
table.d1a = data.frame(matrix(table.d1a, nrow = 2, ncol = 2))
row.names(table.d1a) = c("observed non-MR", "observed MR")
names(table.d1a) = c("classified non-MR", "classified MR")
write.csv(table.d1a, table.d1a.file.name)

# bye
rm(cart, cart.predictions, sum, table.d1a, table.d1a.file.name)

################################################################################

# RANDOM FOREST (UNBALANCED)

# run algorithm
rf = randomForest(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno", "mineid"))], 
                  mtry = 15, importance = TRUE, type = "class", ntree = 1000)
rf.predictions = predict(rf, data[701:1018, !(names(data) %in% c("documentno", "mineid"))], 
                         type = "class")

# generate confusion matrix and summary statistics
table.d1b = table(data[701:1018, "MR"], predicted = rf.predictions)
sum = summarize(table.d1b)
summary[summary$Algorithm == "Random Forest", 2:6] = sum

# output confusion matrix
table.d1b = data.frame(matrix(table.d1b, nrow = 2, ncol = 2))
row.names(table.d1b) = c("observed non-MR", "observed MR")
names(table.d1b) = c("classified non-MR", "classified MR")
write.csv(table.d1b, table.d1b.file.name)

# bye
rm(rf, rf.predictions, sum, table.d1b, table.d1b.file.name)

################################################################################

# RANDOM FOREST (ROSE OVERSAMPLED)

# run algorithm
data.rosex = ROSE(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno", "mineid"))])$data
rand = runif(nrow(data.rosex))
data.rose = data.rosex[order(rand), ]
rf.rose = randomForest(MR ~ ., data = data.rose, mtry = 15, ntree = 1000)
rf.rose.pred = predict(rf.rose, data[701:1018, !(names(data) %in% c("documentno", "mineid"))], type = "class")

# generate confusion matrix and summary statistics
table.d1c = table(data[701:1018, "MR"], predicted = rf.rose.pred)
sum = summarize(table.d1c)
summary[summary$Algorithm == "Random Forest - ROSE", 2:6] = sum

# output confusion matrix
table.d1c = data.frame(matrix(table.d1c, nrow = 2, ncol = 2))
row.names(table.d1c) = c("observed non-MR", "observed MR")
names(table.d1c) = c("classified non-MR", "classified MR")
write.csv(table.d1c, table.d1c.file.name)

# bye
remove(data.rosex, rand, data.rose, rf.rose, rf.rose.pred, sum, table.d1c, table.d1c.file.name)

################################################################################

# RANDOM FOREST (SMOTE OVERSAMPLED)

# run algorithm
smote.trainx = data[1:700, !(names(data) %in% c("documentno", "mineid"))]
smote.test = data[701:1018, !(names(data) %in% c("documentno", "mineid"))]
smote = SMOTE(MR ~ ., smote.trainx, perc.over = 100, perc.under = 100)
rf.smo = randomForest(MR ~ ., data = smote, mtry = 10, ntree = 800)
rf.smo.pred = predict(rf.smo, smote.test, type = "class")

# generate confusion matrix and summary statistics
table.d1d = table(data[701:1018, "MR"], predicted = rf.smo.pred)
sum = summarize(table.d1d)
summary[summary$Algorithm == "Random Forest - SMOTE", 2:6] = sum

# output confusion matrix
table.d1d = data.frame(matrix(table.d1d, nrow = 2, ncol = 2))
row.names(table.d1d) = c("observed non-MR", "observed MR")
names(table.d1d) = c("classified non-MR", "classified MR")
write.csv(table.d1d, table.d1d.file.name)

# bye
rm(smote.trainx, smote.test, smote, rf.smo, rf.smo.pred, sum, table.d1d, table.d1d.file.name)

################################################################################

# RANDOM FOREST (UNDER-SAMPLED)

# run algorithm
nmin = sum(data$MR == "YES")
ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
rf.downsampled = train(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno", "mineid"))], 
                       method = "rf", ntree = 800, 
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = data$MR, sampsize = rep(nmin, 2))
down.prob = predict(rf.downsampled, data[701:1018, !(names(data) %in% c("documentno", "mineid"))], type = "prob")
down.prob = ifelse(down.prob$YES > 0.50, 1, 0)

# generate confusion matrix and summary statistics
table.d1e = table(data[701:1018, "MR"], predicted = down.prob)
sum = summarize(table.d1e)
summary[summary$Algorithm == "Random Forest - Undersampled", 2:6] = sum

# output confusion matrix
table.d1e = data.frame(matrix(table.d1e, nrow = 2, ncol = 2))
row.names(table.d1e) = c("observed non-MR", "observed MR")
names(table.d1e) = c("classified non-MR", "classified MR")
write.csv(table.d1e, table.d1e.file.name)

# bye
rm(nmin, ctrl, down.prob, rf.downsampled, sum, table.d1e, table.d1e.file.name)

################################################################################

# ADAPTIVE BOOSTING

# run algorithm
mr.adaboost = boosting(MR ~ . , data = data[1:700, !(names(data) %in% c("documentno", "mineid"))], 
                       boos = TRUE, mfinal = 300, coeflearn = "Freund")
adaboost.pred = predict.boosting(mr.adaboost, newdata = data[701:1018, !(names(data) %in% c("documentno", "mineid"))])

# generate confusion matrix and summary statistics
table.d1f = t(adaboost.pred$confusion)
sum = summarize(table.d1f)
summary[summary$Algorithm == "Adaptive Boosting", 2:6] = sum

# output confusion matrix
table.d1f = data.frame(matrix(table.d1f, nrow = 2, ncol = 2))
row.names(table.d1f) = c("observed non-MR", "observed MR")
names(table.d1f) = c("classified non-MR", "classified MR")
write.csv(table.d1f, table.d1f.file.name)

# bye
rm(adaboost.pred, mr.adaboost, sum, table.d1f, table.d1f.file.name)

################################################################################

# OUTPUT SUMMARY STATISTICS

# output summary statistics
write.csv(summary, summary.file.name, row.names = FALSE)

# bye
rm(list = ls())

################################################################################
