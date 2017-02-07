# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 8 - Train/Test PS (Pinning and Striking) Accidents
  # Train and test a variety of algorithms to classify accidents in the PS 
    # training/testing set (produced in 6_prepare_PS) as PS/non-PS

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
  # prepared PS training/testing set
    # produced in 6_prepare_PS
prepared.train.test.in.file.name = paste0(prepared.path, "/prepared_PS_train_test.rds", collapse = NULL)
  # seeds
seed.file.name =  paste0(seed.path, "/train.test.PS.seed.txt", collapse = NULL)

# outputs
  # CART Confusion Matrix
table.d2a.file.name =  paste0(results.path, "/Table D2a - Confusion Matrix for CaRT Algorithm.csv", collapse = NULL)
  # Random Forest Confusion Matrix
table.d2b.file.name =  paste0(results.path, "/Table D2b - Confusion Matrix for Random Forest (Unbalanced) Algorithm.csv", collapse = NULL)
  # Random Forest (ROSE Oversampled) Confusion Matrix
table.d2c.file.name =  paste0(results.path, "/Table D2c - Confusion Matrix for Random Forest (ROSE Oversampled) Algorithm.csv", collapse = NULL)
  # Random Forest (SMOTE Oversampled) Confusion Matrix
table.d2d.file.name =  paste0(results.path, "/Table D2d - Confusion Matrix for Random Forest (SMOTE Oversampled) Algorithm.csv", collapse = NULL)
  # Random Forest (Under-sampled) Confusion Matrix
table.d2e.file.name =  paste0(results.path, "/Table D2e - Confusion Matrix for Random Forest (Under-sampled) Algorithm.csv", collapse = NULL)
  # Adaptive Boosting Confusion Matrix
table.d2f.file.name =  paste0(results.path, "/Table D2f - Confusion Matrix for Adaptive Boosting Algorithm.csv", collapse = NULL)
  # Summary Statistics
summary.file.name =  paste0(results.path, "/PS Classification Algorithm Summary.csv", collapse = NULL)

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

# read prepared PS training/testing set
  # 1000 rows; 100 columns; unique on documentno 
data = readRDS(prepared.train.test.in.file.name)

# read seed
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
cart = rpart(PS ~ ., data = data[1:700, !(names(data) %in% c("documentno"))], 
             method = "class")
cart.predictions = predict(cart, data[701:1000, !(names(data) %in% c("documentno"))], 
                           type = "class")

# generate confusion matrix and summary statistics
table.d2a = table(data[701:1000, "PS"], predicted = cart.predictions)
sum = summarize(table.d2a)
summary[summary$Algorithm == "CART", 2:6] = sum

# output confusion matrix
table.d2a = data.frame(matrix(table.d2a, nrow = 2, ncol = 2))
row.names(table.d2a) = c("observed non-PS", "observed PS")
names(table.d2a) = c("classified non-PS", "classified PS")
write.csv(table.d2a, table.d2a.file.name)

# bye
rm(cart, cart.predictions, sum, table.d2a, table.d2a.file.name)

################################################################################

# RANDOM FOREST (UNBALANCED)

# run algorithm
rf = randomForest(PS ~ ., data = data[1:700, !(names(data) %in% c("documentno"))], 
                  mtry = 3, importance = TRUE, type = "class", ntree = 800)
rf.predictions = predict(rf, data[701:1000, !(names(data) %in% c("documentno"))], 
                         type = "class")

# generate confusion matrix and summary statistics
table.d2b = table(data[701:1000, "PS"], predicted = rf.predictions)
sum = summarize(table.d2b)
summary[summary$Algorithm == "Random Forest", 2:6] = sum

# output confusion matrix
table.d2b = data.frame(matrix(table.d2b, nrow = 2, ncol = 2))
row.names(table.d2b) = c("observed non-PS", "observed PS")
names(table.d2b) = c("classified non-PS", "classified PS")
write.csv(table.d2b, table.d2b.file.name)

# bye
rm(rf, rf.predictions, sum, table.d2b, table.d2b.file.name)

################################################################################

# RANDOM FOREST (ROSE OVERSAMPLED)

# run algorithm
data.rosex = ROSE(PS ~ ., data = data[1:700, !(names(data) %in% c("documentno"))])$data
rand = runif(nrow(data.rosex))
data.rose = data.rosex[order(rand), ]
rf.rose = randomForest(PS ~ . , data = data.rose, mtry = 15, ntree = 1000)
rf.rose.pred = predict(rf.rose, data[701:1000, !(names(data) %in% c("documentno"))], type = "class")

# generate confusion matrix and summary statistics
table.d2c = table(data[701:1000, "PS"], predicted = rf.rose.pred)
sum = summarize(table.d2c)
summary[summary$Algorithm == "Random Forest - ROSE", 2:6] = sum

# output confusion matrix
table.d2c = data.frame(matrix(table.d2c, nrow = 2, ncol = 2))
row.names(table.d2c) = c("observed non-PS", "observed PS")
names(table.d2c) = c("classified non-PS", "classified PS")
write.csv(table.d2c, table.d2c.file.name)

# bye
rm(data.rosex, rand, data.rose, rf.rose, rf.rose.pred, sum, table.d2c, table.d2c.file.name)

################################################################################

# RANDOM FOREST (SMOTE OVERSAMPLED)

# run algorithm
smote.trainx = data[1:700, !(names(data) %in% c("documentno"))]
smote.test = data[701:1000, !(names(data) %in% c("documentno"))]
smote = SMOTE(PS ~ ., smote.trainx, perc.over = 100, perc.under = 100)
rf.smo = randomForest(PS ~ ., data = smote, mtry = 10, ntree = 800)
rf.smo.pred = predict(rf.smo, smote.test, type = "class")

# generate confusion matrix and summary statistics
table.d2d = table(data[701:1000, "PS"], predicted = rf.smo.pred)
sum = summarize(table.d2d)
summary[summary$Algorithm == "Random Forest - SMOTE", 2:6] = sum

# output confusion matrix
table.d2d = data.frame(matrix(table.d2d, nrow = 2, ncol = 2))
row.names(table.d2d) = c("observed non-PS", "observed PS")
names(table.d2d) = c("classified non-PS", "classified PS")
write.csv(table.d2d, table.d2d.file.name)

# bye
rm(smote.trainx, smote.test, smote, rf.smo, rf.smo.pred, sum, table.d2d, table.d2d.file.name)

################################################################################

# RANDOM FOREST (UNDER-SAMPLED)

# run algorithm
nmin = sum(data$PS == "YES")
ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
rf.downsampled = train(PS ~ ., data = data[1:700,!(names(data) %in% c("documentno"))], 
                       method = "rf", ntree = 800,
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = data$PS, sampsize = rep(nmin, 2))
down.prob = predict(rf.downsampled, 
                    data[701:1000, !(names(data) %in% c("documentno"))], type = "prob")
down.prob = ifelse(down.prob$YES > 0.50, 1, 0)

# generate confusion matrix and summary statistics
table.d2e = table(data[701:1000, "PS"], predicted = down.prob)
sum = summarize(table.d2e)
summary[summary$Algorithm == "Random Forest - Undersampled", 2:6] = sum

# output confusion matrix
table.d2e = data.frame(matrix(table.d2e, nrow = 2, ncol = 2))
row.names(table.d2e) = c("observed non-PS", "observed PS")
names(table.d2e) = c("classified non-PS", "classified PS")
write.csv(table.d2e, table.d2e.file.name)

# bye
rm(nmin, ctrl, down.prob, rf.downsampled, sum, table.d2e, table.d2e.file.name)

################################################################################

# ADAPTIVE BOOSTING

# run algorithm
ps.adaboost = boosting(PS ~ ., data = data[1:700, !(names(data) %in% c("documentno"))], 
                       boos = TRUE, mfinal = 800, coeflearn = "Freund")
adaboost.pred = predict.boosting(ps.adaboost, newdata = data[701:1000, !(names(data) %in% c("documentno"))])

# generate confusion matrix and summary statistics
table.d2f = t(adaboost.pred$confusion)
sum = summarize(table.d2f)
summary[summary$Algorithm == "Adaptive Boosting", 2:6] = sum

# output confusion matrix
table.d2f = data.frame(matrix(table.d2f, nrow = 2, ncol = 2))
row.names(table.d2f) = c("observed non-PS", "observed PS")
names(table.d2f) = c("classified non-PS", "classified PS")
write.csv(table.d2f, table.d2f.file.name)

# bye
rm(adaboost.pred, ps.adaboost, sum, table.d2f, table.d2f.file.name)

################################################################################

# OUTPUT SUMMARY STATISTICS

# output summary statistics
write.csv(summary, summary.file.name, row.names = FALSE)

# bye
rm(list = ls())

################################################################################
