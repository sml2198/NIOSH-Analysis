# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 7 - Train/Test MR (Maintenance and Repair) Accidents
  # Train and test a variety of algorithms to classify accidents in the MR 
    # training/testing set (produced in 5_prepare_MR) as MR/non-MR

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 1/31/17

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
prepared.train.test.in.file.name = paste0(prepared.path, "/prepared_MR_train_test.rds", collapse = NULL)
  # seeds
seed.1.file.name =  paste0(seed.path, "/train.test.MR.seed.1.txt", collapse = NULL)

# outputs
  # CART Confusion Matrix
table.d1a.file.name =  paste0(results.path, "/Table D1a.csv", collapse = NULL)
  # Random Forest Confusion Matrix
table.d1b.file.name =  paste0(results.path, "/Table D1b.csv", collapse = NULL)
  # Random Forest, over-sampled with ROSE, Confusion Matrix
table.d1c.file.name =  paste0(results.path, "/Table D1c.csv", collapse = NULL)
  # Random Forest, over-sampled with SMOTE, Confusion Matrix
table.d1d.file.name =  paste0(results.path, "/Table D1d.csv", collapse = NULL)
  # Random Forest, under-sampled, Confusion Matrix
table.d1e.file.name =  paste0(results.path, "/Table D1e.csv", collapse = NULL)
  # Adaptive Boosting Confusion Matrix
table.d1f.file.name =  paste0(results.path, "/Table D1f.csv", collapse = NULL)
  # Summary Statistics
summary.file.name =  paste0(results.path, "/MR Classification Algorithm Summary.csv", collapse = NULL)

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
  
  print(paste("True Positive Rate:", TPR, sep = " "))
  print(paste("False Positive Rate:", FPR, sep = " "))
  print(paste("True Negative Rate:", TNR, sep = " "))
  print(paste("False Negative Rate:", FNR, sep = " "))
  print(paste("Positive Predictive Value:", PPV, sep = " "))
  
  return(c(TPR, FPR, TNR, FNR, PPV))
}

################################################################################

# READ DATA

# read prepared MR training/testing set
  # 1018 rows; 55 columns; unique on documentno 
data = readRDS(prepared.train.test.in.file.name)

# read seeds
seed1 = read.table(seed.1.file.name)
seed1 = seed1[, 1]

# bye
rm(prepared.train.test.in.file.name, seed.1.file.name)

################################################################################

# PREPARE EMPTY DATASET TO STORE SUMMARY

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

set.seed(seed1)
rm(seed1)

################################################################################

# CART
  # Creates Table D.1a: Confusion Matrix for CaRT Algorithm

cart = rpart(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno", "mineid"))], 
             method = "class")
cart.predictions = predict(cart, data[701:1018, !(names(data) %in% c("documentno", "mineid"))], 
                           type = "class")
table(data[701:1018, "MR"], predicted = cart.predictions)
sum = summarize(table(data[701:1018, "MR"], predicted = cart.predictions))

# output results
write.csv(table(data[701:1018, "MR"], predicted = cart.predictions), cart.cm.file.name)
summary[summary$Algorithm == "CART", 2:6] = sum

# True Positive Rate: 82.64
# False Positive Rate: 4.06
# True Negative Rate: 95.94
# False Negative Rate: 17.36
# Positive Predictive Value: 92.59

# bye
rm(cart, cart.predictions, sum, cart.cm.file.name)

################################################################################

# RANDOM FOREST
  # Creates Table D.1b: Confusion Matrix for Random Forest (Unbalanced) Algorithm

rf = randomForest(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno", "mineid"))], 
                  mtry = 15, importance = TRUE, type = "class", ntree = 1000)
rf.predictions = predict(rf, data[701:1018, !(names(data) %in% c("documentno", "mineid"))], 
                         type = "class")
table(data[701:1018, "MR"], predicted = rf.predictions)
sum = summarize(table(data[701:1018, "MR"], predicted = rf.predictions))

# output results
write.csv(table(data[701:1018, "MR"], predicted = rf.predictions), rf.cm.file.name)
summary[summary$Algorithm == "Random Forest", 2:6] = sum

# True Positive Rate: 87.60
# False Positive Rate: 4.06
# True Negative Rate: 95.94
# False Negative Rate: 12.40
# Positive Predictive Value: 92.98

# bye
rm(rf, rf.predictions, sum, rf.cm.file.name)

################################################################################

# RANDOM FOREST WITH ROSE
  # Creates Table D.1c: Confusion Matrix for Random Forest (ROSE Oversampled) Algorithm

data.rosex = ROSE(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno", "mineid"))])$data
rand = runif(nrow(data.rosex))
data.rose = data.rosex[order(rand), ]
rf.rose = randomForest(MR ~ ., data = data.rose, mtry = 15, ntree = 1000)
rf.rose.pred = predict(rf.rose, data[701:1018, !(names(data) %in% c("documentno", "mineid"))], type = "class")
table(data[701:1018, "MR"], predicted = rf.rose.pred)
sum = summarize(table(data[701:1018, "MR"], predicted = rf.rose.pred))

# output results
write.csv(table(data[701:1018, "MR"], predicted = rf.rose.pred), rf.rose.cm.file.name)
summary[summary$Algorithm == "Random Forest - ROSE", 2:6] = sum

# True Positive Rate: 72.73
# False Positive Rate: 3.55
# True Negative Rate: 96.45
# False Negative Rate: 27.27
# Positive Predictive Value: 92.63

# bye
remove(data.rosex, rand, data.rose, rf.rose, rf.rose.pred, sum, rf.rose.cm.file.name)

################################################################################

# RANDOM FOREST WITH SMOTE
  # See Table D.1d: Confusion Matrix for Random Forest (SMOTE Oversampled) Algorithm

smote.trainx = data[1:700, !(names(data) %in% c("documentno", "mineid"))]
smote.test = data[701:1018, !(names(data) %in% c("documentno", "mineid"))]
smote = SMOTE(MR ~ ., smote.trainx, perc.over = 100, perc.under = 100)
rf.smo = randomForest(MR ~ ., data = smote, mtry = 10, ntree = 800)
rf.smo.pred = predict(rf.smo, smote.test, type = "class")
table(data[701:1018, "MR"], predicted = rf.smo.pred)
sum = summarize(table(data[701:1018, "MR"], predicted = rf.smo.pred))

# output results
write.csv(table(data[701:1018, "MR"], predicted = rf.smo.pred), rf.smote.cm.file.name)
summary[summary$Algorithm == "Random Forest - SMOTE", 2:6] = sum

# True Positive Rate: 88.43
# False Positive Rate: 5.08
# True Negative Rate: 94.92
# False Negative Rate: 11.57
# Positive Predictive Value: 91.45

# bye
rm(smote.trainx, smote.test, smote, rf.smo, rf.smo.pred, sum, rf.smote.cm.file.name)

################################################################################

# RANDOM FOREST WITH UNDER-SAMPLING
# Creates Table D.1e: Confusion Matrix for Random Forest (Under-sampled) Algorithm

nmin = sum(data$MR == "YES")
ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
rf.downsampled = train(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno", "mineid"))], 
                       method = "rf", ntree = 800, 
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = data$MR, sampsize = rep(nmin, 2))
down.prob = predict(rf.downsampled, data[701:1018, !(names(data) %in% c("documentno", "mineid"))], type = "prob")
down.prob = ifelse(down.prob$YES > 0.50, 1, 0)
table(data[701:1018, "MR"], predicted = down.prob)
sum = summarize(table(data[701:1018, "MR"], predicted = down.prob))

# output results
write.csv(table(data[701:1018, "MR"], predicted = down.prob), rf.under.cm.file.name)
summary[summary$Algorithm == "Random Forest - Undersampled", 2:6] = sum

# True Positive Rate: 91.74
# False Positive Rate: 6.60
# True Negative Rate: 93.40
# False Negative Rate: 8.26
# Positive Predictive Value: 89.52

# bye
rm(nmin, ctrl, down.prob, rf.downsampled, sum, rf.under.cm.file.name)

################################################################################

# ADAPTIVE BOOSTING
  # See Table D.1f: Confusion Matrix for Adaptive Boosting Algorithm

mr.adaboost = boosting(MR ~ . , data = data[1:700, !(names(data) %in% c("documentno", "mineid"))], 
                       boos = T, mfinal = 300, coeflearn = "Freund")
adaboost.pred = predict.boosting(mr.adaboost, newdata = data[701:1018, !(names(data) %in% c("documentno", "mineid"))])
t(adaboost.pred$confusion)
sum = summarize(t(adaboost.pred$confusion))

# output results
write.csv(t(adaboost.pred$confusion), adaboost.cm.file.name)
summary[summary$Algorithm == "Adaptive Boosting", 2:6] = sum

#            predicted
# observed   NO    YES
#       NO   190   7
#      YES   16    105

# True Positive Rate: 86.78
# False Positive Rate: 3.55
# True Negative Rate: 96.45
# False Negative Rate: 13.22
# Positive Predictive Value: 93.75

# bye
rm(adaboost.pred, mr.adaboost, sum, adaboost.cm.file.name)

################################################################################

write.csv(summary, summary.file.name)

################################################################################

# bye
rm(list = ls())

################################################################################