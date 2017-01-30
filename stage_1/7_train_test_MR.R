# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 7 - Train/Test MR (Maintenance and Repair) Accidents
  # Train and test a variety of algorithms to classify accidents in the MR 
    # training/testing set (produced in 5_prepare_MR) as MR/non-MR

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 1/30/17

################################################################################

library(rpart)
library(randomForest)
library(ROSE)
library(DMwR)
library(caret)
library(adabag)
library(mlbench)
library(ggplot2)
library(lattice)

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

# bye
rm(root, prepared.path)

################################################################################

# READ DATA

# read prepared MR training/testing set
  # 1018 rows; 55 columns; unique on documentno 
data = readRDS(prepared.train.test.in.file.name)

# bye
rm(prepared.train.test.in.file.name)

################################################################################

# set seed
set.seed(626)

################################################################################

# CART
  # Creates Table D.1a: Confusion Matrix for CaRT Algorithm

cart = rpart(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno"))], 
             method = "class")
cart.predictions = predict(cart, data[701:1018, !(names(data) %in% c("documentno"))], 
                           type = "class")
table(data[701:1018, "MR"], predicted = cart.predictions)

#            predicted
# observed   NO    YES
#       NO   188   9
#      YES   21    100

### IN PAPER
#            predicted
# observed   NO    YES
#       NO   188   9
#      YES   21    100

# bye
rm(cart, cart.predictions)

################################################################################

# RANDOM FOREST
  # Creates Table D.1b: Confusion Matrix for Random Forest (Unbalanced) Algorithm

rf = randomForest(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno"))], 
                  mtry = 15, importance = TRUE, type = "class", ntree = 1000)
rf.predictions = predict(rf, data[701:1018, !(names(data) %in% c("documentno"))], 
                         type = "class")
table(data[701:1018, "MR"], predicted = rf.predictions)

#            predicted
# observed   NO    YES
#       NO   189   8
#      YES   17    104

##### IN PAPER (NOTE: sums to 300)
#            predicted
# observed   NO    YES
#       NO   190   5
#      YES   17    88

# bye
rm(rf, rf.predictions)

################################################################################

# RANDOM FOREST WITH UNDER-SAMPLING
  # Creates Table D.1e: Confusion Matrix for Random Forest (Under-sampled) Algorithm

nmin = sum(data$MR == "YES")
ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
rf.downsampled = train(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno"))], 
                       method = "rf", ntree = 800, 
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = data$MR, sampsize = rep(nmin, 2))
down.prob = predict(rf.downsampled, data[701:1018, !(names(data) %in% c("documentno"))], type = "prob")
down.prob = ifelse(down.prob$YES > 0.50, 1, 0)
table(data[701:1018, "MR"], predicted = down.prob)

# WITH MINEID
#            predicted
# observed   NO    YES
#       NO   184   13
#      YES   12    109

# WITHOUT MINEID
#            predicted
# observed   NO    YES
#       NO   185   12
#      YES   12     109

##### IN PAPER (NOTE: sums to 300)
#            predicted
# observed   NO    YES
#       NO   195   34
#      YES   40    31

# bye
rm(nmin, ctrl, down.prob, rf.downsampled)

################################################################################

# RANDOM FOREST WITH ROSE
  # Creates Table D.1c: Confusion Matrix for Random Forest (ROSE Oversampled) Algorithm

data.rosex = ROSE(MR ~ ., data = data[1:700, !(names(data) %in% c("documentno"))])$data
rand = runif(nrow(data.rosex))
data.rose = data.rosex[order(rand), ]
rf.rose = randomForest(MR ~ ., data = data.rose, mtry = 15, ntree = 1000)
rf.rose.pred = predict(rf.rose, data[701:1018, !(names(data) %in% c("documentno"))], type = "class")
table(data[701:1018, "MR"], predicted = rf.rose.pred)

#            predicted
# observed   NO    YES
#       NO   192   5
#      YES   33    88

### IN PAPER (note: sums to 322)
#            predicted
# observed   NO    YES
#       NO   192   20
#      YES   33    77

# bye
remove(data.rosex, rand, data.rose, rf.rose, rf.rose.pred)

################################################################################

# RANDOM FOREST WITH SMOTE
  # See Table D.1d: Confusion Matrix for Random Forest (SMOTE Oversampled) Algorithm

smote.trainx = data[1:700, !(names(data) %in% c("documentno", "mineid"))]
smote.test = data[701:1018, !(names(data) %in% c("documentno", "mineid"))]
smote = SMOTE(MR ~ ., smote.trainx, perc.over = 100, perc.under = 100)
rf.smo = randomForest(MR ~ ., data = smote, mtry = 10, ntree = 800)
rf.smo.pred = predict(rf.smo, smote.test, type = "class")
table(data[701:1018, "MR"], predicted = rf.smo.pred)


## WITH MINEID
#            predicted
# observed   NO    YES
#       NO   184   13
#      YES   14   107

## WITHOUT MINEID
#            predicted
# observed   NO    YES
#       NO   186   11
#      YES   14    107

### IN PAPER


# bye
rm(smote.trainx, smote.test, smote, rf.smo, rf.smo.pred)

################################################################################

# ADAPTIVE BOOSTING
  # See Table D.1f: Confusion Matrix for Adaptive Boosting Algorithm

mr.adaboost = boosting(MR ~ . , data = data[1:700,!(names(data) %in% c("documentno"))], boos = T, mfinal = 300, coeflearn = 'Freund')
adaboost.pred = predict.boosting(mr.adaboost, newdata = data[701:1019,!(names(data) %in% c("documentno"))])
adaboost.pred$confusion

#            predicted
# observed   NO    YES
#       NO   190   7
#      YES   16    105

# bye
rm(adaboost.pred, mr.adaboost)

################################################################################

# bye
rm(list = ls())

################################################################################