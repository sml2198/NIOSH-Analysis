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

# read cleaned MR training set data and remive "type" field (it's all the same)
  # 1018 rows; 59 columns; unique on documentno 
simple = readRDS(prepped.train.set.in.file.name)
simple = simple[, -c(match("type", names(simple)))]

# print PS indicator column number - 2
which(colnames(simple) == "MR") 

# bye
rm(root, prepped.input.path, prepped.train.set.in.file.name)

# enforce factor storage
vars = names(simple)
for (i in 1:length(vars)) {
  simple[, vars[i]] = factor(simple[, vars[i]])
}

# 10 missing likely source - WHY IS THIS HAPPENING?
simple$likely.source = ifelse(is.na(simple$likely.source), 0, simple$likely.source)

# also, why do some of these only have 1 factor level?
# likely.class maybe.activy lug 

################################################################################

# CART
  # See Table D.1a: Confusion Matrix for CaRT Algorithm
cart = rpart(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno'))], method = "class")
cart.predictions = predict(cart, simple[701:1019,!(names(simple) %in% c('documentno'))], type = "class")
table(simple[701:1019,2], predicted = cart.predictions)

#     NO YES
# NO 170   3
# YES 18 127

################################################################################

# RANDOM FOREST
  # See Table D.1b: Confusion Matrix for Random Forest (Unbalanced) Algorithm

rf = randomForest(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno'))], mtry = 15, 
                  importance = TRUE, type = "class", ntree = 1000)
rf.predictions = predict(rf, simple[701:1019,!(names(simple) %in% c('documentno'))], type = "class")
table(simple[701:1019,2], predicted = rf.predictions)

#     NO YES
# NO  169   4
# YES  12 133

################################################################################

# RANDOM FOREST WITH ROSE
  # See Table D.1c: Confusion Matrix for Random Forest (ROSE Oversampled) Algorithm

simple.rosex = ROSE(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno'))])$data
rand = runif(nrow(simple.rosex))
simple.rose = simple.rosex[order(rand),]
remove(simple.rosex, rand)
rf.rose = randomForest(MR ~ ., data = simple.rose, mtry = 15, ntree = 1000)
rf.rose.pred = predict(rf.rose, simple[701:1019,!(names(simple) %in% c('documentno'))], type = "class")
table(simple[701:1019,2], predicted = rf.rose.pred)

#      NO YES
# NO  161  10
# YES   8 134

################################################################################

# RANDOM FOREST WITH SMOTE
  # See Table D.1d: Confusion Matrix for Random Forest (SMOTE Oversampled) Algorithm

smote.trainx = simple[1:700, !(names(simple) %in% c('documentno'))]
smote.test = simple[701:1019, !(names(simple) %in% c('documentno'))]
smote = SMOTE(MR ~ ., smote.trainx, perc.over = 100, perc.under = 100)
rf.smo = randomForest(MR ~ ., data = smote, mtry = 10, ntree = 800)
rf.smo.pred = predict(rf.smo, smote.test, type = "class")
table(simple[701:1019,2], predicted = rf.smo.pred)

#      NO YES
# NO  159  14
# YES   6 139

################################################################################

# DOWNSAMPLE NEGATIVE OUTCOMES FOR RANDOM FOREST
  # See Table D.1e: Confusion Matrix for Random Forest (Under-sampled) Algorithm

nmin = sum(simple$MR == "YES")
nmin
ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
rf.downsampled = train(MR ~ ., data = simple[1:700,!(names(simple) %in% c('documentno', 'lug'))], method = "rf", ntree = 800,
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = simple$MR, sampsize = rep(nmin, 2))
down.prob = predict(rf.downsampled, simple[701:1019,!(names(simple) %in% c('documentno'))], type = "prob")[,1]

################################################################################

# ADAPTIVE BOOSTING
  # See Table D.1f: Confusion Matrix for Adaptive Boosting Algorithm

mr.adaboost = boosting(MR ~ . , data = simple[1:700,!(names(simple) %in% c('documentno','narrative'))], boos = T, mfinal = 300, coeflearn = 'Freund')
adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[701:1019,!(names(simple) %in% c('documentno','narrative'))])
adaboost.pred$confusion

#      NO YES
# NO  169  12
# YES   4 133

################################################################################

rm(list = ls())

################################################################################