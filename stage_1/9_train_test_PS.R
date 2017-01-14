# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 9 - Train/Test PS (Pinning and Striking) 
  # Train/Test:

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

library(rpart)
library(randomForest)
library(DMwR)
library(ROSE)
library(caret)
library(ggplot2)
#library(tree)
#library(randomForest)
#library(reshape2)
#library(adabag)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
prepped.input.path = paste0(root, "/5_prepped", collapse = NULL) 

# inputs
  # prepped PS training/testing data
prepped.train.set.in.file.name = paste0(prepped.input.path, "/prepped_PS_train_test.rds", collapse = NULL)

# no outputs!

# generate file paths
dir.create(prepped.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# set seed to enable reproducible results
set.seed(625)

# read cleaned PS training set data and remive "type" field (it's all the same)
  # 1000 rows; 100 columns; unique on documentno 
simple.ps = readRDS(prepped.train.set.in.file.name)
simple.ps = simple.ps[, -c(match("type", names(simple.ps)))]

# print PS indicator column number
which(colnames(simple.ps) == "PS") 

# bye
rm(root, prepped.input.path, prepped.train.set.in.file.name)

################################################################################

# CART

cart = rpart(PS ~ ., data = simple.ps[1:700, !(names(simple.ps) %in% c("documentno"))], method = "class")
cart.predictions = predict(cart, simple.ps[701:1000,], type = "class")
table(simple.ps[701:1000,2], predicted = cart.predictions)

#      NO YES
# NO  213  15
# YES  22  50

################################################################################

# RANDOM FOREST

rf = randomForest(PS ~ . -documentno, data = simple.ps[1:700,], mtry = 3, importance = TRUE, type = "class", ntree = 800)
rf.predictions = predict(rf, simple.ps[701:1000,], type = "class")
table(simple.ps[701:1000, 2], predicted = rf.predictions)

#      NO YES
# NO  216  12
# YES  27  45

################################################################################

# RANDOM FOREST WITH ROSE

simple.rosex = ROSE(PS ~ ., data = simple.ps[1:700, ])$data
rand3 = runif(nrow(simple.rosex))
simple.rose = simple.rosex[order(rand3), ]
rf.rose = randomForest(PS ~ . -documentno, data = simple.rose, mtry = 15, ntree = 1000)
rf.rose.pred = predict(rf.rose, simple.ps[701:1000, ], type = "class")
table(simple.ps[701:1000, 2], predicted = rf.rose.pred)

#      NO YES
# NO  201  27
# YES   9  63

################################################################################

# RANDOM FOREST WITH SMOTE

smote.trainx = simple.ps[1:700, ]
smote.test = simple.ps[701:1000, ]
smote = SMOTE(PS ~ ., smote.trainx, perc.over = 100, perc.under = 100)
rf.smo = randomForest(PS ~ . -documentno, data = smote, mtry = 10, ntree = 800)
rf.smo.pred = predict(rf.smo, smote.test, type = "class")
table(simple.ps[701:1000, 2], predicted = rf.smo.pred)

#      NO YES
# NO  187  41
# YES   9  63

################################################################################

# DOWNSAMPLE NEGATIVE OUTCOMES FOR RANDOM FOREST

nmin = sum(simple.ps$PS == "YES")
nmin
ctrl = trainControl(method = "cv", classProbs = TRUE, summaryFunction = twoClassSummary)
rf.downsampled = train(PS ~ ., data = simple.ps[1:700,!(names(simple.ps) %in% c("documentno", "narrative"))], 
                       method = "rf", ntree = 800,
                       tuneLength = 10, metric = "ROC", trControl = ctrl, 
                       strata = simple.ps$PS, sampsize = rep(nmin, 2))
down.prob = predict(rf.downsampled, 
                    simple.ps[701:1000,!(names(simple.ps) %in% c("documentno", "narrative"))], type = "prob")[,1]

################################################################################

# BOOSTING

ps.adaboost = boosting(PS ~ ., 
                       data = simple.ps[1:700, !(names(simple.ps) %in% c("documentno"))], 
                       boos = T, mfinal = 800, coeflearn = "Freund")
simple.adaboost.pred = predict.boosting(ps.adaboost, newdata = simple.ps[701:1000,])
simple.adaboost.pred$confusion

# generate variable with boosting predictions
simple.adaboost.pred$class = as.factor(simple.adaboost.pred$class)
predictions = simple.ps[601:1000,]
predictions = cbind(predictions, simple.adaboost.pred$class)
names(predictions)[names(predictions) == "simple.adaboost.pred$class"] = "prediction"

# print variable importance
pdf("plots.pdf", width = 40, height = 30)
importanceplot(ps.adaboost)
dev.off()

# retrieve narratives of misclassified observations
predictions = merge(predictions, all_vars[, c("narrative", "old_narrative", "documentno", "mineid")], by = "documentno")

# re-code common false positives
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

# now report final predictive accuracy
table = table(predictions$prediction, predictions$PS)

################################################################################

rm(list = ls())

################################################################################