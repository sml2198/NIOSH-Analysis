# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 10 - Classify MR (Maintenance and Repair) 
  # Classifies all accidents from MSHA open data portal as MR/non-MR

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

library(adabag)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
prepped.input.path = paste0(root, "/5_prepped", collapse = NULL) 
coded.output.path = paste0(root, "/3_coded", collapse = NULL)

# inputs
  # prepped MR data for classification
prepped.classify.in.file.name = paste0(prepped.input.path, "/prepped_MR_classify.rds", collapse = NULL)

# outputs
  # accidents data, classified as MR/non-MR (R dataset)
classified.accidents.file.name = paste0(coded.output.path, "/classified_accidents_MR.rds", collapse = NULL)
  # accidents data, classified as MR/non-MR (csv)
classified.accidents.file.name.csv = paste0(coded.output.path, "/classified_accidents_MR.csv", collapse = NULL)

# generate file paths
dir.create(coded.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# READ DATA

# set seed to enable reproducible results
set.seed(625)

# prepped MR data for classification
  # 75700 rows; 59 columns; unique on documentno 
simple = readRDS(prepped.classify.in.file.name)

# print PS indicator column number - 2
which(colnames(simple) == "MR") 

# bye
rm(root, prepped.input.path, coded.output.path, prepped.classify.in.file.name)

################################################################################

# USE BOOSTING TO CLASSIFY REAL ACCIDENTS DATA WITH UNKNOWN "MR" STATUS

# implement Adaptive Boosting
mr.adaboost = boosting(MR ~ . , data = simple[simple$type!="unclassified",!(names(simple) %in% c('documentno','narrative','type','mineid'))], 
                       boos = T, mfinal = 300, coeflearn = 'Freund')

# generate predictions
adaboost.pred = predict.boosting(mr.adaboost, newdata = simple[simple$type=="unclassified",!(names(simple) %in% c('documentno','narrative','type','mineid'))])

# apply predictions to unclassified injuries
accidents.data = cbind(simple[simple$type=="unclassified",], adaboost.pred$class)
names(accidents.data)[names(accidents.data) == 'adaboost.pred$class'] = 'adaboost'

################################################################################

# POST-PROCESSING

# merge back in the rest of the variables from the original accidents data     
accidents.original = readRDS(accidents.data.file.name)
accidents.data = merge(accidents.data, accidents.original, by = "documentno", all = TRUE)

# clean up variable names from the merge
rm(train, simple, simple.data, mr.fatalities, accidents.original)
accidents.data = accidents.data[, c(-grep("\\.x", names(accidents.data)))]
names(accidents.data) = gsub("\\.[x|y]", "", names(accidents.data))

# now manually weed out false positives and negatives that could not have been foreseen in the training data 
accidents.data$manual.predict = ifelse(((accidents.data$likely.activy == 1 & 
                                           accidents.data$likely.class == 1 & 
                                           accidents.data$false.keyword == 0) |
                                          (accidents.data$likely.occup == 1 & 
                                             (accidents.data$maybe.activy == 1 | 
                                                accidents.data$likely.activy == 1 | 
                                                accidents.data$likely.class == 1 | 
                                                accidents.data$maybe.keyword == 1)) |
                                          (accidents.data$likely.activy == 1 & 
                                             (accidents.data$maybe.occup == 1 | 
                                                accidents.data$likely.class == 1 | 
                                                accidents.data$maybe.keyword == 1)) |
                                          (accidents.data$maybe.occup == 1 & 
                                             ((accidents.data$maybe.activy == 1 & 
                                                 accidents.data$likely.class == 1) | 
                                                (accidents.data$likely.class == 1 & 
                                                   accidents.data$maybe.keyword == 1) | 
                                                (accidents.data$maybe.activy == 1 & 
                                                   accidents.data$maybe.keyword == 1))) |
                                          accidents.data$likely.keyword == 1) & 
                                         accidents.data$accident.only == 0, 1, 0)

# SARAH -- MOVE VARIABLE CREATION SOMEWHERE ELSE

# same process for false negatives 
accidents.data$flashburn = ifelse(grepl("weld.{1,40}flash( |-)*burn", accidents.data$narrative) | 
                                    grepl("flash( |-)*burn.{1,40}weld", accidents.data$narrative), 1, 0)
accidents.data$false.neg = ifelse(accidents.data$flashburn == 1 & accidents.data$adaboost == "NO", 1, 0)

# same process for false positives   
accidents.data$carpal.tunnel = ifelse((grepl("carp(a|u|e)l( |-)*tun(n)*(e|l)(e|l)", accidents.data$narrative) | 
                                         grepl("bursitis", accidents.data$narrative)) &
                                        !grepl("fracture", accidents.data$narrative), 1, 0)
accidents.data$cumulative = ifelse(grepl("rep(e|i)t(e|a|i)ti(v|n)e", accidents.data$narrative) | 
                                     grepl("(cumulative|degenerativ)", accidents.data$narrative) |
                                     grepl("repeated(ed)*.{1,10}(mo(tion|vement)|trauma|irritation)", accidents.data$narrative) |
                                     grepl("long( |-)*term", accidents.data$narrative) | 
                                     grepl("slow( |-)*on( |-)*set", accidents.data$narrative), 1, 0)
accidents.data$hearingloss = ifelse(grepl("hearing.los", accidents.data$narrative) | 
                                      grepl("los.{1,10}hearing", accidents.data$narrative) |
                                      grepl("n(o|i)(o|i)se exposur", accidents.data$narrative) |
                                      grepl("dimini.{1,10}hearing", accidents.data$narrative) |
                                      grepl("thr(e|i)s(h)*( |-)*hold( |-)*shift", accidents.data$narrative) |
                                      grepl("shift.{1,4}change.{1,30}hear", accidents.data$narrative) |
                                      grepl("exposur(e)*.{1,20}noise", accidents.data$narrative), 1, 0)
accidents.data$exposure = ifelse(grepl("((prolon|occupation|long( |-)*term).{1,8}exposur)|(exposur.{1,20}(noise|we(a)*ther))|p(n|h)e(n)*umo(n)*co(nio)*sis", accidents.data$narrative) | 
                                   accidents.data$natureofinjury == "pneumoconiosis,black lung", 1, 0)
accidents.data$heartattack = ifelse(grepl("heart( |-)*at(t)*ac(k|h)", accidents.data$narrative) | 
                                      accidents.data$natureofinjury == "heart attack", 1, 0)
accidents.data$unrelated = ifelse(grepl("not work relat", accidents.data$narrative) | 
                                    grepl("no.{1,20}(specific|single).{1,5}(accident|injury|indicent|exposure)", accidents.data$narrative) |  
                                    (grepl("no (accident|incident|injury)", accidents.data$narrative) & 
                                       !grepl("no (accident|incident|injury).{1,5}report", accidents.data$narrative)), 1, 0)

# last ditch attempt to find likely verbs and nouns before dropping false positives 
accidents.data$working.on = ifelse(grepl("(to work|workin(g)*)( |-)*(on|in|under|out|at)", accidents.data$narrative), 1, 0)
accidents.data$barring = ifelse(grepl("barr(ed|ing).{1,10}(rock|motor)", accidents.data$narrative), 1, 0)
accidents.data$otherverb = ifelse(grepl("( |^)patch", accidents.data$narrative) | 
                                    grepl("re(-)*(build|pack|fuel|assembl|lin)", accidents.data$narrative) | 
                                    grepl("(re)*adjust", accidents.data$narrative) | 
                                    grepl("(secure|unplug)", accidents.data$narrative) | 
                                    grepl("trouble( |-)*shoot", accidents.data$narrative) | 
                                    grepl("to drain", accidents.data$narrative) | 
                                    grepl("mod(i|y)f(y|ication)", accidents.data$narrative) | 
                                    grepl("(mount|splic|bolt|adjust|digg|drill|cutt|unload|dislodg|pump|lift|jack|lay|haul|spread|position|tap(p)*|air|oil|fuel|drain|hook)(ing|ign|ed)", accidents.data$narrative) |
                                    grepl("(mov|hang|chan|putt|load|pry|assembl|push|pul(l)*(l)*|swing|trim(m)*|carry|strip(p)*|torqu(e)*|shovel(l)*|plac|pick|dispos)(ing|ign|ed)", accidents.data$narrative) |
                                    grepl("(grind|tension|clip(p)*|notch|straighten|band|guid(e)*|throw|rotat|saw|apply|align|tear|(un)*screw|attach|latch|goug|clear|restor)(ing|ign|ed)", accidents.data$narrative) |
                                    grepl("(set(t)*|put(t)*|pump|tak|prim)(ing).{1,10}(tire|wheel|pump|oil|links|fuel)", accidents.data$narrative) | 
                                    grepl("tr(ied|ying|yign).{1,3}to.{1,3}(free|take( |-)*out|shut( |-)*down|position|start|lift|pry|press|rotate|roll|sep(e|a)rat|releas)", accidents.data$narrative) | 
                                    grepl("attempt(ed|ing)*.{1,3}to.{1,3}(free|take( |-)*out|position|shut( |-)*down|pry|lift|put|get|unplug|unstop|lay|clear|start|press|rotate|roll|sep(e|a)rat)", accidents.data$narrative), 1, 0)
accidents.data$othernoun = ifelse(grepl("anti( |-)*freeze", accidents.data$narrative) | 
                                    grepl("sweeper", accidents.data$narrative), 1, 0)

accidents.data$other.keyword = ifelse((accidents.data$otherverb == 1 | 
                                         accidents.data$othernoun == 1 |
                                         accidents.data$trash == 1 | 
                                         accidents.data$wrench == 1 |
                                         accidents.data$moretools == 1 | 
                                         accidents.data$loosen == 1 |
                                         accidents.data$tire == 1 | 
                                         accidents.data$splice == 1 |
                                         accidents.data$working.on == 1 | 
                                         accidents.data$barring == 1) &
                                        accidents.data$accident.only == 0, 1, 0)

# flag definitely and likely false positives (including accident-only observations)
accidents.data$false.pos = ifelse((accidents.data$carpal.tunnel == 1 | 
                                     accidents.data$cumulative == 1 | 
                                     accidents.data$heartattack == 1 |
                                     accidents.data$hearingloss == 1 | 
                                     accidents.data$exposure == 1 |
                                     accidents.data$unrelated == 1 | 
                                     accidents.data$accident.only == 1) & 
                                    accidents.data$adaboos == "YES", 1, 0)
accidents.data$false.pos = ifelse(accidents.data$adaboos == "YES" & 
                                    accidents.data$likely.keyword == 0 &
                                    accidents.data$maybe.keyword == 0 & 
                                    accidents.data$other.keyword == 0, 1, accidents.data$false.pos)  

# format classifications
accidents.data$MR  = ifelse((accidents.data$adaboost == "YES" & 
                               accidents.data$false.pos == 0) | 
                              accidents.data$false.neg == 1, 1, 0)

# remove unessential variables
accidents.data = accidents.data[, c(match("MR", names(accidents.data)),
                                    match("mineid", names(accidents.data)),
                                    match("accidentdate", names(accidents.data)),
                                    match("documentno", names(accidents.data)))]

# merge on predictions from training obs
accidents.data = merge(accidents.data, mr.data[,c("documentno", "MR")], by = "documentno", all = F)
accidents.data$MR.y = ifelse(accidents.data$MR.y == "YES", "1", "0")
accidents.data$MR = ifelse(!is.na(accidents.data$MR.x), accidents.data$MR.x, accidents.data$MR.y)

# remove unessential variables
accidents.data = accidents.data[, c(-grep("MR\\.[x|y]", names(accidents.data)))]

################################################################################

# OUTPUT CLASSIFIED DATA

# save a CSV file
write.csv(accidents.data, file = classified.accidents.file.name.csv)

# save R dataset
saveRDS(accidents.data, file = classified.accidents.file.name)

################################################################################

rm(list = ls())

################################################################################