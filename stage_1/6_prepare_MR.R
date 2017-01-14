# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Prepare MR (Maintenance and Repair) 
  # Train/Test:

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
root = "/Users/Sarah/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"

# define file paths
cleaned.input.path = paste0(root, "/1_cleaned", collapse = NULL) 
merged.input.path = paste0(root, "/2_merged", collapse = NULL) 
prepped.output.path = paste0(root, "/5_prepped", collapse = NULL) 

# inputs
  # clean MR training set
training.set.in.file.name = paste0(cleaned.input.path, "/clean_MR_training_set.rds", collapse = NULL)
  # merged MR accidents
merged.MR.in.file.name =  paste0(merged.input.path, "/merged_MR_accidents.rds", collapse = NULL)

# outputs
  # prepped MR training set
prepped.train.out.file.name = paste0(prepped.output.path, "/prepped_MR_train_test.rds", collapse = NULL)
  # prepped and merged MR-accidents data
prepped.classify.out.file.name = paste0(prepped.output.path, "/prepped_MR_classify.rds", collapse = NULL)

# generate file paths
dir.create(prepped.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# DEFINE LOOP THAT WILL ITERATE THROUGH PURPOSES

for (purpose in c("train.test", "classify")) { # make datasets for both training/testing AND accident classification
  
  ################################################################################
  
  # READ DATA
  
  if (purpose == "train.test") {
    # read cleaned MR training set data
      # 1018 rows; 106 columns; unique on documentno 
    mr.data = readRDS(training.set.in.file.name)
    rm(training.set.in.file.name, root, cleaned.input.path, 
       merged.input.path, prepped.output.path)
  }
  
  if (purpose == "classify") {
    # read merged MR accidents data 
      # 75700 rows; 56 columns; unique on documentno 
    mr.data = readRDS(merged.MR.in.file.name)
    rm(merged.MR.in.file.name)
  }
  
  ################################################################################
  
  # CLEAN DATASET

  # destring variables
  mr.data[, grep("numberofemployees", names(mr.data))] = gsub(pattern = ",", replacement =  "", mr.data[, grep("numberofemployees", names(mr.data))])
  mr.data[, grep("numberofemployees", names(mr.data))] = as.numeric(mr.data[, grep("numberofemployees", names(mr.data))])
  mr.data[, grep("methaneliberation", names(mr.data))] = gsub(pattern = ",", replacement =  "", mr.data[, grep("methaneliberation", names(mr.data))])
  mr.data[, grep("methaneliberation", names(mr.data))] = as.numeric(mr.data[, grep("methaneliberation", names(mr.data))])
  mr.data[, grep("averagemineheight", names(mr.data))] = gsub(pattern = ",", replacement =  "", mr.data[, grep("averagemineheight", names(mr.data))])
  mr.data[, grep("averagemineheight", names(mr.data))] = as.numeric(mr.data[, grep("averagemineheight", names(mr.data))])
  
  # standardize "not-found" values within variables (e.g., "Unknown" or "Other" are changed to "NO VALUE FOUND")
  mr.data$uglocation = ifelse(mr.data$uglocation == "not marked", "no value found", mr.data$uglocation)
  mr.data$immediatenotificationclass = ifelse(mr.data$immediatenotificationclass == "not marked", "no value found", mr.data$immediatenotificationclass)
  mr.data$natureofinjury = ifelse(mr.data$natureofinjury == "unclassified,not determed", "no value found", mr.data$natureofinjury)
  mr.data$equipmanufacturer = ifelse(mr.data$equipmanufacturer == "not reported", "no value found", mr.data$equipmanufacturer)
  
  # 3 observations are coded as MR, but are apparently non-injury accident 
  mr.data$accident.only = ifelse(mr.data$degreeofinjury == "accident only" | mr.data$accidenttype == "acc type, without injuries", 1, 0)
  mr.data$MR = ifelse(mr.data$MR == "YES" & mr.data$accident.only == 0, 1, 0)
  mr.data$MR[mr.data$MR == "YES" & mr.data$accident.only == 1] = 0
  
  # format MR variable
  mr.data$MR = factor(ifelse(mr.data$MR == 1, "YES", "NO"))
  
  # some narrative fields are polluted with other columns; split and replace these  
  mr.data$messy = ifelse(grepl("\\|[0-9]*[0-9]*[0-9]*\\|", mr.data$narrative), 1, 0)
  narrative.split = strsplit(mr.data[mr.data$messy == 1, "narrative"], "|", fixed = T)
  messy.rows = row.names(mr.data[mr.data$messy == 1, ])
  for (i in 1:length(messy.rows)) {
    mr.data[messy.rows[i], "narrative"] = unlist(narrative.split[i])[1]
    mr.data[messy.rows[i], "occupcode3digit"] = unlist(narrative.split[i])[2]
    mr.data[messy.rows[i], "occupation"] = unlist(narrative.split[i])[3]
    mr.data[messy.rows[i], "returntoworkdate"] = unlist(narrative.split[i])[4]
  }
  mr.data = mr.data[, c(-match("messy", names(mr.data)))]
  
  # clean number typos in the narratives
  mr.data$numbertypo = ifelse(grepl("[a-z][0-9][a-z]", mr.data$narrative), 1, 0)
  for (i in 0:9) {
    mr.data[mr.data$numbertypo == 1, "narrative"] = gsub(i, "", mr.data[mr.data$numbertypo == 1, "narrative"])
  }
  
  # convert date variables
  indices.with.date = grep("date", names(mr.data))
  for (i in indices.with.date) {
    mr.data[,i] = as.Date(mr.data[,i], "%m/%d/%Y")
  }
  
  # bye
  rm(i, indices.with.date, messy.rows, narrative.split)
  
  ################################################################################
  
  # GENERATE POSITIVELY PREDICTIVE KEY WORDS
  
  mr.data$repair = ifelse(grepl("(^| )r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data$narrative) &
                            !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,20}hernia", mr.data$narrative) &
                            !grepl("hernia.{1,10}r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data$narrative) &
                            !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,10}wound", mr.data$narrative) &
                            !grepl("wound.{1,20}r(e|a)(p|[0-9])a(i*)r[a-z]*", mr.data$narrative), 1, 0)
  
  mr.data$rplace = ifelse(grepl("(^| )replac(e|i)[a-z]*", mr.data$narrative), 1, 0)
  
  mr.data$service = ifelse(grepl("serviced", mr.data$narrative) | 
                             grepl("servicing", mr.data$narrative), 1, 0)
  
  mr.data$fix = ifelse(grepl("(^| )fix[a-z]*", mr.data$narrative) & 
                         !grepl("(^| )fixture", mr.data$narrative), 1, 0) 
  
  mr.data$changing = ifelse(grepl("chang(e|ing|ed)( |-)*out", mr.data$narrative) |
                              (grepl("chang(e|ing|ed)", mr.data$narrative) & 
                                 !grepl("chang(e|ing|ed).{1,10}(shift|place|positi)", mr.data$narrative)), 1, 0)
  
  mr.data$retrack = ifelse(grepl("re(rail|track|trakc)(ed|ing)", mr.data$narrative) |
                             grepl("pull(ing|ed)*.{1,5}track", mr.data$narrative), 1, 0)
  
  mr.data$pullbelt = ifelse(grepl("pull( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", mr.data$narrative) |
                              grepl("(belt|rope|spool|tube|tubing).{1,15}pull( |ing|ed|s)*", mr.data$narrative) |
                              grepl("(belt|rope|spool|tube|tubing).{1,15}load( |ing|ed|s)*", mr.data$narrative) |
                              grepl("load( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", mr.data$narrative), 1, 0)
  
  mr.data$reposition = ifelse(grepl("re( |-)*pos(i|t)(i|t)(i|o)(i|o)n", mr.data$narrative), 1, 0) 
  
  mr.data$mrworker = ifelse(grepl("(mechanic|electrician|repairm(a|e)n)", mr.data$narrative), 1, 0) 
  
  mr.data$cover = ifelse((grepl("(replac|l(i|e)ft).{1,20}(panel|cover| lid|hood)", mr.data$narrative) |
                            grepl("(panel|cover| lid|hood){1,5}fell", mr.data$narrative) | 
                            grepl("drop.{1,10}(panel|cover| lid|hood)", mr.data$narrative)) &
                           !grepl("eye.{1,5}lid", mr.data$narrative), 1, 0) 
  
  mr.data$toolbox = ifelse(grepl("( |^)tool", mr.data$narrative), 1, 0)
  
  
  # "cleaning the rib" refers to a production activity (non-MR)
  mr.data$cleaning = ifelse(grepl("cl(ean|(e)*aning)", mr.data$narrative) & 
                              !grepl("clean.{1,10} rib", mr.data$narrative), 1, 0) 
  
  mr.data$maintain = ifelse(grepl("(^| )maint(ain|en|ean)[a-z]*", mr.data$narrative) | 
                              grepl("maint.{1,9}work", mr.data$narrative), 1, 0)
  
  # avoid "inspection"/"inspector" (noun) and grab mentions of "inspect" (verb) 
  mr.data$inspect = ifelse(grepl("inspect( |ed|s|ing|\\.|,|$)", mr.data$narrative), 1, 0)
  
  mr.data$shovel = ifelse(grepl("shovel(ing|ed).{1,5}coal)", mr.data$narrative) |
                            grepl("coal.{1,15}shovel(ing|ed)", mr.data$narrative) |
                            grepl("shovel(ing|ed).{1,20}belt", mr.data$narrative) |
                            grepl("shovel(ing|ed).{1,20}convey(e|o)r", mr.data$narrative) |
                            grepl("shovel(ing|ed).{1,20}tail( |-)*p(e|i)(e|i)ce", mr.data$narrative) |
                            grepl("shovel(ing|ed).{1,20}(head|drive|guide|bend|lagged|tail)*( |-)*pull(y|ey|ies|ys)", mr.data$narrative) |
                            grepl("shovel(ing|ed).{1,20}(roller|idler)", mr.data$narrative) |
                            grepl("shovel(ing|ed).{1,20}(west|header|drive)", mr.data$narrative), 1, 0)
  
  # avoid the noun "hose" and grab the verb "hose"; also avoid "whose" and "bullhose"
  mr.data$washingdown = ifelse(grepl("( |^|\\.|,)(wash|hose)(d|ed|ing| )", mr.data$narrative), 1, 0) 
  
  mr.data$grease = ifelse(grepl("greas(ed|ing|e|er)", mr.data$narrative), 1, 0) 
  
  # avoid "doctor checked out injury"
  mr.data$check = ifelse(grepl("che(c|k)(c|k)", mr.data$narrative) &
                           !grepl("doctor", mr.data$narrative) &
                           !grepl("hospital", mr.data$narrative) &
                           !grepl("emergency", mr.data$narrative) &
                           !grepl("clinic", mr.data$narrative), 1, 0) 
  
  # avoid "doctors' tests" and "testicles"
  mr.data$tests = ifelse(grepl("test(ing|ed)", mr.data$narrative) &
                           !grepl("doctor", mr.data$narrative) &
                           !grepl("hospital", mr.data$narrative) &
                           !grepl("emergency", mr.data$narrative) &
                           !grepl("clinic", mr.data$narrative), 1, 0) 
  
  # oil in mention of can/drum/barrel often means something is being greased (MR),
  # but it also apears in other contexts (being slipped on, lit, etc.)
  mr.data$oil = ifelse(grepl("(^| )(oil).{1,25}(can|drum|barrel|container)", mr.data$narrative) |
                         grepl("(can|drum|barrel|container).{1,25}oil", mr.data$narrative) | 
                         grepl("(chang|add)(e|ing).{1,6}(oil|fuel|equipment)", mr.data$narrative) |
                         grepl("(( |^)oil|fuel)ing", mr.data$narrative) | 
                         grepl("to (((change|add) (oil|fuel))|fuel)", mr.data$narrative), 1, 0) 
  
  mr.data$dismantl = ifelse(grepl("dismant(el|le|al|il|l)", mr.data$narrative), 1, 0) 
  mr.data$rethread = ifelse(grepl("re( |-)*thr(ea|e)d", mr.data$narrative), 1, 0)
  mr.data$remove = ifelse(grepl("re(m)*ov(e|ed|ing|al)", mr.data$narrative) | 
                            grepl("rem(o)*v(e|ed|ing|al)", mr.data$narrative), 1, 0) 
  
  mr.data$bits = ifelse(grepl("set(t)*(ing)*( |-)*bits", mr.data$narrative), 1, 0)
  mr.data$conveyor = ifelse(grepl("convey(o|e)r", mr.data$narrative), 1, 0)
  
  # flag "helping", not "help"
  mr.data$helping = ifelse(grepl("help(ed|in(g)*|er)", mr.data$narrative) |
                             grepl("assis(s)*t(ed|in(g))*", mr.data$narrative), 1, 0)
  
  mr.data$belt = ifelse(grepl("belt|spool|tube|tubing", mr.data$narrative), 1, 0)
  mr.data$tighten = ifelse(grepl("tighten", mr.data$narrative), 1, 0)
  mr.data$loosen = ifelse(grepl("loos(en|ing)", mr.data$narrative), 1, 0)
  
  # most cases of changing batteries are MR, but sometimes someone might trip on a charger, or be operating a "battery personnel carrier"
  mr.data$battery = ifelse(grepl("bat(t)*(e)*r(y|ies)", mr.data$narrative) &
                             !grepl("bat(t)*(e)*r(y|ies).{1,6}charg(er|ing)", mr.data$narrative) &
                             !grepl("bat(t)*(e)*r(y|ies).{1,8}person(n)*(el|le)", mr.data$narrative) &
                             !grepl("bat(t)*(e)*r(y|ies).{1,5}car(r)*i(e|o)r", mr.data$narrative), 1, 0)
  
  # installs are not MR if relevant to contrsution or production
  mr.data$roof.bolt = ifelse(grepl("(roof|rib)*( |-)*(bolt)(er|s|ing| |$|\\.|,).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data$narrative) | 
                               grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(roof|rib)*( |-)*(bolt)(er|s|ing| |$|\\.|,)", mr.data$narrative), 1, 0)  
  
  mr.data$rib.hole = ifelse(grepl("(rib)( |-)*(hole).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data$narrative) | 
                              grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(rib)( |-)*(hole)", mr.data$narrative), 1, 0)  
  
  mr.data$install = ifelse(grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", mr.data$narrative) &
                             !grepl("(^| )an( |e|n)i(s|n|t)(s|n|t)(s|n|t)alled", mr.data$narrative) & 
                             (mr.data$rib.hole != 1 & mr.data$roof.bolt != 1), 1, 0)
  
  ################################################################################
  
  # GENERATE NEGATIVELY PREDICTIVE KEY WORDS
  
  # "pain" and "injured" indicate surgical "repairs"
  mr.data$pain = ifelse(grepl("(^| )(pain|hurt)(s)*( |$|\\.|,|:)", mr.data$narrative), 1, 0)
  mr.data$injured = ifelse(grepl("injur", mr.data$narrative), 1, 0)
  
  # elevator-related
  mr.data$hoist = ifelse(((grepl("(^| )hoist(s| |$|\\.|,|:)", mr.data$narrative) |
                             grepl("(^| )el(e|a|i)vat(o|e)r", mr.data$narrative)) & 
                            !grepl("(elevat(o|e)r|hoist).{1,10}(door|cable|hous|cylinder|hook|rope|convey(o|e)r|drum|jack|motor|chain|grip|brake|pedal|slope|cage|frame)", mr.data$narrative) & 
                            !grepl("(door|cable|hous|cylinder|hook|rope|convey(o|e)r|drum|jack|motor|chain|grip|brake|pedal|slope|cage|frame).{1,10}(elevat(o|e)r|hoist)", mr.data$narrative) & 
                            !grepl("(^| )us(e|ing).{1,10}(elevat(o|e)r|hoist)", mr.data$narrative) & 
                            mr.data$pain == 0 & 
                            mr.data$injured == 0), 1, 0) 
  
  mr.data$surgery = ifelse((grepl("surger[a-z]*", mr.data$narrative) | 
                              grepl("surgic[a-z]*", mr.data$narrative)) & 
                             mr.data$pain == 0 & 
                             mr.data$injured == 0, 1, 0)
  
  ################################################################################
  
  # GENERATE ADDITIONAL KEY WORDS ABOUT WHICH WE HAVE NO PRIORS
  
  mr.data$power = ifelse(grepl("pow(e)*r", mr.data$narrative), 1, 0)
  
  mr.data$splice = ifelse(grepl("splice", mr.data$narrative) & 
                            (mr.data$occupcode3digit %in% c("004", "418")), 1, 0)
  
  mr.data$lug = ifelse(grepl("( |^)lug(g)*", mr.data$narrative) & 
                         (mr.data$occupcode3digit %in% c("004", "418")), 1, 0)
  
  mr.data$wrench = ifelse(grepl("wrench", mr.data$narrative), 1, 0)
  mr.data$trash = ifelse(grepl("(trash|garbage|dumpster)", mr.data$narrative), 1, 0)
  mr.data$roller = ifelse(grepl("roller", mr.data$narrative), 1, 0)
  
  mr.data$moretools = ifelse(grepl("(pry|crow|jack)( |-)*bar", mr.data$narrative) | 
                               grepl("(hammer|screw( |-)*driver|shovel( |\\.|$|,|:)|ratchet)", mr.data$narrative) | 
                               grepl("com(e)*(-)*(a)*(-)*long", mr.data$narrative), 1, 0)
  
  mr.data$welding = ifelse((grepl("(( |^)tank|ac(c)*etyle(ne|en)|weld)", mr.data$narrative) | 
                              grepl("(oxygen|o2)( )*(bottle|cylinder)", mr.data$narrative)) &
                             !grepl("chemic.{1,10}tank)", mr.data$narrative), 1, 0)
  
  mr.data$tire = ifelse(grepl("(chang|pump)(e|ed|ing).{1,5}tire", mr.data$narrative), 1, 0)
  
  ################################################################################
  
  # GENERATE FLAGS ABOUT THE ACCIDENT (E.G. DID SOMETHING FALL?)
  
  mr.data$falling.class = ifelse(mr.data$accidentclassification == "fall of roof or back", 1, 0)
  
  mr.data$falling.word = ifelse(grepl("rock( )*fell", mr.data$narrative) |
                                  grepl("fell.{1,20}roof", mr.data$narrative) |
                                  grepl("roof( )*f(a|e)ll", mr.data$narrative), 1, 0)
  
  mr.data$falling.accident = ifelse(mr.data$falling.class == 1 | 
                                      mr.data$falling.word == 1, 1, 0)
  
  mr.data$falling.class =
    mr.data$falling.word = NULL
  
  mr.data$accident.only = ifelse((mr.data$degreeofinjury == "accident only" | 
                                    mr.data$accidenttype == "acc type, without injuries"), 1, 0)
  
  ################################################################################
  
  # PREPARE TIME AND DATE VARIABLES

  date = strptime(mr.data$calendaryear, "%Y")
  format(date, "%Y")
  mr.data$year = format(date, "%Y")
  mr.data$quarter = as.yearqtr(mr.data$accidentdate,"%Y-%m-%d")
  mr.data = mr.data[, c(-grep("calendar", names(mr.data)), 
                        -grep("accidentdate", names(mr.data)))]
  
  drop = c("roof.bolt", "rib.hole", "transferredorterminated",
           "accidenttime", "accidenttypecode", "activitycode", 
           "bodypartcode", "controllerid", "classificationcode",
           "degreeofinjurycode", "equiptypecode", "equipmanufacturercode",
           "fipsstatecode", "injurysourcecode", "natureofinjurycode",
           "immediatenotificationcode", "occupcode3digit", "operatorid",
           "subunitcode", "uglocationcode", "ugminingmethodcode")
  simple.data = mr.data[, !(names(mr.data) %in% drops)] 
  
  # create lists storing the types of variables
  var_classes = sapply(mr.data[,names(mr.data)], class)
  charac_vars = names(var_classes[c(grep("character", var_classes), grep("factor", var_classes))])
  num_vars = names(var_classes[c(grep("numeric", var_classes), grep("integer", var_classes))])
  
  for (i in 1:length(charac_vars)) {
    mr.data[, charac_vars[i]] = ifelse((mr.data[,charac_vars[i]] == "no value found" | 
                                          mr.data[,charac_vars[i]] == "unknown" | 
                                          mr.data[,charac_vars[i]] == "?" | 
                                          mr.data[,charac_vars[i]] == ""), NA_character_, as.character(mr.data[,charac_vars[i]]))
    mr.data[, charac_vars[i]] = factor(mr.data[, charac_vars[i]])
  }
  
  ################################################################################
  
  # CREATE SIMPLE DATA CONTAINING JUST THE VARIABLES USED FOR ANALYSIS
  
  # drop remaining unnecessary vars
  drop = c("bodypart", "contractor_accident","contractorid", 
           "controllername", "dayslost", "daysrestrictedduty",
           "equipmanufacturer", "equipmentmodelno", "immediatenotificationclass",
           "injured", "investigationbegindate", "jobexperience", 
           "mineexperience", "natureofinjury", "numberofinjuries", 
           "numbertypo", "operatorname", "quarter", 
           "returntoworkdate", "schedulechargedays", "shiftbeginningtime",       
           "subunit", "totalexperience", "typeofequipment",
           "uglocation", "ugminingmethod", "year")
  simple.data = mr.data[, !(names(mr.data) %in% drop)] 
  
  ################################################################################
  
  # CREATE LIKELY/MAYBE/UNLIKELY GROUPS OF VALUES OF CATEGORICAL VARIABLES
  
  simple.data$likely.occup = ifelse(grepl("maintenance", simple.data$occupation) & 
                                      simple.data$accident.only == 0, 1, 0)
  
  simple.data$maybe.occup = ifelse(grepl("electrician", simple.data$occupation) & 
                                     simple.data$accident.only == 0, 1, 0)
  
  simple.data$likely.activy = ifelse(grepl("maintenance", simple.data$mineractivity) | 
                                       grepl("wet down working place", simple.data$mineractivity) & 
                                       simple.data$accident.only == 0, 1, 0)
  
  simple.data$maybe.activy = ifelse(match("handling supplies/materials", simple.data$mineractivity) |
                                      match("hand tools (not powered)", simple.data$mineractivity) |
                                      match("no value found", simple.data$mineractivity) |
                                      match("unknown", simple.data$mineractivity) | 
                                      match("clean up", simple.data$mineractivity) | 
                                      match("inspect equipment", simple.data$mineractivity) & 
                                      simple.data$accident.only == 0, 1, 0)
  
  simple.data$likely.class = ifelse(match("handtools (nonpowered)", simple.data$accidentclassification) |
                                      match("machinery", simple.data$accidentclassification) |
                                      match("electrical", simple.data$accidentclassification) & 
                                      simple.data$accident.only == 0, 1, 0)
  
  simple.data$likely.source = ifelse((simple.data$sourceofinjury == "wrench" | 
                                        simple.data$sourceofinjury == "knife" |
                                        simple.data$sourceofinjury == "power saw" | 
                                        simple.data$sourceofinjury == "hand tools,nonpowered,nec" |
                                        simple.data$sourceofinjury == "crowbar,pry bar" | 
                                        simple.data$sourceofinjury == "axe,hammer,sledge") & 
                                       simple.data$accident.only == 0, 1, 0)
  
  # all "surgeries" are false keywords, but only "hoist/elevator" in combo with words that refer to elevator service are false keywords
  simple.data$false.keyword = ifelse((simple.data$repair & simple.data$surgery == 1 ) |
                                       (simple.data$fix & simple.data$surgery == 1 ) |
                                       (simple.data$rplace & simple.data$surgery == 1 ) |
                                       (simple.data$repair & simple.data$hoist == 1 ) |
                                       (simple.data$maintain & simple.data$hoist == 1 ) |
                                       (simple.data$service & simple.data$hoist == 1 ) |
                                       (simple.data$fix & simple.data$hoist == 1 ), 1, 0)
  
  simple.data$likely.keyword = ifelse((simple.data$repair == 1 | simple.data$fix == 1 | 
                                         simple.data$maintain == 1 | simple.data$rplace == 1 |
                                         simple.data$install == 1 | simple.data$service == 1 |
                                         simple.data$cleaning == 1 | simple.data$changing == 1 |
                                         simple.data$retrack == 1 | simple.data$inspect == 1 |
                                         simple.data$shovel == 1 | simple.data$reposition == 1 | 
                                         simple.data$pullbelt == 1 | simple.data$grease == 1 |
                                         simple.data$washingdown == 1 | simple.data$check == 1 |
                                         simple.data$oil == 1 | simple.data$mrworker == 1 |                                      
                                         simple.data$cover == 1 | simple.data$tests == 1 |
                                         simple.data$toolbox == 1 ) & simple.data$accident.only == 0 &
                                        simple.data$false.keyword == 0, 1, 0)
  
  simple.data$maybe.keyword = ifelse( (simple.data$remove == 1 | simple.data$dismantl == 1 | 
                                         simple.data$rethread == 1 | simple.data$welding == 1 | 
                                         simple.data$bits == 1 | simple.data$helping == 1 |
                                         simple.data$conveyor == 1 | simple.data$belt == 1 |
                                         simple.data$tighten == 1 | simple.data$battery == 1 ) & simple.data$accident.only == 0 &
                                        simple.data$false.keyword == 0, 1, 0)
  
  # remove all categorical variables - keep narratives and document number for model training
  simple.data = simple.data[, c(-grep("accidentclassification", names(simple.data)),
                                -grep("accidenttype", names(simple.data)), 
                                -grep("degreeofinjury", names(simple.data)), 
                                -grep("mineractivity", names(simple.data)),
                                -grep("occupation", names(simple.data)), 
                                -grep("sourceofinjury", names(simple.data)))]
  
  # now drop categorical variables from data
  drop = c("sourceofinjury", "equipmentmodelno", "fipscountyname", 
           "controllername", "mineractivity", "minename", 
           "operatorname", "quarter", "occupation")
  simple.data = simple.data[, !(names(simple.data) %in% drop)]
  
  ################################################################################
  
  # BEGIN ALGORITHM - RANDOMLY SORT DATA 
  
  # set seed so the randomizations are conducted equally each time this file is run
  set.seed(626)
  rand = runif(nrow(mr.data))
  train = mr.data[order(rand),]
  rand2 = runif(nrow(simple.data))
  simple = simple.data[order(rand2),]

  # this code in just to find out which column number the MR indicator is, so we can report accuracy
  which(colnames(train)=="MR")
  which(colnames(simple)=="MR")
  
  # bye 
  rm(rand, rand2)
  
  ################################################################################
  
  # OUTPUT DATA
  
  if (purpose == "train.test") {
    # output prepped MR training set
      # 1018 rows; 101 columns; unique on documentno 
    saveRDS(mr.data, file = prepped.train.out.file.name)
  }
  
  if (purpose == "classify") {
    # output prepped and merged MR accidents data 
      # 75743 rows; 101 columns; unique on documentno 
    saveRDS(mr.data, file = prepped.classify.out.file.name)
  }
  
  ################################################################################
  
} # end of loop for classify and train.test

################################################################################

rm(list = ls())

################################################################################
