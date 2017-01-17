# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Prepare MR (Maintenance and Repair) 
  # Train/Test:

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

################################################################################

library(zoo)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/data"
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
      # 1018 rows; 107 columns; unique on documentno 
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
  
  # bye
  rm(i, messy.rows, narrative.split)
  
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
  
  # CREATE LIKELY/MAYBE/UNLIKELY GROUPS OF VALUES OF CATEGORICAL VARIABLES
  
  mr.data$likely.occup = ifelse(grepl("maintenance", mr.data$occupation) & 
                                      mr.data$accident.only == 0, 1, 0)
  
  mr.data$maybe.occup = ifelse(grepl("electrician", mr.data$occupation) & 
                                     mr.data$accident.only == 0, 1, 0)
  
  mr.data$likely.activy = ifelse(grepl("maintenance", mr.data$mineractivity) | 
                                       grepl("wet down working place", mr.data$mineractivity) & 
                                       mr.data$accident.only == 0, 1, 0)
  
  mr.data$maybe.activy = ifelse(match("handling supplies/materials", mr.data$mineractivity) |
                                      match("hand tools (not powered)", mr.data$mineractivity) |
                                      match("no value found", mr.data$mineractivity) |
                                      match("unknown", mr.data$mineractivity) | 
                                      match("clean up", mr.data$mineractivity) | 
                                      match("inspect equipment", mr.data$mineractivity) & 
                                      mr.data$accident.only == 0, 1, 0)
  
  mr.data$likely.class = ifelse(match("handtools (nonpowered)", mr.data$accidentclassification) |
                                      match("machinery", mr.data$accidentclassification) |
                                      match("electrical", mr.data$accidentclassification) & 
                                      mr.data$accident.only == 0, 1, 0)
  
  mr.data$likely.source = ifelse((mr.data$sourceofinjury == "wrench" | 
                                        mr.data$sourceofinjury == "knife" |
                                        mr.data$sourceofinjury == "power saw" | 
                                        mr.data$sourceofinjury == "hand tools,nonpowered,nec" |
                                        mr.data$sourceofinjury == "crowbar,pry bar" | 
                                        mr.data$sourceofinjury == "axe,hammer,sledge") & 
                                       mr.data$accident.only == 0, 1, 0)
  mr.data$likely.source = ifelse(is.na(mr.data$likely.source), 0, mr.data$likely.source)
  
  # all "surgeries" are false keywords, but only "hoist/elevator" in combo with words that refer to elevator service are false keywords
  mr.data$false.keyword = ifelse((mr.data$repair & mr.data$surgery == 1 ) |
                                       (mr.data$fix & mr.data$surgery == 1 ) |
                                       (mr.data$rplace & mr.data$surgery == 1 ) |
                                       (mr.data$repair & mr.data$hoist == 1 ) |
                                       (mr.data$maintain & mr.data$hoist == 1 ) |
                                       (mr.data$service & mr.data$hoist == 1 ) |
                                       (mr.data$fix & mr.data$hoist == 1 ), 1, 0)
  
  mr.data$likely.keyword = ifelse((mr.data$repair == 1 | mr.data$fix == 1 | 
                                         mr.data$maintain == 1 | mr.data$rplace == 1 |
                                         mr.data$install == 1 | mr.data$service == 1 |
                                         mr.data$cleaning == 1 | mr.data$changing == 1 |
                                         mr.data$retrack == 1 | mr.data$inspect == 1 |
                                         mr.data$shovel == 1 | mr.data$reposition == 1 | 
                                         mr.data$pullbelt == 1 | mr.data$grease == 1 |
                                         mr.data$washingdown == 1 | mr.data$check == 1 |
                                         mr.data$oil == 1 | mr.data$mrworker == 1 |                                      
                                         mr.data$cover == 1 | mr.data$tests == 1 |
                                         mr.data$toolbox == 1 ) & mr.data$accident.only == 0 &
                                        mr.data$false.keyword == 0, 1, 0)
  
  mr.data$maybe.keyword = ifelse( (mr.data$remove == 1 | mr.data$dismantl == 1 | 
                                         mr.data$rethread == 1 | mr.data$welding == 1 | 
                                         mr.data$bits == 1 | mr.data$helping == 1 |
                                         mr.data$conveyor == 1 | mr.data$belt == 1 |
                                         mr.data$tighten == 1 | mr.data$battery == 1 ) & mr.data$accident.only == 0 &
                                        mr.data$false.keyword == 0, 1, 0)
  
  ################################################################################
  
  # PRODUCE VARIABLES THAT WILL ONLY BE USED AFTER CLASSIFICATION
  
  # same process for false negatives 
  mr.data$flashburn = ifelse(grepl("weld.{1,40}flash( |-)*burn", mr.data$narrative) | 
                               grepl("flash( |-)*burn.{1,40}weld", mr.data$narrative), 1, 0)
  
  # same process for false positives   
  mr.data$carpal.tunnel = ifelse((grepl("carp(a|u|e)l( |-)*tun(n)*(e|l)(e|l)", mr.data$narrative) | 
                                           grepl("bursitis", mr.data$narrative)) &
                                          !grepl("fracture", mr.data$narrative), 1, 0)
  mr.data$cumulative = ifelse(grepl("rep(e|i)t(e|a|i)ti(v|n)e", mr.data$narrative) | 
                                       grepl("(cumulative|degenerativ)", mr.data$narrative) |
                                       grepl("repeated(ed)*.{1,10}(mo(tion|vement)|trauma|irritation)", mr.data$narrative) |
                                       grepl("long( |-)*term", mr.data$narrative) | 
                                       grepl("slow( |-)*on( |-)*set", mr.data$narrative), 1, 0)
  mr.data$hearingloss = ifelse(grepl("hearing.los", mr.data$narrative) | 
                                 grepl("los.{1,10}hearing", mr.data$narrative) |
                                 grepl("n(o|i)(o|i)se exposur", mr.data$narrative) |
                                 grepl("dimini.{1,10}hearing", mr.data$narrative) |
                                 grepl("thr(e|i)s(h)*( |-)*hold( |-)*shift", mr.data$narrative) |
                                 grepl("shift.{1,4}change.{1,30}hear", mr.data$narrative) |
                                 grepl("exposur(e)*.{1,20}noise", mr.data$narrative), 1, 0)
  mr.data$exposure = ifelse(grepl("((prolon|occupation|long( |-)*term).{1,8}exposur)|(exposur.{1,20}(noise|we(a)*ther))|p(n|h)e(n)*umo(n)*co(nio)*sis", mr.data$narrative) | 
                              mr.data$natureofinjury == "pneumoconiosis,black lung", 1, 0)
  mr.data$heartattack = ifelse(grepl("heart( |-)*at(t)*ac(k|h)", mr.data$narrative) | 
                                 mr.data$natureofinjury == "heart attack", 1, 0)
  mr.data$unrelated = ifelse(grepl("not work relat", mr.data$narrative) | 
                               grepl("no.{1,20}(specific|single).{1,5}(accident|injury|indicent|exposure)", mr.data$narrative) |  
                               (grepl("no (accident|incident|injury)", mr.data$narrative) & 
                                  !grepl("no (accident|incident|injury).{1,5}report", mr.data$narrative)), 1, 0)
  
  # last ditch attempt to find likely verbs and nouns before dropping false positives 
  mr.data$working.on = ifelse(grepl("(to work|workin(g)*)( |-)*(on|in|under|out|at)", mr.data$narrative), 1, 0)
  mr.data$barring = ifelse(grepl("barr(ed|ing).{1,10}(rock|motor)", mr.data$narrative), 1, 0)
  mr.data$otherverb = ifelse(grepl("( |^)patch", mr.data$narrative) | 
                               grepl("re(-)*(build|pack|fuel|assembl|lin)", mr.data$narrative) | 
                               grepl("(re)*adjust", mr.data$narrative) | 
                               grepl("(secure|unplug)", mr.data$narrative) | 
                               grepl("trouble( |-)*shoot", mr.data$narrative) | 
                               grepl("to drain", mr.data$narrative) | 
                               grepl("mod(i|y)f(y|ication)", mr.data$narrative) | 
                               grepl("(mount|splic|bolt|adjust|digg|drill|cutt|unload|dislodg|pump|lift|jack|lay|haul|spread|position|tap(p)*|air|oil|fuel|drain|hook)(ing|ign|ed)", mr.data$narrative) |
                               grepl("(mov|hang|chan|putt|load|pry|assembl|push|pul(l)*(l)*|swing|trim(m)*|carry|strip(p)*|torqu(e)*|shovel(l)*|plac|pick|dispos)(ing|ign|ed)", mr.data$narrative) |
                               grepl("(grind|tension|clip(p)*|notch|straighten|band|guid(e)*|throw|rotat|saw|apply|align|tear|(un)*screw|attach|latch|goug|clear|restor)(ing|ign|ed)", mr.data$narrative) |
                               grepl("(set(t)*|put(t)*|pump|tak|prim)(ing).{1,10}(tire|wheel|pump|oil|links|fuel)", mr.data$narrative) | 
                               grepl("tr(ied|ying|yign).{1,3}to.{1,3}(free|take( |-)*out|shut( |-)*down|position|start|lift|pry|press|rotate|roll|sep(e|a)rat|releas)", mr.data$narrative) | 
                               grepl("attempt(ed|ing)*.{1,3}to.{1,3}(free|take( |-)*out|position|shut( |-)*down|pry|lift|put|get|unplug|unstop|lay|clear|start|press|rotate|roll|sep(e|a)rat)", mr.data$narrative), 1, 0)
  mr.data$othernoun = ifelse(grepl("anti( |-)*freeze", mr.data$narrative) | 
                               grepl("sweeper", mr.data$narrative), 1, 0)
  
  mr.data$other.keyword = ifelse((mr.data$otherverb == 1 | 
                                    mr.data$othernoun == 1 |
                                    mr.data$trash == 1 | 
                                    mr.data$wrench == 1 |
                                    mr.data$moretools == 1 | 
                                    mr.data$loosen == 1 |
                                    mr.data$tire == 1 |
                                    mr.data$splice == 1 |
                                    mr.data$working.on == 1 | 
                                    mr.data$barring == 1) &
                                   mr.data$accident.only == 0, 1, 0)
  
  post.algorithm = c("flashburn", "carpal.tunnel", "cumulative", 
                     "hearingloss", "exposure", "heartattack", 
                     "unrelated", "working.on", "barring",
                     "otherverb", "othernoun", "other.keyword")
  
  ################################################################################
  
  # DROP UNNECESSARY VARIABLES & FINAL FORMAT
  
  # create list of only necessary variables (used by final algorithm)
  keep = c("accidentdate", "accident.only", "battery", "belt", "bits", 
           "changing", "check", "cleaning", "conveyor", "cover", 
           "dismantl", "documentno", "falling.accident", "false.keyword", "fix", 
           "grease", "helping", "hoist", "inspect", "install", 
           "likely.activy", "likely.class", "likely.keyword", "likely.occup", 
           "likely.source", "loosen", "lug", "maintain", "maybe.activy", 
           "maybe.keyword", "maybe.occup", "mineid", "moretools", "MR", 
           "mrworker", "oil", "pain", "power", "pullbelt", 
           "remove", "repair", "reposition", "rethread", "retrack", 
           "rib.hole", "roller", "roof.bolt", "rplace", "service", 
           "shovel", "splice", "surgery", "tests", "tighten", 
           "tire", "toolbox", "trash", "type", "washingdown", 
           "welding", "wrench")
  
  if (purpose == "train.test") {
    # only use "keep" variables
    mr.data = mr.data[, (names(mr.data) %in% keep)]
    mr.data = mr.data[, c(-match("accidentdate", names(mr.data)),
                          -match("mineid", names(mr.data)),
                          -match("type", names(mr.data)))]
  }
  
  if (purpose == "classify") {
    # also preserve "post.algorithm" vars, which are used after boosting to eliminate false positives
    keep = c(keep, post.algorithm)
    mr.data = mr.data[, (names(mr.data) %in% keep)]
  }
  
  # bye
  rm(keep, post.algorithm)
  
  # enforce factor storage
  vars = names(mr.data)
  for (i in 1:length(vars)) {
    mr.data[, vars[i]] = factor(mr.data[, vars[i]])
  }
  
  ################################################################################
  
  # OUTPUT DATA
  
  # set seed so the randomizations are conducted equally each time this file is run
  set.seed(626)
  rand = runif(nrow(mr.data))
  mr.data = mr.data[order(rand),]

  if (purpose == "train.test") {
    # output prepped MR training set
      # 1018 rows; 58 columns; unique on documentno 
    saveRDS(mr.data, file = prepped.train.out.file.name)
  }
  
  if (purpose == "classify") {
    # output prepped and merged MR accidents data 
      # 75700 rows; 73 columns; unique on documentno 
    saveRDS(mr.data, file = prepped.classify.out.file.name)
  }
  
  ################################################################################
  
} # end of loop for classify and train.test

################################################################################

rm(list = ls())

################################################################################
