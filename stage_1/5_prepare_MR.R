# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 5 - Prepare MR (Maintenance and Repair) Accidents Data
  # Prepares MR training/testing set (produced in 2_clean_MR_train_test_set) 
    # or merged MR accidents data (produced in 4_merge_accidents)
    # to be used for classification algorithm training and testing 
    # or accident classification purposes, respectively
  # Generates key word and category variables for classification purposes

# Coded by: Sarah Levine, sarah.michael.levine@gmail.com
      # and Nikhil Saifullah, nikhil.saifullah@gmail.com

# Last edit 1/25/2017

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
cleaned.path = paste0(root, "/data/1_cleaned", collapse = NULL) 
merged.path = paste0(root, "/data/2_merged", collapse = NULL) 
prepared.path = paste0(root, "/data/5_prepared", collapse = NULL) 

# inputs
  # cleaned MR training/testing set
    # produced in 2_clean_MR_train_test_set
train.test.set.in.file.name = paste0(cleaned.path, "/clean_MR_train_test_set.rds", collapse = NULL)
  # merged MR accidents data
    # produced in 4_merge_accidents
merged.data.in.file.name =  paste0(merged.path, "/merged_MR_accidents.rds", collapse = NULL)

# outputs
  # prepared MR training/testing set
prepared.train.test.out.file.name = paste0(prepared.path, "/prepared_MR_train_test.rds", collapse = NULL)
  # prepared merged MR accidents data
prepared.classify.out.file.name = paste0(prepared.path, "/prepared_MR_classify.rds", collapse = NULL)

# generate file paths
dir.create(prepared.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, cleaned.path, merged.path, prepared.path)

SEED = read.csv("C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/seeds3.csv")

################################################################################
purpose = "train.test"
#for (purpose in c("train.test", "classify")) { # prepare datasets for both training/testing and classification purposes
  
  # READ DATA
  
  if (purpose == "train.test") {
    # read cleaned MR training/testing set
      # 1018 rows; 18 columns; unique on documentno 
    data = readRDS(train.test.set.in.file.name)
    rm(train.test.set.in.file.name)
  }
  
  if (purpose == "classify") {
    # read merged MR accidents data 
      # 75700 rows; 19 columns; unique on documentno 
    data = readRDS(merged.data.in.file.name)
    rm(merged.data.in.file.name)
  }

  ##############################################################################
  
  # CLEAN DATA

  # drop non-injury accidents 
  data$accident.only = ifelse(data$degreeofinjury == "accident only" | data$accidenttype == "acc type, without injuries", 1, 0)
  data$MR = ifelse(data$MR == "YES" & data$accident.only == 0, 1, 0)
  data$MR = factor(ifelse(data$MR == 1, "YES", "NO"))
  
  # clean number typos
  data$numbertypo = ifelse(grepl("[a-z][0-9][a-z]", data$narrative), 1, 0)
  for (i in 0:9) {
    data[data$numbertypo == 1, "narrative"] = gsub(i, "", data[data$numbertypo == 1, "narrative"])
  }
  
  # clean "not-found" values
  data$natureofinjury = ifelse(data$natureofinjury == "unclassified,not determed", "no value found", data$natureofinjury)
  
  # bye
  rm(i)
  
  ##############################################################################
  
  # GENERATE POSITIVELY PREDICTIVE KEY WORDS
  
  data$repair = ifelse(grepl("(^| )r(e|a)(p|[0-9])a(i*)r[a-z]*", data$narrative) &
                         !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,20}hernia", data$narrative) &
                         !grepl("hernia.{1,10}r(e|a)(p|[0-9])a(i*)r[a-z]*", data$narrative) &
                         !grepl("r(e|a)(p|[0-9])a(i*)r[a-z]*.{1,10}wound", data$narrative) &
                         !grepl("wound.{1,20}r(e|a)(p|[0-9])a(i*)r[a-z]*", data$narrative), 1, 0)
  
  data$rplace = ifelse(grepl("(^| )replac(e|i)[a-z]*", data$narrative), 1, 0)
  
  data$service = ifelse(grepl("serviced", data$narrative) | 
                          grepl("servicing", data$narrative), 1, 0)
  
  data$fix = ifelse(grepl("(^| )fix[a-z]*", data$narrative) & 
                      !grepl("(^| )fixture", data$narrative), 1, 0) 
  
  data$changing = ifelse(grepl("chang(e|ing|ed)( |-)*out", data$narrative) |
                           (grepl("chang(e|ing|ed)", data$narrative) & 
                              !grepl("chang(e|ing|ed).{1,10}(shift|place|positi)", data$narrative)), 1, 0)
  
  data$retrack = ifelse(grepl("re(rail|track|trakc)(ed|ing)", data$narrative) |
                          grepl("pull(ing|ed)*.{1,5}track", data$narrative), 1, 0)
  
  data$pullbelt = ifelse(grepl("pull( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", data$narrative) |
                           grepl("(belt|rope|spool|tube|tubing).{1,15}pull( |ing|ed|s)*", data$narrative) |
                           grepl("(belt|rope|spool|tube|tubing).{1,15}load( |ing|ed|s)*", data$narrative) |
                           grepl("load( |ing|ed|s)*.{1,15}(belt|rope|tube|tubing)", data$narrative), 1, 0)
  
  data$reposition = ifelse(grepl("re( |-)*pos(i|t)(i|t)(i|o)(i|o)n", data$narrative), 1, 0) 
  
  data$mrworker = ifelse(grepl("(mechanic|electrician|repairm(a|e)n)", data$narrative), 1, 0) 
  
  data$cover = ifelse((grepl("(replac|l(i|e)ft).{1,20}(panel|cover| lid|hood)", data$narrative) |
                         grepl("(panel|cover| lid|hood){1,5}fell", data$narrative) | 
                         grepl("drop.{1,10}(panel|cover| lid|hood)", data$narrative)) &
                        !grepl("eye.{1,5}lid", data$narrative), 1, 0) 
  
  data$toolbox = ifelse(grepl("( |^)tool", data$narrative), 1, 0)
  
  # "cleaning the rib" refers to a production activity (non-MR)
  data$cleaning = ifelse(grepl("cl(ean|(e)*aning)", data$narrative) & 
                           !grepl("clean.{1,10} rib", data$narrative), 1, 0) 
  
  data$maintain = ifelse(grepl("(^| )maint(ain|en|ean)[a-z]*", data$narrative) | 
                           grepl("maint.{1,9}work", data$narrative), 1, 0)
  
  # avoid "inspection"/"inspector" (noun)
  data$inspect = ifelse(grepl("inspect( |ed|s|ing|\\.|,|$)", data$narrative), 1, 0)
  
  data$shovel = ifelse(grepl("shovel(ing|ed).{1,5}coal)", data$narrative) |
                         grepl("coal.{1,15}shovel(ing|ed)", data$narrative) |
                         grepl("shovel(ing|ed).{1,20}belt", data$narrative) |
                         grepl("shovel(ing|ed).{1,20}convey(e|o)r", data$narrative) |
                         grepl("shovel(ing|ed).{1,20}tail( |-)*p(e|i)(e|i)ce", data$narrative) |
                         grepl("shovel(ing|ed).{1,20}(head|drive|guide|bend|lagged|tail)*( |-)*pull(y|ey|ies|ys)", data$narrative) |
                         grepl("shovel(ing|ed).{1,20}(roller|idler)", data$narrative) |
                         grepl("shovel(ing|ed).{1,20}(west|header|drive)", data$narrative), 1, 0)
  
  # avoid the noun "hose" and grab the verb "hose"; also avoid "whose" and "bullhose"
  data$washingdown = ifelse(grepl("( |^|\\.|,)(wash|hose)(d|ed|ing| )", data$narrative), 1, 0) 
  
  data$grease = ifelse(grepl("greas(ed|ing|e|er)", data$narrative), 1, 0) 
  
  # avoid doctor checking injury
  data$check = ifelse(grepl("che(c|k)(c|k)", data$narrative) &
                        !grepl("doctor", data$narrative) &
                        !grepl("hospital", data$narrative) &
                        !grepl("emergency", data$narrative) &
                        !grepl("clinic", data$narrative), 1, 0) 
  
  # avoid doctors' tests and testicles
  data$tests = ifelse(grepl("test(ing|ed)", data$narrative) &
                        !grepl("doctor", data$narrative) &
                        !grepl("hospital", data$narrative) &
                        !grepl("emergency", data$narrative) &
                        !grepl("clinic", data$narrative), 1, 0) 
  
  # oil in mention of can/drum/barrel often means something is being greased (MR),
    # but it also apears in other contexts (being slipped on, lit, etc.)
  data$oil = ifelse(grepl("(^| )(oil).{1,25}(can|drum|barrel|container)", data$narrative) |
                      grepl("(can|drum|barrel|container).{1,25}oil", data$narrative) | 
                      grepl("(chang|add)(e|ing).{1,6}(oil|fuel|equipment)", data$narrative) |
                      grepl("(( |^)oil|fuel)ing", data$narrative) | 
                      grepl("to (((change|add) (oil|fuel))|fuel)", data$narrative), 1, 0) 
  
  data$dismantl = ifelse(grepl("dismant(el|le|al|il|l)", data$narrative), 1, 0) 
  
  data$rethread = ifelse(grepl("re( |-)*thr(ea|e)d", data$narrative), 1, 0)
  
  data$remove = ifelse(grepl("re(m)*ov(e|ed|ing|al)", data$narrative) | 
                         grepl("rem(o)*v(e|ed|ing|al)", data$narrative), 1, 0) 
  
  data$bits = ifelse(grepl("set(t)*(ing)*( |-)*bits", data$narrative), 1, 0)
  
  data$conveyor = ifelse(grepl("convey(o|e)r", data$narrative), 1, 0)
  
  # flag "helping" but not "help"
  data$helping = ifelse(grepl("help(ed|in(g)*|er)", data$narrative) |
                          grepl("assis(s)*t(ed|in(g))*", data$narrative), 1, 0)
  
  data$belt = ifelse(grepl("belt|spool|tube|tubing", data$narrative), 1, 0)
  
  data$tighten = ifelse(grepl("tighten", data$narrative), 1, 0)
  
  data$loosen = ifelse(grepl("loos(en|ing)", data$narrative), 1, 0)
  
  # most cases of changing batteries are MR
    # but sometimes someone might trip on a charger, or be operating a "battery personnel carrier"
  data$battery = ifelse(grepl("bat(t)*(e)*r(y|ies)", data$narrative) &
                          !grepl("bat(t)*(e)*r(y|ies).{1,6}charg(er|ing)", data$narrative) &
                          !grepl("bat(t)*(e)*r(y|ies).{1,8}person(n)*(el|le)", data$narrative) &
                          !grepl("bat(t)*(e)*r(y|ies).{1,5}car(r)*i(e|o)r", data$narrative), 1, 0)
  
  # installs are not MR if relevant to construction or production
  data$roof.bolt = ifelse(grepl("(roof|rib)*( |-)*(bolt)(er|s|ing| |$|\\.|,).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", data$narrative) | 
                            grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(roof|rib)*( |-)*(bolt)(er|s|ing| |$|\\.|,)", data$narrative), 1, 0)  
  
  data$rib.hole = ifelse(grepl("(rib)( |-)*(hole).{1,20}(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", data$narrative) | 
                           grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*.{1,20}(rib)( |-)*(hole)", data$narrative), 1, 0)  
  
  data$install = ifelse(grepl("(^| |e|n)i(s|n|t)(s|n|t)(s|n|t)al[a-z]*", data$narrative) &
                          !grepl("(^| )an( |e|n)i(s|n|t)(s|n|t)(s|n|t)alled", data$narrative) & 
                          (data$rib.hole != 1 & data$roof.bolt != 1), 1, 0)
  
  ##############################################################################
  
  # GENERATE NEGATIVELY PREDICTIVE KEY WORDS
  
  # "pain" and "injured" indicate surgical "repairs"
  data$pain = ifelse(grepl("(^| )(pain|hurt)(s)*( |$|\\.|,|:)", data$narrative), 1, 0)
    data$injured = ifelse(grepl("injur", data$narrative), 1, 0)
  
  # elevator-related
  data$hoist = ifelse(((grepl("(^| )hoist(s| |$|\\.|,|:)", data$narrative) |
                          grepl("(^| )el(e|a|i)vat(o|e)r", data$narrative)) & 
                         !grepl("(elevat(o|e)r|hoist).{1,10}(door|cable|hous|cylinder|hook|rope|convey(o|e)r|drum|jack|motor|chain|grip|brake|pedal|slope|cage|frame)", data$narrative) & 
                         !grepl("(door|cable|hous|cylinder|hook|rope|convey(o|e)r|drum|jack|motor|chain|grip|brake|pedal|slope|cage|frame).{1,10}(elevat(o|e)r|hoist)", data$narrative) & 
                         !grepl("(^| )us(e|ing).{1,10}(elevat(o|e)r|hoist)", data$narrative) & 
                         data$pain == 0 & 
                         data$injured == 0), 1, 0) 
  
  data$surgery = ifelse((grepl("surger[a-z]*", data$narrative) | 
                           grepl("surgic[a-z]*", data$narrative)) & 
                          data$pain == 0 & 
                          data$injured == 0, 1, 0)
  
  ##############################################################################
  
  # GENERATE KEY WORDS ABOUT WHICH WE HAVE NO PRIORS
  
  data$power = ifelse(grepl("pow(e)*r", data$narrative), 1, 0)
  
  data$splice = ifelse(grepl("splice", data$narrative) & 
                         (data$occupcode3digit %in% c("004", "418")), 1, 0)
  
  data$lug = ifelse(grepl("( |^)lug(g)*", data$narrative) & 
                      (data$occupcode3digit %in% c("004", "418")), 1, 0)
  
  data$wrench = ifelse(grepl("wrench", data$narrative), 1, 0)
  
  data$trash = ifelse(grepl("(trash|garbage|dumpster)", data$narrative), 1, 0)
  
  data$roller = ifelse(grepl("roller", data$narrative), 1, 0)
  
  data$moretools = ifelse(grepl("(pry|crow|jack)( |-)*bar", data$narrative) | 
                            grepl("(hammer|screw( |-)*driver|shovel( |\\.|$|,|:)|ratchet)", data$narrative) | 
                            grepl("com(e)*(-)*(a)*(-)*long", data$narrative), 1, 0)
  
  data$welding = ifelse((grepl("(( |^)tank|ac(c)*etyle(ne|en)|weld)", data$narrative) | 
                           grepl("(oxygen|o2)( )*(bottle|cylinder)", data$narrative)) &
                          !grepl("chemic.{1,10}tank)", data$narrative), 1, 0)
  
  data$tire = ifelse(grepl("(chang|pump)(e|ed|ing).{1,5}tire", data$narrative), 1, 0)
  
  ##############################################################################
  
  # GENERATE KEY WORDS RELATED TO THE ACCIDENT (E.G., DID SOMETHING FALL?)
  
  data$falling.class = ifelse(data$accidentclassification == "fall of roof or back", 1, 0)
  
  data$falling.word = ifelse(grepl("rock( )*fell", data$narrative) |
                               grepl("fell.{1,20}roof", data$narrative) |
                               grepl("roof( )*f(a|e)ll", data$narrative), 1, 0)
  
  data$falling.accident = ifelse(data$falling.class == 1 | data$falling.word == 1, 1, 0)
  
  data$accident.only = ifelse(data$degreeofinjury == "accident only" | 
                                data$accidenttype == "acc type, without injuries", 1, 0)
  
  ##############################################################################
  
  ### ADD COMMENTS HERE
  
  # IMPUTATION
  
  if (purpose == "train.test") {
    
    num.vars = c(paste("temp", 1:16, sep = "."))
    
    charac.vars = c(paste("temp", 17:18, sep = "."), 
                    "accidentclassification", "accidenttype", "sourceofinjury", "natureofinjury", 
                    paste("temp", 19:23, sep = "."), 
                    "mineractivity", "occupation", 
                    paste("temp", 24:25, sep = "."))
    
    data[, paste("temp", 1:25, sep = ".")] = NA
    
    set.seed(100)
    for (var in names(data)[grepl("temp", names(data))]) {
      data[, var] = sample(1:100, nrow(data), replace = TRUE)
    }
    
    data$temp.1[1:400] = NA
    data$temp.2[1:372] = NA
    data$temp.3[1:379] = NA
    data$temp.4[1:89] = NA
    data$temp.5[1:50] = NA
    data$temp.6[1:1] = NA
    data$temp.7[1:2] = NA
    data$temp.8[1:762] = NA
    data$temp.9[1:396] = NA
    data$temp.10[1:767] = NA
    data$temp.11[1:1002] = NA
    data$temp.12[1:388] = NA
    data$temp.13[1:358] = NA
    data$temp.14[1:343] = NA
    data$temp.15[1:1016] = NA
    data$temp.16[1:404] = NA
    data$temp.17[1:996] = NA
    data$temp.18[1:137] = NA
    data$temp.19[1:355] = NA
    data$temp.20[1:624] = NA
    data$temp.21[1:746] = NA
    data$temp.22[1:836] = NA
    data$temp.23[1:655] = NA
    data$temp.24[1:1] = NA
    data$temp.25[1:424] = NA
    
    for (var in num.vars) {
      data[, var] = as.numeric(data[, var])
    }
    
    for (var in charac.vars) {
      data[, var] = as.character(data[, var])
    }
    
    for (i in 1:length(charac.vars)) {
      data[, charac.vars[i]] = ifelse((data[,charac.vars[i]] == "no value found" | 
                                         data[,charac.vars[i]] == "unknown" | 
                                         data[,charac.vars[i]] == "?" | 
                                         data[,charac.vars[i]] == ""), NA_character_, as.character(data[,charac.vars[i]]))
      data[, charac.vars[i]] = factor(data[, charac.vars[i]])
    }
    
    for (var in charac.vars) {
      print(var)
      print(sum(is.na(data[, var])))
    }
    
    set.seed(625)
    j = 0
    for (i in 1:length(num.vars)) {
      i.rowsmissing = row.names(data)[is.na(data[, num.vars[i]])]
      while (sum(!complete.cases(data[, num.vars[i]])) > 0) {
        replace.rows = sample(setdiff(row.names(data), i.rowsmissing), length(i.rowsmissing), replace = T)
        
        j = j + 1
        
        #.Random.seed = SEED[, j]
        
        assign(paste0("new.seed", j), get(".Random.seed", .GlobalEnv))
        
        data[i.rowsmissing, num.vars[i]] = data[replace.rows, num.vars[i]]
      }
    }
    for (i in 1:length(charac.vars)) {
      i.rowsmissing = row.names(data)[is.na(data[, charac.vars[i]])]
      while (sum(!complete.cases(data[, charac.vars[i]])) > 0) {
        replace.rows = sample(setdiff(row.names(data), i.rowsmissing), length(i.rowsmissing), replace = T)
        
        j = j + 1
        
        #.Random.seed = SEED[, j]
        
        assign(paste0("new.seed", j), get(".Random.seed", .GlobalEnv))
        
        data[i.rowsmissing, charac.vars[i]] = data[replace.rows, charac.vars[i]]
        
      }
    }
  }
  
  ##############################################################################
  
  # CREATE LIKELY/MAYBE LIKELY/UNLIKELY GROUPS FROM CATEGORICAL VARIABLES
  
  data$likely.occup = ifelse(grepl("maintenance", data$occupation) & 
                               data$accident.only == 0, 1, 0)
  
  data$maybe.occup = ifelse(grepl("electrician", data$occupation) & 
                              data$accident.only == 0, 1, 0)
  
  data$likely.activity = ifelse(grepl("maintenance", data$mineractivity) | 
                                  grepl("wet down working place", data$mineractivity) & 
                                  data$accident.only == 0, 1, 0)
  
  data$maybe.activity = ifelse(data$mineractivity == "handling supplies/materials" |
                                 data$mineractivity == "hand tools (not powered)" | 
                                 data$mineractivity == "no value found" | 
                                 data$mineractivity == "unknown" | 
                                 data$mineractivity == "clean up" | 
                                 data$mineractivity == "inspect equipment" & 
                                 data$accident.only == 0, 1, 0)
  
  data$likely.class = ifelse(data$accidentclassification == "handtools (nonpowered)" |
                               data$accidentclassification == "machinery" |
                               data$accidentclassification == "electrical" & 
                               data$accident.only == 0, 1, 0)
  
  data$likely.source = ifelse((data$sourceofinjury == "wrench" | 
                                 data$sourceofinjury == "knife" |
                                 data$sourceofinjury == "power saw" | 
                                 data$sourceofinjury == "hand tools,nonpowered,nec" |
                                 data$sourceofinjury == "crowbar,pry bar" | 
                                 data$sourceofinjury == "axe,hammer,sledge") & 
                                data$accident.only == 0, 1, 0)
  data$likely.source = ifelse(is.na(data$likely.source), 0, data$likely.source)
  
  # all "surgeries" are false keywords, but only "hoist/elevator" in combo with words that refer to elevator service are false keywords
  data$false.keyword = ifelse((data$repair == 1 & data$surgery == 1) |
                                (data$fix == 1 & data$surgery == 1) |
                                (data$rplace == 1 & data$surgery == 1) |
                                (data$repair == 1 & data$hoist == 1) |
                                (data$maintain == 1 & data$hoist == 1) |
                                (data$service == 1 & data$hoist == 1) |
                                (data$fix == 1 & data$hoist == 1), 1, 0)
  
  data$likely.keyword = ifelse((data$repair == 1 | data$fix == 1 | 
                                  data$maintain == 1 | data$rplace == 1 |
                                  data$install == 1 | data$service == 1 |
                                  data$cleaning == 1 | data$changing == 1 |
                                  data$retrack == 1 | data$inspect == 1 |
                                  data$shovel == 1 | data$reposition == 1 | 
                                  data$pullbelt == 1 | data$grease == 1 |
                                  data$washingdown == 1 | data$check == 1 |
                                  data$oil == 1 | data$mrworker == 1 |                                      
                                  data$cover == 1 | data$tests == 1 |
                                  data$toolbox == 1 ) & 
                                 data$accident.only == 0 &
                                 data$false.keyword == 0, 1, 0)
  
  data$maybe.keyword = ifelse((data$remove == 1 | data$dismantl == 1 | 
                                 data$rethread == 1 | data$welding == 1 | 
                                 data$bits == 1 | data$helping == 1 |
                                 data$conveyor == 1 | data$belt == 1 |
                                 data$tighten == 1 | data$battery == 1 ) & 
                                data$accident.only == 0 &
                                data$false.keyword == 0, 1, 0)
  
  ##############################################################################
  
  # GENERATE KEY WORDS TO IDENTIFY FALSE POSITIVES AFTER CLASSIFICATION
  
  data$flashburn = ifelse(grepl("weld.{1,40}flash( |-)*burn", data$narrative) | 
                            grepl("flash( |-)*burn.{1,40}weld", data$narrative), 1, 0)
  
  data$carpal.tunnel = ifelse((grepl("carp(a|u|e)l( |-)*tun(n)*(e|l)(e|l)", data$narrative) | 
                                 grepl("bursitis", data$narrative)) &
                                !grepl("fracture", data$narrative), 1, 0)
  
  data$cumulative = ifelse(grepl("rep(e|i)t(e|a|i)ti(v|n)e", data$narrative) | 
                             grepl("(cumulative|degenerativ)", data$narrative) |
                             grepl("repeated(ed)*.{1,10}(mo(tion|vement)|trauma|irritation)", data$narrative) |
                             grepl("long( |-)*term", data$narrative) | 
                             grepl("slow( |-)*on( |-)*set", data$narrative), 1, 0)
  
  data$hearingloss = ifelse(grepl("hearing.los", data$narrative) | 
                              grepl("los.{1,10}hearing", data$narrative) |
                              grepl("n(o|i)(o|i)se exposur", data$narrative) |
                              grepl("dimini.{1,10}hearing", data$narrative) |
                              grepl("thr(e|i)s(h)*( |-)*hold( |-)*shift", data$narrative) |
                              grepl("shift.{1,4}change.{1,30}hear", data$narrative) |
                              grepl("exposur(e)*.{1,20}noise", data$narrative), 1, 0)
  
  data$exposure = ifelse(grepl("((prolon|occupation|long( |-)*term).{1,8}exposur)|(exposur.{1,20}(noise|we(a)*ther))|p(n|h)e(n)*umo(n)*co(nio)*sis", data$narrative) | 
                           data$natureofinjury == "pneumoconiosis,black lung", 1, 0)
  
  data$heartattack = ifelse(grepl("heart( |-)*at(t)*ac(k|h)", data$narrative) | 
                              data$natureofinjury == "heart attack", 1, 0)
  
  data$unrelated = ifelse(grepl("not work relat", data$narrative) |
                            grepl("no.{1,20}(specific|single).{1,5}(accident|injury|indicent|exposure)", data$narrative) |  
                            (grepl("no (accident|incident|injury)", data$narrative) & 
                               !grepl("no (accident|incident|injury).{1,5}report", data$narrative)), 1, 0)
  
  data$working.on = ifelse(grepl("(to work|workin(g)*)( |-)*(on|in|under|out|at)", data$narrative), 1, 0)

  data$barring = ifelse(grepl("barr(ed|ing).{1,10}(rock|motor)", data$narrative), 1, 0)
  
  data$otherverb = ifelse(grepl("( |^)patch", data$narrative) | 
                            grepl("re(-)*(build|pack|fuel|assembl|lin)", data$narrative) | 
                            grepl("(re)*adjust", data$narrative) | 
                            grepl("(secure|unplug)", data$narrative) | 
                            grepl("trouble( |-)*shoot", data$narrative) | 
                            grepl("to drain", data$narrative) | 
                            grepl("mod(i|y)f(y|ication)", data$narrative) | 
                            grepl("(mount|splic|bolt|adjust|digg|drill|cutt|unload|dislodg|pump|lift|jack|lay|haul|spread|position|tap(p)*|air|oil|fuel|drain|hook)(ing|ign|ed)", data$narrative) |
                            grepl("(mov|hang|chan|putt|load|pry|assembl|push|pul(l)*(l)*|swing|trim(m)*|carry|strip(p)*|torqu(e)*|shovel(l)*|plac|pick|dispos)(ing|ign|ed)", data$narrative) |
                            grepl("(grind|tension|clip(p)*|notch|straighten|band|guid(e)*|throw|rotat|saw|apply|align|tear|(un)*screw|attach|latch|goug|clear|restor)(ing|ign|ed)", data$narrative) |
                            grepl("(set(t)*|put(t)*|pump|tak|prim)(ing).{1,10}(tire|wheel|pump|oil|links|fuel)", data$narrative) | 
                            grepl("tr(ied|ying|yign).{1,3}to.{1,3}(free|take( |-)*out|shut( |-)*down|position|start|lift|pry|press|rotate|roll|sep(e|a)rat|releas)", data$narrative) | 
                            grepl("attempt(ed|ing)*.{1,3}to.{1,3}(free|take( |-)*out|position|shut( |-)*down|pry|lift|put|get|unplug|unstop|lay|clear|start|press|rotate|roll|sep(e|a)rat)", data$narrative), 1, 0)
  
  data$othernoun = ifelse(grepl("anti( |-)*freeze", data$narrative) | 
                            grepl("sweeper", data$narrative), 1, 0)
  
  data$other.keyword = ifelse((data$otherverb == 1 | 
                                 data$othernoun == 1 |
                                 data$trash == 1 | 
                                 data$wrench == 1 |
                                 data$moretools == 1 | 
                                 data$loosen == 1 |
                                 data$tire == 1 |
                                 data$splice == 1 |
                                 data$working.on == 1 | 
                                 data$barring == 1) &
                                data$accident.only == 0, 1, 0)
  
  post.classification = c("flashburn", "carpal.tunnel", "cumulative", 
                          "hearingloss", "exposure", "heartattack", 
                          "unrelated", "working.on", "barring",
                          "otherverb", "othernoun", "other.keyword")
  
  ##############################################################################
  
  # DROP UNNECESSARY VARIABLES & FINAL FORMAT
  
  # drop unnecessry variables
  keep = c("accidentdate", "accident.only", "battery", 
           "belt", "bits", "changing", 
           "check", "cleaning", "conveyor", 
           "cover", "dismantl", "documentno", 
           "falling.accident", "false.keyword", "fix", 
           "grease", "helping", "hoist", 
           "inspect", "install", "likely.activity", 
           "likely.class", "likely.keyword", "likely.occup", 
           "likely.source", "loosen", "lug", 
           "maintain", "maybe.activity", "maybe.keyword", 
           "maybe.occup", "mineid", "moretools", 
           "MR", "mrworker", "oil", 
           "pain", "power", "pullbelt", 
           "remove", "repair", "reposition", 
           "rethread", "retrack", "mineid",
           "roller", "rplace", 
           "service", "shovel", "splice", 
           "surgery", "tests", "tighten", 
           "tire", "toolbox", "trash", 
           "type", "washingdown", "welding", 
           "wrench")
  
  if (purpose == "train.test") {
      # 1018 rows; 58 columns; unique on documentno
    data = data[, (names(data) %in% keep)]
    data$accidentdate = NULL #data$mineid = NULL
  }
  
  if (purpose == "classify") {
      # 75700 rows; 73 columns; unique on documentno
    keep = c(keep, post.classification)
    data = data[, (names(data) %in% keep)]
  }
  
  # enforce factor storage
  #for (var in names(data)) {
  #  data[, var] = factor(data[, var])
  #}

  # randomly sort data (in case it was ordered)
  set.seed(626)
  rand = runif(nrow(data))
  data = data[order(rand),]
  
  # bye
  rm(keep, var, rand, post.classification)
  
  ##############################################################################
  
  # OUTPUT DATA
  
  if (purpose == "train.test") {
    # output prepared MR training/testing set
      # 1018 rows; 58 columns; unique on documentno 
    saveRDS(data, file = prepared.train.test.out.file.name)
    rm(prepared.train.test.out.file.name)
  }
  
  if (purpose == "classify") {
    # output prepared merged MR accidents data
      # 75700 rows; 73 columns; unique on documentno 
    saveRDS(data, file = prepared.classify.out.file.name)
    rm(prepared.classify.out.file.name)
  }
    
# }

################################################################################

# bye
# rm(list = ls())

################################################################################
