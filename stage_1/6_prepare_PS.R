# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Prepare PS (Pinning and Striking) Accidents Data
  # Prepares PS training/testing set (produced in 3_clean_PS_training_set) 
    # or merged PS accidents data (produced in 4_merge_accidents)
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
  # cleaned PS training/testing set
    # produced in 3_clean_PS_train_test_set
train.test.set.in.file.name = paste0(cleaned.path, "/clean_PS_train_test_set.rds", collapse = NULL)
  # merged PS accidents data
    # produced in 4_merge_accidents
merged.data.in.file.name =  paste0(merged.path, "/merged_PS_accidents.rds", collapse = NULL)

# outputs
  # prepared PS training/testing set
prepared.train.test.out.file.name = paste0(prepared.path, "/prepared_PS_train_test.rds", collapse = NULL)
  # prepared merged PS accidents data
prepared.classify.out.file.name = paste0(prepared.path, "/prepared_PS_classify.rds", collapse = NULL)

# generate file paths
dir.create(prepared.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root, cleaned.path, merged.path, prepared.path)

################################################################################

# DEFINE LOOP THAT WILL ITERATE THROUGH PURPOSES

for (purpose in c("train.test", "classify")) { # prepare datasets for both training/testing and classification purposes
  
  # READ DATA
  
  if (purpose == "train.test") {
    # read cleaned PS training/testing set
      # 1000 rows; 18 columns; unique on documentno 
    data = readRDS(train.test.set.in.file.name)
    rm(train.test.set.in.file.name)
  }
  
  if (purpose == "classify") {
    # read merged PS accidents data 
      # 75743 rows; 19 columns; unique on documentno 
    data = readRDS(merged.data.in.file.name)
    rm(merged.data.in.file.name)
  }
  
  ##############################################################################
  
  # CLEAN DATA
  
  # drop non-injury accidents 
  data$accident.only = ifelse((data$degreeofinjury == "accident only" | 
                                 data$accidenttypecode == 44) & 
                                !is.na(data$accidenttypecode), 1, 0)
  
  # clean number typos
  data$numbertypo = ifelse(grepl("[a-z][0-9][a-z]", data$narrative), 1, 0)
  for (i in 0:9) {
    data[data$numbertypo == 1, "narrative"] = gsub(i, "", data[data$numbertypo == 1, "narrative"])
  }
  data$numbertypo = NULL
  
  # clean common typos
  data$narrative = gsub("ag(a)*( )*(in)*st", "against", data$narrative)
  
  # clean "not-found" values
  data$accidenttype = ifelse(data$accidenttype == "not elsewhereclassified", "NO VALUE FOUND", data$accidenttype)
  data$natureofinjury = ifelse(data$natureofinjury == "unclassified,not determed", "NO VALUE FOUND", data$natureofinjury)
  
  # format accident type codes
  data$accidenttypecode = as.factor(data$accidenttypecode)

  # bye
  rm(i)
  
  ##############################################################################
  
  # GENERATE POSITIVELY PREDICTIVE KEY WORDS
  
  data$pin = ifelse(grepl("(^| )pin(n*)(e|i)[a-z]+", data$narrative) &
                      !grepl("(^| )pinion", data$narrative) &
                      !grepl("(^| )pinner", data$narrative) &
                      !grepl("pinn(ing|ed)( ).{1,5}top", data$narrative) &
                      !grepl("pinn(ing|ed).{1,5}(him|his|her)self", data$narrative), 1, 0)
  
  data$strike = ifelse(grepl("str(i|u)(.*)k[a-z]*", data$narrative) &
                         !grepl("str(i|u)(.*)k[a-z]*.{1,6}head", data$narrative) &
                         !grepl("head.{1,6}str(i|u)(.*)k[a-z]*", data$narrative), 1, 0)
  
  data$trap = ifelse(grepl("( )trap[a-z]*", data$narrative), 1, 0)
  
  data$collided = ifelse(grepl("col(l)*i(de|ded|sion|ssion)", data$narrative), 1, 0)
  
  data$ranover = ifelse(grepl("( |^)r(a|u)n( )*(over|into)", data$narrative), 1, 0)
  
  data$rolled = ifelse(grepl("rolled( )*(over|into|onto|on|down)", data$narrative), 1, 0)
  
  data$between = ifelse(grepl("between", data$narrative) | 
                          grepl("btwn", data$narrative), 1, 0)
  
  data$wheel = ifelse(grepl("wheel", data$narrative) & 
                        !grepl("wheeler", data$narrative), 1, 0)
  
  data$by = ifelse(grepl("by", data$narrative), 1, 0)
  
  ##############################################################################
  
  # GENERATE NEGATIVELY PREDICTIVE KEY WORDS
  
  data$jarring = ifelse(grepl("jar(r)*(ed|ing)", data$narrative) |
                          grepl("jo(lt|stl)(ed|ing)", data$narrative), 1, 0)
  
  data$bounced = ifelse(grepl("boun(c)*( )*(e|ing)", data$narrative), 1, 0)
  
  data$rock = ifelse(grepl("rock( |$|\\.|s|,)", data$narrative) & 
                       !grepl("rock( )*dust", data$narrative), 1, 0)
  
  data$digit = ifelse(grepl("(finger(s)*|pinky|hand(s)*|thumb|hand( |\\.|,|$))", data$narrative), 1, 0)
  
  data$derail = ifelse((grepl("(left|off|jumped).{1,15}track", data$narrative) & 
                          !grepl("(left|off|jumped).{1,15}track.{1,3}switch", data$narrative)) | 
                         grepl("derai", data$narrative), 1, 0)
  
  data$steering = ifelse(grepl("ste(e|a)ring( )*wheel.{1,15}sp(u|i)n", data$narrative), 1, 0)
  
  data$wrench = ifelse(grepl("wrench", data$narrative), 1, 0)
  
  data$controls = ifelse(grepl("(lever|stick)", data$narrative), 1, 0)
  
  data$resin = ifelse(grepl("resin", data$narrative), 1, 0)
  
  data$atrs = ifelse(grepl("a(\\.)*t(\\.)*r(\\.)*s(\\.)*", data$narrative), 1, 0)
  
  data$flew = ifelse(grepl("fl(ew|y|ing)", data$narrative), 1, 0)
  
  data$loose = ifelse(grepl("loose", data$narrative), 1, 0)
  
  data$broke = ifelse(grepl("br(oke|eak)", data$narrative), 1, 0)
  
  data$bent = ifelse(grepl("bent", data$narrative) & 
                       !grepl("bent( )*over", data$narrative), 1, 0)
  
  data$canopy = ifelse(grepl("canopy", data$narrative), 1, 0)
  
  ##############################################################################
  
  # GENERATE KEY WORDS TO IDENTIFY FALSE POSITIVES
  
  # being jostled against the seat
  data$bodyseat = ifelse(grepl("(back|head|neck|shoulder|elbo).{1,10}seat", data$narrative) &
                           !grepl("backward.{1,10}seat", data$narrative) &
                           !grepl("(bolt|over|drill)( )*head.{1,10}seat", data$narrative), 1, 0) 
  
  # hitting head against canopy
  data$headcanopy = ifelse((grepl("(head|neck).{1,5}(on|str(ike|uck)|hit|against).{1,5}(canopy)", data$narrative) |
                              grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(canopy)", data$narrative)) &
                             !grepl("drill( )*head.{1,10}canopy", data$narrative) &
                             !grepl("over( )*head.{1,10}canopy", data$narrative) &
                             !grepl("head(ing|er|ed).{1,10}canopy", data$narrative), 1, 0) 
  
  # going over a bump and operator hitting head 
  data$hole = ifelse(grepl("(hit|str(ike|uck)|r(a|u)n( )*over|(went|go)( )*over).{1,10}(rock|hole|(h|b)ump(s| |$|\\.|,)|dip|depression|uneven|off( |-)*set|((low|bad|rough)( |-)*(spot|patch|place)))", data$narrative), 1, 0)
  
  data$unevenbottom = ifelse(grepl("(hole|bump(s| |$|\\.|,)|dip|depression|uneven|off( |-)*set|((low|bad|rough)( |-)*(spot|place|patch)))", data$narrative) & 
                               !grepl("(bolt|drill|steel|cable|test|pin).{1,15}hole", data$narrative), 1, 0)
  
  # roof bolting/drilling steel injuries
  data$drillsteel = ifelse(grepl("drill.{1,5}steel", data$narrative) & 
                             grepl("(between|btwn).{1,17}steel.{1,25}(drill|head|roof|guide|canopy|ring)", data$narrative), 1, 0)
  
  # drill steel breaking/bending during roofbolting
  data$brokensteel = ifelse(grepl("drill.{1,5}steel", data$narrative) & 
                              (grepl("(drill|roof).{1,5}(steel|bolt).{1,15}(burst|ben(t|d)|br(eak|oke)|loose|drop(ped|ping)*|c(a|o)*me( )*( )*out|f(a|e)ll|stuck|clog)", data$narrative) |
                                 grepl("wrench.{1,5}(slip|c(a|o)me).{1,5}off.{1,15}(bolt|drill head)", data$narrative) |  
                                 grepl("wrench.{1,15}broke", data$narrative)), 1, 0)
  
  data$roofbolt = ifelse(grepl("(roof|( |^)rib).{1,10}bolt", data$narrative) | 
                           grepl("(roof|rib).{1,25}bolting", data$narrative) |
                           grepl("roof.{1,35}bolting", data$narrative) |
                           grepl("bolt.{1,10}instal", data$narrative) |
                           grepl("instal.{1,20}bolt", data$narrative), 1, 0)
  
  data$bolting = ifelse(grepl("bolting", data$narrative) |
                          grepl("put(t)*(ing)*( )*bolt.{1,10}top", data$narrative), 1, 0)
  
  # gloves getting caught during roof bolting
  data$entrapment = ifelse((grepl("drill.{1,5}steel", data$narrative) | 
                              data$roofbolt == 1 |
                              data$bolting == 1) & 
                             (grepl("(caught|catching|snagg(ed|ing)|grab).{1,10}(glove|shirt|sleeve)", data$narrative) | 
                                grepl("(glove|shi(r)*t|sle(e)*ve).{1,10}(entangl|cau( )*ght|catching|snagg(ed|ing))", data$narrative)), 1, 0)
  
  ##############################################################################
  
  # REPLACE ALL MENTIONS OF VEHICLES WITH "VEHICLE"
  
  # save original narrative variable
  data$old_narrative = data$narrative
  
  # replace mentions of any vehicle with "VEHICLE"
  data$narrative = gsub("(man|ram|s(ch|h)uttle|scoop)( |-|- |v)*(trip|car)( car)*", "VEHICLE1", data$narrative)
  data$narrative = gsub("( |^)car( |-|s|\\.|,|$)", " VEHICLE2 ", data$narrative)
  data$narrative = gsub("(m|a)(m|a)n( |-|- )*bus", "VEHICLE3", data$narrative)
  data$narrative = gsub("vehic(l|e)(l|e)", "VEHICLE4", data$narrative)
  data$narrative = gsub("person(n)*(e|a)l carrier", "VEHICLE5", data$narrative)
  data$narrative = gsub("wheeler", "VEHICLE6", data$narrative)
  data$narrative = gsub("scooter", "VEHICLE7", data$narrative)
  data$narrative = gsub("s(ch|h)uttle", "VEHICLE8", data$narrative)
  data$narrative = gsub("cricket ", "VEHICLE9", data$narrative)
  data$narrative = gsub("(rock|roof)( |-)*bolter", "VEHICLE10", data$narrative)
  data$narrative = gsub("( |^)truck", " VEHICLE11", data$narrative)
  data$narrative = gsub("buggy", "VEHICLE12", data$narrative)
  data$narrative = gsub("stam(m)*ler", "VEHICLE13", data$narrative)
  data$narrative = gsub("mac( |-)*(8|eight)", "VEHICLE14", data$narrative)
  data$narrative = gsub("(3|three)( |-)*wh(ee)*l(e)*r", "VEHICLE15", data$narrative)
  data$narrative = gsub("(c)*ont.{1,10}mi(e)*n(r|er|ing)", "VEHICLE16", data$narrative)
  data$narrative = gsub("long( |-)*wall", "VEHICLE17", data$narrative)
  data$narrative = gsub("load( |-)*haul(-| )*dump", "VEHICLE18", data$narrative)
  data$narrative = gsub("(mining|miner|minr|loading|(roof)*( )*bolt(ing)*)( )*machine", "VEHICLE19", data$narrative)
  data$narrative = gsub("tunnel( |-)*borer", "VEHICLE20", data$narrative)
  data$narrative = gsub("fork( |-)*lift", "VEHICLE21", data$narrative)
  data$narrative = gsub("(front( |-)*end|scraper)( )*loader", "VEHICLE22", data$narrative)
  data$narrative = gsub("locomotiv(e)*", "VEHICLE23", data$narrative)
  data$narrative = gsub("(road|motor)( |-)*grader", "VEHICLE24", data$narrative)
  data$narrative = gsub("motor", "VEHICLE25", data$narrative)
  data$narrative = gsub("tractor", "VEHICLE26", data$narrative)
  data$narrative = gsub("jeep", "VEHICLE27", data$narrative)
  data$narrative = gsub("(ore)*( |-)haul(er|age)", "VEHICLE28", data$narrative)
  data$narrative = gsub("rail( |-)*runner", "VEHICLE29", data$narrative)
  data$narrative = gsub("feeder", "VEHICLE30", data$narrative)
  data$narrative = gsub("s/c", "VEHICLE31", data$narrative)
  data$narrative = gsub("shearer", "VEHICLE32", data$narrative)
  data$narrative = gsub("mucker", "VEHICLE33", data$narrative)
  data$narrative = gsub("eimco", "VEHICLE34", data$narrative)
  data$narrative = gsub("jitney", "VEHICLE35", data$narrative)
  data$narrative = gsub("bolter", "VEHICLE36", data$narrative)
  data$narrative = gsub("rail( |-)*runner", "VEHICLE37", data$narrative)
  data$narrative = gsub("mobile", "VEHICLE38", data$narrative)
  data$narrative = gsub("porta(l)*( |-)*bus", "VEHICLE39", data$narrative)
  data$narrative = gsub("( |^|-)bus(es| |\\.|,|$)", " VEHICLE40 ", data$narrative)
  
  data[!grepl("troll(e)*y( )*pol(e|l)", data$narrative), "narrative"] = 
    gsub("trol(l)*(e)*y", " VEHICLE41 ", data[!grepl("troll(e)*y( )*pol(e|l)", data$narrative), "narrative"])
  
  data[!grepl("to trip", data$narrative), "narrative"] =
    gsub("( |^)trip( |$|,|\\.)", " VEHICLE42 ", data[!grepl("to trip", data$narrative),]$narrative)
  
  data[!grepl("scoop(er|ing)", data$narrative), "narrative"] = 
    gsub("scoop", " VEHICLE43 ", data[!grepl("scoop(er|ing)", data$narrative), "narrative"])
  
  data[!grepl("to tram", data$narrative) & 
         !grepl("tram.{1,5}(lever|sprocket|pedal|chain)", data$narrative), "narrative"] = 
    gsub("tram( |$|\\.|,)", " VEHICLE44 ", data[!grepl("to tram", data$narrative) & 
                                                  !grepl("tram.{1,5}(lever|sprocket|pedal|chain)", data$narrative), "narrative"])
  
  data$narrative = gsub("mucker", "VEHICLE45", data$narrative)
  
  # create special vehicle flag
  data$shuttlecar.or.rbolter = ifelse(grepl("VEHICLE(8|10|36)", data$narrative) | 
                                        grepl("(s(ch|h)uttle).{1,30}( |-|- |v)*(trip|car)( car)*", data$old_narrative), 1, 0)
  
  ##############################################################################
  
  # REPLACE ALL MENTIONS OF BODY PARTS WITH "BODY"
  
  data$narrative = gsub("hand(s| |\\.|,|$)", "BODY ", data$narrative)
  data$narrative = gsub("finger(s)*", "BODY", data$narrative)
  data$narrative = gsub("thumb", "BODY", data$narrative)
  data$narrative = gsub("ankle(s)*", "BODY", data$narrative)
  data$narrative = gsub("shoulder(s)*", "BODY", data$narrative)
  data$narrative = gsub("knee( |s|\\.|,|$)", "BODY ", data$narrative) # avoid "kneel"
  data$narrative = gsub("wrist(s)*", "BODY", data$narrative)
  data$narrative = gsub("cal(f|ve|ves)", "BODY", data$narrative)
  data$narrative = gsub("( |^)leg(s)*", "BODY", data$narrative)
  data$narrative = gsub("eye(lid|brow|s)*", "BODY", data$narrative)
  data$narrative = gsub("cheek(s|bones)*", "BODY", data$narrative)
  data$narrative = gsub("bone(s)*", "BODY", data$narrative)
  data$narrative = gsub("( |^)lip(s)*", " BODY", data$narrative)
  data$narrative = gsub("( |^)ear(s)*", " BODY", data$narrative)
  data$narrative = gsub("chin( |$|\\.|,)", "BODY", data$narrative)
  data$narrative = gsub("neck", "BODY", data$narrative)
  data$narrative = gsub("(^| )(fore|for)*arm", " BODY", data$narrative)
  data$narrative = gsub("mouth", "BODY", data$narrative)
  data$narrative = gsub("nose( |s|\\.|,|$)", "BODY ", data$narrative)
  data$narrative = gsub("pelvis", "BODY", data$narrative)
  data$narrative = gsub("chest", "BODY", data$narrative)
  data$narrative = gsub("groin", "BODY", data$narrative)
  data$narrative = gsub("(t|f)ibia", "BODY", data$narrative)
  data$narrative = gsub("ulna", "BODY", data$narrative)
  data$narrative = gsub("radia", "BODY", data$narrative)
  data$narrative = gsub("rib( |-)*cage", "BODY", data$narrative)
  data$narrative = gsub("buttock(s)*", "BODY", data$narrative)
  data$narrative = gsub("spine", "BODY", data$narrative)
  data$narrative = gsub("elbow(s)*", "BODY", data$narrative)
  data$narrative = gsub("testicle(s)*", "BODY", data$narrative)
  data$narrative = gsub("t(ee|oo)th", "BODY", data$narrative)
  data$narrative = gsub("(top|bottom) of|(r(igh)*t|l(e)*ft|his|her|both|onto|r\\.)( )*(foot|feet)", "BODY", data$narrative)
  data$narrative = gsub("( |^)hip(s)*", " BODY", data$narrative)
  
  data[!grepl("backward", data$narrative), "narrative"] = 
    gsub("(lowe(r)*|upper|PERSON|the|strain).{1,8}back", " BODY", data[!grepl("backward", data$narrative), "narrative"])
  
  data[!grepl("drill.{1,5}head", data$narrative) & 
         !grepl("(over|cutter)( )*head", data$narrative), "narrative"] = 
    gsub("(^| )head( |$|\\.|,)", " BODY ", data[!grepl("drill.{1,5}head", data$narrative) & 
                                                  !grepl("(over|cutter)( )*head", data$narrative), "narrative"])
  
  data[!grepl("(coal|the).{1,5}face", data$narrative) & 
         !grepl("surface", data$narrative), "narrative"] = 
    gsub("face", "BODY", data[!grepl("(coal|the).{1,5}face", data$narrative) & 
                                !grepl("surface", data$narrative), "narrative"])
  
  ##############################################################################
  
  # GENERATE KEY WORDS USING BODY PARTS
  
  data$bumped = ifelse((grepl("bump(ed|ing)( )*(over|into|onto)", data$narrative) | 
                          grepl("bump(ed|ing).{1,10}BODY", data$narrative)) &
                         !grepl("bump(ed|ing).{1,10}head", data$old_narrative), 1, 0)
  
  data$caught = ifelse(grepl("caught.{1,15}(between| in )", data$old_narrative) |
                         grepl("caught.{1,10}BODY", data$narrative) |
                         grepl("BODY.{1,6}caught", data$narrative), 1, 0)
  
  data$hit = ifelse(grepl("( |^)hit.{1,5}(by|him|his|her|employee|ee)", data$old_narrative) |
                         grepl("( |^)hit.{1,10}BODY", data$narrative), 1, 0)
  
  data$dropped = ifelse(grepl("(lowe(r)*(ing|ed)*|drop(p)*(ing|ed)*).{1,15}(bucket|drill( |-)*head|drill( |-)*pod|pinner( |-)*head).{1,15}BODY", data$narrative), 1, 0)
  
  data$neg.wrench = ifelse(data$wrench == 1 & 
                             (grepl("(burst|ben(t|d)|br(eak|oke)|loose|dislodge|shifted|drop(ped|ping)*|c(a|o)*me( )*( )*(out|off)|f(a|e)ll|stuck|clog|slipped)+", data$old_narrative) | 
                                data$flew == 1 | 
                                data$caught == 1), 1, 0)
  
  ##############################################################################
  
  # REPLACE ALL MENTIONS OF PINNING AND STRIKING WITH "PINNED/STRUCK"
  
  data$narrative = gsub("( |^)pin(n)*(ed|ing)", " PINNED/STRUCK", data$narrative)
  data$narrative = gsub("(s)*tr(u|i)(c)*k(e|ing)*", "PINNED/STRUCK", data$narrative)
  data$narrative = gsub("r(a|u)n( )*(into|over)", "PINNED/STRUCK", data$narrative)
  data$narrative = gsub("col(l)*ided( w| with)*", "PINNED/STRUCK", data$narrative)
  data$narrative = gsub("( |^)trap(p)*(ed|ing)", " PINNED/STRUCK", data$narrative)
  data$narrative = gsub("rolled (into|onto|over)", "PINNED/STRUCK", data$narrative)
  data$narrative = gsub("c(a|u)(a|u)ght", "PINNED/STRUCK", data$narrative)
  data$narrative = gsub("catching|to catch", "PINNED/STRUCK", data$narrative)
  
  data[data$hole == 0, "narrative"] = gsub("( |^)hit(t)*(ing)*( |$|\\.|,|s)", "PINNED/STRUCK", 
                                           data[data$hole == 0, "narrative"])
  
  data[grepl("VEHICLE.{1,5}got on{1,5}BODY", data$narrative), "narrative"] = 
    gsub("got on", "PINNED/STRUCK", data[grepl("VEHICLE.{1,5}got on{1,5}BODY", data$narrative), "narrative"])
  
  ##############################################################################
  
  # REPLACE ALL MENTIONS OF PEOPLE WITH "PERSON"
  
  data$narrative = gsub("( |^)e(e|mp|mpl|mploye)(e)*( |$|,|\\.)", " PERSON ", data$narrative)
  data$narrative = gsub("( |^)i(,|\\.| )*name", " PERSON", data$narrative)
  data$narrative = gsub("injured person", "PERSON", data$narrative)
  data$narrative = gsub("(him|her)self", "PERSON", data$narrative)
  data$narrative = gsub("vi(c)*tim", "PERSON", data$narrative)
  data$narrative = gsub("repairm(a|e)n", "PERSON", data$narrative)
  data$narrative = gsub("maintenance( )*m(a|e)n", "PERSON", data$narrative)
  data$narrative = gsub("( |^)m(a|e)n( |$|,|\\.)", " PERSON ", data$narrative)
  data$narrative = gsub("fore(m|a)(a|e|m)n", "PERSON", data$narrative)
  data$narrative = gsub("ind(i)*v(idual)*(s)*", "PERSON", data$narrative)
  data$narrative = gsub("helper(s)*", "PERSON", data$narrative)
  data$narrative = gsub("person(s)*", "PERSON", data$narrative)
  data$narrative = gsub("worker(s)*", "PERSON", data$narrative)
  data$narrative = gsub("^inj(\\.)*(ured)*", "PERSON", data$narrative)
  data$narrative = gsub("the.{1,6}injured", "PERSON", data$narrative)
  data$narrative = gsub("injured was", "PERSON", data$narrative)
  data$narrative = gsub("( |^)(s)*he(r|rs)*( |$|,|\\.)", " PERSON ", data$narrative)
  data$narrative = gsub("( |^)hi(s|m)( |$|,|\\.)", " PERSON ", data$narrative)
  data$narrative = gsub("(wo)*m(a|e)n( |$|,|\\.)", "PERSON ", data$narrative)
  data$narrative = gsub("operat(o|e)r", "PERSON", data$narrative)
  data$narrative = gsub("passenger", "PERSON", data$narrative)
  data$narrative = gsub("driver", "PERSON", data$narrative)
  
  ##############################################################################
  
  # COUNT CERTAIN WORDS IN EACH NARRATIVE
  
  # count uppercase words
  data$num.vehicles = str_count(data$narrative, "VEHICLE")
  data$num.pinstrike = str_count(data$narrative, "PINNED/STRUCK")
  data$num.person = str_count(data$narrative, "PERSON")
  data$num.body = str_count(data$narrative, "BODY")
  
  # count unique vehicles
  uniq_vehcls = function(x) {
    return(length(unique(substr(unlist(regmatches(x, gregexpr("VEHICLE[0-9][0-9]*", x))), 8, 9))))
  }
  data$num.unique.vehcl = sapply(data$narrative, uniq_vehcls)
  data$mult.vehcl = ifelse(data$num.unique.vehcl > 1, 1, 0)
  
  # bye
  rm(uniq_vehcls)
  
  ##############################################################################
  
  # GENERATE KEY WORDS USING UPPERCASE WORDS
  
  data$dif.vehicle = ifelse(grepl("(second|another|different).{1,5}VEHICLE", data$narrative), 1, 0)
  
  data$loose.rbolting = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", data$narrative), 1, 0)
  
  data$drill.action = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", data$narrative), 1, 0)
  
  data$operating = ifelse((grepl("( |^|was|while|had)(tr(a)*m(m)*[^ ]{0,3}|op(e)*r(a)*t[^ ]{0,3}|backin.{1,10}VEHICLE|r(a|u)n|makin(g)*( )*(a)*( )*tu(r)*n|remote.{1,5}control|driv)", data$narrative) &
                             (!grepl("PERSON.{1,20}(splic(e|ing)|crawl(ing)*|repair|fix)", data$narrative) & 
                                grepl("(splic(e|ing)|crawl(ing)*|repair|fix).{1,20}PERSON", data$narrative)) &
                             (grepl("operat", data$mineractivity) | 
                                (grepl("roof bolt", data$mineractivity) & 
                                   !grepl("help(ing|er|)", data$old_narrative))) &
                             (!grepl("(side of|right|left|beside).{1,10}VEHICLE", data$narrative) | 
                                grepl("remote.{1,5}control", data$narrative))), 1, 0)
  
  ##############################################################################
  
  # GENERATE PREDICTIVE CIRCUMSTANCE FLAGS
  
  # driver hitting head against vehicle roof
  data$headroof = ifelse((grepl("(head|neck).{1,5}(on|str(ike|uck)|hit|against).{1,5}(roof|top)", data$old_narrative) |
                            grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(roof|top)", data$old_narrative) | 
                            (grepl("whip( )*lash", data$old_narrative) & 
                               data$operating == 1) | 
                            grepl("jerked.{1,10}(head|neck)", data$old_narrative)) &
                           !grepl("drill( )*head.{1,10}roof", data$old_narrative) &
                           !grepl("over( )*head.{1,10}roof", data$old_narrative) &
                           !grepl("head(ing|er|ed).{1,10}roof", data$old_narrative) &
                           !grepl("head.{1,10}roof.{1,5}bolt", data$old_narrative), 1, 0) 
  
  # roof bolting
  data$pos.roofbolt = ifelse(data$roofbolt == 1 & 
                               grepl("PINNED/STRUCK.{1,15}between.{1,15}(roof( bolt)*|canopy|boom|(bolter|drill)( |-)*(pot|guide|head)|(drill|roof)*( |-)*(steel|guide)|( |^)rib|(bolt(er)*|torque)( |-)*wrench|lead|top).{1,30}(top|roof( bolt)*|canopy|boom|(bolter|drill)( |-)*(pot|guide|(h|l)ead)|(drill|roof)*( |-)*(steel|guide)|( |^)rib|(bolt(er)*|torque)( |-)*wrench|lead)", data$narrative), 1, 0)
  
  data$neg.roofbolt = ifelse(data$roofbolt == 1 & 
                               (data$entrapment == 1 | 
                                  data$brokensteel == 1), 1, 0)
  
  # person inside (or hanging outside) of the vehicle
  data$in.vehicle = ifelse(grepl("riding.{1,10}(passenger|driver|operat(o|e)r)", data$old_narrative) | 
                             grepl("PERSON.{1,8}riding", data$narrative) |
                             grepl("riding.{1,5}outside", data$narrative) |
                             !grepl("riding.{1,15}VEHICLE", data$narrative), 1, 0)
  
  # operator arm or hand trailing outside vehicle
  data$outsidevehicle = ifelse(((grepl("BODY.{1,15}(resting| hanging).{1,5}(over|out|on)", data$narrative) & 
                                   grepl("VEHICLE", data$narrative)) |
                                  grepl("BODY.{1,15}out( )*side.{1,30}VEHICLE", data$narrative)) &
                                 !grepl("overhang", data$narrative), 1, 0)
  
  # miner struck by a cable (non-PS); miner struck by the boom while replacing the cable (PS)
  data$cable = ifelse((grepl("cable.{1,30}PINNED/STRUCK", data$narrative) | 
                         grepl("PINNED/STRUCK.{1,30}cable", data$narrative)) & 
                        (!grepl("boom", data$narrative) &
                           !grepl("cable.{1,30}PINNED/STRUCK.{1,15}(against|between)", data$narrative) &
                           !grepl("cable( )*bolt", data$narrative)), 1, 0)
  
  data$strap = ifelse(grepl("strap.{1,20}PINNED/STRUCK", data$narrative) | 
                        grepl("PINNED/STRUCK.{1,20}strap", data$narrative), 1, 0)
  
  data$trolleypole = ifelse(grepl("PINNED/STRUCK.{1,20}troll(e)*y( )*pol(e|l)", data$narrative) | 
                              grepl("troll(e)*y( )*pol(e|l).{1,20}PINNED/STRUCK", data$narrative) |
                              (grepl(" pol(e|l).{1,20}PINNED/STRUCK", data$narrative) & 
                                 grepl("troll(e)*y( )*pol(e|l)", data$narrative)) |                                    
                              (grepl("PINNED/STRUCK.{1,20} pol(e|l)", data$narrative) & 
                                 grepl("troll(e)*y( )*pol(e|l)", data$narrative)), 1, 0)
  
  data$tool.break = ifelse(grepl("(wrench|roof bolt).{1,15}(br(eak|oke)|ben(d|t))", data$old_narrative) & 
                             !grepl("(wrench|roof bolt).{1,15}(br(eak|oke)|ben(d|t)).{1,20}PINNED/STRUCK.{1,15}between", data$narrative), 1, 0)
  
  data$vcomp.test = ifelse(grepl("(seat|rail|canopy|battery|drill|steel|chain|cable)+.{1,20}VEHICLE", data$narrative) | 
                             grepl("VEHICLE.{1,20}(seat|rail|canopy|battery|drill|steel|chain|cable)+", data$narrative), 1, 0)
  
  data$psobject.test = ifelse(grepl("(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+.{1,20}PINNED/STRUCK", data$narrative) | 
                                grepl("PINNED/STRUCK.{1,20}(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+", data$narrative), 1, 0)
  
  data$strikerib = ifelse((grepl("PINNED/STRUCK.{0,20}( )rib", data$narrative) | 
                             grepl("( )rib.{1,20}PINNED/STRUCK", data$narrative)) &
                            (!grepl("(lower|upper|side|cage|right|left| in |fractured|bruised).{0,10}( )rib", data$old_narrative) & 
                               !grepl("PERSON.{0,10}( )rib", data$old_narrative) & 
                               !grepl(" ribs", data$old_narrative)), 1, 0)
  
  # falling rock 
  data$falling.class = ifelse(data$accidentclassification == "fall of roof or back", 1, 0)
  
  data$falling.word = ifelse(grepl("rock( )*fell", data$narrative) |
                               grepl("fell.{1,20}roof", data$narrative) |
                               grepl("roof( )*f(a|e)ll", data$narrative) |
                               grepl("(rolled|fell) (from|.ff|out).{0,}( )rib", data$narrative) |
                               grepl("( )rib.{0,15}(rolled|fell) (from|.ff|out)", data$narrative), 1, 0)
  
  data$falling.accident = ifelse(data$falling.class == 1 | data$falling.word == 1, 1, 0)
  
  data$falling.class = 
    data$falling.word = NULL
  
  ##############################################################################
  
  # GROUP KEY WORDS
  
  data$keyword = ifelse((data$pin == 1 | 
                           data$strike == 1 | 
                           data$pos.roofbolt == 1 | 
                           data$trap == 1 | 
                           data$collided == 1 | 
                           data$hit == 1 | 
                           data$dropped == 1 | 
                           data$ranover == 1 | 
                           data$bumped == 1 | 
                           data$caught == 1 |
                           data$rolled == 1 | 
                           data$between == 1 | 
                           data$wheel == 1) &
                          (data$falling.accident == 0), 1, 0)
  
  data$false.keyword = ifelse(data$jarring == 1 | 
                                data$outsidevehicle == 1 | 
                                data$steering == 1 | 
                                data$neg.roofbolt == 1 |
                                data$bounced == 1 | 
                                data$rock == 1 | 
                                data$derail == 1 | 
                                data$cable == 1 | 
                                data$tool.break == 1 | 
                                data$bodyseat == 1 | 
                                data$headroof == 1 | 
                                data$strap == 1 | 
                                data$trolleypole == 1 | 
                                data$entrapment == 1 |
                                data$hole == 1, 1, 0)
  
  data$maybe.false.keyword = ifelse(data$digit == 1 | 
                                      data$operating == 1 |
                                      data$bent == 1 | 
                                      data$strikerib == 1 |
                                      data$wrench == 1 |
                                      data$controls == 1 | 
                                      data$resin == 1 |
                                      data$loose == 1 | 
                                      data$broke == 1 | 
                                      data$canopy == 1 | 
                                      data$flew == 1, 1, 0)
  
  ##############################################################################
  
  # CREATE LIKELY/MAYBE LIKELY/UNLIKELY GROUPS FROM CATEGORICAL VARIABLES

  data$likely.class = ifelse(data$accidentclassification == "powered haulage" | 
                               data$accidentclassification == "machinery", 1, 0)
  
  data$unlikely.class = ifelse(data$accidentclassification == "disorders (repeated trauma)" | 
                                 data$accidentclassification == "electrical" |
                                 data$accidentclassification == "explosives and breaking agents" | 
                                 data$accidentclassification == "stepping or kneeling on object" | 
                                 data$accidentclassification == "ignition or explosion of gas or dust", 1, 0)
  
  data$uncertain.class = ifelse(data$accidentclassification == "fall of roof or back" |
                                  data$accidentclassification == "handling of materials" |
                                  data$accidentclassification == "slip or fall of person" |
                                  data$accidentclassification == "fall of face/rib/pillar/side/highwall" |
                                  data$accidentclassification == "handtools (nonpowered)" |
                                  data$accidentclassification == "no value found" |
                                  data$accidentclassification == "other" |
                                  data$accidentclassification == "striking or bumping", 1, 0)
  
  data$likely.type = ifelse(data$accidenttype == "struck by, nec" | 
                              data$accidenttype == "struck by powered moving obj" |
                              data$accidenttype == "struck by rollng or slidng obj" |
                              data$accidenttype == "struck against moving object" |
                              data$accidenttype == "cght i, u, b, rnng, mshng objs" |
                              data$accidenttype == "cght i, u, b, mvng & sttn objs" |
                              data$accidenttype == "caught i, u, b, moving objects" |
                              data$accidenttype == "cght in, under, or btween, nec" |
                              data$accidenttype == "struck against moving object", 1, 0)
  
  data$unlikely.type = ifelse(data$accidenttype == "fall from ladders" | 
                                data$accidenttype == "fall to lower level, nec" |
                                data$accidenttype == "fall to wlkway or wrkng surfc" |
                                data$accidenttype == "fall onto or against objects" |
                                data$accidenttype == "rubbed or abraded, nec" |
                                data$accidenttype == "bodily reaction, nec" |
                                data$accidenttype == "over-exertion in lifting objs" |
                                data$accidenttype == "ovr-exrtn in pllng, pshng objs" |
                                data$accidenttype == "ovrexrtn in wldng, thrwng objs" |
                                data$accidenttype == "contact with elctric current" |
                                data$accidenttype == "acc type, without injuries" |
                                data$accidenttype == "contct w/ hot objs or substanc" |
                                data$accidenttype == "absrtn rad caust txc & nox sbs" |
                                data$accidenttype == "flash burns (electric)" |
                                data$accidenttype == "over-exertion, nec", 1, 0)
  
  data$uncertain.type = ifelse(data$accidenttype == "struck against stationary obj" |
                                 data$accidenttype == "fall frm mach, vehicle, equip" |
                                 data$accidenttype == "struck by falling object" |
                                 data$accidenttype == "struck by flying object" |
                                 data$accidenttype == "no value found" |
                                 data$accidenttype == "not elsewhere classified", 1, 0)
  
  data$maybe.type = ifelse((data$accidenttype == "acc type, without injuries" |
                              data$accidenttype == "struck against stationary obj" |
                              data$accidenttype == "fall frm mach, vehicle, equip" |
                              data$accidenttype == "struck by falling object" |
                              data$accidenttype == "struck by flying object" |
                              data$accidenttype == "no value found" |
                              data$accidenttype == "not elsewhere classified") & 
                             data$false.keyword == 0, 1, 0)
  
  data$moving.vehcl = ifelse(!(data$equiptypecode %in% c("06", "13", "28", "53", "?")), 1, 0)
  
  data$likely.equip = ifelse((data$equiptypecode %in% c("12", "23", "33", "34", "35", 
                                                        "37", "41", "61", "67")) &
                               data$false.keyword == 0, 1, 0)
  
  data$unlikely.equip = ifelse(data$equiptypecode %in% c("06", "09", "15", "16", "20", 
                                                         "28", "29", "53", "55", "?"), 1, 0)
  
  data$uncertain.equip = ifelse(data$equiptypecode %in% c("54", "71", "25", "13", "14"), 1, 0)
  
  data$likely.source = ifelse((data$injurysourcecode %in% c("074", "077", "081", "087", "104", 
                                                            "105", "106", "107", "108", "110")) & 
                                data$false.keyword == 0, 1, 0)
  
  data$unlikely.source = ifelse(data$injurysourcecode %in% c("003", "004", "006", "007", "008", 
                                                             "009", "012", "051", "089", "067", 
                                                             "068", "078", "079", "080", "083", 
                                                             "090", "092", "093", "096", "098", 
                                                             "112", "116", "125"), 1, 0)
  
  data$uncertain.source = ifelse(data$likely.source == 0 & data$unlikely.source == 0, 1, 0)
  
  data$likely.nature = ifelse(data$natureofinjury == "crushing", 1, 0)
  
  data$unlikely.nature = ifelse(data$natureofinjury == "burn or scald (heat)" |
                                  data$natureofinjury == "burn,chemicl-fume,compoun" |
                                  data$natureofinjury == "elect shock,electrocution" |
                                  data$natureofinjury == "hearing loss or impairmnt" |
                                  data$natureofinjury == "dust in eyes" |
                                  data$natureofinjury == "elect.arc burn-not contac", 1, 0)
  
  data$uncertain.nature = ifelse(data$natureofinjury == "no value found" |
                                   data$natureofinjury == "sprain,strain rupt disc" |
                                   data$natureofinjury == "cut,lacer,punct-opn wound" |
                                   data$natureofinjury == "contusn,bruise,intac skin" |
                                   data$natureofinjury == "fracture,chip" |
                                   data$natureofinjury == "multiple injuries" |
                                   data$natureofinjury == "amputation or enucleation" |
                                   data$natureofinjury == "dislocation" |
                                   data$natureofinjury == "other injury,nec" |
                                   data$natureofinjury == "scratch,abrasion,superfcl" |
                                   data$natureofinjury == "concussion-brain,cerebral" |
                                   data$natureofinjury == "joint,tendon,muscl inflam", 1, 0)
  
  data$likely.actvity = ifelse(grepl("operate", data$mineractivity) | grepl("roof", data$mineractivity), 1, 0)
  
  data$maybe.likely.actvity = ifelse(grepl("move/reel", data$mineractivity) | 
                                      grepl("handling supplies/materials", data$mineractivity), 1, 0)
  
  data$unlikely.activity = ifelse(data$activitycode %in% c("009", "016", "020", "022", "025", 
                                                           "026", "027", "029", "030", "032", 
                                                           "034", "036", "075", "066", "065",
                                                           "056"), 1, 0)
  
  data$uncertain.activity = ifelse(data$likely.actvity == 0 & 
                                     data$maybe.likely.actvity == 0 & 
                                     data$unlikely.activity == 0, 1, 0)
  
  data$likely.occup = ifelse(data$occupcode3digit %in% c("050", "046", "028", "016", "036"), 1, 0)
  
  data$unlikely.body = ifelse(data$bodypartcode %in% c("200", "340","420"), 1, 0)
  
  ##############################################################################
  
  # COMBINE LIKELY/MAYBE LIKELY/UNLIKELY GROUPS
  
  data$keyword.pts = rowSums(data[, c("pin", "strike", "drillsteel", 
                                      "trap", "collided", "hit", 
                                      "dropped", "ranover", "bumped", 
                                      "caught", "rolled", "between", 
                                      "wheel")], na.rm = TRUE)
  
  data$neg.keyword.pts = rowSums(data[, c("jarring", "outsidevehicle", "steering", 
                                          "bounced", "rock", "derail", 
                                          "cable", "strap", "trolleypole", 
                                          "tool.break", "bodyseat", "headroof", 
                                          "hole")], na.rm = TRUE)
  
  data$pos.pts = rowSums(data[, c("likely.class", "likely.equip", "likely.nature", 
                                  "likely.source", "likely.type")], na.rm = TRUE)
  
  data$neg.pts = rowSums(data[, c("unlikely.class", "unlikely.equip", "unlikely.source", 
                                  "unlikely.nature", "unlikely.type", "uncertain.activity")], na.rm = TRUE)
  
  ##############################################################################
  
  # GENERATE ADDITIONAL KEY WORDS USING EXISTING KEY WORDS
  
  data$no.vehcl = ifelse(!grepl("VEHICLE", data$narrative), 1, 0)
  
  data$v.to.v = ifelse((grepl("(VEHICLE|drill|steel|bolter|shear|cutter|tire).{1,35}PINNED/STRUCK.{1,35}VEHICLE", data$narrative) |
                          grepl("VEHICLE.{1,35}PINNED/STRUCK.{1,35}(VEHICLE|drill|steel|bolter|shear|cutter|tire)", data$narrative)) & 
                         data$hole == 0, 1, 0)
  
  data$v.to.p = ifelse((grepl("(VEHICLE).{1,20}PINNED/STRUCK.{1,20}(PERSON|BODY)", data$narrative) |
                          grepl("(PERSON|BODY).{1,20}PINNED/STRUCK.{1,20}(VEHICLE)", data$narrative)) & 
                         data$false.keyword == 0, 1, 0)
  
  data$int.obj.strike = ifelse(grepl("( )(block|rock|cho(c)*k|chunk|rail|i-beam)( )", data$old_narrative) & 
                                 grepl("VEHICLE", data$narrative) & 
                                 grepl("PINNED/STRUCK", data$narrative) &
                                 grepl("(tr(a)*m(m)*[^ ]{0,3}|op(e)*r(a)*t[^ ]{0,3}|back(in|ed).{1,10}VEHICLE|VEHICLE.{1,10}back(in|ed)|r(a|u)n|makin(g)*( )*(a)*( )*tu(r)*n|remote.{1,5}control|driv|pull)", data$narrative) &
                                 grepl("(steering |(hand )*knob).{1,20}(PINNED/STRUCK).{1,20}(BODY|PERSON)", data$narrative) &
                                 (data$accidenttypecode %in% c(8, 5)) & 
                                 data$falling.accident == 0 & 
                                 data$operating == 0, 1, 0)
  
  ##############################################################################
  
  # COMBINE KEY WORDS AND LIKELY/MAYBE LIKELY/UNLIKELY GROUPS
  
  data$holistic = ifelse((((data$likely.type == 1) | 
                             (data$maybe.type == 1)) & 
                            (data$likely.actvity == 1 | 
                               data$maybe.likely.actvity == 1) & 
                            (data$likely.class == 1) & 
                            data$moving.vehcl == 1), 1, 0)
  
  data$potential.ps = ifelse(data$keyword == 1 | 
                               data$likely.class == 1 | 
                               data$v.to.v == 1 | 
                               data$v.to.p == 1, 1, 0)
  
  data$likely.ps = ifelse((data$keyword == 1 | 
                             data$likely.class == 1 | 
                             data$v.to.v == 1 | 
                             data$v.to.p == 1) &
                            (data$falling.accident == 0) &
                            (data$bodyseat == 0 & 
                               data$headroof == 0 & 
                               data$hole == 0 &
                               data$cable == 0 & 
                               data$strap == 0 & 
                               data$tool.break == 0 &
                               data$outsidevehicle == 0 & 
                               data$derail == 0 & 
                               data$bounced == 0  & 
                               data$trolleypole == 0 & 
                               data$neg.roofbolt == 0 & 
                               data$unlikely.nature == 0 & 
                               data$unlikely.source == 0) &
                            (data$neg.keyword.pts < 2 & 
                               data$pos.pts > 1 & 
                               data$neg.pts < 3), 1, 0)
  
  ##############################################################################
  
  # DROP UNNECESSARY VARIABLES & FINAL FORMAT
  
  # drop unnecessry variables
  keep = c("accidentdate", "accident.only", "atrs", 
           "bent", "between", "bodyseat", 
           "bounced", "broke", "brokensteel", 
           "bumped", "by", "cable", 
           "canopy", "caught", "collided", 
           "controls", "derail", "dif.vehicle", 
           "digit", "documentno", "drill.action", 
           "drillsteel", "dropped", "entrapment", 
           "falling.accident",  "false.keyword", "flew", 
           "headcanopy", "headroof", "hit", 
           "hole", "in.vehicle", "int.obj.strike", 
           "jarring", "keyword", "keyword.pts", 
           "likely.actvity", "likely.class", "likely.equip", 
           "likely.nature", "likely.occup", "likely.ps", 
           "likely.source", "likely.type", "loose", 
           "loose.rbolting", "maybe.false.keyword", "maybe.likely.actvity", 
           "maybe.type", "mineid", "moving.vehcl", 
           "mult.vehcl", "neg.keyword.pts", "neg.pts", 
           "neg.roofbolt", "neg.wrench", "no.vehcl", 
           "num.body", "num.person", "num.pinstrike", 
           "num.vehicles", "num.unique.vehcl", "operating", 
           "outsidevehicle", "pin", "pos.pts", 
           "pos.roofbolt", "potential.ps", "PS", 
           "psobject.test", "ranover", "resin", 
           "rock", "rolled", "roofbolt", 
           "shuttlecar.or.rbolter", "steering", "strap", 
           "strike", "strikerib", "tool.break", "trap", 
           "trolleypole", "type", "uncertain.activity", 
           "uncertain.class", "uncertain.equip", "uncertain.nature", 
           "uncertain.source", "uncertain.type", "unevenbottom", 
           "unlikely.activity", "unlikely.body", "unlikely.class", 
           "unlikely.equip", "unlikely.nature", "unlikely.source", 
           "unlikely.type", "v.to.p", "v.to.v", "vcomp.test", 
           "wheel", "wrench")
  
  if (purpose == "train.test") {
    # 1000 rows; 100 columns; unique on documentno 
    data = data[, (names(data) %in% keep)] 
    data$accidentdate = 
      data$mineid = NULL
  }
  
  if (purpose == "classify") {
    # 75743 rows; 103 columns; unique on documentno 
    data = data[, (names(data) %in% keep)] 
  }
  
  # enforce factor storage
  for (var in names(data)) {
    data[, var] = factor(data[, var])
  }
  
  # randomly sort data (in case it was ordered)
  set.seed(625)
  rand = runif(nrow(data))
  data = data[order(rand), ]
  
  # bye 
  rm(keep, var, rand)
  
  ##############################################################################
  
  # OUTPUT DATA
  
  if (purpose == "train.test") {
    # output prepared PS training/testing set
      # 1000 rows; 101 columns; unique on documentno 
    saveRDS(data, file = prepared.train.test.out.file.name)
    rm(prepared.train.test.out.file.name)
  }
  
  if (purpose == "classify") {
    # output prepared merged PS accidents data
      # 75743 rows; 103 columns; unique on documentno 
    saveRDS(data, file = prepared.classify.out.file.name)
    rm(prepared.classify.out.file.name)
  }
  
}

################################################################################

# bye
rm(list = ls())

################################################################################
