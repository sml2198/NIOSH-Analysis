# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 7 - Prepare PS (Pinning and Striking) 
  # Train/Test:

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/13/17

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
  # clean PS training set
training.set.in.file.name = paste0(cleaned.input.path, "/clean_PS_training_set.rds", collapse = NULL)
  # merged PS accidents
merged.PS.in.file.name =  paste0(merged.input.path, "/merged_PS_accidents.rds", collapse = NULL)

# outputs
  # prepped PS training set
prepped.train.out.file.name = paste0(prepped.output.path, "/prepped_PS_train_test.rds", collapse = NULL)
  # prepped and merged PS-accidents data
prepped.classify.out.file.name = paste0(prepped.output.path, "/prepped_PS_classify.rds", collapse = NULL)

# generate file paths
dir.create(prepped.output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

################################################################################

# DEFINE LOOP THAT WILL ITERATE THROUGH PURPOSES

for (purpose in c("train.test", "classify")) { # make datasets for both training/testing AND accident classification
  
  ################################################################################
  
  # READ DATA
  
  if (purpose == "train.test") {
    # read cleaned PS training set data
      # 1000 rows; 105 columns; unique on documentno 
    ps.data = readRDS(training.set.in.file.name)
    rm(training.set.in.file.name)
  }
  
  if (purpose == "classify") {
    # read merged PS accidents data 
      # 75743 rows; 54 columns; unique on documentno 
    ps.data = readRDS(merged.PS.in.file.name)
    rm(merged.PS.in.file.name)
  }
  
  ################################################################################
  
  # CLEAN DATASET
  
  # clean number typos in the narratives
  ps.data$numbertypo = ifelse(grepl("[a-z][0-9][a-z]", ps.data$narrative), 1, 0)
  for (i in 0:9) {
    ps.data[ps.data$numbertypo == 1, "narrative"] = gsub(i, "", ps.data[ps.data$numbertypo == 1, "narrative"])
  }
  ps.data$numbertypo = NULL
  
  # clean common typos that may affect keyword searches
  ps.data$narrative = gsub("ag(a)*( )*(in)*st", "against", ps.data$narrative)
  
  # standardize "not-found" values within variables (e.g., "Unknown" or "Other" are changed to "NO VALUE FOUND")
  ps.data$accidenttype = ifelse(ps.data$accidenttype == "not elsewhereclassified", "NO VALUE FOUND", ps.data$accidenttype)
  ps.data$natureofinjury = ifelse(ps.data$natureofinjury == "unclassified,not determed", "NO VALUE FOUND", ps.data$natureofinjury)
  
  # format accident type codes
  ps.data$accidenttypecode = as.factor(ps.data$accidenttypecode)

  ################################################################################
  
  # GENERATE POSITIVELY PREDICTIVE KEY WORDS
  
  ps.data$pin = ifelse(grepl("(^| )pin(n*)(e|i)[a-z]+", ps.data$narrative) &
                         !grepl("(^| )pinion", ps.data$narrative) &
                         !grepl("(^| )pinner", ps.data$narrative) &
                         !grepl("pinn(ing|ed)( ).{1,5}top", ps.data$narrative) &
                         !grepl("pinn(ing|ed).{1,5}(him|his|her)self", ps.data$narrative), 1, 0)
  
  ps.data$strike = ifelse(grepl("str(i|u)(.*)k[a-z]*", ps.data$narrative) &
                            !grepl("str(i|u)(.*)k[a-z]*.{1,6}head", ps.data$narrative) &
                            !grepl("head.{1,6}str(i|u)(.*)k[a-z]*", ps.data$narrative), 1, 0)
  
  ps.data$trap = ifelse(grepl("( )trap[a-z]*", ps.data$narrative), 1, 0)
  
  ps.data$collided = ifelse(grepl("col(l)*i(de|ded|sion|ssion)", ps.data$narrative), 1, 0)
  
  ps.data$ranover = ifelse(grepl("( |^)r(a|u)n( )*(over|into)", ps.data$narrative), 1, 0)
  
  ps.data$rolled = ifelse(grepl("rolled( )*(over|into|onto|on|down)", ps.data$narrative), 1, 0)
  
  ps.data$between = ifelse(grepl("between", ps.data$narrative) | 
                             grepl("btwn", ps.data$narrative), 1, 0)
  
  ps.data$wheel = ifelse(grepl("wheel", ps.data$narrative) & 
                           !grepl("wheeler", ps.data$narrative), 1, 0)
  
  ps.data$by = ifelse(grepl("by", ps.data$narrative), 1, 0)
  
  ################################################################################
  
  # GENERATE NEGATIVELY PREDICTIVE KEY WORDS
  
  ps.data$jarring = ifelse(grepl("jar(r)*(ed|ing)", ps.data$narrative) |
                             grepl("jo(lt|stl)(ed|ing)", ps.data$narrative), 1, 0)
  
  ps.data$bounced = ifelse(grepl("boun(c)*( )*(e|ing)", ps.data$narrative), 1, 0)
  
  ps.data$rock = ifelse(grepl("rock( |$|\\.|s|,)", ps.data$narrative) & 
                          !grepl("rock( )*dust", ps.data$narrative), 1, 0)
  
  ps.data$digit = ifelse(grepl("(finger(s)*|pinky|hand(s)*|thumb|hand( |\\.|,|$))", ps.data$narrative), 1, 0)
  
  ps.data$derail = ifelse((grepl("(left|off|jumped).{1,15}track", ps.data$narrative) & 
                             !grepl("(left|off|jumped).{1,15}track.{1,3}switch", ps.data$narrative)) | 
                            grepl("derai", ps.data$narrative), 1, 0)
  
  ps.data$steering = ifelse(grepl("ste(e|a)ring( )*wheel.{1,15}sp(u|i)n", ps.data$narrative), 1, 0)
  
  ps.data$wrench = ifelse(grepl("wrench", ps.data$narrative), 1, 0)
  
  ps.data$controls = ifelse(grepl("(lever|stick)", ps.data$narrative), 1, 0)
  
  ps.data$resin = ifelse(grepl("resin", ps.data$narrative), 1, 0)
  
  ps.data$atrs = ifelse(grepl("a(\\.)*t(\\.)*r(\\.)*s(\\.)*", ps.data$narrative), 1, 0)
  
  ps.data$flew = ifelse(grepl("fl(ew|y|ing)", ps.data$narrative), 1, 0)
  
  ps.data$loose = ifelse(grepl("loose", ps.data$narrative), 1, 0)
  
  ps.data$broke = ifelse(grepl("br(oke|eak)", ps.data$narrative), 1, 0)
  
  ps.data$bent = ifelse(grepl("bent", ps.data$narrative) & 
                          !grepl("bent( )*over", ps.data$narrative), 1, 0)
  
  ps.data$canopy = ifelse(grepl("canopy", ps.data$narrative), 1, 0)
  
  ################################################################################
  
  # GENERATE KEY WORDS TO IDENTIFY FALSE POSITIVES
  
  # being jostled against the seat
  ps.data$bodyseat = ifelse(grepl("(back|head|neck|shoulder|elbo).{1,10}seat", ps.data$narrative) &
                              !grepl("backward.{1,10}seat", ps.data$narrative) &
                              !grepl("(bolt|over|drill)( )*head.{1,10}seat", ps.data$narrative), 1, 0) 
  
  # hitting head against canopy
  ps.data$headcanopy = ifelse((grepl("(head|neck).{1,5}(on|str(ike|uck)|hit|against).{1,5}(canopy)", ps.data$narrative) |
                                 grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(canopy)", ps.data$narrative)) &
                                !grepl("drill( )*head.{1,10}canopy", ps.data$narrative) &
                                !grepl("over( )*head.{1,10}canopy", ps.data$narrative) &
                                !grepl("head(ing|er|ed).{1,10}canopy", ps.data$narrative), 1, 0) 
  
  # going over a bump and operator hitting head 
  ps.data$hole = ifelse(grepl("(hit|str(ike|uck)|r(a|u)n( )*over|(went|go)( )*over).{1,10}(rock|hole|(h|b)ump(s| |$|\\.|,)|dip|depression|uneven|off( |-)*set|((low|bad|rough)( |-)*(spot|patch|place)))", ps.data$narrative), 1, 0)
  
  ps.data$unevenbottom = ifelse(grepl("(hole|bump(s| |$|\\.|,)|dip|depression|uneven|off( |-)*set|((low|bad|rough)( |-)*(spot|place|patch)))", ps.data$narrative) & 
                                  !grepl("(bolt|drill|steel|cable|test|pin).{1,15}hole", ps.data$narrative), 1, 0)
  
  # roof bolting/drilling steel injuries
  ps.data$drillsteel = ifelse(grepl("drill.{1,5}steel", ps.data$narrative) & 
                                grepl("(between|btwn).{1,17}steel.{1,25}(drill|head|roof|guide|canopy|ring)", ps.data$narrative), 1, 0)
  
  # drill steel breaking/bending during roofbolting
  ps.data$brokensteel = ifelse(grepl("drill.{1,5}steel", ps.data$narrative) & 
                                 (grepl("(drill|roof).{1,5}(steel|bolt).{1,15}(burst|ben(t|d)|br(eak|oke)|loose|drop(ped|ping)*|c(a|o)*me( )*( )*out|f(a|e)ll|stuck|clog)", ps.data$narrative) |
                                    grepl("wrench.{1,5}(slip|c(a|o)me).{1,5}off.{1,15}(bolt|drill head)", ps.data$narrative) |  
                                    grepl("wrench.{1,15}broke", ps.data$narrative)), 1, 0)
  
  ps.data$roofbolt = ifelse(grepl("(roof|( |^)rib).{1,10}bolt", ps.data$narrative) | 
                              grepl("(roof|rib).{1,25}bolting", ps.data$narrative) |
                              grepl("roof.{1,35}bolting", ps.data$narrative) |
                              grepl("bolt.{1,10}instal", ps.data$narrative) |
                              grepl("instal.{1,20}bolt", ps.data$narrative), 1, 0)
  
  ps.data$bolting = ifelse(grepl("bolting", ps.data$narrative) |
                             grepl("put(t)*(ing)*( )*bolt.{1,10}top", ps.data$narrative), 1, 0)
  
  # gloves getting caught during roof bolting
  ps.data$entrapment = ifelse((grepl("drill.{1,5}steel", ps.data$narrative) | 
                                 ps.data$roofbolt == 1 |
                                 ps.data$bolting == 1 ) & 
                                (grepl("(caught|catching|snagg(ed|ing)|grab).{1,10}(glove|shirt|sleeve)", ps.data$narrative) | 
                                   grepl("(glove|shi(r)*t|sle(e)*ve).{1,10}(entangl|cau( )*ght|catching|snagg(ed|ing))", ps.data$narrative)), 1, 0)
  
  ################################################################################
  
  # REPLACE ALL MENTIONS OF VEHICLES WITH "VEHICLE"
  
  # rename narrative variable - just in case 
  ps.data$old_narrative = ps.data$narrative
  
  # replace mentions of vehicle with "VEHICLE"
  ps.data$narrative = gsub("(man|ram|s(ch|h)uttle|scoop)( |-|- |v)*(trip|car)( car)*", "VEHICLE1", ps.data$narrative)
  ps.data$narrative = gsub("( |^)car( |-|s|\\.|,|$)", " VEHICLE2 ", ps.data$narrative)
  ps.data$narrative = gsub("(m|a)(m|a)n( |-|- )*bus", "VEHICLE3", ps.data$narrative)
  ps.data$narrative = gsub("vehic(l|e)(l|e)", "VEHICLE4", ps.data$narrative)
  ps.data$narrative = gsub("person(n)*(e|a)l carrier", "VEHICLE5", ps.data$narrative)
  ps.data$narrative = gsub("wheeler", "VEHICLE6", ps.data$narrative)
  ps.data$narrative = gsub("scooter", "VEHICLE7", ps.data$narrative)
  ps.data$narrative = gsub("s(ch|h)uttle", "VEHICLE8", ps.data$narrative)
  ps.data$narrative = gsub("cricket ", "VEHICLE9", ps.data$narrative)
  ps.data$narrative = gsub("(rock|roof)( |-)*bolter", "VEHICLE10", ps.data$narrative)
  ps.data$narrative = gsub("( |^)truck", " VEHICLE11", ps.data$narrative)
  ps.data$narrative = gsub("buggy", "VEHICLE12", ps.data$narrative)
  ps.data$narrative = gsub("stam(m)*ler", "VEHICLE13", ps.data$narrative)
  ps.data$narrative = gsub("mac( |-)*(8|eight)", "VEHICLE14", ps.data$narrative)
  ps.data$narrative = gsub("(3|three)( |-)*wh(ee)*l(e)*r", "VEHICLE15", ps.data$narrative)
  ps.data$narrative = gsub("(c)*ont.{1,10}mi(e)*n(r|er|ing)", "VEHICLE16", ps.data$narrative)
  ps.data$narrative = gsub("long( |-)*wall", "VEHICLE17", ps.data$narrative)
  ps.data$narrative = gsub("load( |-)*haul(-| )*dump", "VEHICLE18", ps.data$narrative)
  ps.data$narrative = gsub("(mining|miner|minr|loading|(roof)*( )*bolt(ing)*)( )*machine", "VEHICLE19", ps.data$narrative)
  ps.data$narrative = gsub("tunnel( |-)*borer", "VEHICLE20", ps.data$narrative)
  ps.data$narrative = gsub("fork( |-)*lift", "VEHICLE21", ps.data$narrative)
  ps.data$narrative = gsub("(front( |-)*end|scraper)( )*loader", "VEHICLE22", ps.data$narrative)
  ps.data$narrative = gsub("locomotiv(e)*", "VEHICLE23", ps.data$narrative)
  ps.data$narrative = gsub("(road|motor)( |-)*grader", "VEHICLE24", ps.data$narrative)
  ps.data$narrative = gsub("motor", "VEHICLE25", ps.data$narrative)
  ps.data$narrative = gsub("tractor", "VEHICLE26", ps.data$narrative)
  ps.data$narrative = gsub("jeep", "VEHICLE27", ps.data$narrative)
  ps.data$narrative = gsub("(ore)*( |-)haul(er|age)", "VEHICLE28", ps.data$narrative)
  ps.data$narrative = gsub("rail( |-)*runner", "VEHICLE29", ps.data$narrative)
  ps.data$narrative = gsub("feeder", "VEHICLE30", ps.data$narrative)
  ps.data$narrative = gsub("s/c", "VEHICLE31", ps.data$narrative)
  ps.data$narrative = gsub("shearer", "VEHICLE32", ps.data$narrative)
  ps.data$narrative = gsub("mucker", "VEHICLE33", ps.data$narrative)
  ps.data$narrative = gsub("eimco", "VEHICLE34", ps.data$narrative)
  ps.data$narrative = gsub("jitney", "VEHICLE35", ps.data$narrative)
  ps.data$narrative = gsub("bolter", "VEHICLE36", ps.data$narrative)
  ps.data$narrative = gsub("rail( |-)*runner", "VEHICLE37", ps.data$narrative)
  ps.data$narrative = gsub("mobile", "VEHICLE38", ps.data$narrative)
  ps.data$narrative = gsub("porta(l)*( |-)*bus", "VEHICLE39", ps.data$narrative)
  ps.data$narrative = gsub("( |^|-)bus(es| |\\.|,|$)", " VEHICLE40 ", ps.data$narrative)
  
  ps.data[!grepl("troll(e)*y( )*pol(e|l)", ps.data$narrative), "narrative"] = 
    gsub("trol(l)*(e)*y", " VEHICLE41 ", ps.data[!grepl("troll(e)*y( )*pol(e|l)", ps.data$narrative), "narrative"])
  
  ps.data[!grepl("to trip", ps.data$narrative), "narrative"] =
    gsub("( |^)trip( |$|,|\\.)", " VEHICLE42 ", ps.data[!grepl("to trip", ps.data$narrative),]$narrative)
  
  ps.data[!grepl("scoop(er|ing)", ps.data$narrative), "narrative"] = gsub("scoop", " VEHICLE43 ", ps.data[!grepl("scoop(er|ing)", ps.data$narrative), "narrative"])
  
  ps.data[!grepl("to tram", ps.data$narrative) & 
            !grepl("tram.{1,5}(lever|sprocket|pedal|chain)", ps.data$narrative), "narrative"] = 
    gsub("tram( |$|\\.|,)", " VEHICLE44 ", ps.data[!grepl("to tram", ps.data$narrative) & 
                                                     !grepl("tram.{1,5}(lever|sprocket|pedal|chain)", ps.data$narrative), "narrative"])
  
  ps.data$narrative = gsub("mucker", "VEHICLE45", ps.data$narrative)
  
  # create special vehicle flag
  ps.data$shuttlecar_or_rbolter = ifelse(grepl("VEHICLE(8|10|36)", ps.data$narrative) | 
                                           grepl("(s(ch|h)uttle).{1,30}( |-|- |v)*(trip|car)( car)*", ps.data$old_narrative), 1, 0)
  
  ################################################################################
  
  # REPLACE ALL MENTIONS OF BODY PARTS WITH "BODY"
  
  ps.data$narrative = gsub("hand(s| |\\.|,|$)", "BODY ", ps.data$narrative)
  ps.data$narrative = gsub("finger(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("thumb", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("ankle(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("shoulder(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("knee( |s|\\.|,|$)", "BODY ", ps.data$narrative) # avoid "kneel"
  ps.data$narrative = gsub("wrist(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("cal(f|ve|ves)", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("( |^)leg(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("eye(lid|brow|s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("cheek(s|bones)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("bone(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("( |^)lip(s)*", " BODY", ps.data$narrative)
  ps.data$narrative = gsub("( |^)ear(s)*", " BODY", ps.data$narrative)
  ps.data$narrative = gsub("chin( |$|\\.|,)", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("neck", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("(^| )(fore|for)*arm", " BODY", ps.data$narrative)
  ps.data$narrative = gsub("mouth", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("nose( |s|\\.|,|$)", "BODY ", ps.data$narrative)
  ps.data$narrative = gsub("pelvis", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("chest", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("groin", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("(t|f)ibia", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("ulna", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("radia", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("rib( |-)*cage", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("buttock(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("spine", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("elbow(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("testicle(s)*", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("t(ee|oo)th", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("(top|bottom) of|(r(igh)*t|l(e)*ft|his|her|both|onto|r\\.)( )*(foot|feet)", "BODY", ps.data$narrative)
  ps.data$narrative = gsub("( |^)hip(s)*", " BODY", ps.data$narrative)
  
  ps.data[!grepl("backward", ps.data$narrative), "narrative"] = 
    gsub("(lowe(r)*|upper|PERSON|the|strain).{1,8}back", " BODY", ps.data[!grepl("backward", ps.data$narrative), "narrative"])
  
  ps.data[!grepl("drill.{1,5}head", ps.data$narrative) & 
            !grepl("(over|cutter)( )*head", ps.data$narrative), "narrative"] = 
    gsub("(^| )head( |$|\\.|,)", " BODY ", ps.data[!grepl("drill.{1,5}head", ps.data$narrative) & 
                                                     !grepl("(over|cutter)( )*head", ps.data$narrative), "narrative"])
  
  ps.data[!grepl("(coal|the).{1,5}face", ps.data$narrative) & 
            !grepl("surface", ps.data$narrative), "narrative"] = 
    gsub("face", "BODY", ps.data[!grepl("(coal|the).{1,5}face", ps.data$narrative) & 
                                   !grepl("surface", ps.data$narrative), "narrative"])
  
  # generate positively predictive key words using body parts
  ps.data$bumped = ifelse((grepl("bump(ed|ing)( )*(over|into|onto)", ps.data$narrative) | 
                             grepl("bump(ed|ing).{1,10}BODY", ps.data$narrative)) &
                            !grepl("bump(ed|ing).{1,10}head", ps.data$old_narrative), 1, 0)
  
  ps.data$caught = ifelse(grepl("caught.{1,15}(between| in )", ps.data$old_narrative) |
                            grepl("caught.{1,10}BODY", ps.data$narrative) |
                            grepl("BODY.{1,6}caught", ps.data$narrative), 1, 0)
  
  ps.data$hit = ifelse(grepl("( |^)hit.{1,5}(by|him|his|her|employee|ee)", ps.data$old_narrative) |
                         grepl("( |^)hit.{1,10}BODY", ps.data$narrative), 1, 0)
  
  ps.data$dropped = ifelse(grepl("(lowe(r)*(ing|ed)*|drop(p)*(ing|ed)*).{1,15}(bucket|drill( |-)*head|drill( |-)*pod|pinner( |-)*head).{1,15}BODY", ps.data$narrative), 1, 0)
  
  ps.data$neg_wrench = ifelse(ps.data$wrench == 1 & 
                                (grepl("(burst|ben(t|d)|br(eak|oke)|loose|dislodge|shifted|drop(ped|ping)*|c(a|o)*me( )*( )*(out|off)|f(a|e)ll|stuck|clog|slipped)+", ps.data$old_narrative) | 
                                   ps.data$flew == 1 | 
                                   ps.data$caught == 1), 1, 0)
  
  ################################################################################
  
  # REPLACE ALL MENTIONS OF PINNING AND STRIKING WITH "PINNED/STRUCK"
  
  ps.data$narrative = gsub("( |^)pin(n)*(ed|ing)", " PINNED/STRUCK", ps.data$narrative)
  ps.data$narrative = gsub("(s)*tr(u|i)(c)*k(e|ing)*", "PINNED/STRUCK", ps.data$narrative)
  ps.data$narrative = gsub("r(a|u)n( )*(into|over)", "PINNED/STRUCK", ps.data$narrative)
  ps.data$narrative = gsub("col(l)*ided( w| with)*", "PINNED/STRUCK", ps.data$narrative)
  ps.data$narrative = gsub("( |^)trap(p)*(ed|ing)", " PINNED/STRUCK", ps.data$narrative)
  ps.data$narrative = gsub("rolled (into|onto|over)", "PINNED/STRUCK", ps.data$narrative)
  ps.data$narrative = gsub("c(a|u)(a|u)ght", "PINNED/STRUCK", ps.data$narrative)
  ps.data$narrative = gsub("catching|to catch", "PINNED/STRUCK", ps.data$narrative)
  
  ps.data[ps.data$hole == 0, "narrative"] = gsub("( |^)hit(t)*(ing)*( |$|\\.|,|s)", "PINNED/STRUCK", 
                                                 ps.data[ps.data$hole == 0, "narrative"])
  
  ps.data[grepl("VEHICLE.{1,5}got on{1,5}BODY", ps.data$narrative), "narrative"] = 
    gsub("got on", "PINNED/STRUCK", ps.data[grepl("VEHICLE.{1,5}got on{1,5}BODY", ps.data$narrative), "narrative"])
  
  ################################################################################
  
  # REPLACE ALL MENTIONS OF PEOPLE WITH "PERSON"
  
  ps.data$narrative = gsub("( |^)e(e|mp|mpl|mploye)(e)*( |$|,|\\.)", " PERSON ", ps.data$narrative)
  ps.data$narrative = gsub("( |^)i(,|\\.| )*name", " PERSON", ps.data$narrative)
  ps.data$narrative = gsub("injured person", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("(him|her)self", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("vi(c)*tim", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("repairm(a|e)n", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("maintenance( )*m(a|e)n", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("( |^)m(a|e)n( |$|,|\\.)", " PERSON ", ps.data$narrative)
  ps.data$narrative = gsub("fore(m|a)(a|e|m)n", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("ind(i)*v(idual)*(s)*", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("helper(s)*", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("person(s)*", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("worker(s)*", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("^inj(\\.)*(ured)*", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("the.{1,6}injured", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("injured was", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("( |^)(s)*he(r|rs)*( |$|,|\\.)", " PERSON ", ps.data$narrative)
  ps.data$narrative = gsub("( |^)hi(s|m)( |$|,|\\.)", " PERSON ", ps.data$narrative)
  ps.data$narrative = gsub("(wo)*m(a|e)n( |$|,|\\.)", "PERSON ", ps.data$narrative)
  ps.data$narrative = gsub("operat(o|e)r", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("passenger", "PERSON", ps.data$narrative)
  ps.data$narrative = gsub("driver", "PERSON", ps.data$narrative)
  
  ################################################################################
  
  # COUNT NUMBER OF UPPERCASE WORDS AND VEHICLES IN EACH NARRATIVE
  
  # count uppercase words in each narrative
  ps.data$num.vehicles = str_count(ps.data$narrative, "VEHICLE")
  ps.data$num.pinstrike = str_count(ps.data$narrative, "PINNED/STRUCK")
  ps.data$num.person = str_count(ps.data$narrative, "PERSON")
  ps.data$num.body = str_count(ps.data$narrative, "BODY")
  
  # count unique vehicles in each narrative
  uniq_vehcls = function(x) {
    return(length(unique(substr(unlist(regmatches(x, gregexpr("VEHICLE[0-9][0-9]*", x))), 8, 9))))
  }
  ps.data$num_unique_vehcl = sapply(ps.data$narrative, uniq_vehcls)
  ps.data$mult_vehcl = ifelse(ps.data$num_unique_vehcl > 1, 1, 0)
  
  # bye
  rm(uniq_vehcls)
  
  ################################################################################
  
  # GENERATE KEY WORDS USING UPPERCASE WORDS
  
  ps.data$dif_vehicle = ifelse(grepl("(second|another|different).{1,5}VEHICLE", ps.data$narrative), 1, 0)
  ps.data$loose_rbolting = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", ps.data$narrative), 1, 0)
  ps.data$drill_action = ifelse(grepl("(plate|bit|bolt)+.{1,10}PINNED/STRUCK", ps.data$narrative), 1, 0)
  
  ps.data$operating = ifelse((grepl("( |^|was|while|had)(tr(a)*m(m)*[^ ]{0,3}|op(e)*r(a)*t[^ ]{0,3}|backin.{1,10}VEHICLE|r(a|u)n|makin(g)*( )*(a)*( )*tu(r)*n|remote.{1,5}control|driv)", ps.data$narrative) &
                                (!grepl("PERSON.{1,20}(splic(e|ing)|crawl(ing)*|repair|fix)", ps.data$narrative) & 
                                   grepl("(splic(e|ing)|crawl(ing)*|repair|fix).{1,20}PERSON", ps.data$narrative)) &
                                (grepl("operat", ps.data$mineractivity) | 
                                   (grepl("roof bolt", ps.data$mineractivity) & 
                                      !grepl("help(ing|er|)", ps.data$old_narrative))) &
                                (!grepl("(side of|right|left|beside).{1,10}VEHICLE", ps.data$narrative) | 
                                   grepl("remote.{1,5}control", ps.data$narrative))), 1, 0)
  
  ################################################################################
  
  # GENERATE POSITIVELY PREDICTIVE CIRCUMSTANCE FLAGS
  
  # driver hitting head against vehicle roof
  ps.data$headroof = ifelse((grepl("(head|neck).{1,5}(on|str(ike|uck)|hit|against).{1,5}(roof|top)", ps.data$old_narrative) |
                               grepl("(bump|str(ike|uck)|hit).{1,5}(head|neck).{1,5}(roof|top)", ps.data$old_narrative) | 
                               (grepl("whip( )*lash", ps.data$old_narrative) & 
                                  ps.data$operating == 1) | 
                               grepl("jerked.{1,10}(head|neck)", ps.data$old_narrative)) &
                              !grepl("drill( )*head.{1,10}roof", ps.data$old_narrative) &
                              !grepl("over( )*head.{1,10}roof", ps.data$old_narrative) &
                              !grepl("head(ing|er|ed).{1,10}roof", ps.data$old_narrative) &
                              !grepl("head.{1,10}roof.{1,5}bolt", ps.data$old_narrative), 1, 0) 
  
  # roof bolting
  ps.data$pos_roofbolt = ifelse(ps.data$roofbolt == 1 & 
                                  grepl("PINNED/STRUCK.{1,15}between.{1,15}(roof( bolt)*|canopy|boom|(bolter|drill)( |-)*(pot|guide|head)|(drill|roof)*( |-)*(steel|guide)|( |^)rib|(bolt(er)*|torque)( |-)*wrench|lead|top).{1,30}(top|roof( bolt)*|canopy|boom|(bolter|drill)( |-)*(pot|guide|(h|l)ead)|(drill|roof)*( |-)*(steel|guide)|( |^)rib|(bolt(er)*|torque)( |-)*wrench|lead)", ps.data$narrative), 1, 0)
  
  ps.data$neg_roofbolt = ifelse(ps.data$roofbolt == 1 & 
                                  (ps.data$entrapment == 1 | 
                                     ps.data$brokensteel == 1), 1, 0)
  
  # person inside (or hanging outside) of the vehicle
  ps.data$in_vehicle = ifelse(grepl("riding.{1,10}(passenger|driver|operat(o|e)r)", ps.data$old_narrative) | 
                                grepl("PERSON.{1,8}riding", ps.data$narrative) |
                                grepl("riding.{1,5}outside", ps.data$narrative) |
                                !grepl("riding.{1,15}VEHICLE", ps.data$narrative), 1, 0)
  
  # operator arm or hand trailing outside vehicle
  ps.data$outsidevehicle = ifelse(((grepl("BODY.{1,15}(resting| hanging).{1,5}(over|out|on)", ps.data$narrative) & 
                                      grepl("VEHICLE", ps.data$narrative)) |
                                     grepl("BODY.{1,15}out( )*side.{1,30}VEHICLE", ps.data$narrative)) &
                                    !grepl("overhang", ps.data$narrative), 1, 0)
  
  # miner struck by a cable (non-PS); miner struck by the boom while replacing the cable (PS)
  ps.data$cable = ifelse((grepl("cable.{1,30}PINNED/STRUCK", ps.data$narrative) | 
                            grepl("PINNED/STRUCK.{1,30}cable", ps.data$narrative)) & 
                           (!grepl("boom", ps.data$narrative) &
                              !grepl("cable.{1,30}PINNED/STRUCK.{1,15}(against|between)", ps.data$narrative) &
                              !grepl("cable( )*bolt", ps.data$narrative)), 1, 0)
  
  ps.data$strap = ifelse(grepl("strap.{1,20}PINNED/STRUCK", ps.data$narrative) | 
                           grepl("PINNED/STRUCK.{1,20}strap", ps.data$narrative), 1, 0)
  
  ps.data$trolleypole = ifelse(grepl("PINNED/STRUCK.{1,20}troll(e)*y( )*pol(e|l)", ps.data$narrative) | 
                                 grepl("troll(e)*y( )*pol(e|l).{1,20}PINNED/STRUCK", ps.data$narrative) |
                                 (grepl(" pol(e|l).{1,20}PINNED/STRUCK", ps.data$narrative) & 
                                    grepl("troll(e)*y( )*pol(e|l)", ps.data$narrative)) |                                    
                                 (grepl("PINNED/STRUCK.{1,20} pol(e|l)", ps.data$narrative) & 
                                    grepl("troll(e)*y( )*pol(e|l)", ps.data$narrative)), 1, 0)
  
  ps.data$tool_break = ifelse(grepl("(wrench|roof bolt).{1,15}(br(eak|oke)|ben(d|t))", ps.data$old_narrative) & 
                                !grepl("(wrench|roof bolt).{1,15}(br(eak|oke)|ben(d|t)).{1,20}PINNED/STRUCK.{1,15}between", ps.data$narrative), 1, 0)
  
  ps.data$vcomp_test = ifelse(grepl("(seat|rail|canopy|battery|drill|steel|chain|cable)+.{1,20}VEHICLE", ps.data$narrative) | 
                                grepl("VEHICLE.{1,20}(seat|rail|canopy|battery|drill|steel|chain|cable)+", ps.data$narrative), 1, 0)
  
  ps.data$psobject_test = ifelse(grepl("(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+.{1,20}PINNED/STRUCK", ps.data$narrative) | 
                                   grepl("PINNED/STRUCK.{1,20}(corner|beam|overcast|rib|wall|coal|rock|header|top|seat|canopy)+", ps.data$narrative), 1, 0)
  
  ps.data$strikerib = ifelse((grepl("PINNED/STRUCK.{0,20}( )rib", ps.data$narrative) | 
                                grepl("( )rib.{1,20}PINNED/STRUCK", ps.data$narrative)) &
                               (!grepl("(lower|upper|side|cage|right|left| in |fractured|bruised).{0,10}( )rib", ps.data$old_narrative) & 
                                  !grepl("PERSON.{0,10}( )rib", ps.data$old_narrative) & 
                                  !grepl(" ribs", ps.data$old_narrative)), 1, 0)
  
  # falling rock 
  ps.data$falling.class = ifelse(ps.data$accidentclassification == "fall of roof or back", 1, 0)
  
  ps.data$falling.word = ifelse(grepl("rock( )*fell", ps.data$narrative) |
                                  grepl("fell.{1,20}roof", ps.data$narrative) |
                                  grepl("roof( )*f(a|e)ll", ps.data$narrative) |
                                  grepl("(rolled|fell) (from|.ff|out).{0,}( )rib", ps.data$narrative) |
                                  grepl("( )rib.{0,15}(rolled|fell) (from|.ff|out)", ps.data$narrative), 1, 0)
  
  ps.data$falling.accident = ifelse(ps.data$falling.class == 1 | ps.data$falling.word == 1, 1, 0)
  
  ps.data$falling.class = 
    ps.data$falling.word = NULL
  
  # identify accident only cases without missing accident type code
  ps.data$accident.only = ifelse((ps.data$degreeofinjury == "accident only" | 
                                    ps.data$accidenttypecode == 44) & 
                                   !is.na(ps.data$accidenttypecode), 1, 0)
  
  ################################################################################
  
  # GROUP KEY WORDS
  
  ps.data$keyword = ifelse((ps.data$pin == 1 | 
                              ps.data$strike == 1 | 
                              ps.data$pos_roofbolt == 1 | 
                              ps.data$trap == 1 | 
                              ps.data$collided == 1 | 
                              ps.data$hit == 1 | 
                              ps.data$dropped == 1 | 
                              ps.data$ranover == 1 | 
                              ps.data$bumped == 1 | 
                              ps.data$caught == 1 |
                              ps.data$rolled == 1 | 
                              ps.data$between == 1 | 
                              ps.data$wheel == 1) &
                             (ps.data$falling.accident == 0), 1, 0)
  
  ps.data$false_keyword = ifelse((ps.data$jarring == 1 | 
                                    ps.data$outsidevehicle == 1 | 
                                    ps.data$steering == 1 | 
                                    ps.data$neg_roofbolt == 1 |
                                    ps.data$bounced == 1 | 
                                    ps.data$rock == 1 | 
                                    ps.data$derail == 1 | 
                                    ps.data$cable == 1 | 
                                    ps.data$tool_break == 1 | 
                                    ps.data$bodyseat == 1 | 
                                    ps.data$headroof == 1 | 
                                    ps.data$strap == 1 | 
                                    ps.data$trolleypole == 1 | 
                                    ps.data$entrapment == 1 |
                                    ps.data$hole == 1), 1, 0)
  
  ps.data$maybe_false_keyword = ifelse((ps.data$digit == 1 | 
                                          ps.data$operating == 1 |
                                          ps.data$bent == 1 | 
                                          ps.data$strikerib == 1 |
                                          ps.data$wrench == 1 |
                                          ps.data$controls == 1 | 
                                          ps.data$resin == 1 |
                                          ps.data$loose == 1 | 
                                          ps.data$broke == 1 | 
                                          ps.data$canopy == 1 | 
                                          ps.data$flew == 1), 1, 0)
  
  ################################################################################
  
  # GENERATE LIKELY/MAYBE LIKELY/UNLIKELY INDICATORS
  # based on discussions with representatives from NIOSH and variable selection analyses
  
  # accident class indicators
  ps.data$likely_class = ifelse((ps.data$accidentclassification == "powered haulage" | 
                                   ps.data$accidentclassification == "machinery" ), 1, 0)
  
  ps.data$unlikely_class = ifelse((ps.data$accidentclassification == "disorders (repeated trauma)" | 
                                     ps.data$accidentclassification == "electrical" |
                                     ps.data$accidentclassification == "explosives and breaking agents" | 
                                     ps.data$accidentclassification == "stepping or kneeling on object" | 
                                     ps.data$accidentclassification == "ignition or explosion of gas or dust"), 1, 0)
  
  ps.data$uncertain_class = ifelse((ps.data$accidentclassification == "fall of roof or back" |
                                      ps.data$accidentclassification == "handling of materials" |
                                      ps.data$accidentclassification == "slip or fall of person" |
                                      ps.data$accidentclassification == "fall of face/rib/pillar/side/highwall" |
                                      ps.data$accidentclassification == "handtools (nonpowered)" |
                                      ps.data$accidentclassification == "no value found" |
                                      ps.data$accidentclassification == "other" |
                                      ps.data$accidentclassification == "striking or bumping"), 1, 0)
  
  # accident type indicators
  ps.data$likely_type = ifelse((ps.data$accidenttype == "struck by, nec" | 
                                  ps.data$accidenttype == "struck by powered moving obj" |
                                  ps.data$accidenttype == "struck by rollng or slidng obj" |
                                  ps.data$accidenttype == "struck against moving object" |
                                  ps.data$accidenttype == "cght i, u, b, rnng, mshng objs" |
                                  ps.data$accidenttype == "cght i, u, b, mvng & sttn objs" |
                                  ps.data$accidenttype == "caught i, u, b, moving objects" |
                                  ps.data$accidenttype == "cght in, under, or btween, nec" |
                                  ps.data$accidenttype == "struck against moving object"), 1, 0)
  
  ps.data$unlikely_type = ifelse((ps.data$accidenttype == "fall from ladders" | 
                                    ps.data$accidenttype == "fall to lower level, nec" |
                                    ps.data$accidenttype == "fall to wlkway or wrkng surfc" |
                                    ps.data$accidenttype == "fall onto or against objects" |
                                    ps.data$accidenttype == "rubbed or abraded, nec" |
                                    ps.data$accidenttype == "bodily reaction, nec" |
                                    ps.data$accidenttype == "over-exertion in lifting objs" |
                                    ps.data$accidenttype == "ovr-exrtn in pllng, pshng objs" |
                                    ps.data$accidenttype == "ovrexrtn in wldng, thrwng objs" |
                                    ps.data$accidenttype == "contact with elctric current" |
                                    ps.data$accidenttype == "acc type, without injuries" |
                                    ps.data$accidenttype == "contct w/ hot objs or substanc" |
                                    ps.data$accidenttype == "absrtn rad caust txc & nox sbs" |
                                    ps.data$accidenttype == "flash burns (electric)" |
                                    ps.data$accidenttype == "over-exertion, nec"), 1, 0)
  
  ps.data$uncertain_type = ifelse((ps.data$accidenttype == "struck against stationary obj" |
                                     ps.data$accidenttype == "fall frm mach, vehicle, equip" |
                                     ps.data$accidenttype == "struck by falling object" |
                                     ps.data$accidenttype == "struck by flying object" |
                                     ps.data$accidenttype == "no value found" |
                                     ps.data$accidenttype == "not elsewhere classified"), 1, 0)
  
  ps.data$maybe_type = ifelse((ps.data$accidenttype == "acc type, without injuries" |
                                 ps.data$accidenttype == "struck against stationary obj" |
                                 ps.data$accidenttype == "fall frm mach, vehicle, equip" |
                                 ps.data$accidenttype == "struck by falling object" |
                                 ps.data$accidenttype == "struck by flying object" |
                                 ps.data$accidenttype == "no value found" |
                                 ps.data$accidenttype == "not elsewhere classified") & 
                                ps.data$false_keyword == 0, 1, 0)
  
  # equipment indicators
  vehcl_equip_codes = c("06", "13", "28", "53", "?")
  ps.data$moving_vehcl = ifelse(!(ps.data$equiptypecode %in% vehcl_equip_codes), 1, 0)
  
  ps.data$likely_equip = ifelse((ps.data$equiptypecode == "12" |  
                                   ps.data$equiptypecode == "23" |
                                   ps.data$equiptypecode == "33" | 
                                   ps.data$equiptypecode == "34" |
                                   ps.data$equiptypecode == "35" | 
                                   ps.data$equiptypecode == "37" |
                                   ps.data$equiptypecode == "41" | 
                                   ps.data$equiptypecode == "61" |
                                   ps.data$equiptypecode == "67") & 
                                  ps.data$false_keyword == 0, 1, 0)
  
  ps.data$unlikely_equip = ifelse((ps.data$equiptypecode == "06" |  
                                     ps.data$equiptypecode == "09" |
                                     ps.data$equiptypecode == "15" |  
                                     ps.data$equiptypecode == "16" |
                                     ps.data$equiptypecode == "20" |
                                     ps.data$equiptypecode == "28" |
                                     ps.data$equiptypecode == "29" |
                                     ps.data$equiptypecode == "53" |
                                     ps.data$equiptypecode == "55" | 
                                     ps.data$equiptypecode == "?" ), 1, 0)
  
  ps.data$uncertain_equip = ifelse((ps.data$equiptypecode == "54" |
                                      ps.data$equiptypecode == "71" |  
                                      ps.data$equiptypecode == "25" |
                                      ps.data$equiptypecode == "13" | 
                                      ps.data$equiptypecode == "14"), 1, 0)
  
  # accident source indicators
  ps.data$likely_source = ifelse((ps.data$injurysourcecode == "074" | 
                                    ps.data$injurysourcecode == "077" | 
                                    ps.data$injurysourcecode == "081" | 
                                    ps.data$injurysourcecode == "087" | 
                                    ps.data$injurysourcecode == "104" | 
                                    ps.data$injurysourcecode == "105" |
                                    ps.data$injurysourcecode == "106" | 
                                    ps.data$injurysourcecode == "107" | 
                                    ps.data$injurysourcecode == "108" | 
                                    ps.data$injurysourcecode == "110") & 
                                   ps.data$false_keyword == 0, 1, 0)
  
  ps.data$unlikely_source = ifelse((ps.data$injurysourcecode == "003" | 
                                      ps.data$injurysourcecode == "004" | 
                                      ps.data$injurysourcecode == "006" | 
                                      ps.data$injurysourcecode == "007" | 
                                      ps.data$injurysourcecode == "008" | 
                                      ps.data$injurysourcecode == "009" |
                                      ps.data$injurysourcecode == "012" | 
                                      ps.data$injurysourcecode == "051" | 
                                      ps.data$injurysourcecode == "089" |
                                      ps.data$injurysourcecode == "067" | 
                                      ps.data$injurysourcecode == "068" |
                                      ps.data$injurysourcecode == "078" | 
                                      ps.data$injurysourcecode == "079" | 
                                      ps.data$injurysourcecode == "080" | 
                                      ps.data$injurysourcecode == "083" |
                                      ps.data$injurysourcecode == "090" |
                                      ps.data$injurysourcecode == "092" | 
                                      ps.data$injurysourcecode == "093" | 
                                      ps.data$injurysourcecode == "096" | 
                                      ps.data$injurysourcecode == "098" | 
                                      ps.data$injurysourcecode == "112" |
                                      ps.data$injurysourcecode == "116" | 
                                      ps.data$injurysourcecode == "125"), 1, 0)
  
  ps.data$uncertain_source = ifelse((ps.data$likely_source == 0 & ps.data$unlikely_source == 0), 1, 0)
  
  # accident nature indicators
  ps.data$likely_nature = ifelse(ps.data$natureofinjury == "crushing", 1, 0)
  
  ps.data$unlikely_nature = ifelse((ps.data$natureofinjury == "burn or scald (heat)" |
                                      ps.data$natureofinjury == "burn,chemicl-fume,compoun" |
                                      ps.data$natureofinjury == "elect shock,electrocution" |
                                      ps.data$natureofinjury == "hearing loss or impairmnt" |
                                      ps.data$natureofinjury == "dust in eyes" |
                                      ps.data$natureofinjury == "elect.arc burn-not contac"), 1, 0)
  
  ps.data$uncertain_nature = ifelse((ps.data$natureofinjury == "no value found" |
                                       ps.data$natureofinjury == "sprain,strain rupt disc" |
                                       ps.data$natureofinjury == "cut,lacer,punct-opn wound" |
                                       ps.data$natureofinjury == "contusn,bruise,intac skin" |
                                       ps.data$natureofinjury == "fracture,chip" |
                                       ps.data$natureofinjury == "multiple injuries" |
                                       ps.data$natureofinjury == "amputation or enucleation" |
                                       ps.data$natureofinjury == "dislocation" |
                                       ps.data$natureofinjury == "other injury,nec" |
                                       ps.data$natureofinjury == "scratch,abrasion,superfcl" |
                                       ps.data$natureofinjury == "concussion-brain,cerebral" |
                                       ps.data$natureofinjury == "joint,tendon,muscl inflam"), 1, 0)
  
  # activity indicators
  ps.data$likely_actvty = ifelse((grepl("operate", ps.data$mineractivity) | grepl("roof", ps.data$mineractivity)), 1, 0)
  
  ps.data$maybe_likely_actvty = ifelse(grepl("move/reel", ps.data$mineractivity) | 
                                         grepl("handling supplies/materials", ps.data$mineractivity), 1, 0)
  
  ps.data$unlikely_activity = ifelse((ps.data$activitycode == "009" | 
                                        ps.data$activitycode == "016" | 
                                        ps.data$activitycode == "020" | 
                                        ps.data$activitycode == "022" | 
                                        ps.data$activitycode == "025" | 
                                        ps.data$activitycode == "026" |
                                        ps.data$activitycode == "027" | 
                                        ps.data$activitycode == "029" | 
                                        ps.data$activitycode == "030" | 
                                        ps.data$activitycode == "032" | 
                                        ps.data$activitycode == "034" | 
                                        ps.data$activitycode == "036" |
                                        ps.data$activitycode == "075" | 
                                        ps.data$activitycode == "066" | 
                                        ps.data$activitycode == "065" | 
                                        ps.data$activitycode == "056"), 1, 0)
  
  ps.data$uncertain_activity = ifelse((ps.data$likely_actvty == 0 & 
                                         ps.data$maybe_likely_actvty == 0 & 
                                         ps.data$unlikely_activity == 0), 1, 0)
  
  # occupations indicator
  ps.data$likely_occup = ifelse((ps.data$occupcode3digit == "050" | 
                                   ps.data$occupcode3digit == "046" | 
                                   ps.data$occupcode3digit == "028" | 
                                   ps.data$occupcode3digit == "016" | 
                                   ps.data$occupcode3digit == "036"), 1, 0)
  
  # body parts indicator
  ps.data$unlikely_body = ifelse((ps.data$bodypartcode == "200" | 
                                    ps.data$bodypartcode == "340" | 
                                    ps.data$bodypartcode == "420"), 1, 0)
  
  # bye
  rm(vehcl_equip_codes)
  
  ################################################################################
  
  # SUM UP LIKELY AND UNLIKELY INDICATORS
  
  ps.data$keyword_pts = rowSums(ps.data[, c("pin", "strike", "drillsteel", 
                                            "trap", "collided", "hit", 
                                            "dropped", "ranover", "bumped", 
                                            "caught", "rolled", "between", 
                                            "wheel")], na.rm = TRUE)
  
  ps.data$neg_keyword_pts = rowSums(ps.data[, c("jarring", "outsidevehicle", "steering", 
                                                "bounced", "rock", "derail", 
                                                "cable", "strap", "trolleypole", 
                                                "tool_break", "bodyseat", "headroof", 
                                                "hole")], na.rm = TRUE)
  
  ps.data$pos_pts = rowSums(ps.data[, c("likely_class", "likely_equip", "likely_nature", 
                                        "likely_source", "likely_type")], na.rm = TRUE)
  
  ps.data$neg_pts = rowSums(ps.data[, c("unlikely_class", "unlikely_equip", "unlikely_source", 
                                        "unlikely_nature", "unlikely_type", "uncertain_activity")], na.rm = TRUE)
  
  ################################################################################
  
  # GENERATE KEY WORDS USING EXISTING KEY WORDS
  
  ps.data$no_vehcl = ifelse(!grepl("VEHICLE", ps.data$narrative), 1, 0)
  
  ps.data$v_to_v = ifelse((grepl("(VEHICLE|drill|steel|bolter|shear|cutter|tire).{1,35}PINNED/STRUCK.{1,35}VEHICLE", ps.data$narrative) |
                             grepl("VEHICLE.{1,35}PINNED/STRUCK.{1,35}(VEHICLE|drill|steel|bolter|shear|cutter|tire)", ps.data$narrative)) & 
                            ps.data$hole == 0, 1, 0)
  
  ps.data$v_to_p = ifelse((grepl("(VEHICLE).{1,20}PINNED/STRUCK.{1,20}(PERSON|BODY)", ps.data$narrative) |
                             grepl("(PERSON|BODY).{1,20}PINNED/STRUCK.{1,20}(VEHICLE)", ps.data$narrative)) & 
                            ps.data$false_keyword == 0, 1, 0)
  
  ps.data$int_obj_strike = ifelse((grepl("( )(block|rock|cho(c)*k|chunk|rail|i-beam)( )", ps.data$old_narrative) & 
                                     grepl("VEHICLE", ps.data$narrative) & 
                                     grepl("PINNED/STRUCK", ps.data$narrative) &
                                     grepl("(tr(a)*m(m)*[^ ]{0,3}|op(e)*r(a)*t[^ ]{0,3}|back(in|ed).{1,10}VEHICLE|VEHICLE.{1,10}back(in|ed)|r(a|u)n|makin(g)*( )*(a)*( )*tu(r)*n|remote.{1,5}control|driv|pull)", ps.data$narrative) &
                                     grepl("(steering |(hand )*knob).{1,20}(PINNED/STRUCK).{1,20}(BODY|PERSON)", ps.data$narrative) &
                                     (ps.data$accidenttypecode %in% c(8, 5)) & 
                                     ps.data$falling.accident == 0 & 
                                     ps.data$operating == 0), 1, 0)
  
  ################################################################################
  
  # GENERATE SIMPLE ALGORITHMS (USING ONLY KEY WORD AND CATEGORY INDICATORS)
  
  ps.data$holistic = ifelse((((ps.data$likely_type == 1) | 
                                (ps.data$maybe_type == 1)) & 
                               (ps.data$likely_actvty == 1 | 
                                  ps.data$maybe_likely_actvty == 1) & 
                               (ps.data$likely_class == 1) & 
                               ps.data$moving_vehcl == 1), 1, 0)
  
  ps.data$potential_ps = ifelse(ps.data$keyword == 1 | 
                                  ps.data$likely_class == 1 | 
                                  ps.data$v_to_v == 1 | 
                                  ps.data$v_to_p == 1, 1, 0)
  
  ps.data$likely_ps = ifelse((ps.data$keyword == 1 | 
                                ps.data$likely_class == 1 | 
                                ps.data$v_to_v == 1 | 
                                ps.data$v_to_p == 1) &
                               (ps.data$falling.accident == 0) &
                               (ps.data$bodyseat == 0 & 
                                  ps.data$headroof == 0 & 
                                  ps.data$hole == 0 &
                                  ps.data$cable == 0 & 
                                  ps.data$strap == 0 & 
                                  ps.data$tool_break == 0 &
                                  ps.data$outsidevehicle == 0 & 
                                  ps.data$derail == 0 & 
                                  ps.data$bounced == 0  & 
                                  ps.data$trolleypole == 0 & 
                                  ps.data$neg_roofbolt == 0 & 
                                  ps.data$unlikely_nature == 0 & 
                                  ps.data$unlikely_source == 0) &
                               (ps.data$neg_keyword_pts < 2 & 
                                  ps.data$pos_pts > 1 & 
                                  ps.data$neg_pts < 3), 1, 0)
  
  ################################################################################
  
  # DROP UNNECESSARY VARIABLES & FINAL FORMAT
  
  # 75743 rows; 156 columns; unique on documentno
  keep = c("accidentdate", "accident.only", "atrs", "bent", "between", "bodyseat", "bounced", "broke", "brokensteel", 
           "bumped", "by", "cable", "canopy", "caught", "collided", "controls", "derail", 
           "dif_vehicle", "digit", "documentno", "drill_action", "drillsteel", "dropped", "entrapment", "falling.accident", 
           "false_keyword", "flew", "headcanopy", "headroof", "hit", "hole", "in_vehicle", "int_obj_strike", 
           "jarring", "keyword", "keyword_pts", "likely_actvty", "likely_class", "likely_equip", "likely_nature", "likely_occup", 
           "likely_ps", "likely_source", "likely_type", "loose", "loose_rbolting", "maybe_false_keyword", "maybe_likely_actvty", "maybe_type", 
           "mineid", "moving_vehcl", "mult_vehcl", "neg_keyword_pts", "neg_pts", "neg_roofbolt", "neg_wrench", "no_vehcl", "num.body", 
           "num.person", "num.pinstrike", "num.vehicles", "num_unique_vehcl", "operating", "outsidevehicle", "pin", "pos_pts", 
           "pos_roofbolt", "potential_ps", "PS", "psobject_test", "ranover", "resin", "rock", "rolled", 
           "roofbolt", "shuttlecar_or_rbolter", "steering", "strap", "strike", "strikerib", "tool_break", "trap", 
           "trolleypole", "type", "uncertain_activity", "uncertain_class", "uncertain_equip", "uncertain_nature", "uncertain_source", "uncertain_type", 
           "unevenbottom", "unlikely_activity", "unlikely_body", "unlikely_class", "unlikely_equip", "unlikely_nature", "unlikely_source", 
           "unlikely_type", "v_to_p", "v_to_v", "vcomp_test", "wheel", "wrench")
  ps.data = ps.data[, (names(ps.data) %in% keep)] 
  
  # enforce factor storage
  vars = names(ps.data)
  for (i in 1:length(vars)) {
    ps.data[, vars[i]] = factor(ps.data[, vars[i]])
  }
  
  # randomly sort data (in case it was ordered)
  set.seed(625)
  rand = runif(nrow(ps.data))
  ps.data = ps.data[order(rand), ]
  
  # bye 
  rm(keep, vars, i, rand)
  
  ################################################################################
  
  # OUTPUT DATA
  
  if (purpose == "train.test") {
    # output prepped PS training set
      # 1000 rows; 101 columns; unique on documentno 
    ps.data = ps.data[, c(-match("accidentdate", names(ps.data)),
                          -match("mineid", names(ps.data)),
                          -match("type", names(ps.data)))]
    saveRDS(ps.data, file = prepped.train.out.file.name)
  }
  
  if (purpose == "classify") {
    # output prepped and merged PS accidents data 
      # 75743 rows; 102 columns; unique on documentno 
    saveRDS(ps.data, file = prepped.classify.out.file.name)
  }
  
  ################################################################################

} # end of loop for classify and train.test

################################################################################

rm(list = ls())

################################################################################
