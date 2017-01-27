# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 12 - Analyze RI Method 1
  # Takes in results from first randomization inference procedure
  # Outputs data taken into second randomizaiton inference procedure

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
  # Last edit 1/26/17

################################################################################

library(foreign)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/results"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/results"

dtaroot = paste0(root, "/dta/", collapse = NULL)
csvroot = paste0(root, "/csv/", collapse = NULL)

################################################################################

specification.test = "on" # analyze results from models with union & longwall indicators
#specification.test = "off"

#lag_3 = "on" # cannot be on at same time as ulw specification test
#lag_5 = "on" # cannot be on at same time as ulw specification test
lag_5 = "off"
lag_3 = "off"

# WHAT DO YOU WANT TO DO WITH THIS SAMPLE?

# analyze.method.1 = "on" # analyze method 1, spit out csvs of robustly significant subparts (p < 0.05)
analyze.method.1 = "off" # make a csv of each model set appended - cannot be done if analyzing method 1 

if (analyze.method.1 == "on") {
  append.models = "off"
}
if (analyze.method.1 == "off") {
  append.models = "on"
}


################################################################################

# DEFINE DTA AND CSV ROOTS FOR SPECIFICATION TESTS

if (specification.test == "on") {
  ulw.ext = "_ulw"
}
if (specification.test != "on") {
  ulw.ext = ""
}

if (specification.test == "on") {
  dtaroot = paste0(dtaroot, "ulw/", collapse = NULL)
  csvroot = paste0(csvroot, "ulw/", collapse = NULL)
}

dtaroot3 = paste0(dtaroot, "lag_3/", collapse = NULL)
csvroot3 = paste0(csvroot, "lag_3/", collapse = NULL)
dtaroot5 = paste0(dtaroot, "lag_5/", collapse = NULL)
csvroot5 = paste0(csvroot, "lag_5/", collapse = NULL)

################################################################################

# LOOP THROUGH MODELS

for (injury in c("MR", "PS")) {
  for (form in c("VR", "VC")) {
  
  ################################################################################
  
  # CSVS WITH LISTS OF SIGNIFICANT VARIABLES FROM PREFERRED MODELS
  
  if (lag_3 == "off" & lag_5 == "off") {
    B.1.sig.in.file = paste0(csvroot, injury, "_B_1_", form, "_sig_2012", ulw.ext, ".csv", collapse = NULL)
    B.4.sig.in.file = paste0(csvroot, injury, "_B_4_", form, "_sig_2012", ulw.ext, ".csv", collapse = NULL)
    C.1.sig.in.file = paste0(csvroot, injury, "_C_1_", form, "_sig_2012", ulw.ext, ".csv", collapse = NULL)
    C.4.sig.in.file = paste0(csvroot, injury, "_C_4_", form, "_sig_2012", ulw.ext, ".csv", collapse = NULL)  
  }
  if (lag_3 == "on" | lag_5 == "on") {
      # never going to be true at same time as union-longwall test
    B.1.sig.in.file = paste0(csvroot3, injury, "_B_3_", form, "_sig_2012.csv", collapse = NULL)
    B.4.sig.in.file = paste0(csvroot5, injury, "_B_5_", form, "_sig_2012.csv", collapse = NULL)
    C.1.sig.in.file = paste0(csvroot3, injury, "_C_3_", form, "_sig_2012.csv", collapse = NULL)
    C.4.sig.in.file = paste0(csvroot5, injury, "_C_5_", form, "_sig_2012.csv", collapse = NULL)  
  }
    
  # RESULTS OF RANDOMIZATION PROCEDURE METHOD 1 (must be Stata 12 .dtas)
  
  if (lag_3 == "off" & lag_5 == "off") {
    B.1.ri.in.file = paste0(dtaroot, injury, "_B_1_", form, "_ri.dta", collapse = NULL)
    B.4.ri.in.file = paste0(dtaroot, injury, "_B_4_", form, "_ri.dta", collapse = NULL)
    C.1.ri.in.file = paste0(dtaroot, injury, "_C_1_", form, "_ri.dta", collapse = NULL)
    C.4.ri.in.file = paste0(dtaroot, injury, "_C_4_", form, "_ri.dta", collapse = NULL)
  }
  if (lag_3 == "on" | lag_5 == "on") {
    B.1.ri.in.file = paste0(dtaroot3, injury, "_B_3_", form, "_ri.dta", collapse = NULL)
    B.4.ri.in.file = paste0(dtaroot5, injury, "_B_5_", form, "_ri.dta", collapse = NULL)
    C.1.ri.in.file = paste0(dtaroot3, injury, "_B_3_", form, "_ri.dta", collapse = NULL)
    C.4.ri.in.file = paste0(dtaroot5, injury, "_B_5_", form, "_ri.dta", collapse = NULL)
  }
  
  ################################################################################
  
  # NAME OUTPUT FILES: CSVS OF SUBPARTS THAT ARE ROBUSTLY SIGNIFICANT AFTER RANDOMZIATION INFERENCE METHOD 1
  
  if (lag_3 == "off" & lag_5 == "off") {
    B.1.out.file = paste0(csvroot, injury, "_B_1_", form, "_method_2_input", ulw.ext, ".csv", collapse = NULL)
    B.4.out.file = paste0(csvroot, injury, "_B_4_", form, "_method_2_input", ulw.ext, ".csv", collapse = NULL)
    C.1.out.file = paste0(csvroot, injury, "_C_1_", form, "_method_2_input", ulw.ext, ".csv", collapse = NULL)
    C.4.out.file = paste0(csvroot, injury, "_C_4_", form, "_method_2_input", ulw.ext, ".csv", collapse = NULL)
    
    # final csv with results from all 4 models  
    all.out.file = paste0(csvroot, injury, "_", form, ulw.ext, ".csv",  collapse = NULL)
  }
  if (lag_3 == "on" | lag_5 == "on") {
    B.4.out.file = paste0(csvroot3, injury, "_B_3_", form, "_method_2_input", ulw.ext, ".csv", collapse = NULL)
    B.4.out.file = paste0(csvroot5, injury, "_B_5_", form, "_method_2_input", ulw.ext, ".csv", collapse = NULL)
    C.1.out.file = paste0(csvroot3, injury, "_C_3_", form, "_method_2_input", ulw.ext, ".csv", collapse = NULL)
    C.4.out.file = paste0(csvroot5, injury, "_C_5_", form, "_method_2_input", ulw.ext, ".csv", collapse = NULL)
    
    # final csv with results from all 4 models  
    all.out.file = paste0(csvroot, injury, "_", form, "_3-5", ulw.ext, ".csv",  collapse = NULL)
  }
  
  ################################################################################
  
  # LOAD DATA FROM PREFERRED MODELS
  
  # load in preferred model results (significant subparts)
  B.1.sig = read.table(B.1.sig.in.file, sep = ",")
  names = c("subpart", "coefficient", "pvalue")
  names(B.1.sig) = names
  B.1.sig = B.1.sig[c(4: nrow(B.1.sig)),]
  B.1.sig = B.1.sig[which(B.1.sig$pvalue != "."),]
  B.1.sig$pvalue = as.numeric(as.character(B.1.sig$pvalue))
  B.1.sig$coefficient = gsub("\\*", "", B.1.sig$coefficient)
  
  B.4.sig = read.table(B.4.sig.in.file, sep = ",")
  names(B.4.sig) = names
  B.4.sig = B.4.sig[c(4: nrow(B.4.sig)),]
  B.4.sig = B.4.sig[which(B.4.sig$pvalue != "."),]
  B.4.sig$pvalue = as.numeric(as.character(B.4.sig$pvalue))
  B.4.sig$coefficient = gsub("\\*", "", B.4.sig$coefficient)
  
  C.1.sig = read.table(C.1.sig.in.file, sep = ",")
  names(C.1.sig) = names
  C.1.sig = C.1.sig[c(4: nrow(C.1.sig)),]
  C.1.sig = C.1.sig[which(C.1.sig$pvalue != "."),]
  C.1.sig$pvalue = as.numeric(as.character(C.1.sig$pvalue))
  C.1.sig$coefficient = gsub("\\*", "", C.1.sig$coefficient)
  
  C.4.sig = read.table(C.4.sig.in.file, sep = ",")
  names(C.4.sig) = names
  C.4.sig = C.4.sig[c(4: nrow(C.4.sig)),]
  C.4.sig = C.4.sig[which(C.4.sig$pvalue != "."),]
  C.4.sig$pvalue = as.numeric(as.character(C.4.sig$pvalue))
  C.4.sig$coefficient = gsub("\\*", "", C.4.sig$coefficient)
  
  ################################################################################
  
  # FORMAT DATA FROM PREFERRED MODELS
  
  for (d in c("B.1", "B.4", "C.1", "C.4")) {
    data = eval(parse(text = paste(d, "sig", sep = ".")))
    data$subpart = as.character(data$subpart)
    data$coefficient = as.numeric(as.character(data$coefficient))
    assign(paste(d, "sig", sep = "."), data)
    
    # create new dataframes with a column to store the new p-values that we will calculate next 
    temp = data.frame(data$subpart)
    names(temp) = "subpart"
    temp$subpart = as.character(temp$subpart)
    temp$p = numeric(nrow(temp))
    assign(paste(d, "ri", sep = "."), temp)
  }
  
  ################################################################################
  
  # APPEND RESULTS FROM PREFERRED MODELS - FOR MAKING LATEX TABLES
  
  if (append.models == "on") {
    
    # for each file, create a "strip" of the length necessary to extract the prefix from 
      # violation subpart variable names, so that only the subpart name goes into the table
    for (d in c("B.1", "B.4", "C.1", "C.4")) {
      if ((d == "B.1" | d == "C.1") & (lag_3 != "on" & lag_5 != "on")) {
        strip = 5
      }
      if ((d == "B.4" | d == "C.4") | ((lag_3 == "on" | lag_5 == "on") & (d == "B.1" | d == "C.1"))) {
        strip = 6
      }
      data = eval(parse(text = paste(d, "sig", sep = ".")))
      data$subpart = substr(data$subpart, 1, nchar(data$subpart) - strip)
      assign(paste(d, "sig", sep = "."), data)
    }
    
    # capture all subparts across each set of models that will go into one table (B-1, B-4, C-1, C-4)
      # this is because we can a row for every subpart for each column in this table, regardless
        # of whether or not that subpart will have a reported (significant) coefficient in the table
    all.subparts = c(B.1.sig$subpart, B.4.sig$subpart, C.1.sig$subpart, C.4.sig$subpart)
    all.subparts = unique(all.subparts)
    
    # now make sure each dataframe has rows for every significant subpart in this model-set
    for (d in c("B.1", "B.4", "C.1", "C.4")) {
      data = eval(parse(text = paste(d, "sig", sep = ".")))
      add = setdiff(all.subparts, data$subpart)
      add = data.frame(add)
      names(add) = "subpart"
      add$coefficient = NA
      add$pvalue = NA
      data = rbind(data, add)
      assign(paste(d, "sig", sep = "."), data)
    }
    
    # merge and clean each dataset (rename coefficient/p value)
    data = merge(B.1.sig, B.4.sig, by = "subpart", all = T)
    names(data)[names(data) == "coefficient.x"] = "c.B.1.sig"
    names(data)[names(data) == "pvalue.x"] = "p.B.1.sig"
    names(data)[names(data) == "coefficient.y"] = "c.B.4.sig"
    names(data)[names(data) == "pvalue.y"] = "p.B.4.sig"
    
    # merge on remaining models, rename variables
    data = merge(data, C.1.sig, by = "subpart", all = T)
    names(data)[names(data) == "coefficient"] = "c.C.1.sig"
    names(data)[names(data) == "pvalue"] = "p.C.1.sig"
    data = merge(data, C.4.sig, by = "subpart", all = T)
    names(data)[names(data) == "coefficient"] = "c.C.4.sig"
    names(data)[names(data) == "pvalue"] = "p.C.4.sig"

    # format coefficients (c) and p-values (p)
    for (d in c("c.B.1.sig", "c.B.4.sig", "c.C.1.sig", "c.C.4.sig")) {
      # format coefficient variables
      data[, d] = as.numeric(as.character(data[, d]))
      data[, d] = round(data[, d], digits = 3)
    }
    for (d in c("p.B.1.sig", "p.B.4.sig", "p.C.1.sig", "p.C.4.sig")) {
      # format coefficient variables
      data[, d] = as.numeric(as.character(data[, d]))
      data[, d] = round(data[, d], digits = 3)
    }
    
    # format coefficient variables, make sure trailing zero's will be preserved 
      # (so everything is nice and 3 digits after the decimal point in the table)
    for (d in c("c.B.1.sig", "c.B.4.sig", "c.C.1.sig", "c.C.4.sig")) {
      data[, d] = sprintf("%.3f", round(data[, d],3))
    }
    
    # add significance stars back in based on p-values
    data$c.B.1.sig = ifelse(data$p.B.1.sig < 0.001, paste0(data$c.B.1.sig, "***"), data$c.B.1.sig)
    data$c.B.1.sig = ifelse(data$p.B.1.sig < 0.01 & data$p.B.1.sig >= 0.001, paste0(data$c.B.1.sig, "**"), data$c.B.1.sig)
    data$c.B.1.sig = ifelse(data$p.B.1.sig <= 0.05 & data$p.B.1.sig >= 0.01, paste0(data$c.B.1.sig, "*"), data$c.B.1.sig)
    
    data$c.B.4.sig = ifelse(data$p.B.4.sig < 0.001, paste0(data$c.B.4.sig, "***"), data$c.B.4.sig)
    data$c.B.4.sig = ifelse(data$p.B.4.sig < 0.01 & data$p.B.4.sig >= 0.001, paste0(data$c.B.4.sig, "**"), data$c.B.4.sig)
    data$c.B.4.sig = ifelse(data$p.B.4.sig <= 0.05 & data$p.B.4.sig >= 0.01, paste0(data$c.B.4.sig, "*"), data$c.B.4.sig)
    
    data$c.C.1.sig = ifelse(data$p.C.1.sig < 0.001, paste0(data$c.C.1.sig, "***"), data$c.C.1.sig)
    data$c.C.1.sig = ifelse(data$p.C.1.sig < 0.01 & data$p.C.1.sig >= 0.001, paste0(data$c.C.1.sig, "**"), data$c.C.1.sig)
    data$c.C.1.sig = ifelse(data$p.C.1.sig <= 0.05 & data$p.C.1.sig >= 0.01, paste0(data$c.C.1.sig, "*"), data$c.C.1.sig)
    
    data$c.C.4.sig = ifelse(data$p.C.4.sig < 0.001, paste0(data$c.C.4.sig, "***"), data$c.C.4.sig)
    data$c.C.4.sig = ifelse(data$p.C.4.sig < 0.01 & data$p.C.4.sig >= 0.001, paste0(data$c.C.4.sig, "**"), data$c.C.4.sig)
    data$c.C.4.sig = ifelse(data$p.C.4.sig <= 0.05 & data$p.C.4.sig >= 0.01, paste0(data$c.C.4.sig, "*"), data$c.C.4.sig)
    
    # remove unnecssary variables and prep data so that it can be popped into a pretty latex table
    data = data[, c(-grep("^p", names(data)))]
    data$subpart = substr(data$subpart, 3, nchar(data$subpart) - 0)
    data$subpart = gsub("_", ".", data$subpart)
    
    # even if we format subpart as a character, when R writes to a csv, any subparts ending in "0"
      # will have the 0's dropped, e.g. "75.800" will become "75.8". adding a single quote around 
        # the subpart will guarantee zero's aren't dropped, and then when we paste the excel table in
          # our helpful online latex generator (http://www.tablesgenerator.com/latex_tables) the single quotes
            # will automatically be dropped.
    data$subpart = paste0("'", data$subpart, "'")
    data$subpart = as.character(data$subpart)
    
    # if lags 3 and 5 are being run, we imported "3" as "1" and "5" as "4" (because this file we coded before 3 and 5 were
    # being used as a robustness test), so rename the variables appropriately now before outputting
    if (lag_3 == "on" | lag_5 == "on") {
      names_3_and_5 = c("subpart", "c.B_3_sig", "c.B_5_sig", "c.C_3_sig", "c.C_5_sig")
      names(data) = names_3_and_5
    }
    
    write.csv(data, file = all.out.file, row.names = FALSE, na = "")
  } # end appending models
  
  ################################################################################
  
  if (analyze.method.1 == "on")  {
  
    # PROCESS RESULTS OF RANDOMIZATION INFERENCE METHOD 1
    
    # load in RI results
    B.1 = read.dta(B.1.ri.in.file)
    B.4 = read.dta(B.4.ri.in.file)
    C.1 = read.dta(C.1.ri.in.file)
    C.4 = read.dta(C.4.ri.in.file)
    
    # calculate new post method 1 p values for each subpart
    for (d in c("B.1", "B.4", "C.1", "C.4")) {
      true = eval(parse(text = paste(d, "sig", sep = ".")))
      fake = eval(parse(text = d))
      ri = eval(parse(text = paste(d, "ri", sep = ".")))
      for (i in 1:nrow(true)) {
        sp = true$subpart[i]
        true_coef = true[true$subpart == sp, "coefficient"]
        fake_coefs = fake[, grepl(sp, names(fake))]
        # replace ".ri" datasets with new p values
        p = sum(fake_coefs >= true_coef, na.rm = TRUE) / sum(!is.na(fake_coefs))
        ri[ri$subpart == sp, "p"] = p
        assign(paste(d, "ri", sep = "."), ri)
      }
    }
    
    ################################################################################
    
    # OUTPUT METHOD 1 RESULTS/METHOD 2 INPUT
    
    # drop observations where supbarts are no longer significant
    B.1.ri = B.1.ri[which(B.1.ri$p < 0.05) ,]
    B.4.ri = B.4.ri[which(B.4.ri$p < 0.05) ,]
    C.1.ri = C.1.ri[which(C.1.ri$p < 0.05) ,]
    C.4.ri = C.4.ri[which(C.4.ri$p < 0.05) ,]
    
    # save lists of robustly significant subparts (if there are any)
    if (nrow(B.1.ri) != 0) {
      write.csv(B.1.ri, file = B.1.out.file, row.names = FALSE)
    }
    if (nrow(B.4.ri) != 0) {
      write.csv(B.4.ri, file = B.4.out.file, row.names = FALSE)
    }
    if (nrow(C.1.ri) != 0) {
      write.csv(C.1.ri, file = C.1.out.file, row.names = FALSE)
    }
    if (nrow(C.4.ri) != 0) {
      write.csv(C.4.ri, file = C.4.out.file, row.names = FALSE)
    }
  } # end of loop of analyze method 1 = on
    
  ################################################################################

  } # end of rate/not-a-rate loop
} # end of MR/PS injury loop
  
################################################################################

rm(list = ls())

################################################################################
