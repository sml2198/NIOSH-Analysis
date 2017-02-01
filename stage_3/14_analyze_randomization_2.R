# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 14 - Analyze RI Method 2
  # Takes in results from second randomization inference procedure
  # Outputs lists of robustly significant subparts

# Coded by Sarah Levine, sarah.michael.levine@gmail.com
# Last edit 1/30/17

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

# PREFERENCES

# specification.test = "on" # analyze results from models with union & longwall indicators
specification.test = "off"

#lag_3 = "on" # cannot be on at same time as ulw specification test. will also run lag 5
#lag_5 = "on" 
lag_3 = "off"
lag_5 = "off"

################################################################################

# define file names
if (lag_3 == "off" & lag_5 == "off") {
  dtaroot = paste0(root, "/dta/method_2/", collapse = NULL)
  csvroot = paste0(root, "/csv/",  collapse = NULL)
}
if (lag_3 == "on" | lag_5 == "on") {
  dtaroot3 = paste0(root, "/dta/lag_3/method_2/", collapse = NULL)
  csvroot3 = paste0(root, "/csv/lag_3/", collapse = NULL)
  dtaroot5 = paste0(root, "/dta/lag_5/method_2/", collapse = NULL)
  csvroot5 = paste0(root, "/csv/lag_5/", collapse = NULL)  
}

################################################################################

# LOOP THROUGH MODELS

injury = "MR"
injury = "PS"

#for (injury in c("MR", "PS")) {
 # for (form in c("VR","VC")) {
  
  ################################################################################
    
  # inputs: significant variable list 
    # load in 4 csvs - preferred model results
  if (lag_3 == "off" & lag_5 == "off") {
    B.1.in.file.name = paste0(csvroot, injtype, "_B_1_", form, "_sig_2012.csv", collapse = NULL)
    B.4.in.file.name = paste0(csvroot, injtype, "_B_4_", form, "_sig_2012.csv", collapse = NULL)
    C.1.in.file.name = paste0(csvroot, injtype, "_C_1_", form, "_sig_2012.csv", collapse = NULL)
    C.4.in.file.name = paste0(csvroot, injtype, "_C_4_", form, "_sig_2012.csv", collapse = NULL)
  }
  if (lag_3 == "on" | lag_5 == "on") {
    B.1.in.file.name = paste0(csvroot3, injtype, "_B_3_", form, "_sig_2012.csv", collapse = NULL)
    B.4.in.file.name = paste0(csvroot5, injtype, "_B_5_", form, "_sig_2012.csv", collapse = NULL)
    C.1.in.file.name = paste0(csvroot3, injtype, "_C_3_", form, "_sig_2012.csv", collapse = NULL)
    C.4.in.file.name = paste0(csvroot5, injtype, "_C_5_", form, "_sig_2012.csv", collapse = NULL)
  }

  ################################################################################
    
  # inputs: method 2 inputs
    # load in 4 csvs - method 1 RI results
  if (lag_3 == "off" & lag_5 == "off") {
    B.1.sig.in.file.name = paste0(csvroot, injtype, "_B_1_", form, "_method_2_input.csv", collapse = NULL)
    B.4.sig.in.file.name = paste0(csvroot, injtype, "_B_4_", form, "_method_2_input.csv", collapse = NULL)
    C.1.sig.in.file.name = paste0(csvroot, injtype, "_C_1_", form, "_method_2_input.csv", collapse = NULL)
    C.4.sig.in.file.name = paste0(csvroot, injtype, "_C_4_", form, "_method_2_input.csv", collapse = NULL)
  }
  if (lag_3 == "on" | lag_5 == "on") {
    B.1.sig.in.file.name = paste0(csvroot3, injtype, "_B_3_", form, "_method_2_input.csv", collapse = NULL)
    B.4.sig.in.file.name = paste0(csvroot5, injtype, "_B_5_", form, "_method_2_input.csv", collapse = NULL)
    C.1.sig.in.file.name = paste0(csvroot3, injtype, "_C_3_", form, "_method_2_input.csv", collapse = NULL)
    C.4.sig.in.file.name = paste0(csvroot5, injtype, "_C_5_", form, "_method_2_input.csv", collapse = NULL)
  }

  ################################################################################
  
  # outputs: method 2 results
  if (lag_3 == "off" & lag_5 == "off") {
    B.1.out.file.name = paste0(csvroot, injtype, "_B_1_", form, "_method_2_output.csv", collapse = NULL)
    B.4.out.file.name = paste0(csvroot, injtype, "_B_4_", form, "_method_2_output.csv", collapse = NULL)
    C.1.out.file.name = paste0(csvroot, injtype, "_C_1_", form, "_method_2_output.csv", collapse = NULL)
    C.4.out.file.name = paste0(csvroot, injtype, "_C_4_", form, "_method_2_output.csv", collapse = NULL)
  }
  if (lag_3 == "on" | lag_5 == "on") {  
    B.1.out.file.name = paste0(csvroot3, injtype, "_B_3_", form, "_method_2_output.csv", collapse = NULL)
    B.4.out.file.name = paste0(csvroot5, injtype, "_B_5_", form, "_method_2_output.csv", collapse = NULL)
    C.1.out.file.name = paste0(csvroot3, injtype, "_C_3_", form, "_method_2_output.csv", collapse = NULL)
    C.4.out.file.name = paste0(csvroot5, injtype, "_C_5_", form, "_method_2_output.csv", collapse = NULL)
  }

  ################################################################################
  
  # LOAD DATA
  
  # load in preferred model results
  B.1 = read.table(B.1.in.file.name, sep = ",", header = T)
  B.4 = read.table(B.4.in.file.name, sep = ",", header = T)
  C.1 = read.table(C.1.in.file.name, sep = ",", header = T)
  C.4 = read.table(C.4.in.file.name, sep = ",", header = T)
  
  # load in method 2 inputs 
  B.1.sig = read.table(B.1.sig.in.file.name, sep = ",", header = T)
  B.4.sig = read.table(B.4.sig.in.file.name, sep = ",", header = T)
  C.1.sig = read.table(C.1.sig.in.file.name, sep = ",", header = T)
  C.4.sig = read.table(C.4.sig.in.file.name, sep = ",", header = T)
  
  # create lists of the subparts that were tested in method 2 (and their file paths)
  B.1.sig.list = B.1.sig$subpart
  B.4.sig.list = B.4.sig$subpart
  C.1.sig.list = C.1.sig$subpart
  C.4.sig.list = C.4.sig$subpart
  
  # create lists of the file paths to the method 2 output
  if (lag_3 == "off" & lag_5 == "off") {
    B.1.sig_files = paste0(dtaroot, injtype, "_B_1_", form, B.1.sig.list, ".dta", collapse = NULL)
    B.4.sig_files = paste0(dtaroot, injtype, "_B_4_", form, B.4.sig.list, ".dta", collapse = NULL)
    C.1.sig_files = paste0(dtaroot, injtype, "_C_1_", form, C.1.sig.list, ".dta", collapse = NULL)
    C.4.sig_files = paste0(dtaroot, injtype, "_C_4_", form, C.4.sig.list, ".dta", collapse = NULL)
  }
  if (lag_3 == "on" | lag_5 == "on") {
    B.1.sig_files = paste0(dtaroot3, injtype, "_B_1_", form, B.1.sig.list, ".dta", collapse = NULL)
    B.4.sig_files = paste0(dtaroot5, injtype, "_B_4_", form, B.4.sig.list, ".dta", collapse = NULL)
    C.1.sig_files = paste0(dtaroot3, injtype, "_C_1_", form, C.1.sig.list, ".dta", collapse = NULL)
    C.4.sig_files = paste0(dtaroot5, injtype, "_C_4_", form, C.4.sig.list, ".dta", collapse = NULL)
  }
  
  ################################################################################
  
  # format coefficients and subparts
  
  names = c("subpart", "b", "p")
  names(B_1) = names
  names(B_4) = names
  names(C_1) = names
  names(C_4) = names
  
  # remove whitespace in tables
  B.1 = B.1[which(B.1$p != "."), ]
  B.1 = B.1[-1, ]
  B.1 = B.1[-1, ]
  
  B.4 = B.4[which(B.4$p != "."), ]
  B.4 = B.4[-1, ]
  B.4 = B.4[-1, ]
  
  C.1 = C.1[which(C.1$p != "."), ]
  C.1 = C.1[-1, ]
  C.1 = C.1[-1, ]
  
  C.4 = C.4[which(C.4$p != "."), ]
  C.4 = C.4[-1, ]
  C.4 = C.4[-1, ]
  
  # strip asterisks, if present (otherwise b will be dropped when formatted as a number)
  B.1$b = gsub("\\*", "", B.1$b)
  B.4$b = gsub("\\*", "", B.4$b)
  C.1$b = gsub("\\*", "", C.1$b)
  C.4$b = gsub("\\*", "", C.4$b)
  
  # format coefficients as numeric
  B.1$b = as.numeric(as.character(B.1$b))
  B.4$b = as.numeric(as.character(B.4$b))
  C.1$b = as.numeric(as.character(C.1$b))
  C.4$b = as.numeric(as.character(C.4$b))
  
  ################################################################################
  
  # LOAD IN RI METHOD 2 RESULTS & CALCULATE P VALUES FOR EACH SUBPART
  
  names = "fake_coef"
  B.1.sig$new_p = NA
  for (a in B.1.sig.list) {
    if (lag_3 == "on") {
      data = read.dta(paste0(dtaroot3, injtype, "_B_3_", form, a, ".dta", collapse = NULL))
    }
    if (lag_3 == "off") {
      data = read.dta(paste0(dtaroot, injtype, "_B_1_", form, a, ".dta", collapse = NULL))
    }  
    names(data) = names
    data$fake.coef = as.numeric(as.character(data$fake.coef))
    fake.coefs = data$fake.coef
    true.coef = B.1[B.1$subpart == a, "b"]
    p = sum(fake.coefs >= true.coef, na.rm = TRUE) / sum(!is.na(fake.coefs))
    B.1.sig[B.1.sig$subpart == a, "new_p"] = p
  }
  
  B.4.sig$new_p = NA
  for (a in B.4.sig.list) {
    if (lag_5 == "on") {
      data = read.dta(paste0(dtaroot5, injtype, "_B_5_", form, a, ".dta", collapse = NULL))
    }
    if (lag_5 == "off") {
      data = read.dta(paste0(dtaroot, injtype, "_B_4_", form, a, ".dta", collapse = NULL))
    }  
    names(data) = names
    data$fake.coef = as.numeric(as.character(data$fake.coef))
    fake.coefs = data$fake.coef
    true.coef = B_4[B_4$subpart == a, "b"]
    p = sum(fake.coefs >= true.coef, na.rm = TRUE) / sum(!is.na(fake.coefs))
    B_4_sig[B_4_sig$subpart == a, "new_p"] = p
  }
  
  C.1.sig$new_p = NA
  for (a in C.1.sig.list) {
    if (lag_3 == "on") {
      data = read.dta(paste0(dtaroot3, injtype, "_C_3_", form, a, ".dta", collapse = NULL))
    }
    if (lag_3 == "off") {
      data = read.dta(paste0(dtaroot, injtype, "_C_1_", form, a, ".dta", collapse = NULL))
    }  
    names(data) = names
    data$fake.coef = as.numeric(as.character(data$fake.coef))
    fake.coefs = data$fake.coef
    true.coef = C.1[C.1$subpart == a, "b"]
    p = sum(fake.coefs >= true.coef, na.rm = TRUE) / sum(!is.na(fake.coefs))
    C.1.sig[C.1.sig$subpart == a, "new_p"] = p
  }
  
  C.4.sig$new_p = NA
  for (a in C.4.sig.list) {
    if (lag_5 == "on") {
      data = read.dta(paste0(dtaroot5, injtype, "_C_5_", form, a, ".dta", collapse = NULL))
    }
    if (lag_5 == "off") {
      data = read.dta(paste0(dtaroot, injtype, "_C_4_", form, a, ".dta", collapse = NULL))
    }  
    names(data) = names
    data$fake.coef = as.numeric(as.character(data$fake.coef))
    fake.coefs = data$fake.coef
    true.coef = C.4[C.4$subpart == a, "b"]
    p = sum(fake.coefs >= true.coef, na.rm = TRUE) / sum(!is.na(fake.coefs))
    C.4.sig[C.4.sig$subpart == a, "new_p"] = p
  }
  
  ################################################################################
  
  # SAVE CSVS WITH ROBUSTLY SIGNIFICANT SUBPARTS (AFTER METHOD 2)
  
  B.1.sig = B.1.sig[which(B.1.sig$new_p < 0.05) ,]
  B.4.sig = B.4.sig[which(B.4.sig$new_p < 0.05) ,]
  C.1.sig = C.1.sig[which(C.1.sig$new_p < 0.05) ,]
  C.4.sig = C.4.sig[which(C.4.sig$new_p < 0.05) ,]
  
  if (nrow(B_1_sig) != 0) {
    write.csv(B.1.sig, file = B.1.out.file.name, row.names = FALSE)
  }
  if (nrow(B_4_sig) != 0) {
    write.csv(B.4.sig, file = B.4.out.file.name, row.names = FALSE)
  }
  if (nrow(C_1_sig) != 0) {
    write.csv(C.1.sig, file = C.1.out.file.name, row.names = FALSE)
  }
  if (nrow(C_4_sig) != 0) {
    write.csv(C.4.sig, file = C.4.out.file.name, row.names = FALSE)
  }

  #############################################################################

  } # end of injury loop
} # end of VC/VR loop

################################################################################

rm(list = ls())

################################################################################