# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 14 - Analyze Randomization Inference Method 2
  # Takes in results from second randomization inference procedure:
    # 13_randomization_inference_method_2.
  # Outputs lists of robustly significant subparts.

# Coded by: Julia Bodson, juliabodson@gmail.com

# Last edit 2/14/17

################################################################################

library(foreign)

################################################################################

# set root directory
# root = "/NIOSH-Analysis/data"
root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/results"
# root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis/results"

# inputs
  # defined dynamically within file (see lines 57-118)
    # csv lists of significant subparts from preferred models, 
    # produced in 10_fit_models, and .dta results of the second
    # randomization inference procedure, produced in
    # 13_randomization_inference_method_2.

# outputs
  # defined dynamically within file (see lines 122-134)
    # csv lists of robustly significant subparts from the
    # second randomization ifnerence procedure.

################################################################################

# PREFERENCES

# analyze results from models with union & longwall indicators? (default is "off")
specification.test = "off"

# analyze results from lag 3 and 5 robustness tests? (default is "off")
lag.3 = "off"
lag.5 = "off"

################################################################################

# define file paths
dtaroot = paste0(root, "/dta/", collapse = NULL)
csvroot = paste0(root, "/csv/", collapse = NULL)

# define file names
if (lag.3 == "off" & lag.5 == "off" & specification.test != "on") {
  dtaroot = paste0(root, "/dta/method_2/", collapse = NULL)
  csvroot = paste0(root, "/csv/",  collapse = NULL)
}
if (lag.3 == "on" | lag.5 == "on") {
  dtaroot3 = paste0(root, "/dta/lag_3/method_2/", collapse = NULL)
  csvroot3 = paste0(root, "/csv/lag_3/", collapse = NULL)
  dtaroot5 = paste0(root, "/dta/lag_5/method_2/", collapse = NULL)
  csvroot5 = paste0(root, "/csv/lag_5/", collapse = NULL)  
}
if (specification.test == "on") {
  dtaroot = paste0(root, "/dta/ulw/method_2/", collapse = NULL)
  csvroot = paste0(root, "/csv/ulw/",  collapse = NULL)
  file.ext = "_ulw"
}
if (specification.test != "on") {
  file.ext = ""
}

rm(root)

################################################################################

# LOOP THROUGH MODELS

for (injury in c("MR","PS")) {
 for (form in c("VR","VC")) {
  
  ################################################################################
    
  # inputs: significant variable list 
    # load in 4 csvs - preferred model results
  if (lag.3 == "off" & lag.5 == "off") {
    B.1.in.file.name = paste0(csvroot, injury, "_B_1_", form, "_sig_2012", file.ext, ".csv", collapse = NULL)
    B.4.in.file.name = paste0(csvroot, injury, "_B_4_", form, "_sig_2012", file.ext, ".csv", collapse = NULL)
    C.1.in.file.name = paste0(csvroot, injury, "_C_1_", form, "_sig_2012", file.ext, ".csv", collapse = NULL)
    C.4.in.file.name = paste0(csvroot, injury, "_C_4_", form, "_sig_2012", file.ext, ".csv", collapse = NULL)
  }
  if (lag.3 == "on" | lag.5 == "on") {
    B.1.in.file.name = paste0(csvroot3, injury, "_B_3_", form, "_sig_2012.csv", collapse = NULL)
    B.4.in.file.name = paste0(csvroot5, injury, "_B_5_", form, "_sig_2012.csv", collapse = NULL)
    C.1.in.file.name = paste0(csvroot3, injury, "_C_3_", form, "_sig_2012.csv", collapse = NULL)
    C.4.in.file.name = paste0(csvroot5, injury, "_C_5_", form, "_sig_2012.csv", collapse = NULL)
  }

  ################################################################################
    
  # inputs: method 2 inputs
    # load in 4 csvs - method 1 RI results
  if (lag.3 == "off" & lag.5 == "off") {
    B.1.ri.in.file.name = paste0(csvroot, injury, "_B_1_", form, "_method_2_input", file.ext, ".csv", collapse = NULL)
    B.4.ri.in.file.name = paste0(csvroot, injury, "_B_4_", form, "_method_2_input", file.ext, ".csv", collapse = NULL)
    C.1.ri.in.file.name = paste0(csvroot, injury, "_C_1_", form, "_method_2_input", file.ext, ".csv", collapse = NULL)
    C.4.ri.in.file.name = paste0(csvroot, injury, "_C_4_", form, "_method_2_input", file.ext, ".csv", collapse = NULL)
  }
  if (lag.3 == "on" | lag.5 == "on") {
    B.1.ri.in.file.name = paste0(csvroot3, injury, "_B_3_", form, "_method_2_input.csv", collapse = NULL)
    B.4.ri.in.file.name = paste0(csvroot5, injury, "_B_5_", form, "_method_2_input.csv", collapse = NULL)
    C.1.ri.in.file.name = paste0(csvroot3, injury, "_C_3_", form, "_method_2_input.csv", collapse = NULL)
    C.4.ri.in.file.name = paste0(csvroot5, injury, "_C_5_", form, "_method_2_input.csv", collapse = NULL)
  }

  ################################################################################
  
  # outputs: method 2 results
  if (lag.3 == "off" & lag.5 == "off") {
    B.1.out.file.name = paste0(csvroot, injury, "_B_1_", form, "_method_2_output", file.ext, ".csv", collapse = NULL)
    B.4.out.file.name = paste0(csvroot, injury, "_B_4_", form, "_method_2_output", file.ext, ".csv", collapse = NULL)
    C.1.out.file.name = paste0(csvroot, injury, "_C_1_", form, "_method_2_output", file.ext, ".csv", collapse = NULL)
    C.4.out.file.name = paste0(csvroot, injury, "_C_4_", form, "_method_2_output", file.ext, ".csv", collapse = NULL)
  }
  if (lag.3 == "on" | lag.5 == "on") {  
    B.1.out.file.name = paste0(csvroot3, injury, "_B_3_", form, "_method_2_output.csv", collapse = NULL)
    B.4.out.file.name = paste0(csvroot5, injury, "_B_5_", form, "_method_2_output.csv", collapse = NULL)
    C.1.out.file.name = paste0(csvroot3, injury, "_C_3_", form, "_method_2_output.csv", collapse = NULL)
    C.4.out.file.name = paste0(csvroot5, injury, "_C_5_", form, "_method_2_output.csv", collapse = NULL)
  }

  ################################################################################
  
  # LOAD DATA
  
  # load in preferred model results
  B.1 = read.table(B.1.in.file.name, sep = ",", header = T)
  B.4 = read.table(B.4.in.file.name, sep = ",", header = T)
  C.1 = read.table(C.1.in.file.name, sep = ",", header = T)
  C.4 = read.table(C.4.in.file.name, sep = ",", header = T)
  
  # load in method 2 inputs 
  B.1.ri = read.table(B.1.ri.in.file.name, sep = ",", header = T)
  B.4.ri = read.table(B.4.ri.in.file.name, sep = ",", header = T)
  C.1.ri = read.table(C.1.ri.in.file.name, sep = ",", header = T)
  C.4.ri = read.table(C.4.ri.in.file.name, sep = ",", header = T)
  
  # bye
  rm(B.1.in.file.name, B.4.in.file.name, C.1.in.file.name, C.4.in.file.name,
     B.1.ri.in.file.name, B.4.ri.in.file.name, C.1.ri.in.file.name, C.4.ri.in.file.name)
  
  ################################################################################
  
  # create lists of the subparts that were tested in method 2 (and their file paths)
  B.1.ri.list = B.1.ri$subpart
  B.4.ri.list = B.4.ri$subpart
  C.1.ri.list = C.1.ri$subpart
  C.4.ri.list = C.4.ri$subpart
  
  # create lists of the file paths to the method 2 output
  if (lag.3 == "off" & lag.5 == "off") {
    B.1.ri.files = paste0(dtaroot, injury, "_B_1_", form, "_", B.1.ri.list, ".dta", collapse = NULL)
    B.4.ri.files = paste0(dtaroot, injury, "_B_4_", form, "_", B.4.ri.list, ".dta", collapse = NULL)
    C.1.ri.files = paste0(dtaroot, injury, "_C_1_", form, "_", C.1.ri.list, ".dta", collapse = NULL)
    C.4.ri.files = paste0(dtaroot, injury, "_C_4_", form, "_", C.4.ri.list, ".dta", collapse = NULL)
  }
  if (lag.3 == "on" | lag.5 == "on") {
    B.1.ri.files = paste0(dtaroot3, injury, "_B_1_", form, "_", B.1.ri.list, ".dta", collapse = NULL)
    B.4.ri.files = paste0(dtaroot5, injury, "_B_4_", form, "_", B.4.ri.list, ".dta", collapse = NULL)
    C.1.ri.files = paste0(dtaroot3, injury, "_C_1_", form, "_", C.1.ri.list, ".dta", collapse = NULL)
    C.4.ri.files = paste0(dtaroot5, injury, "_C_4_", form, "_", C.4.ri.list, ".dta", collapse = NULL)
  }
  
  ################################################################################
  
  # format coefficients and p values
  names = c("subpart", "b", "p")
  
  # remove whitespace in tables
  for (d in c("B.1", "B.4", "C.1", "C.4")) {
    data = eval(parse(text = paste(d)))
    
    # rename variables and remove whitespace
    names(data) = names
    data = data[which(data$p != "."), ]
    data = data[-1, ]
    data = data[-1, ]
    
    # strip asterisks, if present (otherwise b will be dropped when formatted as a number)
    data$b = gsub("\\*", "", data$b)
    
    # format coefficients as numeric
    data$b = as.numeric(as.character(data$b))
    
    assign(paste(d), data)
  }
  
  ################################################################################
  
  # CALCULATE P VALUES FOR EACH SUBPART FROM METHOD 2
  
  names = "fake.coef"
  B.1.ri$new.p = NA
  for (a in B.1.ri.list) {
    if (lag.3 == "on") {
      data = read.dta(paste0(dtaroot3, injury, "_B_3_", form, "_", a, ".dta", collapse = NULL))
    }
    if (lag.3 == "off") {
      data = read.dta(paste0(dtaroot, injury, "_B_1_", form, "_", a, ".dta", collapse = NULL))
    }  
    names(data) = names
    data$fake.coef = as.numeric(as.character(data$fake.coef))
    fake.coefs = data$fake.coef
    true.coef = B.1[B.1$subpart == a, "b"]
    p = sum(fake.coefs >= true.coef, na.rm = TRUE) / sum(!is.na(fake.coefs))
    B.1.ri[B.1.ri$subpart == a, "new.p"] = p
  }
  
  B.4.ri$new.p = NA
  for (a in B.4.ri.list) {
    if (lag.5 == "on") {
      data = read.dta(paste0(dtaroot5, injury, "_B_5_", form, "_", a, ".dta", collapse = NULL))
    }
    if (lag.5 == "off") {
      data = read.dta(paste0(dtaroot, injury, "_B_4_", form, "_", a, ".dta", collapse = NULL))
    }  
    names(data) = names
    data$fake.coef = as.numeric(as.character(data$fake.coef))
    fake.coefs = data$fake.coef
    true.coef = B.4[B.4$subpart == a, "b"]
    p = sum(fake.coefs >= true.coef, na.rm = TRUE) / sum(!is.na(fake.coefs))
    B.4.ri[B.4.ri$subpart == a, "new.p"] = p
  }
  
  C.1.ri$new.p = NA
  for (a in C.1.ri.list) {
    if (lag.3 == "on") {
      data = read.dta(paste0(dtaroot3, injury, "_C_3_", form, "_", a, ".dta", collapse = NULL))
    }
    if (lag.3 == "off") {
      data = read.dta(paste0(dtaroot, injury, "_C_1_", form, "_", a, ".dta", collapse = NULL))
    }  
    names(data) = names
    data$fake.coef = as.numeric(as.character(data$fake.coef))
    fake.coefs = data$fake.coef
    true.coef = C.1[C.1$subpart == a, "b"]
    p = sum(fake.coefs >= true.coef, na.rm = TRUE) / sum(!is.na(fake.coefs))
    C.1.ri[C.1.ri$subpart == a, "new.p"] = p
  }
  
  C.4.ri$new.p = NA
  for (a in C.4.ri.list) {
    if (lag.5 == "on") {
      data = read.dta(paste0(dtaroot5, injury, "_C_5_", form, "_", a, ".dta", collapse = NULL))
    }
    if (lag.5 == "off") {
      data = read.dta(paste0(dtaroot, injury, "_C_4_", form, "_", a, ".dta", collapse = NULL))
    }  
    names(data) = names
    data$fake.coef = as.numeric(as.character(data$fake.coef))
    fake.coefs = data$fake.coef
    true.coef = C.4[C.4$subpart == a, "b"]
    p = sum(fake.coefs >= true.coef, na.rm = TRUE) / sum(!is.na(fake.coefs))
    C.4.ri[C.4.ri$subpart == a, "new.p"] = p
  }
  
  # bye
  rm (a, d, data, fake.coefs, true.coef, p, names,
      B.1.ri.list, B.4.ri.list, C.1.ri.list, C.4.ri.list,
      B.1.ri.files, B.4.ri.files, C.1.ri.files, C.4.ri.files)
  
  ################################################################################
  
  # SAVE CSVS WITH ROBUSTLY SIGNIFICANT SUBPARTS (AFTER METHOD 2)
  
  # only keep observations for significant subparts
  for (d in c("B.1", "B.4", "C.1", "C.4")) {
    data = eval(parse(text = paste(d, "ri", sep = ".")))
    data = data[which(data$new.p < 0.05) ,]
    file.name = eval(parse(text = paste(d, "out.file.name", sep = ".")))
    write.csv(data, file = file.name, row.names = FALSE)
    assign(paste(d, "ri", sep = "."), data)
  }
  
  #############################################################################
  } # end of injury loop
} # end of VC/VR loop

################################################################################

rm(list = ls())

################################################################################