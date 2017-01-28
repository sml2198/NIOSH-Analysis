# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 15 - Analyze Predictions

# Coded by: Julia Bodson, juliabodson@gmail.com

# Last edit 1/27/2017

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
results.in.path = paste0(root, "/results/csv", collapse = NULL) 
results.out.path = paste0(root, "/results/csv/prediction", collapse = NULL) 

# inputs

# outputs

# generate file paths
dir.create(results.out.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
rm(root)

################################################################################

# DEFINE FUNCTIONS TO CALCULATE MEASURES OF INTEREST

calc.TP = function(data, predict.var.name, true.var.name) { # true positive
  return(sum(data[, predict.var.name] == 1 & data[, true.var.name] == 1))
}
calc.TN = function(data, predict.var.name, true.var.name) { # true negative
  return(sum(data[, predict.var.name] == 0 & data[, true.var.name] == 0))
}
calc.FP = function(data, predict.var.name, true.var.name) { # false positive
  return(sum(data[, predict.var.name] == 1 & data[, true.var.name] == 0))
}
calc.FN = function(data, predict.var.name, true.var.name) { # false negative
  return(sum(data[, predict.var.name] == 0 & data[, true.var.name] == 1))
}
calc.CCR = function(TP, TN, FP, FN) { # correct classification rate
  return(round(100 * ((TP + TN) / (TP + TN + FP + FN)), 2))
}
calc.FPR = function(TP, TN, FP, FN) { # false positive rate
  return(round(100 * (FP / (FP + TN)), 2))
}
calc.FNR = function(TP, TN, FP, FN) { # false negative rate
  return(round(100 * (FN / (FN + TP)), 2))
}

################################################################################

for (injury in c("MR", "PS")) {
  
  for (year in 2010:2014) {
    
    for (var in c("VC", "VR")) {
      
      # READ DATA
      
      # read predictions
        # produced in 10_fit_models
        # 6253 rows; 49 columns; unique on mineid-year
      data = read.csv(paste0(results.in.path, "/", paste(injury, var, toString(year), "predictions", sep = "_"), ".csv", collapse = NULL))
      
      ############################################################################
      
      # CLEAN DATA
      
      # drop unnecessary variables
        # 6253 rows; 13 columns; unique on mineid-year
      data = data[, c(names(data)[grepl(injury, names(data))], "dv", "dv_indicator")]

      # group data based on B or C model type
        # 6253 rows; 6 columns; unique on mineid-year
      b.data = data[, c(names(data)[grepl("B", names(data))], "dv_indicator")]
        # 6253 rows; 7 columns; unique on mineid-year
      c.data = data[, c(names(data)[grepl("C", names(data))], "dv", "dv_indicator")]

      # transform predictions
      b.data = data.frame(ifelse(b.data <= 0.5, 0, 1))
      c.as.b.data = data.frame(ifelse(c.data <= 0.5, 0, 1))
      c.data = data.frame(round(c.data))
      
      # rename variables
      names(b.data) = c(paste0("B_1_", var), 
                          paste0("B_4_", var),
                          "B_WEAK_NULL",
                          "B_STRONG_NULL_1",
                          "B_STRONG_NULL_2",
                          "true_indicator")
      names(c.data) = c(paste0("C_1_", var), 
                            paste0("C_4_", var),
                            "C_WEAK_NULL",
                            "C_STRONG_NULL_1",
                            "C_STRONG_NULL_2",
                            "true_count", 
                            "true_indicator")
      names(c.as.b.data) = names(c.data)
      
      # bye
      rm(data)
      
      ############################################################################
      
      # PREPARE EMPTY DATAFRAME TO HOLD MEASURES OF INTEREST
      
      # B models
        # 5 rows; 8 columns
      b.model.sum = data.frame(names(b.data)[grepl("B", names(b.data))])
      names(b.model.sum) = "Model"
      b.model.sum$Model = as.character(b.model.sum$Model)
      b.model.sum[, c("TP", "TN", "FP", "FN", "CCR", "FPR", "FNR")] = NA
    
      # C models
        # 5 rows; 11 columns
      c.model.sum = data.frame(names(c.data)[grepl("C", names(c.data))])
      names(c.model.sum) = "Model"
      c.model.sum$Model = as.character(c.model.sum$Model)
      c.model.sum[, c("TP", "TN", "FP", "FN", "CCR", "FPR", "FNR", "SSD", "SSPD", "SSND")] = NA
  
      ############################################################################
      
      # CALCULATE MEASURES OF INTEREST
      
      for (type in c("B", "C")) {
        
        # use data for either B or C models
        if (type == "B") {
          data = b.data
          model.sum.data = b.model.sum
        }
        else if (type == "C") {
          data = c.as.b.data
          model.sum.data = c.model.sum
        }
        
        # calculate measures of interest
        for (i in 1:nrow(model.sum.data)) {
          model = model.sum.data$Model[i]
          model.data = data[!is.na(data[, model]), c(model, "true_indicator")]
          
          for (measure in c("TP", "TN", "FP", "FN")) {
            f = eval(parse(text = paste0("calc.", measure)))
            model.sum.data[i, measure] = f(model.data, model, "true_indicator")
          }
          
          for (measure in c("CCR", "FPR", "FNR")) {
            f = eval(parse(text = paste0("calc.", measure)))
            model.sum.data[i, measure] = f(model.sum.data[i, "TP"],
                                           model.sum.data[i, "TN"],
                                           model.sum.data[i, "FP"],
                                           model.sum.data[i, "FN"])
          }
        
          if (type == "C") { # for C models, calculate SSD, SSPD, and SSND
            c.model.data = c.data[!is.na(c.data[, model]), c(model, "true_count")]
            D = c.model.data[, model] - c.model.data$true_count
            PD = D[D > 0]
            ND = D[D < 0]
            model.sum.data$SSD[i] = sum(D ^ 2) 
            model.sum.data$SSPD[i] = sum(PD ^ 2)  
            model.sum.data$SSND[i] = sum(ND ^ 2)
          }
        }
        
        # save data for either B or C models
        if (type == "B") {
          b.model.sum = model.sum.data
        }
        if(type == "C") {
          c.model.sum = model.sum.data
        }
        
      }
      
      # label B and C data to bind VC and VR data later
      assign(paste(var, "b.model.sum", sep = "."), b.model.sum)
      assign(paste(var, "c.model.sum", sep = "."), c.model.sum)
      
      # bye
      drop = c("TP", "TN", "FP", "FN")
      b.model.sum = b.model.sum[, !(names(b.model.sum) %in% drop)]
      c.model.sum = c.model.sum[, !(names(c.model.sum) %in% drop)]
      rm(b.data, c.data, c.as.b.data, 
         model.data, model.sum.data, data, c.model.data, 
         i, model, measure, f, D, PD, ND, drop, type)
      
    }
  
    ##############################################################################

    # COMBINE B AND C DATA FOR VR AND VC MODELS
    
      # 10 rows; 8 columns
    b.model.sum = rbind(VC.b.model.sum, VR.b.model.sum)
      # 10 rows; 11 columns
    c.model.sum = rbind(VC.c.model.sum, VR.c.model.sum)  
    
    # drop duplicates
      # 7 rows; 8 columns
    b.model.sum = b.model.sum[!duplicated(b.model.sum$Model), ]
      # 7 rows; 11 columns
    c.model.sum = c.model.sum[!duplicated(c.model.sum$Model), ]
    
    # sort
    b.model.sum = b.model.sum[order(b.model.sum$Model), ]
    c.model.sum = c.model.sum[order(c.model.sum$Model), ]
    
    # bye
    rm(VC.b.model.sum, VR.b.model.sum, VC.c.model.sum, VR.c.model.sum)
    
    ##############################################################################
    
    # PREPARE EMPTY DATAFRAME TO HOLD DIFFERENCES OF INTEREST
    
    # get model names
    b.model.names = b.model.sum[!grepl("NULL",  b.model.sum$Model), "Model"]
    b.null.model.names = b.model.sum[grepl("NULL",  b.model.sum$Model), "Model"]
    c.model.names = c.model.sum[!grepl("NULL",  c.model.sum$Model), "Model"]
    c.null.model.names = c.model.sum[grepl("NULL",  c.model.sum$Model), "Model"]
    
    # B models
      # 12 rows; 5 columns
    b.out = data.frame(sort(rep(b.model.names, 3)))
    names(b.out) = "Model"
    b.out$Model = as.character(b.out$Model)
    b.out$Measure = rep(c("CCR", "FPR", "FNR"), length(b.model.names))
    b.out[, b.null.model.names] = NA
    
    # C models
      # 24 rows; 5 columns
    c.out = data.frame(sort(rep(c.model.names, 6)))
    names(c.out) = "Model"
    c.out$Model = as.character(c.out$Model)
    c.out$Measure = rep(c("CCR", "FPR", "FNR", "SSD", "SSPD", "SSND"), length(c.model.names))
    c.out[, c.null.model.names] = NA

    ##############################################################################
    
    # CALCULATE DIFFERENCES OF INTEREST
    
    for (type in c("B", "C")) {
      
      # use data for either B or C models
      if (type == "B") {
        out.data = b.out
        model.sum = b.model.sum
        null.model.names = b.null.model.names
      }
      if (type == "C") {
        out.data = c.out
        model.sum = c.model.sum
        null.model.names = c.null.model.names
      }
      
      # calculate differences of interest
      for (null.model in null.model.names) {
        for (i in 1:nrow(out.data)) {
          model = out.data$Model[i]
          measure = out.data$Measure[i]
          target = model.sum[model.sum$Model == model, measure]
          null = model.sum[model.sum$Model == null.model, measure]
          out.data[i, null.model] = target - null 
        }
      }
      
      # save data for either B or C models
      if (type == "B") {
        b.out = out.data
      }
      if (type == "C") {
        c.out = out.data
      }
      
    }
    
    ##############################################################################
    
    # OUTPUT DATA
    
    # 
    write.csv(b.out, paste0(results.out.path, "/", paste(injury, toString(year), "Predictive Performance for B Models", sep = "_"), ".csv", collapse = NULL), row.names = F)
    
    write.csv(c.out, paste0(results.out.path, "/", paste(injury, toString(year), "Predictive Performance for C Models", sep = "_"), ".csv", collapse = NULL), row.names = F)
    
    write.csv(b.model.sum, paste0(results.out.path, "/", paste(injury, toString(year), "Predictive Performance for B Models - Appendix", sep = "_"), ".csv", collapse = NULL), row.names = F)
    
    write.csv(c.model.sum, paste0(results.out.path, "/", paste(injury, toString(year), "Predictive Performance for C Models - Appendix", sep = "_"), ".csv", collapse = NULL), row.names = F)
    
  }
}

################################################################################

# bye
rm(list = ls())

################################################################################
