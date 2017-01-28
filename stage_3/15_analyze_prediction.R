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
results.out.path = paste0(root, "/results/csv", collapse = NULL) 

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
      b.data = data[, c(names(data)[grepl("B", names(data))], "dv_indicator")]
      c.data = data[, c(names(data)[grepl("C", names(data))], "dv", "dv_indicator")]
    }}}
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
      b.model.sum = data.frame(names(b.data)[grepl("B", names(b.data))])
      names(b.model.sum) = "Model"
      b.model.sum$Model = as.character(b.model.sum$Model)
      b.model.sum[, c("TP", "TN", "FP", "FN", "CCR", "FPR", "FNR")] = NA
    
      # C models
      c.model.sum = data.frame(names(c.data)[grepl("C", names(c.data))])
      names(c.model.sum) = "Model"
      c.model.sum$Model = as.character(c.model.sum$Model)
      c.model.sum[, c("TP", "TN", "FP", "FN", "CCR", "FPR", "FNR", "SSD", "SSPD", "SSND")] = NA
      
      ############################################################################
      
      # FILL IN MEASURES OF INTEREST
      
      for (type in c("B", "C")) {
        
        # use the data for either B or C models
        if (type == "B") {
          data = b.data
          model.sum.data = b.model.sum
        }
        else if (type == "C") {
          data = c.as.b.data
          model.sum.data = c.model.sum
        }
        
        # fill in measures of interest
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
         i, model, measure, f, D, PD, ND, drop)
      
    }
  
    ##############################################################################

    # COMBINE B AND C DATA FOR VR AND VC MODELS
    
    b.model.sum = rbind(VC.b.model.sum, VR.b.model.sum)
    c.model.sum = rbind(VC.c.model.sum, VR.c.model.sum)  

    # drop duplicates
    b.model.sum = b.model.sum[!duplicated(b.model.sum$Model), ]
    c.model.sum = c.model.sum[!duplicated(c.model.sum$Model), ]
    
    # sort
    b.model.sum = b.model.sum[order(b.model.sum$Model), ]
    c.model.sum = c.model.sum[order(c.model.sum$Model), ]
    
    # bye
    rm(VC.b.model.sum, VR.b.model.sum, VC.c.model.sum, VR.c.model.sum, data)
    
    ##############################################################################
    
    # PREPARE EMPTY DATAFRAME TO HOLD PRESENTED RESULTS
    
    # get model names
    bin.model.names = b.model.sum[!grepl("NULL",  b.model.sum$Model), "Model"]
    bin.null.model.names = b.model.sum[grepl("NULL",  b.model.sum$Model), "Model"]
    count.model.names = c.model.sum[!grepl("NULL",  c.model.sum$Model), "Model"]
    count.null.model.names = c.model.sum[grepl("NULL",  c.model.sum$Model), "Model"]
    
    # B models
    bin.out = data.frame(sort(rep(bin.model.names, 3)))
    names(bin.out) = "Model"
    bin.out$Model = as.character(bin.out$Model)
    bin.out$Measure = rep(c("CCR", "FPR", "FNR"), length(bin.model.names))
    bin.out[, bin.null.model.names] = NA
    
    # C models
    count.out = data.frame(sort(rep(count.model.names, 6)))
    names(count.out) = "Model"
    count.out$Model = as.character(count.out$Model)
    count.out$Measure = rep(c("CCR", "FPR", "FNR", "SSD", "SSPD", "SSND"), length(count.model.names))
    count.out[, count.null.model.names] = NA
    
    ##############################################################################
    
    
    for (type in c("B", "C")) {
      
      if (type == "B") {
        out.data = bin.out
        model.sum = b.model.sum
        null.model.names = bin.null.model.names
      }
      if (type == "C") {
        out.data = count.out
        model.sum = c.model.sum
        null.model.names = count.null.model.names
      }
      
      for (null.model in null.model.names) {
        for (i in 1:nrow(out.data)) {
          model = out.data$Model[i]
          measure = out.data$Measure[i]
          target = model.sum[model.sum$Model == model, measure]
          null = model.sum[model.sum$Model == null.model, measure]
          out.data[i, null.model] = target - null 
        }
      }
      
      if (type == "B") {
        bin.out = out.data
      }
      if (type == "C") {
        count.out = out.data
      }
      
    }
    
    ##############################################################################
    
    # OUTPUT DATA
    
    write.csv(bin.out, paste0(results.out.path, "/", paste(injury, toString(year), "Binary Diff Table", sep = "_"), ".csv", collapse = NULL), row.names = F)
    write.csv(count.out, paste0(results.out.path, "/", paste(injury, toString(year), "Count Diff Table", sep = "_"), ".csv", collapse = NULL), row.names = F)
    write.csv(b.model.sum, paste0(results.out.path, "/", paste(injury, toString(year), "Appendix Binary Diff Table", sep = "_"), ".csv", collapse = NULL), row.names = F)
    write.csv(c.model.sum, paste0(results.out.path, "/", paste(injury, toString(year), "Appendix COunt Diff Table", sep = "_"), ".csv", collapse = NULL), row.names = F)
    
  }
}

################################################################################

# bye
rm(list = ls())

################################################################################
