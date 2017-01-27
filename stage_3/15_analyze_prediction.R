# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 15 - Analyze Predictions

# Coded by: Julia Bodson, juliabodson@gmail.com

# Last edit 1/25/2017

################################################################################

# define root directory
# root = "/NIOSH-Analysis"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
results.in.path = paste0(root, "/results/csv", collapse = NULL) 
results.out.path = paste0(root, "/results/csv", collapse = NULL) 
  
# inputs
  # targeting algorithm predictions
    # produced in 9_fit_models
#for (injury in c("MR", "PS")) {
#  for (var in c("VC", "VR")) {
#    for (year in 2010:2014) {
#      assign(paste(injury, var, toString(year), "predictions", "file.name", sep = "."), 
#             paste0(results.in.path, "/", paste(injury, var, toString(year), "predictions", sep = "_"), ".csv", collapse = NULL))
#    }
#  }
#}

# outputs
  # tables presenting targeting algorithm predictions
#for (injury in c("MR", "PS")) {
#  for (year in 2010:2014) {
#    assign(paste(injury, toString(year), "binary", "file.name", sep = "."), 
#           paste0(results.out.path, "/", paste(injury, toString(year), "Binary Diff Table", sep = "_"), ".csv", collapse = NULL))
#    assign(paste(injury, toString(year), "count", "file.name", sep = "."), 
#           paste0(results.out.path, "/", paste(injury, toString(year), "Count Diff Table", sep = "_"), ".csv", collapse = NULL))
#    assign(paste(injury, toString(year), "appendix.binary", "file.name", sep = "."), 
#           paste0(results.out.path, "/", paste(injury, toString(year), "Appendix Binary Diff Table", sep = "_"), ".csv", collapse = NULL))
#    assign(paste(injury, toString(year), "appendix.count", "file.name", sep = "."), 
#           paste0(results.out.path, "/", paste(injury, toString(year), "Appendix COunt Diff Table", sep = "_"), ".csv", collapse = NULL))
#  }
#}

# generate file paths
dir.create(results.out.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist) 

# bye
#rm(root, results.in.path, results.out.path)

################################################################################

calc.TP = function(data, predict.var.name, true.var.name) {
  return(sum(data[, predict.var.name] == 1 & data[, true.var.name] == 1))
}
calc.TN = function(data, predict.var.name, true.var.name) {
  return(sum(data[, predict.var.name] == 0 & data[, true.var.name] == 0))
}
calc.FP = function(data, predict.var.name, true.var.name) {
  return(sum(data[, predict.var.name] == 1 & data[, true.var.name] == 0))
}
calc.FN = function(data, predict.var.name, true.var.name) {
  return(sum(data[, predict.var.name] == 0 & data[, true.var.name] == 1))
}
calc.CCR = function(TP, TN, FP, FN) {
  return(round(100 * ((TP + TN) / (TP + TN + FP + FN)), 2))
}
calc.FPR = function(TP, TN, FP, FN) {
  return(round(100 * (FP / (FP + TN)), 2))
}
calc.FNR = function(TP, TN, FP, FN) {
  return(round(100 * (FN / (FN + TP)), 2))
}


injury = "MR"
#for (injury in c("MR", "PS")) {

  year = 2012
  #for (year in 2010:2014) {

    var = "VC"
    #for (var in c("VC", "VR")) {

      # read data
      data = read.csv(paste0(results.in.path, "/", paste(injury, var, toString(year), "predictions", sep = "_"), ".csv", collapse = NULL))
  
      # drop variables that shouldn't be in the dataset anyway
      data = data[, c(names(data)[grepl(injury, names(data))], "dv", "dv_indicator")]
      
      # group model types
      bin.data = data[, c(names(data)[grepl("B", names(data))], "dv_indicator")]
      count.data = data[, c(names(data)[grepl("C", names(data))], "dv", "dv_indicator")]
  
      # transform predictions
        # binary: FILL IN
        # count: FILL IN
      bin.data = data.frame(ifelse(bin.data <= 0.5, 0, 1))
      count.as.bin.data = data.frame(ifelse(count.data <= 0.5, 0, 1))
      count.data = data.frame(round(count.data))
      
      # rename variables
      names(bin.data) = c(paste0("B_1_", var), 
                          paste0("B_4_", var),
                          "B_NULL_1",
                          "B_NULL_2",
                          "B_NULL_3",
                          "true_indicator")
      names(count.data) = c(paste0("C_1_", var), 
                            paste0("C_4_", var),
                            "C_NULL_1",
                            "C_NULL_2",
                            "C_NULL_3",
                            "true_count", 
                            "true_indicator")
      names(count.as.bin.data) = names(count.data)
      
      # prepare summary data frame
        # binary
      bin.model.sum = data.frame(names(bin.data)[grepl("B", names(bin.data))])
      names(bin.model.sum) = "Model"
      bin.model.sum$Model = as.character(bin.model.sum$Model)
      bin.model.sum[, c("TP", "TN", "FP", "FN", "CCR", "FPR", "FNR")] = NA
    
        # count
      count.model.sum = data.frame(names(count.data)[grepl("C", names(count.data))])
      names(count.model.sum) = "Model"
      count.model.sum$Model = as.character(count.model.sum$Model)
      count.model.sum[, c("TP", "TN", "FP", "FN", "CCR", "FPR", "FNR", "SSD", "SSPD", "SSND")] = NA
      
      # fill in measures of interest
      for (type in c("bin", "count")) {
        
        if (type == "bin") {
          temp.data = bin.data
          model.sum.data = bin.model.sum
        }
        else if (type == "count") {
          temp.data = count.as.bin.data
          model.sum.data = count.model.sum
        }
        
        for (i in 1:nrow(model.sum.data)) {
          model = model.sum.data$Model[i]
          model.data = temp.data[!is.na(temp.data[, model]), c(model, "true_indicator")]
          
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
        
          if (type == "count") {
            count.model.data = count.data[!is.na(count.data[, model]), c(model, "true_count")]
            D = count.model.data[, model] - count.model.data$true_count
            PD = D[D > 0]
            ND = D[D < 0]
            model.sum.data$SSD[i] = sum(D ^ 2) 
            model.sum.data$SSPD[i] = sum(PD ^ 2)  
            model.sum.data$SSND[i] = sum(ND ^ 2)
          }
        }
      
        if (type == "bin") {
          bin.model.sum = model.sum.data
        }
        if(type == "count") {
          count.model.sum = model.sum.data
        }
      }
      
      assign(paste(var, "bin.model.sum", sep = "."), bin.model.sum)
      assign(paste(var, "count.model.sum", sep = "."), count.model.sum)
      
#    }
#  }
#}

  

  
  bin_models = names(bin_data)[grepl("B", names(bin_data)) & !grepl("NULL", names(bin_data))]
  bin_null_models = names(bin_data)[grepl("B", names(bin_data)) & grepl("NULL", names(bin_data))]
  bin_models = sort(rep(bin_models, 3))
  
  bin_out = data.frame(bin_models)
  names(bin_out) = "Model"
  bin_out$Model = as.character(bin_out$Model)
  bin_out$Measure = rep(c("CCR", "FPR", "FNR"), 4)
  bin_out[, bin_null_models] = NA
  
  for (j in 3:ncol(bin_out)) {
    null_model = names(bin_out)[j]
    for (i in 1:nrow(bin_out)) {
      model = bin_out$Model[i]
      measure = bin_out$Measure[i]
      pref = bin_model_stats[bin_model_stats$model == model, measure]
      null = bin_model_stats[bin_model_stats$model == null_model, measure]
      bin_out[i, j] = pref - null 
    }
  }
  
  count_models = names(count_data)[grepl("C", names(count_data)) & !grepl("NULL", names(count_data))]
  count_null_models = names(count_data)[grepl("C", names(count_data)) & grepl("NULL", names(count_data))]
  count_models = sort(rep(count_models, 6))
  
  count_out = data.frame(count_models)
  names(count_out) = "Model"
  count_out$Model = as.character(count_out$Model)
  count_out$Measure = rep(c("CCR", "FPR", "FNR", "SSD", "SSPD", "SSND"), 4)
  count_out[, count_null_models] = NA
  
  for (j in 3:ncol(count_out)) {
    null_model = names(count_out)[j]
    for (i in 1:nrow(count_out)) {
      model = count_out$Model[i]
      measure = count_out$Measure[i]
      pref = count_model_stats[count_model_stats$model == model, measure]
      null = count_model_stats[count_model_stats$model == null_model, measure]
      count_out[i, j] = pref - null 
    }
  }
  
  
  bin_out[, 3:ncol(bin_out)] = data.frame(round(bin_out[, 3:ncol(bin_out)], 2))
  count_out[, 3:ncol(count_out)] = data.frame(round(count_out[, 3:ncol(count_out)], 2))
  
  bin_model_stats = bin_model_stats[, c("model", "CCR", "FPR", "FNR")]
  bin_model_stats[, 2:ncol(bin_model_stats)] = data.frame(round(bin_model_stats[, 2:ncol(bin_model_stats)], 2))
  
  count_model_stats = count_model_stats[, c("model", "CCR", "FPR", "FNR", "SSD", "SSPD", "SSND")]
  count_model_stats[, 2:ncol(count_model_stats)] = data.frame(round(count_model_stats[, 2:ncol(count_model_stats)], 2))
  
  write.csv(bin_out, paste("C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/prediction tables/Binary Diff Table ", injtype, year, ".csv", sep = ""), row.names = F)
  write.csv(count_out, paste("C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/prediction tables/Count Diff Table ", injtype, year, ".csv", sep = ""), row.names = F)
  write.csv(bin_model_stats, paste("C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/prediction tables/Binary Appendix Table ", injtype, year, ".csv", sep = ""), row.names = F)
  write.csv(count_model_stats, paste("C:/Users/jbodson/Dropbox (Stanford Law School)/R-code/prediction tables/Count Appendix Table ", injtype, year, ".csv", sep = ""), row.names = F)
  

