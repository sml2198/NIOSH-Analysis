# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 2 - Generate Figures
  # Outputs stage 2 figures

# Coded by JUlia Bodson, juliabodson@gmail.com
# Last edit 1/4/17

################################################################################

library(plyr)

################################################################################

for (injury in c("MR", "PS")) { # make plots for MR and PS injuries
  
  # SETTINGS
  
  if (injury == "MR") {
    data_file_name = "~/Dropbox (Stanford Law School)/R-code/data/MR-data.csv"
    out_directory = "~/Dropbox (Stanford Law School)/R-code/LATEX/Plots2/MR_Plot"
  }
  
  if (injury == "PS") {
    data_file_name ="~/Dropbox (Stanford Law School)/R-code/data/PS-data.csv"
    out_directory = "~/Dropbox (Stanford Law School)/R-code/LATEX/Plots2/PS_Plot"
  }
  
  plot_num = 1
  
  ##############################################################################
  
  # READ DATA
  
  # input data
  data = read.csv(data_file_name)
  
  # keep variables of interest
  dataPS = data[, c("mineid", "year", 
                  "dv", "total_injuries",
                  "hours", "coal_prod", "employment",
                  "district", "apalachia")]
  
  # generate outcome variables
  data$dv_exp = data$dv / data$hours
  data$dv_rel = data$dv / data$total_injuries
  data[is.na(data)] = NA
  
  # bye
  data$dv = NULL
  rm(data_file_name)
  
  ##############################################################################
  
  # CALCULATE YEAR AVERAGES AND MEDIANS
  
  # averages
  data_year_avg = aggregate(data, list(data$year),
                            FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
  
  # medians
  data_year_med = aggregate(data, list(data$year),
                            FUN = function(x) median(as.numeric(x), na.rm = TRUE))
  
  ##############################################################################
  
  # PLOT YEAR AVERAGES AND MEDIANS
  
  for (dv in c("dv_exp", "dv_rel")) {
    
    for (method in c("avg", "med")) {
      
      if (dv == "dv_exp") {
        y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
      }
      if (dv == "dv_rel") {
        y_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
      }
      
      if (method == "avg") {
        y_lab = paste("Mean", y_lab, sep = " ")
      }
      if (method == "med") {
        y_lab = paste("Median", y_lab, sep = " ")
      }
      
      d = eval(parse(text = paste("data_year", method, sep = "_")))
      
      out_file = paste(out_directory, plot_num, ".png", sep = "")
      png(out_file)
      par(mar = c(7, 7, 4.1, 2.1), mgp = c(4, 1, 0))  
      
      plot(d$year, d[, dv], 
           xlab = "Year", ylab = y_lab,
           type = "l", cex.axis = 1.5, cex.lab = 1.5)
      
      dev.off()
      plot_num = plot_num + 1
      
    }
  }
  
  # bye
  rm(d, dv, method, out_file, y_lab, data_year_avg, data_year_med)
  
  ##############################################################################
  
  # GROUP MINES BY COVARIATES OF INTEREST
  
  # calculate percentile cutoffs for variables of interest
  year_info = data.frame(unique(data$year))
  names(year_info) = "year"
  for (var in c("hours", "employment", "coal_prod")) {
    year_info[, c(paste(var, seq(5, 100, 5), sep = "_"))] = NA
  }
  
  for (var in c("hours", "employment", "coal_prod")) {
    for (p in seq(5, 100, 5)) {
      x = paste(var, p, sep = "_")
      for (i in 1:nrow(year_info)) {
        year = year_info$year[i]
        temp = data[data$year == year, ]
        q = quantile(temp[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[p + 1]
        year_info[i, x] = q
      }
    }
  }
  
  # bye
  rm(temp, i, p, q, var, x, year)
  
  # create dynamic groups
  cutoff_high = 80
  cutoff_low = 50
  for (var in c("hours", "employment", "coal_prod")) {
    new_var = paste(var, "dynamic", sep = "_")
    data[, new_var] = NA
    for (i in 1:nrow(data)) {
      year = data[i, "year"]
      data[i, new_var] = ifelse(data[i, var] >= year_info[year_info$year == year, paste(var, toString(cutoff_high), sep = "_")], 2, 
                                ifelse(data[i, var] < year_info[year_info$year == year, paste(var, toString(cutoff_low), sep = "_")], 0, 1))
    }
  }
  
  # bye
  rm(cutoff_high, cutoff_low, i, new_var, var, year)
  
  # create static groups
  make_static_var = function(mine_data, var) {
    static_var = paste(var, "static", sep = "_")
    dynamic_var = paste(var, "dynamic", sep = "_")
    mine_data[, static_var] = ifelse(nrow(mine_data) == sum(mine_data[, dynamic_var] == 2), 2, 
                                     ifelse(nrow(mine_data) == sum(mine_data[, dynamic_var] >= 1), 1, 0))
    return(mine_data)
  }
  
  data$hours_static = NA
  data = ddply(data, "mineid", make_static_var, var = "hours")
  
  data$employment_static = NA
  data = ddply(data, "mineid", make_static_var, var = "employment")
  
  data$coal_prod_static = NA
  data = ddply(data, "mineid", make_static_var, var = "coal_prod")
  
  # bye
  rm(make_static_var, year_info)
  
  ##############################################################################  
  
  # PLOT YEAR AVERAGES AND MEDIANS BY GROUPS OF COVARIATES OF INTEREST
  
  data$hours_dynamic = factor(data$hours_dynamic, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$hours_static = factor(data$hours_static, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$district = factor(data$district, levels = seq(1, 10), labels = seq(1, 10))
  data$apalachia = factor(data$apalachia, levels = c(0, 1), labels = c("Out", "In"))
  
  # collapse datasets by groups
  for (var in c("hours_dynamic", "hours_static", "district", "apalachia")) {
    for (level in unique(data[, var])) {
      assign(paste("data_year_avg", var, level, sep = "_"), 
             aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                       FUN = function(x) mean(as.numeric(x), na.rm = TRUE)))
      assign(paste("data_year_med", var, level, sep = "_"), 
             aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                       FUN = function(x) median(as.numeric(x), na.rm = TRUE)))
    }
  }
  
  # bye
  rm(level, var)
  
  # generate plots
  for (var in c("hours_dynamic", "hours_static", "district", "apalachia")) {
    
    # not sure why this needs to be done twice, but it really freaks out otherwise
    color = palette(rainbow(length(unique(data[, var]))))
    color = palette(rainbow(length(unique(data[, var])))) 
    
    for (dv in c("dv_exp", "dv_rel")) {
      
      for (method in c("avg", "med")) {
        
        if (dv == "dv_exp") {
          y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
        }
        if (dv == "dv_rel") {
          y_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
        }
        
        if (method == "avg") {
          y_lab = paste("Mean", y_lab, sep = " ")
        }
        if (method == "med") {
          y_lab = paste("Median", y_lab, sep = " ")
        }
        
        x = ls()[grepl(paste("data_year", method, var, sep = "_"), ls())]
        n = eval(parse(text = x[1]))
        for (j in 2:length(x)) {
          n2 = eval(parse(text = x[j]))
          n = rbind(n, n2)
        }
        
        lb = min(n[, dv])
        ub = max(n[, dv])
        
        out_file = paste(out_directory, plot_num, ".png", sep = "")
        
        if (plot_num == 7) {
          out_file = paste(out_directory, "9.png", sep = "")
        }
        if (plot_num == 8) {
          out_file = paste(out_directory, "10.png", sep = "")
        }
        if (plot_num == 9) {
          out_file = paste(out_directory, "7.png", sep = "")
        }
        if (plot_num == 10) {
          out_file = paste(out_directory, "8.png", sep = "")
        }
        
        png(out_file)
        par(mar = c(7, 7, 4.1, 2.1), mgp = c(4, 1, 0))  
        
        i = 1
        for (level in sort(unique(data[, var]))) {
          
          d = eval(parse(text = paste("data_year", method, var, level, sep = "_")))
          
          if (i == 1) {
            plot(d[, "year"], d[, dv], 
                 xlab = "Year", ylab = y_lab,
                 ylim = c(lb, ub),
                 type = "l", col = color[i], cex.axis = 1.5, cex.lab = 1.5)
          }
          else {
            lines(d[, "year"], d[, dv], col = color[i])
          }
          
          i = i + 1
          
        }
        
        legend("topright", 
               legend = sort(unique(data[, var])), 
               lty = c(rep(1, length(unique(data[, var])))), col = color)
        
        dev.off()
        plot_num = plot_num + 1
        
      }
    }
  }
  
  # bye
  rm(list = ls()[grepl("data_year", ls())])
  rm(color, dv, i, j, lb, level, method, out_file, ub, var, x, y_lab, d, n, n2) 
  
  ##############################################################################
  
  # CALCULATE MINE AVERAGES ANE MEDIANS
  
  # averages
  data_mine_avg = aggregate(data, list(data$mineid),
                            FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
  
  # medians
  data_mine_med = aggregate(data, list(data$mineid),
                            FUN = function(x) median(as.numeric(x), na.rm = TRUE))
  
  ##############################################################################
  
  # PLOT MINE AVERAGES AND MEDIANS
  
  for (dv in c("dv_exp", "dv_rel")) {
    
    for (method in c("avg", "med")) {
      
      if (dv == "dv_exp") {
        x_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
      }
      if (dv == "dv_rel") {
        x_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
      }
      
      if (method == "avg") {
        x_lab = paste("Mean", x_lab, sep = " ")
      }
      if (method == "med") {
        x_lab = paste("Median", x_lab, sep = " ")
      }
      
      d = eval(parse(text = paste("data_mine", method, sep = "_")))
      
      out_file = paste(out_directory, plot_num, ".png", sep = "")
      png(out_file)
      par(mar = c(7, 7, 4.1, 2.1), mgp = c(4, 1, 0))  
      
      
      hist(d[, dv], main = NULL, xlab = x_lab, cex.axis = 1.5, cex.lab = 1.5)
      
      dev.off()
      plot_num = plot_num + 1
      
    }
  }
  
  # bye
  rm (d, data_mine_avg, data_mine_med, dv, method, x_lab, out_file)

  ##############################################################################
  
  # MAKE APPENDIX PLOTS
  
  out_directory = paste(out_directory, "APP", sep = "_")
  plot_num = 1

  data$employment_dynamic = factor(data$employment_dynamic, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$employment_static = factor(data$employment_static, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$coal_prod_dynamic = factor(data$coal_prod_dynamic, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$coal_prod_static = factor(data$coal_prod_static, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  
  # collapse datasets by groups
  for (var in c("employment_dynamic", "employment_static", "coal_prod_dynamic", "coal_prod_static")) {
    for (level in unique(data[, var])) {
      assign(paste("data_year_avg", var, level, sep = "_"), 
             aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                       FUN = function(x) mean(as.numeric(x), na.rm = TRUE)))
      assign(paste("data_year_med", var, level, sep = "_"), 
             aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                       FUN = function(x) median(as.numeric(x), na.rm = TRUE)))
    }
  }
  
  # bye
  rm(level, var)
  
  # generate plots
  for (var in c("employment_dynamic", "employment_static", "coal_prod_dynamic", "coal_prod_static")) {
    
    # not sure why this needs to be done twice, but it really freaks out otherwise
    color = palette(rainbow(length(unique(data[, var]))))
    color = palette(rainbow(length(unique(data[, var])))) 
    
    for (dv in c("dv_exp", "dv_rel")) {
      
      for (method in c("avg", "med")) {
        
        if (dv == "dv_exp") {
          y_lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
        }
        if (dv == "dv_rel") {
          y_lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
        }
        
        if (method == "avg") {
          y_lab = paste("Mean", y_lab, sep = " ")
        }
        if (method == "med") {
          y_lab = paste("Median", y_lab, sep = " ")
        }
        
        x = ls()[grepl(paste("data_year", method, var, sep = "_"), ls())]
        n = eval(parse(text = x[1]))
        for (j in 2:length(x)) {
          n2 = eval(parse(text = x[j]))
          n = rbind(n, n2)
        }
        
        lb = min(n[, dv])
        ub = max(n[, dv])
        
        out_file = paste(out_directory, plot_num, ".png", sep = "")
        
        if (plot_num == 3) {
          out_file = paste(out_directory, "5.png", sep = "")
        }
        if (plot_num == 4) {
          out_file = paste(out_directory, "6.png", sep = "")
        }
        if (plot_num == 5) {
          out_file = paste(out_directory, "3.png", sep = "")
        }
        if (plot_num == 6) {
          out_file = paste(out_directory, "4.png", sep = "")
        }
        if (plot_num == 11) {
          out_file = paste(out_directory, "13.png", sep = "")
        }
        if (plot_num == 12) {
          out_file = paste(out_directory, "14.png", sep = "")
        }
        if (plot_num == 13) {
          out_file = paste(out_directory, "11.png", sep = "")
        }
        if (plot_num == 14) {
          out_file = paste(out_directory, "12.png", sep = "")
        }
        
        png(out_file)
        par(mar = c(7, 7, 4.1, 2.1), mgp = c(4, 1, 0))  
        
        i = 1
        for (level in sort(unique(data[, var]))) {
          
          d = eval(parse(text = paste("data_year", method, var, level, sep = "_")))
          
          if (i == 1) {
            plot(d[, "year"], d[, dv], 
                 xlab = "Year", ylab = y_lab,
                 ylim = c(lb, ub),
                 type = "l", col = color[i], cex.axis = 1.5, cex.lab = 1.5)
          }
          else {
            lines(d[, "year"], d[, dv], col = color[i])
          }
          
          i = i + 1
          
        }
        
        legend("topright", 
               legend = sort(unique(data[, var])), 
               lty = c(rep(1, length(unique(data[, var])))), col = color)
        
        dev.off()
        plot_num = plot_num + 1
        
      }
    }
  }
  
  # bye
  rm(list = ls())
  
}

######################################################################################################
