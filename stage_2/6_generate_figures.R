# NIOSH Project 2014-N-15776
# Designing a Statistical Algorithm for Strategic Identification and Development 
# of New Mine Safety Technologies and Technological Applications

# Primary Investigator: Alison Morantz, amorantz@law.stanford.edu

# 6 - Generate Figures
  # Generates Stage 2 figures

# Coded by Julia Bodson, juliabodson@gmail.com
# Last edit 1/6/17

################################################################################

library(plyr)

################################################################################

# define root directory
# root = "/NIOSH-Analysis/data"
# root = "C:/Users/slevine2/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"
root = "C:/Users/jbodson/Dropbox (Stanford Law School)/NIOSH/NIOSH-Analysis"

# define file paths
input.path = paste0(root, "/data/5_prepped", collapse = NULL)
output.path = paste0(root, "/figures", collapse = NULL)

# inputs
  # mine-year-level dataset created in file X
data.file.name = paste0(input.path, "/prepped_mine_years.rds", collapse = NULL)

# outputs
  # figures in paper
out.path = paste0(output.path, "/Figure_", collapse = NULL)
  # figures in appendix
out.path.appendix = paste0(output.path, "/Appendix_Figure_", collapse = NULL)

# generate file paths 
dir.create(output.path, recursive = TRUE) # (recursive = TRUE creates file structure if it does not exist)

################################################################################

fig.num = 1
fig.ab = "a"

for (injury in c("MR", "PS")) { # make plots for MR and PS injuries

  # READ DATA
  
  # input data
  data = readRDS(data.file.name)
  
  if (injury == "MR") {
    data$PS = NULL
    names(data)[names(data) == "MR"] = "dv"
  }
  
  if (injury == "PS") {
    data$MR = NULL
    names(data)[names(data) == "PS"] = "dv"
    }
  
  # keep variables of interest
  data$numquarters = 
    data$operatortime = 
    data$safetycommittee = NULL
  
  # generate outcome variables
  data$dv.exp = data$dv / data$hours
  data$dv.rel = data$dv / data$total_injuries
  data[is.na(data)] = NA
  
  # bye
  data$dv = data$total_injuries = NULL
  rm(input.path, data.file.name)
  
  ##############################################################################
  
  # CALCULATE YEAR AVERAGES AND MEDIANS
  
  # averages
  data.year.avg = aggregate(data, list(data$year),
                            FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
  
  # medians
  data.year.med = aggregate(data, list(data$year),
                            FUN = function(x) median(as.numeric(x), na.rm = TRUE))
  
  ##############################################################################
  
  # PLOT YEAR AVERAGES AND MEDIANS
  
  for (dv in c("dv.exp", "dv.rel")) {
    
    for (method in c("avg", "med")) {
      print(fig.ab)
      #if (method == "avg") {
       # fig.ab = "a"
      #}
      #if (method == "med") {
      #  fig.ab = "b"
      #}
      
      # set labels
      if (dv == "dv.exp") {
        y.lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
      }
      if (dv == "dv.rel") {
        y.lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
      }
      
      if (method == "avg") {
        y.lab = paste("Mean", y.lab, sep = " ")
      }
      if (method == "med") {
        y.lab = paste("Median", y.lab, sep = " ")
      }
      
      # generate figures
      d = eval(parse(text = paste("data.year", method, sep = ".")))
      
      out.file = paste(out.path, paste0(toString(fig.num), fig.ab, collapse = NULL), ".png", sep = "")
      png(out.file)
      par(mar = c(7, 7, 4.1, 2.1), mgp = c(4, 1, 0)) # set margins
      
      plot(d$year, d[, dv], 
           xlab = "Year", ylab = y.lab,
           type = "l", cex.axis = 1.5, cex.lab = 1.5)
      
      dev.off()
      
      # update naming scheme
      if (fig.ab == "a") {
        fig.ab = "b"
      }
      else if (fig.ab == "b") {
        fig.num = fig.num + 1
        fig.ab = "a"
      }
      
    }
    
  }
  
  # bye
  rm(d, dv, method, out.file, y.lab, data.year.avg, data.year.med)
  
  ##############################################################################
  
  # GROUP MINES BY COVARIATES OF INTEREST
  
  # calculate percentile cutoffs for variables of interest
  year.info = data.frame(unique(data$year))
  names(year.info) = "year"
  for (var in c("hours", "employment", "coal_prod")) {
    year.info[, c(paste(var, seq(5, 100, 5), sep = "."))] = NA
  }
  
  for (var in c("hours", "employment", "coal_prod")) {
    for (p in seq(5, 100, 5)) {
      x = paste(var, p, sep = ".")
      for (i in 1:nrow(year.info)) {
        year = year.info$year[i]
        temp = data[data$year == year, ]
        q = quantile(temp[, var], probs = seq(0, 1, 0.01), na.rm = TRUE)[p + 1]
        year.info[i, x] = q
      }
    }
  }
  
  # bye
  rm(temp, i, p, q, var, x, year)
  
  # create dynamic groups
  cutoff.high = 80
  cutoff.low = 50
  for (var in c("hours", "employment", "coal_prod")) {
    new.var = paste(var, "dynamic", sep = ".")
    data[, new.var] = NA
    for (i in 1:nrow(data)) {
      year = data[i, "year"]
      data[i, new.var] = ifelse(data[i, var] >= year.info[year.info$year == year, paste(var, toString(cutoff.high), sep = ".")], 2, 
                                ifelse(data[i, var] < year.info[year.info$year == year, paste(var, toString(cutoff.low), sep = ".")], 0, 1))
    }
  }
  
  # bye
  rm(cutoff.high, cutoff.low, i, new.var, var, year)
  
  # create static groups
  make.static.var = function(mine.data, var) {
    static.var = paste(var, "static", sep = ".")
    dynamic.var = paste(var, "dynamic", sep = ".")
    mine.data[, static.var] = ifelse(nrow(mine.data) == sum(mine.data[, dynamic.var] == 2), 2, 
                                     ifelse(nrow(mine.data) == sum(mine.data[, dynamic.var] >= 1), 1, 0))
    return(mine.data)
  }
  
  data$hours.static = NA
  data = ddply(data, "mineid", make.static.var, var = "hours")
  
  data$employment.static = NA
  data = ddply(data, "mineid", make.static.var, var = "employment")
  
  data$coal_prod.static = NA
  data = ddply(data, "mineid", make.static.var, var = "coal_prod")
  
  # bye
  rm(make.static.var, year.info)
  
  ##############################################################################  
  
  # PLOT YEAR AVERAGES AND MEDIANS BY GROUPS OF COVARIATES OF INTEREST
  
  # format variables
  data$hours.dynamic = factor(data$hours.dynamic, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$hours.static = factor(data$hours.static, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$district = factor(data$district, levels = seq(1, 10), labels = seq(1, 10))
  data$apalachia = factor(data$apalachia, levels = c(0, 1), labels = c("Out", "In"))
  
  # collapse datasets by groups
  for (var in c("hours.dynamic", "hours.static", "district", "apalachia")) {
    for (level in unique(data[, var])) {
      assign(paste("data.year.avg", var, level, sep = "."), 
             aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                       FUN = function(x) mean(as.numeric(x), na.rm = TRUE)))
      assign(paste("data.year.med", var, level, sep = "."), 
             aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                       FUN = function(x) median(as.numeric(x), na.rm = TRUE)))
    }
  }
  
  # bye
  rm(level, var)
  
  # generate figures
  for (var in c("hours.dynamic", "hours.static", "district", "apalachia")) {
    
    # not sure why this needs to be done twice, but it really freaks out otherwise
    color = palette(rainbow(length(unique(data[, var]))))
    color = palette(rainbow(length(unique(data[, var])))) 
    
    for (dv in c("dv.exp", "dv.rel")) {
      
      for (method in c("avg", "med")) {
        
        # set labels
        if (dv == "dv.exp") {
          y.lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
        }
        if (dv == "dv.rel") {
          y.lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
        }
        
        if (method == "avg") {
          y.lab = paste("Mean", y.lab, sep = " ")
        }
        if (method == "med") {
          y.lab = paste("Median", y.lab, sep = " ")
        }
        
        # generate figures
        x = ls()[grepl(paste("data.year", method, var, sep = "."), ls())]
        n = eval(parse(text = x[1]))
        for (j in 2:length(x)) {
          n2 = eval(parse(text = x[j]))
          n = rbind(n, n2)
        }
        
        lb = min(n[, dv])
        ub = max(n[, dv])
        
        # force figure numbers to match report
          # without this change, the figures by mine size would be incorrectly ordered
        if (fig.num == 4) {
          fig.num.modified = 5
        }
        else if (fig.num == 5) {
          fig.num.modified = 4
        }
        else if (fig.num == 14) {
          fig.num.modified = 15
        }
        else if (fig.num == 15) {
          fig.num.modified = 14
        }
        else {
          fig.num.modified = fig.num
        }
        
        out.file = paste(out.path, paste0(toString(fig.num.modified), fig.ab, collapse = NULL), ".png", sep = "")
        png(out.file)
        par(mar = c(7, 7, 4.1, 2.1), mgp = c(4, 1, 0)) # set margins
        
        i = 1 # track levels to know when to create new plot vs. modify existing plot
        for (level in sort(unique(data[, var]))) {
          
          d = eval(parse(text = paste("data.year", method, var, level, sep = ".")))
          
          if (i == 1) {
            plot(d[, "year"], d[, dv], 
                 xlab = "Year", ylab = y.lab,
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
        
        # update naming scheme
        if (fig.ab == "a") {
          fig.ab = "b"
        }
        if (fig.ab == "b") {
          fig.num = fig.num + 1
          fig.ab = "a"
        }

      }
    }
  }
  
  # bye
  rm(list = ls()[grepl("data.year", ls())])
  rm(color, dv, i, j, lb, level, method, out.file, ub, var, x, y.lab, d, n, n2) 
  
  ##############################################################################
  
  # CALCULATE MINE AVERAGES ANE MEDIANS
  
  # averages
  data.mine.avg = aggregate(data, list(data$mineid),
                            FUN = function(x) mean(as.numeric(x), na.rm = TRUE))
  
  # medians
  data.mine.med = aggregate(data, list(data$mineid),
                            FUN = function(x) median(as.numeric(x), na.rm = TRUE))
  
  ##############################################################################
  
  # PLOT MINE AVERAGES AND MEDIANS
  
  for (dv in c("dv.exp", "dv.rel")) {
    
    for (method in c("avg", "med")) {
      
      # set labels
      if (dv == "dv.exp") {
        x.lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
      }
      if (dv == "dv.rel") {
        x.lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
      }
      
      if (method == "avg") {
        x.lab = paste("Mean", x.lab, sep = " ")
      }
      if (method == "med") {
        x.lab = paste("Median", x.lab, sep = " ")
      }
      
      # generate figures 
      d = eval(parse(text = paste("data.mine", method, sep = ".")))
      
      out.file = paste(out.path, paste0(toString(fig.num), fig.ab, collapse = NULL), ".png", sep = "")
      png(out.file)
      par(mar = c(7, 7, 4.1, 2.1), mgp = c(4, 1, 0)) # set margins
      
      hist(d[, dv], main = NULL, xlab = x.lab, cex.axis = 1.5, cex.lab = 1.5)
      
      dev.off()
      
      # update naming scheme
      if (fig.ab == "a") {
        fig.ab = "b"
      }
      if (fig.ab == "b") {
        fig.num = fig.num + 1
        fig.ab = "a"   
      }
      
    }
  }
  
  # bye
  rm (d, data.mine.avg, data.mine.med, dv, method, x.lab, out.file)

  ##############################################################################
  
  # MAKE APPENDIX PLOTS
  
  if (injury == "MR") {
    appendix.fig.num = 1
    appendix.fig.ab = "a"
  }
  
  if (injury == "PS") {
    appendix.fig.num = 1
    appendix.fig.ab = "a"
  }
  
  # format variables
  data$employment.dynamic = factor(data$employment.dynamic, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$employment.static = factor(data$employment.static, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$coal_prod.dynamic = factor(data$coal_prod.dynamic, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  data$coal_prod.static = factor(data$coal_prod.static, levels = c(0, 1, 2), labels = c("Small", "Medium", "Large"))
  
  # collapse datasets by groups
  for (var in c("employment.dynamic", "employment.static", "coal_prod.dynamic", "coal_prod.static")) {
    for (level in unique(data[, var])) {
      assign(paste("data.year.avg", var, level, sep = "."), 
             aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                       FUN = function(x) mean(as.numeric(x), na.rm = TRUE)))
      assign(paste("data.year.med", var, level, sep = "."), 
             aggregate(data[data[, var] == level, ], list(data[data[, var] == level, "year"]),
                       FUN = function(x) median(as.numeric(x), na.rm = TRUE)))
    }
  }
  
  # bye
  rm(level, var)
  
  # generate plots
  for (var in c("employment.dynamic", "employment.static", "coal_prod.dynamic", "coal_prod.static")) {
    
    # not sure why this needs to be done twice, but it really freaks out otherwise
    color = palette(rainbow(length(unique(data[, var]))))
    color = palette(rainbow(length(unique(data[, var])))) 
    
    for (dv in c("dv.exp", "dv.rel")) {
      
      for (method in c("avg", "med")) {
        
        # set labels
        if (dv == "dv.exp") {
          y.lab = paste("Number of", injury, "Injuries per Hour Worked", sep = " ")
        }
        if (dv == "dv.rel") {
          y.lab = paste("Share of Total Injuries that are", injury, "Injuries", sep = " ")
        }
        
        if (method == "avg") {
          y.lab = paste("Mean", y.lab, sep = " ")
        }
        if (method == "med") {
          y.lab = paste("Median", y.lab, sep = " ")
        }
        
        # generate figures
        x = ls()[grepl(paste("data.year", method, var, sep = "."), ls())]
        n = eval(parse(text = x[1]))
        for (j in 2:length(x)) {
          n2 = eval(parse(text = x[j]))
          n = rbind(n, n2)
        }
        
        lb = min(n[, dv])
        ub = max(n[, dv])
        
        # force figure numbers to match report
          # without this change, the figures by mine size would be incorrectly ordered
        if (appendix.fig.num == 2) {
          appendix.fig.num.modified = 3
        }
        else if (appendix.fig.num == 3) {
          appendix.fig.num.modified = 2
        }
        else if (appendix.fig.num == 6) {
          appendix.fig.num.modified = 7
        }
        else if (appendix.fig.num == 7) {
          appendix.fig.num.modified = 6
        }
        else {
          appendix.fig.num.modified = appendix.fig.num
        }
        
        out.file = paste(out.path, paste0(toString(appendix.fig.num.modified), fig.ab, collapse = NULL), ".png", sep = "")
        png(out.file)
        par(mar = c(7, 7, 4.1, 2.1), mgp = c(4, 1, 0)) # set margins
        
        i = 1 # track levels to know when to create new plot vs. modify existing plot
        for (level in sort(unique(data[, var]))) {
          
          d = eval(parse(text = paste("data.year", method, var, level, sep = ".")))
          
          if (i == 1) {
            plot(d[, "year"], d[, dv], 
                 xlab = "Year", ylab = y.lab,
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

        # update naming scheme
        if (appendix.fig.ab == "a") {
          appendix.fig.ab = "b"
        }
        if (appendix.fig.ab == "b") {
          appendix.fig.num = appendix.fig.num + 1
          appendix.fig.ab = "a"   
        }
        
      }
    }
  }
  
  # bye
  rm(list = ls())
  
}

######################################################################################################
