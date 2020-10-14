# MODEL SELECTION OF NETWORK STRENGTHS FOR FIVE-SPECIES NETWORKS
# COMPARING FIT OF UNIFORM, NORMAL, LOGNORMAL, EXPONENTIAL, AND PARETO DISTRIBUTIONS
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INITIALIZE MODEL SELECTION --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
sample.size <- 1000  # number of samples to take from fitted distributions
mono.ctrl.levels <- c("Armas & Pugnaire 2011", "Svenning et al. 2008", "Niu & Wan 2008", "Mariotte et al. 2012", 
                      "Jiang et al. 2014", "Miller & Werner 1987", "Engel & Weltzin 2008", "Goldberg & Landa 1991",
                      "Kinlock unpublished")
mono.ctrl.levels.ordered <- c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012", 
                              "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008", "Armas & Pugnaire 2011", 
                              "Kinlock unpublished")
true.ctrl.levels <- c("Armas & Pugnaire 2011", "Svenning et al. 2008", "Löf et al. 2014", "Goldberg & Landa 1991",
                      "Kinlock unpublished (b)", "Kinlock unpublished")
true.ctrl.levels.ordered <- c("Goldberg & Landa 1991", "Svenning et al. 2008", "Armas & Pugnaire 2011", "Löf et al. 2014",
                              "Kinlock unpublished (b)", "Kinlock unpublished")
#
# model needed to sample from Pareto distribution (not in base R)
# simulate data in JAGS, saved as text
paretomodel <- "
data {
for (j in 1:case) {
for (i in 1:N) {
y[i, j] ~ dpar(alpha[j], c[j])
}
}
}
model {
fake <- 0
}
"
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INPUT NETWORKS, LOOP THROUGH METRICS ----------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
metric.names <- c("In-strength", "Out-strength")
for (i in 1:2) {
  model.selection <- data.frame(Case = character(), metric = character(), Distribution = character(), param1 = numeric(), param2 = numeric(), 
                                D = numeric(), PPPmedian = numeric(), PPPmean = numeric(), PPPupper = numeric(), elpdWAIC = numeric(),
                                pWAIC = numeric(), WAIC = numeric(), SEelpdWAIC = numeric(), SEpWAIC = numeric(), SEWAIC = numeric(), 
                                stringsAsFactors = FALSE)
  rii.type.name <- c("Monoculture", "True control")[i]
  rii.type.file.name <- c("MonoCtrl", "TrueCtrlOnly")[i]
  # load all networks
  if (i == 1) {
    setwd(paste(path, "Output/Networks", sep = ""))
  } else if (i == 2) {
    setwd(paste(path, "Output/Networks/TrueControl/", sep = ""))
  }
  files <- dir(pattern = "*.csv", full.names = TRUE, recursive = FALSE)
  case.names <- c()
  for (w in 1:length(files)) {
    case.names[w] <- substr(files[w], 3, nchar(files[w]) - 4)
  }
  # remove networks with different abiotic conditions (treatments)
  treatment.test <- substr(case.names, nchar(case.names), nchar(case.names))
  treatment.indices <- c(grep(pattern = "[[:digit:]]", x = treatment.test), grep(pattern = "l", x = treatment.test))
  case.names <- case.names[treatment.indices]
  files <- files[treatment.indices]
  if (i == 2) {
    # indices for networks with true control (used when calculating RII in loop)
    coding.df.subset <- coding.df[coding.df$Filename %in% case.names, ]
    true.control <- coding.df.subset$Filename[which(coding.df.subset$CtrlTreatment == "True ctrl")]
    coding.df.subset <- coding.df.subset[which(coding.df.subset$CtrlTreatment == "True ctrl"), ]
    ctrl.indices <- which(case.names %in% true.control)
    case.names <- case.names[ctrl.indices]
    files <- files[ctrl.indices]
  } else {
    coding.df.subset <- coding.df[coding.df$Filename %in% case.names, ]
  }
  # order case.names and file list the same as the coding data frame
  # this is to use the coding DF to extract studies with 5 or more species
  reorder.by.coding.df <- vector(mode = "integer", length = length(case.names))
  for (x in 1:length(case.names)) {
    reorder.by.coding.df[x] <- grep(pattern = coding.df.subset$Filename[x], x = case.names)
  }
  case.names <- case.names[reorder.by.coding.df]
  files <- files[reorder.by.coding.df]
  five.spp <- which(coding.df.subset$SpeciesNumber >= 5)
  case.names <- case.names[five.spp]
  files <- files[five.spp]
  networks.all <- lapply(files, function(x) unname(as.matrix(read.csv(x, header = FALSE))))
  #
  # loop through all networks
  for (n in 1:length(networks.all)) {
    network <- networks.all[[n]]
    species <- nrow(network)
    out.strength <- colSums(network, na.rm = TRUE)  # calculate out-strength
    in.strength <- rowSums(network, na.rm = TRUE)  # calculate in-strength
    # if strength is zero, this means entire row/col was NA
    metric.list <- list(abs(in.strength[!is.na(in.strength) & in.strength != 0]), 
                        abs(out.strength[!is.na(out.strength) & out.strength != 0]))
    #
    # loop through all metrics
    for (m in 1:length(metric.names)) {
      #
      # fit parameters
      #
      # normal fit
      normal.fit <- FitDistribution(num.observations = length(metric.list[[m]]), y.input = metric.list[[m]], parameter.inits = normal.inits, 
                                    file.jags = normal)
      # lognormal fit
      lnormal.fit <- FitDistribution(num.observations = length(metric.list[[m]]), y.input = metric.list[[m]], parameter.inits = normal.inits,
                                    file.jags = lognormal)
      # exponential fit
      exponential.fit <- FitDistribution(num.observations = length(metric.list[[m]]), y.input = metric.list[[m]], 
                                            parameter.inits = exponential.inits, file.jags = exponential)
      # pareto fit
      pareto.fit <- FitDistribution(num.observations = length(metric.list[[m]]), y.input = metric.list[[m]], parameter.inits = pareto.inits, 
                                    file.jags = pareto)
      #
      # check convergence of parameters visually using trace plots
      #
      MCMCtrace(object = normal.fit, params = c("param1", "param2"),  pdf = TRUE, open_pdf = FALSE, 
                filename = paste(case.names[n], "Normal", metric.names[m], rii.type.file.name, sep = "_"), 
                wd = paste(path, "Output/Figures/Convergence/ModelSelection", sep = ""))
      MCMCtrace(object = lnormal.fit, params = c("param1", "param2"),  pdf = TRUE, open_pdf = FALSE, 
                filename = paste(case.names[n], "Lognormal", metric.names[m], rii.type.file.name, sep = "_"), 
                wd = paste(path, "Output/Figures/Convergence/ModelSelection", sep = ""))
      MCMCtrace(object = exponential.fit, params = c("param1"),  pdf = TRUE, open_pdf = FALSE, 
                filename = paste(case.names[n], "Exponential", metric.names[m], rii.type.file.name, sep = "_"), 
                wd = paste(path, "Output/Figures/Convergence/ModelSelection", sep = ""))
      MCMCtrace(object = pareto.fit, params = c("param1", "param2"),  pdf = TRUE, open_pdf = FALSE, 
                filename = paste(case.names[n], "Pareto", metric.names[m], rii.type.file.name, sep = "_"), 
                wd = paste(path, "Output/Figures/Convergence/ModelSelection", sep = ""))
      #
      # posterior predictive checks
      #
      posterior.predictive.normal <- PosteriorPredictiveCheck(fit = normal.fit, fit.func = SampleNormalDistribution, 
                                                              obs.dat = metric.list[[m]], dist.name = "Normal")
      posterior.predictive.lnormal <- PosteriorPredictiveCheck(fit = lnormal.fit, fit.func = SampleLNormDistribution, 
                                                              obs.dat = metric.list[[m]], dist.name = "Lognormal")
      posterior.predictive.exponential <- PosteriorPredictiveCheck(fit = exponential.fit, fit.func = SampleExponentialDistribution, 
                                                              obs.dat = metric.list[[m]], dist.name = "Exponential")
      posterior.predictive.pareto <- PosteriorPredictiveCheck(fit = pareto.fit, fit.func = SampleParetoDistribution, 
                                                              obs.dat = metric.list[[m]], dist.name = "Pareto")
      #
      # output
      # parameters and WAIC
      normal.waic <- CalculateWAIC(fit = normal.fit)
      lnormal.waic <- CalculateWAIC(fit = lnormal.fit)
      exponential.waic <- CalculateWAIC(fit = exponential.fit)
      pareto.waic <- CalculateWAIC(fit = pareto.fit)
      allmodels.waic <- loo::compare(normal.waic, lnormal.waic, exponential.waic, pareto.waic)
      # store output in data frame
      norm <- data.frame(Case = case.names[n], posterior.predictive.normal, as.data.frame(t(normal.waic$estimates[, 1])), 
                          as.data.frame(t(normal.waic$estimates[, 2])), stringsAsFactors = FALSE)
      lnorm <- data.frame(Case = case.names[n], posterior.predictive.lnormal, as.data.frame(t(lnormal.waic$estimates[, 1])), 
                          as.data.frame(t(lnormal.waic$estimates[, 2])), stringsAsFactors = FALSE)
      exp <- data.frame(Case = case.names[n], posterior.predictive.exponential, as.data.frame(t(exponential.waic$estimates[, 1])), 
                          as.data.frame(t(exponential.waic$estimates[, 2])), stringsAsFactors = FALSE)
      par <- data.frame(Case = case.names[n], posterior.predictive.pareto, as.data.frame(t(pareto.waic$estimates[, 1])), 
                          as.data.frame(t(pareto.waic$estimates[, 2])), stringsAsFactors = FALSE)
      model.selection <- rbind(model.selection, norm, lnorm, exp, par)
    }
    print(paste("Model selection for network ", case.names[n], " is complete"))
  } 
  #
  #
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # MANIPULATE OUTPUT AND SAVE --------------------------------------------------
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  #
  # save file with parameters and fit statistics
  colnames(model.selection) <- c("Case", "Metric", "Distribution", "param1", "param2", "D", "PPPmedian", "PPPmean", 
                                  "PPPupper", "elpdWAIC", "pWAIC", "WAIC", "SEelpdWAIC", "SEpWAIC", "SEWAIC")
  # subset by minimum WAIC
  best.fit.df.init <- data.table(model.selection)
  best.fit.df <- as.data.frame(best.fit.df.init[ , ifelse(WAIC <= min(WAIC) + 2, "best fit", "other fit"), by = list(Case, Metric)])
  # complete dataset
  model.selection$Fit <- best.fit.df$V1
  model.selection$CompetitionRelativeTo <- rii.type.name
  write.csv(x = model.selection, file = paste(path, "Output/ModelSelectionSubset_", rii.type.file.name, ".csv", sep = ""), row.names = FALSE)
  #
  # compare observed data with fitted parameters
  #
  # metric 1: in-strength
  models.in.strength <- model.selection[which(model.selection$Metric == "In-strength"), ]
  in.strength.init <- lapply(networks.all, function(x) rowSums(x, na.rm = TRUE))
  names.row <- c()
  for (j in 1:length(in.strength.init)) {
    test <- rep(case.names[j], length(in.strength.init[[j]]))
    names.row <- c(names.row, test)
  }
  in.strength <- unlist(in.strength.init)
  in.strength.all <- data.frame(Case = names.row, Type = rep("Observed", length(names.row)), Distribution = rep("Data", length(names.row)), 
                                Fit = rep("data", length(names.row)), Values = abs(in.strength))
  normal.in.strength <- SamplePosteriorNetwork(models.init = models.in.strength, dist.name = "Normal", dist.func = SampleNormalDistribution)
  lnormal.in.strength <- SamplePosteriorNetwork(models.init = models.in.strength, dist.name = "Lognormal", dist.func = SampleLNormDistribution)
  exponential.in.strength <- SamplePosteriorNetwork(models.init = models.in.strength, dist.name = "Exponential", dist.func = SampleExponentialDistribution)
  pareto.in.strength <- SamplePosteriorParetoNetwork(models = models.in.strength)
  dist.in.strength <- rbind(in.strength.all, normal.in.strength, lnormal.in.strength, exponential.in.strength, pareto.in.strength)
  if (i == 1) {
    levels(dist.in.strength$Case) <- mono.ctrl.levels
    dist.in.strength$Case <- factor(dist.in.strength$Case, levels = mono.ctrl.levels.ordered)
  } else if (i == 2) {
    levels(dist.in.strength$Case) <- true.ctrl.levels
    dist.in.strength$Case <- factor(dist.in.strength$Case, levels = true.ctrl.levels.ordered)
  }
  dist.in.strength$Distribution <- factor(dist.in.strength$Distribution, levels = c("Data", "Normal", "Lognormal", "Exponential", "Pareto"))
  write.csv(dist.in.strength, paste(path, "Output/InStrengthSamples_", rii.type.file.name, ".csv", sep = ""))
  #
  # metric 2: out-strength
  models.out.strength <- model.selection[which(model.selection$Metric == "Out-strength"), ]
  row.names(models.out.strength) <- 1:nrow(models.out.strength)
  out.strength.init <- lapply(networks.all, function(x) colSums(x, na.rm = TRUE))
  out.strength <- unlist(out.strength.init)
  out.strength.all <- data.frame(Case = names.row, Type = rep("Observed", length(names.row)), Distribution = rep("Data", length(names.row)), 
                                  Fit = rep("data", length(names.row)), Values = abs(out.strength))
  normal.out.strength <- SamplePosteriorNetwork(models.init = models.out.strength, dist.name = "Normal", dist.func = SampleNormalDistribution)
  lnormal.out.strength <- SamplePosteriorNetwork(models.init = models.out.strength, dist.name = "Lognormal", dist.func = SampleLNormDistribution)
  exponential.out.strength <- SamplePosteriorNetwork(models.init = models.out.strength, dist.name = "Exponential", dist.func = SampleExponentialDistribution)
  pareto.out.strength <- SamplePosteriorParetoNetwork(models = models.out.strength)
  dist.out.strength <- rbind(out.strength.all, normal.out.strength, lnormal.out.strength, exponential.out.strength, pareto.out.strength)
  if (i == 1) {
    levels(dist.out.strength$Case) <- mono.ctrl.levels
    dist.out.strength$Case <- factor(dist.out.strength$Case, levels = mono.ctrl.levels.ordered)
  } else if (i == 2) {
    levels(dist.out.strength$Case) <- true.ctrl.levels
    dist.out.strength$Case <- factor(dist.out.strength$Case, levels = true.ctrl.levels.ordered)
  }
  dist.out.strength$Distribution <- factor(dist.out.strength$Distribution, levels = c("Data", "Normal", "Lognormal", "Exponential", "Pareto"))
  write.csv(dist.out.strength, paste(path, "Output/OutStrengthSamples_", rii.type.file.name, ".csv", sep = ""))
}

