# MODEL SELECTION OF NETWORK STRENGTHS AND WEIGHTS
# COMPARING FIT OF UNIFORM, NORMAL, LOGNORMAL, EXPONENTIAL, AND PARETO DISTRIBUTIONS
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INITIALIZE MODEL SELECTION --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

sample.size <- 1000  # number of samples to take from fitted distributions

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


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INPUT NETWORKS, LOOP THROUGH METRICS ----------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
#
# inputs used for all networks (metric names and initial data frame)
metric.names <- c("Weight", "In-strength", "Out-strength")
model.selection <- data.frame(Case = character(), metric = character(), Distribution = character(), param1 = numeric(), param2 = numeric(), 
                              D = numeric(), PPP = numeric(), elpdWAIC = numeric(), pWAIC = numeric(), WAIC = numeric(), 
                              SEelpdWAIC = numeric(), SEpWAIC = numeric(), SEWAIC = numeric(), stringsAsFactors = FALSE)

# load all networks
setwd("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks")
files <- dir(pattern = "*.csv", full.names = TRUE, recursive = FALSE)
case.names <- c()
for (w in 1:length(files)) {
  case.names[w] <- substr(files[w], 3, nchar(files[w]) - 4)
}
networks.all <- lapply(files, function(x) unname(as.matrix(read.csv(x, header = FALSE))))

# don't include treatments
last.char <- substr(case.names, start = nchar(case.names), stop = nchar(case.names))
networks.all <- networks.all[last.char %in% c(as.character(0:9), "l")]
case.names <- case.names[last.char %in% c(as.character(0:9), "l")]

# loop through all networks
for (n in 1:length(networks.all)) {
  network <- networks.all[[n]]
  species <- nrow(network)
  network <- t(network)
  out.strength <- rowSums(network, na.rm = TRUE)  # calculate out-strength
  in.strength <- colSums(network, na.rm = TRUE)  # calculate in-strength
  weight <- as.vector(x = network)  # weights in vector form
  metric.list <- list(abs(weight[!is.na(weight) & weight != 0]), abs(in.strength[!is.na(in.strength) & in.strength != 0]), abs(out.strength[!is.na(out.strength) & out.strength != 0]))  # if strength is zero, this means entire row/col was NA
  if (case.names[n] == "1731_Bush_2004" | case.names[n] == "236_Jiang_2014") {
    metric.list[[1]][which(metric.list[[1]] == 0)] <- metric.list[[1]][which(metric.list[[1]] == 0)] + 0.000000001  # for weights with true zeros, Bush and Jiang
  }
  
  # loop through all metrics
  for (m in 1:length(metric.names)) {
    #
    # fit parameters
    #
    # normal fit
    normal.fit <- FitDistribution(num.observations = length(metric.list[[m]]), y.input = metric.list[[m]], parameter.inits = normal.inits, file.jags = normal)
    # lognormal fit
    lnormal.fit <- FitDistribution(num.observations = length(metric.list[[m]]), y.input = metric.list[[m]], parameter.inits = normal.inits, file.jags = lognormal)
    # exponential fit
    exponential.fit <- FitDistribution(num.observations = length(metric.list[[m]]), y.input = metric.list[[m]], parameter.inits = exponential.inits, file.jags = exponential)
    # pareto fit
    pareto.fit <- FitDistribution(num.observations = length(metric.list[[m]]), y.input = metric.list[[m]], parameter.inits = pareto.inits, file.jags = pareto)
    #
    # check convergence of parameters visually using trace plots
    #
    MCMCtrace(object = normal.fit, params = c("param1", "param2"),  pdf = TRUE, filename = paste(case.names[n], "Normal", metric.names[m], sep = "_"), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/ModelSelection", open_pdf = FALSE)
    MCMCtrace(object = lnormal.fit, params = c("param1", "param2"),  pdf = TRUE, filename = paste(case.names[n], "Lognormal", metric.names[m], sep = "_"), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/ModelSelection", open_pdf = FALSE)
    MCMCtrace(object = exponential.fit, params = c("param1"),  pdf = TRUE, filename = paste(case.names[n], "Exponential", metric.names[m], sep = "_"), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/ModelSelection", open_pdf = FALSE)
    MCMCtrace(object = pareto.fit, params = c("param1", "param2"),  pdf = TRUE, filename = paste(case.names[n], "Pareto", metric.names[m], sep = "_"), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/ModelSelection", open_pdf = FALSE)
    #
    # posterior predictive checks
    #
    posterior.predictive.normal <- PosteriorPredictiveCheck(fit = normal.fit, fit.func = SampleNormalDistribution, obs.dat = metric.list[[m]], dist.name = "Normal")
    posterior.predictive.lnormal <- PosteriorPredictiveCheck(fit = lnormal.fit, fit.func = SampleLNormDistribution, obs.dat = metric.list[[m]], dist.name = "Lognormal")
    posterior.predictive.exponential <- PosteriorPredictiveCheck(fit = exponential.fit, fit.func = SampleExponentialDistribution, obs.dat = metric.list[[m]], dist.name = "Exponential")
    posterior.predictive.pareto <- PosteriorPredictiveCheck(fit = pareto.fit, fit.func = SampleParetoDistribution, obs.dat = metric.list[[m]], dist.name = "Pareto")
    #
    # output
    # parameters and WAIC
    #
    normal.waic <- CalculateWAIC(fit = normal.fit)
    lnormal.waic <- CalculateWAIC(fit = lnormal.fit)
    exponential.waic <- CalculateWAIC(fit = exponential.fit)
    pareto.waic <- CalculateWAIC(fit = pareto.fit)
    allmodels.waic <- loo::compare(normal.waic, lnormal.waic, exponential.waic, pareto.waic)
    # store output in data frame
    norm <- data.frame(Case = case.names[n], posterior.predictive.normal, as.data.frame(t(normal.waic$estimates[, 1])), as.data.frame(t(normal.waic$estimates[, 2])), stringsAsFactors = FALSE)
    lnorm <- data.frame(Case = case.names[n], posterior.predictive.lnormal, as.data.frame(t(lnormal.waic$estimates[, 1])), as.data.frame(t(lnormal.waic$estimates[, 2])), stringsAsFactors = FALSE)
    exp <- data.frame(Case = case.names[n], posterior.predictive.exponential, as.data.frame(t(exponential.waic$estimates[, 1])), as.data.frame(t(exponential.waic$estimates[, 2])), stringsAsFactors = FALSE)
    par <- data.frame(Case = case.names[n], posterior.predictive.pareto, as.data.frame(t(pareto.waic$estimates[, 1])), as.data.frame(t(pareto.waic$estimates[, 2])), stringsAsFactors = FALSE)
    model.selection <- rbind(model.selection, norm, lnorm, exp, par)
  }
  print(paste("Model selection for network ", case.names[n], " is complete"))
} 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MANIPULATE OUTPUT AND SAVE --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# save file with parameters and fit statistics
#
colnames(model.selection) <- c("Case", "Metric", "Distribution", "param1", "param2", "D", "PPP", "elpdWAIC", "pWAIC", "WAIC", "SEelpdWAIC", "SEpWAIC", "SEWAIC")
# subset by minimum WAIC
best.fit.df.init <- data.table(model.selection)
best.fit.df <- as.data.frame(best.fit.df.init[ , ifelse(WAIC <= min(WAIC) + 2, "best fit", "other fit"), by = list(Case, Metric)])
# complete dataset
model.selection$Fit <- best.fit.df$V1
# subset is studies with 5+ species
model.selection.subset <- model.selection[which(model.selection$Case %in% c("492_Miller_1987", "805_Engel_2008", "1566_Niu_2008", "1630_Mariotte_2012", "236_Jiang_2014",
                                                                            "838_Goldberg_1991", "1479_Svenning_2008", "1198_Armas_2011", "7_Lof_2014", "9998_Kinlock_unpubl",
                                                                            "9999_Kinlock_unpubl")), ]
write.csv(x = model.selection, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/ModelSelection.csv", row.names = FALSE)
write.csv(x = model.selection.subset, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/ModelSelectionSubset.csv", row.names = FALSE)


# load file, if needed
# model.selection <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/ModelSelection.csv")

# compare observed data with fitted parameters
#
# metric 1: weight
models.weight <- model.selection[which(model.selection$Metric == "Weight"), ]  # subset by weight only
row.names(models.weight) <- 1:nrow(models.weight)  # relabel row names
weights.init <- lapply(networks.all, as.vector)  # observed weights (from networks) to compare with fitted weights
# for each weight, assign case (needed for long form data)
names.all <- c()
for(i in 1:length(weights.init)) {
  names.network <- rep(case.names[i], length(weights.init[[i]]))
  names.all <- c(names.all, names.network)
}
weights <- unlist(weights.init)
weights.all <- data.frame(Case = names.all, Type = rep("Observed", length(names.all)), Distribution = rep("Data", length(names.all)), Fit = rep("data", length(names.all)), Values = abs(weights))
# generate 1000 random samples from the posterior distibutions for each network to get fitted weights
normal.weight <- SamplePosteriorNetwork(models.init = models.weight, dist.name = "Normal", dist.func = SampleNormalDistribution)
ln.weight <- SamplePosteriorNetwork(models.init = models.weight, dist.name = "Lognormal", dist.func = SampleLNormDistribution)
exponential.weight <- SamplePosteriorNetwork(models.init = models.weight, dist.name = "Exponential", dist.func = SampleExponentialDistribution)
pareto.weight <- SamplePosteriorParetoNetwork(models = models.weight)
# combine data and relabel cases
dist.weight <- rbind(weights.all, normal.weight, ln.weight, exponential.weight, pareto.weight)
levels(dist.weight$Case) <- c("Domènech & Vilà 2008", "Costa et al. 2003", "Hedberg et al. 2005", "Sangakk. & Roberts 1985",
                              "Armas & Pugnaire 2011", "Gao et al. 2014", "Baude et al. 2011", "Gurevitch et al. 1990",
                              "Pfeifer-Meis. et al. 2008", "Pausch et al. 2013", "Svenning et al. 2008", "Saccone et al. 2010",
                              "Cuda et al. 2015", "Marty et al. 2009", "Niu & Wan 2008", "Mariotte et al. 2012", 
                              "Chacón & Muñoz 2007", "Bush & Van Auken 2004", "Weigelt et al. 2002", "Amanull. & Stewart 2013",
                              "Fortner & Weltzin 2007", "Frérot et al. 2006", "Jiang et al. 2014", "Hendriks et al. 2015",
                              "Farrer & Goldberg 2011", "Miller & Werner 1987", "Dehlin et al. 2008", "Löf et al. 2014",
                              "Engel & Weltzin 2008", "Goldberg & Landa 1991", "Kinlock unpublished (b)", "Kinlock unpublished")
dist.weight$Case <- factor(dist.weight$Case, levels = c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Bush & Van Auken 2004", "Frérot et al. 2006",
                                                        "Chacón & Muñoz 2007", "Dehlin et al. 2008", "Engel & Weltzin 2008", "Niu & Wan 2008", 
                                                        "Pfeifer-Meis. et al. 2008", "Marty et al. 2009", "Baude et al. 2011", "Mariotte et al. 2012",
                                                        "Amanull. & Stewart 2013", "Pausch et al. 2013", "Jiang et al. 2014", "Cuda et al. 2015", 
                                                        "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg & Landa 1991", "Weigelt et al. 2002",
                                                        "Costa et al. 2003", "Hedberg et al. 2005", "Fortner & Weltzin 2007", "Domènech & Vilà 2008",
                                                        "Svenning et al. 2008", "Saccone et al. 2010", "Armas & Pugnaire 2011", "Farrer & Goldberg 2011",
                                                        "Gao et al. 2014", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))
dist.weight$Distribution <- factor(dist.weight$Distribution, levels = c("Data", "Normal", "Lognormal", "Exponential", "Pareto"))
write.csv(dist.weight, "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/WeightSamples.csv")

# metric 2: in-strength
models.in.strength <- model.selection[which(model.selection$Metric == "In-strength"), ]
models.in.strength <- models.in.strength[which(models.in.strength$Case %in% c("492_Miller_1987", "805_Engel_2008", "1566_Niu_2008", "1630_Mariotte_2012", "236_Jiang_2014",
                                                   "838_Goldberg_1991", "1479_Svenning_2008", "1198_Armas_2011", "7_Lof_2014", "9998_Kinlock_unpubl",
                                                   "9999_Kinlock_unpubl")), ]
five.spp <- c(5, 11, 15, 16, 23, 26, 28, 29, 30, 31, 32)  # only include studies with at least 5 species
networks.five.spp <- networks.all[five.spp]
case.names.sub <- case.names[five.spp]
row.names(models.in.strength) <- 1:nrow(models.in.strength)
in.strength.init <- lapply(networks.five.spp, function(x) colSums(t(x), na.rm = TRUE))
names.row <- c()
for (i in 1:length(in.strength.init)) {
  test <- rep(case.names.sub[i], length(in.strength.init[[i]]))
  names.row <- c(names.row, test)
}
in.strength <- unlist(in.strength.init)
in.strength.all <- data.frame(Case = names.row, Type = rep("Observed", length(names.row)), Distribution = rep("Data", length(names.row)), Fit = rep("data", length(names.row)), Values = abs(in.strength))
normal.in.strength <- SamplePosteriorNetwork(models.init = models.in.strength, dist.name = "Normal", dist.func = SampleNormalDistribution)
lnormal.in.strength <- SamplePosteriorNetwork(models.init = models.in.strength, dist.name = "Lognormal", dist.func = SampleLNormDistribution)
exponential.in.strength <- SamplePosteriorNetwork(models.init = models.in.strength, dist.name = "Exponential", dist.func = SampleExponentialDistribution)
pareto.in.strength <- SamplePosteriorParetoNetwork(models = models.in.strength)
dist.in.strength <- rbind(in.strength.all, normal.in.strength, lnormal.in.strength, exponential.in.strength, pareto.in.strength)
levels(dist.in.strength$Case) <- c("Armas & Pugnaire 2011", "Svenning et al. 2008", "Niu & Wan 2008", "Mariotte et al. 2012", 
                          "Jiang et al. 2014", "Miller & Werner 1987", "Löf et al. 2014", "Engel & Weltzin 2008", 
                          "Goldberg & Landa 1991", "Kinlock unpublished (b)", "Kinlock unpublished")
dist.in.strength$Case <- factor(dist.in.strength$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012", "Jiang et al. 2014", 
                                                "Goldberg & Landa 1991", "Svenning et al. 2008", "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))
dist.in.strength$Distribution <- factor(dist.in.strength$Distribution, levels = c("Data", "Normal", "Lognormal", "Exponential", "Pareto"))
write.csv(dist.in.strength, "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/InStrengthSamples.csv")

# metric 3: out-strength
models.out.strength <- model.selection[which(model.selection$Metric == "Out-strength"), ]
models.out.strength <- models.out.strength[which(models.out.strength$Case %in% c("492_Miller_1987", "805_Engel_2008", "1566_Niu_2008", "1630_Mariotte_2012", "236_Jiang_2014",
                                                   "838_Goldberg_1991", "1479_Svenning_2008", "1198_Armas_2011", "7_Lof_2014", "9998_Kinlock_unpubl",
                                                   "9999_Kinlock_unpubl")), ]
row.names(models.out.strength) <- 1:nrow(models.out.strength)
out.strength.init <- lapply(networks.five.spp, function(x) colSums(t(x), na.rm = TRUE))
out.strength <- unlist(out.strength.init)
out.strength.all <- data.frame(Case = names.row, Type = rep("Observed", length(names.row)), Distribution = rep("Data", length(names.row)), Fit = rep("data", length(names.row)), Values = abs(out.strength))
normal.out.strength <- SamplePosteriorNetwork(models.init = models.out.strength, dist.name = "Normal", dist.func = SampleNormalDistribution)
lnormal.out.strength <- SamplePosteriorNetwork(models.init = models.out.strength, dist.name = "Lognormal", dist.func = SampleLNormDistribution)
exponential.out.strength <- SamplePosteriorNetwork(models.init = models.out.strength, dist.name = "Exponential", dist.func = SampleExponentialDistribution)
pareto.out.strength <- SamplePosteriorParetoNetwork(models = models.out.strength)
dist.out.strength <- rbind(out.strength.all, normal.out.strength, lnormal.out.strength, exponential.out.strength, pareto.out.strength)
levels(dist.out.strength$Case) <- c("Armas & Pugnaire 2011", "Svenning et al. 2008", "Niu & Wan 2008", "Mariotte et al. 2012", 
                          "Jiang et al. 2014", "Miller & Werner 1987", "Löf et al. 2014", "Engel & Weltzin 2008", 
                          "Goldberg & Landa 1991", "Kinlock unpublished (b)", "Kinlock unpublished")
dist.out.strength$Case <- factor(dist.out.strength$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012", "Jiang et al. 2014", 
                                                "Goldberg & Landa 1991", "Svenning et al. 2008", "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))
dist.out.strength$Distribution <- factor(dist.out.strength$Distribution, levels = c("Data", "Normal", "Lognormal", "Exponential", "Pareto"))
write.csv(dist.out.strength, "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/OutStrengthSamples.csv")


