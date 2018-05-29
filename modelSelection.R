# MODEL SELECTION OF NETWORK STRENGTHS AND WEIGHTS
# COMPARING FIT OF UNIFORM, NORMAL, LOGNORMAL, EXPONENTIAL, AND PARETO DISTRIBUTIONS
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INITIALIZE MODEL SELECTION --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
library(actuar)
library(loo)
library(data.table)
#
# JAGS models
#
# uniform distribution 
# data ~ Uniform(min, max)
# uniform <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/uniform.jags"
# sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/uniform.jags")
# cat("
#     model {
#     for (i in 1:k) {
#     y[i] ~ dunif(param1, param2)
#     loglik[i] <- logdensity.unif(y[i], param1, param2)
#     }
#     param1 ~ dnorm(0, 1E-6)
#     param2 ~ dnorm(0, 1E-6)
#     }", fill = TRUE)
# sink()

# normal distribution
# data ~ N(mu, sigma)
normal <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/normal.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/normal.jags")
cat("
    model {
    for (i in 1:k) {
    y[i] ~ dnorm(param1, tau.param2)
    loglik[i] <- logdensity.norm(y[i], param1, tau.param2)
    }
    param1 ~ dnorm(0, 1E-6)
    param2 ~ dunif(0, 1E6)
    tau.param2 <- pow(param2, -2)
    }", fill = TRUE)
sink()

# lognormal distribution
# data ~ LN(mu, sigma)
lognormal <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/lognormal.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/lognormal.jags")
cat("
    model {
    for (i in 1:k) {
    y[i] ~ dlnorm(param1, tau.param2)
    loglik[i] <- logdensity.lnorm(y[i], param1, tau.param2)
    }
    param1 ~ dnorm(0, 1E-6)
    param2 ~ dunif(0, 1E6)
    tau.param2 <- pow(param2, -2)
    }", fill = TRUE)
sink()

# exponential distribution
# data ~ Exponential(lambda)
exponential <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/exponential.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/exponential.jags")
cat("
    model {
    for (i in 1:k) {
    y[i] ~ dexp(param1)
    loglik[i] <- logdensity.exp(y[i], param1)
    }
    param1 ~ dunif(0, 1E6)
    }", fill = TRUE)
sink()

# power law function = Pareto distribution
# data ~ Pareto(a, c)
pareto <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/pareto.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/pareto.jags")
cat("
    model {
    for (i in 1:k) {
    y[i] ~ dpar(param1, param2)
    loglik[i] <- logdensity.par(y[i], param1, param2)
    }
    param1 ~ dunif(0, 1E6)
    param2 ~ dunif(0, 1E6)
    }", fill = TRUE)
sink()

#
# model needed to sample from Pareto distribution (not in base R)
# simulate data in JAGS
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
# functions
#
# function to run JAGS given the data, initial values for parameters, and model with likelihood and priors
dist.fit.func <- function(k.input, y.input, param.inits, file.jags) {
  jags.data <- list(k = k.input, y = y.input)
  jags.inits <- param.inits
  if(is.null(jags.inits()$param2)) {
    jags.pars <- c("param1", "loglik")
  } else {
    jags.pars <- c("param1", "param2", "loglik")
  }
  jags.fit <- jags(inits = jags.inits, n.chains = nc, model.file = file.jags, working.directory = getwd(),
                   data = jags.data, parameters.to.save = jags.pars, n.thin = nt, n.iter = ni, n.burnin = nb, DIC = TRUE)
  return(jags.fit)
}

# functions to set initial values
# functions to draw random values (from a given distribution) for each parallel chain
#
# uniform initial values, min must be smaller than max
# unif.inits <- function() {
#   list(param1 = runif(n = 1, min = -5, max = min(metric.list[[m]])), param2 = runif(n = 1, min = max(metric.list[[m]]), max = 10))
# }

# normal inits, mean drawn from std. normal, sd drawn from uniform greater than zero and less than 2
norm.inits <- function() {
  list(param1 = rnorm(n = 1, mean = 0, sd = 1), param2 = runif(n = 1, min = 0, max = 2))
}

# exponential inits for rate, uniform distribution greater than zero and less than 10
exp.inits <- function() {
  list(param1 = runif(n = 1, min = 0, max = 10))
}

# pareto inits for alpha, uniform greater than 2 and less than 10, and c, uniform must be less than the minimum value of data
par.inits <- function() {
  list(param1 = runif(n = 1, min = 2, max = 10), param2 = runif(n = 1, min = 0, max = min(metric.list[[m]])))
}

# distribution-specific functions to take random samples of a certain size given parameters
# unif.fit.func <- function(samp.size, param1, param2) {
#   mapply(runif, n = samp.size, min = param1, max = param2)
# }

norm.fit.func <- function(samp.size, param1, param2) {
  mapply(rnorm, n = samp.size, mean = param1, sd = param2)
}

lnorm.fit.func <- function(samp.size, param1, param2) {
  mapply(rlnorm, n = samp.size, meanlog = param1, sdlog = param2)
}

exp.fit.func <- function(samp.size, param1) {
  mapply(rexp, n = samp.size, rate = param1)
}

par.fit.func <- function(samp.size, param1, param2) {
  mapply(rpareto, n = samp.size, shape = param1, scale = param2)
}

# function to run posterior predictive checks: comparing yrep and y (the posterior distribution and observed Distribution) 
# and comparing the medians of each distribution in bootstrap
pp.func <- function(fit, fit.func, obs.dat, dist.name) {
  if(any(fit$parameters.to.save == "param2")) {
    # visual check
    pp.predict <- fit.func(samp.size = 1000, param1 = fit$BUGSoutput$mean$param1, param2 = fit$BUGSoutput$mean$param2)
    pp <- data.frame(metric = rep(metric.names[m], length(c(obs.dat, pp.predict))), Distribution = c(rep("observed", length(obs.dat)), rep(dist.name, length(pp.predict))), values = c(obs.dat, pp.predict))
    pp.plot1 <- ggplot(data = pp, aes(x = values, group = Distribution, fill = Distribution)) + geom_density(alpha = 0.2) + 
      labs(y = "Frequency", main = paste(case.names[n], metric.names[m], "Posterior predictive check: comparing yrep and y", sep = "")) + theme_classic()
    # bootstrap of medians for random samples of distributions with fitted parameters
    # compared to median of observed data
    iterations <- nrow(fit$BUGSoutput$sims.matrix)
    pp.param1 <- sample(x = fit$BUGSoutput$sims.matrix[, "param1"], size = iterations)
    pp.param2 <- sample(x = fit$BUGSoutput$sims.matrix[, "param2"], size = iterations)
    pp.predict <- fit.func(samp.size = length(obs.dat), param1 = pp.param1, param2 = pp.param2)
    pp.median <- melt(apply(pp.predict, 2, median), value.name = "Median")
    p.pp <- length(which(pp.median > median(obs.dat))) / iterations
    pp.out <- data.frame(metric = metric.names[m], Distribution = dist.name, param1 = fit$BUGSoutput$mean$param1, param2 = fit$BUGSoutput$mean$param2, D = fit$BUGSoutput$mean$deviance, PPP = p.pp)
    pp.plot2 <- ggplot(data = pp.median, aes(x = Median)) + geom_histogram(breaks = seq(min(obs.dat), max(obs.dat), length.out = 30), color = "gray50", fill = "gray80") + 
      geom_vline(xintercept = median(obs.dat)) + annotate("text", x = 0.01, y = 150, label = paste("PPP = ", p.pp)) + labs(y = "Frequency", main = "Posterior predictive check: comparing medians") + theme_classic()
    pdf(file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Diagnostics/ModelSelection/", case.names[n], "_", metric.names[m], "_", dist.name, "_ppc", sep = ""))
    pp.plots <- plot_grid(pp.plot1, pp.plot2)
    print(pp.plots)
    dev.off()
    return(pp.out)
  } else {
    pp.predict <- fit.func(samp.size = 1000, param1 = fit$BUGSoutput$mean$param1)
    pp <- data.frame(metric = rep(metric.names[m], length(c(obs.dat, pp.predict))), Distribution = c(rep("observed", length(obs.dat)), rep(dist.name, length(pp.predict))), values = c(obs.dat, pp.predict))
    pp.plot1 <- ggplot(data = pp, aes(x = values, group = Distribution, fill = Distribution)) + geom_density(alpha = 0.2) + 
      labs(y = "Frequency", main = paste(case.names[n], metric.names[m], "Posterior predictive check: comparing yrep and y", sep = "")) + theme_classic()
    iterations <- nrow(fit$BUGSoutput$sims.matrix)
    pp.param1 <- sample(x = fit$BUGSoutput$sims.matrix[, "param1"], size = iterations)
    pp.predict <- fit.func(samp.size = length(obs.dat), param1 = pp.param1)
    pp.median <- melt(apply(pp.predict, 2, median), value.name = "Median")
    p.pp <- length(which(pp.median > median(obs.dat))) / iterations
    pp.out <- data.frame(metric = metric.names[m], Distribution = dist.name, param1 = fit$BUGSoutput$mean$param1, param2 = NA, D = fit$BUGSoutput$mean$deviance, PPP = p.pp)
    pp.plot2 <- ggplot(data = pp.median, aes(x = Median)) + geom_histogram(breaks = seq(min(obs.dat), max(obs.dat), length.out = 30), color = "gray50", fill = "gray80") + 
      geom_vline(xintercept = median(obs.dat)) + annotate("text", x = 0.01, y = 150, label = paste("PPP = ", p.pp)) + labs(y = "Frequency", main = "Posterior predictive check: comparing medians") + theme_classic()
    pdf(file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Diagnostics/ModelSelection/", case.names[n], "_", metric.names[m], "_", dist.name, "_ppc", sep = ""))
    pp.plots <- plot_grid(pp.plot1, pp.plot2)
    print(pp.plots)
    dev.off()
    return(pp.out)
  }
}

# function to calculate WAIC given JAGS fit
waic.func <- function(fit) {
  param <- fit$BUGSoutput$sims.list
  LL <- param$loglik
  WAIC <- waic(LL)
  return(WAIC)
}



# functions for plotting fitted distributions
#
samp.size <- 1000

# sample from a given distribution: provide distribution name and sampling function
# will sample from all networks that best fit that distribution and compile data in single df (for plotting)
dist.samp <- function(models.init, dist.name, dist.func) {
  models <- models.init[which(models.init$Distribution == dist.name), ]
  samp <- matrix(NA, nrow = samp.size, ncol = nrow(models))
  if(dist.name != "Exponential") {
    for (i in 1:nrow(models)) {
      row.temp <- models[i, ]
      samp.temp <- dist.func(samp.size = samp.size, param1 = row.temp$param1, param2 = row.temp$param2)
      samp[, i] <- samp.temp
    }
  } else {
    for (i in 1:nrow(models)) {
      row.temp <- models[i, ]
      samp.temp <- dist.func(samp.size = samp.size, param1 = row.temp$param1)
      samp[, i] <- samp.temp
    }
  }
  colnames(samp) <- models$Case
  samp <- melt(samp)
  df <- data.frame(Case = samp[, 2], Type = rep("Fitted", nrow(samp)), Distribution = rep(dist.name, nrow(samp)), Fit = rep(models$Fit, each = samp.size), Values = samp$value)
  return(df)
}

pareto.samp <- function(models) {
  dat <- list(alpha = models[which(models$Distribution == "Pareto"), "param1"], 
              c = models[which(models$Distribution == "Pareto"), "param2"], N = samp.size, 
              case = nrow(models[which(models$Distribution == "Pareto"), ]))
  sim <- run.jags(model = paretomodel, data = dat, monitor = c("y"), sample = 1, n.chains = 1, summarise = FALSE)
  sim <- coda::as.mcmc(sim)
  sim <- matrix(data = as.vector(sim), nrow = samp.size, ncol = nrow(models[which(models$Distribution == "Pareto"), ]))
  colnames(sim) <- models[which(models$Distribution == "Pareto"), "Case"]
  samp <- melt(sim)
  df <- data.frame(Case = samp[, 2], Type = rep("Fitted", nrow(samp)), Distribution = rep("Pareto", nrow(samp)), Fit = rep(models[which(models$Distribution == "Pareto"), "Fit"], each = samp.size), Values = samp$value)
}

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
setwd("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Complete")
files <- dir(pattern = "*.csv", full.names = TRUE)
case.names <- c()
for (w in 1:length(files)) {
  case.names[w] <- substr(files[w], 3, nchar(files[w]) - 4)
}
M.all <- lapply(files, function(x) unname(as.matrix(read.csv(x, header = FALSE))))

# don't include treatments
last.char <- substr(case.names, start = nchar(case.names), stop = nchar(case.names))
M.all <- M.all[last.char %in% c(as.character(0:9), "l")]
case.names <- case.names[last.char %in% c(as.character(0:9), "l")]

# loop through all networks
for (n in 1:length(M.all)) {
  M.obs <- M.all[[n]]
  species <- nrow(M.obs)
  M <- t(M.obs)
  s.out <- rowSums(M, na.rm = TRUE)  # calculate out-strength
  s.in <- colSums(M, na.rm = TRUE)  # calculate in-strength
  weight <- as.vector(x = M)  # weights in vector form
  metric.list <- list(abs(weight[!is.na(weight)]), abs(s.in[!is.na(s.in) & s.in != 0]), abs(s.out[!is.na(s.out) & s.out != 0]))  # if strength is zero, this means entire row/col was NA
  if (case.names[n] == "1731_Bush_2004" | case.names[n] == "236_Jiang_2014") {
    metric.list[[1]][which(metric.list[[1]] == 0)] <- metric.list[[1]][which(metric.list[[1]] == 0)] + 0.000000001  # for weights with true zeros, Bush and Jiang
  }
  
  # loop through all metrics
  for (m in 1:length(metric.names)) {
    #
    # fit parameters
    #
    # normal fit
    norm.fit <- dist.fit.func(k.input = length(metric.list[[m]]), y.input = metric.list[[m]], param.inits = norm.inits, file.jags = normal)
    # lognormal fit
    lnorm.fit <- dist.fit.func(k.input = length(metric.list[[m]]), y.input = metric.list[[m]], param.inits = norm.inits, file.jags = lognormal)
    # exponential fit
    exp.fit <- dist.fit.func(k.input = length(metric.list[[m]]), y.input = metric.list[[m]], param.inits = exp.inits, file.jags = exponential)
    # pareto fit
    par.fit <- dist.fit.func(k.input = length(metric.list[[m]]), y.input = metric.list[[m]], param.inits = par.inits, file.jags = pareto)
    #
    # check convergence of parameters visually using trace plots
    #
    MCMCtrace(object = norm.fit, params = c("param1", "param2"),  pdf = TRUE, filename = paste(case.names[n], "Normal", metric.names[m], sep = "_"), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence/ModelSelection", open_pdf = FALSE)
    MCMCtrace(object = lnorm.fit, params = c("param1", "param2"),  pdf = TRUE, filename = paste(case.names[n], "Lognormal", metric.names[m], sep = "_"), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence/ModelSelection", open_pdf = FALSE)
    MCMCtrace(object = exp.fit, params = c("param1"),  pdf = TRUE, filename = paste(case.names[n], "Exponential", metric.names[m], sep = "_"), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence/ModelSelection", open_pdf = FALSE)
    MCMCtrace(object = par.fit, params = c("param1", "param2"),  pdf = TRUE, filename = paste(case.names[n], "Pareto", metric.names[m], sep = "_"), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence/ModelSelection", open_pdf = FALSE)
    #
    # posterior predictive checks
    #
    pp.norm <- pp.func(fit = norm.fit, fit.func = norm.fit.func, obs.dat = metric.list[[m]], dist.name = "Normal")
    pp.lnorm <- pp.func(fit = lnorm.fit, fit.func = lnorm.fit.func, obs.dat = metric.list[[m]], dist.name = "Lognormal")
    pp.exp <- pp.func(fit = exp.fit, fit.func = exp.fit.func, obs.dat = metric.list[[m]], dist.name = "Exponential")
    pp.par <- pp.func(fit = par.fit, fit.func = par.fit.func, obs.dat = metric.list[[m]], dist.name = "Pareto")
    #
    # output
    # parameters and WAIC
    #
    norm.waic <- waic.func(fit = norm.fit)
    lnorm.waic <- waic.func(fit = lnorm.fit)
    exp.waic <- waic.func(fit = exp.fit)
    par.waic <- waic.func(fit = par.fit)
    allmodels.waic <- loo::compare(norm.waic, lnorm.waic, exp.waic, par.waic)
    # store output in data frame
    norm <- data.frame(Case = case.names[n], pp.norm, as.data.frame(t(norm.waic$estimates[, 1])), as.data.frame(t(norm.waic$estimates[, 2])), stringsAsFactors = FALSE)
    lnorm <- data.frame(Case = case.names[n], pp.lnorm, as.data.frame(t(lnorm.waic$estimates[, 1])), as.data.frame(t(lnorm.waic$estimates[, 2])), stringsAsFactors = FALSE)
    exp <- data.frame(Case = case.names[n], pp.exp, as.data.frame(t(exp.waic$estimates[, 1])), as.data.frame(t(exp.waic$estimates[, 2])), stringsAsFactors = FALSE)
    par <- data.frame(Case = case.names[n], pp.par, as.data.frame(t(par.waic$estimates[, 1])), as.data.frame(t(par.waic$estimates[, 2])), stringsAsFactors = FALSE)
    model.selection <- rbind(model.selection, norm, lnorm, exp, par)
  }
} 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MANIPULATE OUTPUT AND SAVE --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# save file with parameters and fit statistics
#
colnames(model.selection) <- c("Case", "Metric", "Distribution", "param1", "param2", "D", "PPP", "elpdWAIC", "pWAIC", "WAIC", "SEelpdWAIC", "SEpWAIC", "SEWAIC")
# subset by minimum WAIC
DT <- data.table(model.selection)
best.fit.df <- as.data.frame(DT[ , ifelse(WAIC <= min(WAIC) + 2, "best fit", "other fit"), by = list(Case, Metric)])
# complete dataset
model.selection$Fit <- best.fit.df$V1
model.selection.subset <- model.selection[which(model.selection$Case %in% c("492_Miller_1987", "805_Engel_2008", "1566_Niu_2008", "1630_Mariotte_2012", "236_Jiang_2014",
                                                                            "838_Goldberg_1991", "1479_Svenning_2008", "1198_Armas_2011", "7_Lof_2014", "9998_Kinlock_unpubl",
                                                                            "9999_Kinlock_unpubl")), ]
write.csv(x = model.selection, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/model_selection.csv", row.names = FALSE)
write.csv(x = model.selection.subset, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/model_selection_subset.csv", row.names = FALSE)



# load file, if needed
# model.selection <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/model_selection.csv")

# compare observed data with fitted parameters
#
# metric 1: weight
models.weight <- model.selection[which(model.selection$Metric == "Weight"), ]  # subset by weight only
row.names(models.weight) <- 1:nrow(models.weight)  # relabel row names
weights.init <- lapply(M.all, as.vector)  # observed weights (from networks) to compare with fitted weights
# for each weight, assign case (needed for long form data)
names.all <- c()
for(i in 1:length(weights.init)) {
  names.network <- rep(case.names[i], length(weights.init[[i]]))
  names.all <- c(names.all, names.network)
}
weights <- unlist(weights.init)
weights.all <- data.frame(Case = names.all, Type = rep("Observed", length(names.all)), Distribution = rep("Data", length(names.all)), Fit = rep("data", length(names.all)), Values = abs(weights))
# generate 1000 random samples from the posterior distibutions for each network to get fitted weights
norm.weight <- dist.samp(models.init = models.weight, dist.name = "Normal", dist.func = norm.fit.func)
ln.weight <- dist.samp(models.init = models.weight, dist.name = "Lognormal", dist.func = lnorm.fit.func)
exp.weight <- dist.samp(models.init = models.weight, dist.name = "Exponential", dist.func = exp.fit.func)
par.weight <- pareto.samp(models = models.weight)
# combine data and relabel cases
dist.weight <- rbind(weights.all, norm.weight, ln.weight, exp.weight, par.weight)
levels(dist.weight$Case) <- c("Domenech and Vila 2008", "Costa et al. 2003", "Hedberg et al. 2005", "Sangakkara and Roberts 1985",
                              "Armas and Pugnaire 2011", "Gao et al. 2014", "Baude et al. 2011", "Gurevitch et al. 1990",
                              "Pfeifer-Meister et al. 2008", "Pausch et al. 2013", "Svenning et al. 2008", "Saccone et al. 2010",
                              "Cuda et al. 2015", "Marty et al. 2009", "Niu and Wan 2008", "Mariotte et al. 2012", 
                              "Chacon and Munoz 2007", "Bush and Van Auken 2004", "Weigelt et al. 2002", "Amanullah 2013",
                              "Fortner and Weltzin 2007", "Frerot et al. 2006", "Jiang et al. 2014", "Hendriks et al. 2015",
                              "Farrer and Goldberg 2011", "Miller and Werner 1987", "Dehlin et al. 2008", "Lof et al. 2014",
                              "Engel and Weltzin 2008", "Goldberg and Landa 1991", "Kinlock unpublished", "Kinlock unpublished b")
dist.weight$Case <- factor(dist.weight$Case, levels = c("Sangakkara and Roberts 1985", "Miller and Werner 1987", "Bush and Van Auken 2004", "Frerot et al. 2006",
                                                        "Chacon and Munoz 2007", "Dehlin et al. 2008", "Engel and Weltzin 2008", "Niu and Wan 2008", 
                                                        "Pfeifer-Meister et al. 2008", "Marty et al. 2009", "Baude et al. 2011", "Mariotte et al. 2012",
                                                        "Amanullah 2013", "Pausch et al. 2013", "Jiang et al. 2014", "Cuda et al. 2015", 
                                                        "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg and Landa 1991", "Weigelt et al. 2002",
                                                        "Costa et al. 2003", "Hedberg et al. 2005", "Fortner and Weltzin 2007", "Domenech and Vila 2008",
                                                        "Svenning et al. 2008", "Saccone et al. 2010", "Armas and Pugnaire 2011", "Farrer and Goldberg 2011",
                                                        "Gao et al. 2014", "Lof et al. 2014", "Kinlock unpublished", "Kinlock unpublished b"))
dist.weight$Distribution <- factor(dist.weight$Distribution, levels = c("Data", "Normal", "Lognormal", "Exponential", "Pareto"))
write.csv(dist.weight, "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/weight_distsamples.csv")

# metric 2: in-strength
models.is <- model.selection[which(model.selection$Metric == "In-strength"), ]
models.is <- models.is[which(models.is$Case %in% c("492_Miller_1987", "805_Engel_2008", "1566_Niu_2008", "1630_Mariotte_2012", "236_Jiang_2014",
                                                   "838_Goldberg_1991", "1479_Svenning_2008", "1198_Armas_2011", "7_Lof_2014", "9998_Kinlock_unpubl",
                                                   "9999_Kinlock_unpubl")), ]
five.spp <- c(5, 11, 15, 16, 23, 26, 28, 29, 30, 31, 32)  # only include studies with at least 5 species
M.sub <- M.all[five.spp]
case.names.sub <- case.names[five.spp]
row.names(models.is) <- 1:nrow(models.is)
in.strength.init <- lapply(M.sub, function(x) colSums(t(x), na.rm = TRUE))
names.row <- c()
for (i in 1:length(in.strength.init)) {
  test <- rep(case.names.sub[i], length(in.strength.init[[i]]))
  names.row <- c(names.row, test)
}
in.strength <- unlist(in.strength.init)
in.strength.all <- data.frame(Case = names.row, Type = rep("Observed", length(names.row)), Distribution = rep("Data", length(names.row)), Fit = rep("data", length(names.row)), Values = abs(in.strength))
norm.is <- dist.samp(models.init = models.is, dist.name = "Normal", dist.func = norm.fit.func)
lnorm.is <- dist.samp(models.init = models.is, dist.name = "Lognormal", dist.func = lnorm.fit.func)
exp.is <- dist.samp(models.init = models.is, dist.name = "Exponential", dist.func = exp.fit.func)
par.is <- pareto.samp(models = models.is)
dist.is <- rbind(in.strength.all, norm.is, lnorm.is, exp.is, par.is)
levels(dist.is$Case) <- c("Armas and Pugnaire 2011", "Svenning et al. 2008", "Niu and Wan 2008", "Mariotte et al. 2012", 
                          "Jiang et al. 2014", "Miller and Werner 1987", "Lof et al. 2014", "Engel and Weltzin 2008", 
                          "Goldberg and Landa 1991", "Kinlock unpublished", "Kinlock unpublished b")
dist.is$Case <- factor(dist.is$Case, levels = c("Miller and Werner 1987", "Engel and Weltzin 2008", "Niu and Wan 2008", "Mariotte et al. 2012", "Jiang et al. 2014", 
                                                "Goldberg and Landa 1991", "Svenning et al. 2008", "Armas and Pugnaire 2011", "Lof et al. 2014", "Kinlock unpublished", "Kinlock unpublished b"))
dist.is$Distribution <- factor(dist.is$Distribution, levels = c("Data", "Normal", "Lognormal", "Exponential", "Pareto"))
write.csv(dist.is, "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/instr_distsamples.csv")

# metric 3: out-strength
models.os <- model.selection[which(model.selection$Metric == "Out-strength"), ]
models.os <- models.os[which(models.os$Case %in% c("492_Miller_1987", "805_Engel_2008", "1566_Niu_2008", "1630_Mariotte_2012", "236_Jiang_2014",
                                                   "838_Goldberg_1991", "1479_Svenning_2008", "1198_Armas_2011", "7_Lof_2014", "9998_Kinlock_unpubl",
                                                   "9999_Kinlock_unpubl")), ]
row.names(models.os) <- 1:nrow(models.os)
out.strength.init <- lapply(M.sub, function(x) colSums(t(x), na.rm = TRUE))
out.strength <- unlist(out.strength.init)
out.strength.all <- data.frame(Case = names.row, Type = rep("Observed", length(names.row)), Distribution = rep("Data", length(names.row)), Fit = rep("data", length(names.row)), Values = abs(out.strength))
norm.os <- dist.samp(models.init = models.os, dist.name = "Normal", dist.func = norm.fit.func)
lnorm.os <- dist.samp(models.init = models.os, dist.name = "Lognormal", dist.func = lnorm.fit.func)
exp.os <- dist.samp(models.init = models.os, dist.name = "Exponential", dist.func = exp.fit.func)
par.os <- pareto.samp(models = models.os)
dist.os <- rbind(out.strength.all, norm.os, lnorm.os, exp.os, par.os)
levels(dist.os$Case) <- c("Armas and Pugnaire 2011", "Svenning et al. 2008", "Niu and Wan 2008", "Mariotte et al. 2012", 
                          "Jiang et al. 2014", "Miller and Werner 1987", "Lof et al. 2014", "Engel and Weltzin 2008", 
                          "Goldberg and Landa 1991", "Kinlock unpublished", "Kinlock unpublished b")
dist.os$Case <- factor(dist.os$Case, levels = c("Miller and Werner 1987", "Engel and Weltzin 2008", "Niu and Wan 2008", "Mariotte et al. 2012", "Jiang et al. 2014", 
                                                "Goldberg and Landa 1991", "Svenning et al. 2008", "Armas and Pugnaire 2011", "Lof et al. 2014", "Kinlock unpublished", "Kinlock unpublished b"))
dist.os$Distribution <- factor(dist.os$Distribution, levels = c("Data", "Normal", "Lognormal", "Exponential", "Pareto"))
write.csv(dist.os, "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/outstr_distsamples.csv")


