# META-FILE FOR NETWORK META-ANALYSIS
# USE THIS FILE TO RUN ANALYSES AND SAVE OUTPUT
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD PACKAGES, INITIALIZE ---------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
library(runjags)
library(reshape2)
library(R2jags)
library(MCMCvis)
library(bPCA)
library(ggplot2)
library(cowplot)
library(plyr)
<<<<<<< HEAD
library(actuar)
library(loo)
library(data.table)
library(extrafont)
loadfonts(device = "postscript")
path <- "/home/nlkinlock/Documents/NetworkMetaAnalysis/"
#
# bayesian bootstrap parameters to adjust
||||||| 2b1eac9

# Bayesian bootstrap parameters to adjust
=======
library(actuar)
library(loo)
library(data.table)
library(extrafont)
loadfonts(device = "postscript")

# Bayesian bootstrap parameters to adjust
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# number of bootstrap iterations
R <- 10000  # 10000
# size of simulated dataset
N <- 1000  # 1000
# initiate loop
case <- 1
# number of networks
net.num <- 32
# MCMC inputs
<<<<<<< HEAD
chain.length <- 500000 # number of draws from the posterior, 500000
thinning.rate <- 10   # thinning rate, 10
num.burnin <- 50000 # number to discard for burn-in, 50000
num.chains <- 3  # number of chains, 3
# convergence thresholds, Rhat (Gelman-Rubin) and effective n
gr.threshold <- 1.1  # 1.1
neff.threshold <- 1000  # 1000
#
||||||| 2b1eac9
ni <- 200000  # number of draws from the posterior
nt <- 10    # thinning rate
nb <- 100000  # number to discard for burn-in
nc <- 3  # number of chains

=======
chain.length <- 500000 # number of draws from the posterior, 500000
thinning.rate <- 10   # thinning rate, 10
num.burnin <- 50000  # number to discard for burn-in, 50000
num.chains <- 3  # number of chains, 3
# metrics with range 0,1 fit with binomial or truncated normal?
binom.ma <- FALSE
# convergence thresholds, Rhat (Gelman-Rubin) and effective n
gr.threshold <- 1.1  # 1.1
neff.threshold <- 1000  # 1000

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# text for simulation in JAGS
# data block saved as text string
# all parameters known (from study)
# model block essentially empty
gammamodel <- "
data {
for (j in 1:treatment) {
for (i in 1:N) {
int[i, j] ~ dgamma(alpha[j], rate[j])
}
}
}
model {
fake <- 0
}
"
#
# for Gao only, Gao provided RII values, which are bounded between -1 and 1
betamodel <- "
data {
for (j in 1:n) {
for (i in 1:N) {
y[i, j] ~ dbeta(alpha[j], beta[j])
}
}
}
model {
fake <- 0
}
"
<<<<<<< HEAD
#
# load functions
source(file = paste(path, "Scripts/functions.R", sep = ""))
# load JAGS models
source(file = paste(path, "Scripts/JAGSmodels.R", sep = ""))
#
||||||| 2b1eac9

# load transitivity inputs: max and min variance for networks of a given size
var.min.s <- t(read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/var_min.csv"))
var.max.s <- t(read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/var_max.csv"))

=======
# load functions
source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/functions.R")


>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD DESCRIPTIVE DATA -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load coding table with descriptive data for each case
<<<<<<< HEAD
coding.init <- read.csv(paste(path, "Input/CodingTable.csv", sep = ""))
col.remove <- c("AbundanceRank", "TargetDensity", "NeighborDensity", "Connectance",
||||||| 2b1eac9
df.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/CodingTable.csv")
col.remove <- c("Filename", "AbundanceRank", "TargetDensity", "NeighborDensity", "Connectance",
=======
coding.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CodingTable.csv")
col.remove <- c("AbundanceRank", "TargetDensity", "NeighborDensity", "Connectance",
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
                "WateringRegime", "PotVolume", "SoilType", "Treatments", "Authors", "Journal", "Title",
                "doi", "Abstract", "OtherTreatments", "Metric", "UniqueNotes")  # unwanted columns
<<<<<<< HEAD
coding.df <- coding.init[, !(names(coding.init) %in% col.remove)] # only completed cases with RII
coding.df$UniqueID <- as.factor(coding.df$UniqueID)
coding.df$ExperimentType <- factor(coding.df$ExperimentType)
||||||| 2b1eac9
df <- df.init[, !(names(df.init) %in% col.remove)] # only completed cases with RII
df$UniqueID <- as.factor(df$UniqueID)
df$ExperimentType <- factor(df$ExperimentType)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# BOOTSTRAP -------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
=======
coding.df <- coding.init[, !(names(coding.init) %in% col.remove)] # only completed cases with RII
coding.df$UniqueID <- as.factor(coding.df$UniqueID)
coding.df$ExperimentType <- factor(coding.df$ExperimentType)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# BOOTSTRAP -------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
<<<<<<< HEAD
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# RUN BOOTSTRAP ---------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# run Bayesian bootstrap from external files and save 'NetworkMetrics' output
print(paste("Bootstrapping began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/bootstrap.R", sep = ""))
print(paste("Bootstrapping completed at ", Sys.time), sep = "")
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# RUN META-ANALYSES -----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load output and manipulate in order to run meta-analysis from external files
# studies with only a true control (k = 1) and for all studies using monoculture control (k = 2)
for (k in 1:2) {
  if (k == 1) {
    network.metrics.init <- read.csv(paste(path, "Output/NetworkMetrics_TrueCtrlOnly.csv", sep = ""), row.names = 1)
  } else if (k == 2) {
    network.metrics.init <- read.csv(paste(path, "Output/NetworkMetrics.csv", sep = ""), row.names = 1)
||||||| 2b1eac9
source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/bootstrap.R")
print(paste("All bootstrapping complete at ", Sys.time(), sep = ""))

# load output
metrics.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Output/allMetrics.csv", row.names = 1)
metrics <- data.frame(metrics.init, SD.l = metrics.init$obs - metrics.init$sd, SD.u = metrics.init$obs + metrics.init$sd)
metrics$var <- metrics$sd^2
UniqueID <- c()
# loop to extract unique ID from network name
for (z in 1:length(metrics$network)) {
  if (substring(metrics$network[z], 5, 5) == "_") {
    UniqueID[z] <- as.integer(substring(metrics$network[z], 1, 4))
  } else if (substring(metrics$network[z], 2, 2) == "_") {
    UniqueID[z] <- as.integer(substring(metrics$network[z], 1, 1))
  } else {
    UniqueID[z] <- as.integer(substring(metrics$network[z], 1, 3))
  }
}
metrics$UniqueID <- as.factor(UniqueID)
metrics <- metrics[which(metrics$metric != "r"), ]
metrics$metric <- factor(metrics$metric, levels = c("strength", "indirect_effect", "asymm_diff", "relative_intransitivity", "connectance"))
levels(metrics$metric) <- c("strength", "ind eff", "asymm", "RI", "connect")
# combine coding table and bootstrap output
merged.df <- join_all(list(metrics, df), by = "UniqueID")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INITIALIZE META-ANALYSES ----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Models
#
# normally distributed data, grand mean
ma_normal <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/MAnormal.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/MAnormal.jags")
cat("
    model {
    for (i in 1:k) {
    y[i] ~ dnorm(y.hat[i], w[i])
    w[i] <- 1 / v[i]
    y.hat[i] ~ dnorm(mu, tau)
    }
    sigma ~ dunif(0, 100)
    tau <- pow(sigma, -2)
    mu ~ dnorm(0, 1E-6)
    }", fill = TRUE)
sink()

# normally distributed data, groupwise means
ma_normal_grp <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/MAnormal_group.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/MAnormal_group.jags")
cat("
    model {
    for (i in 1:k) {
    y[i] ~ dnorm(y.hat[i], w[i])
    w[i] <- 1 / v[i]
    y.hat[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta[group[i]]
    }
    tau <- pow(sigma, -2)
    sigma ~ dunif(0, 100)

    for (j in 1:g) {
    beta[j] ~ dnorm(0, 1E-3)
    }
    }", fill = TRUE)
sink()

# binomially distributed data, grand mean
ma_binom <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/MAbinom.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/MAbinom.jags")
cat("model
    {
    for (i in 1:N) {
    counts[i] ~ dbin(p[i], n[i])
    logit(p[i]) <- p.hat[i]
    p.hat[i] ~ dnorm(mu, tau)
    }
    mu ~ dnorm(0, 1E-6)
    sigma ~ dunif(0, 100)
    tau <- pow(sigma, -2)
    }
    ")
sink()

# binomially distributed data, groupwise means
ma_binom_grp <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/MAbinom_group.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/MAbinom_group.jags")
cat("model
    {
    for (i in 1:N) {
    counts[i] ~ dbin(p[i], n[i])
    logit(p[i]) <- p.hat[i]
    p.hat[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta[group[i]]
    }
    sigma ~ dunif(0, 100)
    tau <- pow(sigma, -2)
    
    for (j in 1:g) {
    beta[j] ~ dnorm(0, 1E-3)
    }
    }
    ")
sink()

#
# Functions
#
# function to run JAGS meta-analyses given the data, initial values for parameters, and model with likelihood and priors
ma.jags.func <- function(k.input, y.input, v.input, g.input, group.input, N.input, n.input, counts.input, param.inits, par.vec, file.jags) {
  if(hasArg(g.input) & !hasArg(n.input)) {
    jags.data <- list(k = k.input, y = y.input, v = v.input, g = g.input, group = group.input)
  } else if(hasArg(n.input) & !hasArg(g.input)) {
    jags.data <- list(N = N.input, n = n.input, counts = counts.input)
  } else if(hasArg(n.input) & hasArg(g.input)) {
    jags.data <- list(N = N.input, n = n.input, counts = counts.input, g = g.input, group = group.input)
  } else {
    jags.data <- list(k = k.input, y = y.input, v = v.input)
=======
print(paste("Bootstrapping began at ", Sys.time(), sep = ""))
source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/bootstrap.R")
print(paste("Bootstrapping completed at ", Sys.time(), sep = ""))

# load output
# studies with only a true control (iteration 1) and for all studies using monoculture control (iteration 2)
for (k in 1:2) {
  if (k == 1) {
    network.metrics.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/NetworkMetrics_TrueCtrlOnly.csv", row.names = 1)
  } else if (k == 2) {
    network.metrics.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/NetworkMetrics.csv", row.names = 1)
  }
  network.metrics.df <- data.frame(network.metrics.init, BootstrapSDL = network.metrics.init$ObservedMean - network.metrics.init$BootstrapSD, 
                                   BootstrapSDU = network.metrics.init$ObservedMean + network.metrics.init$BootstrapSD)
  network.metrics.df$BootstrapVar <- network.metrics.df$BootstrapSD^2
  UniqueID <- ExtractUniqueID(network.metrics.df$Network)
  network.metrics.df$UniqueID <- as.factor(UniqueID)
  network.metrics.df$MetricName <- factor(network.metrics.df$MetricName, levels = c("MeanStrength", "IndirectEffect", "Imbalance", "RelativeIntransitivity",
                                                                            "WeightedConnectance", "WeightedCompConnectance", "WeightedFacConnectance",
                                                                            "Asymmetry"))
  post.bootstrap.df <- join_all(list(network.metrics.df, coding.df), by = "UniqueID")

  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # META-ANALYSES ---------------------------------------------------------------
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
  #
  # load JAGS models
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/JAGSmodels.R")

  if (k == 1) {
    rii.type.name <- "TrueCtrlOnly"
  } else if (k == 2) {
    rii.type.name <- "MonoCtrl"
  }
  print(paste("Normal meta-analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/metaanalysisNormal.R")
  print(paste("Normal meta-analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))
  
  if (binom.ma == TRUE) {
    print(paste("Binomial meta-analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
    source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/metaanalysisBinom.R")
    print(paste("Binomial meta-analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))
  } else if (binom.ma == FALSE) {
    print(paste("Trunc. normal meta-analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
    source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/metaanalysisTnorm.R")
    print(paste("Trunc. normal meta-analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  }
<<<<<<< HEAD
  network.metrics.df <- data.frame(network.metrics.init, 
                                    BootstrapSDL = network.metrics.init$ObservedMean - network.metrics.init$BootstrapSD, 
                                    BootstrapSDU = network.metrics.init$ObservedMean + network.metrics.init$BootstrapSD)
  network.metrics.df$BootstrapVar <- network.metrics.df$BootstrapSD^2
  UniqueID <- ExtractUniqueID(network.metrics.df$Network)
  network.metrics.df$UniqueID <- as.factor(UniqueID)
  network.metrics.df$MetricName <- factor(network.metrics.df$MetricName, 
                                          levels = c("MeanStrength", "IndirectEffect", "Imbalance", "RelativeIntransitivity",
                                                    "WeightedConnectance", "WeightedCompConnectance", "WeightedFacConnectance",
                                                    "Asymmetry"))
  post.bootstrap.df <- join_all(list(network.metrics.df, coding.df), by = "UniqueID")
  #
  if (k == 1) {
    rii.type.name <- "TrueCtrlOnly"
  } else if (k == 2) {
    rii.type.name <- "MonoCtrl"
||||||| 2b1eac9
  jags.inits <- param.inits
  jags.pars <- par.vec
  jags.fit <- jags(inits = jags.inits, n.chains = nc, model.file = file.jags, working.directory = getwd(),
                   data = jags.data, parameters.to.save = jags.pars, n.thin = nt, n.iter = ni, n.burnin = nb, DIC = TRUE)
  return(jags.fit)
}

# function to generate initial parameter values for grand mean, normally distributed parameters
ma.inits <- function() {
  list(mu = rnorm(1), sigma = runif(1))
}

# function to generate initial parameter values for groupwise means, normally distributed parameters
ma.grp.inits <- function() {
  list(beta = rnorm(length(unique(dat$Grp))), sigma = runif(1))
}

# function to extract mean, variance, confidence intervals from JAGS output, grand mean
jags.extract <- function(fit) {
  # extract means and CIs for mu (grand mean) and sigma (heterogeneity)
  mu <- grep("mu", row.names(fit$BUGSoutput$summary))
  sigma <- grep("sigma", row.names(fit$BUGSoutput$summary))
  if(length(grep("y.hat", row.names(fit$BUGSoutput$summary))) > 0) {
    y <- grep("y.hat", row.names(fit$BUGSoutput$summary))
  } else {
    y <- grep("p.hat", row.names(fit$BUGSoutput$summary))
=======

  # load output
  cases.meta.analysis <- read.csv(paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_ByNetwork_Normal_", rii.type.name, ".csv", sep = ""), row.names = 1)
  meta.analysis.normal <- read.csv(paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_Normal_", rii.type.name, ".csv", sep = ""), row.names = 1)
  if (binom.ma == TRUE) {
    cases.meta.analysis.binom <- read.csv(paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_ByNetwork_Binomial_", rii.type.name, ".csv", sep = ""), row.names = 1)
    meta.analysis.binom <- read.csv(paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_Binomial_", rii.type.name, ".csv", sep = ""), row.names = 1)
    cases.post.meta.analysis <- rbind(cases.meta.analysis, cases.meta.analysis.binom)  # meta-analysis estimates of metrics for each network
    meta.analysis.df <- rbind(meta.analysis.normal, meta.analysis.binom)
  } else if (binom.ma == FALSE) {
    cases.meta.analysis.tnorm <- read.csv(paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_ByNetwork_TNormal_", rii.type.name, ".csv", sep = ""), row.names = 1)
    meta.analysis.tnorm <- read.csv(paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_TNormal_", rii.type.name, ".csv", sep = ""), row.names = 1)
    cases.post.meta.analysis <- rbind(cases.meta.analysis, cases.meta.analysis.tnorm)
    meta.analysis.df <- rbind(meta.analysis.normal, meta.analysis.tnorm)
  }

  write.csv(x = meta.analysis.df, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteMetaAnalysisOutput_", rii.type.name, ".csv", sep = ""))
  
  # df with by-network estimates from meta-analyses paired with coding table
  cases.df <- join_all(dfs = list(cases.post.meta.analysis, network.metrics.df), by = c("Network", "MetricName"))
  if (k == 1) {
    levels(cases.df$Network) <- c("Domènech & Vilà 2008", "Costa et al. 2003", "Hedberg et al. 2005", 
                                  "Armas & Pugnaire 2011", "Gao et al. 2014", "Gurevitch et al. 1990",
                                   "Svenning et al. 2008", "Saccone et al. 2010", "Weigelt et al. 2002", 
                                  "Fortner & Weltzin 2007", "Farrer & Goldberg 2011", "Löf et al. 2014",
                                   "Goldberg & Landa 1991", "Kinlock unpublished (b)", "Kinlock unpublished")
  } else if (k == 2) {
    levels(cases.df$Network) <- c("Domènech & Vilà 2008", "Costa et al. 2003", "Hedberg et al. 2005", "Sangakk. & Roberts 1985",
                                  "Armas & Pugnaire 2011", "Baude et al. 2011", "Gurevitch et al. 1990",
                                  "Pfeifer-Meis. et al. 2008", "Pausch et al. 2013", "Svenning et al. 2008", 
                                  "Cuda et al. 2015", "Marty et al. 2009", "Niu & Wan 2008", "Mariotte et al. 2012", 
                                  "Chacón & Muñoz 2007", "Bush & Van Auken 2004", "Weigelt et al. 2002", "Amanull. & Stewart 2013",
                                  "Fortner & Weltzin 2007", "Frérot et al. 2006", "Jiang et al. 2014", "Hendriks et al. 2015",
                                  "Farrer & Goldberg 2011", "Miller & Werner 1987", "Engel & Weltzin 2008", "Goldberg & Landa 1991", "Kinlock unpublished")
  }

  # PCA of networks by network metrics
  # need wide data
  cases.pca.init <- reshape(data = cases.df[, c(4, 5, 7)], timevar = "MetricName", idvar = "Network", direction = "wide")
  if (k == 1) {
    cases.pca.init <- cases.pca.init[-12, ]
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  }
<<<<<<< HEAD
  # run normally distributed meta-analytic models from external file
  print(paste("Normal meta-analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
  source(file = paste(path, "Scripts/metaanalysisNormal.R", sep = ""))
  print(paste("Normal meta-analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))
  #
  # run truncated normally distributed meta-analytic models from external files
  print(paste("Trunc. normal meta-analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
  source(file = paste(path, "Scripts/metaanalysisTnorm.R", sep = ""))
  print(paste("Trunc. normal meta-analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))
  #
  # load meta-analysis output and combine network-level and global level output into one data frame
  cases.meta.analysis <- read.csv(paste(path, "Output/MetaAnalysis_ByNetwork_Normal_", rii.type.name, ".csv", sep = ""), row.names = 1)
  meta.analysis.normal <- read.csv(paste(path, "Output/MetaAnalysis_Normal_", rii.type.name, ".csv", sep = ""), row.names = 1)
  cases.meta.analysis.tnorm <- read.csv(paste(path, "Output/MetaAnalysis_ByNetwork_TNormal_", rii.type.name, ".csv", sep = ""), row.names = 1)
  meta.analysis.tnorm <- read.csv(paste(path, "Output/MetaAnalysis_TNormal_", rii.type.name, ".csv", sep = ""), row.names = 1)
  cases.post.meta.analysis <- rbind(cases.meta.analysis, cases.meta.analysis.tnorm)
  meta.analysis.df <- rbind(meta.analysis.normal, meta.analysis.tnorm)
  write.csv(x = meta.analysis.df, file = paste(path, "Output/CompleteMetaAnalysisOutput_", rii.type.name, ".csv", sep = ""))
  #
  # df with by-network estimates from meta-analyses paired with the coding table
  cases.df <- join_all(dfs = list(cases.post.meta.analysis, network.metrics.df), by = c("Network", "MetricName"))
  if (k == 1) {
    levels(cases.df$Network) <- c("Domènech & Vilà 2008", "Costa et al. 2003", "Hedberg et al. 2005", 
                                  "Armas & Pugnaire 2011", "Gao et al. 2014", "Gurevitch et al. 1990",
                                   "Svenning et al. 2008", "Saccone et al. 2010", "Weigelt et al. 2002", 
                                  "Fortner & Weltzin 2007", "Farrer & Goldberg 2011", "Löf et al. 2014",
                                   "Goldberg & Landa 1991", "Kinlock unpublished (b)", "Kinlock unpublished")
  } else if (k == 2) {
    levels(cases.df$Network) <- c("Domènech & Vilà 2008", "Costa et al. 2003", "Hedberg et al. 2005", "Sangakk. & Roberts 1985",
                                  "Armas & Pugnaire 2011", "Baude et al. 2011", "Gurevitch et al. 1990",
                                  "Pfeifer-Meis. et al. 2008", "Pausch et al. 2013", "Svenning et al. 2008", 
                                  "Cuda et al. 2015", "Marty et al. 2009", "Niu & Wan 2008", "Mariotte et al. 2012", 
                                  "Chacón & Muñoz 2007", "Bush & Van Auken 2004", "Weigelt et al. 2002", "Amanull. & Stewart 2013",
                                  "Fortner & Weltzin 2007", "Frérot et al. 2006", "Jiang et al. 2014", "Hendriks et al. 2015",
                                  "Farrer & Goldberg 2011", "Miller & Werner 1987", "Engel & Weltzin 2008", "Goldberg & Landa 1991",
                                  "Kinlock unpublished")
||||||| 2b1eac9
    CI.95.low <- fit$BUGSoutput$summary[c(y, mu, sigma), 3]  # 95% CI
    CI.95.high <- fit$BUGSoutput$summary[c(y, mu, sigma), 7]
    est <- fit$BUGSoutput$summary[c(y, mu, sigma), 1]  # mean
    # calculate within study heterogeneity
    Q <- sum((1 / dat[which(!is.na(dat$mean)), "var"]) * (dat[which(!is.na(dat$obs)), "obs"] - est[1])^2)
    if (metrics.ma[a] == "connect" | metrics.ma[a] == "RI") {
      CI.95.low <- exp(CI.95.low) / (1 + exp(CI.95.low))
      CI.95.high <- exp(CI.95.high) / (1 + exp(CI.95.high))
      est <- exp(est) / (1 + exp(est))
      Q <- sum((1 / dat[which(!is.na(dat$EffectSize)), "var"]) * (dat[which(!is.na(dat$EffectSize)), "EffectSize"] - est[1])^2)
=======
  names.pca <- cases.pca.init[, 1]
  net.lab <- unlist(lapply(as.character(names.pca), function(x) substring(x, first = 1, last = 4)))  # abbreviate names for biplot
  net.lab[length(net.lab) - 1] <- "Kinl b"
  cases.pca <- scale(x = cases.pca.init[, c(2, 3, 4, 5, 8)], center = TRUE, scale = TRUE)  # need to standardize because all metrics are on a different scale
  colnames(cases.pca) <- c("Facilitative (direct)", "Imbalanced", "Facilitative (indirect)", "Connected", "Intransitive")
  # using package 'bPCA', Bayesian PCA
  # priors for means are normal(0, 0.001), priors for covariance matrix are inverse wishart (iter = 500000, burn-in = 50000)
  net.pca <- sim.bPCA(data = cases.pca, n.chains = 3, n.iter = chain.length, n.burnin = num.burnin)
  V <- ncol(cases.pca)
  # extract percent of variance explained
  sims <- net.pca$BUGSoutput$sims.matrix
  sims <- sims[, 1:(V * V)]
  eigen.chains <- matrix(nrow = nrow(sims), ncol = V)
  for(l in 1:nrow(sims))
  {
    covm <- matrix(sims[l,], V, V)
    eigen.chains[l,] <- eigen(covm)$values
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  }
<<<<<<< HEAD
  complete.df <- join_all(list(cases.df, coding.df), by = "UniqueID")
  # write by-network estimate file
  write.csv(x = complete.df, file = paste(path, "Output/CompleteOutput_", rii.type.name,".csv", sep = ""))
||||||| 2b1eac9
  output <- data.frame(metric = rep(metrics.ma[a], length(c(est, Q))), network = c(as.character(dat$network), rep("All", length(c(mu, sigma, Q)))), param = c(paste("y", 1:length(y), sep = ""), "mu", "sigma", "Q"), est = c(est, Q), CI.l = c(CI.95.low, NA), CI.u = c(CI.95.high, NA))
  return(output)
}

# function to extract mean, variance, confidence intervals from JAGS output, groupwise means
jags.extract.grp <- function(fit) {
    beta <- grep("beta", row.names(fit$BUGSoutput$summary))
    sigma <- grep("sigma", row.names(fit$BUGSoutput$summary))
    CI.95.low <- fit$BUGSoutput$summary[c(beta, sigma), 3]
    CI.95.high <- fit$BUGSoutput$summary[c(beta, sigma), 7]
    est <- fit$BUGSoutput$summary[c(beta, sigma), 1]
    if (metrics.ma[a] == "connect" | metrics.ma[a] == "RI") {
      CI.95.low <- exp(CI.95.low) / (1 + exp(CI.95.low))
      CI.95.high <- exp(CI.95.high) / (1 + exp(CI.95.high))
      est <- exp(est) / (1 + exp(est))
    }
  output <- data.frame(metric = rep(metrics.ma[a], length(est)), network = rep("All", length(est)), param = c(grplabels, "sigma"), est = est, CI.l = CI.95.low, CI.u = CI.95.high)
  return(output)
=======
  exp.vars <- eigen.chains/rowSums(eigen.chains) * 100
  exp.var.sum <- summary(exp.vars)
  pc1.var <- substring(text = exp.var.sum[4, 1], first = 9, last = nchar(exp.var.sum[4, 1]) - 2)  # variance explained by PC 1
  pc2.var <- substring(text = exp.var.sum[4, 2], first = 9, last = nchar(exp.var.sum[4, 1]) - 2)  # variance explained by PC 2
  # extract loadings and scores
  scale.input <- 0.1
  covm <- matrix(net.pca$BUGSoutput$summary[1:(V^2), "mean"], V, V)
  mu <- net.pca$BUGSoutput$summary[((V^2) + 1):((V^2) + V), "mean"]
  eig <- eigen(covm)
  df.loadings <- t(t(eig$vectors) * (eig$values^scale.input))
  df.loadings <- data.frame(Variables = c("Facilitative (direct)", "Imbalanced", "Facilitative (indirect)", "Connected", "Intransitive"), PC1 = df.loadings[, 1], PC2 = df.loadings[, 2])
  centered <- scale(cases.pca, center = mu, scale = FALSE)
  df.scores <- centered  %*% eig$vectors
  df.scores <- data.frame(Networks = as.character(net.lab), PC1 = df.scores[, 1], PC2 = df.scores[, 2], PC1VarExplained = pc1.var, PC2VarExplained = pc2.var)
  write.csv(x = df.scores, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/PCAScores_", rii.type.name, ".csv", sep = ""))
  write.csv(x = df.loadings, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/PCALoadings_", rii.type.name, ".csv", sep = ""))
  
  complete.df <- join_all(list(cases.df, coding.df), by = "UniqueID")
  # write by-network estimate file
  write.csv(x = complete.df, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteOutput_", rii.type.name,".csv", sep = ""))
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# RUN OTHER ANALYSES ----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
print(paste("PCA ", rii.type.name, " began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/PCA.R", sep = ""))
print(paste("PCA ", rii.type.name, " completed at ", Sys.time(), sep = ""))
||||||| 2b1eac9


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# META-ANALYSES ---------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysisNormal.R")
print(paste("Normal meta-analysis complete at ", Sys.time(), sep = ""))
source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysisBinom.R")
print(paste("Binomial meta-analysis complete at ", Sys.time(), sep = ""))

# load output
dat.post.norm <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_networks_normal.csv", row.names = 1)
meta.norm <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_normal.csv", row.names = 1)
dat.post.binom <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_networks_binom.csv", row.names = 1)
meta.binom <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_binom.csv", row.names = 1)
dat.post <- rbind(dat.post.norm, dat.post.binom)  # meta-analysis estimates of metrics for each network
meta <- rbind(meta.norm, meta.binom)
# write complete meta-analysis file
write.csv(x = meta, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_output.csv")
# df with by-network estimates from meta-analyses paired with coding table
dat.post$est.post <- dat.post$est
dat.post$CI.l.post <- dat.post$CI.l
dat.post$CI.u.post <- dat.post$CI.u
dat.post <- dat.post[, -c(1, 2, 5, 6, 7, 8)]
forest <- join_all(dfs = list(dat.post, metrics), by = c("network", "metric"))
levels(forest$network) <- c("Domenech and Vila 2008", "Costa et al. 2003", "Hedberg et al. 2005", "Sangakkara and Roberts 1985",
                         "Armas and Pugnaire 2011", "Gao et al. 2014", "Baude et al. 2011", "Gurevitch et al. 1990",
                         "Pfeifer-Meister et al. 2008", "Pausch et al. 2013", "Svenning et al. 2008", "Saccone et al. 2010",
                         "Cuda et al. 2015", "Marty et al. 2009", "Niu and Wan 2008", "Mariotte et al. 2012", 
                         "Chacon and Munoz 2007", "Bush and Van Auken 2004", "Weigelt et al. 2002", "Amanullah 2013",
                         "Fortner and Weltzin 2007", "Frerot et al. 2006", "Jiang et al. 2014", "Hendriks et al. 2015",
                         "Farrer and Goldberg 2011", "Miller and Werner 1987", "Dehlin et al. 2008", "Lof et al. 2014",
                         "Engel and Weltzin 2008", "Goldberg and Landa 1991", "Kinlock unpublished", "Kinlock unpublished b")

# PCA of networks by network metrics
# need wide data
forest.pca.init <- reshape(data = forest[,1:3], timevar = "metric", idvar = "network", direction = "wide")
names.pca <- forest.pca.init[, 1]
net.lab <- unlist(lapply(as.character(names.pca), function(x) substring(x, first = 1, last = 4)))  # abbreviate names for biplot
net.lab[32] <- "Kinl b"
forest.pca <- scale(x = forest.pca.init[-1, 2:6], center = TRUE, scale = TRUE)  # need to standardize because all metrics are on a different scale
colnames(forest.pca) <- c("Facilitative (direct)", "Asymmetric", "Facilitative (indirect)", "Connected", "Intransitive")
# using package 'bPCA', Bayesian PVA
# priors for means are normal(0, 0.001), priors for covariance matrix are inverse wishart 
net.pca <- sim.bPCA(data = forest.pca, n.chains = 3, n.iter = 200000, n.burnin = 100000)
V <- ncol(forest.pca)
# extract percent of variance explained
sims <- net.pca$BUGSoutput$sims.matrix
sims <- sims[, 1:(V * V)]
eigen.chains <- matrix(nrow = nrow(sims), ncol = V)
for(i in 1:nrow(sims))
{
  covm <- matrix(sims[i,], V, V)
  eigen.chains[i,] <- eigen(covm)$values
}
exp.vars <- eigen.chains/rowSums(eigen.chains) * 100
exp.var.sum <- summary(exp.vars)
pc1.var <- substring(text = exp.var.sum[4, 1], first = 9, last = nchar(net.pca.eigvar$Exp.var[4, 1]) - 2)  # variance explained by PC 1
pc2.var <- substring(text = exp.var.sum[4, 2], first = 9, last = nchar(net.pca.eigvar$Exp.var[4, 1]) - 2)  # variance explained by PC 2
# extract loadings and scores
scale.input <- 0.1
covm <- matrix(net.pca$BUGSoutput$summary[1:(V^2), "mean"], V, V)
mu <- net.pca$BUGSoutput$summary[((V^2) + 1):((V^2) + V), "mean"]
eig <- eigen(covm)
df.loadings <- t(t(eig$vectors) * (eig$values^scale.input))
df.loadings <- data.frame(Variables = c("Facilitative (direct)", "Asymmetric", "Facilitative (indirect)", "Connected", "Intransitive"), PC1 = df.loadings[, 1], PC2 = df.loadings[, 2])
centered <- scale(forest.pca, center = mu, scale = FALSE)
df.scores <- centered  %*% eig$vectors
df.scores <- data.frame(Networks = as.character(net.lab[-1]), PC1 = df.scores[, 1], PC2 = df.scores[, 2])
write.csv(x = df.scores, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/pca_scores.csv")
write.csv(x = df.loadings, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/pca_loadings.csv")

final.df <- join_all(list(forest, df), by = "UniqueID")
# write by-network estimate file
write.csv(x = final.df, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/codingtable_metrics.csv")
=======

source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/treatment.R")

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae

print(paste("species position analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/sppPosition.R", sep = ""))
print(paste("species position analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))

print(paste("species characteristic analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/sppChar.R", sep = ""))
print(paste("species characteristic analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))

print(paste("model selection ", rii.type.name, " began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/modelSelection.R", sep = ""))
print(paste("model selection ", rii.type.name, " completed at ", Sys.time(), sep = ""))