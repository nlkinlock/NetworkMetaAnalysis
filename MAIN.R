# META-FILE FOR BAYESIAN BOOTSTRAP
# USE THIS FILE TO LOAD PACKAGES, INITIALIZE MODELS AND FUNCTIONS, CALL SUB-FILES, AND WRITE OUTPUT
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
library(actuar)
library(loo)
library(data.table)

# Bayesian bootstrap parameters to adjust
# number of bootstrap iterations
R <- 10000  # 10000
# size of simulated dataset
N <- 1000  # 1000
# initiate loop
case <- 1
# number of networks
net.num <- 32
# MCMC inputs
chain.length <- 500000 # number of draws from the posterior, 500000
thinning.rate <- 10   # thinning rate, 10
num.burnin <- 50000  # number to discard for burn-in, 50000
num.chains <- 3  # number of chains, 3
# metrics with range 0,1 fit with binomial or truncated normal?
binom.ma <- FALSE
# convergence thresholds, Rhat (Gelman-Rubin) and effective n
gr.threshold <- 1.1  # 1.1
neff.threshold <- 1000  # 1000

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
# load functions
source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/functions.R")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD DESCRIPTIVE DATA -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load coding table with descriptive data for each case
coding.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CodingTable.csv")
col.remove <- c("AbundanceRank", "TargetDensity", "NeighborDensity", "Connectance",
                "WateringRegime", "PotVolume", "SoilType", "Treatments", "Authors", "Journal", "Title",
                "doi", "Abstract", "OtherTreatments", "Metric", "UniqueNotes")  # unwanted columns
coding.df <- coding.init[, !(names(coding.init) %in% col.remove)] # only completed cases with RII
coding.df$UniqueID <- as.factor(coding.df$UniqueID)
coding.df$ExperimentType <- factor(coding.df$ExperimentType)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# BOOTSTRAP -------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
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
  }

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
  }
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
  }
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
  df.scores <- data.frame(Networks = as.character(net.lab), PC1 = df.scores[, 1], PC2 = df.scores[, 2])
  write.csv(x = df.scores, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/PCAScores_", rii.type.name, ".csv", sep = ""))
  write.csv(x = df.loadings, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/PCALoadings_", rii.type.name, ".csv", sep = ""))
  
  complete.df <- join_all(list(cases.df, coding.df), by = "UniqueID")
  # write by-network estimate file
  write.csv(x = complete.df, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteOutput_", rii.type.name,".csv", sep = ""))
}

source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Scripts/treatment.R")




