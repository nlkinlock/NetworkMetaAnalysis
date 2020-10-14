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
library(actuar)
library(loo)
library(data.table)
library(extrafont)
loadfonts(device = "postscript")
path <- "/home/nlkinlock/Documents/NetworkMetaAnalysis/"
#
# bayesian bootstrap parameters to adjust
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
num.burnin <- 50000 # number to discard for burn-in, 50000
num.chains <- 3  # number of chains, 3
# convergence thresholds, Rhat (Gelman-Rubin) and effective n
gr.threshold <- 1.1  # 1.1
neff.threshold <- 1000  # 1000
#
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
#
# load functions
source(file = paste(path, "Scripts/functions.R", sep = ""))
# load JAGS models
source(file = paste(path, "Scripts/JAGSmodels.R", sep = ""))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD DESCRIPTIVE DATA -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load coding table with descriptive data for each case
coding.init <- read.csv(paste(path, "Input/CodingTable.csv", sep = ""))
col.remove <- c("AbundanceRank", "TargetDensity", "NeighborDensity", "Connectance",
                "WateringRegime", "PotVolume", "SoilType", "Treatments", "Authors", "Journal", "Title",
                "doi", "Abstract", "OtherTreatments", "Metric", "UniqueNotes")  # unwanted columns
coding.df <- coding.init[, !(names(coding.init) %in% col.remove)] # only completed cases with RII
coding.df$UniqueID <- as.factor(coding.df$UniqueID)
coding.df$ExperimentType <- factor(coding.df$ExperimentType)
#
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
  }
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
  }
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
  }
  complete.df <- join_all(list(cases.df, coding.df), by = "UniqueID")
  # write by-network estimate file
  write.csv(x = complete.df, file = paste(path, "Output/CompleteOutput_", rii.type.name,".csv", sep = ""))
}
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# RUN OTHER ANALYSES ----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
print(paste("PCA ", rii.type.name, " began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/PCA.R", sep = ""))
print(paste("PCA ", rii.type.name, " completed at ", Sys.time(), sep = ""))

print(paste("species position analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/sppPosition.R", sep = ""))
print(paste("species position analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))

print(paste("species characteristic analysis ", rii.type.name, " began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/sppChar.R", sep = ""))
print(paste("species characteristic analysis ", rii.type.name, " completed at ", Sys.time(), sep = ""))

print(paste("model selection ", rii.type.name, " began at ", Sys.time(), sep = ""))
source(file = paste(path, "Scripts/modelSelection.R", sep = ""))
print(paste("model selection ", rii.type.name, " completed at ", Sys.time(), sep = ""))