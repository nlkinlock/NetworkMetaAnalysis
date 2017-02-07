library(igraph)
library(fitdistrplus)
# library(poweRlaw)
library(MuMIn)
library(abind)
library(reshape2)
library(plyr)


#
# LOAD DATA ------------------------------------------------------------
#
M.init <- read.csv("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl/Kinlock-2016-RII.csv", header = FALSE)
M.sd.init <- read.csv("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl/Kinlock-2016-RIIsd.csv", header = FALSE)

#
# INITIALIZE LOOP ------------------------------------------------------------
#
# empty data frames to store output for metrics
# general metrics separate from distribution fit metrics
metric.names <- c("strength", "comp_strength", "fac_strength", "weight", "connectance", "linkage_diversity", "r", "indirect_effect", "relative_intransitivity")
meta.dat.mean <- matrix(NA, nrow = 1, ncol = length(metric.names))
meta.dat.sd <- matrix(NA, nrow = 1, ncol = length(metric.names))
colnames(meta.dat.mean) <- metric.names
colnames(meta.dat.sd) <- metric.names
aicc.meta <- data.frame(Metric = character(0), Distribution = character(0), freq = integer(0), Frequency = numeric(0), Case = character(0))

# set number of bootstrap iterations
iterations <- 10

setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis")

#
# LOOP ------------------------------------------------------------
#

  # initialize temp vectors to store output for a given network
  s.store <- numeric(length = iterations)
  s.c.store <- numeric(length = iterations)
  s.f.store <- numeric(length = iterations)
  weight.store <- numeric(length = iterations)
  C.qw.store <- numeric(length = iterations)
  LD.qw.store <- numeric(length = iterations)
  r.intrans.store <- numeric(length = iterations)
  r.store <- numeric(length = iterations)
  ind.eff.store <- numeric(length = iterations)
  aicc.store <- data.frame(Metric = character(0), Distribution = character(0), AICc = numeric(0), Iteration = integer(0))
  for (count in 1:iterations) {
    # nested loop for bootstrap
    # incorporate variability using sd of network elements
    mean.matrix <- t(M.init)
    sd.matrix <- t(M.sd.init)
    if (any(sd.matrix == 0)) {
      sd.matrix <- sd.matrix + 1E-9
    }
    M <- matrix(mapply(rnorm, 1, mean.matrix, sd.matrix), nrow = nrow(mean.matrix), ncol = ncol(mean.matrix))
    M[which(abs(M) <= 1E-9, arr.ind = TRUE)] <- 0
    # node characteristics (weight, in/out strength) and distribution fits
    source(file = "topology.R")
    s.store[count] <- mean.s
    s.c.store[count] <- mean.s.c
    s.f.store[count] <- mean.s.f
    weight.store[count] <- mean.weight
    aicc.store <- rbind(aicc.store, all.aicc)
    # connectance and linkage density
    source(file = "connectance.R")
    C.qw.store[count] <- C.qw
    LD.qw.store[count] <- LD.qw
    # relative intransitivity
    source(file = "transitivity.R")
    r.intrans.store[count] <- r.intrans
    # asymmetry (correlation coefficient r)
    source(file = "asymmetry.R")
    r.store[count] <- r
    # indirect effect
    source(file = "indirecteffect.R")
    ind.eff.store[count] <- mean.ind.eff
  }
  # add output from one network to data frame with all networks
  meta.dat.table <- data.frame(s.store, s.c.store, s.f.store, weight.store, C.qw.store, LD.qw.store, r.store, ind.eff.store, r.intrans.store)
  # fit bootstrapped distribution of each metric to a normal to get mu and sigma
  fit <- apply(meta.dat.table, 2, function(x) fitdist(data = x, distr = "norm", method = "mme"))
  mean.vec <- numeric(length = length(fit))
  sd.vec <- numeric(length = length(fit))
  for (w in 1:length(fit)) {
    mean.vec[w] <- unname(fit[[w]][[1]][1])
    sd.vec[w] <- unname(fit[[w]][[1]][2])
  }
  meta.dat.mean <- mean.vec
  meta.dat.sd <- sd.vec
  names(meta.dat.mean) <- metric.names
  names(meta.dat.sd) <- metric.names

  
  # hist(s.out.store)
  # hist(s.in.store)
  # hist(s.in.c.store)
  # hist(s.out.c.store)
  # hist(s.in.f.store)
  # hist(s.out.f.store)
  # hist(weight.store)
  # hist(C.qw.store)
  # hist(LD.qw.store)
  # hist(r.intrans.store)
  # hist(r.store)
  # hist(ind.eff.store)
  # hist(ratio.ind.eff.store)
  
  # convert output from distribution fits (in list)
  
  aicc.store$Metric <- factor(aicc.store$Metric, levels = c("weight", "s", "s.in.c", "s.in.f", "s.out", "s.out.c", "s.out.f"))
  aicc.store$Distribution <- factor(aicc.store$Distribution, levels = c("Uniform", "Normal", "Lognormal", "Exponential", "PowerLaw"))
  aicc.output <- count(aicc.store, c("Metric", "Distribution"))
  aicc.output$Frequency <- aicc.output$freq / iterations
  aicc.output$Case <- rep(files.names, nrow(aicc.output))
  colnames(aicc.output) <- c("Metric", "Distribution", "freq", "Frequency", "Case")
  aicc.output



#
# SAVE OUTPUT ------------------------------------------------------------
#


# write.csv(x = meta.dat.mean, file = "meansRIIKinlock.csv")
# write.csv(x = meta.dat.sd, file = "sdsRIIKinlock.csv")
# write.csv(x = aicc.meta, file = "distributionsRIIKinlock.csv")





