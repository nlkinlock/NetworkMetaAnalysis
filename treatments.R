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


# using empirical data
# RII MixMono
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/MixMono/Treatments/")
files <- dir(pattern = "*.csv", full.names = TRUE)
files.mean <- grep(pattern = "RIIsd", files, value = TRUE, invert = TRUE)
files.se <- grep(pattern = "RIIsd", files, value = TRUE)
M.treat <- lapply(files.mean, function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.treat <- lapply(files.se, function(x) unname(as.matrix(read.csv(x, header = FALSE))))
# RII Cntrl
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl/Treatments/")
files2 <- dir(pattern = "*.csv", full.names = TRUE, recursive = TRUE)
files.mean2 <- grep(pattern = "RIIsd", files2, value = TRUE, invert = TRUE)
files.se2 <- grep(pattern = "RIIsd", files2, value = TRUE)
M.treat2 <- lapply(files.mean2, function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.treat2 <- lapply(files.se2, function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.treat <- append(M.treat, M.treat2)
M.sd.treat <- append(M.sd.treat, M.sd.treat2)
files.tot <- c(files.mean, files.mean2)
files.names <- c()
for (w in 1:length(files.tot)) {
  files.names[w] <- substr(files.tot[w], 3, nchar(files.tot[w]) - 4)
}
names(M.treat) <- files.names
names(M.sd.treat) <- files.names
for (r in 1:length(M.treat)) {
  M.treat[[r]][which(is.na(M.treat[[r]]), arr.ind = TRUE)] <- 0
  M.sd.treat[[r]][which(is.na(M.sd.treat[[r]]), arr.ind = TRUE)] <- 0
}



#
# INITIALIZE LOOP ------------------------------------------------------------
#
# empty data frames to store output for metrics
# general metrics separate from distribution fit metrics
metric.names <- c("strength", "comp_strength", "fac_strength", "weight", "connectance", "linkage_diversity", "r", "indirect_effect", "relative_intransitivity")
treat.mean <- matrix(NA, nrow = length(M.all), ncol = length(metric.names))
treat.sd <- matrix(NA, nrow = length(M.all), ncol = length(metric.names))
colnames(treat.mean) <- metric.names
colnames(treat.sd) <- metric.names
aicc.treat <- data.frame(Metric = character(0), Distribution = character(0), freq = integer(0), Frequency = numeric(0), Case = character(0))

# set number of bootstrap iterations
iterations <- 10000

setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis")

#
# LOOP ------------------------------------------------------------
#
for (k in 1:length(M.treat)) {
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
    mean.matrix <- t(M.treat[[k]])
    sd.matrix <- t(M.sd.treat[[k]])
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
  fit <- apply(meta.dat.table, 2, function(x) fitdist(data = x, distr = "norm", method = "mme"))
  mean.vec <- numeric(length = length(fit))
  sd.vec <- numeric(length = length(fit))
  for (w in 1:length(fit)) {
    mean.vec[w] <- unname(fit[[w]][[1]][1])
    sd.vec[w] <- unname(fit[[w]][[1]][2])
  }
  treat.mean[k, ] <- mean.vec
  treat.sd[k, ] <- sd.vec
  
  # convert output from distribution fits (in list)
  
  aicc.store$Metric <- factor(aicc.store$Metric, levels = c("weight", "s.in", "s.in.c", "s.in.f", "s.out", "s.out.c", "s.out.f"))
  aicc.store$Distribution <- factor(aicc.store$Distribution, levels = c("Uniform", "Normal", "Lognormal", "Exponential", "PowerLaw"))
  aicc.output <- count(aicc.store, c("Metric", "Distribution"))
  aicc.output$Frequency <- aicc.output$freq / iterations
  aicc.output$Case <- rep(files.names[[k]], nrow(aicc.output))
  colnames(aicc.output) <- c("Metric", "Distribution", "freq", "Frequency", "Case")
  aicc.treat <- rbind(aicc.treat, aicc.output)
  
  print(paste("Network ", k, " is complete"))
}

#
# SAVE OUTPUT ------------------------------------------------------------
#

rownames(treat.mean) <- files.names
rownames(treat.sd) <- files.names
write.csv(x = treat.mean, file = "TreatmeansRII.csv")
write.csv(x = treat.sd, file = "TreatsdsRII.csv")
write.csv(x = aicc.treat, file = "TreatdistributionsRII.csv")





