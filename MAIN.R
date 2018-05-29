# META-FILE FOR BAYESIAN BOOTSTRAP
# USE THIS FILE TO LOAD PACKAGES, INITIALIZE MODELS AND FUNCTIONS, CALL SUB-FILES, AND WRITE OUTPUT
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD PACKAGES, INITIALIZE ---------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
library(R2jags)
library(runjags)
library(MCMCvis)
library(abind)
library(boot)
library(bPCA)
library(igraph)
library(ggplot2)
library(reshape2)
library(plyr)

# Bayesian bootstrap parameters to adjust
# number of bootstrap iterations
R <- 10000
# size of simulated dataset
N <- 1000
# initiate loop
case <- 1
# number of networks
net.num <- 32
# MCMC inputs
ni <- 200000  # number of draws from the posterior
nt <- 10    # thinning rate
nb <- 100000  # number to discard for burn-in
nc <- 3  # number of chains

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

# load transitivity inputs: max and min variance for networks of a given size
var.min.s <- t(read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/var_min.csv"))
var.max.s <- t(read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/var_max.csv"))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD DESCRIPTIVE DATA -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load coding table with descriptive data for each case
df.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/CodingTable.csv")
col.remove <- c("Filename", "AbundanceRank", "TargetDensity", "NeighborDensity", "Connectance",
                "WateringRegime", "PotVolume", "SoilType", "Treatments", "Authors", "Journal", "Title",
                "doi", "Abstract", "OtherTreatments", "Metric", "UniqueNotes")  # unwanted columns
df <- df.init[, !(names(df.init) %in% col.remove)] # only completed cases with RII
df$UniqueID <- as.factor(df$UniqueID)
df$ExperimentType <- factor(df$ExperimentType)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# BOOTSTRAP -------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
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
  }
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
  }
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
  }
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
}


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



