par.defaults <- par(no.readonly = TRUE)
save(par.defaults, file="R.default.par.RData")
load("R.default.par.RData")

library(R2jags)
library(abind)
library(boot)
library(ggplot2)

#
# load data, create data frame ------------------------------------------------
#
# load data from MAIN.R
# means (theta.hat) and variance (v) for each metric
# metrics are columns in data frame, loop through columns and get the grand means
# MA.mean <- data.frame(meta.dat.mean)
# MA.var <- data.frame(meta.dat.sd^2)
# load data from .csv
# Cntrl
# C.mean <- read.csv("meansRIICntrl.csv", row.names = 1)
# C.var <- read.csv("sdsRIICntrl.csv", row.names = 1)
# C.var <- C.var^2
# K.var <- read.csv("sdsRIIKinlock.csv", row.names = 1)
# K.var <- K.var^2
# C.mean <- rbind(C.mean, K.mean)
# C.var <- rbind(C.var, K.var)
# MixMono
# MA.mean <- read.csv("meansRIIMixMono.csv", row.names = 1)
# MA.var <- read.csv("sdsRIIMixMono.csv", row.names = 1)
# MA.var <- MA.var^2
# Together
C.mean <- read.csv("meansRIICntrl.csv", row.names = 1)
C.var <- read.csv("sdsRIICntrl.csv", row.names = 1)
C.var <- C.var^2
K.mean <- read.csv("meansRIIKinlock.csv", row.names = 1)
K.var <- read.csv("sdsRIIKinlock.csv", row.names = 1)
K.var <- K.var^2
C.mean <- rbind(C.mean, as.vector(t(K.mean)))
C.var <- rbind(C.var, as.vector(t(K.var)))
M.mean <- read.csv("meansRIIMixMono.csv", row.names = 1)
M.var <- read.csv("sdsRIIMixMono.csv", row.names = 1)
M.var <- M.var^2
MA.mean <- rbind(C.mean, M.mean)
MA.var <- rbind(C.var, M.var)

zeros <- which(MA.mean == 0, arr.ind = TRUE)
MA.mean[zeros] <- MA.mean[zeros] + 1E-9
zeros <- which(MA.var == 0, arr.ind = TRUE)
MA.var[zeros] <- MA.var[zeros] + 1E-9

# create empty dataframes to store estimates
dat.mu <- data.frame(metric = character(), name = character(), est = numeric(), CI.l = numeric(), CI.h = numeric(), stringsAsFactors=FALSE) 
dat.tau <- data.frame(metric = character(), name = character(), est = numeric(), CI.l = numeric(), CI.h = numeric(), stringsAsFactors=FALSE) 

for (a in 1:ncol(MA.mean)) {
  dat <- data.frame(MA.mean[, a], MA.var[, a])  # load one metric
  metrics.name <- colnames(MA.mean)
  dat.name <- metrics.name[a]  # metric name
  
#
# write statistical model code to a text file ------------------------------------------------
#

sink("metaanalysis.jags")

cat("
    model {
    for (i in 1:k) {
    theta.hat[i] ~ dnorm(theta[i], w[i])
    w[i] <- 1 / v[i]
    theta[i] ~ dnorm(mu, precision.tau2)
    }
    precision.tau2 ~ dgamma(0.001, 0.001)
    tau <- sqrt(1 / precision.tau2)
    mu ~ dnorm(0, 1E-3)
    }",fill = TRUE)

sink()

#
# create list with data model needs to run ------------------------------------------------
#
Dat <- list(
  k = nrow(dat),
  theta.hat = dat[, 1],
  v = dat[, 2]
)
#
# make function with list of parameters and initial values ------------------------------------------------
#
InitStage <- function() {
  list(mu = 0, precision.tau2 = 1)
}
#
# make column vector with the parameters to track ------------------------------------------------
#
ParsStage <- c("mu", "tau")
#
# set the variables for MCMC ------------------------------------------------
#
ni <- 100000  # number of draws from the posterior
nt <- 1    # thinning rate
nb <- 1000  # number to discard for burn-in
nc <- 2  # number of chains
#
# call jags function to run the code ------------------------------------------------
#
m <- jags(inits = InitStage,
          n.chains = nc,
          model.file = "metaanalysis.jags",
          working.directory = getwd(),
          data = Dat,
          parameters.to.save = ParsStage,
          n.thin = nt,
          n.iter = ni,
          n.burnin = nb,
          DIC = TRUE)
#
# print summary ------------------------------------------------
#
m
dim(m$BUGSoutput$sims.array)
dim(m$BUGSoutput$sims.matrix)
#
# plot results ------------------------------------------------
#

# # convergence for mu
# par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
# beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 2])
# beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 2])
# densplot(beta1.chain.1, main = "Chain 1", xlab = "")
# densplot(beta1.chain.2, main = "Chain 2", xlab = "")
# traceplot(beta1.chain.1, xlab = "mu" )
# traceplot(beta1.chain.2, xlab = "mu")
# 
# # convergence for tau
# beta2.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 3]), 1, 3])
# beta2.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 3]), 2, 3])
# densplot(beta2.chain.1, main = "Chain 1", xlab = "")
# densplot(beta2.chain.2, main = "Chain 2", xlab = "")
# traceplot(beta2.chain.1, xlab = "tau" )
# traceplot(beta2.chain.2, xlab = "tau")
# 
# # extract means and CIs for mu (grand mean) and tau (heterogeneity)
mu <- grep("mu", row.names(m$BUGSoutput$summary))
tau <- grep("tau", row.names(m$BUGSoutput$summary))
CI.95.low <- m$BUGSoutput$summary[c(mu, tau), 3]  # 95% CI
CI.95.high <- m$BUGSoutput$summary[c(mu, tau), 7]
est <- m$BUGSoutput$summary[c(mu, tau), 1]  # mean

# output from current meta-analysis
dat.output <- data.frame(metric = c(dat.name, dat.name), name = c("mu", "tau"), est = est, CI.l = CI.95.low, CI.h = CI.95.high)

# plot estimates
# ggplot(data = dat.output) + geom_bar(stat = "identity", aes(x = name, y = est)) + geom_errorbar(aes(x = name, ymin = CI.95.low, ymax = CI.95.high, width = 0.4)) + ggtitle("Estimates") + theme_bw()

# store in overall dataframe with all metrics
dat.mu <- rbind(dat.mu, dat.output[1, ])
dat.tau <- rbind(dat.tau, dat.output[2, ])

}

dat.mu
# write.csv(x = dat.mu, file = "overallmuMixMono.csv")
# write.csv(x = dat.mu, file = "overallmuCntrl.csv")
write.csv(x = dat.mu, file = "overallmu.csv")

#
# GROUPED BY EXPERIMENT TYPE
#
#
# load data, create data frame ------------------------------------------------
#
# create empty dataframes to store estimates
dat.betas <- data.frame(metric = character(), name = character(), est = numeric(), CI.l = numeric(), CI.h = numeric(), stringsAsFactors=FALSE) 

for (a in 1:ncol(MA.mean)) {
  dat <- data.frame(MA.mean[, a], MA.var[, a])  # load one metric
  dat$Group <- as.factor(c(rep(1, nrow(C.mean)), rep(2, nrow(M.mean))))
  metrics.name <- colnames(MA.mean)
  dat.name <- metrics.name[a]  # metric name
  #
  # write statistical model code to a text file ------------------------------------------------
  #
  sink("metaanalysisgroup.jags")
  
  cat("
      model {
      for (i in 1:k) {
      theta.hat[i] ~ dnorm(theta[i], w[i])
      w[i] <- 1 / v[i]
      theta[i] ~ dnorm(mu[i], precision.tau2)
      mu[i] <- beta[Group[i]]
      }
      beta[1] ~ dnorm(0, 1E-3)
      beta[2] ~ dnorm(0, 1E-3)
      precision.tau2 ~ dgamma(0.001, 0.001)
      tau <- sqrt(1 / precision.tau2)
      }", fill = TRUE)

  sink()
  #
  # create list with data model needs to run ------------------------------------------------
  #
  Dat <- list(
    k = nrow(dat),
    theta.hat = dat$MA.mean...a.,
    v = dat$MA.var...a.,
    Group = dat$Group
  )
  #
  # make function with list of parameters and initial values ------------------------------------------------
  #
  InitStage <- function() {
    list(precision.tau2 = 1, beta = c(0, 0))
  }
  #
  # make column vector with the parameters to track ------------------------------------------------
  #
  ParsStage <- c("beta", "tau")
  #
  # set the variables for MCMC ------------------------------------------------
  #
  ni <- 100000  # number of draws from the posterior
  nt <- 1    # thinning rate
  nb <- 1000  # number to discard for burn-in
  nc <- 2  # number of chains
  #
  # call jags function to run the code ------------------------------------------------
  #
  m <- jags(inits = InitStage,
            n.chains = nc,
            model.file = "metaanalysisgroup.jags",
            working.directory = getwd(),
            data = Dat,
            parameters.to.save = ParsStage,
            n.thin = nt,
            n.iter = ni,
            n.burnin = nb,
            DIC = TRUE)
  #
  # print summary ------------------------------------------------
  #
  m
  #
  # plot results ------------------------------------------------
  #
  # # convergence for beta1 (Cntrl)
  # par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
  # beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 1])
  # beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 1])
  # densplot(beta1.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta1.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta1.chain.1, xlab = "beta1" )
  # traceplot(beta1.chain.2, xlab = "beta1")
  # 
  # # convergence for beta2 (MixMono)
  # par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
  # beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 2])
  # beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 2])
  # densplot(beta1.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta1.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta1.chain.1, xlab = "beta2" )
  # traceplot(beta1.chain.2, xlab = "beta2")
  # 
  # # convergence for tau
  # beta2.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 3]), 1, 3])
  # beta2.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 3]), 2, 3])
  # densplot(beta2.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta2.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta2.chain.1, xlab = "tau" )
  # traceplot(beta2.chain.2, xlab = "tau")
  # 
  # extract means and CIs for beta (group means)
  beta1 <- grep("beta", row.names(m$BUGSoutput$summary))[1]
  beta2 <- grep("beta", row.names(m$BUGSoutput$summary))[2]
  CI.95.low <- m$BUGSoutput$summary[c(beta1, beta2), 3]  # 95% CI
  CI.95.high <- m$BUGSoutput$summary[c(beta1, beta2), 7]
  est <- m$BUGSoutput$summary[c(beta1, beta2), 1]  # mean
  # output from current meta-analysis
  dat.output <- data.frame(metric = c(dat.name, dat.name), name = c("beta1", "beta2"), est = est, CI.l = CI.95.low, CI.h = CI.95.high)
  # plot estimates
  # ggplot(data = dat.output) + geom_bar(stat = "identity", aes(x = name, y = est)) + geom_errorbar(aes(x = name, ymin = CI.95.low, ymax = CI.95.high, width = 0.4)) + ggtitle("Estimates") + theme_bw()
  # store in overall dataframe with all metrics
  dat.betas <- rbind(dat.betas, dat.output)
  
}
dat.betas
write.csv(x = dat.betas, file = "byexperiment.csv")



#
# GROUPED BY OLD FIELD/GRASSLAND
#
#
# load data, create data frame ------------------------------------------------
#
# create empty dataframes to store estimates
dat.betas <- data.frame(metric = character(), name = character(), est = numeric(), CI.l = numeric(), CI.h = numeric(), stringsAsFactors=FALSE) 
df.init <- read.csv("CodingTable.csv")
col.remove <- c("Title", "doi", "Abstract", "Status", "OtherTreatments", "Metric", "Variation", "Variation.1")  # unwanted columns
df <- df.init[df.init$Status == "Complete" & df.init$RII == 1, !(names(df.init) %in% col.remove)]  # only completed cases with RII
df$Habitat <- as.character(df$Habitat)
df$Habitat[which(df$Habitat == "Coniferous forest" | df$Habitat == "Deciduous forest" | df$Habitat == "Plantation forest" | df$Habitat == "Tropical forest")] <- "Forest"
df$Habitat[which(df$Habitat == "Chaparral")] <- "Grassland"
df$Habitat <- factor(df$Habitat, levels = c("Grassland", "Old field", "Forest", "Estuarine", "Agricultural", "Urban", "Desert"))
ind <- which(df$Habitat == "Old field" | df$Habitat == "Grassland")

for (a in 1:ncol(MA.mean)) {
  dat <- data.frame(MA.mean[, a], MA.var[, a])  # load one metric
  dat$HabTest <- NA
  dat[ind, "HabTest"] <- 1
  dat[-ind, "HabTest"] <- 2
  metrics.name <- colnames(MA.mean)
  dat.name <- metrics.name[a]  # metric name
  #
  # create list with data model needs to run ------------------------------------------------
  #
  Dat <- list(
    k = nrow(dat),
    theta.hat = dat$MA.mean...a.,
    v = dat$MA.var...a.,
    Group = dat$HabTest
  )
  #
  # make function with list of parameters and initial values ------------------------------------------------
  #
  InitStage <- function() {
    list(precision.tau2 = 1, beta = c(0, 0))
  }
  #
  # make column vector with the parameters to track ------------------------------------------------
  #
  ParsStage <- c("beta", "tau")
  #
  # set the variables for MCMC ------------------------------------------------
  #
  ni <- 100000  # number of draws from the posterior
  nt <- 1    # thinning rate
  nb <- 1000  # number to discard for burn-in
  nc <- 2  # number of chains
  #
  # call jags function to run the code ------------------------------------------------
  #
  m <- jags(inits = InitStage,
            n.chains = nc,
            model.file = "metaanalysisgroup.jags",
            working.directory = getwd(),
            data = Dat,
            parameters.to.save = ParsStage,
            n.thin = nt,
            n.iter = ni,
            n.burnin = nb,
            DIC = TRUE)
  #
  # print summary ------------------------------------------------
  #
  m
  #
  # plot results ------------------------------------------------
  #
  # 
  # # convergence for beta1 (OldFieldGrassland)
  # par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
  # beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 1])
  # beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 1])
  # densplot(beta1.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta1.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta1.chain.1, xlab = "beta1" )
  # traceplot(beta1.chain.2, xlab = "beta1")
  # 
  # # convergence for beta2 (Other)
  # par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
  # beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 2])
  # beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 2])
  # densplot(beta1.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta1.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta1.chain.1, xlab = "beta2" )
  # traceplot(beta1.chain.2, xlab = "beta2")
  # 
  # # convergence for tau
  # beta2.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 3]), 1, 3])
  # beta2.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 3]), 2, 3])
  # densplot(beta2.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta2.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta2.chain.1, xlab = "tau" )
  # traceplot(beta2.chain.2, xlab = "tau")
  
  # extract means and CIs for beta (group means)
  beta1 <- grep("beta", row.names(m$BUGSoutput$summary))[1]
  beta2 <- grep("beta", row.names(m$BUGSoutput$summary))[2]
  CI.95.low <- m$BUGSoutput$summary[c(beta1, beta2), 3]  # 95% CI
  CI.95.high <- m$BUGSoutput$summary[c(beta1, beta2), 7]
  est <- m$BUGSoutput$summary[c(beta1, beta2), 1]  # mean
  # output from current meta-analysis
  dat.output <- data.frame(metric = c(dat.name, dat.name), name = c("beta1", "beta2"), est = est, CI.l = CI.95.low, CI.h = CI.95.high)
  # plot estimates
  # ggplot(data = dat.output) + geom_bar(stat = "identity", aes(x = name, y = est)) + geom_errorbar(aes(x = name, ymin = CI.95.low, ymax = CI.95.high, width = 0.4)) + ggtitle("Estimates") + theme_bw()
  # store in overall dataframe with all metrics
  dat.betas <- rbind(dat.betas, dat.output)
}

dat.betas
write.csv(x = dat.betas, file = "byhabitat.csv")

#
# GROUPED BY GREENHOUSE/FIELD/GARDEN
#
#
# load data, create data frame ------------------------------------------------
#
# create empty dataframes to store estimates
dat.betas <- data.frame(metric = character(), name = character(), est = numeric(), CI.l = numeric(), CI.h = numeric(), stringsAsFactors=FALSE) 
gh <- which(df$ExperimentClass == "Greenhouse")
fd <- which(df$ExperimentClass == "Garden")
gdn <- which(df$ExperimentClass == "Field")
for (a in 1:ncol(MA.mean)) {
  dat <- data.frame(MA.mean[, a], MA.var[, a])  # load one metric
  dat$GhTest <- NA
  dat[gh, "GhTest"] <- 1
  dat[fd, "GhTest"] <- 2
  dat[gdn, "GhTest"] <- 3
  metrics.name <- colnames(MA.mean)
  dat.name <- metrics.name[a]  # metric name
  #
  # write statistical model code to a text file ------------------------------------------------
  #
  sink("metaanalysismultigroup.jags")
  
  cat("
      model {
      for (i in 1:k) {
      theta.hat[i] ~ dnorm(theta[i], w[i])
      w[i] <- 1 / v[i]
      theta[i] ~ dnorm(mu[i], precision.tau2)
      mu[i] <- beta[Group[i]]
      }
      beta[1] ~ dnorm(0, 1E-3)
      beta[2] ~ dnorm(0, 1E-3)
      beta[3] ~ dnorm(0, 1E-3)
      precision.tau2 ~ dgamma(0.001, 0.001)
      tau <- sqrt(1 / precision.tau2)
      }", fill = TRUE)

  sink()
  #
  # create list with data model needs to run ------------------------------------------------
  #
  Dat <- list(
    k = nrow(dat),
    theta.hat = dat$MA.mean...a.,
    v = dat$MA.var...a.,
    Group = dat$GhTest
  )
  #
  # make function with list of parameters and initial values ------------------------------------------------
  #
  InitStage <- function() {
    list(precision.tau2 = 1, beta = c(0, 0, 0))
  }
  #
  # make column vector with the parameters to track ------------------------------------------------
  #
  ParsStage <- c("beta", "tau")
  #
  # set the variables for MCMC ------------------------------------------------
  #
  ni <- 100000  # number of draws from the posterior
  nt <- 1    # thinning rate
  nb <- 1000  # number to discard for burn-in
  nc <- 2  # number of chains
  #
  # call jags function to run the code ------------------------------------------------
  #
  m <- jags(inits = InitStage,
            n.chains = nc,
            model.file = "metaanalysismultigroup.jags",
            working.directory = getwd(),
            data = Dat,
            parameters.to.save = ParsStage,
            n.thin = nt,
            n.iter = ni,
            n.burnin = nb,
            DIC = TRUE)
  #
  # print summary ------------------------------------------------
  #
  m
  #
  # plot results ------------------------------------------------
  #
  # 
  # # convergence for beta1 (Greenhouse)
  # par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
  # beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 1])
  # beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 1])
  # densplot(beta1.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta1.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta1.chain.1, xlab = "beta1" )
  # traceplot(beta1.chain.2, xlab = "beta1")
  # # 
  # # convergence for beta2 (Garden)
  # par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
  # beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 2])
  # beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 2])
  # densplot(beta1.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta1.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta1.chain.1, xlab = "beta2" )
  # traceplot(beta1.chain.2, xlab = "beta2")
  # # convergence for beta3 (Field)
  # par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
  # beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 3])
  # beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 3])
  # densplot(beta1.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta1.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta1.chain.1, xlab = "beta3" )
  # traceplot(beta1.chain.2, xlab = "beta3")
  # # 
  # # convergence for tau
  # beta2.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 3]), 1, 4])
  # beta2.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 3]), 2, 4])
  # densplot(beta2.chain.1, main = "Chain 1", xlab = "")
  # densplot(beta2.chain.2, main = "Chain 2", xlab = "")
  # traceplot(beta2.chain.1, xlab = "tau" )
  # traceplot(beta2.chain.2, xlab = "tau")
  
  # extract means and CIs for beta (group means)
  beta1 <- grep("beta", row.names(m$BUGSoutput$summary))[1]
  beta2 <- grep("beta", row.names(m$BUGSoutput$summary))[2]
  beta3 <- grep("beta", row.names(m$BUGSoutput$summary))[3]
  CI.95.low <- m$BUGSoutput$summary[c(beta1, beta2, beta3), 3]  # 95% CI
  CI.95.high <- m$BUGSoutput$summary[c(beta1, beta2, beta3), 7]
  est <- m$BUGSoutput$summary[c(beta1, beta2, beta3), 1]  # mean
  # output from current meta-analysis
  dat.output <- data.frame(metric = c(dat.name, dat.name, dat.name), name = c("beta1", "beta2", "beta3"), est = est, CI.l = CI.95.low, CI.h = CI.95.high)
  # plot estimates
  # ggplot(data = dat.output) + geom_bar(stat = "identity", aes(x = name, y = est)) + geom_errorbar(aes(x = name, ymin = CI.95.low, ymax = CI.95.high, width = 0.4)) + ggtitle("Estimates") + theme_bw()
  # store in overall dataframe with all metrics
  dat.betas <- rbind(dat.betas, dat.output)
}

dat.betas
write.csv(x = dat.betas, file = "bygreenhouse.csv")

