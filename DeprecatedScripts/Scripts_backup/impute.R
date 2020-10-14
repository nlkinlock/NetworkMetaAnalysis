par.defaults <- par(no.readonly = TRUE)
save(par.defaults, file="R.default.par.RData")
load("R.default.par.RData")

library(R2jags)
library(ggplot2)
#
# load data, create data frame ------------------------------------------------
#
# extract all .csv files that contain biomass and SE (convert to SD)
# create list of data frames
# reduce list to column with biomass and column with SD
setwd("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono")
files <- dir(pattern = "*.csv", full.names = TRUE)
files <- files[-c(11, 13, 18)]  # only include studies with biomass as performance metric
tables <- lapply(files, function(x) read.csv(x))
setwd("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl")
files2 <- dir(pattern = "*.csv", full.names = TRUE)
files2 <- files2[-c(1, 3)]
tables2 <- lapply(files2, function(x) read.csv(x))
tables <- c(tables, tables2)
indices <- sapply(tables, function(x) any(colnames(x) == "SE"))
tables <- tables[indices]  # subset by only studies with SE
tables.metric <- lapply(tables, `[`, 3)
tables.SE <- lapply(tables, `[`, c(4, 5))
tables.SD <- lapply(tables.SE, function(x) sqrt(x[, 2]) * x[, 1])
metric <- unlist(lapply(tables.metric, function(x) as.vector(t(x))))
SD <- unlist(lapply(tables.SD, function(x) as.vector(t(x))))
dat <- data.frame(Metric = metric, SD = SD)
dat <- dat[which(dat$SD != 0), ]
dat <- dat[dat$SD < 5, ]
# extract data frames with missing SE
# add to data frame for prediction
Lof <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/Impute/7_Lof_2014.csv")
Aman <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/Impute/192_Amanullah_2013_HW.csv")
Aman_LW <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/Impute/192_Amanullah_2013_LW.csv")
missing <- c(Lof$Metric[Lof$Metric != 0], Aman$Metric, Aman_LW$Metric)
missing.SD <- rep(NA, length(missing))
newrows <- cbind(Metric = missing, SD = missing.SD)
dat <- rbind(dat, newrows)


setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis")
#
# write statistical model code to a text file ------------------------------------------------
#

sink("imputeBiomass.jags")

cat("
    model {
    for (i in 1:length(y)) {
    y[i] ~ dgamma(alpha, alpha / exp(theta[i]))
    predicted_obs[i] ~ dgamma(alpha, alpha / exp(theta[i]))
    theta[i] <- beta0 + beta1 * metric[i]
    }
    beta0 ~ dnorm(0, 1.0E-6)
    beta1 ~ dnorm(0, 1.0E-6)
    alpha ~ dunif(0, 100)
    }", fill = TRUE)

sink()

#
# create list with data model needs to run ------------------------------------------------
#

Dat <- list(
  y = dat[, 2],
  metric = dat[, 1]
)

#
# make function with list of parameters and initial values ------------------------------------------------
#

InitStage <- function() {
  list(beta0 = 0.5, beta1 = 1, alpha = 2)
}

#
# make column vector with the parameters to track ------------------------------------------------
#

ParsStage <- c("beta0", "beta1", "alpha", "theta", "predicted_obs")

#
# set the variables for MCMC ------------------------------------------------
#

ni <- 10000  # number of draws from the posterior
nt <- 1    # thinning rate
nb <- 1000  # number to discard for burn-in
nc <- 3  # number of chains

#
# call jags function to run the code ------------------------------------------------
#

m <- jags(inits = InitStage,
          n.chains = nc,
          model.file = "imputeBiomass.jags",
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
# check convergence
# for alpha
par(mfrow = c(2, 2), mar = c(4, 4, 2, 0.4))
beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 1]), 1, 1])
beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 1]), 2, 1])
densplot(beta1.chain.1, main = "Chain 1", xlab = "")
densplot(beta1.chain.2, main = "Chain 2", xlab = "")
traceplot(beta1.chain.1, xlab = "alpha" )
traceplot(beta1.chain.2, xlab = "alpha")
# for beta0
beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 2]), 1, 2])
beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 2]), 2, 2])
densplot(beta1.chain.1, main = "Chain 1", xlab = "")
densplot(beta1.chain.2, main = "Chain 2", xlab = "")
traceplot(beta1.chain.1, xlab = "beta0" )
traceplot(beta1.chain.2, xlab = "beta0")
# for beta1
beta1.chain.1 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 1, 3]), 1, 3])
beta1.chain.2 <- as.mcmc(m$BUGSoutput$sims.array[nb: length(m$BUGSoutput$sims.array[, 2, 3]), 2, 3])
densplot(beta1.chain.1, main = "Chain 1", xlab = "")
densplot(beta1.chain.2, main = "Chain 2", xlab = "")
traceplot(beta1.chain.1, xlab = "beta1" )
traceplot(beta1.chain.2, xlab = "beta1")

# visualize regression
theta <- grep("theta", row.names(m$BUGSoutput$summary))
predicted_obs <- grep("predicted_obs", row.names(m$BUGSoutput$summary))
CI.95.low <- m$BUGSoutput$summary[theta, 3]  # 95% CIs for every theta
CI.95.high <- m$BUGSoutput$summary[theta, 7]
PI.95.low <- m$BUGSoutput$summary[predicted_obs, 3]  # 95% PIs for every predicted obs.
PI.95.high <- m$BUGSoutput$summary[predicted_obs, 7]
theta.mean <- m$BUGSoutput$summary[theta, 1]  # mean for every theta (linear predictor)
dat.output <- data.frame(dat, theta = theta.mean, CI.l = CI.95.low, CI.h = CI.95.high, PI.l = PI.95.low, PI.h = PI.95.high)

ggplot(data = dat.output) + geom_point(aes(x = Metric, y = SD)) + geom_line(aes(x = Metric, y = exp(theta))) +
  geom_line(aes(x = Metric, y = exp(CI.l)), linetype = "dashed", colour = "seagreen4") + 
  geom_line(aes(x = Metric, y = exp(CI.h)), linetype = "dashed", colour = "seagreen4") +
  geom_line(aes(x = Metric, y = PI.l), linetype = "dotted", colour = "dodgerblue1") + 
  geom_line(aes(x = Metric, y = PI.h), linetype = "dotted", colour = "dodgerblue1") +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 20)) + ylab("SD") + xlab("Metric") + ggtitle("Imputing SD") + theme_bw()

#
# imputation ------------------------------------------------
#
# subset output to only predicted observations
impute.mean <- m$BUGSoutput$summary[predicted_obs, 1] # replace missing SDs with mean predicted value from regression
missing <- which(is.na(dat$SD))
dat$SD[missing] <- impute.mean[missing]



#
# save imputed SDs to file ------------------------------------------------
#
dat.imputed <- dat[missing, ]
write.csv(x = dat.imputed[1:19, 2], file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/Impute/7_Lof_2014_impSE.csv")
write.csv(x = dat.imputed[20:28, 2], file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/Impute/192_Amanullah_2013_HW_impSE.csv")
write.csv(x = dat.imputed[29:37, 2], file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/Impute/192_Amanullah_2013_LW_impSE.csv")





