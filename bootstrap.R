# BAYESIAN BOOTSTRAP: CALCULATE RII AND NETWORK METRICS FOR ALL NETWORKS
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD NETWORKS ---------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load data frames with plant performance and variance in performance for all pairwise combinations
<<<<<<< HEAD
# locate files and create vector of names
case.names <- list.files(path = paste(path, "Input/CaseData", sep = ""), recursive = FALSE)
case.names <- substr(case.names, 0, nchar(case.names) - 4)
case.names[grep(case.names, pattern = "_imp")] <- substr(case.names[grep(case.names, pattern = "_imp")], 0, nchar(case.names[grep(case.names, pattern = "_imp")]) - 4)
case.names[grep(case.names, pattern = "_Cntrl")] <- substr(case.names[grep(case.names, pattern = "_Cntrl")], 0, nchar(case.names[grep(case.names, pattern = "_Cntrl")]) - 6)
#
# load input files
file.paths <- list.files(path = paste(path, "Input/CaseData", sep = ""), full.names = TRUE)
case.data <- lapply(file.paths, read.csv)
#
# indices for networks with true control (used when calculating RII in loop)
ctrl.indices <- c(1, 2, 3, 5, 6, 8, 9, 13, 14, 22, 23, 26, 32, 34, 36, 37, 38)
# networks with monoculture control
mono.indices <- c(4, 7, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21, 24, 25, 27, 28, 29, 30, 31, 33, 35)
# networks that do have a true control, but are suitable to calculate RII with monoculture as control (MonoTest)
mono.test.indices <- c(1, 2, 3, 5, 6, 8, 9, 13, 22, 23, 26, 32, 36, 38)
# networks with different abiotic conditions (treatments)
treatment.indices <- c(9, 11, 18, 23, 25, 29, 30)
treatment.ctrl.indices <- c(8, 10, 17, 22, 24, 28)
# empty data frames to store output of network metrics
||||||| 2b1eac9
case.names <- c()
b.all <- list()
i <- 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/7_Lof_2014_imp.csv")
case.names[i] <- "7_Lof_2014"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/106_Costa_2003.csv")
case.names[i] <- "106_Costa_2003"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/152_Cuda_2015.csv")
case.names[i] <- "152_Cuda_2015"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/192_Amanullah_2013_imp.csv")
case.names[i] <- "192_Amanullah_2013"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/219_Frerot_2006.csv")
case.names[i] <- "219_Frerot_2006"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/236_Jiang_2014_Cntrl.csv")
case.names[i] <- "236_Jiang_2014"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/274_Hendriks_2015.csv")
case.names[i] <- "274_Hendriks_2015"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/387_Farrer_2011.csv")
case.names[i] <- "387_Farrer_2011"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/492_Miller_1987.csv")
case.names[i] <- "492_Miller_1987"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/674_Dehlin_2008.csv")
case.names[i] <- "674_Dehlin_2008"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/805_Engel_2008.csv")
case.names[i] <- "805_Engel_2008"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/838_Goldberg_1991.csv")
case.names[i] <- "838_Goldberg_1991"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1002_Domenech_2008.csv")
case.names[i] <- "1002_Domenech_2008"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1097_Hedberg_2005.csv")
case.names[i] <- "1097_Hedberg_2005"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1164_Sangakkara_1985.csv")
case.names[i] <- "1164_Sangakkara_1985"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1198_Armas_2011.csv")
case.names[i] <- "1198_Armas_2011"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/1205_Gao_2014.csv")
case.names[i] <- "1205_Gao_2014"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1425_Baude_2011.csv")
case.names[i] <- "1425_Baude_2011"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1448_Gurevitch_1990_Cntrl.csv")
case.names[i] <- "1448_Gurevitch_1990"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1454_PfeiferMeister_2008_HighNutrient.csv")
case.names[i] <- "1454_PfeiferMeister_2008"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1467_Pausch_2013.csv")
case.names[i] <- "1467_Pausch_2013"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1479_Svenning_2008.csv")
case.names[i] <- "1479_Svenning_2008"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1507_Saccone_2010_imp.csv")
case.names[i] <- "1507_Saccone_2010"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1555_Marty_2009.csv")
case.names[i] <- "1555_Marty_2009"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1566_Niu_2008_Cntrl.csv")
case.names[i] <- "1566_Niu_2008"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1630_Mariotte_2012.csv")
case.names[i] <- "1630_Mariotte_2012"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1708_Chacon_2007.csv")
case.names[i] <- "1708_Chacon_2007"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1731_Bush_2004.csv")
case.names[i] <- "1731_Bush_2004"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1787_Weigelt_2002_HighWater.csv")
case.names[i] <- "1787_Weigelt_2002"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1998_Fortner_2007.csv")
case.names[i] <- "1998_Fortner_2007"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/9999_Kinlock_unpubl.csv")
case.names[i] <- "9999_Kinlock_unpubl"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/9998_Kinlock_unpubl.csv")
case.names[i] <- "9998_Kinlock_unpubl"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1448_Gurevitch_1990_Fert.csv")
case.names[i] <- "1448_Gurevitch_1990_Fert"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1787_Weigelt_2002_LowWater.csv")
case.names[i] <- "1787_Weigelt_2002_LowWater"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1454_PfeiferMeister_2008_LowNutrient.csv")
case.names[i] <- "1454_PfeiferMeister_2008_LowNutrient"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/1566_Niu_2008_Warm.csv")
case.names[i] <- "1566_Niu_2008_Warm"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/192_Amanullah_2013_LW_imp.csv")
case.names[i] <- "192_Amanullah_2013_LW"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/236_Jiang_2014_N.csv")
case.names[i] <- "236_Jiang_2014_N"
i <- i + 1
b.all[[i]] <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/236_Jiang_2014_Water.csv")
case.names[i] <- "236_Jiang_2014_Water"

# indices for networks with true control (used when calculating RII in loop)
b.ctrl <- c(1, 2, 8, 12, 13, 14, 16, 17, 19, 22, 23, 29, 30, 31, 32, 33, 34)

# empty data frame to store output of network metrics
=======
case.names <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", recursive = FALSE)
case.names <- substr(case.names, 0, nchar(case.names) - 4)
case.names[grep(case.names, pattern = "_imp")] <- substr(case.names[grep(case.names, pattern = "_imp")], 0, nchar(case.names[grep(case.names, pattern = "_imp")]) - 4)
case.names[grep(case.names, pattern = "_Cntrl")] <- substr(case.names[grep(case.names, pattern = "_Cntrl")], 0, nchar(case.names[grep(case.names, pattern = "_Cntrl")]) - 6)

file.paths <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", full.names = TRUE)
case.data <- lapply(file.paths, read.csv)

# indices for networks with true control (used when calculating RII in loop)
ctrl.indices <- c(1, 2, 3, 5, 6, 8, 9, 13, 14, 22, 23, 26, 32, 34, 36, 37, 38)
# networks with monoculture control
mono.indices <- c(4, 7, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21, 24, 25, 27, 28, 29, 30, 31, 33, 35)
# networks that do have a true control, but are suitable to calculate RII with monoculture as control (MonoTest)
mono.test.indices <- c(1, 2, 3, 5, 6, 8, 9, 13, 22, 23, 26, 32, 36, 38)
# networks with different abiotic conditions (treatments)
treatment.indices <- c(9, 11, 18, 23, 25, 29, 30)
treatment.ctrl.indices <- c(8, 10, 17, 22, 24, 28)
# empty data frames to store output of network metrics
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# mean, standard deviation, and credible intervals
<<<<<<< HEAD
metric.names <- c("MeanStrength", "WeightedConnectance", "WeightedFacConnectance", "WeightedCompConnectance", 
                    "Imbalance", "Asymmetry", "IndirectEffect", "RelativeIntransitivity")
metrics.df <- data.frame(Network = character(), MetricName = character(), ObservedMean = numeric(), BootstrapMean = numeric(),
                       BootstrapSD = numeric(), BootstrapCIL = numeric(), BootstrapCIU = numeric())
mono.test.df <- data.frame(Network = character(), MetricName = character(), ObservedMean = numeric(), BootstrapMean = numeric(),
                         BootstrapSD = numeric(), BootstrapCIL = numeric(), BootstrapCIU = numeric())
#
#
||||||| 2b1eac9
metric.names <- c("strength", "connectance", "asymm_diff", "indirect_effect", "relative_intransitivity")
meta <- data.frame(network = character(), metric = character(), obs = numeric(), mean = numeric(), sd = numeric(), CI.l = numeric(), CI.u = numeric(), stringsAsFactors = FALSE)


=======
metric.names <- c("MeanStrength", "WeightedConnectance", "WeightedFacConnectance", "WeightedCompConnectance", "Imbalance", "Asymmetry", "IndirectEffect", "RelativeIntransitivity")
metrics.df <- data.frame(Network = character(), MetricName = character(), ObservedMean = numeric(), BootstrapMean = numeric(),
                       BootstrapSD = numeric(), BootstrapCIL = numeric(), BootstrapCIU = numeric())
mono.test.df <- data.frame(Network = character(), MetricName = character(), ObservedMean = numeric(), BootstrapMean = numeric(),
                         BootstrapSD = numeric(), BootstrapCIL = numeric(), BootstrapCIU = numeric())

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOOP THROUGH NETWORKS -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#
# 1) Simulate performance data
# 2) Bootstrap performance data
# 3) In bootstrap: calculate RII
# 4) In bootstrap: calculate network metrics
# 5) Store network metrics (mean, sd, CIs)
#
for (case in 1:length(case.names)) {
  dat <- case.data[[case]]
  # study-specific parameters
  #
  # number of species in study
  num.species <- max(unique(dat$Target))
  # number of different treatments in study
  treatment <- nrow(dat[dat$Metric > 0, ])
  # sample size (within treatment) in study
  sample.size <- max(dat$N)
  # statistics
  #
  # calculate sd from se (using number of replicates)
  if (any(colnames(dat) == "SE")) {
    SD <- dat$SE * sqrt(dat$N)
    dat$SD <- SD
  }
  # calculate variance
  dat$var <- dat$SD^2
  if (case.names[case] == "1205_Gao_2014") {
    # calculate beta params
    #
    RII.trans <- (dat$Metric + 1) / 2  # transform RII bounded -1, 1 to be bounded 0, 1 (beta distributed)
    RII.sd.trans <- dat$SD / 2  # dividing by a constant influences sd
    # moment matching means and variances to parameters of a beta distribution
    # need to use a bounded distribution to correctly simulate potential values of RII
    alpha <- (((1 - RII.trans) / RII.sd.trans^2) - (1 / RII.trans)) * RII.trans^2
    beta <- alpha * ((1 / RII.trans) - 1)
    values <- which(!is.infinite(alpha))  # remove 0's before simulating data
    n <- length(values)
    alpha <- alpha[values]
    beta <- beta[values]
    # simulate data
    #
    # list of parameters for data simulation
    jags.data <- list(alpha = alpha, beta = beta, N = N, n = n)
    # run JAGS to simulate networks (matrices of RIIs)
    # output is 1000 data points for each RIIij
    b.sim <- run.jags(model = betamodel, data = jags.data, monitor = c("y"), sample = 1, n.chains = 1, summarise = FALSE)
    # convert output
    b.sim <- coda::as.mcmc(b.sim)
    b.sim <- matrix(data = as.vector(b.sim), nrow = N, ncol = n)
    b.sim <- b.sim * 2 - 1  # rescale to bounds -1, 1
  } else {
    # moment matching
    # gamma distribution is appropriate for biomass (and other measures of performance), positive and continuous
    # calculate alpha and beta for a gamma distribution using mean and variance from study
    dat$beta <- dat$var / dat$Metric
    dat$alpha <- dat$var / dat$beta^2
    dat$rate <- 1 / dat$beta  # JAGS parameterizes gamma using rate instead of beta (scale)
    dat[is.nan(dat$beta), "beta"] <- 0
    dat[is.nan(dat$alpha), "alpha"] <- 0
    dat[is.nan(dat$rate), "rate"] <- 0
    #
    # SIMULATE DATA
    #
    # create list with parameters model needs to run
    #
    # alpha and rate to parameterize gamma dist. (rate = 1 / beta)
    jags.data <- list(alpha = dat$alpha[dat$alpha > 0], rate = dat$rate[dat$rate > 0], N = N, treatment = treatment)
    # call jags function to simulate data
    b.sim <- run.jags(model = gammamodel, data = jags.data, monitor = c("int"), sample = 1, n.chains = 1, summarise = FALSE)
    # store output
    b.sim <- coda::as.mcmc(b.sim)
    b.sim <- matrix(data = as.vector(b.sim), nrow = N, ncol = treatment)
  }
  targets <- dat$Target[which(dat$Metric != 0)]
  neighbors <- dat$Neighbor[which(dat$Metric != 0)]
  #
  # create empty data frames for network metrics
  strength.store <- numeric(length = R)
  connectance.store <- numeric(length = R)
  connectance.fac.store <- numeric(length = R)
  connectance.comp.store <- numeric(length = R)
  intransitivity.store <- numeric(length = R)
  imbalance.store <- numeric(length = R)
  asymmetry.store <- numeric(length = R)
  indirect.effect.store <- numeric(length = R)
  # 
  if (case %in% mono.test.indices) {
    strength.mono.test <- numeric(length = R)
    connectance.mono.test <- numeric(length = R)
    connectance.fac.mono.test <- numeric(length = R)
    connectance.comp.mono.test <- numeric(length = R)
    intransitivity.mono.test <- numeric(length = R)
    imbalance.mono.test <- numeric(length = R)
    asymmetry.mono.test <- numeric(length = R)
    indirect.effect.mono.test <- numeric(length = R)
  }
  #
  # BAYESIAN BOOTSTRAP
  #
  # weight matrix dimensions: total sample size (all possible values, N) and number of bootstrap iterations (R)
  # uniform Dirichlet distribution, all values sum to 1 and any combination is equally likely
  weights <- matrix(rexp(N * R, 1) , ncol = N, byrow = TRUE)
  weights <- weights / rowSums(weights)
  for (iteration in 1:R) {
    # sample plant performance from simulated data set (size N) with bootstrap sample size equal to the number of replicates in the study
    # probabilities supplied in weight matrix
    # calculate RII using randomly sampled performance measures
    # format output to matrix
    #
    # include special cases, where RII is calculated differently (Gao, Costa, Saccone)
    # and calculate RII based on true control v. mono control
    if (case.names[case] == "1205_Gao_2014") {
      b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = 1, replace = TRUE, prob = weights[iteration, ]))
      M.boot <- matrix(b.boot, nrow = num.species, ncol = num.species)
      M.mono.test <- NA
    } else {
<<<<<<< HEAD
      b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample.size, replace = TRUE, prob = weights[iteration, ]))
      b.boot.long <- reshape2::melt(b.boot)
      b.boot.long <- data.frame(Target = rep(targets, each = sample.size), Neighbor = rep(neighbors, each = sample.size), Metric = b.boot.long[, 3])
||||||| 2b1eac9
      b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample_size, replace = TRUE, prob = weights[iteration, ]))
      b.boot.long <- melt(b.boot)
      b.boot.long <- data.frame(Target = rep(targets, each = sample_size), Neighbor = rep(neighbors, each = sample_size), Metric = b.boot.long[, 3])
      rii.boot <- c()
      count <- 1
=======
      b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample.size, replace = TRUE, prob = weights[iteration, ]))
      b.boot.long <- melt(b.boot)
      b.boot.long <- data.frame(Target = rep(targets, each = sample.size), Neighbor = rep(neighbors, each = sample.size), Metric = b.boot.long[, 3])
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
      if (case.names[case] == "106_Costa_2003") {
        M.boot <- SpecialCaseRII.Costa(b.boot, num.species, type = "bootstrap")
        M.mono.test <- MonoControlRII(b.boot.long, num.species)
      } else if (case.names[case] == "1507_Saccone_2010") {
        M.boot <- SpecialCaseRII.Saccone(b.boot, num.species, type = "bootstrap")
        M.mono.test <- NA
      } else if (case %in% ctrl.indices) {
        M.boot <- TrueControlRII(b.boot.long, num.species)
        if (case %in% mono.test.indices) {
          M.mono.test <- MonoControlRII(b.boot.long, num.species)
        } else {
          M.mono.test <- NA
        }
      } else if (case %in% mono.indices) {
        M.boot <- MonoControlRII(b.boot.long, num.species)
        M.mono.test <- NA
      }
    }
    diag(M.boot) <- NA
    #
    # CALCULATE NETWORK METRICS
    # using bootstrapped networks
    #
    if (!all(is.na(M.mono.test))) {
      diag(M.mono.test) <- NA
      strength.mono.test[iteration] <- MeanStrength(M.mono.test)
      connectance.mono.test[iteration] <- Connectance(M.mono.test)
      M.fac.ind <- which(M.mono.test > 0, arr.ind = TRUE)
      M.fac <- matrix(NA, num.species, num.species)
      M.fac[M.fac.ind] <- M.mono.test[M.fac.ind]
      M.comp.ind <- which(M.mono.test < 0, arr.ind = TRUE)
      M.comp <- matrix(NA, num.species, num.species)
      M.comp[M.comp.ind] <- M.mono.test[M.comp.ind]
      connectance.fac.mono.test[iteration] <- Connectance(M.fac)
      connectance.comp.mono.test[iteration] <- Connectance(M.comp)
      intransitivity.mono.test[iteration] <- Intransitivity(M.mono.test)
      imbalance.mono.test[iteration] <- Imbalance(M.mono.test)
      asymmetry.mono.test[iteration] <- Asymmetry(M.mono.test)
      indirect.effect.mono.test[iteration] <- IndirectEffect(M.mono.test)
    }
    strength.store[iteration] <- MeanStrength(M.boot)
    connectance.store[iteration] <- Connectance(M.boot)
    M.fac.ind <- which(M.boot > 0, arr.ind = TRUE)
    M.fac <- matrix(NA, num.species, num.species)
    M.fac[M.fac.ind] <- M.boot[M.fac.ind]
    M.comp.ind <- which(M.boot < 0, arr.ind = TRUE)
    M.comp <- matrix(NA, num.species, num.species)
    M.comp[M.comp.ind] <- M.boot[M.comp.ind]
    connectance.fac.store[iteration] <- Connectance(M.fac)
    connectance.comp.store[iteration] <- Connectance(M.comp)
    intransitivity.store[iteration] <- Intransitivity(M.boot)
    imbalance.store[iteration] <- Imbalance(M.boot)
    asymmetry.store[iteration] <- Asymmetry(M.boot)
    indirect.effect.store[iteration] <- IndirectEffect(M.boot)
  }
  #
  # calculate metrics using observed data
  #
  # RII using mean performance values from study
  if (case.names[case] == "1205_Gao_2014") {
<<<<<<< HEAD
    M.obs <- matrix(dat$Metric, nrow = num.species, ncol = num.species)
    write.table(x = M.obs, file = paste(path, "Output/Networks/TrueControl/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
    M.obs.mono.test <- NA
||||||| 2b1eac9
    M.obs <- matrix(dat$Metric, nrow = species, ncol = species)
=======
    M.obs <- matrix(dat$Metric, nrow = num.species, ncol = num.species)
    write.table(x = M.obs, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks/TrueControl/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
    M.obs.mono.test <- NA
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  } else if (case.names[case] == "106_Costa_2003") {
<<<<<<< HEAD
    M.obs <- SpecialCaseRII.Costa(dat, num.species, type = "observed")
    write.table(x = M.obs, file = paste(path, "Output/Networks/TrueControl/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
    M.obs.mono.test <- MonoControlRII(dat, num.species)
    write.table(x = M.obs.mono.test, file = paste(path, "Output/Networks/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
||||||| 2b1eac9
    rii.obs <- c()
    one.one <- (dat$Metric[11] - dat$Metric[8]) / (dat$Metric[11] + dat$Metric[8])
    two.one <- (dat$Metric[10] - dat$Metric[7]) / (dat$Metric[10] + dat$Metric[7])
    three.one <- (dat$Metric[12] - dat$Metric[9]) / (dat$Metric[12] + dat$Metric[9])
    one.two <- (dat$Metric[5] - dat$Metric[1]) / (dat$Metric[5] + dat$Metric[1])
    two.two <- (dat$Metric[4] - dat$Metric[2]) / (dat$Metric[4] + dat$Metric[2])
    three.two <- (dat$Metric[6] - dat$Metric[3]) / (dat$Metric[6] + dat$Metric[3])
    one.three <- (dat$Metric[17] - dat$Metric[14]) / (dat$Metric[17] + dat$Metric[14])
    two.three <- (dat$Metric[16] - dat$Metric[13]) / (dat$Metric[16] + dat$Metric[13])
    three.three <- (dat$Metric[18] - dat$Metric[15]) / (dat$Metric[18] + dat$Metric[15])
    rii.obs <- c(one.one, one.two, one.three, two.one, two.two, two.three, three.one, three.two, three.three)
    M.obs <- matrix(rii.obs, nrow = species, ncol = species, byrow = TRUE)
=======
    M.obs <- SpecialCaseRII.Costa(dat, num.species, type = "observed")
    write.table(x = M.obs, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks/TrueControl/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
    M.obs.mono.test <- MonoControlRII(dat, num.species)
    write.table(x = M.obs.mono.test, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  } else if (case.names[case] == "1507_Saccone_2010") {
<<<<<<< HEAD
    M.obs <- SpecialCaseRII.Saccone(dat, num.species, type = "observed")
    write.table(x = M.obs, file = paste(path, "Output/Networks/TrueControl/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
    M.obs.mono.test <- NA
  } else if (case %in% ctrl.indices) {
    M.obs <- TrueControlRII(dat, num.species)
    write.table(x = M.obs, file = paste(path, "Output/Networks/TrueControl/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
    if (case %in% mono.test.indices) {
      M.obs.mono.test <- MonoControlRII(dat, num.species)
      write.table(x = M.obs.mono.test, file = paste(path, "Output/Networks/", case.names[case], ".csv", sep = ""),
                  sep = ",", row.names = FALSE, col.names = FALSE)
    } else {
      M.obs.mono.test <- NA
    }
  } else if (case %in% mono.indices) {
    M.obs <- MonoControlRII(dat, num.species)
    M.obs.mono.test <- NA
    write.table(x = M.obs, file = paste(path, "Output/Networks/", case.names[case], ".csv", sep = ""), sep = ",",
                row.names = FALSE, col.names = FALSE)
  }
  diag(M.obs) <- NA
  strength.store[iteration + 1] <- MeanStrength(M.obs)
  connectance.store[iteration + 1] <- Connectance(M.obs)
  M.fac.ind <- which(M.obs > 0, arr.ind = TRUE)
  M.fac <- matrix(NA, num.species, num.species)
  M.fac[M.fac.ind] <- M.obs[M.fac.ind]
  M.comp.ind <- which(M.obs < 0, arr.ind = TRUE)
  M.comp <- matrix(NA, num.species, num.species)
  M.comp[M.comp.ind] <- M.obs[M.comp.ind]
  connectance.fac.store[iteration + 1] <- Connectance(M.fac)
  connectance.comp.store[iteration + 1] <- Connectance(M.comp)
  intransitivity.store[iteration + 1] <- Intransitivity(M.obs)
  imbalance.store[iteration + 1] <- Imbalance(M.obs)
  asymmetry.store[iteration + 1] <- Asymmetry(M.obs)
  indirect.effect.store[iteration + 1] <- IndirectEffect(M.obs)
  observed.metrics <- c(strength.store[iteration + 1], connectance.store[iteration + 1], connectance.fac.store[iteration + 1], connectance.comp.store[iteration + 1], 
                        imbalance.store[iteration + 1], asymmetry.store[iteration + 1], indirect.effect.store[iteration + 1], intransitivity.store[iteration + 1])
  names(observed.metrics) <- metric.names
  #
  # MANIPULATE AND STORE BOOTSTRAP OUTPUT
  #
  if (!all(is.na(M.obs.mono.test))) {
    # studies with a true control have sufficient data to calculate RII using monoculture as a control
    # calculate this and compare metrics to those calculated with true control RII
    diag(M.obs.mono.test) <- NA
    strength.mono.test[iteration + 1] <- MeanStrength(M.obs.mono.test)
    connectance.mono.test[iteration + 1] <- Connectance(M.obs.mono.test)
    M.fac.ind <- which(M.obs.mono.test > 0, arr.ind = TRUE)
    M.fac <- matrix(NA, num.species, num.species)
    M.fac[M.fac.ind] <- M.obs.mono.test[M.fac.ind]
    M.comp.ind <- which(M.obs.mono.test < 0, arr.ind = TRUE)
    M.comp <- matrix(NA, num.species, num.species)
    M.comp[M.comp.ind] <- M.obs.mono.test[M.comp.ind]
    connectance.fac.mono.test[iteration + 1] <- Connectance(M.fac)
    connectance.comp.mono.test[iteration + 1] <- Connectance(M.comp)
    intransitivity.mono.test[iteration + 1] <- Intransitivity(M.obs.mono.test)
    imbalance.mono.test[iteration + 1] <- Imbalance(M.obs.mono.test)
    asymmetry.mono.test[iteration + 1] <- Asymmetry(M.obs.mono.test)
    indirect.effect.mono.test[iteration + 1] <- IndirectEffect(M.obs.mono.test)
    observed.metrics.mono.test <- c(strength.mono.test[iteration + 1], connectance.mono.test[iteration + 1], connectance.fac.mono.test[iteration + 1], 
                                    connectance.comp.mono.test[iteration + 1], imbalance.mono.test[iteration + 1], asymmetry.mono.test[iteration + 1],
                                    indirect.effect.mono.test[iteration + 1], intransitivity.mono.test[iteration + 1])
    names(observed.metrics.mono.test) <- metric.names
    # store bootstrap distributions for all network metrics
    bootstrap.mono.test <- data.frame(strength.mono.test, connectance.mono.test, connectance.fac.mono.test, connectance.comp.mono.test, imbalance.mono.test,
                                      asymmetry.mono.test, indirect.effect.mono.test, intransitivity.mono.test)
    colnames(bootstrap.mono.test) <- metric.names
    bootstrap.mono.test.mean <- apply(bootstrap.mono.test, 2, function(x) mean(x, na.rm = TRUE))
    # diagnostic plots to assess bias and variance for each bootstrapped metric
    pdf(file = paste(path, "Output/Figures/Diagnostics/boot_", case.names[case], "_MonoTest.pdf", sep = ""))
    par(mfrow = c(4, 2))
    par(cex = 0.6)
    par(mar = c(2, 2, 2, 2))
    for (m in 1:ncol(bootstrap.mono.test)) {
      if (all(is.na(bootstrap.mono.test[, m]) | is.nan(bootstrap.mono.test[, m]))) {
        next
      }
      hist(bootstrap.mono.test[, m], main = metric.names[m], xlab = "Bootstrap values")
      abline(v = observed.metrics.mono.test[m], col = "red")
      abline(v = bootstrap.mono.test.mean[m], col = "blue")
      legend(x = "topright", legend = c("Observed mean", "Bootstrap mean"), col = c("red", "blue"), lty = c(1))
||||||| 2b1eac9
    one.one <- (dat$Metric[1] - dat$Metric[2]) / (dat$Metric[1] + dat$Metric[2])
    one.two <- (dat$Metric[3] - dat$Metric[4]) / (dat$Metric[3] + dat$Metric[4])
    one.four <- (dat$Metric[5] - dat$Metric[6]) / (dat$Metric[5] + dat$Metric[6])
    two.one <- (dat$Metric[7] - dat$Metric[8]) / (dat$Metric[7] + dat$Metric[8])
    two.two <- (dat$Metric[9] - dat$Metric[10]) / (dat$Metric[9] + dat$Metric[10])
    two.four <- (dat$Metric[11] - dat$Metric[12]) / (dat$Metric[11] + dat$Metric[12])
    three.one <- (dat$Metric[13] - dat$Metric[14]) / (dat$Metric[13] + dat$Metric[14])
    three.two <- (dat$Metric[15] - dat$Metric[16]) / (dat$Metric[15] + dat$Metric[16])
    four.one <- (dat$Metric[19] - dat$Metric[20]) / (dat$Metric[19] + dat$Metric[20])
    four.two <- (dat$Metric[21] - dat$Metric[22]) / (dat$Metric[21] + dat$Metric[22])
    four.four <- (dat$Metric[23] - dat$Metric[24]) / (dat$Metric[23] + dat$Metric[24])
    rii.obs <- c(one.one, one.two, NA, one.four, two.one, two.two, NA, two.four, three.one, three.two, NA, NA, four.one, four.two, NA, four.four)
    M.obs <- matrix(rii.obs, nrow = species, ncol = species, byrow = TRUE)
  } else if (case %in% b.ctrl) {
    for (i in 1:species) {
      ctrl <- which(dat$Target == i & dat$Neighbor == 0)
      for (j in 1:species) {
        mix <- which(dat$Target == i & dat$Neighbor == j)
        if (dat$Metric[mix] == 0) {
          rii.obs[mix - i] <- NA
        } else {
          rii.obs[mix - i] <- (dat$Metric[mix] - dat$Metric[ctrl]) / (dat$Metric[mix] + dat$Metric[ctrl])
        }
      }
    }
    M.obs <- matrix(rii.obs, nrow = species, ncol = species, byrow = TRUE)
  } else {
    rii.obs <- c()
    for (i in 1:species) {
      mono <- which(dat$Target == i & dat$Neighbor == i)
      for (j in 1:species) {
        mix <- which(dat$Target == i & dat$Neighbor == j)
        rii.obs[mix] <- (dat$Metric[mix] - dat$Metric[mono]) / (dat$Metric[mix] + dat$Metric[mono])
      }
=======
    M.obs <- SpecialCaseRII.Saccone(dat, num.species, type = "observed")
    write.table(x = M.obs, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks/TrueControl/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
    M.obs.mono.test <- NA
  } else if (case %in% ctrl.indices) {
    M.obs <- TrueControlRII(dat, num.species)
    write.table(x = M.obs, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks/TrueControl/", case.names[case], ".csv", sep = ""),
                sep = ",", row.names = FALSE, col.names = FALSE)
    if (case %in% mono.test.indices) {
      M.obs.mono.test <- MonoControlRII(dat, num.species)
      write.table(x = M.obs.mono.test, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks/", case.names[case], ".csv", sep = ""),
                  sep = ",", row.names = FALSE, col.names = FALSE)
    } else {
      M.obs.mono.test <- NA
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
    }
<<<<<<< HEAD
    dev.off()
    observed.metrics.mono.test <- reshape2::melt(observed.metrics.mono.test)
    # calculate 95% credible intervals and standard deviation from bootstrap distribution
    bootstrap.mono.test.mean <- reshape2::melt(bootstrap.mono.test.mean)
    bootstrap.mono.test.sd <- reshape2::melt(apply(bootstrap.mono.test, 2, function(x) sd(x, na.rm = TRUE)))
    bootstrap.mono.test.cilb <- reshape2::melt(apply(bootstrap.mono.test, 2, function(x) quantile(x, probs = c(0.025), na.rm = TRUE)))
    bootstrap.mono.test.ciub <- reshape2::melt(apply(bootstrap.mono.test, 2, function(x) quantile(x, probs = c(0.975), na.rm = TRUE)))
    mono.test.row <- data.frame(Network = case.names[case], MetricName = row.names(observed.metrics.mono.test), 
                                ObservedMean = observed.metrics.mono.test$value, BootstrapMean = bootstrap.mono.test.mean$value, 
                                BootstrapSD = bootstrap.mono.test.sd$value, BootstrapCIL = bootstrap.mono.test.cilb$value,
                                BootstrapCIU = bootstrap.mono.test.ciub$value)
    mono.test.df <- rbind(mono.test.df, mono.test.row)
||||||| 2b1eac9
    M.obs <- matrix(rii.obs, nrow = species, ncol = species, byrow = TRUE)
=======
  } else if (case %in% mono.indices) {
    M.obs <- MonoControlRII(dat, num.species)
    M.obs.mono.test <- NA
    write.table(x = M.obs, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks/", case.names[case], ".csv", sep = ""), sep = ",",
                row.names = FALSE, col.names = FALSE)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  }
<<<<<<< HEAD
||||||| 2b1eac9
  write.table(x = M.obs, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Complete/", case.names[case], ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
  diag(M.obs) <- NA
  M <- t(M.obs)
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/topology.R")
  strength <- mean.s
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/connectance.R")
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/transitivity.R")
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/asymmetry.R")
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/indirecteffect.R")
  ind.eff <- mean.ind.eff
  obs.vec <- c(strength, C.qw, asymm.diff, ind.eff, r.intrans)
  names(obs.vec) <- metric.names
  
  # STORE BOOTSTRAP OUTPUT
=======
  diag(M.obs) <- NA

  strength.store[iteration + 1] <- MeanStrength(M.obs)
  connectance.store[iteration + 1] <- Connectance(M.obs)
  M.fac.ind <- which(M.obs > 0, arr.ind = TRUE)
  M.fac <- matrix(NA, num.species, num.species)
  M.fac[M.fac.ind] <- M.obs[M.fac.ind]
  M.comp.ind <- which(M.obs < 0, arr.ind = TRUE)
  M.comp <- matrix(NA, num.species, num.species)
  M.comp[M.comp.ind] <- M.obs[M.comp.ind]
  connectance.fac.store[iteration + 1] <- Connectance(M.fac)
  connectance.comp.store[iteration + 1] <- Connectance(M.comp)
  intransitivity.store[iteration + 1] <- Intransitivity(M.obs)
  imbalance.store[iteration + 1] <- Imbalance(M.obs)
  asymmetry.store[iteration + 1] <- Asymmetry(M.obs)
  indirect.effect.store[iteration + 1] <- IndirectEffect(M.obs)
  observed.metrics <- c(strength.store[iteration + 1], connectance.store[iteration + 1], connectance.fac.store[iteration + 1], connectance.comp.store[iteration + 1], 
                        imbalance.store[iteration + 1], asymmetry.store[iteration + 1], indirect.effect.store[iteration + 1], intransitivity.store[iteration + 1])
  names(observed.metrics) <- metric.names
  
  # MANIPULATE AND STORE BOOTSTRAP OUTPUT
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  #
<<<<<<< HEAD
  # store bootstrap distributions for all network metrics
  bootstrap.table <- data.frame(strength.store, connectance.store, connectance.fac.store, connectance.comp.store, imbalance.store, asymmetry.store, 
                                indirect.effect.store, intransitivity.store)
  colnames(bootstrap.table) <- metric.names
||||||| 2b1eac9
=======
  if (!all(is.na(M.obs.mono.test))) {
    # studies with a true control have sufficient data to calculate RII using monoculture as a control
    # calculate this and compare metrics to those calculated with true control RII
    diag(M.obs.mono.test) <- NA
    strength.mono.test[iteration + 1] <- MeanStrength(M.obs.mono.test)
    connectance.mono.test[iteration + 1] <- Connectance(M.obs.mono.test)
    M.fac.ind <- which(M.obs.mono.test > 0, arr.ind = TRUE)
    M.fac <- matrix(NA, num.species, num.species)
    M.fac[M.fac.ind] <- M.obs.mono.test[M.fac.ind]
    M.comp.ind <- which(M.obs.mono.test < 0, arr.ind = TRUE)
    M.comp <- matrix(NA, num.species, num.species)
    M.comp[M.comp.ind] <- M.obs.mono.test[M.comp.ind]
    connectance.fac.mono.test[iteration + 1] <- Connectance(M.fac)
    connectance.comp.mono.test[iteration + 1] <- Connectance(M.comp)
    intransitivity.mono.test[iteration + 1] <- Intransitivity(M.obs.mono.test)
    imbalance.mono.test[iteration + 1] <- Imbalance(M.obs.mono.test)
    asymmetry.mono.test[iteration + 1] <- Asymmetry(M.obs.mono.test)
    indirect.effect.mono.test[iteration + 1] <- IndirectEffect(M.obs.mono.test)
    observed.metrics.mono.test <- c(strength.mono.test[iteration + 1], connectance.mono.test[iteration + 1], connectance.fac.mono.test[iteration + 1], 
                                    connectance.comp.mono.test[iteration + 1], imbalance.mono.test[iteration + 1], asymmetry.mono.test[iteration + 1],
                                    indirect.effect.mono.test[iteration + 1], intransitivity.mono.test[iteration + 1])
    names(observed.metrics.mono.test) <- metric.names
    # store bootstrap distributions for all network metrics
    bootstrap.mono.test <- data.frame(strength.mono.test, connectance.mono.test, connectance.fac.mono.test, connectance.comp.mono.test, imbalance.mono.test,
                                      asymmetry.mono.test, indirect.effect.mono.test, intransitivity.mono.test)
    colnames(bootstrap.mono.test) <- metric.names
    bootstrap.mono.test.mean <- apply(bootstrap.mono.test, 2, function(x) mean(x, na.rm = TRUE))
    # diagnostic plots to assess bias and variance for each bootstrapped metric
    pdf(file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Diagnostics/boot_", case.names[case], "_MonoTest.pdf", sep = ""))
    par(mfrow = c(4, 2))
    par(cex = 0.6)
    par(mar = c(2, 2, 2, 2))
    for (m in 1:ncol(bootstrap.mono.test)) {
      if (all(is.na(bootstrap.mono.test[, m]) | is.nan(bootstrap.mono.test[, m]))) {
        next
      }
      hist(bootstrap.mono.test[, m], main = metric.names[m], xlab = "Bootstrap values")
      abline(v = observed.metrics.mono.test[m], col = "red")
      abline(v = bootstrap.mono.test.mean[m], col = "blue")
      legend(x = "topright", legend = c("Observed mean", "Bootstrap mean"), col = c("red", "blue"), lty = c(1))
    }
    dev.off()
    observed.metrics.mono.test <- melt(observed.metrics.mono.test)
    # calculate 95% credible intervals and standard deviation from bootstrap distribution
    bootstrap.mono.test.mean <- melt(bootstrap.mono.test.mean)
    bootstrap.mono.test.sd <- melt(apply(bootstrap.mono.test, 2, function(x) sd(x, na.rm = TRUE)))
    bootstrap.mono.test.cilb <- melt(apply(bootstrap.mono.test, 2, function(x) quantile(x, probs = c(0.025), na.rm = TRUE)))
    bootstrap.mono.test.ciub <- melt(apply(bootstrap.mono.test, 2, function(x) quantile(x, probs = c(0.975), na.rm = TRUE)))
    mono.test.row <- data.frame(Network = case.names[case], MetricName = row.names(observed.metrics.mono.test), ObservedMean = observed.metrics.mono.test$value, 
                                BootstrapMean = bootstrap.mono.test.mean$value, BootstrapSD = bootstrap.mono.test.sd$value, BootstrapCIL = bootstrap.mono.test.cilb$value,
                                BootstrapCIU = bootstrap.mono.test.ciub$value)
    mono.test.df <- rbind(mono.test.df, mono.test.row)
  }
  
  # store bootstrap distributions for all network metrics
  bootstrap.table <- data.frame(strength.store, connectance.store, connectance.fac.store, connectance.comp.store, imbalance.store, asymmetry.store, 
                                indirect.effect.store, intransitivity.store)
  colnames(bootstrap.table) <- metric.names
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  # calculate 95% credible intervals and standard deviation from bootstrap distribution
  bootstrap.mean <- apply(bootstrap.table, 2, function(x) mean(x, na.rm = TRUE))
  bootstrap.sd <- apply(bootstrap.table, 2, function(x) sd(x, na.rm = TRUE))
  bootstrap.cilb <- apply(bootstrap.table, 2, function(x) quantile(x, probs = c(0.025), na.rm = TRUE))
  bootstrap.ciub <- apply(bootstrap.table, 2, function(x) quantile(x, probs = c(0.975), na.rm = TRUE))
  # diagnostic plots to assess bias and variance for each bootstrapped metric
<<<<<<< HEAD
  pdf(file = paste(path, "Output/Figures/Diagnostics/boot_", case.names[case], ".pdf", sep = ""))
||||||| 2b1eac9
  pdf(file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Diagnostics/boot_", case.names[case], ".pdf", sep = ""))
=======
  pdf(file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Diagnostics/boot_", case.names[case], ".pdf", sep = ""))
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  par(mfrow = c(4, 2))
  par(cex = 0.6)
  par(mar = c(2, 2, 2, 2))
  for (m in 1:ncol(bootstrap.table)) {
    if (all(is.na(bootstrap.table[, m]) | is.nan(bootstrap.table[, m]))) {
      next
    }
    hist(bootstrap.table[, m], main = metric.names[m], xlab = "Bootstrap values")
    abline(v = observed.metrics[m], col = "red")
    abline(v = bootstrap.mean[m], col = "blue")
    legend(x = "topright", legend = c("Observed mean", "Bootstrap mean"), col = c("red", "blue"), lty = c(1))
  }
  dev.off()
<<<<<<< HEAD
  observed.metrics <- reshape2::melt(observed.metrics)
  bootstrap.mean <- reshape2::melt(bootstrap.mean)
  bootstrap.sd <- reshape2::melt(bootstrap.sd)
  bootstrap.cilb <- reshape2::melt(bootstrap.cilb)
  bootstrap.ciub <- reshape2::melt(bootstrap.ciub)
||||||| 2b1eac9
=======
  observed.metrics <- melt(observed.metrics)
  bootstrap.mean <- melt(bootstrap.mean)
  bootstrap.sd <- melt(bootstrap.sd)
  bootstrap.cilb <- melt(bootstrap.cilb)
  bootstrap.ciub <- melt(bootstrap.ciub)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  # store output with all other cases
  metrics.row <- data.frame(Network = case.names[case], MetricName = row.names(observed.metrics), ObservedMean = observed.metrics$value, BootstrapMean = bootstrap.mean$value,
                         BootstrapSD = bootstrap.sd$value, BootstrapCIL = bootstrap.cilb$value, BootstrapCIU = bootstrap.ciub$value)
  metrics.df <- rbind(metrics.df, metrics.row)
  print(paste("Network ", case, " ", case.names[case], " is complete at ", Sys.time(), sep = ""))
}
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# STORE OUTPUT ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
<<<<<<< HEAD
# subset treatments
metrics.treatments <- mono.test.df[mono.test.df$Network %in% case.names[treatment.indices], ]
metrics.treatments <- rbind(metrics.treatments, mono.test.df[mono.test.df$Network %in% case.names[treatment.ctrl.indices], ])
metrics.treatments <- rbind(metrics.treatments, metrics.df[(metrics.df$Network %in% case.names[treatment.indices]) & (metrics.df$Network %in% case.names[mono.indices]), ])
metrics.treatments <- rbind(metrics.treatments, metrics.df[(metrics.df$Network %in% case.names[treatment.ctrl.indices]) & (metrics.df$Network %in% case.names[mono.indices]), ])
metrics.treatments$CompetitionComparison <- "Monoculture"
metrics.treatments.true.ctrl <- metrics.df[(metrics.df$Network %in% case.names[treatment.indices]) & (metrics.df$Network %in% case.names[ctrl.indices]), ]
metrics.treatments.true.ctrl <- rbind(metrics.treatments.true.ctrl, metrics.df[(metrics.df$Network %in% case.names[treatment.ctrl.indices]) & (metrics.df$Network %in% case.names[ctrl.indices]), ])
metrics.treatments.true.ctrl$CompetitionComparison <- "True control"
metrics.treatments <- rbind(metrics.treatments, metrics.treatments.true.ctrl)
#
# subset monoculture control
metrics.mono <- metrics.df[!(metrics.df$Network %in% case.names[treatment.indices]) & (metrics.df$Network %in% case.names[mono.indices]), ]
metrics.mono.test <- mono.test.df[!(mono.test.df$Network %in% case.names[treatment.indices]), ]
metrics.notreatments <- rbind(metrics.mono, metrics.mono.test)
# subset true control
metrics.true.ctrl <- metrics.df[!(metrics.df$Network %in% case.names[treatment.indices]) & (metrics.df$Network %in% case.names[ctrl.indices]), ]
#
# write output to file
write.csv(x = metrics.notreatments, file = paste(path, "Output/NetworkMetrics.csv", sep = ""))
write.csv(x = metrics.treatments, file = paste(path, "Output/NetworkMetricsTreatments.csv", sep = ""))
write.csv(x = metrics.true.ctrl, file = paste(path, "Output/NetworkMetrics_TrueCtrlOnly.csv", sep = ""))
||||||| 2b1eac9
meta.treatments <- meta[(net.num * length(levels(meta$metric)) + 1):nrow(meta), ]
meta.notreatments <- meta[1:(net.num * length(levels(meta$metric))), ]
write.csv(x = meta.notreatments, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Output/allMetrics.csv")
write.csv(x = meta.treatments, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Output/allMetrics_Treatments.csv")

=======
metrics.treatments <- mono.test.df[mono.test.df$Network %in% case.names[treatment.indices], ]
metrics.treatments <- rbind(metrics.treatments, mono.test.df[mono.test.df$Network %in% case.names[treatment.ctrl.indices], ])
metrics.treatments <- rbind(metrics.treatments, metrics.df[(metrics.df$Network %in% case.names[treatment.indices]) & (metrics.df$Network %in% case.names[mono.indices]), ])
metrics.treatments <- rbind(metrics.treatments, metrics.df[(metrics.df$Network %in% case.names[treatment.ctrl.indices]) & (metrics.df$Network %in% case.names[mono.indices]), ])
metrics.treatments$CompetitionComparison <- "Monoculture"
metrics.treatments.true.ctrl <- metrics.df[(metrics.df$Network %in% case.names[treatment.indices]) & (metrics.df$Network %in% case.names[ctrl.indices]), ]
metrics.treatments.true.ctrl <- rbind(metrics.treatments.true.ctrl, metrics.df[(metrics.df$Network %in% case.names[treatment.ctrl.indices]) & (metrics.df$Network %in% case.names[ctrl.indices]), ])
metrics.treatments.true.ctrl$CompetitionComparison <- "True control"
metrics.treatments <- rbind(metrics.treatments, metrics.treatments.true.ctrl)

metrics.mono <- metrics.df[!(metrics.df$Network %in% case.names[treatment.indices]) & (metrics.df$Network %in% case.names[mono.indices]), ]
metrics.mono.test <- mono.test.df[!(mono.test.df$Network %in% case.names[treatment.indices]), ]
metrics.notreatments <- rbind(metrics.mono, metrics.mono.test)

metrics.true.ctrl <- metrics.df[!(metrics.df$Network %in% case.names[treatment.indices]) & (metrics.df$Network %in% case.names[ctrl.indices]), ]

write.csv(x = metrics.notreatments, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/NetworkMetrics.csv")
write.csv(x = metrics.treatments, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/NetworkMetricsTreatments.csv")
write.csv(x = metrics.true.ctrl, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/NetworkMetrics_TrueCtrlOnly.csv")
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae


