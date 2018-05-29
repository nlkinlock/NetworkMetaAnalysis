# BAYESIAN BOOTSTRAP: CALCULATE RII AND NETWORK METRICS FOR ALL NETWORKS
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD NETWORKS ---------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load data frames with plant performance and variance in performance for all pairwise combinations
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
# mean, standard deviation, and credible intervals
metric.names <- c("strength", "connectance", "r", "asymm_diff", "indirect_effect", "relative_intransitivity")
meta <- data.frame(network = character(), metric = character(), obs = numeric(), mean = numeric(), sd = numeric(), CI.l = numeric(), CI.u = numeric(), stringsAsFactors = FALSE)


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
  dat <- b.all[[case]]
  # study-specific parameters
  #
  # number of species in study
  species <- max(unique(dat$Target))
  # number of different treatments in study
  treatment <- nrow(dat[dat$Metric > 0, ])
  # sample size (within treatment) in study
  sample_size <- max(dat$N)
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
  
  # create empty data frames for network metrics
  s.store <- numeric(length = R)
  C.qw.store <- numeric(length = R)
  r.intrans.store <- numeric(length = R)
  r.store <- numeric(length = R)
  asymm.diff.store <- numeric(length = R)
  ind.eff.store <- numeric(length = R)
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
      M.boot <- matrix(b.boot, nrow = species, ncol = species)
    } else {
      b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample_size, replace = TRUE, prob = weights[iteration, ]))
      b.boot.long <- melt(b.boot)
      b.boot.long <- data.frame(Target = rep(targets, each = sample_size), Neighbor = rep(neighbors, each = sample_size), Metric = b.boot.long[, 3])
      rii.boot <- c()
      count <- 1
      if (case.names[case] == "106_Costa_2003") {
        rii.boot <- c()
        rii.boot[1] <- (mean(b.boot[, 11]) - mean(b.boot[, 8])) / (mean(b.boot[, 11]) + mean(b.boot[, 8]))
        rii.boot[2] <- (mean(b.boot[, 5]) - mean(b.boot[, 1])) / (mean(b.boot[, 5]) + mean(b.boot[, 1]))
        rii.boot[3] <- (mean(b.boot[, 17]) - mean(b.boot[, 14])) / (mean(b.boot[, 17]) + mean(b.boot[, 14]))
        rii.boot[4] <- (mean(b.boot[, 10]) - mean(b.boot[, 7])) / (mean(b.boot[, 10]) + mean(b.boot[, 7]))
        rii.boot[5] <- (mean(b.boot[, 4]) - mean(b.boot[, 2])) / (mean(b.boot[, 4]) + mean(b.boot[, 2]))
        rii.boot[6] <- (mean(b.boot[, 16]) - mean(b.boot[, 13])) / (mean(b.boot[, 16]) + mean(b.boot[, 13]))
        rii.boot[7] <- (mean(b.boot[, 12]) - mean(b.boot[, 9])) / (mean(b.boot[, 12]) + mean(b.boot[, 9]))
        rii.boot[8] <- (mean(b.boot[, 6]) - mean(b.boot[, 3])) / (mean(b.boot[, 6]) + mean(b.boot[, 3]))
        rii.boot[9] <- (mean(b.boot[, 18]) - mean(b.boot[, 15])) / (mean(b.boot[, 18]) + mean(b.boot[, 15]))
        M.boot <- matrix(rii.boot, nrow = species, ncol = species, byrow = TRUE)
      } else if (case.names[case] == "1507_Saccone_2010") {
        rii.boot <- c()
        rii.boot[1] <- (mean(b.boot[, 1]) - mean(b.boot[, 2])) / (mean(b.boot[, 1]) + mean(b.boot[, 2]))
        rii.boot[2] <- (mean(b.boot[, 3]) - mean(b.boot[, 4])) / (mean(b.boot[, 3]) + mean(b.boot[, 4]))
        rii.boot[3] <- NA
        rii.boot[4] <- (mean(b.boot[, 5]) - mean(b.boot[, 6])) / (mean(b.boot[, 5]) + mean(b.boot[, 6]))
        rii.boot[5] <- (mean(b.boot[, 7]) - mean(b.boot[, 8])) / (mean(b.boot[, 7]) + mean(b.boot[, 8]))
        rii.boot[6] <- (mean(b.boot[, 9]) - mean(b.boot[, 10])) / (mean(b.boot[, 9]) + mean(b.boot[, 10]))
        rii.boot[7] <- NA
        rii.boot[8] <- (mean(b.boot[, 11]) - mean(b.boot[, 12])) / (mean(b.boot[, 11]) + mean(b.boot[, 12]))
        rii.boot[9] <- (mean(b.boot[, 13]) - mean(b.boot[, 14])) / (mean(b.boot[, 13]) + mean(b.boot[, 14]))
        rii.boot[10] <- (mean(b.boot[, 15]) - mean(b.boot[, 16])) / (mean(b.boot[, 15]) + mean(b.boot[, 16]))
        rii.boot[11] <- NA
        rii.boot[12] <- NA
        rii.boot[13] <- (mean(b.boot[, 18]) - mean(b.boot[, 19])) / (mean(b.boot[, 18]) + mean(b.boot[, 19]))
        rii.boot[14] <- (mean(b.boot[, 20]) - mean(b.boot[, 21])) / (mean(b.boot[, 20]) + mean(b.boot[, 21]))
        rii.boot[15] <- NA
        rii.boot[16] <- (mean(b.boot[, 22]) - mean(b.boot[, 23])) / (mean(b.boot[, 22]) + mean(b.boot[, 23]))
        M.boot <- matrix(rii.boot, nrow = species, ncol = species, byrow = TRUE)
      } else if (case %in% b.ctrl) {
        for (i in 1:species) {
          ctrl <- which(b.boot.long$Target == i & b.boot.long$Neighbor == 0)
          for (j in 1:species) {
            mix <- which(b.boot.long$Target == i & b.boot.long$Neighbor == j)
            if (length(mix) == 0 | length(ctrl) == 0) {
              rii.boot[count] <- NA
            } else {
              rii.boot[count] <- (mean(b.boot.long$Metric[mix]) - mean(b.boot.long$Metric[ctrl])) / (mean(b.boot.long$Metric[mix]) + mean(b.boot.long$Metric[ctrl]))
            }
            count <- count + 1
          }
        }
        M.boot <- matrix(rii.boot, nrow = species, ncol = species, byrow = TRUE)
      } else {
        for (i in 1:species) {
          mono <- which(b.boot.long$Target == i & b.boot.long$Neighbor == i)
          for (j in 1:species) {
            mix <- which(b.boot.long$Target == i & b.boot.long$Neighbor == j)
            rii.boot[count] <- (mean(b.boot.long$Metric[mix]) - mean(b.boot.long$Metric[mono])) / (mean(b.boot.long$Metric[mix]) + mean(b.boot.long$Metric[mono]))
            count <- count + 1
          }
        }
        rii.boot[is.nan(rii.boot)] <- NA
        M.boot <- matrix(rii.boot, nrow = species, ncol = species, byrow = TRUE)
        diag(M.boot) <- NA
      }
    }
    M <- t(M.boot)
    #
    # CALCULATE NETWORK METRICS
    # using bootstrapped networks
    #
    # node characteristics (weight, in/out strength) and distribution fits
    source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/topology.R")
    s.store[iteration] <- mean.s
    # connectance
    source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/connectance.R")
    C.qw.store[iteration] <- C.qw
    # relative intransitivity
    source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/transitivity.R")
    r.intrans.store[iteration] <- r.intrans
    # asymmetry (correlation coefficient r, mean difference in weights)
    source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/asymmetry.R")
    r.store[iteration] <- r
    asymm.diff.store[iteration] <- asymm.diff
    # indirect effect
    source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/indirecteffect.R")
    ind.eff.store[iteration] <- mean.ind.eff
  }
  # store bootstrap distributions for all network metrics
  boot.table <- data.frame(s.store, C.qw.store, r.store, asymm.diff.store, ind.eff.store, r.intrans.store)
  colnames(boot.table) <- metric.names
  #
  # calculate metrics using observed data
  #
  # RII using mean performance values from study
  rii.obs <- c()
  if (case.names[case] == "1205_Gao_2014") {
    M.obs <- matrix(dat$Metric, nrow = species, ncol = species)
  } else if (case.names[case] == "106_Costa_2003") {
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
  } else if (case.names[case] == "1507_Saccone_2010") {
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
    }
    M.obs <- matrix(rii.obs, nrow = species, ncol = species, byrow = TRUE)
    diag(M.obs) <- NA
  }
  write.table(x = M.obs, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Complete/", case.names[case], ".csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
  M <- t(M.obs)
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/topology.R")
  strength <- mean.s
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/connectance.R")
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/transitivity.R")
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/asymmetry.R")
  source(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/indirecteffect.R")
  ind.eff <- mean.ind.eff
  obs.vec <- c(strength, C.qw, r, asymm.diff, ind.eff, r.intrans)
  names(obs.vec) <- metric.names
  
  # STORE BOOTSTRAP OUTPUT
  #
  # calculate 95% credible intervals and standard deviation from bootstrap distribution
  mean.vec <- apply(boot.table, 2, function(x) mean(x, na.rm = TRUE))
  sd.vec <- apply(boot.table, 2, function(x) sd(x, na.rm = TRUE))
  cilb.vec <- apply(boot.table, 2, function(x) quantile(x, probs = c(0.025), na.rm = TRUE))
  ciub.vec <- apply(boot.table, 2, function(x) quantile(x, probs = c(0.975), na.rm = TRUE))
  # diagnostic plots to assess bias and variance for each bootstrapped metric
  pdf(file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Diagnostics/boot_", case.names[case], ".pdf", sep = ""))
  par(mfrow = c(4, 2))
  par(cex = 0.6)
  par(mar = c(2, 2, 2, 2))
  for (m in 1:ncol(boot.table)) {
    if (all(is.na(boot.table[, m]) | is.nan(boot.table[, m]))) {
      next
    }
    hist(boot.table[, m], main = metric.names[m], xlab = "Bootstrap values")
    abline(v = obs.vec[m], col = "red")
    abline(v = mean.vec[m], col = "blue")
    legend(x = "topright", legend = c("Observed mean", "Bootstrap mean"), col = c("red", "blue"), lty = c(1))
  }
  dev.off()
  # store output with all other cases
  obs.l <- melt(obs.vec)
  mean.l <- melt(mean.vec)
  sd.l <- melt(sd.vec)
  cilb.l <- melt(cilb.vec)
  ciub.l <- melt(ciub.vec)
  meta.row <- data.frame(network = case.names[case], metric = row.names(obs.l), obs = obs.l$value, mean = mean.l$value, sd = sd.l$value, CI.l = cilb.l$value, CI.u = ciub.l$value)
  meta <- rbind(meta, meta.row)
  print(paste("Network ", case, case.names[case], " is complete"))
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# STORE OUTPUT ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
meta.treatments <- meta[(net.num * length(levels(meta$metric)) + 1):nrow(meta), ]
meta.notreatments <- meta[1:(net.num * length(levels(meta$metric))), ]
write.csv(x = meta.notreatments, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Output/allMetrics.csv")
write.csv(x = meta.treatments, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Output/allMetrics_Treatments.csv")



