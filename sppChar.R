# COMPARE IN- AND OUT-STRENGTHS BASED ON INVASIVE STATUS, C4 PHOTOSYNTHESIS, AND N-FIXATION WITHIN NETWORKS
# COMBINE DIFFERENCES FROM MULTIPLE NETWORKS USING HIERARCHICAL T-TEST
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INITIALIZE MODEL AND FUNCTIONS ----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# model
#
# hierarchical model of differences, parameter of interest is mean difference
diff.char <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/diff_char.jags"
sink("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/diff_char.jags")
cat("
    model {
    for (i in 1:in_N) {
    s_in_obs[i] ~ dnorm(s_in[i], tau_in_obs[i])
    tau_in_obs[i] <- pow(sig_in_obs[i], -2)
    s_in[i] ~ dnorm(mu_in, tau_in)
    }
    sig_in ~ dunif(0, 100)
    tau_in <- pow(sig_in, -2)
    mu_in ~ dnorm(0, 1E-6)

    for (i in 1:out_N) {
    s_out_obs[i] ~ dnorm(s_out[i], tau_out_obs[i])
    tau_out_obs[i] <- pow(sig_out_obs[i], -2)
    s_out[i] ~ dnorm(mu_out, tau_out)
    }
    sig_out ~ dunif(0, 100)
    tau_out <- pow(sig_out, -2)
    mu_out ~ dnorm(0, 1E-6)
    }", fill = TRUE)
sink()

# functions
#
# function to determine difference in mean strengths grouped by character (like a t-test of unequal variances)
diff.jags.func <- function(s.in, sig.in, in.N, s.out, sig.out, out.N) {
  jags.data <- list(in_N = in.N, out_N = out.N, s_out_obs = s.out, sig_out_obs = sig.out,
                    s_in_obs = s.in, sig_in_obs = sig.in)
  jags.inits <- function() {
    list(mu_out = rnorm(1), mu_in = rnorm(1), sig_out = runif(1), sig_in = runif(1))
  }
  jags.pars <- c("mu_out", "mu_in", "sig_out", "sig_in")
  jags.fit <- jags(inits = jags.inits, n.chains = nc, model.file = diff.char, working.directory = getwd(),
                   data = jags.data, parameters.to.save = jags.pars, n.thin = nt, n.iter = ni, n.burnin = nb, DIC = TRUE)
  return(jags.fit)
}

# function to extract mean, variance, confidence intervals from JAGS output, grand mean
diff.extract.func <- function(fit, charname) {
  # extract means and CIs for mu (grand mean) and sigma (sd)
  mu <- grep("mu", row.names(fit$BUGSoutput$summary))
  sigma <- grep("sig", row.names(fit$BUGSoutput$summary))
  sub.output <- fit$BUGSoutput$summary[c(mu, sigma), c(1, 2, 3, 7)]
  output <- data.frame(comparison = rep(charname, nrow(sub.output)), metric = c("In-strength", "Out-strength", "SD in-strength", "SD out-strength"),
                       mean = sub.output[, 1],  sd = sub.output[, 2], CILL = sub.output[, 3], CIUL = sub.output[, 4])
  return(output)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INPUT NETWORKS --------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
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
b.ctrl <- c(1, 2, 8, 12, 13, 14, 16, 17, 19, 22, 23, 29, 30, 31, 32)

# set up list with Unique ID of case and all species
spp.list <- setNames(split(as.character(df$Species), seq(nrow(df))), df$UniqueID)  # split df into list where each element is string with species
spp.list <- lapply(spp.list, function(x) strsplit(x, ", "))  # split string into vector
spp.list <- lapply(spp.list, unlist)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOOP THROUGH CHARACTERS -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# what character should be subsetted?
# invasive status
# C4 photosynthesis status
# N-fixing status
char.names <- c("Invasive", "C3C4", "Nfixing")
comparison.names <- c("out diff", "in diff")
meta.comp <- data.frame(network = character(), character = character(), comparison = character(), obs = numeric(), mean = numeric(), sd = numeric(), CI.l = numeric(), CI.u = numeric(), stringsAsFactors = FALSE)

for (h in 1:length(char.names)) {
  stat.list <- setNames(split(as.character(df[, paste(char.names[h])]), seq(nrow(df))), df$UniqueID)  # split df into list where each element is string with invasives/C4/Nfixers
  stat.list <- lapply(stat.list, function(x) strsplit(x, ", "))  # split string into vector
  stat.list <- lapply(stat.list, function(x) as.numeric(unlist(x)))
  
  # loop to create a list with the unique case ID and the status of each species from each case in each element
  stat.output <- list()
  for (i in 1:length(spp.list)) {
    if (all(stat.list[[i]] == 0)) {
      uniqueid <- rep(as.integer(names(spp.list)[i]), length(spp.list[[i]]))
      status <- rep(0, length(spp.list[[i]]))
      temp <- data.frame(UniqueID = uniqueid, Status = status)
      stat.output[[i]] <- temp
    } else {
      uniqueid <- rep(as.integer(names(spp.list)[i]), length(spp.list[[i]]))
      status <- integer(length = length(spp.list[[i]]))
      status[stat.list[[i]]] <- 1
      status[!stat.list[[i]]] <- 0
      temp <- data.frame(UniqueID = uniqueid, Status = status)
      stat.output[[i]] <- temp
    }
  }
  
  for (case in 1:length(case.names)) {
    char.vec <- stat.output[[case]][, 2]
    if (all(char.vec == 0)) {
      next
    }
    dat <- b.all[[case]]
    species <- max(unique(dat$Target))
    treatment <- nrow(dat[dat$Metric > 0, ])
    sample_size <- max(dat$N)
    if (any(colnames(dat) == "SE")) {
      SD <- dat$SE * sqrt(dat$N)
      dat$SD <- SD
    }
    dat$var <- dat$SD^2
    if (case.names[case] == "1205_Gao_2014") {
      RII.trans <- (dat$Metric + 1) / 2
      RII.sd.trans <- dat$SD / 2
      alpha <- (((1 - RII.trans) / RII.sd.trans^2) - (1 / RII.trans)) * RII.trans^2
      beta <- alpha * ((1 / RII.trans) - 1)
      values <- which(!is.infinite(alpha))
      n <- length(values)
      alpha <- alpha[values]
      beta <- beta[values]
      jags.data <- list(alpha = alpha, beta = beta, N = N, n = n)
      b.sim <- run.jags(model = betamodel, data = jags.data, monitor = c("y"), sample = 1, n.chains = 1, summarise = FALSE)
      b.sim <- coda::as.mcmc(b.sim)
      b.sim <- matrix(data = as.vector(b.sim), nrow = N, ncol = n)
      b.sim <- b.sim * 2 - 1
    } else {
      dat$beta <- dat$var / dat$Metric
      dat$alpha <- dat$var / dat$beta^2
      dat$rate <- 1 / dat$beta
      dat[is.nan(dat$beta), "beta"] <- 0
      dat[is.nan(dat$alpha), "alpha"] <- 0
      dat[is.nan(dat$rate), "rate"] <- 0
      jags.data <- list(alpha = dat$alpha[dat$alpha > 0], rate = dat$rate[dat$rate > 0], N = N, treatment = treatment)
      b.sim <- run.jags(model = gammamodel, data = jags.data, monitor = c("int"), sample = 1, n.chains = 1, summarise = FALSE)
      b.sim <- coda::as.mcmc(b.sim)
      b.sim <- matrix(data = as.vector(b.sim), nrow = N, ncol = treatment)
    }
    targets <- dat$Target[which(dat$Metric != 0)]
    neighbors <- dat$Neighbor[which(dat$Metric != 0)]
    
    # create empty data frames for comparisons
    s.out.store <- numeric(length = R)
    s.in.store <- numeric(length = R)
    
    weights <- matrix(rexp(N * R, 1) , ncol = N, byrow = TRUE)
    weights <- weights / rowSums(weights)
    for (iteration in 1:R) {
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
      # calculate comparisons between mean strength with the character and without
      # on bootstrapped networks
      #
      s.out <- rowSums(M, na.rm = TRUE)
      s.out.store[iteration] <- mean(s.out[char.vec]) / length(s.out[char.vec]) - mean(s.out[-char.vec]) / length(s.out[-char.vec])
      s.in <- colSums(M, na.rm = TRUE)
      s.in.store[iteration] <- mean(s.in[char.vec]) / length(s.in[char.vec]) - mean(s.in[-char.vec]) / length(s.in[-char.vec])
    }
    boot.table <- data.frame(s.out.store, s.in.store)
    colnames(boot.table) <- comparison.names
    
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
    M <- t(M.obs)
    s.out <- rowSums(M, na.rm = TRUE)
    s.out.diff <- mean(s.out[char.vec]) / length(s.out[char.vec]) - mean(s.out[-char.vec]) / length(s.out[-char.vec])
    s.in <- colSums(M, na.rm = TRUE)
    s.in.diff <- mean(s.in[char.vec]) / length(s.in[char.vec]) - mean(s.in[-char.vec]) / length(s.in[-char.vec])
    if (any(c(s.out.diff, s.in.diff) == 0)) {
      next
    }
    obs.vec <- c(s.out.diff, s.in.diff)
    names(obs.vec) <- comparison.names
    
    mean.vec <- apply(boot.table, 2, function(x) mean(x, na.rm = TRUE))
    sd.vec <- apply(boot.table, 2, function(x) sd(x, na.rm = TRUE))
    cilb.vec <- apply(boot.table, 2, function(x) quantile(x, probs = c(0.025), na.rm = TRUE))
    ciub.vec <- apply(boot.table, 2, function(x) quantile(x, probs = c(0.975), na.rm = TRUE))
    
    obs.l <- melt(obs.vec)
    mean.l <- melt(mean.vec)
    sd.l <- melt(sd.vec)
    cilb.l <- melt(cilb.vec)
    ciub.l <- melt(ciub.vec)
    
    meta.comp.row <- data.frame(network = case.names[case], character = char.names[h], comparison = row.names(obs.l), obs = obs.l$value, mean = mean.l$value, sd = sd.l$value, CI.l = cilb.l$value, CI.u = ciub.l$value)
    meta.comp <- rbind(meta.comp, meta.comp.row)
    print(paste("Network ", case, case.names[case], " is complete"))
  }
}
# store output by network
# write.csv(x = meta.comp, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_character_bynetwork.csv")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# HIERARCHICAL T-TEST OF CHAR. DIFFERENCES ------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# differences between invasive and native species
sub.df <- meta.comp[which(meta.comp$character == "Invasive"), ]
inv.fit <- diff.jags.func(s.in = sub.df$obs[which(sub.df$comparison == "in diff")], sig.in = sub.df$sd[which(sub.df$comparison == "in diff")],
                          in.N = length(sub.df$obs[which(sub.df$comparison == "in diff")]), 
                          s.out = sub.df$obs[which(sub.df$comparison == "out diff")], sig.out = sub.df$sd[which(sub.df$comparison == "out diff")], 
                          out.N = length(sub.df$obs[which(sub.df$comparison == "out diff")]))
inv.output <- diff.extract.func(inv.fit, charname = "Invasive status")
# differences between C4 and C3 species
sub.df <- meta.comp[which(meta.comp$character == "C3C4"), ]
C4.fit <- diff.jags.func(s.in = sub.df$obs[which(sub.df$comparison == "in diff")], sig.in = sub.df$sd[which(sub.df$comparison == "in diff")],
                      in.N = length(sub.df$obs[which(sub.df$comparison == "in diff")]), 
                      s.out = sub.df$obs[which(sub.df$comparison == "out diff")], sig.out = sub.df$sd[which(sub.df$comparison == "out diff")], 
                      out.N = length(sub.df$obs[which(sub.df$comparison == "out diff")]))
C4.output <- diff.extract.func(C4.fit, charname = "C4 photosynthesis")
# differences between N-fixing and non N-fixing species
sub.df <- meta.comp[which(meta.comp$character == "Nfixing"), ]
N.fit <- diff.jags.func(s.in = sub.df$obs[which(sub.df$comparison == "in diff")], sig.in = sub.df$sd[which(sub.df$comparison == "in diff")],
                        in.N = length(sub.df$obs[which(sub.df$comparison == "in diff")]), 
                        s.out = sub.df$obs[which(sub.df$comparison == "out diff")], sig.out = sub.df$sd[which(sub.df$comparison == "out diff")], 
                        out.N = length(sub.df$obs[which(sub.df$comparison == "out diff")]))
N.output <- diff.extract.func(N.fit, charname = c("N-fixing ability"))
# store output
char.test <- rbind(inv.output, C4.output, N.output)
write.csv(x = char.test, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_character_diff.csv")



