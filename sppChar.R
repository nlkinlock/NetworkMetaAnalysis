# COMPARE IN- AND OUT-STRENGTHS BASED ON INVASIVE STATUS, C4 PHOTOSYNTHESIS, AND N-FIXATION WITHIN NETWORKS
# COMBINE DIFFERENCES FROM MULTIPLE NETWORKS USING HIERARCHICAL T-TEST
#
<<<<<<< HEAD
# all networks with monoculture control (1) or networks with true control only (2)?
set.rii.control.type <- 1
#
||||||| 2b1eac9
=======

# all networks with monoculture control (1) or networks with true control only (2)?
set.rii.control.type <- 1

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INPUT NETWORKS --------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
<<<<<<< HEAD
# load data frames with plant performance and variance in performance for all pairwise combinations
case.names <- list.files(path = paste("Input/CaseData", sep = ""), recursive = FALSE)
case.names <- substr(case.names, 0, nchar(case.names) - 4)
case.names[grep(case.names, pattern = "_imp")] <- substr(case.names[grep(case.names, pattern = "_imp")], 0, nchar(case.names[grep(case.names, pattern = "_imp")]) - 4)
case.names[grep(case.names, pattern = "_Cntrl")] <- substr(case.names[grep(case.names, pattern = "_Cntrl")], 0, nchar(case.names[grep(case.names, pattern = "_Cntrl")]) - 6)
# remove networks with different abiotic conditions (treatments)
treatment.test <- substr(case.names, nchar(case.names), nchar(case.names))
treatment.indices <- c(grep(pattern = "[[:digit:]]", x = treatment.test), grep(pattern = "l", x = treatment.test))
case.names <- case.names[treatment.indices]
||||||| 2b1eac9
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
=======
# load data frames with plant performance and variance in performance for all pairwise combinations
case.names <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", recursive = FALSE)
case.names <- substr(case.names, 0, nchar(case.names) - 4)
case.names[grep(case.names, pattern = "_imp")] <- substr(case.names[grep(case.names, pattern = "_imp")], 0, nchar(case.names[grep(case.names, pattern = "_imp")]) - 4)
case.names[grep(case.names, pattern = "_Cntrl")] <- substr(case.names[grep(case.names, pattern = "_Cntrl")], 0, nchar(case.names[grep(case.names, pattern = "_Cntrl")]) - 6)
# remove networks with different abiotic conditions (treatments)
treatment.test <- substr(case.names, nchar(case.names), nchar(case.names))
treatment.indices <- c(grep(pattern = "[[:digit:]]", x = treatment.test), grep(pattern = "l", x = treatment.test))
case.names <- case.names[treatment.indices]
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae

if (set.rii.control.type == 2) {
  # indices for networks with true control (used when calculating RII in loop)
  true.control <- coding.df$Filename[which(coding.df$CtrlTreatment == "True ctrl")]
  coding.df.subset <- coding.df[which(coding.df$CtrlTreatment == "True ctrl"), ]
  ctrl.indices <- which(case.names %in% true.control)
  case.names <- case.names[ctrl.indices]
} else {
  # indices for networks with monoculture control
  mono.control <- coding.df$Filename[which(coding.df$PlantAge != "Both" & coding.df$Filename != "1205_Gao_2014")]
  coding.df.subset <- coding.df[which(coding.df$PlantAge != "Both" & coding.df$Filename != "1205_Gao_2014"), ]
  ctrl.indices <- which(case.names %in% mono.control)
  case.names <- case.names[ctrl.indices]
}

reorder.by.coding.df <- vector(mode = "integer", length = length(case.names))
for (x in 1:length(case.names)) {
  reorder.by.coding.df[x] <- grep(pattern = coding.df.subset$Filename[x], x = case.names)
}
case.names <- case.names[reorder.by.coding.df]

<<<<<<< HEAD
file.paths <- list.files(path = paste(path, "Input/CaseData", sep = ""), full.names = TRUE)
file.paths <- file.paths[treatment.indices]
file.paths <- file.paths[ctrl.indices]
file.paths <- file.paths[reorder.by.coding.df]
case.data <- lapply(file.paths, read.csv)
#
||||||| 2b1eac9

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

=======
file.paths <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", full.names = TRUE)
file.paths <- file.paths[treatment.indices]
file.paths <- file.paths[ctrl.indices]
file.paths <- file.paths[reorder.by.coding.df]
case.data <- lapply(file.paths, read.csv)


>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# set up list with Unique ID of case and all species
<<<<<<< HEAD
# split df into list where each element is string with species
species.list <- setNames(split(as.character(coding.df.subset$Species), seq(nrow(coding.df.subset))), coding.df.subset$UniqueID)
# split string into vector
species.list <- lapply(species.list, function(x) strsplit(x, ", "))
species.list <- lapply(species.list, unlist)
#
#
||||||| 2b1eac9
spp.list <- setNames(split(as.character(df$Species), seq(nrow(df))), df$UniqueID)  # split df into list where each element is string with species
spp.list <- lapply(spp.list, function(x) strsplit(x, ", "))  # split string into vector
spp.list <- lapply(spp.list, unlist)

=======
species.list <- setNames(split(as.character(coding.df.subset$Species), seq(nrow(coding.df.subset))), coding.df.subset$UniqueID)  # split df into list where each element is string with species
species.list <- lapply(species.list, function(x) strsplit(x, ", "))  # split string into vector
species.list <- lapply(species.list, unlist)

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOOP THROUGH CHARACTERS -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# what character should be subsetted?
# invasive status
# C4 photosynthesis status
# N-fixing status
char.names <- c("Invasive", "C3C4", "Nfixing")
<<<<<<< HEAD
comparison.names <- c("OutStrengthPositive", "OutStrengthNegative", "InStrengthPositive", "InStrengthNegative")
species.character.df <- data.frame(Network = character(), Character = character(), Comparison = character(), 
                                    ObservedMean = numeric(), BootstrapMean = numeric(), BootstrapSD = numeric(),
                                    BootstrapCIL = numeric(), BootstrapCIU = numeric(), stringsAsFactors = FALSE)
||||||| 2b1eac9
comparison.names <- c("out diff", "in diff")
meta.comp <- data.frame(network = character(), character = character(), comparison = character(), obs = numeric(), mean = numeric(), sd = numeric(), CI.l = numeric(), CI.u = numeric(), stringsAsFactors = FALSE)

=======
comparison.names <- c("OutStrengthPositive", "OutStrengthNegative", "InStrengthPositive", "InStrengthNegative")
species.character.df <- data.frame(Network = character(), Character = character(), Comparison = character(), ObservedMean = numeric(), BootstrapMean = numeric(), 
                                   BootstrapSD = numeric(), BootstrapCIL = numeric(), BootstrapCIU = numeric(), stringsAsFactors = FALSE)

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
for (h in 1:length(char.names)) {
<<<<<<< HEAD
  # split df into list where each element is string with invasives/C4/Nfixers
  character.list <- setNames(split(as.character(coding.df.subset[, paste(char.names[h])]), seq(nrow(coding.df.subset))), coding.df.subset$UniqueID)
  # split string into vector
  character.list <- lapply(character.list, function(x) strsplit(x, ", "))
  character.list <- lapply(character.list, function(x) as.numeric(unlist(x)))
  #
||||||| 2b1eac9
  stat.list <- setNames(split(as.character(df[, paste(char.names[h])]), seq(nrow(df))), df$UniqueID)  # split df into list where each element is string with invasives/C4/Nfixers
  stat.list <- lapply(stat.list, function(x) strsplit(x, ", "))  # split string into vector
  stat.list <- lapply(stat.list, function(x) as.numeric(unlist(x)))
  
=======
  character.list <- setNames(split(as.character(coding.df.subset[, paste(char.names[h])]), seq(nrow(coding.df.subset))), coding.df.subset$UniqueID)  # split df into list where each element is string with invasives/C4/Nfixers
  character.list <- lapply(character.list, function(x) strsplit(x, ", "))  # split string into vector
  character.list <- lapply(character.list, function(x) as.numeric(unlist(x)))
  
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  # loop to create a list with the unique case ID and the status of each species from each case in each element
  character.output <- list()
  for (i in 1:length(species.list)) {
    if (all(character.list[[i]] == 0)) {
      uniqueid <- rep(as.integer(names(species.list)[i]), length(species.list[[i]]))
      status <- rep(0, length(species.list[[i]]))
      character.output[[i]] <- data.frame(UniqueID = uniqueid, Status = status)
    } else {
      uniqueid <- rep(as.integer(names(species.list)[i]), length(species.list[[i]]))
      status <- integer(length = length(species.list[[i]]))
      status[character.list[[i]]] <- 1
      status[!character.list[[i]]] <- 0
      character.output[[i]] <- data.frame(UniqueID = uniqueid, Status = status)
    }
  }
  for (case in 1:length(case.names)) {
    char.vec <- character.output[[case]][, 2]
    # must be species both with and without the character
    if (all(char.vec == 0) | all(char.vec == 1) | case.names[case] == "7_Lof_2014") {
      next
    }
    dat <- case.data[[case]]
    num.species <- max(unique(dat$Target))
    treatment <- nrow(dat[dat$Metric > 0, ])
    sample.size <- max(dat$N)
    if (any(colnames(dat) == "SE")) {
      SD <- dat$SE * sqrt(dat$N)
      dat$SD <- SD
    }
    dat$var <- dat$SD^2
<<<<<<< HEAD
    if (set.rii.control.type == 2) {
      # true control only
      #
      if (case.names[case] == "1205_Gao_2014") {
        M.obs <- matrix(dat$Metric, nrow = num.species, ncol = num.species)
      } else if (case.names[case] == "1507_Saccone_2010") {
        M.obs <- SpecialCaseRII.Saccone(dat, num.species, type = "observed")
      } else {
        M.obs <- TrueControlRII(dat, num.species)
      }
    } else {
      # monoculture control
      #
      M.obs <- MonoControlRII(dat, num.species)
    }
    diag(M.obs) <- NA
    # calculate strength, subset by species with or without character, remove missing species (strength = 0)
    s.out <- colSums(M.obs, na.rm = TRUE)
    s.out.pos.init <- s.out[as.logical(char.vec)]
    s.out.pos.init <- s.out.pos.init[which(s.out.pos.init != 0)]
    s.out.neg.init <- s.out[!as.logical(char.vec)]
    s.out.neg.init <- s.out.neg.init[which(s.out.neg.init != 0)]
    s.in <- rowSums(M.obs, na.rm = TRUE)
    s.in.pos.init <- s.in[as.logical(char.vec)]
    s.in.pos.init <- s.in.pos.init[which(s.in.pos.init != 0)]
    s.in.neg.init <- s.in[!as.logical(char.vec)]
    s.in.neg.init <- s.in.neg.init[which(s.in.neg.init != 0)]
    if (length(s.out.pos.init) == 0 | length(s.out.neg.init) == 0 | length(s.in.pos.init) == 0 | length(s.in.neg.init) == 0) {
      next
    }
    s.out.pos <- mean(s.out.pos.init) / num.species
    s.out.neg <-  mean(s.out.neg.init) / num.species
    # calculate standardized mean strength for species with and without the character
    s.in.pos <- mean(s.in.pos.init) / num.species
    s.in.neg <-  mean(s.in.neg.init) / num.species
    obs.vec <- c(s.out.pos, s.out.neg, s.in.pos, s.in.neg)
    names(obs.vec) <- comparison.names
||||||| 2b1eac9
=======
    
    if (set.rii.control.type == 2) {
      # true control only
      #
      if (case.names[case] == "1205_Gao_2014") {
        M.obs <- matrix(dat$Metric, nrow = num.species, ncol = num.species)
      } else if (case.names[case] == "1507_Saccone_2010") {
        M.obs <- SpecialCaseRII.Saccone(dat, num.species, type = "observed")
      } else {
        M.obs <- TrueControlRII(dat, num.species)
      }
    } else {
      # monoculture control
      #
      M.obs <- MonoControlRII(dat, num.species)
    }
    diag(M.obs) <- NA
    # calculate strength, subset by species with or without character, remove missing species (strength = 0)
    s.out <- colSums(M.obs, na.rm = TRUE)
    s.out.pos.init <- s.out[as.logical(char.vec)]
    s.out.pos.init <- s.out.pos.init[which(s.out.pos.init != 0)]
    s.out.neg.init <- s.out[!as.logical(char.vec)]
    s.out.neg.init <- s.out.neg.init[which(s.out.neg.init != 0)]
    s.in <- rowSums(M.obs, na.rm = TRUE)
    s.in.pos.init <- s.in[as.logical(char.vec)]
    s.in.pos.init <- s.in.pos.init[which(s.in.pos.init != 0)]
    s.in.neg.init <- s.in[!as.logical(char.vec)]
    s.in.neg.init <- s.in.neg.init[which(s.in.neg.init != 0)]
    if (length(s.out.pos.init) == 0 | length(s.out.neg.init) == 0 | length(s.in.pos.init) == 0 | length(s.in.neg.init) == 0) {
      next
    }
    s.out.pos <- mean(s.out.pos.init) / num.species
    s.out.neg <-  mean(s.out.neg.init) / num.species
    # calculate standardized mean strength for species with and without the character
    s.in.pos <- mean(s.in.pos.init) / num.species
    s.in.neg <-  mean(s.in.neg.init) / num.species
    obs.vec <- c(s.out.pos, s.out.neg, s.in.pos, s.in.neg)
    names(obs.vec) <- comparison.names
    
    
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
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
    #
    # create empty data frames for comparisons
<<<<<<< HEAD
    s.out.pos.store <- numeric(length = R)
    s.in.pos.store <- numeric(length = R)
    s.out.neg.store <- numeric(length = R)
    s.in.neg.store <- numeric(length = R)
    #
||||||| 2b1eac9
    s.out.store <- numeric(length = R)
    s.in.store <- numeric(length = R)
    
=======
    s.out.pos.store <- numeric(length = R)
    s.in.pos.store <- numeric(length = R)
    s.out.neg.store <- numeric(length = R)
    s.in.neg.store <- numeric(length = R)
    
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
    weights <- matrix(rexp(N * R, 1) , ncol = N, byrow = TRUE)
    weights <- weights / rowSums(weights)
    for (iteration in 1:R) {
      if (case.names[case] == "1205_Gao_2014") {
        b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = 1, replace = TRUE, prob = weights[iteration, ]))
        M.boot <- matrix(b.boot, nrow = num.species, ncol = num.species)
      } else {
<<<<<<< HEAD
        b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample.size, replace = TRUE, prob = weights[iteration, ]))
        b.boot.long <- reshape2::melt(b.boot)
        b.boot.long <- data.frame(Target = rep(targets, each = sample.size), Neighbor = rep(neighbors, each = sample.size), Metric = b.boot.long[, 3])
        if (set.rii.control.type == 2) {
          # true control only
          #
          if (case.names[case] == "1507_Saccone_2010") {
            M.boot <- SpecialCaseRII.Saccone(b.boot, num.species, type = "bootstrap")
          } else {
            M.boot <- TrueControlRII(b.boot.long, num.species) 
||||||| 2b1eac9
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
=======
        b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample.size, replace = TRUE, prob = weights[iteration, ]))
        b.boot.long <- melt(b.boot)
        b.boot.long <- data.frame(Target = rep(targets, each = sample.size), Neighbor = rep(neighbors, each = sample.size), Metric = b.boot.long[, 3])
        if (set.rii.control.type == 2) {
          # true control only
          #
          if (case.names[case] == "1507_Saccone_2010") {
            M.boot <- SpecialCaseRII.Saccone(b.boot, num.species, type = "bootstrap")
          } else {
            M.boot <- TrueControlRII(b.boot.long, num.species) 
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
          }
        } else {
          # monoculture control
          #
          M.boot <- MonoControlRII(b.boot.long, num.species)
        }
      }
      diag(M.boot) <- NA
      #
      # calculate comparisons between mean strength with the character and without
      # on bootstrapped networks
      #
      s.out <- colSums(M.boot, na.rm = TRUE)
      s.out.pos.init <- s.out[as.logical(char.vec)]
      s.out.pos.init <- s.out.pos.init[which(s.out.pos.init != 0)]
      s.out.neg.init <- s.out[!as.logical(char.vec)]
      s.out.neg.init <- s.out.neg.init[which(s.out.neg.init != 0)]
      s.in <- rowSums(M.boot, na.rm = TRUE)
      s.in.pos.init <- s.in[as.logical(char.vec)]
      s.in.pos.init <- s.in.pos.init[which(s.in.pos.init != 0)]
      s.in.neg.init <- s.in[!as.logical(char.vec)]
      s.in.neg.init <- s.in.neg.init[which(s.in.neg.init != 0)]
      s.out.pos.store[iteration] <- mean(s.out.pos.init) / num.species
      s.out.neg.store[iteration] <- mean(s.out.neg.init) / num.species
      s.in.pos.store[iteration] <- mean(s.in.pos.init) / num.species
      s.in.neg.store[iteration] <- mean(s.in.neg.init) / num.species
    }
    boot.table <- data.frame(s.out.pos.store, s.out.neg.store, s.in.pos.store, s.in.neg.store)
    colnames(boot.table) <- comparison.names
<<<<<<< HEAD
    #
||||||| 2b1eac9
    
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
    
=======
    
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
    mean.vec <- apply(boot.table, 2, function(x) mean(x, na.rm = TRUE))
    sd.vec <- apply(boot.table, 2, function(x) sd(x, na.rm = TRUE))
    cilb.vec <- apply(boot.table, 2, function(x) quantile(x, probs = c(0.025), na.rm = TRUE))
    ciub.vec <- apply(boot.table, 2, function(x) quantile(x, probs = c(0.975), na.rm = TRUE))
<<<<<<< HEAD
    #
    obs.l <- reshape2::melt(obs.vec)
    mean.l <- reshape2::melt(mean.vec)
    sd.l <- reshape2::melt(sd.vec)
    cilb.l <- reshape2::melt(cilb.vec)
    ciub.l <- reshape2::melt(ciub.vec)
    # combine output
    species.character.row <- data.frame(Network = case.names[case], Character = char.names[h], Comparison = row.names(obs.l),
                                        ObservedMean = obs.l$value, BootstrapMean = mean.l$value, BootstrapSD = sd.l$value,
                                        BootstrapCIL = cilb.l$value, BootstrapCIU = ciub.l$value)
    species.character.df <- rbind(species.character.df, species.character.row)
    print(paste("Species characteristics for network ", case, case.names[case], " is complete"))
||||||| 2b1eac9
    
    obs.l <- melt(obs.vec)
    mean.l <- melt(mean.vec)
    sd.l <- melt(sd.vec)
    cilb.l <- melt(cilb.vec)
    ciub.l <- melt(ciub.vec)
    
    meta.comp.row <- data.frame(network = case.names[case], character = char.names[h], comparison = row.names(obs.l), obs = obs.l$value, mean = mean.l$value, sd = sd.l$value, CI.l = cilb.l$value, CI.u = ciub.l$value)
    meta.comp <- rbind(meta.comp, meta.comp.row)
    print(paste("Network ", case, case.names[case], " is complete"))
=======
    
    obs.l <- melt(obs.vec)
    mean.l <- melt(mean.vec)
    sd.l <- melt(sd.vec)
    cilb.l <- melt(cilb.vec)
    ciub.l <- melt(ciub.vec)
    
    species.character.row <- data.frame(Network = case.names[case], Character = char.names[h], Comparison = row.names(obs.l), ObservedMean = obs.l$value, BootstrapMean = mean.l$value, BootstrapSD = sd.l$value, BootstrapCIL = cilb.l$value, BootstrapCIU = ciub.l$value)
    species.character.df <- rbind(species.character.df, species.character.row)
    print(paste("Species characteristics for network ", case, case.names[case], " is complete"))
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  }
}
# store output by network
<<<<<<< HEAD
if (set.rii.control.type == 2) {
  write.csv(x = species.character.df, file = paste(path, "Output/SpeciesCharacterDifferences_ByNetwork_TrueCtrlOnly.csv", sep = ""))
} else {
  write.csv(x = species.character.df, file = paste(path, "Output/SpeciesCharacterDifferences_ByNetwork_MonoCtrl.csv", sep = ""))
}
num.per.group <- ddply(species.character.df, .(Character), summarise, length(unique(Network)))
#
#
||||||| 2b1eac9
# write.csv(x = meta.comp, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_character_bynetwork.csv")

=======
if (set.rii.control.type == 2) {
  write.csv(x = species.character.df, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_ByNetwork_TrueCtrlOnly.csv")
} else {
  write.csv(x = species.character.df, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_ByNetwork_MonoCtrl.csv")
}
num.per.group <- ddply(species.character.df, .(Character), summarise, length(unique(Network)))


>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# HIERARCHICAL T-TEST OF CHAR. DIFFERENCES ------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# differences between invasive and native species
<<<<<<< HEAD
sub.df <- species.character.df[which(species.character.df$Character == "Invasive"), ]
inv.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
       sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
       s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
       sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
       groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
       groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
inv.output <- ExtractJAGSMeanDifferences(inv.fit, char.name = "Invasive status", char.options = c("Invasive", "Native"))
||||||| 2b1eac9
sub.df <- meta.comp[which(meta.comp$character == "Invasive"), ]
inv.fit <- diff.jags.func(s.in = sub.df$obs[which(sub.df$comparison == "in diff")], sig.in = sub.df$sd[which(sub.df$comparison == "in diff")],
                          in.N = length(sub.df$obs[which(sub.df$comparison == "in diff")]), 
                          s.out = sub.df$obs[which(sub.df$comparison == "out diff")], sig.out = sub.df$sd[which(sub.df$comparison == "out diff")], 
                          out.N = length(sub.df$obs[which(sub.df$comparison == "out diff")]))
inv.output <- diff.extract.func(inv.fit, charname = "Invasive status")
=======
sub.df <- species.character.df[which(species.character.df$Character == "Invasive"), ]
inv.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
                                   sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
                                   s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
                                   sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
                                   groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
                                   groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
inv.output <- ExtractJAGSMeanDifferences(inv.fit, char.name = "Invasive status", char.options = c("Invasive", "Native"))
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# differences between C4 and C3 species
<<<<<<< HEAD
sub.df <- species.character.df[which(species.character.df$Character == "C3C4"), ]
C4.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
        sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
        s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
        sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
        groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
        groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
C4.output <- ExtractJAGSMeanDifferences(C4.fit, char.name = "C4 photosynthesis", char.options = c("C4", "C3"))
||||||| 2b1eac9
sub.df <- meta.comp[which(meta.comp$character == "C3C4"), ]
C4.fit <- diff.jags.func(s.in = sub.df$obs[which(sub.df$comparison == "in diff")], sig.in = sub.df$sd[which(sub.df$comparison == "in diff")],
                      in.N = length(sub.df$obs[which(sub.df$comparison == "in diff")]), 
                      s.out = sub.df$obs[which(sub.df$comparison == "out diff")], sig.out = sub.df$sd[which(sub.df$comparison == "out diff")], 
                      out.N = length(sub.df$obs[which(sub.df$comparison == "out diff")]))
C4.output <- diff.extract.func(C4.fit, charname = "C4 photosynthesis")
=======
sub.df <- species.character.df[which(species.character.df$Character == "C3C4"), ]
C4.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
                                  sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
                                  s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
                                  sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
                                  groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
                                  groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
C4.output <- ExtractJAGSMeanDifferences(C4.fit, char.name = "C4 photosynthesis", char.options = c("C4", "C3"))
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# differences between N-fixing and non N-fixing species
<<<<<<< HEAD
sub.df <- species.character.df[which(species.character.df$Character == "Nfixing"), ]
N.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
       sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
       s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
       sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
       groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
       groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
N.output <- ExtractJAGSMeanDifferences(N.fit, char.name = c("N-fixing ability"), char.options = c("N-fix.", "non-N-fix."))
# combine output
||||||| 2b1eac9
sub.df <- meta.comp[which(meta.comp$character == "Nfixing"), ]
N.fit <- diff.jags.func(s.in = sub.df$obs[which(sub.df$comparison == "in diff")], sig.in = sub.df$sd[which(sub.df$comparison == "in diff")],
                        in.N = length(sub.df$obs[which(sub.df$comparison == "in diff")]), 
                        s.out = sub.df$obs[which(sub.df$comparison == "out diff")], sig.out = sub.df$sd[which(sub.df$comparison == "out diff")], 
                        out.N = length(sub.df$obs[which(sub.df$comparison == "out diff")]))
N.output <- diff.extract.func(N.fit, charname = c("N-fixing ability"))
# store output
=======
sub.df <- species.character.df[which(species.character.df$Character == "Nfixing"), ]
N.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
                                 sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
                                 s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
                                 sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
                                 groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
                                 groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
N.output <- ExtractJAGSMeanDifferences(N.fit, char.name = c("N-fixing ability"), char.options = c("N-fix.", "non-N-fix."))
# store output
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
char.test <- rbind(inv.output, C4.output, N.output)
<<<<<<< HEAD
char.test$SampleSize <- rep(num.per.group[, 2], each = nrow(N.output))
# save output
if (set.rii.control.type == 2) {
  write.csv(x = char.test, file = paste(path, "Output/SpeciesCharacterDifferences_TrueCtrlOnly.csv", sep = ""))
} else {
  write.csv(x = char.test, file = paste(path, "Output/SpeciesCharacterDifferences_MonoCtrl.csv", sep = ""))
}

||||||| 2b1eac9
write.csv(x = char.test, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_character_diff.csv")
=======
char.test$SampleSize <- rep(num.per.group[, 2], each = nrow(N.output))

if (set.rii.control.type == 2) {
  write.csv(x = char.test, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_TrueCtrlOnly.csv")
} else {
  write.csv(x = char.test, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_MonoCtrl.csv")
}

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae



