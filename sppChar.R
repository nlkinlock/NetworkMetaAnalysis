# COMPARE IN- AND OUT-STRENGTHS BASED ON INVASIVE STATUS, C4 PHOTOSYNTHESIS, AND N-FIXATION WITHIN NETWORKS
# COMBINE DIFFERENCES FROM MULTIPLE NETWORKS USING HIERARCHICAL T-TEST
#

# all networks with monoculture control (1) or networks with true control only (2)?
set.rii.control.type <- 1

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INPUT NETWORKS --------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load data frames with plant performance and variance in performance for all pairwise combinations
case.names <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", recursive = FALSE)
case.names <- substr(case.names, 0, nchar(case.names) - 4)
case.names[grep(case.names, pattern = "_imp")] <- substr(case.names[grep(case.names, pattern = "_imp")], 0, nchar(case.names[grep(case.names, pattern = "_imp")]) - 4)
case.names[grep(case.names, pattern = "_Cntrl")] <- substr(case.names[grep(case.names, pattern = "_Cntrl")], 0, nchar(case.names[grep(case.names, pattern = "_Cntrl")]) - 6)
# remove networks with different abiotic conditions (treatments)
treatment.test <- substr(case.names, nchar(case.names), nchar(case.names))
treatment.indices <- c(grep(pattern = "[[:digit:]]", x = treatment.test), grep(pattern = "l", x = treatment.test))
case.names <- case.names[treatment.indices]

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

file.paths <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", full.names = TRUE)
file.paths <- file.paths[treatment.indices]
file.paths <- file.paths[ctrl.indices]
file.paths <- file.paths[reorder.by.coding.df]
case.data <- lapply(file.paths, read.csv)


# set up list with Unique ID of case and all species
species.list <- setNames(split(as.character(coding.df.subset$Species), seq(nrow(coding.df.subset))), coding.df.subset$UniqueID)  # split df into list where each element is string with species
species.list <- lapply(species.list, function(x) strsplit(x, ", "))  # split string into vector
species.list <- lapply(species.list, unlist)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOOP THROUGH CHARACTERS -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# what character should be subsetted?
# invasive status
# C4 photosynthesis status
# N-fixing status
char.names <- c("Invasive", "C3C4", "Nfixing")
comparison.names <- c("OutStrengthPositive", "OutStrengthNegative", "InStrengthPositive", "InStrengthNegative")
species.character.df <- data.frame(Network = character(), Character = character(), Comparison = character(), ObservedMean = numeric(), BootstrapMean = numeric(), 
                                   BootstrapSD = numeric(), BootstrapCIL = numeric(), BootstrapCIU = numeric(), stringsAsFactors = FALSE)

for (h in 1:length(char.names)) {
  character.list <- setNames(split(as.character(coding.df.subset[, paste(char.names[h])]), seq(nrow(coding.df.subset))), coding.df.subset$UniqueID)  # split df into list where each element is string with invasives/C4/Nfixers
  character.list <- lapply(character.list, function(x) strsplit(x, ", "))  # split string into vector
  character.list <- lapply(character.list, function(x) as.numeric(unlist(x)))
  
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
    s.out.pos.store <- numeric(length = R)
    s.in.pos.store <- numeric(length = R)
    s.out.neg.store <- numeric(length = R)
    s.in.neg.store <- numeric(length = R)
    
    weights <- matrix(rexp(N * R, 1) , ncol = N, byrow = TRUE)
    weights <- weights / rowSums(weights)
    for (iteration in 1:R) {
      if (case.names[case] == "1205_Gao_2014") {
        b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = 1, replace = TRUE, prob = weights[iteration, ]))
        M.boot <- matrix(b.boot, nrow = num.species, ncol = num.species)
      } else {
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
    
    mean.vec <- apply(boot.table, 2, function(x) mean(x, na.rm = TRUE))
    sd.vec <- apply(boot.table, 2, function(x) sd(x, na.rm = TRUE))
    cilb.vec <- apply(boot.table, 2, function(x) quantile(x, probs = c(0.025), na.rm = TRUE))
    ciub.vec <- apply(boot.table, 2, function(x) quantile(x, probs = c(0.975), na.rm = TRUE))
    
    obs.l <- melt(obs.vec)
    mean.l <- melt(mean.vec)
    sd.l <- melt(sd.vec)
    cilb.l <- melt(cilb.vec)
    ciub.l <- melt(ciub.vec)
    
    species.character.row <- data.frame(Network = case.names[case], Character = char.names[h], Comparison = row.names(obs.l), ObservedMean = obs.l$value, BootstrapMean = mean.l$value, BootstrapSD = sd.l$value, BootstrapCIL = cilb.l$value, BootstrapCIU = ciub.l$value)
    species.character.df <- rbind(species.character.df, species.character.row)
    print(paste("Species characteristics for network ", case, case.names[case], " is complete"))
  }
}
# store output by network
if (set.rii.control.type == 2) {
  write.csv(x = species.character.df, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_ByNetwork_TrueCtrlOnly.csv")
} else {
  write.csv(x = species.character.df, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_ByNetwork_MonoCtrl.csv")
}
num.per.group <- ddply(species.character.df, .(Character), summarise, length(unique(Network)))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# HIERARCHICAL T-TEST OF CHAR. DIFFERENCES ------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# differences between invasive and native species
sub.df <- species.character.df[which(species.character.df$Character == "Invasive"), ]
inv.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
                                   sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
                                   s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
                                   sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
                                   groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
                                   groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
inv.output <- ExtractJAGSMeanDifferences(inv.fit, char.name = "Invasive status", char.options = c("Invasive", "Native"))
# differences between C4 and C3 species
sub.df <- species.character.df[which(species.character.df$Character == "C3C4"), ]
C4.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
                                  sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
                                  s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
                                  sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
                                  groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
                                  groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
C4.output <- ExtractJAGSMeanDifferences(C4.fit, char.name = "C4 photosynthesis", char.options = c("C4", "C3"))
# differences between N-fixing and non N-fixing species
sub.df <- species.character.df[which(species.character.df$Character == "Nfixing"), ]
N.fit <- EstimateMeanDifferences(s.in.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "InStrengthNegative")]),
                                 sigma.in.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "InStrengthNegative")]),
                                 s.out.obs = c(sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$ObservedMean[which(sub.df$Comparison == "OutStrengthNegative")]),
                                 sigma.out.obs = c(sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthPositive")], sub.df$BootstrapSD[which(sub.df$Comparison == "OutStrengthNegative")]),
                                 groupID.in = ifelse(sub.df$Comparison[grep(pattern = "InStrength", x = sub.df$Comparison)] == "InStrengthPositive", 0, 1),
                                 groupID.out = ifelse(sub.df$Comparison[grep(pattern = "OutStrength", x = sub.df$Comparison)] == "OutStrengthPositive", 0, 1))
N.output <- ExtractJAGSMeanDifferences(N.fit, char.name = c("N-fixing ability"), char.options = c("N-fix.", "non-N-fix."))
# store output
char.test <- rbind(inv.output, C4.output, N.output)
char.test$SampleSize <- rep(num.per.group[, 2], each = nrow(N.output))

if (set.rii.control.type == 2) {
  write.csv(x = char.test, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_TrueCtrlOnly.csv")
} else {
  write.csv(x = char.test, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_MonoCtrl.csv")
}




