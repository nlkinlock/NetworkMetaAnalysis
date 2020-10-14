# FUNCTIONS FOR NETWORK META-ANALYSIS
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# NETWORK METRICS -------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# extract unique IDs from network names
#
ExtractUniqueID <- function(network.name.vec) {
  UniqueID <- c()
  for (z in 1:length(network.name.vec)) {
    if (substring(network.name.vec[z], 5, 5) == "_") {
      UniqueID[z] <- as.integer(substring(network.name.vec[z], 1, 4))
    } else if (substring(network.name.vec[z], 2, 2) == "_") {
      UniqueID[z] <- as.integer(substring(network.name.vec[z], 1, 1))
    } else {
      UniqueID[z] <- as.integer(substring(network.name.vec[z], 1, 3))
    }
  }
  return(UniqueID)
}
#
# measure mean strength (same for in- or out-strength)
#
MeanStrength <- function(M) {
  strength <- colSums(M, na.rm = TRUE)
  strength <- strength[which(strength != 0)]
  mean.strength <- mean(strength) / length(strength)
  return(mean.strength)
}
#
# measure mean indirect effect
#
# weighted average of the direct effects on the target species, weighting by effects of other species on species i (one-step removed effect)
# return mean indirect effect for the community
IndirectEffect <- function(M) {
  num.species <- nrow(M)
  mean.effect <- apply(M, MARGIN = 1, function(x) mean(x, na.rm = TRUE))
  indirect.effect <- apply(M, MARGIN = 1, function(x) sum(x * mean.effect, na.rm = TRUE) / sum(abs(mean.effect[which(!is.na(x))]), na.rm = TRUE))
  mean.indirect.effect <- mean(indirect.effect, na.rm = TRUE) 
  return(mean.indirect.effect)
}
#
# measure imbalance of interactions
#
# is the effect of species i on species j equal to the effect of species j on species i (balanced)
# or is the effect of species i on species j much greater than the effect of species j on species i (imbalanced) 
Imbalance <- function(M) {
  index <- which(upper.tri(M))
  flat <- c(M)  # flatten matrix and its transpose, this gives w(ij) and w(ji) in order (ignoring diagonal)
  flat.t <- c(t(M))
  w.ij <- flat[index]
  w.ji <- flat.t[index]
  imbalance <- mean(abs(w.ij - w.ji), na.rm = TRUE)
  return(imbalance)
}
#
# measure interaction asymmetry
#
# is the effect of species i on species j positive, while the effect of species j on species i is negative? if so, the interaction is asymmetric
# this means that species i does better in mixture with j than in monoculture, while species j does better in monoculture than with species i
# species i will exclude species j given coexistence theory
# return the proportion of asymmetric interactions in the network
# only makes sense for monoculture control
Asymmetry <- function(M) {
  if (case %in% ctrl.indices & !(case %in% mono.test.indices)) {
    proportion.asymmetry <- NA
  } else {
    index <- which(upper.tri(M))
    flat <- c(M)  # flatten matrix and its transpose, this gives w(ij) and w(ji) in order (ignoring diagonal)
    flat.t <- c(t(M))
    w.ij <- flat[index]
    w.ji <- flat.t[index]
    test.asymmetry <- ifelse(test = (w.ij > 0 & w.ji < 0) | (w.ij < 0 & w.ji > 0), 1, 0)
    proportion.asymmetry <- sum(test.asymmetry, na.rm = TRUE) / sum(!is.na(test.asymmetry))
  }
  return(proportion.asymmetry)
}
#
# measure weighted connectance
#
# function to calculate interaction diversity (H, in and out) and weighted connectance of networks, based on Bersier et al. 2002
Connectance <- function(M) {
  num.species <- nrow(M)
  M.abs <- abs(M)
  s.out <- colSums(M.abs, na.rm = TRUE)
  s.in <- rowSums(M.abs, na.rm = TRUE)
  H.in <- c()
  H.out <- c()
  for (i in 1:num.species) {
    H.in[i] <- -sum((M.abs[i, ] / s.in[i]) * log2(M.abs[i, ] / s.in[i]), na.rm = TRUE)
    H.out[i] <- -sum((M.abs[, i] / s.out[i]) * log2(M.abs[, i] / s.out[i]), na.rm = TRUE)
  }
  sum.M <- sum(M.abs, na.rm = TRUE)
  a <- c()
  b <- c()
  for (i in 1:num.species) {
    if (s.in[i] == 0) {
      recip.H.in <- 0
    } else {
      recip.H.in <- 2^(H.in[i])
    }
    if (s.out[i] == 0) {
      recip.H.out <- 0
    } else {
      recip.H.out <- 2^(H.out[i])
    }
    a[i] <- (s.in[i] / sum.M) * recip.H.in
    b[i] <- (s.out[i] / sum.M) * recip.H.out
  }
  LD.qw <- 0.5 * (sum(a, na.rm = TRUE) + sum(b, na.rm = TRUE))
  C.qw <- LD.qw / num.species
  return(C.qw)
}
#
# calculate relative intransitivity
Intransitivity <- function(M) {
  # turn observed network into a competitive outcomes matrix
  num.species <- nrow(M)
  M <- t(M)  # uses traditional network indexing (row acts on column species)
  M.u <- matrix(data = NA, nrow = num.species, ncol = num.species)
  M.l <- matrix(data = NA, nrow = num.species, ncol = num.species)
  for (i in 1:(nrow(M.l) - 1)) {
    for (j in (i + 1):nrow(M.l)) {
      if (is.na(M[i, j]) | is.na(M[j, i])) {
        M.l[i, j] <- NA
        M.u[j, i] <- NA
      } else if (M[i, j] < M[j, i]) {  # if row species outcompetes column species (more negative = better)
        M.l[j, i] <- 0  # column species wins
        M.u[i, j] <- 1  # row species wins
      } else {
        M.l[j, i] <- 1  # column species loses
        M.u[i, j] <- 0  # row species loses
      }
    }
  }
  # combine upper and lower triangles
  M.t <- matrix(NA, num.species, num.species)
  M.t[which(!is.na(M.l), arr.ind = TRUE)] <- M.l[which(!is.na(M.l), arr.ind = TRUE)]
  M.t[which(!is.na(M.u), arr.ind = TRUE)] <- M.u[which(!is.na(M.u), arr.ind = TRUE)]
  # M.t is the competitive outcomes matrix
  #
  # relative intransitivity index (Laird and Schamp 2008)
  M.t.eff <- M.t[rowSums(is.na(M.t)) != ncol(M.t),]  # remove rows with all NAs
  var.obs <- var(rowSums(M.t.eff, na.rm = TRUE))  # observed variance
  #
  # min and max variance for matrices of size num.species by num.species
  # use effective number of species (number of pairs)
  obs.mixed.pairs <- sum(!is.na(M.t[upper.tri(M.t)]))
  # length of upper triangular for a matrix of a given size, find matrix that best matches data (with missing elements removed)
  mixed.pairs <- c()
  for (num.species in 1:10) {
    mixed.pairs[num.species] <- (num.species^2 - num.species) / 2
  }
  num.species.obs <- which.min(abs(mixed.pairs - obs.mixed.pairs))
  #
  # minimum and maximum possible variances for a matrix of this size (from transitivityinputs.R)
  var.max.s <- c(NA, NA, 1.000000, 1.666667, 2.500000, 3.500000, 4.666667, 6.000000, 7.500000, 9.166667, 11.000000, 13.000000, 15.166667, 
                 17.50000, 20.000000, 22.666667, 25.500000, 28.500000, 31.666667, 35.000000)
  var.min.s <- c(NA, NA, 0.0000000, 0.3333333, 0.0000000, 0.3000000, 0.0000000, 0.2857143, 0.2500000, 0.2777778, 0.4000000, 0.2727273,
                 0.3333333, 0.5769231, 0.7142857, 0.6666667, 0.6250000, 0.7352941, 0.7777778, 0.8947368)
  var.min <- var.min.s[num.species.obs]
  var.max <- var.max.s[num.species.obs]
  #
  # formula for relative intransitivity index from Laird and Schamp (2006)
  r.intrans <- pmax(1 - ((var.obs - var.min) / (var.max - var.min)), 0)
  return(r.intrans)
}
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INTERACTION INDICES----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
#
# calculate RII, comparing plant performance in the presence of a different species and alone
# for example, a negative RII implies the species does better alone than with the other species (experiences competition)
TrueControlRII <- function(df, num.species) {
  rii.matrix <- c()
  count <- 1
  for (i in 1:num.species) {
    ctrl <- which(df$Target == i & df$Neighbor == 0)
    for (j in 1:num.species) {
      mix <- which(df$Target == i & df$Neighbor == j)
      if (length(mix) == 0 | length(ctrl) == 0 | all(df$Metric[mix] == 0)) {
        rii.matrix[count] <- NA
      } else {
        rii.matrix[count] <- (mean(df$Metric[mix]) - mean(df$Metric[ctrl])) / (mean(df$Metric[mix]) + mean(df$Metric[ctrl]))
      }
      count <- count + 1
    }
  }
  M <- matrix(rii.matrix, nrow = num.species, ncol = num.species, byrow = TRUE)
  return(M)
}
#
# calculate RII, comparing plant performance in the presence of a different species and in the presence of a conspecific
# for example, a negative RII implies the species does better with members of its own species than with the other species (intraspecific < interspecific)
MonoControlRII <- function(df, num.species) {
  rii.matrix <- c()
  count <- 1
  for (i in 1:num.species) {
    mono <- which(df$Target == i & df$Neighbor == i)
    for (j in 1:num.species) {
      mix <- which(df$Target == i & df$Neighbor == j)
      # make sure that the pairwise combination was actually measured
      # make sure monoculture was measured (otherwise no comparison)
      if (length(mix) == 0 | all(df$Metric[mix] == 0) | length(mono) == 0 | all(df$Metric[mono] == 0) ) {
        rii.matrix[count] <- NA
      } else {
        rii.matrix[count] <- (mean(df$Metric[mix]) - mean(df$Metric[mono])) / (mean(df$Metric[mix]) + mean(df$Metric[mono]))
      }
      count <- count + 1
    }
  }
  rii.matrix[is.nan(rii.matrix)] <- NA
  M <- matrix(rii.matrix, nrow = num.species, ncol = num.species, byrow = TRUE)
  return(M)
}
#
# calculate RII for special cases where there are separate control treatments (plants grown alone) for each mixed species treatment
# 1507_Saccone_2010 and 106_Costa_2003
SpecialCaseRII.Saccone <- function(df, num.species, type) {
  if (type == "bootstrap") {
    rii.matrix <- c()
    rii.matrix[1] <- (mean(df[, 1]) - mean(df[, 2])) / (mean(df[, 1]) + mean(df[, 2]))
    rii.matrix[2] <- (mean(df[, 3]) - mean(df[, 4])) / (mean(df[, 3]) + mean(df[, 4]))
    rii.matrix[3] <- NA
    rii.matrix[4] <- (mean(df[, 5]) - mean(df[, 6])) / (mean(df[, 5]) + mean(df[, 6]))
    rii.matrix[5] <- (mean(df[, 7]) - mean(df[, 8])) / (mean(df[, 7]) + mean(df[, 8]))
    rii.matrix[6] <- (mean(df[, 9]) - mean(df[, 10])) / (mean(df[, 9]) + mean(df[, 10]))
    rii.matrix[7] <- NA
    rii.matrix[8] <- (mean(df[, 11]) - mean(df[, 12])) / (mean(df[, 11]) + mean(df[, 12]))
    rii.matrix[9] <- (mean(df[, 13]) - mean(df[, 14])) / (mean(df[, 13]) + mean(df[, 14]))
    rii.matrix[10] <- (mean(df[, 15]) - mean(df[, 16])) / (mean(df[, 15]) + mean(df[, 16]))
    rii.matrix[11] <- NA
    rii.matrix[12] <- NA
    rii.matrix[13] <- (mean(df[, 18]) - mean(df[, 19])) / (mean(df[, 18]) + mean(df[, 19]))
    rii.matrix[14] <- (mean(df[, 20]) - mean(df[, 21])) / (mean(df[, 20]) + mean(df[, 21]))
    rii.matrix[15] <- NA
    rii.matrix[16] <- (mean(df[, 22]) - mean(df[, 23])) / (mean(df[, 22]) + mean(df[, 23]))
  } else if (type == "observed") {
    one.one <- (df$Metric[1] - df$Metric[2]) / (df$Metric[1] + df$Metric[2])
    one.two <- (df$Metric[3] - df$Metric[4]) / (df$Metric[3] + df$Metric[4])
    one.four <- (df$Metric[5] - df$Metric[6]) / (df$Metric[5] + df$Metric[6])
    two.one <- (df$Metric[7] - df$Metric[8]) / (df$Metric[7] + df$Metric[8])
    two.two <- (df$Metric[9] - df$Metric[10]) / (df$Metric[9] + df$Metric[10])
    two.four <- (df$Metric[11] - df$Metric[12]) / (df$Metric[11] + df$Metric[12])
    three.one <- (df$Metric[13] - df$Metric[14]) / (df$Metric[13] + df$Metric[14])
    three.two <- (df$Metric[15] - df$Metric[16]) / (df$Metric[15] + df$Metric[16])
    four.one <- (df$Metric[19] - df$Metric[20]) / (df$Metric[19] + df$Metric[20])
    four.two <- (df$Metric[21] - df$Metric[22]) / (df$Metric[21] + df$Metric[22])
    four.four <- (df$Metric[23] - df$Metric[24]) / (df$Metric[23] + df$Metric[24])
    rii.matrix <- c(one.one, one.two, NA, one.four, two.one, two.two, NA, two.four, three.one, three.two, NA, NA, four.one, four.two, NA, four.four)
  }
  M <- matrix(rii.matrix, nrow = num.species, ncol = num.species, byrow = TRUE)
  return(M)
}
SpecialCaseRII.Costa <- function(df, num.species, type) {
  if (type == "bootstrap") {
    rii.matrix <- c()
    rii.matrix[1] <- (mean(df[, 1]) - mean(df[, 4])) / (mean(df[, 1]) + mean(df[, 4]))
    rii.matrix[2] <- (mean(df[, 2]) - mean(df[, 5])) / (mean(df[, 2]) + mean(df[, 5]))
    rii.matrix[3] <- (mean(df[, 3]) - mean(df[, 6])) / (mean(df[, 3]) + mean(df[, 6]))
    rii.matrix[4] <- (mean(df[, 7]) - mean(df[, 10])) / (mean(df[, 7]) + mean(df[, 10]))
    rii.matrix[5] <- (mean(df[, 8]) - mean(df[, 11])) / (mean(df[, 8]) + mean(df[, 11]))
    rii.matrix[6] <- (mean(df[, 9]) - mean(df[, 12])) / (mean(df[, 9]) + mean(df[, 12]))
    rii.matrix[7] <- (mean(df[, 13]) - mean(df[, 16])) / (mean(df[, 13]) + mean(df[, 16]))
    rii.matrix[8] <- (mean(df[, 14]) - mean(df[, 17])) / (mean(df[, 14]) + mean(df[, 17]))
    rii.matrix[9] <- (mean(df[, 15]) - mean(df[, 18])) / (mean(df[, 15]) + mean(df[, 18]))
  } else if (type == "observed") {
    one.one <- (df$Metric[1] - df$Metric[4]) / (df$Metric[1] + df$Metric[4])
    one.two <- (df$Metric[2] - df$Metric[5]) / (df$Metric[2] + df$Metric[5])
    one.three <- (df$Metric[3] - df$Metric[6]) / (df$Metric[3] + df$Metric[6])
    two.one <- (df$Metric[7] - df$Metric[10]) / (df$Metric[7] + df$Metric[10])
    two.two <- (df$Metric[8] - df$Metric[11]) / (df$Metric[8] + df$Metric[11])
    two.three <- (df$Metric[9] - df$Metric[12]) / (df$Metric[9] + df$Metric[12])
    three.one <- (df$Metric[13] - df$Metric[16]) / (df$Metric[13] + df$Metric[16])
    three.two <- (df$Metric[14] - df$Metric[17]) / (df$Metric[14] + df$Metric[17])
    three.three <- (df$Metric[15] - df$Metric[18]) / (df$Metric[15] + df$Metric[18])
    rii.matrix <- c(one.one, one.two, one.three, two.one, two.two, two.three, three.one, three.two, three.three)
  }
  M <- matrix(rii.matrix, nrow = num.species, ncol = num.species, byrow = TRUE)
  return(M)
}
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# META-ANALYSIS ---------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# function to run JAGS meta-analyses given the data, initial values for parameters, and model with likelihood and priors
RunMetaAnalysis <- function(num.observations, observed.y, observed.var, num.group, group.input, sample.size, 
                            observed.counts, parameter.inits, save.parameters, file.jags) {
  if(hasArg(num.group) & !hasArg(sample.size)) {
    jags.data <- list(num.observations = num.observations, observed.y = observed.y, observed.var = observed.var, 
                      num.group = num.group, group = group.input)
  } else if(hasArg(sample.size) & !hasArg(num.group)) {
    jags.data <- list(num.observations = num.observations, sample.size = sample.size, observed.counts = observed.counts)
  } else if(hasArg(sample.size) & hasArg(num.group)) {
    jags.data <- list(num.observations = num.observations, sample.size = sample.size, observed.counts = observed.counts, 
                      num.group = num.group, group = group.input)
  } else {
    jags.data <- list(num.observations = num.observations, observed.y = observed.y, observed.var = observed.var)
  }
  jags.fit <- jags(inits = parameter.inits, n.chains = num.chains, model.file = file.jags, working.directory = getwd(),
                    data = jags.data, parameters.to.save = save.parameters, n.thin = thinning.rate, n.iter = chain.length, 
                    n.burnin = num.burnin, DIC = TRUE)
  return(jags.fit)
}
#
# function to generate initial parameter values for meta-analyses
GenerateInits <- function() {
  list(overall.mean = runif(1), overall.precision = runif(1))
}
GenerateInitsGroupwise <- function() {
  list(beta = abs(runif(length(unique(subset.df$Group)))), overall.precision = runif(1))
}
GenerateInitsBinom <- function() {
  list(overall.mean = abs(rnorm(1)), overall.sd = runif(1))
}
GenerateInitsBinomGroupwise <- function() {
  list(beta = abs(rnorm(length(unique(subset.df$Group)))), overall.sd = runif(1))
}
#
# function to extract mean, variance, confidence intervals from JAGS output
ExtractJAGSOutput <- function(fit, groupwise, binom = FALSE) {
  if (groupwise == FALSE) {
    # extract means and CIs for overall mean and overall SD (among study heterogeneity)
    overall.mean <- grep("overall.mean", row.names(fit$BUGSoutput$summary))
    overall.sd <- grep("overall.sd", row.names(fit$BUGSoutput$summary))
    if(length(grep("estimated.y", row.names(fit$BUGSoutput$summary))) > 0) {
      estimated.values <- grep("estimated.y", row.names(fit$BUGSoutput$summary))
    } else {
      estimated.values <- grep("estimated.prob", row.names(fit$BUGSoutput$summary))
    }
    CI.95.low <- fit$BUGSoutput$summary[c(estimated.values, overall.mean, overall.sd), 3]  # 95% CI
    CI.95.high <- fit$BUGSoutput$summary[c(estimated.values, overall.mean, overall.sd), 7]
    estimates <- fit$BUGSoutput$summary[c(estimated.values, overall.mean, overall.sd), 1]  # mean
    GR.diag <- fit$BUGSoutput$summary[c(estimated.values, overall.mean, overall.sd), 8]  # Gelman-Rubin diagnostic
    effective.sample.size <- fit$BUGSoutput$summary[c(estimated.values, overall.mean, overall.sd), 9]  # effective sample size
    # calculate within study heterogeneity
    complete.index <- which(!is.na(subset.df$BootstrapMean) & !is.na(subset.df$ObservedMean))
    heterogeneity <- sum((1 / subset.df[complete.index, "BootstrapVar"]) * (subset.df[complete.index, "ObservedMean"] - estimates[1])^2)
    if (binom == TRUE) {
      # Back-convert binomial estimates
      CI.95.low <- exp(CI.95.low) / (1 + exp(CI.95.low))
      CI.95.high <- exp(CI.95.high) / (1 + exp(CI.95.high))
      estimates <- exp(estimates) / (1 + exp(estimates))
      heterogeneity <- sum((1 / subset.df[which(!is.na(subset.df$EffectSize)), "var"]) * (subset.df[which(!is.na(subset.df$EffectSize)), "EffectSize"] - estimates[1])^2)
    }
    output <- data.frame(MetricName = metric.names[a], Network = c(as.character(subset.df$Network), rep("All", length(c(overall.mean, overall.sd, heterogeneity)))),
                         Parameter = c(paste("MetaAnalysisMean", 1:length(estimated.values), sep = ""), "OverallMean", "OverallSD", "Heterogeneity"), 
                         MetaAnalysisMean = c(estimates, heterogeneity), MetaAnalysisCIL = c(CI.95.low, NA), MetaAnalysisCIU = c(CI.95.high, NA), 
                         GR = c(GR.diag, NA), EffectiveN = c(effective.sample.size, NA))
    return(output)
  } else if (groupwise == TRUE) {
    groupwise.mean <- grep("beta", row.names(fit$BUGSoutput$summary))
    overall.sd <- grep("overall.sd", row.names(fit$BUGSoutput$summary))
    CI.95.low <- fit$BUGSoutput$summary[c(groupwise.mean, overall.sd), 3]
    CI.95.high <- fit$BUGSoutput$summary[c(groupwise.mean, overall.sd), 7]
    estimates <- fit$BUGSoutput$summary[c(groupwise.mean, overall.sd), 1]
    GR.diag <- fit$BUGSoutput$summary[c(groupwise.mean, overall.sd), 8]
    effective.sample.size <- fit$BUGSoutput$summary[c(groupwise.mean, overall.sd), 9]
    if (binom == TRUE) {
      # Back-convert binomial estimates
      CI.95.low <- exp(CI.95.low) / (1 + exp(CI.95.low))
      CI.95.high <- exp(CI.95.high) / (1 + exp(CI.95.high))
      estimates <- exp(estimates) / (1 + exp(estimates))
    }
    output <- data.frame(MetricName = rep(metric.names[a], length(estimates)), Network = rep("All", length(estimates)), Parameter = c(group.labels, "OverallSD"), 
                         MetaAnalysisMean = estimates, MetaAnalysisCIL = CI.95.low, MetaAnalysisCIU = CI.95.high, GR = GR.diag, EffectiveN = effective.sample.size)
    return(output)
  }
}
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MODEL SELECTION----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# function to run JAGS given the data, initial values for parameters, and model with likelihood and priors
FitDistribution <- function(num.observations, y.input, parameter.inits, file.jags) {
  jags.data <- list(num.observations = num.observations, y = y.input)
  if(is.null(parameter.inits()$param2)) {
    save.parameters <- c("param1", "loglik")
  } else {
    save.parameters <- c("param1", "param2", "loglik")
  }
  jags.fit <- jags(inits = parameter.inits, n.chains = num.chains, model.file = file.jags, working.directory = getwd(),
                   data = jags.data, parameters.to.save = save.parameters, n.thin = thinning.rate, n.iter = chain.length,
                   n.burnin = num.burnin, DIC = TRUE)
  return(jags.fit)
}
#
# functions to set initial values by drawing random values (from a given distribution) for each parallel chain
#
# normal inits, mean drawn from std. normal, sd drawn from uniform greater than zero and less than 2
normal.inits <- function() {
  list(param1 = rnorm(n = 1, mean = 0, sd = 1), param2 = runif(n = 1, min = 0, max = 2))
}
#
# exponential inits for rate, uniform distribution greater than zero and less than 10
exponential.inits <- function() {
  list(param1 = runif(n = 1, min = 0, max = 10))
}

# pareto inits for alpha, uniform greater than 2 and less than 10, and c, uniform must be less than the minimum value of data
pareto.inits <- function() {
  list(param1 = runif(n = 1, min = 2, max = 10), param2 = runif(n = 1, min = 0, max = min(metric.list[[m]])))
}
#
# distribution-specific functions to take random samples of a certain size (sample.size) given one or two parameters
#
SampleNormalDistribution <- function(sample.size, param1, param2) {
  mapply(rnorm, n = sample.size, mean = param1, sd = param2)
}
#
SampleLNormDistribution <- function(sample.size, param1, param2) {
  mapply(rlnorm, n = sample.size, meanlog = param1, sdlog = param2)
}
#
SampleExponentialDistribution <- function(sample.size, param1) {
  mapply(rexp, n = sample.size, rate = param1)
}
#
SampleParetoDistribution <- function(sample.size, param1, param2) {
  mapply(rpareto, n = sample.size, shape = param1, scale = param2)
}
#
# function to run posterior predictive checks: comparing yrep and y (the posterior distribution and observed distribution) 
# and comparing the medians of each distribution in bootstrap
#
PosteriorPredictiveCheck <- function(fit, fit.func, obs.dat, dist.name) {
  if(any(fit$parameters.to.save == "param2")) {
    # bootstrap of means and medians for random samples of distributions with fitted parameters from posterior distribution
    # compared to mean or median of observed data
    posterior.predictive.param1 <- fit$BUGSoutput$sims.matrix[, "param1"]
    posterior.predictive.param2 <- fit$BUGSoutput$sims.matrix[, "param2"]
    posterior.predictive <- fit.func(sample.size = length(obs.dat), param1 = posterior.predictive.param1, param2 = posterior.predictive.param2)
    posterior.predictive.median <- reshape2::melt(apply(posterior.predictive, 2, median), value.name = "Median")
    posterior.predictive.median.proportion <- length(which(posterior.predictive.median >= median(obs.dat))) / nrow(fit$BUGSoutput$sims.matrix)
    posterior.predictive.mean <- reshape2::melt(apply(posterior.predictive, 2, mean), value.name = "Mean")
    posterior.predictive.mean.proportion <- length(which(posterior.predictive.mean >= mean(obs.dat))) / nrow(fit$BUGSoutput$sims.matrix)
    posterior.predictive.upper <- reshape2::melt(apply(posterior.predictive, 2, function(x) quantile(x, 0.9)), value.name = "Upper90")
    posterior.predictive.upper.proportion <- length(which(posterior.predictive.upper >= quantile(obs.dat, 0.9))) / nrow(fit$BUGSoutput$sims.matrix)
    posterior.predictive.out <- data.frame(metric = metric.names[m], Distribution = dist.name, param1 = fit$BUGSoutput$mean$param1, 
                                            param2 = fit$BUGSoutput$mean$param2, D = fit$BUGSoutput$mean$deviance, 
                                            PPPmedian = posterior.predictive.median.proportion, PPPmean = posterior.predictive.mean.proportion,
                                            PPPupper = posterior.predictive.upper.proportion)
    return(posterior.predictive.out)
  } else {
    posterior.predictive.param1 <- fit$BUGSoutput$sims.matrix[, "param1"]
    posterior.predictive <- fit.func(sample.size = length(obs.dat), param1 = posterior.predictive.param1)
    posterior.predictive.median <- reshape2::melt(apply(posterior.predictive, 2, median), value.name = "Median")
    posterior.predictive.median.proportion <- length(which(posterior.predictive.median >= median(obs.dat))) / nrow(fit$BUGSoutput$sims.matrix)
    posterior.predictive.mean <- reshape2::melt(apply(posterior.predictive, 2, mean), value.name = "Mean")
    posterior.predictive.mean.proportion <- length(which(posterior.predictive.mean >= mean(obs.dat))) / nrow(fit$BUGSoutput$sims.matrix)
    posterior.predictive.upper <- reshape2::melt(apply(posterior.predictive, 2, function(x) quantile(x, 0.9)), value.name = "Upper90")
    posterior.predictive.upper.proportion <- length(which(posterior.predictive.upper >= quantile(obs.dat, 0.9))) / nrow(fit$BUGSoutput$sims.matrix)   
    posterior.predictive.out <- data.frame(metric = metric.names[m], Distribution = dist.name, param1 = fit$BUGSoutput$mean$param1, param2 = NA, 
                                            D = fit$BUGSoutput$mean$deviance, PPPmedian = posterior.predictive.median.proportion, 
                                            PPPmean = posterior.predictive.mean.proportion, PPPupper = posterior.predictive.upper.proportion)
    return(posterior.predictive.out)
  }
}
#
# function to calculate WAIC given fit from JAGS model
CalculateWAIC <- function(fit) {
  param <- fit$BUGSoutput$sims.list
  LL <- param$loglik
  WAIC <- waic(LL)
  return(WAIC)
}
#
# functions for plotting fitted distributions
#
# sample from a given distribution: provide distribution name and sampling function
# will sample from all networks that best fit that distribution and compile data in single df (for plotting)
SamplePosteriorNetwork <- function(models.init, dist.name, dist.func) {
  models <- models.init[which(models.init$Distribution == dist.name), ]
  samp <- matrix(NA, nrow = sample.size, ncol = nrow(models))
  if(dist.name != "Exponential") {
    for (i in 1:nrow(models)) {
      row.temp <- models[i, ]
      samp.temp <- dist.func(sample.size = sample.size, param1 = row.temp$param1, param2 = row.temp$param2)
      samp[, i] <- samp.temp
    }
  } else {
    for (i in 1:nrow(models)) {
      row.temp <- models[i, ]
      samp.temp <- dist.func(sample.size = sample.size, param1 = row.temp$param1)
      samp[, i] <- samp.temp
    }
  }
  colnames(samp) <- models$Case
  samp <- reshape2::melt(samp)
  df <- data.frame(Case = samp[, 2], Type = rep("Fitted", nrow(samp)), Distribution = rep(dist.name, nrow(samp)), 
                    Fit = rep(models$Fit, each = sample.size), Values = samp$value)
  return(df)
}
#
# need a special function for Pareto
SamplePosteriorParetoNetwork <- function(models) {
  dat <- list(alpha = models[which(models$Distribution == "Pareto"), "param1"], 
              c = models[which(models$Distribution == "Pareto"), "param2"], N = sample.size, 
              case = nrow(models[which(models$Distribution == "Pareto"), ]))
  sim <- run.jags(model = paretomodel, data = dat, monitor = c("y"), sample = 1, n.chains = 1, summarise = FALSE)
  sim <- coda::as.mcmc(sim)
  sim <- matrix(data = as.vector(sim), nrow = sample.size, ncol = nrow(models[which(models$Distribution == "Pareto"), ]))
  colnames(sim) <- models[which(models$Distribution == "Pareto"), "Case"]
  samp <- reshape2::melt(sim)
  df <- data.frame(Case = samp[, 2], Type = rep("Fitted", nrow(samp)), Distribution = rep("Pareto", nrow(samp)), 
                    Fit = rep(models[which(models$Distribution == "Pareto"), "Fit"], each = sample.size), Values = samp$value)
}
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SPECIES CHARACTERISTICS------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# function to determine difference in mean strengths grouped by character (like a t-test of unequal variances)
EstimateMeanDifferences <- function(s.in.obs, s.out.obs, sigma.in.obs, sigma.out.obs, groupID.in, groupID.out) {
  jags.data <- list(s.in.obs = s.in.obs, s.out.obs = s.out.obs, sigma.in.obs = sigma.in.obs, sigma.out.obs = sigma.out.obs, 
                    groupID.in = groupID.in, groupID.out = groupID.out)
  jags.inits <- function() {
    list(beta0.out = rnorm(1), beta0.in = rnorm(1), beta1.out = rnorm(1), beta1.in = rnorm(1), 
         sigma.out = runif(1), sigma.in = runif(1))
  }
  jags.pars <- c("beta0.out", "beta0.in", "beta1.out", "beta1.in", "mu.out", "mu.in", "sigma.out", "sigma.in")
  jags.fit <- jags(inits = jags.inits, n.chains = num.chains, model.file = mean.difference.model, working.directory = getwd(),
                    data = jags.data, parameters.to.save = jags.pars, n.thin = thinning.rate, n.iter = chain.length, 
                    n.burnin = num.burnin, DIC = TRUE)
  return(jags.fit)
}
#
# function to extract mean, variance, confidence intervals from JAGS output, comparison strength given species characters
ExtractJAGSMeanDifferences <- function(fit, char.name, char.options) {
  # extract means and CIs for beta1 (difference in means) mu (group means) and sigma (sd of group means)
  beta1 <- grep("beta1", row.names(fit$BUGSoutput$summary))
  mu.in <- grep("mu.in", row.names(fit$BUGSoutput$summary))[1:2]
  mu.out <- grep("mu.out", row.names(fit$BUGSoutput$summary))[1:2]
  sigma <- grep("sigma", row.names(fit$BUGSoutput$summary))
  sub.output <- fit$BUGSoutput$summary[c(beta1, mu.in, mu.out, sigma), c(1, 2, 3, 7, 8, 9)]
  output <- data.frame(Comparison = rep(char.name, nrow(sub.output)), 
                       MetricName = c("Difference", "Difference", paste("Mean", char.options), paste("Mean", char.options),
                                      "SD", "SD"),
                       Strength = c("In-strength", "Out-strength", "In-strength", "In-strength", "Out-strength", "Out-strength", "In-strength", "Out-strength"),
                       Mean = sub.output[, 1],  SD = sub.output[, 2], CILL = sub.output[, 3], CIUL = sub.output[, 4], 
                       GR = sub.output[, 5], EffectiveN = sub.output[, 6])
  return(output)
}
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ADDITIVITY-------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# function to run JAGS, input standardized differences and group IDs
FitAdditiveJAGSModel <- function(y.input, g.input) {
  jags.data <- list(y = y.input, group = g.input, g = length(unique(g.input)))
  jags.inits <- function() {
    list(tau = runif(1), tau.g = runif(1), mu.g = rnorm(1))
  }
  jags.pars <- c("beta", "mu.g", "sigma", "sigma.g")
  fit <- jags(inits = jags.inits, n.chains = num.chains, model.file = additive.model, working.directory = getwd(),
              data = jags.data, parameters.to.save = jags.pars, n.thin = thinning.rate, n.iter = chain.length, n.burnin = num.burnin, DIC = TRUE)
  return(fit)
}
#
# function to extract parameters from JAGS output
ExtractAdditiveJAGSModel <- function(fit) {
  mu <- grep("mu", row.names(fit$BUGSoutput$summary))
  beta <- grep("beta", row.names(fit$BUGSoutput$summary))
  sigma <- grep("sigma", row.names(fit$BUGSoutput$summary))
  output.raw <- fit$BUGSoutput$summary[c(mu, beta, sigma), c(1, 3, 7, 8, 9)]
  output <- data.frame(Parameter = c("grand mean", rep("group mean", 4), "among-group sd", "within-group sd"), 
                        Network = c("All", name, "All", "All"), Mean = output.raw[, 1], CIL = output.raw[, 2], 
                        CIU = output.raw[, 3], GR = output.raw[, 4], EffectiveN = output.raw[, 5])
  return(output)
}

