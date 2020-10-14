# COMPARING SPECIES' ROLE IN MULTIPLE NETWORKS
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INITIALIZE MATCHES --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# set up list with Unique ID of case and all species
spp.list <- setNames(split(as.character(coding.df$Species), seq(nrow(coding.df))), coding.df$UniqueID)
spp.list <- lapply(spp.list, function(x) strsplit(x, ", "))
spp.list <- lapply(spp.list, unlist)
#
# loop to find species matches in multiple networks
spp.matches <- data.frame(spp = character(), matchFrom = character(), matchIn = character(), Position = character(), 
                          stringsAsFactors = FALSE)
rowcount <- 1
for (i in 1:length(spp.list)) {
  for (j in 1:length(spp.list[[i]])) {
    for (k in 1:length(spp.list)) {
      if (i == k) {
        next
      } else if (length(grep(pattern = spp.list[[i]][j], x = spp.list[[k]])) > 0) {
        match <- c(spp.list[[i]][j], names(spp.list)[i], names(spp.list)[k], 
                    grep(pattern = spp.list[[i]][j], x = spp.list[[k]]))
        spp.matches[rowcount, ] <- match
        rowcount <- rowcount + 1
      } else {
        next
      }
    }
  }
}
spp.matches <- spp.matches[order(spp.matches$spp), ]
# species matches to compare across networks: Dactylis glomerata, Plantago lanceolata, and Trifolium pratense
#
spp.names <- c("Dactylis_glomerata", "Plantago_lanceolata", "Trifolium_pratense")
dact <- spp.matches[which(spp.matches$spp == "Dactylis_glomerata"), ]  # subset only Dactylis, Plantago, or Trifolium
dact <- dact[!duplicated(dact$matchIn), -2]  # remove duplicates
plan <- spp.matches[which(spp.matches$spp == "Plantago_lanceolata"), ]  # subset only Plantago
plan <- plan[!duplicated(plan$matchIn), -2]  # remove duplicates
trif <- spp.matches[which(spp.matches$spp == "Trifolium_pratense"), ]  # subset only Trifolium
trif <- trif[!duplicated(trif$matchIn), -2]  # remove duplicates
spp.df <- rbind(dact, plan, trif)
#
# find matching networks
<<<<<<< HEAD
case.names <- list.files(path = paste(path, "Input/CaseData", sep = ""), recursive = FALSE)
case.names <- substr(case.names, 0, nchar(case.names) - 4)
case.names[grep(case.names, pattern = "_imp")] <- substr(case.names[grep(case.names, pattern = "_imp")], 0, nchar(case.names[grep(case.names, pattern = "_imp")]) - 4)
case.names[grep(case.names, pattern = "_Cntrl")] <- substr(case.names[grep(case.names, pattern = "_Cntrl")], 0, nchar(case.names[grep(case.names, pattern = "_Cntrl")]) - 6)
file.paths <- list.files(path = paste(path, "/Input/CaseData", sep = ""), full.names = TRUE)
treatment.test <- substr(case.names, nchar(case.names), nchar(case.names))
treatment.indices <- c(grep(pattern = "[[:digit:]]", x = treatment.test), grep(pattern = "l", x = treatment.test))
case.names <- case.names[treatment.indices]
case.data <- lapply(file.paths, read.csv)
case.data <- case.data[treatment.indices]
||||||| 2b1eac9
=======
case.names <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", recursive = FALSE)
case.names <- substr(case.names, 0, nchar(case.names) - 4)
case.names[grep(case.names, pattern = "_imp")] <- substr(case.names[grep(case.names, pattern = "_imp")], 0, nchar(case.names[grep(case.names, pattern = "_imp")]) - 4)
case.names[grep(case.names, pattern = "_Cntrl")] <- substr(case.names[grep(case.names, pattern = "_Cntrl")], 0, nchar(case.names[grep(case.names, pattern = "_Cntrl")]) - 6)

file.paths <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", full.names = TRUE)
treatment.test <- substr(case.names, nchar(case.names), nchar(case.names))
treatment.indices <- c(grep(pattern = "[[:digit:]]", x = treatment.test), grep(pattern = "l", x = treatment.test))
case.names <- case.names[treatment.indices]
case.data <- case.data[treatment.indices]
case.data <- lapply(file.paths, read.csv)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
id.matches <- sapply(X = unique(spp.df$matchIn), function(x) grep(pattern = x, x = case.names))
<<<<<<< HEAD
case.names <- case.names[id.matches]
case.data <- case.data[id.matches]
meta.spp <- data.frame(Network = character(), Species = character(), MetricName = character(), ObservedValue = numeric(), 
                        BootstrapMean = numeric(), BootstrapSD = numeric(), BootstrapCIL = numeric(), BootstrapCIU = numeric(), 
                        stringsAsFactors = FALSE)
#
#
||||||| 2b1eac9
case.spp <- case.names[id.matches]
b.spp <- b.all[id.matches]
s.ctrl <- c(2, 3, 7)  # true ctrl

meta.spp <- data.frame(network = character(), species = character(), metric = character(), obs = numeric(), mean = numeric(), sd = numeric(), CI.l = numeric(), CI.u = numeric(), stringsAsFactors = FALSE)

=======
case.names <- case.names[id.matches]
case.data <- case.data[id.matches]

meta.spp <- data.frame(Network = character(), Species = character(), MetricName = character(), ObservedValue = numeric(), BootstrapMean = numeric(),
                       BootstrapSD = numeric(), BootstrapCIL = numeric(), BootstrapCIU = numeric(), stringsAsFactors = FALSE)

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOOP THROUGH NETWORKS -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# calculate in- and out-strength for species in each network and store
for (case in 1:length(case.names)) {
  uniqueid <- as.numeric(substr(x = case.names[case], start = 1, stop = gregexpr("_", case.names[case])[[1]][1] - 1))
  dat <- case.data[[case]]
  num.species <- max(unique(dat$Target))
  treatment <- nrow(dat[dat$Metric > 0, ])
  sample.size <- max(dat$N)
  if (any(colnames(dat) == "SE")) {
    SD <- dat$SE * sqrt(dat$N)
    dat$SD <- SD
  }
  dat$var <- dat$SD^2
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
  targets <- dat$Target[which(dat$Metric != 0)]
  neighbors <- dat$Neighbor[which(dat$Metric != 0)]
  boot.df <- data.frame(Network = character(), Species = character(), MetricName = character(), value = numeric(), stringsAsFactors = FALSE)
  weights <- matrix(rexp(N * R, 1) , ncol = N, byrow = TRUE)
  weights <- weights / rowSums(weights)
  for (iteration in 1:R) {
<<<<<<< HEAD
    b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample.size, replace = TRUE, prob = weights[iteration, ]))
    b.boot.long <- reshape2::melt(b.boot)
    b.boot.long <- data.frame(Target = rep(targets, each = sample.size), Neighbor = rep(neighbors, each = sample.size), Metric = b.boot.long[, 3])
    M.boot <- MonoControlRII(b.boot.long, num.species)
    diag(M.boot) <- NA
||||||| 2b1eac9
    b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample_size, replace = TRUE, prob = weights[iteration, ]))
    b.boot.long <- melt(b.boot)
    b.boot.long <- data.frame(Target = rep(targets, each = sample_size), Neighbor = rep(neighbors, each = sample_size), Metric = b.boot.long[, 3])
    rii.boot <- c()
    count <- 1
    if (case %in% s.ctrl) {
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
    M <- t(M.boot)

=======
    b.boot <- apply(X = b.sim, 2, function(x) sample(x, size = sample.size, replace = TRUE, prob = weights[iteration, ]))
    b.boot.long <- melt(b.boot)
    b.boot.long <- data.frame(Target = rep(targets, each = sample.size), Neighbor = rep(neighbors, each = sample.size), Metric = b.boot.long[, 3])
    M.boot <- MonoControlRII(b.boot.long, num.species)
    diag(M.boot) <- NA
    M <- t(M.boot)

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
    spp.idx <- as.numeric(spp.df$Position[which(spp.df$matchIn == uniqueid)])
    spp.name <- spp.df$spp[which(spp.df$matchIn == uniqueid)]
<<<<<<< HEAD
    s.out <- colSums(M.boot, na.rm = TRUE)
    s.in <- rowSums(M.boot, na.rm = TRUE)
    boot.row <- data.frame(Network = rep(case.names[case], 2 * length(spp.idx)), Species = rep(spp.name, 2),
                            MetricName = rep(c("Out-strength", "In-strength"), each = length(spp.idx)),
                            value = c(s.out[spp.idx], s.in[spp.idx]), stringsAsFactors = FALSE)
||||||| 2b1eac9
    s.out <- rowSums(M, na.rm = TRUE)
    s.in <- colSums(M, na.rm = TRUE)
    boot.row <- data.frame(network = rep(case.spp[case], 2 * length(spp.idx)), species = rep(spp.name, 2), metric = rep(c("out-strength", "in-strength"), each = length(spp.idx)),
               value = c(s.out[spp.idx], s.in[spp.idx]), stringsAsFactors = FALSE)
=======
    s.out <- rowSums(M, na.rm = TRUE)
    s.in <- colSums(M, na.rm = TRUE)
    boot.row <- data.frame(Network = rep(case.names[case], 2 * length(spp.idx)), Species = rep(spp.name, 2), MetricName = rep(c("Out-strength", "In-strength"), each = length(spp.idx)),
               value = c(s.out[spp.idx], s.in[spp.idx]), stringsAsFactors = FALSE)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
    boot.df <- rbind(boot.df, boot.row)
  }
<<<<<<< HEAD
  M.obs <- MonoControlRII(dat, num.species)
  diag(M.boot) <- NA
  s.out <- colSums(M.boot, na.rm = TRUE)
  s.in <- rowSums(M.boot, na.rm = TRUE)
  obs.row <- data.frame(Network = rep(case.names[case], 2 * length(spp.idx)), Species = rep(spp.name, 2), 
                          MetricName = rep(c("Out-strength", "In-strength"), each = length(spp.idx)),
                          ObservedValue = c(s.out[spp.idx], s.in[spp.idx]), stringsAsFactors = FALSE)
  boot.out <- ddply(boot.df, .(Network, Species, MetricName), summarise, BootstrapMean = mean(value, na.rm = TRUE), 
                    BootstrapSD = sd(value, na.rm = TRUE), BootstrapCIL = quantile(value, probs = c(0.025), na.rm = TRUE), 
                    BootstrapCIU = quantile(value, probs = c(0.975), na.rm = TRUE))
||||||| 2b1eac9
  
  rii.obs <- c()
  if (case %in% s.ctrl) {
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
  s.in <- colSums(M, na.rm = TRUE)
  obs.row <- data.frame(network = rep(case.spp[case], 2 * length(spp.idx)), species = rep(spp.name, 2), metric = rep(c("out-strength", "in-strength"), each = length(spp.idx)),
                         obs = c(s.out[spp.idx], s.in[spp.idx]), stringsAsFactors = FALSE)
  boot.out <- ddply(boot.df, .(network, species, metric), summarise, mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE), 
                    CI.L = quantile(value, probs = c(0.025), na.rm = TRUE), CI.U = quantile(value, probs = c(0.975), na.rm = TRUE))
=======
  
  M.obs <- MonoControlRII(dat, num.species)
  M <- t(M.obs)

  s.out <- rowSums(M, na.rm = TRUE)
  s.in <- colSums(M, na.rm = TRUE)
  obs.row <- data.frame(Network = rep(case.names[case], 2 * length(spp.idx)), Species = rep(spp.name, 2), MetricName = rep(c("Out-strength", "In-strength"), each = length(spp.idx)),
                         ObservedValue = c(s.out[spp.idx], s.in[spp.idx]), stringsAsFactors = FALSE)
  boot.out <- ddply(boot.df, .(Network, Species, MetricName), summarise, BootstrapMean = mean(value, na.rm = TRUE), BootstrapSD = sd(value, na.rm = TRUE), 
                    BootstrapCIL = quantile(value, probs = c(0.025), na.rm = TRUE), BootstrapCIU = quantile(value, probs = c(0.975), na.rm = TRUE))
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  meta.spp.init <- merge(x = obs.row, y = boot.out)
  meta.spp <- rbind(meta.spp, meta.spp.init)
  print(paste("Species position for network ", case, case.names[case], " is complete"))
}
#
# save output
<<<<<<< HEAD
write.csv(x = meta.spp, file = paste(path, "Output/SpeciesPosition.csv", sep = ""))
||||||| 2b1eac9
write.csv(x = meta.spp, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_position.csv")
=======
write.csv(x = meta.spp, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesPosition.csv")
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
