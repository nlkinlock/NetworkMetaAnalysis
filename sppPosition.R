# COMPARING SPECIES' ROLE IN MULTIPLE NETWORKS
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INITIALIZE MATCHES --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# set up list with Unique ID of case and all species
spp.list <- setNames(split(as.character(df$Species), seq(nrow(df))), df$UniqueID)
spp.list <- lapply(spp.list, function(x) strsplit(x, ", "))
spp.list <- lapply(spp.list, unlist)

# loop to find species matches in multiple networks
spp.matches <- data.frame(spp = character(), matchFrom = character(), matchIn = character(), Position = character(), stringsAsFactors = FALSE)
rowcount <- 1
for (i in 1:length(spp.list)) {
  for (j in 1:length(spp.list[[i]])) {
    for (k in 1:length(spp.list)) {
      if (i == k) {
        next
      } else if (length(grep(pattern = spp.list[[i]][j], x = spp.list[[k]])) > 0) {
        match <- c(spp.list[[i]][j], names(spp.list)[i], names(spp.list)[k], grep(pattern = spp.list[[i]][j], x = spp.list[[k]]))
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

spp.names <- c("Dactylis_glomerata", "Plantago_lanceolata", "Trifolium_pratense")
dact <- spp.matches[which(spp.matches$spp == "Dactylis_glomerata"), ]  # subset only Dactylis, Plantago, or Trifolium
dact <- dact[!duplicated(dact$matchIn), -2]  # remove duplicates
plan <- spp.matches[which(spp.matches$spp == "Plantago_lanceolata"), ]  # subset only Plantago
plan <- plan[!duplicated(plan$matchIn), -2]  # remove duplicates
trif <- spp.matches[which(spp.matches$spp == "Trifolium_pratense"), ]  # subset only Trifolium
trif <- trif[!duplicated(trif$matchIn), -2]  # remove duplicates
spp.df <- rbind(dact, plan, trif)
# find matching networks
id.matches <- sapply(X = unique(spp.df$matchIn), function(x) grep(pattern = x, x = case.names))
case.spp <- case.names[id.matches]
b.spp <- b.all[id.matches]
s.ctrl <- c(2, 3, 7)  # true ctrl

meta.spp <- data.frame(network = character(), species = character(), metric = character(), obs = numeric(), mean = numeric(), sd = numeric(), CI.l = numeric(), CI.u = numeric(), stringsAsFactors = FALSE)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOOP THROUGH NETWORKS -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# calculate in- and out-strength for species in each network and store
for (case in 1:length(case.spp)) {
  uniqueid <- as.numeric(substr(x = case.spp[case], start = 1, stop = gregexpr("_", case.spp[case])[[1]][1] - 1))
  dat <- b.spp[[case]]
  species <- max(unique(dat$Target))
  treatment <- nrow(dat[dat$Metric > 0, ])
  sample_size <- max(dat$N)
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
  
  boot.df <- data.frame(network = character(), species = character(), metric = character(), value = numeric(), stringsAsFactors = FALSE)
  
  weights <- matrix(rexp(N * R, 1) , ncol = N, byrow = TRUE)
  weights <- weights / rowSums(weights)
  for (iteration in 1:R) {
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

    spp.idx <- as.numeric(spp.df$Position[which(spp.df$matchIn == uniqueid)])
    spp.name <- spp.df$spp[which(spp.df$matchIn == uniqueid)]
    s.out <- rowSums(M, na.rm = TRUE)
    s.in <- colSums(M, na.rm = TRUE)
    boot.row <- data.frame(network = rep(case.spp[case], 2 * length(spp.idx)), species = rep(spp.name, 2), metric = rep(c("out-strength", "in-strength"), each = length(spp.idx)),
               value = c(s.out[spp.idx], s.in[spp.idx]), stringsAsFactors = FALSE)
    boot.df <- rbind(boot.df, boot.row)
  }
  
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
  meta.spp.init <- merge(x = obs.row, y = boot.out)

  meta.spp <- rbind(meta.spp, meta.spp.init)
  print(paste("Network ", case, case.spp[case], " is complete"))
}

# save output
write.csv(x = meta.spp, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_position.csv")
