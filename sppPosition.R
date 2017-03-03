library(fitdistrplus)
library(reshape2)
library(ggplot2)

#
# LOAD DATA ------------------------------------------------------------
#
# load coding table with extracted data from each case
df.init <- read.csv("CodingTable.csv")
col.remove <- c("Title", "doi", "Abstract", "Status", "OtherTreatments", "Metric", "Variation", "Variation.1")  # unwanted columns
df <- df.init[df.init$Status == "Complete", !(names(df.init) %in% col.remove)]

# set up list with Unique ID of case and all species
spp.list <- setNames(split(as.character(df$Species), seq(nrow(df))), df$UniqueID)
spp.list <- lapply(spp.list, function(x) strsplit(x, ", "))
spp.list <- lapply(spp.list, unlist)
unlist(lapply(spp.list, length))
# loop to find species matches in multiple networks
outputdf <- data.frame(spp = character(), matchFrom = character(), matchIn = character(), Position = character(), stringsAsFactors = FALSE)
rowcount <- 1
for (i in 1:length(spp.list)) {
  for (j in 1:length(spp.list[[i]])) {
    for (k in 1:length(spp.list)) {
      if (i == k) {
        next
      } else if (length(grep(pattern = spp.list[[i]][j], x = spp.list[[k]])) > 0) {
        output <- c(spp.list[[i]][j], names(spp.list)[i], names(spp.list)[k], grep(pattern = spp.list[[i]][j], x = spp.list[[k]]))
        outputdf[rowcount, ] <- output
        rowcount <- rowcount + 1
      } else {
        next
      }
    }
  }
}
outputdf <- outputdf[order(outputdf$spp), ]
# species matches to compare across networks: Dactylis glomerata, Festuca arundinacea, Plantago lanceolata, and Trifolium repens

# set number of bootstrap iterations
iterations <- 10000

#
# DACTYLIS GLOMERATA ------------------------------------------------------------
#
dact <- outputdf[which(outputdf$spp == "Dactylis_glomerata"), ]  # subset only Dactylis
dact <- dact[!duplicated(dact$matchIn), -2]  # remove duplicates
dact <- dact[-5, ] # case with RY only
# load respective networks for each case in both MixMono and Cntrl
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/MixMono")
files <- dir(pattern = "*.csv", full.names = TRUE)
matches <- unlist(sapply(dact$matchIn, function(x) grep(pattern = x, files, value = TRUE)))
M.dact <- list()
M.sd.dact <- list()
M.dact[1] <- lapply(matches[1], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.dact[1] <- lapply(matches[2], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.dact[2] <- lapply(matches[3], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.dact[2] <- lapply(matches[4], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl")
files <- dir(pattern = "*.csv", full.names = TRUE)
matches <- unlist(sapply(dact$matchIn, function(x) grep(pattern = x, files, value = TRUE)))
M.dact[3] <- lapply(matches[1], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.dact[3] <- lapply(matches[2], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.dact[4] <- lapply(matches[3], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.dact[4] <- lapply(matches[4], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
for (r in 1:length(M.dact)) {
  M.dact[[r]][which(is.na(M.dact[[r]]), arr.ind = TRUE)] <- 0
  M.sd.dact[[r]][which(is.na(M.sd.dact[[r]]), arr.ind = TRUE)] <- 0
}
# empty data frames to store output for metrics
# general metrics separate from distribution fit metrics
dact.meta <- data.frame(Species = character(), UniqueID = integer(), Metric = character(), Mean = numeric(), CIL = numeric(), CIU = numeric(), stringsAsFactors = FALSE)
rowNames <- c("out strength", "in strength", "comp in strength", "comp out strength", "fac in strength", "fac out strength")
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis")
for (m in 1:length(M.dact)) {
  # initialize temp vectors to store output for a given network
  s.out.store <- numeric(length = iterations)
  s.in.store <- numeric(length = iterations)
  s.in.c.store <- numeric(length = iterations)
  s.out.c.store <- numeric(length = iterations)
  s.in.f.store <- numeric(length = iterations)
  s.out.f.store <- numeric(length = iterations)
  for (count in 1:iterations) {
    # nested loop for bootstrap
    pos <- as.integer(dact$Position[m])
    # incorporate variability using sd of network elements
    mean.matrix <- t(M.dact[[m]])
    sd.matrix <- t(M.sd.dact[[m]])
    if (any(sd.matrix == 0)) {
      sd.matrix <- sd.matrix + 1E-9
    }
    M <- matrix(mapply(rnorm, 1, mean.matrix, sd.matrix), nrow = nrow(mean.matrix), ncol = ncol(mean.matrix))
    M[which(abs(M) <= 1E-9, arr.ind = TRUE)] <- 0
    # node characteristics (weight, in/out strength)
    # both facilitative and competitive
    s.out <- rowSums(M) / nrow(M)
    s.in <- colSums(M) / nrow(M)
    # Only competitive
    M.c <- ifelse(test = M > 0, yes = 0, no = M)
    s.out.c <- rowSums(M.c, na.rm = TRUE) / nrow(M)
    s.in.c <- colSums(M.c, na.rm = TRUE) / nrow(M)
    # Only facilitative
    M.f <- ifelse(test = M < 0, yes = 0, no = M)
    s.out.f <- rowSums(M.f, na.rm = TRUE) / nrow(M)
    s.in.f <- colSums(M.f, na.rm = TRUE) / nrow(M)
    s.out.store[count] <- s.out[pos]
    s.in.store[count] <- s.in[pos]
    s.in.c.store[count] <- s.in.c[pos]
    s.out.c.store[count] <- s.out.c[pos]
    s.in.f.store[count] <- s.in.f[pos]
    s.out.f.store[count] <- s.out.f[pos]
  }
  # add output from one network to data frame with all networks
  dact.mean <- melt(unname(c(mean(s.out.store),  mean(s.in.store), mean(s.in.c.store), mean(s.out.c.store), mean(s.in.f.store), mean(s.out.f.store))), value.name = "Mean")
  dact.CIL <- melt(unname(c(quantile(s.out.store, probs = 0.025), quantile(s.in.store, probs = 0.025), quantile(s.in.c.store, probs = 0.025), quantile(s.out.c.store, probs = 0.025), 
                            quantile(s.in.f.store, probs = 0.025), quantile(s.out.f.store, probs = 0.025))), value.name = "CIL")
  dact.CIU <- melt(unname(c(quantile(s.out.store, probs = 0.975), quantile(s.in.store, probs = 0.975), quantile(s.in.c.store, probs = 0.975), quantile(s.out.c.store, probs = 0.975),
                            quantile(s.in.f.store, probs = 0.975), quantile(s.out.f.store, probs = 0.975))), value.name = "CIU")
  dact.temp <- data.frame(Species = rep("Dactylis glomerata", nrow(dact.mean)), UniqueID = rep(as.numeric(dact$matchIn[m]), nrow(dact.mean)), Metric = rowNames, Mean = dact.mean, CIL = dact.CIL, CIU = dact.CIU)
  dact.meta <- rbind(dact.meta, dact.temp) 
  print(paste("Network ", m, " is complete"))
}


#
# FESTUCA ARUNDINACEA ------------------------------------------------------------
#
fest <- outputdf[which(outputdf$spp == "Festuca_arundinacea"), ]  # subset only Dactylis
fest <- fest[!duplicated(fest$matchIn), -2]  # remove duplicates
fest <- fest[-1, ]
# load respective networks for each case in both MixMono and Cntrl
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/MixMono")
files <- dir(pattern = "*.csv", full.names = TRUE)
matches <- unlist(sapply(fest$matchIn, function(x) grep(pattern = x, files, value = TRUE)))
M.fest <- list()
M.sd.fest <- list()
M.fest[1] <- lapply(matches[1], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.fest[1] <- lapply(matches[2], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl")
files <- dir(pattern = "*.csv", full.names = TRUE)
matches <- unlist(sapply(fest$matchIn, function(x) grep(pattern = x, files, value = TRUE)))
M.fest[2] <- lapply(matches[1], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.fest[2] <- lapply(matches[2], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.fest[3] <- lapply(matches[3], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.fest[3] <- lapply(matches[4], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
for (r in 1:length(M.fest)) {
  M.fest[[r]][which(is.na(M.fest[[r]]), arr.ind = TRUE)] <- 0
  M.sd.fest[[r]][which(is.na(M.sd.fest[[r]]), arr.ind = TRUE)] <- 0
}

# empty data frames to store output for metrics
# general metrics separate from distribution fit metrics
fest.meta <- data.frame(Species = character(), UniqueID = integer(), Metric = character(), Mean = numeric(), CIL = numeric(), CIU = numeric(), stringsAsFactors = FALSE)
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis")
for (m in 1:length(M.fest)) {
  # initialize temp vectors to store output for a given network
  s.out.store <- numeric(length = iterations)
  s.in.store <- numeric(length = iterations)
  s.in.c.store <- numeric(length = iterations)
  s.out.c.store <- numeric(length = iterations)
  s.in.f.store <- numeric(length = iterations)
  s.out.f.store <- numeric(length = iterations)
  for (count in 1:iterations) {
    # nested loop for bootstrap
    pos <- as.integer(fest$Position[m])
    # incorporate variability using sd of network elements
    mean.matrix <- t(M.fest[[m]])
    sd.matrix <- t(M.sd.fest[[m]])
    if (any(sd.matrix == 0)) {
      sd.matrix <- sd.matrix + 1E-9
    }
    M <- matrix(mapply(rnorm, 1, mean.matrix, sd.matrix), nrow = nrow(mean.matrix), ncol = ncol(mean.matrix))
    M[which(abs(M) <= 1E-9, arr.ind = TRUE)] <- 0
    # node characteristics (weight, in/out strength)
    # both facilitative and competitive
    s.out <- rowSums(M) / nrow(M)
    s.in <- colSums(M) / nrow(M)
    # Only competitive
    M.c <- ifelse(test = M > 0, yes = 0, no = M)
    s.out.c <- rowSums(M.c, na.rm = TRUE) / nrow(M)
    s.in.c <- colSums(M.c, na.rm = TRUE) / nrow(M)
    # Only facilitative
    M.f <- ifelse(test = M < 0, yes = 0, no = M)
    s.out.f <- rowSums(M.f, na.rm = TRUE) / nrow(M)
    s.in.f <- colSums(M.f, na.rm = TRUE) / nrow(M)
    s.out.store[count] <- s.out[pos]
    s.in.store[count] <- s.in[pos]
    s.in.c.store[count] <- s.in.c[pos]
    s.out.c.store[count] <- s.out.c[pos]
    s.in.f.store[count] <- s.in.f[pos]
    s.out.f.store[count] <- s.out.f[pos]
  }
  # add output from one network to data frame with all networks
  fest.mean <- melt(unname(c(mean(s.out.store),  mean(s.in.store), mean(s.in.c.store), mean(s.out.c.store), mean(s.in.f.store), mean(s.out.f.store))), value.name = "Mean")
  fest.CIL <- melt(unname(c(quantile(s.out.store, probs = 0.025), quantile(s.in.store, probs = 0.025), quantile(s.in.c.store, probs = 0.025), quantile(s.out.c.store, probs = 0.025), 
                            quantile(s.in.f.store, probs = 0.025), quantile(s.out.f.store, probs = 0.025))), value.name = "CIL")
  fest.CIU <- melt(unname(c(quantile(s.out.store, probs = 0.975), quantile(s.in.store, probs = 0.975), quantile(s.in.c.store, probs = 0.975), quantile(s.out.c.store, probs = 0.975),
                            quantile(s.in.f.store, probs = 0.975), quantile(s.out.f.store, probs = 0.975))), value.name = "CIU")
  fest.temp <- data.frame(Species = rep("Festuca arundinacea", nrow(fest.mean)), UniqueID = rep(as.numeric(fest$matchIn[m]), nrow(fest.mean)), Metric = rowNames, Mean = fest.mean, CIL = fest.CIL, CIU = fest.CIU)
  fest.meta <- rbind(fest.meta, fest.temp)
  print(paste("Network ", m, " is complete"))
}


#
# PLANTAGO LANCEOLATA ------------------------------------------------------------
#
plan <- outputdf[which(outputdf$spp == "Plantago_lanceolata"), ]  # subset only Dactylis
plan <- plan[!duplicated(plan$matchIn), -2]  # remove duplicates
plan <- plan[-2, ]
# load respective networks for each case in both MixMono and Cntrl
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/MixMono")
files <- dir(pattern = "*.csv", full.names = TRUE)
matches <- unlist(sapply(plan$matchIn, function(x) grep(pattern = x, files, value = TRUE)))
M.plan <- list()
M.sd.plan <- list()
M.plan[1] <- lapply(matches[1], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.plan[1] <- lapply(matches[2], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.plan[2] <- lapply(matches[3], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.plan[2] <- lapply(matches[4], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.plan[4] <- lapply(matches[5], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.plan[4] <- lapply(matches[6], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl")
files <- dir(pattern = "*.csv", full.names = TRUE)
matches <- unlist(sapply(plan$matchIn, function(x) grep(pattern = x, files, value = TRUE)))
M.plan[3] <- lapply(matches[1], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.plan[3] <- lapply(matches[2], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
for (r in 1:length(M.plan)) {
  M.plan[[r]][which(is.na(M.plan[[r]]), arr.ind = TRUE)] <- 0
  M.sd.plan[[r]][which(is.na(M.sd.plan[[r]]), arr.ind = TRUE)] <- 0
}

# empty data frames to store output for metrics
# general metrics separate from distribution fit metrics
plan.meta <- data.frame(Species = character(), UniqueID = integer(), Metric = character(), Mean = numeric(), CIL = numeric(), CIU = numeric(), stringsAsFactors = FALSE)
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis")
for (m in 1:length(M.plan)) {
  # initialize temp vectors to store output for a given network
  s.out.store <- numeric(length = iterations)
  s.in.store <- numeric(length = iterations)
  s.in.c.store <- numeric(length = iterations)
  s.out.c.store <- numeric(length = iterations)
  s.in.f.store <- numeric(length = iterations)
  s.out.f.store <- numeric(length = iterations)
  for (count in 1:iterations) {
    # nested loop for bootstrap
    pos <- as.integer(plan$Position[m])
    # incorporate variability using sd of network elements
    mean.matrix <- t(M.plan[[m]])
    sd.matrix <- t(M.sd.plan[[m]])
    if (any(sd.matrix == 0)) {
      sd.matrix <- sd.matrix + 1E-9
    }
    M <- matrix(mapply(rnorm, 1, mean.matrix, sd.matrix), nrow = nrow(mean.matrix), ncol = ncol(mean.matrix))
    M[which(abs(M) <= 1E-9, arr.ind = TRUE)] <- 0
    # node characteristics (weight, in/out strength)
    # both facilitative and competitive
    s.out <- rowSums(M) / nrow(M)
    s.in <- colSums(M) / nrow(M)
    # Only competitive
    M.c <- ifelse(test = M > 0, yes = 0, no = M)
    s.out.c <- rowSums(M.c, na.rm = TRUE) / nrow(M)
    s.in.c <- colSums(M.c, na.rm = TRUE) / nrow(M)
    # Only facilitative
    M.f <- ifelse(test = M < 0, yes = 0, no = M)
    s.out.f <- rowSums(M.f, na.rm = TRUE) / nrow(M)
    s.in.f <- colSums(M.f, na.rm = TRUE) / nrow(M)
    s.out.store[count] <- s.out[pos]
    s.in.store[count] <- s.in[pos]
    s.in.c.store[count] <- s.in.c[pos]
    s.out.c.store[count] <- s.out.c[pos]
    s.in.f.store[count] <- s.in.f[pos]
    s.out.f.store[count] <- s.out.f[pos]
  }
  # add output from one network to data frame with all networks
  plan.mean <- melt(unname(c(mean(s.out.store),  mean(s.in.store), mean(s.in.c.store), mean(s.out.c.store), mean(s.in.f.store), mean(s.out.f.store))), value.name = "Mean")
  plan.CIL <- melt(unname(c(quantile(s.out.store, probs = 0.025), quantile(s.in.store, probs = 0.025), quantile(s.in.c.store, probs = 0.025), quantile(s.out.c.store, probs = 0.025), 
                            quantile(s.in.f.store, probs = 0.025), quantile(s.out.f.store, probs = 0.025))), value.name = "CIL")
  plan.CIU <- melt(unname(c(quantile(s.out.store, probs = 0.975), quantile(s.in.store, probs = 0.975), quantile(s.in.c.store, probs = 0.975), quantile(s.out.c.store, probs = 0.975),
                            quantile(s.in.f.store, probs = 0.975), quantile(s.out.f.store, probs = 0.975))), value.name = "CIU")
  plan.temp <- data.frame(Species = rep("Plantago lanceolata", nrow(plan.mean)), UniqueID = rep(as.numeric(plan$matchIn[m]), nrow(plan.mean)), Metric = rowNames, Mean = plan.mean, CIL = plan.CIL, CIU = plan.CIU)
  plan.meta <- rbind(plan.meta, plan.temp)
  print(paste("Network ", m, " is complete"))
}

#
# TRIFOLIUM REPENS ------------------------------------------------------------
#
trif <- outputdf[which(outputdf$spp == "Trifolium_repens"), ]  # subset only Dactylis
trif <- trif[!duplicated(trif$matchIn), -2]  # remove duplicates
trif <- trif[-2, ]
# load respective networks for each case in both MixMono and Cntrl
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/MixMono")
files <- dir(pattern = "*.csv", full.names = TRUE)
matches <- unlist(sapply(trif$matchIn, function(x) grep(pattern = x, files, value = TRUE)))
M.trif <- list()
M.sd.trif <- list()
M.trif[2] <- lapply(matches[1], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.trif[2] <- lapply(matches[2], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.trif[3] <- lapply(matches[3], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.trif[3] <- lapply(matches[4], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl")
files <- dir(pattern = "*.csv", full.names = TRUE)
matches <- unlist(sapply(trif$matchIn, function(x) grep(pattern = x, files, value = TRUE)))
M.trif[1] <- lapply(matches[1], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
M.sd.trif[1] <- lapply(matches[2], function(x) unname(as.matrix(read.csv(x, header = FALSE))))
for (r in 1:length(M.trif)) {
  M.trif[[r]][which(is.na(M.trif[[r]]), arr.ind = TRUE)] <- 0
  M.sd.trif[[r]][which(is.na(M.sd.trif[[r]]), arr.ind = TRUE)] <- 0
}

# empty data frames to store output for metrics
# general metrics separate from distribution fit metrics
trif.meta <- data.frame(Species = character(), UniqueID = integer(), Metric = character(), Mean = numeric(), CIL = numeric(), CIU = numeric(), stringsAsFactors = FALSE)
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis")
for (m in 1:length(M.trif)) {
  # initialize temp vectors to store output for a given network
  s.out.store <- numeric(length = iterations)
  s.in.store <- numeric(length = iterations)
  s.in.c.store <- numeric(length = iterations)
  s.out.c.store <- numeric(length = iterations)
  s.in.f.store <- numeric(length = iterations)
  s.out.f.store <- numeric(length = iterations)
  for (count in 1:iterations) {
    # nested loop for bootstrap
    pos <- as.integer(trif$Position[m])
    # incorporate variability using sd of network elements
    mean.matrix <- t(M.trif[[m]])
    sd.matrix <- t(M.sd.trif[[m]])
    if (any(sd.matrix == 0)) {
      sd.matrix <- sd.matrix + 1E-9
    }
    M <- matrix(mapply(rnorm, 1, mean.matrix, sd.matrix), nrow = nrow(mean.matrix), ncol = ncol(mean.matrix))
    M[which(abs(M) <= 1E-9, arr.ind = TRUE)] <- 0
    # node characteristics (weight, in/out strength)
    # both facilitative and competitive
    s.out <- rowSums(M) / nrow(M)
    s.in <- colSums(M) / nrow(M)
    # Only competitive
    M.c <- ifelse(test = M > 0, yes = 0, no = M)
    s.out.c <- rowSums(M.c, na.rm = TRUE) / nrow(M)
    s.in.c <- colSums(M.c, na.rm = TRUE) / nrow(M)
    # Only facilitative
    M.f <- ifelse(test = M < 0, yes = 0, no = M)
    s.out.f <- rowSums(M.f, na.rm = TRUE) / nrow(M)
    s.in.f <- colSums(M.f, na.rm = TRUE) / nrow(M)
    s.out.store[count] <- s.out[pos]
    s.in.store[count] <- s.in[pos]
    s.in.c.store[count] <- s.in.c[pos]
    s.out.c.store[count] <- s.out.c[pos]
    s.in.f.store[count] <- s.in.f[pos]
    s.out.f.store[count] <- s.out.f[pos]
  }
  # add output from one network to data frame with all networks
  trif.mean <- melt(unname(c(mean(s.out.store), mean(s.in.store), mean(s.in.c.store), mean(s.out.c.store), mean(s.in.f.store), mean(s.out.f.store))), value.name = "Mean")
  trif.CIL <- melt(unname(c(quantile(s.out.store, probs = 0.025), quantile(s.in.store, probs = 0.025), quantile(s.in.c.store, probs = 0.025), quantile(s.out.c.store, probs = 0.025), 
    quantile(s.in.f.store, probs = 0.025), quantile(s.out.f.store, probs = 0.025))), value.name = "CIL")
  trif.CIU <- melt(unname(c(quantile(s.out.store, probs = 0.975), quantile(s.in.store, probs = 0.975), quantile(s.in.c.store, probs = 0.975), quantile(s.out.c.store, probs = 0.975),
    quantile(s.in.f.store, probs = 0.975), quantile(s.out.f.store, probs = 0.975))), value.name = "CIU")
  trif.temp <- data.frame(Species = rep("Trifolium repens", nrow(trif.mean)), UniqueID = rep(as.numeric(trif$matchIn[m]), nrow(trif.mean)), Metric = rowNames, Mean = trif.mean, CIL = trif.CIL, CIU = trif.CIU)
  trif.meta <- rbind(trif.meta, trif.temp)
  print(paste("Network ", m, " is complete"))
}

#
# VISUALIZE RESULTS ------------------------------------------------------------
# 
sppCompare <- rbind(dact.meta, fest.meta, plan.meta, trif.meta)


write.csv(x = sppCompare, file = "sppCompare.csv")




