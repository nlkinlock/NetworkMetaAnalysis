
#
# load data, extract variables, initialize ------------------------------------------------
#
# number of bootstrap iterations
R <- 100000

# load all data tables with MixMono
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Biomass/Cntrl")
files <- dir(pattern = "*.csv", full.names = TRUE)

for (x in 1:length(files)) {
  dat <- read.csv(files[x])
  name <- substr(files[x], 3, nchar(files[x]) - 4)
# number of spp
sp <- max(unique(dat$Target))
# calculate sd from se (using number of replicates)
if (any(colnames(dat) == "SE")) {
  SD <- dat$SE * sqrt(dat$N)
  dat$SD <- SD
}

#
# RII ------------------------------------------------
#
# calculate RII using bootstrap, biomass mean and sd
# assume normally distributed biomass (truncated normal - biomass is nonzero), sample using mean and sd from paper
# generate distribution of possible RIIs and calculate mean and sd of this distribution (SE)
boot.rii <- matrix(NA, nrow = R, ncol = sp^2)
for (r in 1:R) {
  temp <- rtruncnorm(n = nrow(dat), mean = dat$Metric, sd = dat$SD, a = 0)
  for (i in 1:sp) {
    cntrl <- which(dat$Target == i & dat$Neighbor == 0)
    for (j in 1:sp) {
      mix <- which(dat$Target == i & dat$Neighbor == j)
      boot.rii[r, mix - i] <- (temp[mix] - temp[cntrl]) / (temp[mix] + temp[cntrl])
    }
  }
}
rii.vec <- c()
rii.sd.vec <- c()
rii.fit <- apply(boot.rii, 2, function(x) fitdist(data = x, distr = "norm", method = "mme"))
for (w in 1:length(rii.fit)) {
  rii.vec[w] <- unname(rii.fit[[w]][[1]][1])
  rii.sd.vec[w] <- unname(rii.fit[[w]][[1]][2])
}
rii <- matrix(rii.vec, nrow = sp, ncol = sp, byrow = TRUE)
rii.sd <- matrix(rii.sd.vec, nrow = sp, ncol = sp, byrow = TRUE)
# save matrices of mean RII and sd RII
write.table(x = rii, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl/", name, "-RII.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
write.table(x = rii.sd, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl/", name, "-RIIsd.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)

#
# RY ------------------------------------------------
#
# calculate RY, biomass mean and sd
boot.ry <- matrix(NA, nrow = R, ncol = sp^2)
for (r in 1:R) {
  temp <- rtruncnorm(n = nrow(dat), mean = dat$Metric, sd = dat$SD, a = 0)
  for (i in 1:sp) {
    cntrl <- which(dat$Target == i & dat$Neighbor == 0)
    for (j in 1:sp) {
      mix <- which(dat$Target == i & dat$Neighbor == j)
      boot.ry[r, mix - i] <- temp[mix] / temp[cntrl]
    }
  }
}

ry.vec <- c()
ry.sd.vec <- c()
ry.fit <- apply(boot.ry, 2, function(x) fitdist(data = x, distr = "norm", method = "mme"))
for (w in 1:length(ry.fit)) {
  ry.vec[w] <- unname(ry.fit[[w]][[1]][1])
  ry.sd.vec[w] <- unname(ry.fit[[w]][[1]][2])
}
ry <- matrix(ry.vec, nrow = sp, ncol = sp, byrow = TRUE)
ry.sd <- matrix(ry.sd.vec, nrow = sp, ncol = sp, byrow = TRUE)

write.table(x = ry, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RY/Cntrl/", name, "-RY.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
write.table(x = ry.sd, paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RY/Cntrl/", name, "-RYsd.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
}






