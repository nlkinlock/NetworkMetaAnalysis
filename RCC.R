
#
# load data, extract variables, initialize ------------------------------------------------
#
# number of bootstrap iterations
R <- 100000

# load all data tables with RCC
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/RCC")
files <- dir(pattern = "*.csv", full.names = TRUE)

for (x in 1:length(files)) {
  dat <- read.csv(files[x])
  name <- substr(files[x], 3, nchar(files[x]) - 4)
  # number of spp
  sp <- max(unique(dat$Target))
  # unique Target/Neighbor combination
  dat$UniqueID <- paste(dat$Target, dat$Neighbor, sep = "")
  
#
# RCC ------------------------------------------------
#
# load data, extract variables, initialize
#
# load data table
dat.RCC <- dat
  # unique Target/Neighbor combination
  if (any(colnames(dat.RCC) == "NeighborRCC")) {
  dat.RCC$RCCID <- paste(dat.RCC$Target, dat.RCC$NeighborRCC, sep = "")
  list.RCC <- dlply(dat.RCC, .(RCCID), function(df) lm(Metric ~ NeighborMetric, data = df))
  } else{
    # linear model for each Target/Neighbor combination
    list.RCC <- dlply(dat.RCC, .(UniqueID), function(df) lm(Metric ~ NeighborMetric, data = df))
  }

# extract slopes
rcc <- unlist(lapply(list.RCC, function(x) coefficients(x)[2]))
format(rcc, scientific = FALSE)
# extract standard error of the slope
list.sum <- lapply(list.RCC, summary)
rcc.sd <- unlist(lapply(list.sum, function(x) coefficients(x)[2, 2]))
format(rcc.sd, scientific = FALSE)
# save output in matrices (mean and se)
rcc <- matrix(rcc, nrow = sp, ncol = sp, byrow = TRUE)
rcc.sd <- matrix(rcc.sd, nrow = sp, ncol = sp, byrow = TRUE)
write.table(x = rcc, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RCC/", name, "-RCC.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
write.table(x = rcc.sd, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RCC/", name, "-RCCsd.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)

#
# RII and RY ------------------------------------------------
#
#
if (name == "838-Goldberg-1991") {
# subset of only cases where Neighbor density was 0 (control) and where Neighbor density was between 50 and 150 individuals (used to get some value of SD)
dat <- rbind(dat[which(dat$NeighborMetric < 150 & dat$NeighborMetric > 50), ], dat[which(dat$NeighborMetric == 0), ])
}

# find mean target Biomass among Target/Neighbor combinations for calculating RII and RY
dat <- ddply(dat, .(UniqueID), summarise, SD = sd(Metric, na.rm = TRUE), Metric = mean(Metric, na.rm = TRUE), N = length(UniqueID))

# impute SDs
replace <- dat[which(is.na(dat$SD)), ]
nonmissing <- dat[which(!is.na(dat$SD)), ]
dat[which(is.na(dat$SD)), "SD"] <- sample(nonmissing$SD, size = nrow(replace), replace = TRUE)
dat$SD <- dat$SD + 0.000001 # no nonzero SDs. Zero SDs are likely an artifact of using ImageJ

# add target and neighbor IDs 
tn <- str_split_fixed(dat$UniqueID, "", 2)
dat$Target <- as.numeric(tn[, 1])
dat$Neighbor <- as.numeric(tn[, 2])

#
# RII ------------------------------------------------
#
# calculate RII using bootstrap, biomass mean and sd
# assume normally distributed biomass, sample using mean and sd from paper
# generate distribution of possible RIIs and calculate mean and sd of this distribution (SE)
if (any(dat$Neighbor == 0)) {
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
  } else {
  boot.rii <- matrix(NA, nrow = R, ncol = sp^2)
  for (r in 1:R) {
    temp <- rtruncnorm(n = nrow(dat), mean = dat$Metric, sd = dat$SD, a = 0)
    for (i in 1:sp) {
      mono <- which(dat$Target == i & dat$Neighbor == i)
      for (j in 1:sp) {
        mix <- which(dat$Target == i & dat$Neighbor == j)
        boot.rii[r, mix] <- (temp[mix] - temp[mono]) / (temp[mix] + temp[mono])
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
  write.table(x = rii, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/MixMono/", name, "-RII.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
  write.table(x = rii.sd, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/MixMono/", name, "-RIIsd.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
}

#
# RY ------------------------------------------------
#
# calculate RY, biomass mean and sd
if (any(dat$Neighbor == 0)) {
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
}








