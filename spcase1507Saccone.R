
#
# load data, extract variables, initialize ------------------------------------------------
#
# number of bootstrap iterations
R <- 100000

# load data table (includes biomass and se)
dat <- read.csv("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/1507-Saccone-2010.csv")
name <- c("1507-Saccone-2010")
# number of spp
sp <- max(unique(dat$Target))
# impute SE
missing <- which(is.na(dat$SE))
dat$SE[missing] <- sample(dat$SE, size = length(missing), replace = TRUE)
# calculate sd from se (using number of replicates)
SD <- dat$SE * sqrt(dat$N)
dat$SD <- SD


#
# RII ------------------------------------------------
#
# calculate RII using bootstrap, biomass mean and sd
# assume normally distributed biomass, sample using mean and sd from paper
# generate distribution of possible RIIs and calculate mean and sd of this distribution (SE)
one.one <- c()
one.two <- c()
one.three <- 0
one.four <- c()
two.one <- c()
two.two <- c()
two.three <- 0
two.four <- c()
three.one <- c()
three.two <- c()
three.three <- 0
three.four <- 0
four.one <- c()
four.two <- c()
four.three <- 0
four.four <- c()
for (i in 1:R) {
  temp <- rtruncnorm(n = nrow(dat), mean = dat$Metric, sd = dat$SD, a = 0)
  one.one[i] <- (temp[1] - temp[2]) / (temp[1] + temp[2])
  one.two[i] <- (temp[3] - temp[4]) / (temp[3] + temp[4])
  one.four[i] <- (temp[5] - temp[6]) / (temp[5] + temp[6])
  two.one[i] <- (temp[7] - temp[8]) / (temp[7] + temp[8])
  two.two[i] <- (temp[9] - temp[10]) / (temp[9] + temp[10])
  two.four[i] <- (temp[11] - temp[12]) / (temp[11] + temp[12])
  three.one[i] <- (temp[13] - temp[14]) / (temp[13] + temp[14])
  three.two[i] <- (temp[15] - temp[16]) / (temp[15] + temp[16])
  four.one[i] <- (temp[19] - temp[20]) / (temp[19] + temp[20])
  four.two[i] <- (temp[21] - temp[22]) / (temp[21] + temp[22])
  four.four[i] <- (temp[23] - temp[24]) / (temp[23] + temp[24])
}

boot.rii <- as.matrix(data.frame(one.one, one.two, one.three, one.four, two.one, two.two, two.three, two.four, three.one, three.two, three.three, three.four, four.one, four.two, four.three, four.four))

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
# calculate RY, yield mean and sd
one.one <- c()
one.two <- c()
one.four <- c()
two.one <- c()
two.two <- c()
two.four <- c()
three.one <- c()
three.two <- c()
four.one <- c()
four.two <- c()
four.four <- c()
for (i in 1:R) {
  temp <- rtruncnorm(n = nrow(dat), mean = dat$Metric, sd = dat$SD, a = 0)
  one.one[i] <- temp[1] / temp[2]
  one.two[i] <- temp[3] / temp[4]
  one.four[i] <- temp[5] / temp[6]
  two.one[i] <- temp[7] / temp[8]
  two.two[i] <- temp[9] / temp[10]
  two.four[i] <- temp[11] / temp[12]
  three.one[i] <- temp[13] / temp[14]
  three.two[i] <- temp[15] / temp[16]
  four.one[i] <- temp[19] / temp[20]
  four.two[i] <- temp[21] / temp[22]
  four.four[i] <- temp[23] / temp[24]
}

boot.ry <- as.matrix(data.frame(one.one, one.two, one.three, one.four, two.one, two.two, two.three, two.four, three.one, three.two, three.three, three.four, four.one, four.two, four.three, four.four))

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
