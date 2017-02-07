
#
# load data, extract variables, initialize ------------------------------------------------
#
# number of bootstrap iterations
R <- 100000

# load data table (includes biomass and se)
dat <- read.csv("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/106-Costa-2003.csv")
name <- c("106-Costa-2003")
# number of spp
sp <- max(unique(dat$Target))
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
two.one <- c()
three.one <- c()
one.two <- c()
two.two <- c()
three.two <- c()
one.three <- c()
two.three <- c()
three.three <- c()
for (i in 1:R) {
  temp <- rtruncnorm(n = nrow(dat), mean = dat$Metric, sd = dat$SD, a = 0)
  one.one[i] <- (temp[11] - temp[8]) / (temp[11] + temp[8])
  two.one[i] <- (temp[10] - temp[7]) / (temp[10] + temp[7])
  three.one[i] <- (temp[12] - temp[9]) / (temp[12] + temp[9])
  one.two[i] <- (temp[5] - temp[1]) / (temp[5] + temp[1])
  two.two[i] <- (temp[4] - temp[2]) / (temp[4] + temp[2])
  three.two[i] <- (temp[6] - temp[3]) / (temp[6] + temp[3])
  one.three[i] <- (temp[17] - temp[14]) / (temp[17] + temp[14])
  two.three[i] <- (temp[16] - temp[13]) / (temp[16] + temp[13])
  three.three[i] <- (temp[18] - temp[15]) / (temp[18] + temp[15])
}

boot.rii <- as.matrix(data.frame(one.one, one.two, one.three, two.one, two.two, two.three, three.one, three.two, three.three))

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
two.one <- c()
three.one <- c()
one.two <- c()
two.two <- c()
three.two <- c()
one.three <- c()
two.three <- c()
three.three <- c()
for (i in 1:R) {
  temp <- rtruncnorm(n = nrow(dat), mean = dat$Metric, sd = dat$SD, a = 0)
  one.one[i] <- temp[11] / temp[8]
  two.one[i] <- temp[10] / temp[7]
  three.one[i] <- temp[12] / temp[9]
  one.two[i] <- temp[5] / temp[1]
  two.two[i] <- temp[4] / temp[2]
  three.two[i] <- temp[6] / temp[3]
  one.three[i] <- temp[17] / temp[14]
  two.three[i] <- temp[16] / temp[13]
  three.three[i] <- temp[18] / temp[15]
}

boot.ry <- as.matrix(data.frame(one.one, one.two, one.three, two.one, two.two, two.three, three.one, three.two, three.three))

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
