#
# CREATE DATA TABLE WITH PLANT PERFORMANCE METRIC AND VARIATION
# SPECIAL CASE: 492_Miller_1987
#
# set defaults, load packages ------------------------------------------------
#
par.defaults <- par(no.readonly = TRUE)
save(par.defaults, file="R.default.par.RData")
load("R.default.par.RData")

library(R2jags)
library(runjags)
library(plyr)
library(stringr)

#
# load data, create data frame ------------------------------------------------
#

dat <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/492_Miller_1987.csv")
name <- "492_Miller_1987"

# unique Target/Neighbor combination
dat$UniqueID <- paste(dat$Target, dat$Neighbor, sep = "")
# find mean target Biomass among Target/Neighbor combinations for calculating RII and RY
dat <- ddply(dat, .(UniqueID), summarise, SD = sd(Metric, na.rm = TRUE), Metric = mean(Metric, na.rm = TRUE), N = length(UniqueID))

# add target and neighbor IDs 
tn <- str_split_fixed(dat$UniqueID, "", 2)
dat$Target <- as.numeric(tn[, 1])
dat$Neighbor <- as.numeric(tn[, 2])

write.table(x = dat, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/MixMono/", name, ".csv", sep = ""), sep = ",", row.names = FALSE)



