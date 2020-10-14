#
# CREATE DATA TABLE WITH PLANT PERFORMANCE METRIC AND VARIATION
# SPECIAL CASE: 492_Miller_1987
#
# set defaults, load packages ------------------------------------------------
#
library(plyr)
library(stringr)
#
# load data, create data frame ------------------------------------------------
#
name <- "492_Miller_1987"
dat <- read.csv(paste(path, "Input/BiomassData/", name, ".csv", sep = ""))
#
# unique Target/Neighbor combination
dat$UniqueID <- paste(dat$Target, dat$Neighbor, sep = "")
# find mean target Biomass among Target/Neighbor combinations for calculating RII and RY
dat <- ddply(dat, .(UniqueID), summarise, SD = sd(Metric, na.rm = TRUE), Metric = mean(Metric, na.rm = TRUE), N = length(UniqueID))
# add target and neighbor IDs 
tn <- str_split_fixed(dat$UniqueID, "", 2)
dat$Target <- as.numeric(tn[, 1])
dat$Neighbor <- as.numeric(tn[, 2])
write.table(x = dat, file = paste(path, "Input/CaseData/", name, ".csv", sep = ""), sep = ",", row.names = FALSE)

