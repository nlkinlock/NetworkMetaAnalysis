#
# CREATE DATA TABLE WITH PLANT PERFORMANCE METRIC AND VARIATION
# SPECIAL CASE: 838_GOLDBERG_1991
#
# set defaults, load packages ------------------------------------------------
#
library(plyr)
library(stringr)
#
# load data, create data frame ------------------------------------------------
#
name <- "838_Goldberg_1991"
dat <- read.csv(paste(path, "Input/BiomassData/", name, ".csv", sep = ""))
# unique Target/Neighbor combination
dat$UniqueID <- paste(dat$Target, dat$Neighbor, sep = "")
#
# calculate mean and variation in performance ------------------------------------------------
#
# subset of only cases where Neighbor density was 0 (control) and where Neighbor density was greater than 0 individuals (used to get a reasonable value of SD)
dat <- rbind(dat[which(dat$NeighborMetric > 50), ], dat[which(dat$NeighborMetric == 0), ])
# find mean target Biomass among Target/Neighbor combinations for calculating RII
dat <- ddply(dat, .(UniqueID), summarise, SD = sd(Metric, na.rm = TRUE), Metric = mean(Metric, na.rm = TRUE), N = length(UniqueID))
# impute SDs
replace <- dat[which(is.na(dat$SD)), ]
nonmissing <- dat[which(!is.na(dat$SD)), ]
dat[which(is.na(dat$SD)), "SD"] <- sample(nonmissing$SD, size = nrow(replace), replace = TRUE)
dat$SD <- dat$SD + 0.000001 # no zero SDs. Zero SDs are likely an artifact of using ImageJ
# add target and neighbor IDs 
tn <- str_split_fixed(dat$UniqueID, "", 2)
dat$Target <- as.numeric(tn[, 1])
dat$Neighbor <- as.numeric(tn[, 2])
#
# save data table ------------------------------------------------
#
write.table(x = dat, file = paste(path, "Input/CaseData/", name, ".csv", sep = ""), sep = ",", row.names = FALSE)

