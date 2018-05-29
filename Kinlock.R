#
# CREATE DATA TABLE WITH PLANT PERFORMANCE METRIC AND VARIATION
# SPECIAL CASE: 9998_Kinlock_unpubl (adult-seedling) and 9999_Kinlock_unpubl (seedling-seedling)
#
# set defaults, load packages ------------------------------------------------
#

library(reshape2)
library(plyr)
library(dplyr)

#
# load data, create data frame ------------------------------------------------
#

biomass <- read.csv("/Users/nicolekinlock/Documents/Plant Ecology/PlantInteractionFieldStudy/FinalSeedlingMeasurements.csv")
biomass <- filter(biomass, X == "")  # remove cells with comments. these were cases where an individual was dead/dying.
biomass <- biomass[, -c(1, 9)]  # remove oven and comment columns
str(biomass) 

# separate seedling-seedling and adult seedling experiments
#
# seedling-seedling
ss <- filter(biomass, Experiment == "SS", Individual == "T")  # only studying targets
ss <- filter(ss, Neighbor != "BT/RT" | is.na(Neighbor))  # remove 3 spp treatment
ss <- ss[, -c(1, 5)]  # remove experiment and individual (target/neighbor) columns
ss <- filter(ss, !(ss$Target == "CR" & ss$Neighbor == "CR" & ss$Replicate == 1) | is.na(Neighbor))  # two entries for the same replicate, removing for now
# adult-seedling
as <- filter(biomass, Experiment == "AS")  # only studying targets
as <- as[, -c(1, 5)]  # remove experiment and individual (target/neighbor) columns

#
# calculate mean and variation in performance ------------------------------------------------
#
# seedling-seedling
ss$Target <- as.numeric(factor(ss$Target))
ss$Neighbor <- as.numeric(factor(ss$Neighbor))
ss$Neighbor[is.na(ss$Neighbor)] <- 0
# unique Target/Neighbor combination
ss$UniqueID <- paste(ss$Target, ss$Neighbor, sep = "")
# find mean target Biomass among Target/Neighbor combinations for calculating RII
dat.ss <- ddply(ss, .(UniqueID), summarise, SD = sd(AbovegroundBiomass, na.rm = TRUE), Metric = mean(AbovegroundBiomass, na.rm = TRUE), N = length(UniqueID))
# add target and neighbor IDs 
tn <- str_split_fixed(dat.ss$UniqueID, "", 2)
dat.ss$Target <- as.numeric(tn[, 1])
dat.ss$Neighbor <- as.numeric(tn[, 2])
# impute SDs
replace <- dat.ss[which(is.na(dat.ss$SD)), ]
nonmissing <- dat.ss[which(!is.na(dat.ss$SD)), ]
dat.ss[which(is.na(dat.ss$SD)), "SD"] <- sample(nonmissing$SD, size = nrow(replace), replace = TRUE)

# adult-seedling
as$Target <- as.numeric(factor(as$Target, levels = c("BT", "CR", "EU", "PS", "RM", "RO", "RT")))
as$Neighbor <- as.numeric(factor(as$Neighbor, levels = c("BT", "CR", "EU", "PS", "RM", "RO", "RT")))
as$Neighbor[is.na(as$Neighbor)] <- 0
as$UniqueID <- paste(as$Target, as$Neighbor, sep = "")
dat.as <- ddply(as, .(UniqueID), summarise, SD = sd(AbovegroundBiomass, na.rm = TRUE), Metric = mean(AbovegroundBiomass, na.rm = TRUE), N = length(UniqueID))

tn <- str_split_fixed(dat.as$UniqueID, "", 2)
dat.as$Target <- as.numeric(tn[, 1])
dat.as$Neighbor <- as.numeric(tn[, 2])

replace <- dat.as[which(is.na(dat.as$SD)), ]
nonmissing <- dat.as[which(!is.na(dat.as$SD)), ]
dat.as[which(is.na(dat.as$SD)), "SD"] <- sample(nonmissing$SD, size = nrow(replace), replace = TRUE)
dat.as <- rbind(dat.as, c(33, 0, 0, 0, 3, 3))

#
# save data tables ------------------------------------------------
#

write.table(x = dat.as, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/9998_Kinlock_unpubl.csv", sep = ",", row.names = FALSE)
write.table(x = dat.ss, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/9999_Kinlock_unpubl.csv", sep = ",", row.names = FALSE)





