#
# CREATE DATA TABLE WITH PLANT PERFORMANCE METRIC AND VARIATION
# SPECIAL CASE: 1787_WEIGELT_2002
#
library(plyr)
#
# load data, extract variables, initialize ------------------------------------------------
#
tab.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/1787_Weigelt_2002.csv")
name <- c("1787_Weigelt_2002_LowWater", "1787_Weigelt_2002_HighWater")

tab.init <- tab.init[which(tab.init$Competition == "r/s"), -c(2, 3, 4, 5, 8, 9, 10)]

for (s in 1:length(name)) {
  if (s == 1) {
tab <- tab.init[which(tab.init$Chamber == 1 | tab.init$Chamber == 2), -1]  # low water
  } else {
tab <- tab.init[which(tab.init$Chamber == 3 | tab.init$Chamber == 4), -1]  # high water
  }
# unique Target/Neighbor combination
tab$UniqueID <- paste(tab$Target, tab$Border, sep = "")

# find mean target Biomass among Target/Neighbor combinations for calculating RII and RY
dat <- ddply(tab, .(UniqueID), summarise, SD = sd(Metric, na.rm = TRUE), Metric = mean(Metric, na.rm = TRUE), N = length(UniqueID))

# add target and neighbor IDs 
tn <- str_split_fixed(dat$UniqueID, "", 2)
dat$Target <- as.numeric(tn[, 1])
dat$Neighbor <- as.numeric(tn[, 2])

write.table(x = dat, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/", name[s], ".csv", sep = ""), sep = ",", row.names = FALSE)
}



