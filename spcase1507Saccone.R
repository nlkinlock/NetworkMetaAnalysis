#
# IMPUTE ONE MISSING SE FROM OTHER SEs AND SAVE DF
# SPECIAL CASE: 1507_SACCONE_2010
#
# load data, create data frame ------------------------------------------------
#
# load data table (includes biomass and se)
dat <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1507_Saccone_2010.csv")
name <- c("1507_Saccone_2010")

# calculate statistics
#
# calculate sd from se (using number of replicates)
# impute SE
missing <- which(is.na(dat$SE) & !is.na(dat$Metric))
dat$SE[missing] <- sample(dat$SE, size = length(missing), replace = TRUE)
dat[which(is.na(dat$SE) & is.na(dat$Metric)), c("Metric", "SE")] <- 0 
write.csv(dat, "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Biomass/Cntrl/1507_Saccone_2010_imp.csv")


