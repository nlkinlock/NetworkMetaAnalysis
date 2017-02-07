setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/RII")
files <- dir(pattern = "*.csv", full.names = TRUE)

for (x in 1:length(files)) {
  dat <- read.csv(files[x])
  name <- substr(files[x], 3, nchar(files[x]) - 4)
  sp <-  max(unique(dat$Target))
  # calculate sd from se (using number of replicates)
  if (any(colnames(dat) == "SE")) {
    SD <- dat$SE * sqrt(dat$N)
    dat$SD <- SD
  }
  if (any(colnames(dat) == "Missing")) {
    next
  }
  rii <- matrix(dat$Metric, nrow = sp, ncol = sp, byrow = TRUE)
  rii.sd <- matrix(dat$SD, nrow = sp, ncol = sp, byrow = TRUE)
  write.table(x = rii, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl/", name, "-RII.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
  write.table(x = rii.sd, paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl/", name, "-RIIsd.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
}