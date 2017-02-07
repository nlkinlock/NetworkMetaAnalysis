setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/RY")
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
  if (name == "1110-Roxburgh-2000" | name == "1354-Johansson-1991-imp") {
    ry <- matrix(dat$Metric, nrow = sp, ncol = sp, byrow = TRUE)
    ry.se <- matrix(dat$SD, nrow = sp, ncol = sp, byrow = TRUE)
    write.table(x = ry, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RY/Cntrl/", name, "-RY.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
    write.table(x = ry.sd, paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RY/Cntrl/", name, "-RYsd.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
  } else {
    ry <- matrix(dat$Metric, nrow = sp, ncol = sp, byrow = TRUE)
    ry.se <- matrix(dat$SD, nrow = sp, ncol = sp, byrow = TRUE)
    write.table(x = ry, file = paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RY/MixMono/", name, "-RY.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
    write.table(x = ry.sd, paste("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RY/MixMono/", name, "-RYsd.csv", sep = ""), sep = ",", row.names = FALSE, col.names = FALSE)
  }
}