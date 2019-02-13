# MODEL ADDITIVITY: STANDARDIZED DIFFERENCE B/W PAIRWISE AND 3 SPP. INTERACTIONS
#

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD DATA -------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# load raw data from 3-spp experiments and the pairwise experiments that accompany them
setwd("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/MultipleSpp/")
files <- dir(pattern = "*.csv", full.names = TRUE)  # 3-spp files
b.all <- lapply(files, function(x) read.csv(x))
last.char <- substr(files, start = nchar(files) - 4, stop = nchar(files) - 4)
name <- substr(files[last.char != "p"], start = 3, stop = nchar(files[last.char != "p"]) - 4)
pairwise <- b.all[last.char != "p"]
multispp <- b.all[last.char == "p"]
# networks with mean RII values from each network to standardize values
setwd("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Networks/")
filesnet <- dir(pattern = "*.csv")
rii.all <- lapply(filesnet, function(x) read.csv(x, header = FALSE))
filesnet <- substr(filesnet, start = 1, stop = nchar(filesnet) - 4)
rii <- rii.all[which(filesnet %in% name)]

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# CALCULATE DIFFERENCES AND FIT MODEL -----------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# calculate the difference in RIIs
#
# statistic of interest is the difference in RIIs calculated directly from the multiple (3) spp experiments compared with
# RII of the two pairwise experiments added together (scaled to the difference in the number of individuals - * 0.5)
additive <- data.frame(Network = character(), Target = character(), Neighbor.j = character(), Neighbor.k = character(), Difference = character(), stringsAsFactors = FALSE)
for (x in 1:length(name)) {
# number of spp
  species <- max(unique(multispp[[x]]$Target))
  for (i in 1:species) {
    mono <- which(pairwise[[x]]$Target == i & pairwise[[x]]$Neighbor == i)
    for (j in 1:species) {
      if (j == i) {
        next
      }
      for (k in 1:species) {
        if (k == i | k == j | k < j | length(which(multispp[[x]]$Target == i & multispp[[x]]$Neighbor == paste(j, k, sep = ""))) == 0) {  # filter loop to only find values for which a 3-spp mixture was measured
          next
        }
        # multispecies mixture of target i with neighbors j and k 
        mix.multi <- which(multispp[[x]]$Target == i & multispp[[x]]$Neighbor == paste(j, k, sep = ""))
        rii.multi <- (multispp[[x]]$Metric[mix.multi] - pairwise[[x]]$Metric[mono]) / (multispp[[x]]$Metric[mix.multi] + pairwise[[x]]$Metric[mono])
        # pairwise mixture of target i with neighbor j
        mix.j <- which(pairwise[[x]]$Target == i & pairwise[[x]]$Neighbor == j)
        # pairwise mixture of target i with neighbor k
        mix.k <- which(pairwise[[x]]$Target == i & pairwise[[x]]$Neighbor == k)
        # scaled for difference in neighbor density
        rii.add <- 0.5 * ((pairwise[[x]]$Metric[mix.j] - pairwise[[x]]$Metric[mono]) / (pairwise[[x]]$Metric[mix.j] + pairwise[[x]]$Metric[mono])) +
          0.5 * ((pairwise[[x]]$Metric[mix.k] - pairwise[[x]]$Metric[mono]) / (pairwise[[x]]$Metric[mix.k] + pairwise[[x]]$Metric[mono]))
        stdz.diff <- (rii.add - rii.multi) / sd(unlist(rii[[x]]), na.rm = TRUE)
        difference <- data.frame(Network = name[x], Target = i, Neighbor.j = j, Neighbor.k = k, Difference = stdz.diff, stringsAsFactors = FALSE)
        additive <- rbind(additive, difference)
      }
    }
  }
}
# store raw output
write.csv(additive, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/AdditivityRawOutput.csv")


# fit hierarchical model

# create categorical variable for network identity
additive$Group <- as.numeric(factor(additive$Network, levels = unique(additive$Network)))

add.fit <- FitAdditiveJAGSModel(y.input = additive$Difference, g.input = additive$Group)
MCMCtrace(add.fit, open_pdf = FALSE, excl = "deviance", pdf = TRUE, filename = "additivity", wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence")


add.output <- ExtractAdditiveJAGSModel(add.fit)
# store output from model
write.csv(add.output, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/AdditivityFit.csv")



