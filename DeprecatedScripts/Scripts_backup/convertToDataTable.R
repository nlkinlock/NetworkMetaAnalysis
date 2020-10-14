# CONVERTING OUTPUT TO USABLE FORM FOR TABLES
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# META-ANALYSIS TABLE ---------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# write CSV for complete meta analysis output table in appendices
#
GenerateMATables <- function(ma.table, ma.table.true.ctrl) {
  ma.table <- ma.table[which(ma.table$Metric != "Asymmetry"), ]
  ma.table$CompetitionRelativeTo <- "Monoculture"
  ma.table <- ma.table[, c(12, 2, 5, 6, 7, 8, 9, 3, 10, 11)]
  colnames(ma.table) <- c("CompetitionRelativeTo", "Comparison", "Metric", "Parameter", "Estimate", "CRILowerBound", 
                          "CRIUpperBound", "n", "rhat", "Effectiven")
  ma.table <- ma.table[which(ma.table$Parameter != "Heterogeneity"), ]
  ma.table.true.ctrl <- ma.table.true.ctrl[which(ma.table.true.ctrl$Metric != "Asymmetry"), ]
  ma.table.true.ctrl$CompetitionRelativeTo <- "True control"
  ma.table.true.ctrl <- ma.table.true.ctrl[, c(12, 2, 5, 6, 7, 8, 9, 3, 10, 11)]
  colnames(ma.table.true.ctrl) <- c("CompetitionRelativeTo", "Comparison", "Metric", "Parameter", "Estimate", "CRILowerBound",
                                      "CRIUpperBound", "n", "rhat", "Effectiven")
  ma.table.true.ctrl <- ma.table.true.ctrl[which(ma.table.true.ctrl$Parameter != "Heterogeneity"), ]
  ma.table <- rbind(ma.table, ma.table.true.ctrl)
  ma.table$Parameter[which(ma.table$Parameter == "OverallSD")] <- "Std deviation"
  ma.table$Parameter[which(ma.table$Parameter == "OverallMean")] <- "Grand mean"
  ma.table$Comparison[which(ma.table$Comparison == "Setting")] <- "Experiment setting"
  ma.table$Comparison[which(ma.table$Comparison == "Habit")] <- "Growth habit"
  ma.table$Comparison[which(ma.table$Comparison == "Age")] <- "Plant age"
  ma.table$Metric[which(ma.table$Metric == "MeanStrength")] <- "Mean strength"
  ma.table$Metric[which(ma.table$Metric == "IndirectEffect")] <- "Indirect effect"
  ma.table$Metric[which(ma.table$Metric == "WeightedConnectance")] <- "Weighted connectance"
  ma.table$Metric[which(ma.table$Metric == "WeightedCompConnectance")] <- "W comp connectance"
  ma.table$Metric[which(ma.table$Metric == "WeightedFacConnectance")] <- "W fac connectance"
  ma.table$Metric[which(ma.table$Metric == "RelativeIntransitivity")] <- "Relative intransitivity"
  ma.table$Metric <- factor(ma.table$Metric, 
                                levels = c("Mean strength", "Indirect effect", "Imbalance", "Relative intransitivity", 
                                            "Weighted connectance", "W comp connectance", "W fac connectance"))
  #
  # create short table with grand means and means [95% CRIs]
  ma.table.short <- ma.table
  ma.table.short <- subset(ma.table.short, Comparison == "Grand mean" & Parameter == "Grand mean")
  ma.table.short$Estimate <- round(ma.table.short$Estimate, digits = 2)
  ma.table.short$CRILowerBound <- round(ma.table.short$CRILowerBound, digits = 2)
  ma.table.short$CRIUpperBound <- round(ma.table.short$CRIUpperBound, digits = 2)
  ma.table.short$Mean95CRI <- paste(ma.table.short$Estimate, " [", ma.table.short$CRILowerBound, ", ", ma.table.short$CRIUpperBound, "]", sep = "")
  ma.table.short <- ma.table.short[order(ma.table.short$Metric), ]
  table.to.compare <- ma.table.short[, c(3, 1, 5, 6, 7)]
  ma.table.short <- ma.table.short[, c(3, 1, 11)]
  #
  # edit long table for supplement
  ma.table$Estimate <- round(ma.table$Estimate, digits = 3)
  ma.table$CRILowerBound <- round(ma.table$CRILowerBound, digits = 3)
  ma.table$CRIUpperBound <- round(ma.table$CRIUpperBound, digits = 3)
  ma.table$rhat <- round(ma.table$rhat, digits = 3)
  ma.table$Effectiven <- round(ma.table$Effectiven, digits = 3)
  ma.table$Comparison <- factor(ma.table$Comparison, 
                                levels = c("Grand mean", "Habitat", "Experiment setting", "Growth habit", "Plant age"))
  ma.table <- ma.table[with(ma.table, order(CompetitionRelativeTo, Comparison)), ]
  row.names(ma.table) <- 1:nrow(ma.table)
  return(list(ma.table.short, table.to.compare, ma.table))
}
#
# read in monoculture control
ma.table <- read.csv(paste(path, "Output/CompleteMetaAnalysisOutput_MonoCtrl.csv", sep =""), stringsAsFactors = FALSE)
# read in true control
ma.table.true.ctrl <- read.csv(paste(path, "Output/CompleteMetaAnalysisOutput_TrueCtrlOnly.csv", sep = ""), stringsAsFactors = FALSE)
ma.table.output <- GenerateMATables(ma.table = ma.table, ma.table.true.ctrl = ma.table.true.ctrl)
#
# write tables to file
write.csv(x = ma.table.output[[1]], file = paste(path, "Output/MetaAnalysisTable_Short_Correction.csv", sep = ""), row.names = FALSE)
write.csv(x = ma.table.output[[3]], file = paste(path, "Output/MetaAnalysisTable_Long_Correction.csv", sep = ""), row.names = FALSE)
#
# compare old and corrected versions
ma.old <- read.csv(paste(path, "OldVersions/Oct2020/Output/CompleteMetaAnalysisOutput_MonoCtrl.csv", sep =""), stringsAsFactors = FALSE)
ma.true.ctrl.old <- read.csv(paste(path, "OldVersions/Oct2020/Output/CompleteMetaAnalysisOutput_TrueCtrlOnly.csv", sep = ""), stringsAsFactors = FALSE)
ma.old.output <- GenerateMATables(ma.table = ma.old, ma.table.true.ctrl = ma.true.ctrl.old)
compare.tables <- ma.table.output[[2]]
compare.tables <- cbind(compare.tables, ma.old.output[[2]][, c(3:5)])
compare.tables$CheckEst <- ma.table.output[[2]]$Estimate == ma.old.output[[2]]$Estimate
compare.tables$CheckCIL <- ma.table.output[[2]]$CRILowerBound == ma.old.output[[2]]$CRILowerBound
compare.tables$CheckCIU <- ma.table.output[[2]]$CRIUpperBound == ma.old.output[[2]]$CRIUpperBound
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MODEL SELECTION TABLES  -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# write CSV for model selection table in appendices
# read in monoculture control
model.selection.df <- read.csv(paste(path, "Output/ModelSelectionSubset_MonoCtrl.csv", sep = ""))
levels(model.selection.df$Case) <- c("Armas & Pugnaire 2011", "Svenning et al. 2008", "Niu & Wan 2008", "Mariotte et al. 2012", "Jiang et al. 2014",
                                     "Miller & Werner 1987", "Engel & Weltzin 2008", "Goldberg & Landa 1991", "Kinlock unpublished")
# read in true control
model.selection.true.ctrl.df <- read.csv(paste(path, "Output/ModelSelectionSubset_TrueCtrlOnly.csv", sep = ""))
levels(model.selection.true.ctrl.df$Case) <- c("Armas & Pugnaire 2011", "Svenning et al. 2008", "Löf et al. 2014", "Goldberg & Landa 1991",
                                                "Kinlock unpublished (b)", "Kinlock unpublished")
model.selection.df <- rbind(model.selection.df, model.selection.true.ctrl.df)
model.selection.df$CompetitionRelativeTo <- factor(model.selection.df$CompetitionRelativeTo, levels = c("Monoculture", "True control"))
model.selection.df$Distribution <- factor(model.selection.df$Distribution, 
                                                    levels = c("Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.df$Case <- factor(model.selection.df$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012", 
                                                    "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008", "Armas & Pugnaire 2011", 
                                                    "Löf et al. 2014", "Kinlock unpublished",  "Kinlock unpublished (b)"))
model.selection.df <- model.selection.df[, -c(6, 10, 13, 14)]
colnames(model.selection.df) <- c("Network", "Metric", "Distribution", "Parameter1", "Parameter2", "PPPmedian", "PPPmean",
                                  "PPPupper", "pWAIC", "WAIC", "SEWAIC", "Fit", "CompetitionRelativeTo")
model.selection.df$Parameter1 <- round(model.selection.df$Parameter1, digits = 2)
model.selection.df$Parameter2 <- round(model.selection.df$Parameter2, digits = 2)
model.selection.df$PPPmedian <- round(model.selection.df$PPPmedian, digits = 3)
model.selection.df$PPPmean <- round(model.selection.df$PPPmean, digits = 3)
model.selection.df$PPPupper <- round(model.selection.df$PPPupper, digits = 3)
model.selection.df$pWAIC <- round(model.selection.df$pWAIC, digits = 2)
model.selection.df$WAIC <- round(model.selection.df$WAIC, digits = 2)
model.selection.df$SEWAIC <- round(model.selection.df$SEWAIC, digits = 2)
model.selection.df <- model.selection.df[, c(13, 1:3, 12, 4:11)]
model.selection.df <- model.selection.df[with(model.selection.df, order(CompetitionRelativeTo, Network)), ]
write.csv(x = model.selection.df, file = paste(path, "Output/ModelSelectionTable_Cleaned.csv", sep = ""), row.names = FALSE)
# compare corrected with previous dataset
ms.compare <- model.selection.df[, c(1, 2, 3, 4, 5, 6, 7)]
ms.old <- read.csv(file = paste(path, "OldVersions/Oct2020/Output/ModelSelectionTable_Cleaned.csv", sep = ""))
ms.old$Network <- factor(ms.old$Network, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012", 
                                                    "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008", "Armas & Pugnaire 2011", 
                                                    "Löf et al. 2014", "Kinlock unpublished",  "Kinlock unpublished (b)"))
ms.old$CompetitionRelativeTo <- factor(ms.old$CompetitionRelativeTo, levels = c("Monoculture", "True control"))
ms.old <- ms.old[with(ms.old, order(CompetitionRelativeTo, Network)), ]
# Kinlock unpublished (b) was erroneously included in the monoculture set
ms.old <- ms.old[which(!(ms.old$CompetitionRelativeTo == "Monoculture" & ms.old$Network == "Kinlock unpublished (b)")), ]
ms.compare$CompareFits <- ms.compare$Fit == ms.old$Fit
ms.compare$ComparePar1 <- ms.compare$Parameter1 == ms.old$Parameter1
#
# calculate the best fitting distributions across networks for monoculture and true control
model.selection.subset <- model.selection.df[which(model.selection.df$Fit == "best fit"), ]
model.selection.mono.ctrl <- model.selection.subset[which(model.selection.subset$CompetitionRelativeTo == "Monoculture"), ]
model.selection.true.ctrl <- model.selection.subset[which(model.selection.subset$CompetitionRelativeTo == "True control"), ]
ddply(model.selection.mono.ctrl, .(Metric, Distribution), summarise, Frequency = length(unique(Network)))
ddply(model.selection.true.ctrl, .(Metric, Distribution), summarise, Frequency = length(unique(Network)))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# COLLATE DATA TABLE INFORMATION ----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# calculate the mean and variance of plant performance for each network (to add to DataTable)
case.names.init <- list.files(path = paste(path, "Input/CaseData", sep = ""), recursive = FALSE)
case.names.init <- substr(case.names.init, 0, nchar(case.names.init) - 4)
case.names.init[grep(case.names.init, pattern = "_imp")] <- substr(case.names.init[grep(case.names.init, pattern = "_imp")], 0, nchar(case.names.init[grep(case.names.init, pattern = "_imp")]) - 4)
case.names.init[grep(case.names.init, pattern = "_Cntrl")] <- substr(case.names.init[grep(case.names.init, pattern = "_Cntrl")], 0, nchar(case.names.init[grep(case.names.init, pattern = "_Cntrl")]) - 6)
case.names <- case.names.init
treatment.indices <- c(9, 11, 18, 23, 25, 29, 30)  # remove treatment networks
case.names <- case.names[-treatment.indices]
reorder.by.coding.df <- vector(mode = "integer", length = length(case.names))
for (x in 1:length(case.names)) {
  reorder.by.coding.df[x] <- grep(pattern = coding.df$Filename[x], x = case.names)
}
case.names <- case.names[reorder.by.coding.df]
file.paths <- list.files(path = paste(path, "Input/CaseData", sep = ""), full.names = TRUE)
file.paths <- file.paths[-treatment.indices]
file.paths <- file.paths[reorder.by.coding.df]
case.data <- lapply(file.paths, read.csv)
mean.case.data <- unlist(lapply(case.data, function(x) mean(x$Metric, na.rm = TRUE)))
se.case.data <- unlist(lapply(case.data, function(x) sd(x$Metric, na.rm = TRUE) / sqrt(length(x$Metric))))
mean.case.data <- unlist(lapply(case.data, function(x) mean(x$Metric, na.rm = TRUE)))
#
# calculate the networks for each case (to add to DataTable)
# for NetworkMonoCtrl
file.paths.mono <- list.files(path = paste(path, "Output/Networks", sep = ""), full.names = TRUE, recursive = FALSE, include.dirs = FALSE)
file.paths.mono <- file.paths.mono[-c(7, 9, 15, 20, 22, 26, 27, )]
# for NetworkTrueCtrl
file.paths.ctrl <- list.files(path = paste(path, "Output/Networks/TrueControl", sep = ""), full.names = TRUE, recursive = FALSE, include.dirs = FALSE)
# indices for networks with true control 
ctrl.indices <- c(1, 2, 3, 5, 6, 8, 13, 14, 22, 26, 32, 34, 36, 37, 38)
case.names.ctrl <- case.names.init[ctrl.indices]
# networks with monoculture control
mono.indices <- c(1, 2, 3, 5, 6, 8, 13, 22, 26, 32, 36, 38, 4, 7, 10, 12, 15, 16, 17, 19, 20, 21, 24, 27, 28, 31, 33, 35)
case.names.mono <- case.names.init[mono.indices]
# reorder names based on CodingTable order
reorder.ctrl <- c()
reorder.mono <- c()
for (x in 1:length(coding.df$Filename)) {
  check.ctrl <- grep(pattern = coding.df$Filename[x], x = case.names.ctrl)
  if (length(check.ctrl) > 0) {
    reorder.ctrl <- c(reorder.ctrl, check.ctrl)
  }
  check.mono <- grep(pattern = coding.df$Filename[x], x = case.names.mono)
  if (length(check.mono) > 0) {
    reorder.mono <- c(reorder.mono, check.mono)  
  }
}
case.names.ctrl <- case.names.ctrl[reorder.ctrl]
case.names.mono <- case.names.mono[reorder.mono]
# use reordered names to reorder file paths
file.paths.mono <- file.paths.mono[unname(unlist(sapply(case.names.mono, function(x) grep(pattern = paste(x, ".csv", sep = ""), x = file.paths.mono))))]
file.paths.ctrl <- file.paths.ctrl[unname(unlist(sapply(case.names.ctrl, function(x) grep(pattern = paste(x, ".csv", sep = ""), x = file.paths.ctrl))))]
case.data.mono <- lapply(file.paths.mono, function(x) read.csv(x, header = FALSE, colClasses = "numeric"))
case.data.ctrl <- lapply(file.paths.ctrl, function(x) read.csv(x, header = FALSE, colClasses = "numeric"))
lapply(case.data.mono, function(x) paste(as.vector(t(round(x, digits = 4))), collapse = ", "))
lapply(case.data.ctrl, function(x) paste(as.vector(t(round(x, digits = 4))), collapse = ", "))

