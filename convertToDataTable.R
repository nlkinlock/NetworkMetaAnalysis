# write CSV for complete meta analysis output table in appendices
# read in monoculture control
ma.table <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteMetaAnalysisOutput_MonoCtrl.csv", stringsAsFactors = FALSE)
ma.table <- ma.table[which(ma.table$Metric != "Asymmetry"), ]
ma.table$CompetitionRelativeTo <- "Monoculture"
ma.table <- ma.table[, c(12, 2, 5, 6, 7, 8, 9, 3, 10, 11)]
colnames(ma.table) <- c("CompetitionRelativeTo", "Comparison", "Metric", "Parameter", "Estimate", "CRILowerBound", "CRIUpperBound", "n", "rhat", "Effectiven")
ma.table <- ma.table[which(ma.table$Parameter != "Heterogeneity"), ]
ma.table$Parameter[which(ma.table$Parameter == "OverallSD")] <- "Among-network variation"
ma.table$Parameter[which(ma.table$Parameter == "OverallMean")] <- "Grand mean"
ma.table$Comparison[which(ma.table$Comparison == "Setting")] <- "Experiment setting"
ma.table$Comparison[which(ma.table$Comparison == "Habit")] <- "Growth habit"
ma.table$Comparison[which(ma.table$Comparison == "Age")] <- "Plant age"
ma.table$Metric[which(ma.table$Metric == "MeanStrength")] <- "Mean strength"
ma.table$Metric[which(ma.table$Metric == "IndirectEffect")] <- "Indirect effect"
ma.table$Metric[which(ma.table$Metric == "WeightedConnectance")] <- "Weighted connectance"
ma.table$Metric[which(ma.table$Metric == "WeightedCompConnectance")] <- "W. comp. connectance"
ma.table$Metric[which(ma.table$Metric == "WeightedFacConnectance")] <- "W. fac. connectance"
ma.table$Metric[which(ma.table$Metric == "RelativeIntransitivity")] <- "Relative intransitivity"
ma.table$Estimate <- round(ma.table$Estimate, digits = 3)
ma.table$CRILowerBound <- round(ma.table$CRILowerBound, digits = 3)
ma.table$CRIUpperBound <- round(ma.table$CRIUpperBound, digits = 3)
ma.table$rhat <- round(ma.table$rhat, digits = 3)
ma.table$Effectiven <- round(ma.table$Effectiven, digits = 3)
ma.table$Comparison <- factor(ma.table$Comparison, levels = c("Grand mean", "Habitat", "Experiment setting", "Growth habit", "Plant age"))
ma.table <- ma.table[order(ma.table$Comparison), ]
# read in true control
ma.table.true.ctrl <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteMetaAnalysisOutput_TrueCtrlOnly.csv", stringsAsFactors = FALSE)
ma.table.true.ctrl <- ma.table.true.ctrl[which(ma.table.true.ctrl$Metric != "Asymmetry"), ]
ma.table.true.ctrl$CompetitionRelativeTo <- "True control"
ma.table.true.ctrl <- ma.table.true.ctrl[, c(12, 2, 5, 6, 7, 8, 9, 3, 10, 11)]
colnames(ma.table.true.ctrl) <- c("CompetitionRelativeTo", "Comparison", "Metric", "Parameter", "Estimate", "CRILowerBound", "CRIUpperBound", "n", "rhat", "Effectiven")
ma.table.true.ctrl <- ma.table.true.ctrl[which(ma.table.true.ctrl$Parameter != "Heterogeneity"), ]
ma.table.true.ctrl$Parameter[which(ma.table.true.ctrl$Parameter == "OverallSD")] <- "Among-network variation"
ma.table.true.ctrl$Parameter[which(ma.table.true.ctrl$Parameter == "OverallMean")] <- "Grand mean"
ma.table.true.ctrl$Comparison[which(ma.table.true.ctrl$Comparison == "Setting")] <- "Experiment setting"
ma.table.true.ctrl$Comparison[which(ma.table.true.ctrl$Comparison == "Habit")] <- "Growth habit"
ma.table.true.ctrl$Comparison[which(ma.table.true.ctrl$Comparison == "Age")] <- "Plant age"
ma.table.true.ctrl$Metric[which(ma.table.true.ctrl$Metric == "MeanStrength")] <- "Mean strength"
ma.table.true.ctrl$Metric[which(ma.table.true.ctrl$Metric == "IndirectEffect")] <- "Indirect effect"
ma.table.true.ctrl$Metric[which(ma.table.true.ctrl$Metric == "WeightedConnectance")] <- "Weighted connectance"
ma.table.true.ctrl$Metric[which(ma.table.true.ctrl$Metric == "WeightedCompConnectance")] <- "W. comp. connectance"
ma.table.true.ctrl$Metric[which(ma.table.true.ctrl$Metric == "WeightedFacConnectance")] <- "W. fac. connectance"
ma.table.true.ctrl$Metric[which(ma.table.true.ctrl$Metric == "RelativeIntransitivity")] <- "Relative intransitivity"
ma.table.true.ctrl$Estimate <- round(ma.table.true.ctrl$Estimate, digits = 3)
ma.table.true.ctrl$CRILowerBound <- round(ma.table.true.ctrl$CRILowerBound, digits = 3)
ma.table.true.ctrl$CRIUpperBound <- round(ma.table.true.ctrl$CRIUpperBound, digits = 3)
ma.table.true.ctrl$rhat <- round(ma.table.true.ctrl$rhat, digits = 3)
ma.table.true.ctrl$Effectiven <- round(ma.table.true.ctrl$Effectiven, digits = 3)
ma.table.true.ctrl$Comparison <- factor(ma.table.true.ctrl$Comparison, levels = c("Grand mean", "Habitat", "Experiment setting", "Growth habit", "Plant age"))
ma.table.true.ctrl <- ma.table.true.ctrl[order(ma.table.true.ctrl$Comparison), ]
ma.table <- rbind(ma.table, ma.table.true.ctrl)
row.names(ma.table) <- 1:nrow(ma.table)
write.csv(x = ma.table, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysisTable_Cleaned.csv", row.names = FALSE)

# write CSV for model selection table in appendices
# read in monoculture control
model.selection.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/ModelSelectionSubset_MonoCtrl.csv")
model.selection.df$Distribution <- factor(model.selection.df$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
levels(model.selection.df$Case) <- c("Armas & Pugnaire 2011", "Svenning et al. 2008", "Niu & Wan 2008", "Mariotte et al. 2012", "Jiang et al. 2014",
                                     "Miller & Werner 1987", "Engel & Weltzin 2008", "Goldberg & Landa 1991", "Kinlock unpublished (b)", "Kinlock unpublished")
model.selection.df$Case <- factor(model.selection.df$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012", 
                                                                      "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008", "Armas & Pugnaire 2011", 
                                                                      "Kinlock unpublished (b)", "Kinlock unpublished"))
model.selection.df <- model.selection.df[model.selection.df$Metric != "Weight", ]
model.selection.df <- model.selection.df[, -c(6, 10, 13, 14)]
colnames(model.selection.df) <- c("Network", "Metric", "Distribution", "Parameter1", "Parameter2", "PPPmedian", "PPPmean", "PPPupper", "pWAIC", "WAIC", "SEWAIC", "Fit", "CompetitionRelativeTo")
model.selection.df$Parameter1 <- round(model.selection.df$Parameter1, digits = 2)
model.selection.df$Parameter2 <- round(model.selection.df$Parameter2, digits = 2)
model.selection.df$PPPmedian <- round(model.selection.df$PPPmedian, digits = 3)
model.selection.df$PPPmean <- round(model.selection.df$PPPmean, digits = 3)
model.selection.df$PPPupper <- round(model.selection.df$PPPupper, digits = 3)
model.selection.df$pWAIC <- round(model.selection.df$pWAIC, digits = 2)
model.selection.df$WAIC <- round(model.selection.df$WAIC, digits = 2)
model.selection.df$SEWAIC <- round(model.selection.df$SEWAIC, digits = 2)
# read in true control
model.selection.true.ctrl.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/ModelSelectionSubset_TrueCtrlOnly.csv")
model.selection.true.ctrl.df$Distribution <- factor(model.selection.true.ctrl.df$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
levels(model.selection.true.ctrl.df$Case) <- c("Armas & Pugnaire 2011", "Svenning et al. 2008", "Löf et al. 2014", "Goldberg & Landa 1991", "Kinlock unpublished (b)", "Kinlock unpublished")
model.selection.true.ctrl.df$Case <- factor(model.selection.true.ctrl.df$Case, levels = c("Goldberg & Landa 1991", "Svenning et al. 2008", "Armas & Pugnaire 2011", "Löf et al. 2014",
                                                                                          "Kinlock unpublished (b)", "Kinlock unpublished"))
model.selection.true.ctrl.df <- model.selection.true.ctrl.df[, -c(6, 10, 13, 14)]
colnames(model.selection.true.ctrl.df) <- c("Network", "Metric", "Distribution", "Parameter1", "Parameter2", "PPPmedian", "PPPmean", "PPPupper", "pWAIC", "WAIC", "SEWAIC", "Fit", "CompetitionRelativeTo")
model.selection.true.ctrl.df$Parameter1 <- round(model.selection.true.ctrl.df$Parameter1, digits = 2)
model.selection.true.ctrl.df$Parameter2 <- round(model.selection.true.ctrl.df$Parameter2, digits = 2)
model.selection.true.ctrl.df$PPPmedian <- round(model.selection.true.ctrl.df$PPPmedian, digits = 3)
model.selection.true.ctrl.df$PPPmean <- round(model.selection.true.ctrl.df$PPPmean, digits = 3)
model.selection.true.ctrl.df$PPPupper <- round(model.selection.true.ctrl.df$PPPupper, digits = 3)
model.selection.true.ctrl.df$pWAIC <- round(model.selection.true.ctrl.df$pWAIC, digits = 2)
model.selection.true.ctrl.df$WAIC <- round(model.selection.true.ctrl.df$WAIC, digits = 2)
model.selection.true.ctrl.df$SEWAIC <- round(model.selection.true.ctrl.df$SEWAIC, digits = 2)
model.selection.df <- rbind(model.selection.df, model.selection.true.ctrl.df)
model.selection.df <- model.selection.df[, c(13, 1:3, 12, 4:11)]
write.csv(x = model.selection.df, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/ModelSelectionTable_Cleaned.csv", row.names = FALSE)

# calculate the best fitting distributions across networks for monoculture and true control
model.selection.subset <- model.selection.df[which(model.selection.df$Fit == "best fit"), ]
model.selection.mono.ctrl <- model.selection.subset[which(model.selection.subset$CompetitionRelativeTo == "Monoculture"), ]
model.selection.true.ctrl <- model.selection.subset[which(model.selection.subset$CompetitionRelativeTo == "True control"), ]
ddply(model.selection.mono.ctrl, .(Metric, Distribution), summarise, Frequency = length(unique(Network)))
ddply(model.selection.true.ctrl, .(Metric, Distribution), summarise, Frequency = length(unique(Network)))


ddply(model.selection.mono.ctrl, .(Metric, Distribution), transform, MinPPPMedian = rank(PPPmedian))
ddply(model.selection.true.ctrl, .(Metric, Distribution), transform, MinPPPMedian = rank(PPPmedian))




# calculate the mean and variance of plant performance for each network (to add to DataTable)
case.names <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", recursive = FALSE)
case.names <- substr(case.names, 0, nchar(case.names) - 4)
case.names[grep(case.names, pattern = "_imp")] <- substr(case.names[grep(case.names, pattern = "_imp")], 0, nchar(case.names[grep(case.names, pattern = "_imp")]) - 4)
case.names[grep(case.names, pattern = "_Cntrl")] <- substr(case.names[grep(case.names, pattern = "_Cntrl")], 0, nchar(case.names[grep(case.names, pattern = "_Cntrl")]) - 6)
treatment.indices <- c(9, 11, 18, 23, 25, 29, 30)  # remove treatment networks
case.names <- case.names[-treatment.indices]
reorder.by.coding.df <- vector(mode = "integer", length = length(case.names))
for (x in 1:length(case.names)) {
  reorder.by.coding.df[x] <- grep(pattern = coding.df$Filename[x], x = case.names)
}
case.names <- case.names[reorder.by.coding.df]
file.paths <- list.files(path = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Input/CaseData", full.names = TRUE)
file.paths <- file.paths
file.paths <- file.paths[reorder.by.coding.df]
case.data <- lapply(file.paths, read.csv)

mean.case.data <- unlist(lapply(case.data, function(x) mean(x$Metric, na.rm = TRUE)))
se.case.data <- unlist(lapply(case.data, function(x) sd(x$Metric, na.rm = TRUE) / sqrt(length(x$Metric))))



