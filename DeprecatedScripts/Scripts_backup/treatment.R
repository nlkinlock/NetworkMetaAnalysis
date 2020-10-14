# COMPARE NETWORK METRICS BETWEEN NETWORKS WITH DIFFERENT TREATMENTS
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD TREATMENT NETWORKS AND COMBINE W/ CODING TABLE -------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# all networks with monoculture control (1) or include networks with true control only (2)?
set.rii.control.type <- 2
treatments.df <- read.csv(paste(path, "Output/NetworkMetricsTreatments.csv", sep = ""), row.names = 1)
if (set.rii.control.type == 1) {
  treatments.df <- treatments.df[which(treatments.df$CompetitionComparison == "Monoculture"), ]
} else if (set.rii.control.type == 2) {
  true.ctrl.indices <- c(grep(pattern = "1448_Gurevitch_1990", x = treatments.df$Network[which(treatments.df$CompetitionComparison == "Monoculture")]), 
                         grep(pattern = "1787_Weigelt_2002", x = treatments.df$Network[which(treatments.df$CompetitionComparison == "Monoculture")]))
  treatments.df <- treatments.df[-true.ctrl.indices, ]
}
treatments.df <- data.frame(treatments.df, BootstrapSDL = treatments.df$ObservedMean - treatments.df$BootstrapSD,
                              BootstrapSDU = treatments.df$ObservedMean + treatments.df$BootstrapSD)
treatments.df$BootstrapVar <- treatments.df$BootstrapSD^2
UniqueID <- ExtractUniqueID(treatments.df$Network)
treatments.df$UniqueID <- as.factor(UniqueID)
# combine coding table and bootstrap output
treatments.df <- join_all(list(treatments.df, coding.df), by = "UniqueID")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# META-ANALYSES OF TREATMENT NETWORKS -----------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# meta-analyses necessary for network-specific estimates (y hats)
meta.analysis.init <- treatments.df[which(treatments.df$MetricName == "MeanStrength" | treatments.df$MetricName == "Imbalance" | 
                                            treatments.df$MetricName == "IndirectEffect"), ]
zeros <- which(meta.analysis.init$BootstrapVar == 0)  # zero variance will cause model to fail, set to a negligibly small value
meta.analysis.init$BootstrapVar[zeros] <- meta.analysis.init$BootstrapVar[zeros] + 1E-9
metric.names <- c("MeanStrength", "Imbalance", "IndirectEffect")

meta.analysis.df <- data.frame(ModelType = character(), NumCases = integer(), Distribution = character(), 
                                MetricName = character(), Network = character(), Parameter = character(), 
                                EstimatedValues = numeric(), MetaAnalysisCIL = numeric(), MetaAnalysisCIU = numeric())
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                                        observed.var = subset.df$BootstrapVar, parameter.inits = GenerateInits, 
                                        save.parameters = c("estimated.y", "overall.mean", "overall.sd"), file.jags = ma.normal.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE, 
              filename = paste(path, "Output/Figures/Convergence/normal_overall_", metric.names[a], sep = ""))
  output <- ExtractJAGSOutput(fit = meta.analysis.fit, groupwise = FALSE)
  output.add <- data.frame(ModelType = rep("Grand mean", nrow(output)), NumCases = nrow(subset.df), Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)  # store in overall dataframe with all metrics
}
meta.analysis.by.network <- meta.analysis.df[which(meta.analysis.df$Network != "All"), ]
rownames(meta.analysis.by.network) <- 1:nrow(meta.analysis.by.network)
#
# load df with only truncated normal distributed metrics
meta.analysis.tnorm.init <- treatments.df[which(treatments.df$MetricName == "WeightedConnectance" | 
                                          treatments.df$MetricName == "RelativeIntransitivity"), ]
meta.analysis.tnorm.init$BootstrapVar <- meta.analysis.tnorm.init$BootstrapVar + 1E-6  # negligibly small value helps alleviate numerical issues with convergence
meta.analysis.tnorm.init$ObservedMean <- meta.analysis.tnorm.init$ObservedMean + 1E-6
metric.names <- c("WeightedConnectance", "RelativeIntransitivity")
meta.analysis.tnorm.df <- data.frame(ModelType = character(), NumCases = integer(), Distribution = character(), 
                                      MetricName = character(), Network = character(), Parameter = character(), 
                                      EstimatedValues = numeric(), MetaAnalysisCIL = numeric(), MetaAnalysisCIU = numeric())
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.tnorm.init[which(meta.analysis.tnorm.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  while (1 == 1) {
    meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                                          observed.var = subset.df$BootstrapVar, parameter.inits = GenerateInits,
                                          save.parameters = c("estimated.y", "overall.mean", "overall.sd"), file.jags = ma.tnormal.file)
    if (all(meta.analysis.fit$BUGSoutput$summary[, 8] < gr.threshold & meta.analysis.fit$BUGSoutput$summary[, 9] > neff.threshold)) {
      break
    }
  }
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE, 
              filename = paste(path, "Output/Figures/Convergence/treatment_tnormal_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(fit = meta.analysis.fit, groupwise = FALSE)
  output.add <- data.frame(ModelType = rep("Grand mean", nrow(output)), NumCases = rep(nrow(subset.df), nrow(output)), 
                            Distribution = rep("TNormal", nrow(output)), output)
  meta.analysis.tnorm.df <- rbind(meta.analysis.tnorm.df, output.add)  # store in overall dataframe with all metrics
}
meta.analysis.by.network.tnorm <- meta.analysis.tnorm.df[which(meta.analysis.tnorm.df$Network != "All"), ]
rownames(meta.analysis.by.network.tnorm) <- 1:nrow(meta.analysis.by.network.tnorm)
meta.analysis.treatment <- rbind(meta.analysis.by.network, meta.analysis.by.network.tnorm)
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# CREATE FINAL DATAFRAME ------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
network.name <- as.character(meta.analysis.treatment$Network)
treatment.test <- substr(network.name, nchar(network.name), nchar(network.name))
ctrl.indices <- grep(pattern = "[[:digit:]]", x = treatment.test)
meta.analysis.treatment$Treatment <- NA
meta.analysis.treatment$Treatment[-ctrl.indices] <- "Treatment"
meta.analysis.treatment$Treatment[ctrl.indices] <- "Control"
meta.analysis.treatment$TreatmentType <- NA
meta.analysis.treatment$TreatmentType[ctrl.indices] <- "Control"
meta.analysis.treatment$TreatmentType[which(meta.analysis.treatment$Network == "1448_Gurevitch_1990_Fert")] <- "Increased nutrients"
meta.analysis.treatment$TreatmentType[which(meta.analysis.treatment$Network == "1454_PfeiferMeister_2008_LowNutrient")] <- "Decreased nutrients"
meta.analysis.treatment$TreatmentType[which(meta.analysis.treatment$Network == "1566_Niu_2008_Warm")] <- "Warming"
meta.analysis.treatment$TreatmentType[which(meta.analysis.treatment$Network == "1787_Weigelt_2002_LowWater")] <- "Decreased water"
meta.analysis.treatment$TreatmentType[which(meta.analysis.treatment$Network == "192_Amanullah_2013_LW")] <- "Decreased water"
meta.analysis.treatment$TreatmentType[which(meta.analysis.treatment$Network == "236_Jiang_2014_N")] <- "Increased nutrients"
meta.analysis.treatment$TreatmentType[which(meta.analysis.treatment$Network == "236_Jiang_2014_Water")] <- "Increased water"
meta.analysis.treatment$network_b <- meta.analysis.treatment$Network
meta.analysis.treatment$network_b[which(meta.analysis.treatment$Network == "1448_Gurevitch_1990_Fert")] <- "1448_Gurevitch_1990"
meta.analysis.treatment$network_b[which(meta.analysis.treatment$Network == "1454_PfeiferMeister_2008_LowNutrient")] <- "1454_PfeiferMeister_2008"
meta.analysis.treatment$network_b[which(meta.analysis.treatment$Network == "1566_Niu_2008_Warm")] <- "1566_Niu_2008"
meta.analysis.treatment$network_b[which(meta.analysis.treatment$Network == "1787_Weigelt_2002_LowWater")] <- "1787_Weigelt_2002"
meta.analysis.treatment$network_b[which(meta.analysis.treatment$Network == "192_Amanullah_2013_LW")] <- "192_Amanullah_2013"
meta.analysis.treatment$network_b[which(meta.analysis.treatment$Network == "236_Jiang_2014_N")] <- "236_Jiang_2014"
meta.analysis.treatment$network_b[which(meta.analysis.treatment$Network == "236_Jiang_2014_Water")] <- "236_Jiang_2014"
remove.col <- which(colnames(meta.analysis.treatment) == "Network")
meta.analysis.treatment <- meta.analysis.treatment[, -remove.col]
colnames(meta.analysis.treatment)[ncol(meta.analysis.treatment)] <- "Network"
meta.analysis.treatment$Network <- factor(meta.analysis.treatment$Network)
levels(meta.analysis.treatment$Network) <- c("Gurevitch et al. 1990", "Pfeifer-Meis. et al. 2008", "Niu & Wan 2008",  "Weigelt et al. 2002",
                                              "Amanull. & Stewart 2013", "Jiang et al. 2014")
meta.analysis.treatment$Network <- factor(meta.analysis.treatment$Network, levels = c("Niu & Wan 2008", "Pfeifer-Meis. et al. 2008", 
                                          "Amanull. & Stewart 2013", "Jiang et al. 2014", "Gurevitch et al. 1990", "Weigelt et al. 2002"))
#
# write output to file
if (set.rii.control.type == 2) {
  write.csv(x = meta.analysis.treatment, file = paste(path, "Output/ControlTreatmentNetworkMetrics_TrueCtrlInc.csv", sep = ""))
} else if (set.rii.control.type == 1) {
  write.csv(x = meta.analysis.treatment, file = paste(path, "Output/ControlTreatmentNetworkMetrics_MonoCtrl.csv", sep = ""))
}

