# BAYESIAN RANDOM EFFECTS META-ANALYSIS
# NORMALLY DISTRIBUTED COVARIATES

#
# GRAND MEAN ------------------------------------------------
#
# load main data frame from combine_data.R
# means and variances for each metric
meta.analysis.init <- post.bootstrap.df[which(post.bootstrap.df$MetricName == "MeanStrength" | post.bootstrap.df$MetricName == "Imbalance" | post.bootstrap.df$MetricName == "IndirectEffect"), ]
zeros <- which(meta.analysis.init$BootstrapVar == 0)  # zero variance will cause model to fail, set to a negligibly small value
meta.analysis.init$BootstrapVar[zeros] <- meta.analysis.init$BootstrapVar[zeros] + 1E-9
metric.names <- c("MeanStrength", "Imbalance", "IndirectEffect")

meta.analysis.df <- data.frame(ModelType = character(), NumCases = integer(), Distribution = character(), MetricName = character(), Network = character(), Parameter = character(), 
                               EstimatedValues = numeric(), MetaAnalysisCIL = numeric(), MetaAnalysisCIU = numeric())
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, observed.var = subset.df$BootstrapVar, parameter.inits = GenerateInits, 
                             save.parameters = c("estimated.y", "overall.mean", "overall.sd"), file.jags = ma.normal.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, filename = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/normal_overall_", metric.names[a], "_", rii.type.name, sep = ""), open_pdf = FALSE)
  output <- ExtractJAGSOutput(fit = meta.analysis.fit, groupwise = FALSE)
  output.add <- data.frame(ModelType = rep("Grand mean", nrow(output)), NumCases = rep(nrow(subset.df), nrow(output)), Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)  # store in overall dataframe with all metrics
}
print(paste("Normal meta-analysis for grand mean is complete at ", Sys.time(), sep = ""))

#
# GROUPED BY OLD FIELD/GRASSLAND ------------------------------------------------
#
ind <- which(meta.analysis.init$Habitat == "Old field" | meta.analysis.init$Habitat == "Grassland")
meta.analysis.init$Group <- NA
meta.analysis.init$Group[ind] <- 1
meta.analysis.init$Group[-ind] <- 2
group.labels <- c("Grassland", "Other")
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                         num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, save.parameters = c("beta", "overall.sd"), file.jags = ma.normal.group.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, filename = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/normal_habitat_", metric.names[a], "_", rii.type.name, sep = ""), open_pdf = FALSE)
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Habitat", nrow(output)), NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)), Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
}
print(paste("Normal meta-analysis for habitat is complete at ", Sys.time(), sep = ""))

#
# GROUPED BY GREENHOUSE/FIELD/GARDEN ------------------------------------------------
#
gh <- which(meta.analysis.init$ExperimentClass == "Greenhouse")
fd <- which(meta.analysis.init$ExperimentClass == "Field")
gdn <- which(meta.analysis.init$ExperimentClass == "Garden")
meta.analysis.init$Group <- NA
meta.analysis.init$Group[gh] <- 1
meta.analysis.init$Group[fd] <- 2
meta.analysis.init$Group[gdn] <- 3
group.labels <- c("Greenhouse", "Field", "Garden")
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                         num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, save.parameters = c("beta", "overall.sd"),
                         file.jags = ma.normal.group.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, filename = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/normal_setting_", metric.names[a], "_", rii.type.name, sep = ""), open_pdf = FALSE)
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Setting", nrow(output)), NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), sum(subset.df$Group == 3), nrow(subset.df)),  Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
}
print(paste("Normal meta-analysis for experiment setting is complete at ", Sys.time(), sep = ""))

#
# GROUPED BY HERBACEOUS/WOODY ------------------------------------------------
#
hb <- which(meta.analysis.init$WoodyHerbaceous == "Herbaceous")
wd <- which(meta.analysis.init$WoodyHerbaceous == "Woody" | meta.analysis.init$WoodyHerbaceous == "Both")
meta.analysis.init$Group <- NA
meta.analysis.init$Group[hb] <- 1
meta.analysis.init$Group[wd] <- 2
group.labels <- c("Herbaceous", "Woody")
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                         num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, save.parameters = c("beta", "overall.sd"),
                         file.jags = ma.normal.group.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, filename = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/normal_herbwood_", metric.names[a], "_", rii.type.name, sep = ""), open_pdf = FALSE)
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Habit", nrow(output)),NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)), Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
}
print(paste("Normal meta-analysis for growth habit is complete at ", Sys.time(), sep = ""))

#
# GROUPED BY JUVENILE/ADULT ------------------------------------------------
#
jv <- which(meta.analysis.init$PlantAge == "Juvenile")
ad <- which(meta.analysis.init$PlantAge == "Adult" | meta.analysis.init$PlantAge == "Both")
meta.analysis.init$Group <- NA
meta.analysis.init$Group[jv] <- 1
meta.analysis.init$Group[ad] <- 2
group.labels <- c("Juvenile", "Adult")
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                         num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, save.parameters = c("beta", "overall.sd"),
                         file.jags = ma.normal.group.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, filename = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/Convergence/normal_age_", metric.names[a], "_", rii.type.name, sep = ""), open_pdf = FALSE)
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Age", nrow(output)), NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)), Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
}
print(paste("Normal meta-analysis for plant age is complete at ", Sys.time(), sep = ""))


meta.analysis.by.network <- meta.analysis.df[which(meta.analysis.df$Network != "All"), ]
rownames(meta.analysis.by.network) <- 1:nrow(meta.analysis.by.network)
meta.analysis.df <- meta.analysis.df[which(meta.analysis.df$Network == "All"), ]
meta.analysis.df <- meta.analysis.df[, -5]
rownames(meta.analysis.df) <- 1:nrow(meta.analysis.df)
write.csv(x = meta.analysis.by.network, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_ByNetwork_Normal_", rii.type.name, ".csv", sep = ""))
write.csv(x = meta.analysis.df, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_Normal_", rii.type.name, ".csv", sep = ""))


