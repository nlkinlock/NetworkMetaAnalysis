# BAYESIAN RANDOM EFFECTS META-ANALYSIS
# TRUNCATED NORMAL DISTRIBUTED COVARIATES
#
# GRAND MEAN ------------------------------------------------
#
# load main data frame from combine_data.R
# means and replicate number for each metric
if (k == 2) {
  meta.analysis.init <- post.bootstrap.df[which(post.bootstrap.df$MetricName == "WeightedConnectance" | 
                                                post.bootstrap.df$MetricName == "WeightedCompConnectance" | 
                                                post.bootstrap.df$MetricName == "WeightedFacConnectance" | 
                                                post.bootstrap.df$MetricName == "RelativeIntransitivity" |
                                                post.bootstrap.df$MetricName == "Asymmetry"), ]
  metric.names <- c("WeightedConnectance", "WeightedCompConnectance", "WeightedFacConnectance", "RelativeIntransitivity", "Asymmetry")
} else if (k == 1) {
  meta.analysis.init <- post.bootstrap.df[which(post.bootstrap.df$MetricName == "WeightedConnectance" | 
                                                post.bootstrap.df$MetricName == "WeightedCompConnectance" | 
                                                post.bootstrap.df$MetricName == "WeightedFacConnectance" | 
                                                post.bootstrap.df$MetricName == "RelativeIntransitivity"), ]
  metric.names <- c("WeightedConnectance", "WeightedCompConnectance", "WeightedFacConnectance", "RelativeIntransitivity")
}
meta.analysis.init$BootstrapVar <- meta.analysis.init$BootstrapVar + 1E-6  # negligibly small value helps alleviate numerical issues with convergence
meta.analysis.init$ObservedMean[which(meta.analysis.init$ObservedMean < 1)] <- meta.analysis.init$ObservedMean[which(meta.analysis.init$ObservedMean < 1)] + 1E-6
meta.analysis.init$ObservedMean[which(meta.analysis.init$ObservedMean == 1)] <- meta.analysis.init$ObservedMean[which(meta.analysis.init$ObservedMean == 1)] - 1E-6
meta.analysis.df <- data.frame(ModelType = character(), NumCases = integer(), Distribution = character(), 
                                MetricName = character(), Network = character(), Parameter = character(), 
                                EstimatedValues = numeric(), MetaAnalysisCIL = numeric(), MetaAnalysisCIU = numeric())
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
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
            filename = paste(path, "Output/Figures/Convergence/tnormal_overall_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(fit = meta.analysis.fit, groupwise = FALSE)
  output.add <- data.frame(ModelType = rep("Grand mean", nrow(output)), NumCases = rep(nrow(subset.df), nrow(output)),
                            Distribution = rep("TNormal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)  # store in overall dataframe with all metrics
}
print(paste("Trunc. normal meta-analysis for grand mean is complete at ", Sys.time(), sep = ""))
#
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
  while (1 == 1) {
    meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                                          observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                                          num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, 
                                          save.parameters = c("beta", "overall.sd"), file.jags = ma.tnormal.group.file)
    if (all(meta.analysis.fit$BUGSoutput$summary[, 8] < gr.threshold & meta.analysis.fit$BUGSoutput$summary[, 9] > neff.threshold)) {
      break
    }
  }
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE,
            filename = paste(path, "Output/Figures/Convergence/tnormal_habitat_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Habitat", nrow(output)), 
                            NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)),
                            Distribution = rep("TNormal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
}
print(paste("Trunc. normal meta-analysis for habitat is complete at ", Sys.time(), sep = ""))
#
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
  while (1 == 1) {
    meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                                          observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                                          num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, 
                                          save.parameters = c("beta", "overall.sd"), file.jags = ma.tnormal.group.file)
    if (all(meta.analysis.fit$BUGSoutput$summary[, 8] < gr.threshold & meta.analysis.fit$BUGSoutput$summary[, 9] > neff.threshold)) {
      break
    }
  }
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE,
            filename = paste(path, "Output/Figures/Convergence/tnormal_setting_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Setting", nrow(output)),
                            NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), sum(subset.df$Group == 3), nrow(subset.df)),
                            Distribution = rep("TNormal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
}
print(paste("Trunc. normal meta-analysis for experiment setting is complete at ", Sys.time(), sep = ""))
#
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
  while (1 == 1) {
    meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean,
                                          observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                                          num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, 
                                          save.parameters = c("beta", "overall.sd"), file.jags = ma.tnormal.group.file)
    if (all(meta.analysis.fit$BUGSoutput$summary[, 8] < gr.threshold & meta.analysis.fit$BUGSoutput$summary[, 9] > neff.threshold)) {
      break
    }
  }
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE,
            filename = paste(path, "Output/Figures/Convergence/tnormal_herbwood_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Habit", nrow(output)),
                            NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)),
                            Distribution = rep("TNormal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
}
print(paste("Trunc. normal meta-analysis for growth habit is complete at ", Sys.time(), sep = ""))
#
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
  while (1 == 1) {
    meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                                          observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                                          num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise,
                                          save.parameters = c("beta", "overall.sd"), file.jags = ma.tnormal.group.file)
    if (all(meta.analysis.fit$BUGSoutput$summary[, 8] < gr.threshold & meta.analysis.fit$BUGSoutput$summary[, 9] > neff.threshold)) {
      break
    }
  }
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE,
            filename = paste(path, "Output/Figures/Convergence/tnormal_age_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Age", nrow(output)), 
                            NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)),
                            Distribution = rep("TNormal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
}
print(paste("Trunc. normal meta-analysis for plant age is complete at ", Sys.time(), sep = ""))
#
# save output
meta.analysis.by.network <- meta.analysis.df[which(meta.analysis.df$Network != "All"), ]
rownames(meta.analysis.by.network) <- 1:nrow(meta.analysis.by.network)
meta.analysis.df <- meta.analysis.df[which(meta.analysis.df$Network == "All"), ]
meta.analysis.df <- meta.analysis.df[, -5]
rownames(meta.analysis.df) <- 1:nrow(meta.analysis.df)
write.csv(x = meta.analysis.by.network, file = paste(path, "Output/MetaAnalysis_ByNetwork_TNormal_", rii.type.name, ".csv", sep = ""))
write.csv(x = meta.analysis.df, file = paste(path, "Output/MetaAnalysis_TNormal_", rii.type.name, ".csv", sep = ""))



