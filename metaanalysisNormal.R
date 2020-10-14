# BAYESIAN RANDOM EFFECTS META-ANALYSIS
# NORMALLY DISTRIBUTED COVARIATES
#
# GRAND MEAN ------------------------------------------------
#
# load main data frame from combine_data.R
<<<<<<< HEAD
# means and variances for each metric
meta.analysis.init <- post.bootstrap.df[which(post.bootstrap.df$MetricName == "MeanStrength" | 
                                          post.bootstrap.df$MetricName == "Imbalance" |
                                          post.bootstrap.df$MetricName == "IndirectEffect"), ]
zeros <- which(meta.analysis.init$BootstrapVar == 0)  # zero variance will cause model to fail, set to a negligibly small value
meta.analysis.init$BootstrapVar[zeros] <- meta.analysis.init$BootstrapVar[zeros] + 1E-9
metric.names <- c("MeanStrength", "Imbalance", "IndirectEffect")
meta.analysis.df <- data.frame(ModelType = character(), NumCases = integer(), Distribution = character(), MetricName = character(), 
                                Network = character(), Parameter = character(), EstimatedValues = numeric(), MetaAnalysisCIL = numeric(),
                                MetaAnalysisCIU = numeric())
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                                      observed.var = subset.df$BootstrapVar, parameter.inits = GenerateInits, 
                                      save.parameters = c("estimated.y", "overall.mean", "overall.sd"), file.jags = ma.normal.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE, 
            filename = paste(path, "Output/Figures/Convergence/normal_overall_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(fit = meta.analysis.fit, groupwise = FALSE)
  output.add <- data.frame(ModelType = rep("Grand mean", nrow(output)), NumCases = rep(nrow(subset.df), nrow(output)), 
                            Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)  # store in overall dataframe with all metrics
}
print(paste("Normal meta-analysis for grand mean is complete at ", Sys.time(), sep = ""))
#
||||||| 2b1eac9
# means (y.hat) and variance (v) for each metric
dat.main <- merged.df[which(merged.df$metric == "strength" | merged.df$metric == "asymm" | merged.df$metric == "ind eff"), ]
zeros <- which(dat.main$var == 0)  # zero variance will cause model to fail, set to a negligibly small value
dat.main$var[zeros] <- dat.main$var[zeros] + 1E-9
metrics.ma <- c("strength", "asymm", "ind eff")

MA <- data.frame(comparison = character(), distribution = character(), metric = character(), network = character(), param = character(), est = numeric(), CI.l = numeric(), CI.u = numeric())
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.norm <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, param.inits = ma.inits, par.vec =  c("y.hat", "mu", "sigma"), file.jags = ma_normal)
  MCMCtrace(ma.norm, excl = "deviance", pdf = TRUE, filename = paste("normal_overall_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract(fit = ma.norm)
  output.row <- data.frame(comparison = rep("Grand mean", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)  # store in overall dataframe with all metrics
}

#
# GROUPED BY EXPERIMENT TYPE ------------------------------------------------
#
dat.main$Grp <- as.factor(as.numeric(x = dat.main$CtrlTreatment))
grplabels <- c("Mono ctrl", "True ctrl")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.exp <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
    g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"), file.jags = ma_normal_grp)
  MCMCtrace(ma.exp, excl = "deviance", pdf = TRUE, filename = paste("normal_experiment_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.exp)
  output.row <- data.frame(comparison = rep("Exp type", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
}

=======
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

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
# GROUPED BY OLD FIELD/GRASSLAND ------------------------------------------------
#
<<<<<<< HEAD
ind <- which(meta.analysis.init$Habitat == "Old field" | meta.analysis.init$Habitat == "Grassland")
meta.analysis.init$Group <- NA
meta.analysis.init$Group[ind] <- 1
meta.analysis.init$Group[-ind] <- 2
group.labels <- c("Grassland", "Other")
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                                        observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                                        num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, 
                                        save.parameters = c("beta", "overall.sd"), file.jags = ma.normal.group.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE, 
            filename = paste(path, "Output/Figures/Convergence/normal_habitat_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Habitat", nrow(output)), 
                            NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)), 
                            Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
||||||| 2b1eac9
ind <- which(dat.main$Habitat == "Old field" | dat.main$Habitat == "Grassland")
dat.main$Grp <- NA
dat.main$Grp[ind] <- 1
dat.main$Grp[-ind] <- 2
grplabels <- c("Grassland", "Other")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.hab <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
                         g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"), file.jags = ma_normal_grp)
  MCMCtrace(ma.hab, excl = "deviance", pdf = TRUE, filename = paste("normal_habitat_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.hab)
  output.row <- data.frame(comparison = rep("Habitat", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
print(paste("Normal meta-analysis for habitat is complete at ", Sys.time(), sep = ""))
#
||||||| 2b1eac9

=======
print(paste("Normal meta-analysis for habitat is complete at ", Sys.time(), sep = ""))

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
# GROUPED BY GREENHOUSE/FIELD/GARDEN ------------------------------------------------
#
<<<<<<< HEAD
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
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                          observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                          num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, 
                          save.parameters = c("beta", "overall.sd"), file.jags = ma.normal.group.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE, 
            filename = paste(path, "Output/Figures/Convergence/normal_setting_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Setting", nrow(output)), 
                          NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), sum(subset.df$Group == 3), nrow(subset.df)),
                          Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
||||||| 2b1eac9
gh <- which(dat.main$ExperimentClass == "Greenhouse")
fd <- which(dat.main$ExperimentClass == "Field")
gdn <- which(dat.main$ExperimentClass == "Garden")
dat.main$Grp <- NA
dat.main$Grp[gh] <- 1
dat.main$Grp[fd] <- 2
dat.main$Grp[gdn] <- 3
grplabels <- c("Greenhouse", "Field", "Garden")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.set <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
                         g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"),
                         file.jags = ma_normal_grp)
  MCMCtrace(ma.set, excl = "deviance", pdf = TRUE, filename = paste("normal_setting_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.set)
  output.row <- data.frame(comparison = rep("Setting", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
print(paste("Normal meta-analysis for experiment setting is complete at ", Sys.time(), sep = ""))
#
||||||| 2b1eac9

=======
print(paste("Normal meta-analysis for experiment setting is complete at ", Sys.time(), sep = ""))

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
# GROUPED BY HERBACEOUS/WOODY ------------------------------------------------
#
<<<<<<< HEAD
hb <- which(meta.analysis.init$WoodyHerbaceous == "Herbaceous")
wd <- which(meta.analysis.init$WoodyHerbaceous == "Woody" | meta.analysis.init$WoodyHerbaceous == "Both")
meta.analysis.init$Group <- NA
meta.analysis.init$Group[hb] <- 1
meta.analysis.init$Group[wd] <- 2
group.labels <- c("Herbaceous", "Woody")
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                          observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                          num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, 
                          save.parameters = c("beta", "overall.sd"), file.jags = ma.normal.group.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE,
            filename = paste(path, "Output/Figures/Convergence/normal_herbwood_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Habit", nrow(output)),
                          NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)), 
                          Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
||||||| 2b1eac9
hb <- which(dat.main$WoodyHerbaceous == "Herbaceous")
wd <- which(dat.main$WoodyHerbaceous == "Woody" | dat.main$WoodyHerbaceous == "Both")
dat.main$Grp <- NA
dat.main$Grp[hb] <- 1
dat.main$Grp[wd] <- 2
grplabels <- c("Herbaceous", "Woody")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.hbwd <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
                         g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"),
                         file.jags = ma_normal_grp)
  MCMCtrace(ma.hbwd, excl = "deviance", pdf = TRUE, filename = paste("normal_herbwood_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.hbwd)
  output.row <- data.frame(comparison = rep("Habit", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
print(paste("Normal meta-analysis for growth habit is complete at ", Sys.time(), sep = ""))
#
||||||| 2b1eac9

=======
print(paste("Normal meta-analysis for growth habit is complete at ", Sys.time(), sep = ""))

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
# GROUPED BY JUVENILE/ADULT ------------------------------------------------
#
<<<<<<< HEAD
jv <- which(meta.analysis.init$PlantAge == "Juvenile")
ad <- which(meta.analysis.init$PlantAge == "Adult" | meta.analysis.init$PlantAge == "Both")
meta.analysis.init$Group <- NA
meta.analysis.init$Group[jv] <- 1
meta.analysis.init$Group[ad] <- 2
group.labels <- c("Juvenile", "Adult")
for (a in 1:length(metric.names)) {
  subset.df <- meta.analysis.init[which(meta.analysis.init$MetricName == metric.names[a]), ]
  subset.df <- subset.df[which(!is.na(subset.df$ObservedMean) & !is.na(subset.df$BootstrapVar)), ]
  meta.analysis.fit <- RunMetaAnalysis(num.observations = nrow(subset.df), observed.y = subset.df$ObservedMean, 
                          observed.var = subset.df$BootstrapVar, group.input = subset.df$Group,
                          num.group = length(unique(subset.df$Group)), parameter.inits = GenerateInitsGroupwise, 
                          save.parameters = c("beta", "overall.sd"), file.jags = ma.normal.group.file)
  MCMCtrace(meta.analysis.fit, excl = "deviance", pdf = TRUE, open_pdf = FALSE, 
            filename = paste(path, "Output/Figures/Convergence/normal_age_", metric.names[a], "_", rii.type.name, sep = ""))
  output <- ExtractJAGSOutput(meta.analysis.fit, groupwise = TRUE)
  output.add <- data.frame(ModelType = rep("Age", nrow(output)), 
                          NumCases = c(sum(subset.df$Group == 1), sum(subset.df$Group == 2), nrow(subset.df)), 
                          Distribution = rep("Normal", nrow(output)), output)
  meta.analysis.df <- rbind(meta.analysis.df, output.add)
||||||| 2b1eac9
jv <- which(dat.main$PlantAge == "Juvenile")
ad <- which(dat.main$PlantAge == "Adult" | dat.main$PlantAge == "Both")
dat.main$Grp <- NA
dat.main$Grp[jv] <- 1
dat.main$Grp[ad] <- 2
grplabels <- c("Juvenile", "Adult")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.age <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
                         g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"),
                         file.jags = ma_normal_grp)
  MCMCtrace(ma.age, excl = "deviance", pdf = TRUE, filename = paste("normal_age_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.age)
  output.row <- data.frame(comparison = rep("Age", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
print(paste("Normal meta-analysis for plant age is complete at ", Sys.time(), sep = ""))
#
# save output
meta.analysis.by.network <- meta.analysis.df[which(meta.analysis.df$Network != "All"), ]
rownames(meta.analysis.by.network) <- 1:nrow(meta.analysis.by.network)
meta.analysis.df <- meta.analysis.df[which(meta.analysis.df$Network == "All"), ]
meta.analysis.df <- meta.analysis.df[, -5]
rownames(meta.analysis.df) <- 1:nrow(meta.analysis.df)
write.csv(x = meta.analysis.by.network, file = paste(path, "Output/MetaAnalysis_ByNetwork_Normal_", rii.type.name, ".csv", sep = ""))
write.csv(x = meta.analysis.df, file = paste(path, "Output/MetaAnalysis_Normal_", rii.type.name, ".csv", sep = ""))
||||||| 2b1eac9



MA.networks <- MA[which(MA$network != "All"), ]
rownames(MA.networks) <- 1:nrow(MA.networks)

MA.str.ind <- MA[which(MA$network == "All" & MA$metric != "asymm"), ]
MA.str.ind$n <- NULL
MA.str.ind[which(MA.str.ind$param == "sigma" | MA.str.ind$param == "mu" | MA.str.ind$param == "Q"), "n"] <- net.num
MA.str.ind[which(MA.str.ind$param == "True ctrl"), "n"] <- 13
MA.str.ind[which(MA.str.ind$param == "Mono ctrl"), "n"] <- 17
MA.str.ind[which(MA.str.ind$param == "Grassland"), "n"] <- 19
MA.str.ind[which(MA.str.ind$param == "Other"), "n"] <- 11
MA.str.ind[which(MA.str.ind$param == "Greenhouse"), "n"] <- 12
MA.str.ind[which(MA.str.ind$param == "Field"), "n"] <- 7
MA.str.ind[which(MA.str.ind$param == "Garden"), "n"] <- 12
MA.str.ind[which(MA.str.ind$param == "Herbaceous"), "n"] <- 23
MA.str.ind[which(MA.str.ind$param == "Woody"), "n"] <- 9
MA.str.ind[which(MA.str.ind$param == "Juvenile"), "n"] <- 25
MA.str.ind[which(MA.str.ind$param == "Adult"), "n"] <- 7

MA.asy <- MA[which(MA$network == "All" & MA$metric == "asymm"), ]
MA.asy$n <- NULL
MA.asy[which(MA.asy$param == "sigma" | MA.asy$param == "mu" | MA.asy$param == "Q"), "n"] <- net.num - 1
MA.asy[which(MA.asy$param == "True ctrl"), "n"] <- 12
MA.asy[which(MA.asy$param == "Mono ctrl"), "n"] <- 17
MA.asy[which(MA.asy$param == "Grassland"), "n"] <- 19
MA.asy[which(MA.asy$param == "Other"), "n"] <- 10
MA.asy[which(MA.asy$param == "Greenhouse"), "n"] <- 12
MA.asy[which(MA.asy$param == "Field"), "n"] <- 7
MA.asy[which(MA.asy$param == "Garden"), "n"] <- 11
MA.asy[which(MA.asy$param == "Herbaceous"), "n"] <- 23
MA.asy[which(MA.asy$param == "Woody"), "n"] <- 8
MA.asy[which(MA.asy$param == "Juvenile"), "n"] <- 25
MA.asy[which(MA.asy$param == "Adult"), "n"] <- 6

MA.all <- rbind(MA.str.ind, MA.asy)
MA.all <- MA.all[, -4]
rownames(MA.all) <- 1:nrow(MA.all)
write.csv(x = MA.networks, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_networks_normal.csv")
write.csv(x = MA.all, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_normal.csv")
=======
print(paste("Normal meta-analysis for plant age is complete at ", Sys.time(), sep = ""))


meta.analysis.by.network <- meta.analysis.df[which(meta.analysis.df$Network != "All"), ]
rownames(meta.analysis.by.network) <- 1:nrow(meta.analysis.by.network)
meta.analysis.df <- meta.analysis.df[which(meta.analysis.df$Network == "All"), ]
meta.analysis.df <- meta.analysis.df[, -5]
rownames(meta.analysis.df) <- 1:nrow(meta.analysis.df)
write.csv(x = meta.analysis.by.network, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_ByNetwork_Normal_", rii.type.name, ".csv", sep = ""))
write.csv(x = meta.analysis.df, file = paste("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/MetaAnalysis_Normal_", rii.type.name, ".csv", sep = ""))
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae


