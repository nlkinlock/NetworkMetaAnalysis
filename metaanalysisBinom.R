# BAYESIAN RANDOM EFFECTS META-ANALYSIS
# BINOMIALLY DISTRIBUTED COVARIATES

#
# GRAND MEAN ------------------------------------------------
#
# load main data frame from combine_data.R
# means and replicate number for each metric
# convert to binomial effect size by multiplying proportion by replicate number and rounding
dat.main <- merged.df[which(merged.df$metric == "connect" | merged.df$metric == "RI"), ]
dat.main$n <- round(dat.main$Replicates, digits = 0)
dat.main$EffectSize <- round(dat.main$obs * dat.main$n, digits = 0)
metrics.ma <- c("connect", "RI")

MA.binom <- data.frame(comparison = character(), distribution = character(), metric = character(), network = character(), param = character(), est = numeric(), CI.l = numeric(), CI.u = numeric())
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$EffectSize)), ]
  ma.binom <- ma.jags.func(N.input = nrow(dat), n.input = dat$n, counts.input = dat$EffectSize,
                           param.inits = ma.inits, par.vec =  c("mu", "sigma", "p.hat"), file.jags = ma_binom)
  MCMCtrace(ma.binom, excl = "deviance", pdf = TRUE, filename = paste("binom_overall_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract(fit = ma.binom)
  output.row <- data.frame(comparison = rep("Grand mean", nrow(output)), distribution = rep("Binomial", nrow(output)), output)
  MA.binom <- rbind(MA.binom, output.row)  # store in overall dataframe with all metrics
}

#
# GROUPED BY EXPERIMENT TYPE ------------------------------------------------
#
dat.main$Grp <- as.factor(as.numeric(x = dat.main$CtrlTreatment))
grplabels <- c("Mono ctrl", "True ctrl")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$EffectSize)), ]
  ma.binom.exp <- ma.jags.func(N.input = nrow(dat), n.input = dat$n, counts.input = dat$EffectSize, group.input = dat$Grp,
                               g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma", "p.hat"), file.jags = ma_binom_grp)
  MCMCtrace(ma.binom.exp, excl = "deviance", pdf = TRUE, filename = paste("binom_experiment_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.binom.exp)
  output.row <- data.frame(comparison = rep("Exp type", nrow(output)), distribution = rep("Binomial", nrow(output)), output)
  MA.binom <- rbind(MA.binom, output.row)
}

#
# GROUPED BY OLD FIELD/GRASSLAND ------------------------------------------------
#
ind <- which(dat.main$Habitat == "Old field" | dat.main$Habitat == "Grassland")
dat.main$Grp <- NA
dat.main$Grp[ind] <- 1
dat.main$Grp[-ind] <- 2
grplabels <- c("Grassland", "Other")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$EffectSize)), ]
  ma.binom.hab <- ma.jags.func(N.input = nrow(dat), n.input = dat$n, counts.input = dat$EffectSize, group.input = dat$Grp, 
                               g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma", "p.hat"), file.jags = ma_binom_grp)
  MCMCtrace(ma.binom.hab, excl = "deviance", pdf = TRUE, filename = paste("binom_habitat_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.binom.hab)
  output.row <- data.frame(comparison = rep("Habitat", nrow(output)), distribution = rep("Binomial", nrow(output)), output)
  MA.binom <- rbind(MA.binom, output.row)
}

#
# GROUPED BY GREENHOUSE/FIELD/GARDEN ------------------------------------------------
#
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
  dat <- dat[which(!is.na(dat$EffectSize)), ]
  ma.binom.set <- ma.jags.func(N.input = nrow(dat), n.input = dat$n, counts.input = dat$EffectSize, group.input = dat$Grp,
                               g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma", "p.hat"), file.jags = ma_binom_grp)
  MCMCtrace(ma.binom.set, excl = "deviance", pdf = TRUE, filename = paste("binom_setting_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.binom.set)
  output.row <- data.frame(comparison = rep("Setting", nrow(output)), distribution = rep("Binomial", nrow(output)), output)
  MA.binom <- rbind(MA.binom, output.row)
}

#
# GROUPED BY HERBACEOUS/WOODY ------------------------------------------------
#
hb <- which(dat.main$WoodyHerbaceous == "Herbaceous")
wd <- which(dat.main$WoodyHerbaceous == "Woody" | dat.main$WoodyHerbaceous == "Both")
dat.main$Grp <- NA
dat.main$Grp[hb] <- 1
dat.main$Grp[wd] <- 2
grplabels <- c("Herbaceous", "Woody")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$EffectSize)), ]
  ma.binom.hbwd <- ma.jags.func(N.input = nrow(dat), n.input = dat$n, counts.input = dat$EffectSize, group.input = dat$Grp,
                          g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma", "p.hat"), file.jags = ma_binom_grp)
  MCMCtrace(ma.binom.hbwd, excl = "deviance", pdf = TRUE, filename = paste("binom_herbwood_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.binom.hbwd)
  output.row <- data.frame(comparison = rep("Habit", nrow(output)), distribution = rep("Binomial", nrow(output)), output)
  MA.binom <- rbind(MA.binom, output.row)
}

#
# GROUPED BY JUVENILE/ADULT ------------------------------------------------
#
jv <- which(dat.main$PlantAge == "Juvenile")
ad <- which(dat.main$PlantAge == "Adult" | dat.main$PlantAge == "Both")
dat.main$Grp <- NA
dat.main$Grp[jv] <- 1
dat.main$Grp[ad] <- 2
grplabels <- c("Juvenile", "Adult")
for (a in 1:length(metrics.ma)) {
  dat <- dat.main[which(dat.main$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$EffectSize)), ]
  ma.binom.age <- ma.jags.func(N.input = nrow(dat), n.input = dat$n, counts.input = dat$EffectSize, group.input = dat$Grp,
                          g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma", "p.hat"), file.jags = ma_binom_grp)
  MCMCtrace(ma.binom.age, excl = "deviance", pdf = TRUE, filename = paste("binom_age_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.binom.age)
  output.row <- data.frame(comparison = rep("Age", nrow(output)), distribution = rep("Binomial", nrow(output)), output)
  MA.binom <- rbind(MA.binom, output.row)
}


MA.binom.networks <- MA.binom[which(MA.binom$network != "All"), ]
rownames(MA.binom.networks) <- 1:nrow(MA.binom.networks)
MA.binom.all <- MA.binom[which(MA.binom$network == "All"), ]
MA.binom.all$n <- NULL
MA.binom.all[which(MA.binom.all$param == "sigma" | MA.binom.all$param == "mu" | MA.binom.all$param == "Q"), "n"] <- net.num
MA.binom.all[which(MA.binom.all$param == "True ctrl"), "n"] <- 13
MA.binom.all[which(MA.binom.all$param == "Mono ctrl"), "n"] <- 17
MA.binom.all[which(MA.binom.all$param == "Grassland"), "n"] <- 19
MA.binom.all[which(MA.binom.all$param == "Other"), "n"] <- 11
MA.binom.all[which(MA.binom.all$param == "Greenhouse"), "n"] <- 12
MA.binom.all[which(MA.binom.all$param == "Field"), "n"] <- 7
MA.binom.all[which(MA.binom.all$param == "Garden"), "n"] <- 12
MA.binom.all[which(MA.binom.all$param == "Herbaceous"), "n"] <- 23
MA.binom.all[which(MA.binom.all$param == "Woody"), "n"] <- 9
MA.binom.all[which(MA.binom.all$param == "Juvenile"), "n"] <- 25
MA.binom.all[which(MA.binom.all$param == "Adult"), "n"] <- 7
MA.binom.all <- MA.binom.all[, -4]
rownames(MA.binom.all) <- 1:nrow(MA.binom.all)
write.csv(x = MA.binom.networks, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_networks_binom.csv")
write.csv(x = MA.binom.all, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_binom.csv")


