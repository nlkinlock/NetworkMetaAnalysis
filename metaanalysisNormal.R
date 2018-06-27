# BAYESIAN RANDOM EFFECTS META-ANALYSIS
# NORMALLY DISTRIBUTED COVARIATES

#
# GRAND MEAN ------------------------------------------------
#
# load main data frame from combine_data.R
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
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.hab <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
                         g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"), file.jags = ma_normal_grp)
  MCMCtrace(ma.hab, excl = "deviance", pdf = TRUE, filename = paste("normal_habitat_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.hab)
  output.row <- data.frame(comparison = rep("Habitat", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
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
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.set <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
                         g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"),
                         file.jags = ma_normal_grp)
  MCMCtrace(ma.set, excl = "deviance", pdf = TRUE, filename = paste("normal_setting_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.set)
  output.row <- data.frame(comparison = rep("Setting", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
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
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.hbwd <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
                         g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"),
                         file.jags = ma_normal_grp)
  MCMCtrace(ma.hbwd, excl = "deviance", pdf = TRUE, filename = paste("normal_herbwood_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.hbwd)
  output.row <- data.frame(comparison = rep("Habit", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
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
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.age <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, group.input = dat$Grp,
                         g.input = length(unique(dat$Grp)), param.inits = ma.grp.inits, par.vec = c("beta", "sigma"),
                         file.jags = ma_normal_grp)
  MCMCtrace(ma.age, excl = "deviance", pdf = TRUE, filename = paste("normal_age_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence", open_pdf = FALSE)
  output <- jags.extract.grp(ma.age)
  output.row <- data.frame(comparison = rep("Age", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)
}



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


