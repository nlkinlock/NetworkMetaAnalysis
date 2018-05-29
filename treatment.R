# COMPARE NETWORK METRICS BETWEEN NETWORKS WITH DIFFERENT TREATMENTS
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD TREATMENT NETWORKS AND COMBINE W/ CODING TABLE -------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
treatments <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Networks/Output/allMetrics_Treatments.csv", row.names = 1)
treatments <- data.frame(treatments, SD.l = treatments$obs - treatments$sd, SD.u = treatments$obs + treatments$sd)
treatments$var <- treatments$sd^2
UniqueID <- c()
for (z in 1:length(treatments$network)) {
  if (substring(treatments$network[z], 5, 5) == "_") {
    UniqueID[z] <- as.integer(substring(treatments$network[z], 1, 4))
  } else if (substring(treatments$network[z], 2, 2) == "_") {
    UniqueID[z] <- as.integer(substring(treatments$network[z], 1, 1))
  } else {
    UniqueID[z] <- as.integer(substring(treatments$network[z], 1, 3))
  }
}
treatments$UniqueID <- as.factor(UniqueID)
treatments <- treatments[which(treatments$metric != "r"), ]
treatments$metric <- factor(treatments$metric, levels = c("strength", "indirect_effect", "asymm_diff", "relative_intransitivity", "connectance"))
levels(treatments$metric) <- c("strength", "ind eff", "asymm", "RI", "connect")
# combine coding table and bootstrap output
treat.df <- join_all(list(treatments, df), by = "UniqueID")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# META-ANALYSES OF TREATMENT NETWORKS -----------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# meta-analyses necessary for network-specific estimates (y hats)

# load df with only normally distributed metrics
dat.treat <- treat.df[which(treat.df$metric == "strength" | treat.df$metric == "asymm" | treat.df$metric == "ind eff"), ]
zeros <- which(dat.treat$var == 0)
dat.treat$var[zeros] <- dat.treat$var[zeros] + 1E-9
metrics.ma <- c("strength", "asymm", "ind eff")
MA <- data.frame(comparison = character(), distribution = character(), metric = character(), network = character(), param = character(), est = numeric(), CI.l = numeric(), CI.u = numeric())
for (a in 1:length(metrics.ma)) {
  dat <- dat.treat[which(dat.treat$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$obs)), ]
  ma.norm <- ma.jags.func(k.input = nrow(dat), y.input = dat$obs, v.input = dat$var, param.inits = ma.inits, par.vec =  c("y.hat", "mu", "sigma"), file.jags = ma_normal)
  MCMCtrace(ma.norm, excl = "deviance", pdf = TRUE, filename = paste("normal_overall_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence")
  output <- jags.extract(fit = ma.norm)
  output.row <- data.frame(comparison = rep("Grand mean", nrow(output)), distribution = rep("Normal", nrow(output)), output)
  MA <- rbind(MA, output.row)  # store in overall dataframe with all metrics
}
MA.treat.networks <- MA[which(MA$network != "All"), ]
rownames(MA.treat.networks) <- 1:nrow(MA.treat.networks)

# load df with only binomially distributed metrics
dat.treat <- treat.df[which(treat.df$metric == "connect" | treat.df$metric == "RI"), ]
dat.treat$n <- round(dat.treat$Replicates, digits = 0)
dat.treat$EffectSize <- round(dat.treat$obs * dat.treat$n, digits = 0)
metrics.ma <- c("connect", "RI")
MA.binom <- data.frame(comparison = character(), distribution = character(), metric = character(), network = character(), param = character(), est = numeric(), CI.l = numeric(), CI.u = numeric())
for (a in 1:length(metrics.ma)) {
  dat <- dat.treat[which(dat.treat$metric == metrics.ma[a]), ]
  dat <- dat[which(!is.na(dat$EffectSize)), ]
  ma.binom <- ma.jags.func(N.input = nrow(dat), n.input = dat$n, counts.input = dat$EffectSize,
                           param.inits = ma.inits, par.vec =  c("mu", "sigma", "p.hat"), file.jags = ma_binom)
  MCMCtrace(ma.binom, excl = "deviance", pdf = TRUE, filename = paste("binom_overall_", metrics.ma[a], sep = ""), wd = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Convergence")
  output <- jags.extract(fit = ma.binom)
  output.row <- data.frame(comparison = rep("Grand mean", nrow(output)), distribution = rep("Binomial", nrow(output)), output)
  MA.binom <- rbind(MA.binom, output.row)  # store in overall dataframe with all metrics
}
MA.binom.treat.networks <- MA.binom[which(MA.binom$network != "All"), ]
rownames(MA.binom.treat.networks) <- 1:nrow(MA.binom.treat.networks)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# COMBINE TREATMENT AND CONTROL NETWORKS --------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# control networks
dat.post.norm <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_networks_normal.csv", row.names = 1)
dat.post.binom <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_networks_binom.csv", row.names = 1)
dat.post <- rbind(dat.post.norm, dat.post.binom)  # meta-analysis estimates of metrics for each network
dat.post$est.post <- dat.post$est
dat.post$CI.l.post <- dat.post$CI.l
dat.post$CI.u.post <- dat.post$CI.u
dat.post <- dat.post[, -c(1, 2, 5, 6, 7, 8)]
# subset only networks with treatments
ctrl.post <- dat.post[which(dat.post$network =="1448_Gurevitch_1990" | dat.post$network =="1787_Weigelt_2002" | dat.post$network =="1454_PfeiferMeister_2008" | dat.post$network =="1566_Niu_2008" | dat.post$network =="192_Amanullah_2013" | dat.post$network =="236_Jiang_2014"), ]
ctrl.post$treatment <- rep("Control", nrow(ctrl.post))
ctrl.post$treatmentType <- rep("Control", nrow(ctrl.post))

# treatment networks
treat.post <- rbind(MA.treat.networks, MA.binom.treat.networks)
treat.post$est.post <- treat.post$est
treat.post$CI.l.post <- treat.post$CI.l
treat.post$CI.u.post <- treat.post$CI.u
treat.post <- treat.post[, -c(1, 2, 5, 6, 7, 8)]
treat.post$treatment <- rep("Treatment", nrow(treat.post))
treat.post$treatmentType <- NA
treat.post$treatmentType[which(treat.post$network == "1448_Gurevitch_1990_Fert")] <- "Increased nutrients"
treat.post$treatmentType[which(treat.post$network == "1454_PfeiferMeister_2008_LowNutrient")] <- "Decreased nutrients"
treat.post$treatmentType[which(treat.post$network == "1566_Niu_2008_Warm")] <- "Warming"
treat.post$treatmentType[which(treat.post$network == "1787_Weigelt_2002_LowWater")] <- "Decreased water"
treat.post$treatmentType[which(treat.post$network == "192_Amanullah_2013_LW")] <- "Decreased water"
treat.post$treatmentType[which(treat.post$network == "236_Jiang_2014_N")] <- "Increased nutrients"
treat.post$treatmentType[which(treat.post$network == "236_Jiang_2014_Water")] <- "Increased water"
treat.post$network_b <- NA
treat.post$network_b[which(treat.post$network == "1448_Gurevitch_1990_Fert")] <- "1448_Gurevitch_1990"
treat.post$network_b[which(treat.post$network == "1454_PfeiferMeister_2008_LowNutrient")] <- "1454_PfeiferMeister_2008"
treat.post$network_b[which(treat.post$network == "1566_Niu_2008_Warm")] <- "1566_Niu_2008"
treat.post$network_b[which(treat.post$network == "1787_Weigelt_2002_LowWater")] <- "1787_Weigelt_2002"
treat.post$network_b[which(treat.post$network == "192_Amanullah_2013_LW")] <- "192_Amanullah_2013"
treat.post$network_b[which(treat.post$network == "236_Jiang_2014_N")] <- "236_Jiang_2014"
treat.post$network_b[which(treat.post$network == "236_Jiang_2014_Water")] <- "236_Jiang_2014"
treat.post <- treat.post[, -2]
colnames(treat.post)[7] <- "network"
ctrl.treat.init <- rbind(ctrl.post, treat.post)
ctrl.treat <- join_all(dfs = list(ctrl.treat.init, metrics), by = c("network", "metric"))
ctrl.treat$network <- factor(ctrl.treat$network)
levels(ctrl.treat$network) <- c("Gurevitch et al. 1990", "Pfeifer-Meister et al. 2008", "Niu and Wan 2008",  "Weigelt et al. 2002", "Amanullah 2013", "Jiang et al. 2014")
ctrl.treat$network <- factor(ctrl.treat$network, levels = c("Niu and Wan 2008", "Pfeifer-Meister et al. 2008", "Amanullah 2013", "Jiang et al. 2014", "Gurevitch et al. 1990", "Weigelt et al. 2002"))

write.csv(ctrl.treat, file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/ctrl_treatment.csv")


