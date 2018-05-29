# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INITIALIZE ------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# inputs for figure params
#
# set file type, 1 = eps, other = tiff
file.type <- 1
# function to set limits given min and max
lim.func <- function(df) {
  range.lim <- abs(max(df$CI.u, na.rm = TRUE) - min(df$CI.l, na.rm = TRUE))
  min.lim <- min(df$CI.l, na.rm = TRUE) - (0.1 * range.lim)
  max.lim <- max(df$CI.u, na.rm = TRUE) + (0.2 * range.lim)
  lab.lim <- max(df$CI.u, na.rm = TRUE) + (0.1 * range.lim)
  return(c(min.lim, max.lim, lab.lim))
}
lim.func.for <- function(df) {
  range.lim <- abs(max(df$CI.u.post, na.rm = TRUE) - min(df$CI.l.post, na.rm = TRUE))
  min.lim <- min(df$CI.l.post, na.rm = TRUE) - (0.1 * range.lim)
  max.lim <- max(df$CI.u.post, na.rm = TRUE) + (0.1 * range.lim)
  return(c(min.lim, max.lim))
}
# height and width of saved EPS files for meta-analysis results (.ma), forest plots (.fp), and model-selection distributions (.ms)
width.ma <- 3.5
height.ma <- 5
width.fp <- 5
height.fp <- 5
width.ms <- 7
height.ms <- 6
# toggle sizes, shapes, and line widths for .ma .fp and .ms
ma.sizes <- c(5, 2, 2, 2, 2, 2)
ma.shapes <- c(18, 19, 19, 19, 19, 19)
ma.text <- 2.75
ma.lwd <- 0.6
ma.theme <- theme(legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "none", 
      axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title = element_text(size = 12), 
      axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 12), strip.background = element_blank(),
      panel.background = element_blank(), panel.spacing = unit(0.4, "cm"))
fp.lwd <- 0.6
fp.shape <- c(19, 15)
fp.theme <- theme(legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "none", 
      axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 10), axis.title = element_text(size = 12), 
      axis.line = element_line(size = 0.5))
ms.theme <- theme(axis.title = element_text(size = 10), axis.text = element_text(size = 6), axis.line = element_line(size = 0.5),
                  legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "bottom",
                  strip.placement = "outside", strip.text = element_text(size = 8), strip.background = element_blank(),
                  panel.background = element_blank(), panel.spacing = unit(0.1, "cm"))

pd <- position_dodge(1)
pd.less <- position_dodge(0.5)
gray.hex <- "#6d6d6d"
tiff.res <- 600

#
# load data
#
# estimates by network (for forest plots)
# meta-analytic data (grand means and groups)
final.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/codingtable_metrics.csv", row.names = 1)
final.df$network <- factor(final.df$network, levels = c("Sangakkara and Roberts 1985", "Miller and Werner 1987", "Bush and Van Auken 2004", "Frerot et al. 2006",
                                                    "Chacon and Munoz 2007", "Dehlin et al. 2008", "Engel and Weltzin 2008", "Niu and Wan 2008", 
                                                    "Pfeifer-Meister et al. 2008", "Marty et al. 2009", "Baude et al. 2011", "Mariotte et al. 2012",
                                                    "Amanullah 2013", "Pausch et al. 2013", "Jiang et al. 2014", "Cuda et al. 2015", 
                                                    "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg and Landa 1991", "Weigelt et al. 2002",
                                                    "Costa et al. 2003", "Hedberg et al. 2005", "Fortner and Weltzin 2007", "Domenech and Vila 2008",
                                                    "Svenning et al. 2008", "Saccone et al. 2010", "Armas and Pugnaire 2011", "Farrer and Goldberg 2011",
                                                    "Gao et al. 2014", "Lof et al. 2014", "Kinlock unpublished", "Kinlock unpublished b"))
meta <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_output.csv", row.names = 1)
meta$comparison <- factor(meta$comparison, levels = c("Grand mean", "Exp type", "Setting", "Habitat", "Habit", "Age"))
meta$param <- factor(meta$param, levels = c("mu", "sigma", "Q", "True ctrl", "Mono ctrl", "Other", "Grassland", "Field", "Garden", "Greenhouse", "Woody", "Herbaceous", "Adult", "Juvenile"))



# model selection for weight, in- and out-strength
df.w <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/weight_distsamples.csv", row.names = 1)
df.is <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/instr_distsamples.csv", row.names = 1)
df.os <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/outstr_distsamples.csv", row.names = 1)
df.w$Distribution <- factor(df.w$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal",  "Pareto"))
df.w$Case <- factor(df.w$Case, levels = c("Sangakkara and Roberts 1985", "Miller and Werner 1987", "Bush and Van Auken 2004", "Frerot et al. 2006",
                                                    "Chacon and Munoz 2007", "Dehlin et al. 2008", "Engel and Weltzin 2008", "Niu and Wan 2008", 
                                                    "Pfeifer-Meister et al. 2008", "Marty et al. 2009", "Baude et al. 2011", "Mariotte et al. 2012",
                                                    "Amanullah 2013", "Pausch et al. 2013", "Jiang et al. 2014", "Cuda et al. 2015", 
                                                    "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg and Landa 1991", "Weigelt et al. 2002",
                                                    "Costa et al. 2003", "Hedberg et al. 2005", "Fortner and Weltzin 2007", "Domenech and Vila 2008",
                                                    "Svenning et al. 2008", "Saccone et al. 2010", "Armas and Pugnaire 2011", "Farrer and Goldberg 2011",
                                                    "Gao et al. 2014", "Lof et al. 2014", "Kinlock unpublished", "Kinlock unpublished b"))
df.is$Distribution <- factor(df.is$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
df.is$Case <- factor(df.is$Case, levels = c("Miller and Werner 1987", "Engel and Weltzin 2008", "Niu and Wan 2008", "Mariotte et al. 2012",
                                                    "Jiang et al. 2014", "Goldberg and Landa 1991", "Svenning et al. 2008",
                                                    "Armas and Pugnaire 2011", "Lof et al. 2014", "Kinlock unpublished", "Kinlock unpublished b"))
df.os$Distribution <- factor(df.os$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
df.os$Case <- factor(df.os$Case, levels = c("Miller and Werner 1987", "Engel and Weltzin 2008", "Niu and Wan 2008", "Mariotte et al. 2012",
                                            "Jiang et al. 2014", "Goldberg and Landa 1991", "Svenning et al. 2008",
                                            "Armas and Pugnaire 2011", "Lof et al. 2014", "Kinlock unpublished", "Kinlock unpublished b"))

# pca scores and loadings
df.scores <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/pca_scores.csv", row.names = 1)
df.loadings <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/pca_loadings.csv", row.names = 1)

# comparing invasive/native, C3/C4, and N-fixing spp.
df.char <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_character_diff.csv", row.names = 1)
df.char$comparison <- factor(df.char$comparison, levels = c("Invasive status", "C4 photosynthesis", "N-fixing ability"))

# comparing spp. position
df.spp <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_position.csv", row.names = 1)
levels(df.spp$network) <- c("Sangakkara and Roberts 1985", "Gurevitch et al. 1990", "Mariotte et al. 2012", 
                            "Fortner and Weltzin 2007", "Hendriks et al. 2015", "Miller and Werner 1987", 
                            "Engel and Weltzin 2008", "Goldberg and Landa 1991")
df.spp$network <- factor(df.spp$network, levels = c("Sangakkara and Roberts 1985", "Miller and Werner 1987", "Engel and Weltzin 2008", "Mariotte et al. 2012",
                                                    "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg and Landa 1991", "Fortner and Weltzin 2007"))
levels(df.spp$species) <- c("Dactylis glomerata", "Plantago lanceolata", "Trifolium repens")

# comparing metrics from networks wth treatment/control
df.treat <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/ctrl_treatment.csv", row.names = 1)
df.treat$treatmentType <- factor(df.treat$treatmentType, levels = c("Control", "Decreased nutrients", "Increased nutrients", "Decreased water", "Increased water", "Warming"))
df.treat$metric <- factor(df.treat$metric, levels = c("strength", "ind eff", "asymm", "RI", "connect"))

# comparing standardized differences in RII in pairwise and 3 spp. combinations
df.add <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/additivity.csv", row.names = 1)
df.add$param <- factor(df.add$param, levels = c("grand mean", "group mean", "among-group sd", "within-group sd"))
levels(df.add$network) <- c("Baude et al. 2011", "Pausch et al. 2013", "Marty et al. 2009", "Frerot et al. 2006", "All")
df.add$network <- factor(df.add$network, levels = c("All", "Frerot et al. 2006", "Marty et al. 2009", "Baude et al. 2011", "Pausch et al. 2013"))

world <- map_data("world")
worldmap <- ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "gray95", color = "gray65", size = 0.2) + coord_fixed(1.3) + 
  labs(x = "Longitude", y = "Latitude") + 
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
tiff(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/world.tiff", res = 300, units = "in", width = 4, height = 2.5)
worldmap + geom_point(data = df, aes(x = Longitude, y = Latitude), shape = 19, size = 0.5)
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ESTIMATES FROM META-ANALYSIS ------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# mean strength
sub.df <- meta[which(meta$metric == "strength" & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
df.lim <- lim.func(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/StrengthResults.eps", width = width.ma, height = height.ma)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/StrengthResults.tiff", res = tiff.res, units = "in", width = width.ma, height = height.ma)
  
}
ggplot(data = sub.df) + facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = param, y = est, shape = comparison, size = comparison)) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = df.lim[3]), size = ma.text) +
  coord_flip(ylim = c(df.lim[1], df.lim[2])) +  
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  labs(x = "", y = "Mean strength") + theme_classic() + ma.theme
dev.off()

# indirect effect
sub.df <- meta[which(meta$metric == "ind eff" & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
df.lim <- lim.func(sub.df)
if (file.type == 1) {
postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/IndEffResults.eps", width = width.ma, height = height.ma)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/IndEffResults.tiff", res = tiff.res, units = "in", width = width.ma, height = height.ma)
}
ggplot(data = sub.df) + facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = param, y = est, shape = comparison, size = comparison)) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = df.lim[3]), size = ma.text) +
  coord_flip(ylim = c(df.lim[1], df.lim[2])) + 
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  labs(x = "", y = "Mean weighted strength (WI2)") + theme_classic() + ma.theme
dev.off()

# asymmetry difference
sub.df <- meta[which(meta$metric == "asymm" & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
df.lim <- lim.func(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/AsymmetryDiffResults.eps", width = width.ma, height = height.ma)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/AsymmetryDiffResults.tiff", res = tiff.res, units = "in", width = width.ma, height = height.ma)
}
ggplot(data = sub.df)  + facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = param, y = est, shape = comparison, size = comparison)) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = df.lim[3]), size = ma.text) +
  coord_flip(ylim = c(df.lim[1], df.lim[2])) + 
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  labs(x = "", y = "Interaction asymmetry") + theme_classic() + ma.theme
dev.off()

# transitivity
sub.df <- meta[which(meta$metric == "RI"  & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
df.lim <- lim.func(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/TransitivityResults.eps", width = width.ma, height = height.ma)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/TransitivityResults.tiff", res = tiff.res, units = "in", width = width.ma, height = height.ma)
}
ggplot(data = sub.df) + 
  facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd, position = pd) + 
  geom_point(aes(x = param, y = est, size = comparison, shape = comparison), position = pd) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = df.lim[3]), size = ma.text) +
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  coord_flip(ylim = c(df.lim[1], df.lim[2])) + 
  labs(x = "", y = "Relative intransitivity") + theme_classic() + ma.theme
dev.off()

# connectance
sub.df <- meta[which(meta$metric == "connect"  & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
df.lim <- lim.func(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/ConnectanceResults.eps", width = width.ma, height = height.ma)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/ConnectanceResults.tiff", res = tiff.res, units = "in", width = width.ma, height = height.ma)
}
ggplot(data = sub.df) + 
  facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = param, y = est, size = comparison, shape = comparison)) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = df.lim[3]), size = ma.text) +
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  coord_flip(ylim = c(df.lim[1], df.lim[2])) + 
  labs(x = "", y = "Weighted connectance") + theme_classic() + ma.theme
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FOREST PLOTS ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# strength
sub.df <- final.df[which(final.df$metric == "strength"), ]
df.lim <- lim.func.for(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/StrengthForest.eps", width = width.fp, height = height.fp)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/StrengthForest.tiff", res = tiff.res, units = "in", width = width.fp, height = height.fp)
}
ggplot(data = sub.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Mean strength") + theme_classic() + 
  scale_shape_manual(values = fp.shape) + fp.theme +
  scale_y_continuous(limits = c(df.lim[1], df.lim[2])) + coord_flip()
dev.off()

# indirect effect
sub.df <- final.df[which(final.df$metric == "ind eff"), ]
df.lim <- lim.func.for(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/IndEffForest.eps", width = width.fp, height = height.fp)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/IndEffForest.tiff", res = tiff.res, units = "in", width = width.fp, height = height.fp)
}
ggplot(data = sub.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Mean weighted strength (WI2)") + theme_classic() + fp.theme + 
  scale_shape_manual(values = fp.shape) + scale_y_continuous(limits = c(df.lim[1], df.lim[2])) + coord_flip()
dev.off()

# asymmetry difference
sub.df <- final.df[which(final.df$metric == "asymm"), ]
df.lim <- lim.func.for(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/AsymmetryDiffForest.eps", width = width.fp, height = height.fp)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/AsymmetryDiffForest.tiff", res = tiff.res, units = "in", width = width.fp, height = height.fp)
}
ggplot(data = sub.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Interaction asymmetry") + theme_classic() + fp.theme +
  scale_shape_manual(values = c(19, 15)) + scale_y_continuous(limits = c(df.lim[1], df.lim[2])) + 
  coord_flip()
dev.off()

#transitivity
sub.df <- final.df[which(final.df$metric == "RI"), ]
df.lim <- lim.func.for(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/TransitivityForest.eps", width = width.fp, height = height.fp)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/TransitivityForest.tiff", res = tiff.res, units = "in", width = width.fp, height = height.fp)
}
ggplot(data = sub.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Relative intransitivity") + theme_classic() + fp.theme +
  scale_shape_manual(values = fp.shape) + scale_y_continuous(limits = c(df.lim[1], df.lim[2])) + coord_flip()
dev.off()

# connectance
sub.df <- final.df[which(final.df$metric == "connect"), ]
df.lim <- lim.func.for(sub.df)
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/ConnectanceForest.eps", width = width.fp, height = height.fp)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/ConnectanceForest.tiff", res = tiff.res, units = "in", width = width.fp, height = height.fp)
}
ggplot(data = sub.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Weighted connectance") + theme_classic() + fp.theme +
  scale_shape_manual(values = fp.shape) + scale_y_continuous(limits = c(df.lim[1], df.lim[2])) + coord_flip()
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PCA BIPLOT ------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/PCA.eps", width = 5, height = 5)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/PCA.tiff", res = tiff.res, units = "in", width = 5, height = 5)
  
}
par(mar = c(3, 3, 2, 2), # Dist' from plot to side of page
    mgp = c(2, 0.4, 0), # Dist' plot to label
    las = 1, # Rotate y-axis text
    tck = -0.01, # Reduce tick length
    xaxs = "i", yaxs = "i") # Remove plot padding
palette(c("black", "#e41a1c"))
biplot(x = df.scores[, 2:3], y = df.loadings[, 2:3], xlabs = df.scores[, 1], ylabs = df.loadings[, 1],
       xlab = paste("PC1 (", pc1.var, "% var. explained)", sep = ""), ylab = paste("PC1 (", pc2.var, "% var. explained)", sep = ""), 
       arrow.len = 0.06, xlim = c(-4.5, 4), ylim = c(-4, 4.5), cex = 0.6)
abline(h = 0, lty = 2, col = "#6d6d6d")
abline(v = 0, lty = 2, col = "#6d6d6d")
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MODEL SELECTION FIGURES -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# figures for weight
# plot distributions of observed weights and fitted weights for each network
if (file.type == 1) {
  cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/weight_dist.eps", family = "sans", width = 10, height = height.ms)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/weight_dist.tiff", res = tiff.res, units = "in", width = 10, height = height.ms)
}
  ggplot(dat = df.w[which(df.w$Values < 1), ], aes(x = Values, fill = Distribution, alpha = Fit)) + 
  facet_wrap(~ Case, scales = "free", nrow = 10) + geom_density(size = 0.1) + 
  scale_fill_manual(values = c("#a2a2a2", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_alpha_manual(values = c(1.0, 0.5, 0.1), guide = FALSE) +
  theme_classic() + ms.theme
dev.off()
# plot CDFs pf observed weights and fitted weights, on log-log scale to observe tail behavior
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/weight_cdf.eps", width = 10, height = height.ms)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/weight_cdf.tiff", units = "in", width = 10, height = height.ms, res = tiff.res)
}
ggplot(dat = df.w, aes(x = Values, color = Distribution, size = Fit, linetype = Fit)) + facet_wrap(~ Case, scales = "free", nrow = 4) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(0.6, 0.6, 0.3), guide = FALSE) + 
  scale_linetype_manual(values = c("longdash", "solid", "dotted"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(scaled = TRUE, long = unit(1.5, "mm"), mid = unit(0.5, "mm"), short = unit(NA, "mm")) +
  labs(x = "Weights", y = "complementary CDF") + theme_classic() + ms.theme
dev.off()


# figures for in-strength
# density plots of data for insets
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/miller_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Miller and Werner 1987"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-1.1, 2.6)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/engel_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Engel and Weltzin 2008"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-2, 6.7)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/armas_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Armas and Pugnaire 2011"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.6, 1.4)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/svenning_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Svenning et al. 2008"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.2, 1)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/niu_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Niu and Wan 2008"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.1, 1)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/mariotte_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Mariotte et al. 2012"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-1.2, 5)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/jiang_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Jiang et al. 2014"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.5, 1.2)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/lof_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Lof et al. 2014"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.1, 0.4)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/goldberg_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Goldberg and Landa 1991"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(0, 6)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/kinlockb_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Kinlock unpublished"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-1, 4)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/kinlock_is_dist.eps", width = 1, height = 1)
ggplot(dat = df.is[which(df.is$Fit == "data" & df.is$Case == "Kinlock unpublished b"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-1.2, 5.8)) +
  theme_classic() + ms.theme
dev.off()
# CDF plot
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/instr_cdf.eps", width = width.ms, height = height.ms)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/instr_cdf.tiff", units = "in", width = width.ms, height = height.ms, res = 600)
}
ggplot(dat = df.is, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", nrow = 3) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dotted"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(scaled = TRUE, long = unit(1.5, "mm"), mid = unit(0.5, "mm"), short = unit(NA, "mm")) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + ms.theme
dev.off()

# figures for out-strength
# density plots of data for insets
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/miller_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Miller and Werner 1987"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-1.1, 2.6)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/engel_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Engel and Weltzin 2008"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-2, 6.7)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/armas_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Armas and Pugnaire 2011"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.6, 1.4)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/svenning_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Svenning et al. 2008"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.2, 1)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/niu_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Niu and Wan 2008"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.1, 1)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/mariotte_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Mariotte et al. 2012"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-1.2, 5)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/jiang_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Jiang et al. 2014"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.5, 1.2)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/lof_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Lof et al. 2014"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-0.1, 0.4)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/goldberg_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Goldberg and Landa 1991"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(0, 6)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/kinlockb_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Kinlock unpublished"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-1, 4)) +
  theme_classic() + ms.theme
dev.off()
cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/kinlock_os_dist.eps", width = 1, height = 1)
ggplot(dat = df.os[which(df.os$Fit == "data" & df.os$Case == "Kinlock unpublished b"), ], aes(x = Values)) + 
  geom_density(size = 0.1, fill = c("#a2a2a2")) + 
  scale_x_continuous(limits = c(-1.2, 5.8)) +
  theme_classic() + ms.theme
dev.off()
# CDF plot
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/outstr_cdf.eps", width = width.ms, height = height.ms)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/outstr_cdf.tiff", units = "in", width = width.ms, height = height.ms, res = tiff.res)
}
ggplot(dat = df.os, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", nrow = 3) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dotted"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(scaled = TRUE, long = unit(1.5, "mm"), mid = unit(0.5, "mm"), short = unit(NA, "mm")) +
  labs(x = "Out-strength", y = "complementary CDF") + theme_classic() + ms.theme
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# COMPARE SPP. CHARACTERS FIGURE ----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/sppCharDiff.eps", width = 4, height = 4)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/sppCharDiff.tiff", width = 4, height = 4, units = "in", res = tiff.res)
}
ggplot(dat = df.char[which(df.char$metric == "In-strength" | df.char$metric == "Out-strength"), ]) + 
  facet_wrap(~ metric, scales = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = comparison, ymin = CILL, ymax = CIUL), width = 0.2, size = fp.lwd) + 
  geom_point(aes(x = comparison, y = mean)) + labs(x = "", y = "Difference in strength")  + theme_classic() + ma.theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# COMPARE TREATMENTS ----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/networkTreatments.eps", width = 8, height = 5)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/networkTreatments.tiff", width = 8, height = 5, units = "in", res = tiff.res)
}
  ggplot(dat = df.treat) + facet_wrap(~ network, scales = "free", nrow = 2) +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = metric, ymin = CI.l.post, ymax = CI.u.post, col = treatmentType), position = pd.less, width = 0.2, size = fp.lwd) + 
  geom_point(aes(x = metric, y = est.post, col = treatmentType), size = 0.8, position = pd.less) + 
  scale_color_manual(values = c("#000000", "#b2df8a", "#33a02c", "#a6cee3", "#1f78b4", "#e31a1c")) + 
  labs(x = "", y = "") + theme_classic() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1), axis.text.y = element_text(size = 10), axis.title = element_text(size = 12), 
        axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 11), strip.background = element_blank(),
        panel.background = element_blank(), panel.spacing = unit(0.3, "cm"))
dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SPP. POSITION FIGURE -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/sppPosition.eps", width = 5, height = 5)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/sppPosition.tiff", width = 5, height = 5, units = "in", res = tiff.res)
}
ggplot(dat = df.spp) + facet_grid(metric ~ species, scales = "free_x") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.L, ymax = CI.U, col = network), width = 0.2, size = fp.lwd) + 
  geom_point(aes(x = network, y = mean, col = network)) + labs(x = "", y = "Strength")  + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628", "#f781bf", "#999999"), guide = guide_legend(nrow = 4)) +
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "bottom", 
        axis.text.x = element_blank(), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12), 
        axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 12), 
        strip.background = element_blank(), panel.background = element_blank(), panel.spacing = unit(0.5, "cm"))
dev.off()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ADDITIVITY FIGURE -----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/additivity.eps", width = 3.5, height = 3)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/additivity.tiff", width = 3.5, height = 3, units = "in", res = tiff.res)
}
ggplot(dat = df.add[which(df.add$param == "grand mean" | df.add$param == "group mean" ), ]) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = network, y = est, shape = param, size = param)) + 
  scale_size_manual(values = ma.sizes) + scale_shape_manual(values = ma.shapes) +
  labs(x = "", y = "Standardized mean difference in RII") + theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.position = "none", 
        axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 9), axis.title = element_text(size = 11), 
        axis.line = element_line(size = 0.5)) + coord_flip()
dev.off()



