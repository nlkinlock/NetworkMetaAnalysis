# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# INPUT VARIABLES -------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#

# inputs for figure params
#
# set file type, 1 = eps, other = tiff
file.type <- 1
# function to set limits given min and max
SetAxisLimits <- function(df, is.cases = FALSE) {
  if (is.cases == TRUE) {
    range.lim <- abs(max(df$MetaAnalysisCIU, na.rm = TRUE) - min(df$MetaAnalysisCIL, na.rm = TRUE))
    min.lim <- min(df$MetaAnalysisCIL, na.rm = TRUE) - (0.1 * range.lim)
    max.lim <- max(df$MetaAnalysisCIU, na.rm = TRUE) + (0.1 * range.lim)
    return(c(min.lim, max.lim))
  } else {
    range.lim <- abs(max(df$MetaAnalysisCIL, na.rm = TRUE) - min(df$MetaAnalysisCIL, na.rm = TRUE))
    min.lim <- min(df$MetaAnalysisCIL, na.rm = TRUE) - (0.1 * range.lim)
    max.lim <- max(df$MetaAnalysisCIU, na.rm = TRUE) + (0.2 * range.lim)
    lab.lim <- max(df$MetaAnalysisCIU, na.rm = TRUE) - (0.2 * range.lim)
    return(c(min.lim, max.lim, lab.lim))
  }
  
}
# function to save figures as EPS with embedded font
FigureAsEPS <- function(fig, fig.name, width, height) {
  path <- "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/"
  setEPS()
  postscript(paste(path, fig.name, ".eps", sep = ""), horizontal = FALSE, 
             onefile = FALSE, paper = "special", width = width, height = height, family = "Arial")
  print(fig)
  dev.off()
  embed_fonts(paste(path, fig.name, ".eps", sep = ""), outfile = paste(path, fig.name, "_Embed", ".eps", sep = ""),
              options = "-dEPSCrop")
}
# function to save figures as TIFF
FigureAsTiff <- function(fig, fig.name) {
  tiff(paste(path, fig.name, ".tif", sep = ""), res = tiff.res, units = "in", width = width,
       height = height)
  print(fig)
  dev.off()
}



# height and width of saved EPS files for meta-analysis results, forest plots, and model-selection distributions
rii.type.name <- "MonoCtrl"
height.plot <- 5
width.plot <- 7.5
relative.widths <- c(3, 4)
width.model.selection <- 8
height.model.selection <- 8
# toggle sizes, shapes, and line widths for .ma .fp and .ms
meta.analysis.sizes <- c(4, 2, 2, 2, 2)
gray.hex <- "#808080"
meta.analysis.colors <- c(gray.hex, "#000000")
meta.analysis.shapes <- c(21, 19, 22, 15)
meta.analysis.scale <- seq(-5, 5, by = 0.2)
meta.analysis.scale.fine <- seq(-5, 5, by = 0.1)
meta.analysis.text <- 2.75
meta.analysis.lwd <- 0.6
meta.analysis.theme <- theme(legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "none", 
      axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title = element_text(size = 12), 
      axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 12), strip.background = element_blank(),
      panel.background = element_blank(), panel.spacing = unit(0.4, "cm"))
cases.lwd <- 0.6
cases.shape <- c(21, 19)
cases.scale <- seq(-10, 10, by = 0.2)
cases.theme <- theme(legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "none", 
      axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 10), axis.title = element_text(size = 12), 
      axis.line = element_line(size = 0.5))
model.selection.theme <- theme(axis.title = element_text(size = 12), axis.text = element_text(size = 6), axis.line = element_line(size = 0.5),
                               legend.title = element_blank(), legend.text = element_text(size = 9), legend.justification = c(1, 0),
                               legend.position = c(1, 0), strip.placement = "outside", strip.text = element_text(size = 9), 
                               strip.background = element_blank(), panel.background = element_blank(), panel.spacing = unit(0.1, "cm"))

pd <- position_dodge(0.75)
pd.less <- position_dodge(0.5)
tiff.res <- 600


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD DATA FRAMES ------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
  
#
# load data for meta-analysis, forest plots, and PCA
#
# estimates by network (for forest plots)
# meta-analytic data (grand means and groups)
mono.ctrl.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteOutput_MonoCtrl.csv", row.names = 1)
true.ctrl.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteOutput_TrueCtrlOnly.csv", row.names = 1)
mono.ctrl.init$CompetitionComparison <- "Monoculture"
true.ctrl.init$CompetitionComparison <- "True control"
complete.df <- rbind(mono.ctrl.init, true.ctrl.init)
levels(complete.df$Network)[levels(complete.df$Network) == "Kinlock unpublished (b)"] <- "Kinlock unpublished (B)"
complete.df$Network <- factor(complete.df$Network, levels = rev(c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Bush & Van Auken 2004", "Frérot et al. 2006",
                                                                  "Chacón & Muñoz 2007", "Engel & Weltzin 2008", "Niu & Wan 2008", 
                                                                  "Pfeifer-Meis. et al. 2008", "Marty et al. 2009", "Baude et al. 2011", "Mariotte et al. 2012",
                                                                  "Amanull. & Stewart 2013", "Pausch et al. 2013", "Jiang et al. 2014", "Cuda et al. 2015", 
                                                                  "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg & Landa 1991", "Weigelt et al. 2002",
                                                                  "Costa et al. 2003", "Hedberg et al. 2005", "Fortner & Weltzin 2007", "Domènech & Vilà 2008",
                                                                  "Svenning et al. 2008", "Saccone et al. 2010", "Armas & Pugnaire 2011", "Farrer & Goldberg 2011",
                                                                  "Gao et al. 2014", "Löf et al. 2014", "Kinlock unpublished (B)", "Kinlock unpublished")))
complete.df$CompetitionComparison <- factor(complete.df$CompetitionComparison, levels = c("True control", "Monoculture"))

mono.ctrl.meta.analysis.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteMetaAnalysisOutput_MonoCtrl.csv", row.names = 1)
true.ctrl.meta.analysis.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/CompleteMetaAnalysisOutput_TrueCtrlOnly.csv", row.names = 1)
mono.ctrl.meta.analysis.init$CompetitionComparison <- "Monoculture"
true.ctrl.meta.analysis.init$CompetitionComparison <- "True control"
meta.analysis.df <- rbind(mono.ctrl.meta.analysis.init, true.ctrl.meta.analysis.init)
meta.analysis.df$ModelType <- factor(meta.analysis.df$ModelType, levels = c("Grand mean", "Exp type", "Setting", "Habitat", "Habit", "Age"))
meta.analysis.df$Parameter <- factor(meta.analysis.df$Parameter, levels = c("OverallMean", "OverallSD", "Heterogeneity", "True ctrl", "Mono ctrl", "Other", "Grassland", "Field", "Garden", "Greenhouse", "Woody", "Herbaceous", "Adult", "Juvenile"))
meta.analysis.df$CompetitionComparison <- factor(meta.analysis.df$CompetitionComparison, levels = c("True control", "Monoculture"))
meta.analysis.df$ShapeFactor <- as.character(meta.analysis.df$CompetitionComparison)
meta.analysis.df$ShapeFactor[which(meta.analysis.df$ModelType == "Grand mean" & meta.analysis.df$CompetitionComparison == "True control")] <- "Grand mean true control"
meta.analysis.df$ShapeFactor[which(meta.analysis.df$ModelType == "Grand mean" & meta.analysis.df$CompetitionComparison == "Monoculture")] <- "Grand mean mono control"
meta.analysis.df$ShapeFactor <- factor(meta.analysis.df$ShapeFactor, levels = c("True control", "Monoculture", "Grand mean true control", "Grand mean mono control"))
meta.analysis.df <- meta.analysis.df[meta.analysis.df$Parameter != "OverallSD" & meta.analysis.df$Parameter != "Heterogeneity"  & meta.analysis.df$Parameter != "OverallPrecision", ]


# pca scores and loadings
pca.scores.mono.ctrl.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/PCAScores_MonoCtrl.csv", row.names = 1)
pca.loadings.mono.ctrl.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/PCALoadings_MonoCtrl.csv", row.names = 1)


# comparing invasive/native, C3/C4, and N-fixing spp.
species.character.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_MonoCtrl.csv", row.names = 1)
species.character.true.ctrl.init <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesCharacterDifferences_TrueCtrlOnly.csv", row.names = 1)
# group means only (don't include differences in means or SDs)
species.character.df <- species.character.init[grep(pattern = "Mean", species.character.init$MetricName), ]
species.character.true.ctrl <- species.character.true.ctrl.init[grep(pattern = "Mean", species.character.true.ctrl.init$MetricName), ]
species.character.df$CompetitionComparison <- "Monoculture"
species.character.true.ctrl$CompetitionComparison <- "True control"
species.character.df <- rbind(species.character.df, species.character.true.ctrl)
species.character.df$Comparison <- factor(species.character.df$Comparison, levels = c("Invasive status", "C4 photosynthesis", "N-fixing ability"))
species.character.df$MetricName <- factor(species.character.df$MetricName)
levels(species.character.df$MetricName) <- c("C3 phot.", "C4 phot.", "Invasive", "N-fix.", "Native", "non N-fix.")
species.character.df$MetricName <- factor(species.character.df$MetricName, levels = c("Invasive", "Native", "C4 phot.", "C3 phot.", "N-fix.", "non N-fix."))

# comparing spp. position
species.position.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/SpeciesPosition.csv", row.names = 1)
levels(species.position.df$Network) <- c("Sangakk. & Roberts 1985", "Gurevitch et al. 1990", "Mariotte et al. 2012", 
                                         "Fortner & Weltzin 2007", "Hendriks et al. 2015", "Miller & Werner 1987", 
                                         "Engel & Weltzin 2008", "Goldberg & Landa 1991")
species.position.df$Network <- factor(species.position.df$Network, levels = c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Engel & Weltzin 2008", "Mariotte et al. 2012",
                                                                              "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg & Landa 1991", "Fortner & Weltzin 2007"))
levels(species.position.df$Species) <- c("Dactylis glomerata", "Plantago lanceolata", "Trifolium repens")


# comparing metrics from networks wth treatment/control
treatment.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/ControlTreatmentNetworkMetrics_MonoCtrl.csv", row.names = 1)
treatment.df$TreatmentType <- factor(treatment.df$TreatmentType, levels = c("Control", "Decreased nutrients", "Increased nutrients", "Decreased water", "Increased water", "Warming"))
levels(treatment.df$MetricName) <- c("Imbal", "Ind Eff", "Strength", "RI", "Connect")
treatment.df$MetricName <- factor(treatment.df$MetricName, levels = c("Strength", "Ind Eff", "Imbal", "RI", "Connect"))


# comparing standardized differences in RII in pairwise and 3 spp. combinations
additive.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/AdditivityFit.csv", row.names = 1)
additive.df$Parameter <- factor(additive.df$Parameter, levels = c("grand mean", "group mean", "among-group sd", "within-group sd"))
levels(additive.df$Network) <- c("Baude et al. 2011", "Pausch et al. 2013", "Marty et al. 2009", "Frérot et al. 2006", "All")
additive.df$Network <- factor(additive.df$Network, levels = c("All", "Frérot et al. 2006", "Marty et al. 2009", "Baude et al. 2011", "Pausch et al. 2013"))


# model selection for weight, in- and out-strength
model.selection.instrength.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/InStrengthSamples_MonoCtrl.csv", row.names = 1)
model.selection.outstrength.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/OutStrengthSamples_MonoCtrl.csv", row.names = 1)
model.selection.instrength.true.ctrl <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/InStrengthSamples_TrueCtrlOnly.csv", row.names = 1)
model.selection.outstrength.true.ctrl <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/OutStrengthSamples_TrueCtrlOnly.csv", row.names = 1)
model.selection.instrength.df$Distribution <- factor(model.selection.instrength.df$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.instrength.df$Case <- factor(model.selection.instrength.df$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012",
                                                                                            "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008",
                                                                                            "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))
model.selection.outstrength.df$Distribution <- factor(model.selection.outstrength.df$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.outstrength.df$Case <- factor(model.selection.outstrength.df$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012",
                                                                                              "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008",
                                                                                              "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))
model.selection.instrength.true.ctrl$Distribution <- factor(model.selection.instrength.true.ctrl$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.instrength.true.ctrl$Case <- factor(model.selection.instrength.true.ctrl$Case, levels = c("Goldberg & Landa 1991", "Svenning et al. 2008",
                                                                                            "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))
model.selection.outstrength.true.ctrl$Distribution <- factor(model.selection.outstrength.true.ctrl$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.outstrength.true.ctrl$Case <- factor(model.selection.outstrength.true.ctrl$Case, levels = c("Goldberg & Landa 1991", "Svenning et al. 2008",
                                                                                              "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))

# plot of all study sites over map
world <- map_data("world")
worldmap <- ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "gray95", color = "gray65", size = 0.2) + coord_fixed(1.3) + 
  labs(x = "Longitude", y = "Latitude") + geom_point(data = coding.df, aes(x = Longitude, y = Latitude), shape = 19, size = 0.5) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
postscript(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/StudySitesOnMap.eps", res = 1200, units = "in", width = 4, height = 2.5)
if (file.type == 1) {
  FigureAsEPS(fig = worldmap, fig.name = "StudySitesOnMap", width = 4, height = 2.5)
} else {
  FigureAsTiff(fig = worldmap, fig.name = "StudySitesOnMap", width = 4, height = 2.5)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ESTIMATES FROM META-ANALYSIS ------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# mean strength
strength.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "MeanStrength"), ]
strength.df.lim <- SetAxisLimits(strength.df)
strength <- ggplot(data = strength.df) + facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), width = 0.2, 
                size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, col = CompetitionComparison),
             position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = strength.df.lim[3], col = CompetitionComparison), 
            size = meta.analysis.text, hjust = 0, vjust = 0.5, fontface = "italic", position = pd) +
  coord_flip(ylim = c(strength.df.lim[1], strength.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(x = "", y = "Mean strength") + theme_classic() + meta.analysis.theme

# indirect effect
indirect.effect.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "IndirectEffect"), ]
indirect.effect.df.lim <- SetAxisLimits(indirect.effect.df)
indirect.effect <- ggplot(data = indirect.effect.df) + facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), width = 0.2, 
                size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, col = CompetitionComparison), 
             position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = indirect.effect.df.lim[3], col = CompetitionComparison),
            size = meta.analysis.text, hjust = 0, vjust = 0.5, fontface = "italic", position = pd) +
  coord_flip(ylim = c(indirect.effect.df.lim[1], indirect.effect.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(x = "", y = "Mean indirect effect") + theme_classic() + meta.analysis.theme

# imbalance
imbalance.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "Imbalance"), ]
imbalance.df.lim <- SetAxisLimits(imbalance.df)
imbalance <- ggplot(data = imbalance.df)  + facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), width = 0.2, 
                size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, col = CompetitionComparison), 
             position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = imbalance.df.lim[3] - 0.05, col = CompetitionComparison), 
            size = meta.analysis.text, hjust = 0, vjust = 0.5, fontface = "italic", position = pd) +
  coord_flip(ylim = c(imbalance.df.lim[1], imbalance.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(x = "", y = "Interaction imbalance") + theme_classic() + meta.analysis.theme

# transitivity
intransitivity.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "RelativeIntransitivity"), ]
intransitivity.df.lim <- SetAxisLimits(intransitivity.df)
intransitivity <- ggplot(data = intransitivity.df) + 
  facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), width = 0.2, 
                size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, size = ModelType, shape = ShapeFactor, col = CompetitionComparison), 
             position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = intransitivity.df.lim[3] - 0.1, col = CompetitionComparison), 
            size = meta.analysis.text, hjust = 0, vjust = 0.5, fontface = "italic", position = pd) +
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  coord_flip(ylim = c(intransitivity.df.lim[1], intransitivity.df.lim[2])) + 
  labs(x = "", y = "Relative intransitivity") + theme_classic() + meta.analysis.theme

# connectance
connectance.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "WeightedConnectance"), ]
connectance.df.lim <- SetAxisLimits(connectance.df)
connectance <- ggplot(data = connectance.df) + 
  facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), width = 0.2, 
                size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, size = ModelType, shape = ShapeFactor, col = CompetitionComparison), 
             position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = connectance.df.lim[3], col = CompetitionComparison), 
            size = meta.analysis.text, hjust = 0, vjust = 0.5, fontface = "italic", position = pd) +
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  coord_flip(ylim = c(connectance.df.lim[1], connectance.df.lim[2])) + 
  labs(x = "", y = "Weighted connectance") + theme_classic() + meta.analysis.theme


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FOREST PLOTS ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# strength
strength.cases.df <- complete.df[which(complete.df$MetricName == "MeanStrength"), ]
strength.cases.df.lim <- SetAxisLimits(strength.cases.df, is.cases = TRUE)
strength.cases <- ggplot(data = strength.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), 
                width = 0.3, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, col = CompetitionComparison), 
             position = pd.less, fill = "white") + 
  labs(x = "", y = "Mean strength") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(breaks = cases.scale, limits = c(strength.cases.df.lim[1], strength.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() +
  theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 8), legend.title = element_text(size = 10))

strength.combined <- plot_grid(strength, strength.cases, align = "h", axis = "b", label_fontface = "plain",labels = c("A", "B"), 
                               rel_widths = relative.widths)
if (file.type == 1) {
  FigureAsEPS(fig = strength.combined, fig.name = "Strength", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = strength.combined, fig.name = "Strength", width = width.plot, height = height.plot)
}


# indirect effect
indirect.effect.cases.df <- complete.df[which(complete.df$MetricName == "IndirectEffect"), ]
indirect.effect.cases.df.lim <- SetAxisLimits(indirect.effect.cases.df, is.cases = TRUE)
indirect.effect.cases <- ggplot(data = indirect.effect.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), 
                width = 0.3, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, col = CompetitionComparison), 
             position = pd.less, fill = "white") + 
  labs(x = "", y = "Mean indirect effect") +
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_y_continuous(limits = c(indirect.effect.cases.df.lim[1], indirect.effect.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() +
  theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 8), legend.title = element_text(size = 10))

indirect.effect.combined <- plot_grid(indirect.effect, indirect.effect.cases, align = "h", axis = "b", labels = c("A", "B"), 
                                      label_fontface = "plain", rel_widths = relative.widths)
if (file.type == 1) {
  FigureAsEPS(fig = indirect.effect.combined, fig.name = "IndEff", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = indirect.effect.combined, fig.name = "IndEff", width = width.plot, height = height.plot)
}

# imbalance
imbalance.cases.df <- complete.df[which(complete.df$MetricName == "Imbalance"), ]
imbalance.cases.df.lim <- SetAxisLimits(imbalance.cases.df, is.cases = TRUE)
imbalance.cases <- ggplot(data = imbalance.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), 
                width = 0.3, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, col = CompetitionComparison), 
             position = pd.less, fill = "white") + 
  labs(x = "", y = "Interaction imbalance") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(limits = c(0, imbalance.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() +
  theme(legend.position = c(0.7, 0.2), legend.text = element_text(size = 8), legend.title = element_text(size = 10))

imbalance.combined <- plot_grid(imbalance, imbalance.cases, align = "h", axis = "b", labels = c("A", "B"),
                                label_fontface = "plain", rel_widths = relative.widths)
if (file.type == 1) {
  FigureAsEPS(fig = imbalance.combined, fig.name = "Imbalance", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = imbalance.combined, fig.name = "Imbalance", width = width.plot, height = height.plot)
}

# asymmetry
asymmetry.cases.df <- complete.df[which(complete.df$MetricName == "Asymmetry"), ]
asymmetry.cases.df.lim <- SetAxisLimits(asymmetry.cases.df, is.cases = TRUE)
asymmetry.cases <- ggplot(data = asymmetry.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU), 
                width = 0.3, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean), 
             position = pd.less, fill = "white") + 
  labs(x = "", y = "Percentage asymmetric interactions") + 
  scale_y_continuous(limits = c(0, asymmetry.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip()
if (file.type == 1) {
  FigureAsEPS(fig = asymmetry.cases, fig.name = "Asymmetry", width = width.plot / 2, height = height.plot - 1)
} else {
  FigureAsTiff(fig = asymmetry.cases, fig.name = "Asymmetry", width = width.plot / 2, height = height.plot - 1)
}


# intransitivity
intransitivity.cases.df <- complete.df[which(complete.df$MetricName == "RelativeIntransitivity"), ]
intransitivity.cases.df.lim <- SetAxisLimits(intransitivity.cases.df, is.cases = TRUE)
intransitivity.cases <- ggplot(data = intransitivity.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison),
                width = 0.3, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, col = CompetitionComparison),
             position = pd.less, fill = "white") + 
  labs(x = "", y = "Relative intransitivity") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(breaks = cases.scale, limits = c(0, intransitivity.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() +
  theme(legend.position = c(0.7, 0.8), legend.text = element_text(size = 8), legend.title = element_text(size = 10))

intransitivity.combined <- plot_grid(intransitivity, intransitivity.cases, align = "h", axis = "b", labels = c("A", "B"),
                                     label_fontface = "plain", rel_widths = relative.widths)
if (file.type == 1) {
  FigureAsEPS(fig = intransitivity.combined, fig.name = "Transitivity", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = intransitivity.combined, fig.name = "Transitivity", width = width.plot, height = height.plot)
}

# connectance
connectance.cases.df <- complete.df[which(complete.df$MetricName == "WeightedConnectance"), ]
connectance.cases.df.lim <- SetAxisLimits(connectance.cases.df, is.cases = TRUE)
connectance.cases <- ggplot(data = connectance.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = CompetitionComparison), 
                width = 0.3, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, col = CompetitionComparison),
             position = pd.less, fill = "white") + 
  labs(x = "", y = "Weighted connectance") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(limits = c(connectance.cases.df.lim[1], connectance.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() +
  theme(legend.position = c(0.2, 0.9), legend.text = element_text(size = 8), legend.title = element_text(size = 10))

connectance.combined <- plot_grid(connectance, connectance.cases, align = "h", axis = "b", labels = c("A", "B"), label_fontface = "plain",
                                  rel_widths = relative.widths)
if (file.type == 1) {
  FigureAsEPS(fig = connectance.combined, fig.name = "Connectance", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = connectance.combined, fig.name = "Connectance", width = width.plot, height = height.plot)
}



# connectance: facilitative, competitive, and total (absolute value)
# meta-analysis plots
connectance.compfac.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "WeightedConnectance" | 
                                                   meta.analysis.df$MetricName == "WeightedFacConnectance" | 
                                                   meta.analysis.df$MetricName == "WeightedCompConnectance"), ]
connectance.compfac.df$MetricName <- factor(connectance.compfac.df$MetricName, levels = c("WeightedFacConnectance", "WeightedCompConnectance", "WeightedConnectance"))
connectance.compfac.df$CompetitionComparison <- factor(connectance.compfac.df$CompetitionComparison, levels = c("Monoculture", "True control"))
connectance.compfac.df.lim <- SetAxisLimits(connectance.compfac.df)
connectance.compfac <- ggplot(data = connectance.compfac.df) + 
  facet_grid(ModelType ~ CompetitionComparison, switch = "y", scales = "free", space = "free") +
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = MetricName), width = 0.2, 
                size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, size = ModelType, shape = ShapeFactor, col = MetricName), 
             position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = connectance.compfac.df.lim[3]),
            colour = gray.hex, size = meta.analysis.text, hjust = 0, vjust = 0.5, fontface = "italic", position = pd) +
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#000000"), labels = c("Facilitative", "Competitive", "Absolute value")) +
  scale_shape_manual(values = c(19, 19, 15, 15), guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  coord_flip(ylim = c(connectance.compfac.df.lim[1], connectance.compfac.df.lim[2])) + 
  labs(x = "", y = "Weighted connectance") + theme_classic() + meta.analysis.theme
if (file.type == 1) {
  FigureAsEPS(fig = connectance.compfac, fig.name = "ConnectanceCompFac", width = width.plot - 1, height = height.plot)
} else {
  FigureAsTiff(fig = connectance.compfac, fig.name = "ConnectanceCompFac", width = width.plot - 1, height = height.plot)
}


# forest plots
connectance.cases.compfac.df <- complete.df[which(complete.df$MetricName == "WeightedConnectance" | 
                                                    complete.df$MetricName == "WeightedCompConnectance" | 
                                                    complete.df$MetricName == "WeightedFacConnectance"), ]
connectance.cases.compfac.df$MetricName <- factor(connectance.cases.compfac.df$MetricName, levels = c("WeightedFacConnectance", "WeightedCompConnectance", "WeightedConnectance"))
connectance.cases.compfac.df$CompetitionComparison <- factor(connectance.cases.compfac.df$CompetitionComparison, levels = c("Monoculture", "True control"))
connectance.cases.compfac.df.lim <- SetAxisLimits(connectance.cases.compfac.df, is.cases = TRUE)
connectance.cases.compfac <- ggplot(data = connectance.cases.compfac.df) +
  facet_grid(. ~ CompetitionComparison) +
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = MetricName), 
                width = 0.3, size = cases.lwd, position = pd) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, col = MetricName),
             position = pd, fill = "white") + 
  labs(x = "", y = "Weighted connectance") + theme_classic() + cases.theme + 
  theme(strip.placement = "outside", strip.text = element_text(size = 12), strip.background = element_blank(), 
        panel.background = element_blank(), panel.spacing = unit(0.4, "cm")) +
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#000000"), labels = c("Facilitative", "Competitive", "Absolute value")) +
  scale_y_continuous(limits = c(connectance.cases.compfac.df.lim[1], connectance.cases.compfac.df.lim[2])) + 
  coord_flip() + theme(legend.position = "right")
if (file.type == 1) {
  FigureAsEPS(fig = connectance.cases.compfac, fig.name = "ConnectanceCompFac_Forest", width = width.plot, height = height.plot + 1)
} else {
  FigureAsTiff(fig = connectance.cases.compfac, fig.name = "ConnectanceCompFac_Forest", width = width.plot, height = height.plot + 1)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PCA BIPLOT ------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# monoculture control
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/PCA_MonoCtrl.eps", horizontal = FALSE, onefile = FALSE, 
             paper = "special", width = 5, height = 5)
  par(mar = c(2.5, 1.5, 1.5, 1),  # distance from plot to side of page
      mgp = c(1.2, 0.2, 0),  # distance from plot to label
      las = 1,  # rotate y-axis text
      tck = -0.005,  # reduce tick length
      xaxs = "i", yaxs = "i")  # remove plot padding
  palette(c("black", "#e41a1c"))
  print(biplot(x = pca.scores.mono.ctrl.df[, 2:3], y = pca.loadings.mono.ctrl.df[, 2:3], xlabs = pca.scores.mono.ctrl.df[, 1], 
               ylabs = pca.loadings.mono.ctrl.df[, 1], 
               xlab = paste("PC1 (", 45.63, "% var. explained)", sep = ""), ylab = paste("PC2 (", 26.83, "% var. explained)", sep = ""), 
               arrow.len = 0.06, xlim = c(-4.5, 4.5), ylim = c(-4.5, 4.5), cex = c(0.5, 1)))
  abline(h = 0, lty = 2, col = gray.hex)
  abline(v = 0, lty = 2, col = gray.hex)
  dev.off()
  embed_fonts("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/PCA_MonoCtrl.eps", 
              outfile = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/PCA_MonoCtrl_Embed.eps",
              options = "-dEPSCrop")
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/PCA_MonoCtrl.tiff", res = tiff.res, units = "in", width = 5, height = 5)
  par(mar = c(2.5, 1.5, 1.5, 1),  # distance from plot to side of page
      mgp = c(1.2, 0.2, 0),  # distance from plot to label
      las = 1,  # rotate y-axis text
      tck = -0.005,  # reduce tick length
      xaxs = "i", yaxs = "i")  # remove plot padding
  palette(c("black", "#e41a1c"))
  print(biplot(x = pca.scores.mono.ctrl.df[, 2:3], y = pca.loadings.mono.ctrl.df[, 2:3], xlabs = pca.scores.mono.ctrl.df[, 1], 
               ylabs = pca.loadings.mono.ctrl.df[, 1], 
               xlab = paste("PC1 (", pc1.var, "% var. explained)", sep = ""), ylab = paste("PC2 (", pc2.var, "% var. explained)", sep = ""), 
               arrow.len = 0.06, xlim = c(-4.5, 4.5), ylim = c(-4.5, 4.5), cex = c(0.5, 1)))
  abline(h = 0, lty = 2, col = gray.hex)
  abline(v = 0, lty = 2, col = gray.hex)
  dev.off()
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MODEL SELECTION FIGURES -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#

# CDF plot, log scaled
# Monoculture control
in.strength.cdf.log <- ggplot(dat = model.selection.instrength.df, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 5) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(scaled = TRUE, long = unit(1, "mm"), mid = unit(NA, "mm"), short = unit(NA, "mm")) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + model.selection.theme

out.strength.cdf.log <- ggplot(dat = model.selection.outstrength.df, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 5) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(scaled = TRUE, long = unit(1, "mm"), mid = unit(NA, "mm"), short = unit(NA, "mm")) +
  labs(x = "Out-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = "none")
cdf.log <- plot_grid(in.strength.cdf.log, out.strength.cdf.log, ncol = 1, align = "v", axis = "b", labels = c("A", "B"), label_fontface = "plain")
if (file.type == 1) {
  FigureAsEPS(fig = cdf.log, fig.name = "StrengthCDF_LogLog", width = width.model.selection, height = height.model.selection)
} else {
  FigureAsTiff(fig = cdf.log, fig.name = "StrengthCDF_LogLog", width = width.model.selection, height = height.model.selection)
}



# CDF plot, not log-scaled
# Monoculture control
in.strength.cdf <- ggplot(dat = model.selection.instrength.df[which(model.selection.instrength.df$Values < 10), ], 
                          aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 5) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + model.selection.theme

out.strength.cdf <- ggplot(dat = model.selection.outstrength.df[which(model.selection.outstrength.df$Values < 10), ], 
                           aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 5) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  labs(x = "Out-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = "none")
cdf <- plot_grid(in.strength.cdf, out.strength.cdf, ncol = 1, align = "v", axis = "b", labels = c("A", "B"), label_fontface = "plain")
if (file.type == 1) {
  FigureAsEPS(fig = cdf, fig.name = "StrengthCDF", width = width.model.selection, height = height.model.selection)
} else {
  FigureAsTiff(fig = cdf, fig.name = "StrengthCDF", width = width.model.selection, height = height.model.selection)
}


# CDF plot, log-scaled
# True control
in.strength.true.ctrl.cdf.log <- ggplot(dat = model.selection.instrength.true.ctrl, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(scaled = TRUE, long = unit(1, "mm"), mid = unit(NA, "mm"), short = unit(NA, "mm")) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + model.selection.theme

out.strength.true.ctrl.cdf.log <- ggplot(dat = model.selection.outstrength.true.ctrl, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(scaled = TRUE, long = unit(1, "mm"), mid = unit(NA, "mm"), short = unit(NA, "mm")) +
  labs(x = "Out-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = "none")
cdf.truectrl.log <- plot_grid(in.strength.true.ctrl.cdf.log, out.strength.true.ctrl.cdf.log, ncol = 1, align = "v", axis = "b", labels = c("A", "B"), 
                              label_fontface = "plain")
if (file.type == 1) {
  FigureAsEPS(fig = cdf.truectrl.log, fig.name = "StrengthCDF_TrueCtrlOnly_LogLog", width = width.model.selection - 1.5, height = height.model.selection - 1)
} else {
  FigureAsTiff(fig = cdf.truectrl.log, fig.name = "StrengthCDF_TrueCtrlOnly_LogLog", width = width.model.selection - 1.5, height = height.model.selection - 1)
}


# CDF plot, not log-scaled
# True control
in.strength.true.ctrl.cdf <- ggplot(dat = model.selection.instrength.true.ctrl[which(model.selection.instrength.true.ctrl$Values < 10), ], aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + model.selection.theme

out.strength.true.ctrl.cdf <- ggplot(dat = model.selection.outstrength.true.ctrl[which(model.selection.outstrength.true.ctrl$Values < 10), ], aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  labs(x = "Out-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = "none")
cdf.truectrl <- plot_grid(in.strength.true.ctrl.cdf, out.strength.true.ctrl.cdf, ncol = 1, align = "v", axis = "b", labels = c("A", "B"), label_fontface = "plain")
if (file.type == 1) {
  FigureAsEPS(fig = cdf.truectrl, fig.name = "StrengthCDF_TrueCtrlOnly", width = width.model.selection - 1.5, height = height.model.selection - 1)
} else {
  FigureAsTiff(fig = cdf.truectrl, fig.name = "StrengthCDF_TrueCtrlOnly", width = width.model.selection - 1.5, height = height.model.selection - 1)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# COMPARE SPP. CHARACTERS -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
species.character.plot <- ggplot(dat = species.character.df) + 
  facet_grid(Strength ~ Comparison, scales = "free_x", switch = "y") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = MetricName, ymin = CILL, ymax = CIUL, col = CompetitionComparison), width = 0.2, size = cases.lwd, position = pd) + 
  geom_point(aes(x = MetricName, y = Mean, col = CompetitionComparison), position = pd) + labs(x = "", y = "")  + 
  geom_text(aes(label = paste("n = ", SampleSize, sep = ""), x = MetricName, y = -0.55, col = CompetitionComparison), 
              size = meta.analysis.text, hjust = 0.5, vjust = 0, fontface = "italic", position = pd, angle = 90) +
  scale_color_manual(values = rev(meta.analysis.colors)) +
  theme_classic() + meta.analysis.theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
if (file.type == 1) {
  FigureAsEPS(fig = species.character.plot, fig.name = "SpeciesCharacterDifferences", width = width.plot - 2, height = height.plot)
} else {
  FigureAsTiff(fig = species.character.plot, fig.name = "SpeciesCharacterDifferences", width = width.plot - 2, height = height.plot)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# COMPARE TREATMENTS ----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
treatment.fig <- ggplot(dat = treatment.df) + facet_wrap(~ Network, scales = "free", nrow = 2) +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = MetricName, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = TreatmentType), position = pd.less, width = 0.3, size = cases.lwd) + 
  geom_point(aes(x = MetricName, y = MetaAnalysisMean, col = TreatmentType), size = 0.8, position = pd.less) + 
  scale_color_manual(values = c("#000000", "#b2df8a", "#33a02c", "#a6cee3", "#1f78b4", "#e31a1c")) + 
  labs(x = "", y = "") + theme_classic() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1), axis.text.y = element_text(size = 10), axis.title = element_text(size = 12), 
        axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 11), strip.background = element_blank(),
        panel.background = element_blank(), panel.spacing = unit(0.3, "cm"))
if (file.type == 1) {
  FigureAsEPS(fig = treatment.fig, fig.name = "NetworkTreatments", width = 8, height = 5)
} else {
  FigureAsTiff(fig = treatment.fig, fig.name = "NetworkTreatments", width = 8, height = 5)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SPP. POSITION FIGURE -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
species.position.fig <- ggplot(dat = species.position.df) + facet_grid(MetricName ~ Species, scales = "free_x") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = BootstrapCIL, ymax = BootstrapCIU, col = Network), width = 0.2, size = cases.lwd) + 
  geom_point(aes(x = Network, y = BootstrapMean, col = Network)) + labs(x = "", y = "Strength")  + 
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628", "#f781bf", "#999999"), guide = guide_legend(nrow = 4)) +
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "bottom", 
        axis.text.x = element_blank(), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12), 
        axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 12), 
        strip.background = element_blank(), panel.background = element_blank(), panel.spacing = unit(0.5, "cm"),
        axis.ticks.x = element_blank())
if (file.type == 1) {
  FigureAsEPS(fig = species.position.fig, fig.name = "SpeciesPosition", width = 5, height = 5)
} else {
  FigureAsTiff(fig = species.position.fig, fig.name = "SpeciesPosition", width = 5, height = 5)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ADDITIVITY FIGURE -----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
additive.fig <- ggplot(dat = additive.df[which(additive.df$Parameter == "grand mean" | additive.df$Parameter == "group mean" ), ]) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Network, ymin = CIL, ymax = CIU), width = 0.2, size = meta.analysis.lwd) + 
  geom_point(aes(x = Network, y = Mean, shape = Parameter, size = Parameter),  fill = "white") + 
  scale_size_manual(values = meta.analysis.sizes) + scale_shape_manual(values = meta.analysis.shapes) +
  labs(x = "", y = "Standardized mean difference in RII") + theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.position = "none", 
        axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 9), axis.title = element_text(size = 11), 
        axis.line = element_line(size = 0.5)) + coord_flip()
if (file.type == 1) {
  FigureAsEPS(fig = additive.fig, fig.name = "Additivity", width = 3.5, height = 3)
} else {
  FigureAsTiff(fig = additive.fig, fig.name = "Additivity", width = 3.5, height = 3)
}



