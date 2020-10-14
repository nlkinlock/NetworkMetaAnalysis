library(ggplot2)
# set file type, 1 = eps, other = tiff
file.type <- 1
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# FIGURE FUNCTIONS ------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# set limits given min and max
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
# save figures as EPS with embedded font
FigureAsEPS <- function(fig, fig.name, width, height, embed.font = FALSE) {
  fig.path <- paste(path, "Output/Figures/", sep = "")
  setEPS()
  postscript(paste(fig.path, fig.name, ".eps", sep = ""), horizontal = FALSE, family = "Arial",
             onefile = FALSE, paper = "special", width = width, height = height)
  print(fig)
  dev.off()
  if (embed.font == TRUE) {
    embed_fonts(paste(fig.path, fig.name, ".eps", sep = ""),
                outfile = paste(fig.path, fig.name, "_Embed", ".eps", sep = ""),
                options = "-dEPSCrop")
  }
}
# save figures as TIFF
FigureAsTiff <- function(fig, fig.name) {
  tiff(paste(path, fig.name, ".tif", sep = ""), res = tiff.res, units = "in", width = width,
       height = height)
  print(fig)
  dev.off()
}
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SET VARIABLES ---------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
path <- "/home/nlkinlock/Documents/NetworkMetaAnalysis/"
# height and width of saved EPS files for meta-analysis results, forest plots, and model-selection distributions
rii.type.name <- "MonoCtrl"
height.plot <- 5
width.plot <- 7.5
relative.widths <- c(3, 4)
width.model.selection <- 8.5
height.model.selection <- 8.5
width.model.selection.ctrl <- 6.75
height.model.selection.ctrl <- 8
pd <- position_dodge(0.75)
pd.less <- position_dodge(0.5)
tiff.res <- 600
gray.hex <- "#808080"


model.selection.theme <- theme(axis.title = element_text(size = 12, colour = "black"), 
                                axis.text = element_text(size = 6, colour = "black"), 
                                axis.line = element_line(size = 0.5, colour = "black"),
                                axis.ticks = element_line(colour = "black"),
                                legend.direction = "horizontal",
                                legend.title = element_blank(),
                                legend.text = element_text(size = 9, colour = "black"), 
                                legend.key = element_blank(),
                                legend.background = element_blank(),
                                legend.position = "bottom",legend.text.align = 0, 
                                legend.spacing.x = unit(0.5, "cm"),
                                panel.grid = element_blank(),
                                strip.placement = "outside", 
                                strip.text = element_text(size = 9, colour = "black"), 
                                strip.background = element_blank(),
                                panel.background = element_blank(), 
                                panel.spacing = unit(0.1, "cm"))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD META-ANALYSIS DATA -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# load coding table with descriptive data for each case
coding.init <- read.csv(paste(path, "Input/CodingTable.csv", sep = ""))
col.remove <- c("AbundanceRank", "TargetDensity", "NeighborDensity", "Connectance",
                "WateringRegime", "PotVolume", "SoilType", "Treatments", "Authors", "Journal", "Title",
                "doi", "Abstract", "OtherTreatments", "Metric", "UniqueNotes")  # unwanted columns
coding.df <- coding.init[, !(names(coding.init) %in% col.remove)] # only completed cases with RII
coding.df$UniqueID <- as.factor(coding.df$UniqueID)
coding.df$ExperimentType <- factor(coding.df$ExperimentType)
#
# load data for meta-analysis, forest plots, and PCA
#
# complete dataframe with all network-level estimates (for forest plots)
mono.ctrl.init <- read.csv(paste(path, "Output/CompleteOutput_MonoCtrl.csv", sep = ""), row.names = 1)
true.ctrl.init <- read.csv(paste(path, "Output/CompleteOutput_TrueCtrlOnly.csv", sep = ""), row.names = 1)
mono.ctrl.init$CompetitionComparison <- "Monoculture"
true.ctrl.init$CompetitionComparison <- "True control"
complete.df <- rbind(mono.ctrl.init, true.ctrl.init)
# rename networks using my data as needed
levels(complete.df$Network)[levels(complete.df$Network) == "Kinlock unpublished (b)"] <- "Kinlock unpublished (B)"
# levels(complete.df$Network)[levels(complete.df$Network) == "Kinlock unpublished"] <- "Kinlock Chapter 3"
# order by date and monoculture vs. true control
complete.df$Network <- factor(complete.df$Network, levels = rev(c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Bush & Van Auken 2004", "Frérot et al. 2006",
                                                                  "Chacón & Muñoz 2007", "Engel & Weltzin 2008", "Niu & Wan 2008", 
                                                                  "Pfeifer-Meis. et al. 2008", "Marty et al. 2009", "Baude et al. 2011", "Mariotte et al. 2012",
                                                                  "Amanull. & Stewart 2013", "Pausch et al. 2013", "Jiang et al. 2014", "Cuda et al. 2015", 
                                                                  "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg & Landa 1991", "Weigelt et al. 2002",
                                                                  "Costa et al. 2003", "Hedberg et al. 2005", "Fortner & Weltzin 2007", "Domènech & Vilà 2008",
                                                                  "Svenning et al. 2008", "Saccone et al. 2010", "Armas & Pugnaire 2011", "Farrer & Goldberg 2011",
                                                                  "Gao et al. 2014", "Löf et al. 2014", "Kinlock unpublished (B)", "Kinlock unpublished")))
complete.df$CompetitionComparison <- factor(complete.df$CompetitionComparison, levels = c("True control", "Monoculture"))
#
# dataframe with meta-analysis grand means and group means
mono.ctrl.meta.analysis.init <- read.csv(paste(path, "Output/CompleteMetaAnalysisOutput_MonoCtrl.csv", sep = ""), row.names = 1)
true.ctrl.meta.analysis.init <- read.csv(paste(path, "Output/CompleteMetaAnalysisOutput_TrueCtrlOnly.csv", sep = ""), row.names = 1)
mono.ctrl.meta.analysis.init$CompetitionComparison <- "Monoculture"
true.ctrl.meta.analysis.init$CompetitionComparison <- "True control"
meta.analysis.df <- rbind(mono.ctrl.meta.analysis.init, true.ctrl.meta.analysis.init)
meta.analysis.df$ShapeFactor <- as.character(meta.analysis.df$CompetitionComparison)
meta.analysis.df$ShapeFactor[which(meta.analysis.df$ModelType == "Grand mean" & meta.analysis.df$CompetitionComparison == "True control")] <- "Grand mean true control"
meta.analysis.df$ShapeFactor[which(meta.analysis.df$ModelType == "Grand mean" & meta.analysis.df$CompetitionComparison == "Monoculture")] <- "Grand mean mono control"
meta.analysis.df$ShapeFactor <- factor(meta.analysis.df$ShapeFactor, levels = c("True control", "Monoculture", "Grand mean true control", "Grand mean mono control"))
levels(meta.analysis.df$ModelType) <- c("Age", " ", "Habit", "Habitat", "Setting")
meta.analysis.df$ModelType <- factor(meta.analysis.df$ModelType, levels = c(" ", "Setting", "Habitat", "Habit", "Age"))
levels(meta.analysis.df$Parameter) <- c("Adult", "Field", "Garden", "Grassland", "Greenhouse", "Herbaceous", "Heterogeneity", "Juvenile", "Other", "Grand mean", "Overall SD", "Woody")
meta.analysis.df$Parameter <- factor(meta.analysis.df$Parameter, levels = c("Grand mean", "OverallSD", "Heterogeneity", "True ctrl", "Mono ctrl", "Other", "Grassland", "Field", "Garden", "Greenhouse", "Woody", "Herbaceous", "Adult", "Juvenile"))
meta.analysis.df$CompetitionComparison <- factor(meta.analysis.df$CompetitionComparison, levels = c("True control", "Monoculture"))
meta.analysis.df <- meta.analysis.df[meta.analysis.df$Parameter != "OverallSD" & meta.analysis.df$Parameter != "Heterogeneity"  & meta.analysis.df$Parameter != "OverallPrecision", ]
#
# pca scores and loadings
pca.scores.mono.ctrl.df <- read.csv(paste(path, "Output/PCAScores_MonoCtrl.csv", sep = ""), row.names = 1)
pca.loadings.mono.ctrl.df <- read.csv(paste(path, "Output/PCALoadings_MonoCtrl.csv", sep = ""), row.names = 1)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD SPECIES CHARACTER DATA -------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# comparing invasive/native, C3/C4, and N-fixing spp.
species.character.init <- read.csv(paste(path, "Output/SpeciesCharacterDifferences_MonoCtrl.csv", sep = ""), row.names = 1)
species.character.true.ctrl.init <- read.csv(paste(path, "Output/SpeciesCharacterDifferences_TrueCtrlOnly.csv", sep = ""), row.names = 1)
# plots include group means only (don't include differences in means or SDs)
species.character.df <- species.character.init[grep(pattern = "Mean", species.character.init$MetricName), ]
species.character.true.ctrl <- species.character.true.ctrl.init[grep(pattern = "Mean", species.character.true.ctrl.init$MetricName), ]
species.character.df$CompetitionComparison <- "Monoculture"
species.character.true.ctrl$CompetitionComparison <- "True control"
species.character.df <- rbind(species.character.df, species.character.true.ctrl)
species.character.df$Comparison <- factor(species.character.df$Comparison, levels = c("Invasive status", "C4 photosynthesis", "N-fixing ability"))
species.character.df$MetricName <- factor(species.character.df$MetricName)
# abbreviations for figures
levels(species.character.df$MetricName) <- c("C3 phot.", "C4 phot.", "Invasive", "N-fix.", "Native", "non N-fix.")
species.character.df$MetricName <- factor(species.character.df$MetricName, levels = c("Invasive", "Native", "C4 phot.", "C3 phot.", "N-fix.", "non N-fix."))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD SPECIES POSITION DATA -------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# comparing spp. position
species.position.df <- read.csv(paste(path, "Output/SpeciesPosition.csv", sep = ""), row.names = 1)
levels(species.position.df$Network) <- c("Sangakk. & Roberts 1985", "Gurevitch et al. 1990", "Mariotte et al. 2012", 
                                         "Fortner & Weltzin 2007", "Hendriks et al. 2015", "Miller & Werner 1987", 
                                         "Engel & Weltzin 2008", "Goldberg & Landa 1991")
species.position.df$Network <- factor(species.position.df$Network, levels = c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Engel & Weltzin 2008", "Mariotte et al. 2012",
                                                                              "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg & Landa 1991", "Fortner & Weltzin 2007"))
levels(species.position.df$Species) <- c("Dactylis glomerata", "Plantago lanceolata", "Trifolium repens")
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD MODEL SELECTION DATA ---------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# model selection for weight, in- and out-strength
model.selection.instrength.df <- read.csv(paste(path, "Output/InStrengthSamples_MonoCtrl.csv", sep = ""), row.names = 1)
model.selection.outstrength.df <- read.csv(paste(path, "Output/OutStrengthSamples_MonoCtrl.csv", sep = ""), row.names = 1)
model.selection.instrength.true.ctrl <- read.csv(paste(path, "Output/InStrengthSamples_TrueCtrlOnly.csv", sep = ""), row.names = 1)
model.selection.outstrength.true.ctrl <- read.csv(paste(path, "Output/OutStrengthSamples_TrueCtrlOnly.csv", sep = ""), row.names = 1)
# rename networks using my data as needed
levels(model.selection.instrength.true.ctrl$Case)[levels(model.selection.instrength.true.ctrl$Case) == "Kinlock unpublished (b)"] <- "Kinlock unpublished (B)"
# levels(model.selection.instrength.true.ctrl$Case)[levels(model.selection.instrength.true.ctrl$Case) == "Kinlock unpublished"] <- "Kinlock Chapter 3"
levels(model.selection.outstrength.true.ctrl$Case)[levels(model.selection.outstrength.true.ctrl$Case) == "Kinlock unpublished (b)"] <- "Kinlock unpublished (B)"
# levels(model.selection.outstrength.true.ctrl$Case)[levels(model.selection.outstrength.true.ctrl$Case) == "Kinlock unpublished"] <- "Kinlock Chapter 3"
# re-order networks and distributions for figure
model.selection.instrength.df$Distribution <- factor(model.selection.instrength.df$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.instrength.df$Case <- factor(model.selection.instrength.df$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012",
                                                                                            "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008",
                                                                                            "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished"))
model.selection.outstrength.df$Distribution <- factor(model.selection.outstrength.df$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.outstrength.df$Case <- factor(model.selection.outstrength.df$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012",
                                                                                              "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008",
                                                                                              "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished"))
model.selection.instrength.true.ctrl$Distribution <- factor(model.selection.instrength.true.ctrl$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.instrength.true.ctrl$Case <- factor(model.selection.instrength.true.ctrl$Case, levels = c("Goldberg & Landa 1991", "Svenning et al. 2008",
                                                                                            "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (B)", "Kinlock unpublished"))
model.selection.outstrength.true.ctrl$Distribution <- factor(model.selection.outstrength.true.ctrl$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
model.selection.outstrength.true.ctrl$Case <- factor(model.selection.outstrength.true.ctrl$Case, levels = c("Goldberg & Landa 1991", "Svenning et al. 2008",
                                                                                              "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (B)", "Kinlock unpublished"))
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# META-ANALYSIS FIGURES -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# toggle sizes, shapes, and line widths for .ma .fp and .ms
meta.analysis.sizes <- c(4, 2, 2, 2, 2)
meta.analysis.colors <- c(gray.hex, "#000000")
meta.analysis.shapes <- c(21, 19, 22, 15)
meta.analysis.scale <- seq(-5, 5, by = 0.2)
meta.analysis.scale.fine <- seq(-5, 5, by = 0.1)
meta.analysis.text <- 2.75
meta.analysis.lwd <- 0.6
meta.analysis.err <- 0.2
meta.analysis.theme <- theme(legend.title = element_blank(),
                            legend.text = element_text(size = 10, colour = "black"), 
                            legend.position = "none",
                            legend.key = element_blank(),
                            legend.background = element_blank(),
                            axis.text = element_text(size = 10, colour = "black"),
                            axis.title.y = element_text(size = 12, colour = "black"),
                            axis.title.x = element_blank(),
                            axis.ticks = element_line(colour = "black"),
                            axis.line = element_line(size = 0.5, colour = "black"), 
                            strip.placement = "outside", 
                            strip.text = element_text(size = 12, colour = "black"),
                            strip.background = element_blank(),
                            panel.background = element_blank(),
                            panel.spacing = unit(0.2, "cm"))
strength.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "MeanStrength"), ]
strength.df.lim <- SetAxisLimits(strength.df)
indirect.effect.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "IndirectEffect"), ]
indirect.effect.df.lim <- SetAxisLimits(indirect.effect.df)
imbalance.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "Imbalance"), ]
imbalance.df.lim <- SetAxisLimits(imbalance.df)
intransitivity.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "RelativeIntransitivity"), ]
intransitivity.df.lim <- SetAxisLimits(intransitivity.df)
connectance.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "WeightedConnectance"), ]
connectance.df.lim <- SetAxisLimits(connectance.df)

strength <- ggplot(data = strength.df) + facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), size = meta.analysis.lwd, position = pd, width = meta.analysis.err) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, 
                  col = CompetitionComparison), position = pd, fill = "white") + 
  coord_flip(ylim = c(strength.df.lim[1], strength.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(y = "Mean strength") + theme_classic() + meta.analysis.theme

indirect.effect <- ggplot(data = indirect.effect.df) + 
  facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, 
                  col = CompetitionComparison), position = pd, fill = "white") + 
  coord_flip(ylim = c(indirect.effect.df.lim[1], indirect.effect.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(y = "Mean indirect effect") + theme_classic() + meta.analysis.theme + 
  theme(strip.text = element_blank(), axis.text.y = element_blank())

imbalance <- ggplot(data = imbalance.df)  + facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, 
                  col = CompetitionComparison), position = pd, fill = "white") + 
  coord_flip(ylim = c(imbalance.df.lim[1], imbalance.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(y = "Interaction imbalance") + theme_classic() + meta.analysis.theme + 
  theme(strip.text = element_blank(), axis.text.y = element_blank())

intransitivity <- ggplot(data = intransitivity.df) + 
  facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU,
                    col = CompetitionComparison), width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, size = ModelType, shape = ShapeFactor, 
                col = CompetitionComparison), position = pd, fill = "white") + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  coord_flip(ylim = c(intransitivity.df.lim[1], intransitivity.df.lim[2])) + 
  labs(y = "Relative intransitivity") + theme_classic() + meta.analysis.theme

connectance <- ggplot(data = connectance.df) + 
  facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, size = ModelType, shape = ShapeFactor,
                col = CompetitionComparison), position = pd, fill = "white") + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes) +
  scale_size_manual(values = meta.analysis.sizes) +
  coord_flip(ylim = c(connectance.df.lim[1], connectance.df.lim[2])) + 
  labs(y = "Weighted connectance") + theme_classic() + meta.analysis.theme + 
  theme(strip.text = element_blank(), axis.text.y = element_blank())

all.ma.figs <- plot_grid(strength, indirect.effect, imbalance, intransitivity, connectance, 
                          nrow = 2, ncol = 3, align = "h", axis = "tb", labels = "AUTO", label_fontface = "plain", 
                          rel_widths = c(1.45, 1, 1, 1.45, 1), hjust = -1)
FigureAsEPS(fig = all.ma.figs, fig.name = "Test", width = 8, height = 7)
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# CASE FIGURES ----------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
cases.lwd <- 0.6
cases.shape <- c(21, 19)
cases.scale <- seq(-10, 10, by = 0.2)
cases.theme <- theme(legend.title = element_text(size = 10, colour = "black", hjust = 0.5), 
                      legend.text = element_text(size = 9, colour = "black", 
                                      margin =  margin(t = 0, r = 0, l = 0, b = 0, unit = "pt")),
                      legend.position = "none", 
                      legend.spacing.y = unit(0.1, 'cm'),
                      legend.key.height = unit(0.4, "cm"),
                      legend.background = element_blank(),
                      axis.text.y = element_text(size = 9, colour = "black"), 
                      axis.text.x = element_text(size = 10, colour = "black"), 
                      axis.title.x = element_text(size = 11, colour = "black"),
                      axis.title.y = element_blank(),
                      axis.ticks = element_line(colour = "black"),
                      axis.line = element_line(size = 0.5, colour = "black"))

complete.df <- subset(complete.df, Network == "Costa et al. 2003" | Network == "Engel & Weltzin 2008" | Network == "Mariotte et al. 2012")
strength.cases.df <- complete.df[which(complete.df$MetricName == "MeanStrength"), ]
strength.cases.df.lim <- SetAxisLimits(strength.cases.df, is.cases = TRUE)

indirect.effect.cases.df <- complete.df[which(complete.df$MetricName == "IndirectEffect"), ]
indirect.effect.cases.df.lim <- SetAxisLimits(indirect.effect.cases.df, is.cases = TRUE)

imbalance.cases.df <- complete.df[which(complete.df$MetricName == "Imbalance"), ]
imbalance.cases.df.lim <- SetAxisLimits(imbalance.cases.df, is.cases = TRUE)

intransitivity.cases.df <- complete.df[which(complete.df$MetricName == "RelativeIntransitivity"), ]
intransitivity.cases.df.lim <- SetAxisLimits(intransitivity.cases.df, is.cases = TRUE)

connectance.cases.df <- complete.df[which(complete.df$MetricName == "WeightedConnectance"), ]
connectance.cases.df.lim <- SetAxisLimits(connectance.cases.df, is.cases = TRUE)

strength.cases <- ggplot(data = strength.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), size = cases.lwd, position = pd.less, width = meta.analysis.err) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, 
                col = CompetitionComparison), position = pd.less, fill = "white") + 
  labs(x = "", y = "Mean strength") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(breaks = cases.scale, limits = c(strength.cases.df.lim[1], strength.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip()

indirect.effect.cases <- ggplot(data = indirect.effect.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU,
                    col = CompetitionComparison), width = meta.analysis.err, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, 
                col = CompetitionComparison), position = pd.less, fill = "white") + 
  labs(x = "", y = "Mean indirect effect") +
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_y_continuous(limits = c(indirect.effect.cases.df.lim[1], indirect.effect.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() + theme(axis.text.y = element_blank())

imbalance.cases <- ggplot(data = imbalance.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison,
                col = CompetitionComparison), position = pd.less, fill = "white") + 
  labs(x = "", y = "Interaction imbalance") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(limits = c(-0.06, imbalance.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() + theme(axis.text.y = element_blank())

intransitivity.cases <- ggplot(data = intransitivity.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU,
                    col = CompetitionComparison), width = meta.analysis.err, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, 
                  col = CompetitionComparison), position = pd.less, fill = "white") + 
  labs(x = "", y = "Relative intransitivity") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(breaks = cases.scale, limits = c(0, intransitivity.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip()

connectance.cases <- ggplot(data = connectance.cases.df) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, 
                col = CompetitionComparison), position = pd.less, fill = "white") + 
  labs(x = "", y = "Weighted connectance") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(limits = c(connectance.cases.df.lim[1], connectance.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() + theme(legend.position = c(0, 0.8), axis.text.y = element_blank())

all.cases.figs <- plot_grid(strength.cases, indirect.effect.cases, imbalance.cases, intransitivity.cases, connectance.cases, 
                          nrow = 2, ncol = 3, align = "h", axis = "tb", labels = "AUTO", label_fontface = "plain", 
                          rel_widths = c(1.8, 1, 1, 1.8, 1), hjust = -1)
FigureAsEPS(fig = all.cases.figs, fig.name = "Test", width = 6, height = 3.5)



asymmetry.cases.df <- complete.df[which(complete.df$MetricName == "Asymmetry"), ]
asymmetry.cases.df.lim <- SetAxisLimits(asymmetry.cases.df, is.cases = TRUE)
asymmetry.cases <- ggplot(data = asymmetry.cases.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU), 
                width = meta.analysis.err, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean), 
             position = pd.less, fill = "white") + 
  labs(x = "", y = "Percentage asymmetric interactions") + 
  scale_y_continuous(limits = c(0, asymmetry.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip()

#
# connectance: facilitative, competitive, and total (absolute value)
# meta-analysis plots
conn.theme.add <- theme(legend.position = c(0.47, 0.2), legend.key.height = unit(0.5, "cm"), legend.key.width = unit(0.1, "cm"),
        legend.title = element_text(size = 10, colour = "black", hjust = 0.5), 
                      legend.text = element_text(size = 9, colour = "black", 
                      margin =  margin(t = 0, r = 0, l = 0, b = 0, unit = "pt")))
connectance.compfac.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "WeightedConnectance" | 
                                                   meta.analysis.df$MetricName == "WeightedFacConnectance" | 
                                                   meta.analysis.df$MetricName == "WeightedCompConnectance"), ]
connectance.compfac.df$MetricName <- factor(connectance.compfac.df$MetricName, levels = c("WeightedFacConnectance", "WeightedCompConnectance", "WeightedConnectance"))
connectance.compfac.df$CompetitionComparison <- factor(connectance.compfac.df$CompetitionComparison, levels = c("Monoculture", "True control"))
connectance.compfac.df.lim <- SetAxisLimits(connectance.compfac.df)
connectance.compfac <- ggplot(data = connectance.compfac.df) + 
  facet_grid(ModelType ~ CompetitionComparison, switch = "y", scales = "free", space = "free") +
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = MetricName),
                width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, size = ModelType, shape = ShapeFactor, 
                col = MetricName), position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = connectance.compfac.df.lim[3] + 0.06),
            colour = gray.hex, size = meta.analysis.text, hjust = 0, vjust = 0.5, fontface = "italic",
            position = pd) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#000000"), 
                    labels = c("Facilitative", "Competitive", "Abs. value")) +
  scale_shape_manual(values = c(19, 19, 15, 15), guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  coord_flip(ylim = c(connectance.compfac.df.lim[1], connectance.compfac.df.lim[2])) + 
  guides(color = guide_legend(title = "Network")) +
  labs(x = "", y = "Weighted connectance") + theme_classic() + 
  meta.analysis.theme + conn.theme.add

#
# forest plots
connectance.cases.compfac.df <- complete.df[which(complete.df$MetricName == "WeightedConnectance" | 
                                                    complete.df$MetricName == "WeightedCompConnectance" | 
                                                    complete.df$MetricName == "WeightedFacConnectance"), ]
connectance.cases.compfac.df$MetricName <- factor(connectance.cases.compfac.df$MetricName, 
                                            levels = c("WeightedFacConnectance", "WeightedCompConnectance", "WeightedConnectance"))
connectance.cases.compfac.df$CompetitionComparison <- factor(connectance.cases.compfac.df$CompetitionComparison, levels = c("Monoculture", "True control"))
connectance.cases.compfac.df.lim <- SetAxisLimits(connectance.cases.compfac.df, is.cases = TRUE)
connectance.cases.compfac <- ggplot(data = connectance.cases.compfac.df) +
  facet_grid(. ~ CompetitionComparison) +
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = MetricName), 
                width = meta.analysis.err, size = cases.lwd, position = pd) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, col = MetricName),
             position = pd, fill = "white") + 
  labs(x = "", y = "Weighted connectance") + 
  scale_color_manual(values = c("#377eb8", "#e41a1c", "#000000"), 
                      labels = c("Facilitative", "Competitive", "Abs. value")) +
  scale_y_continuous(limits = c(connectance.cases.compfac.df.lim[1], connectance.cases.compfac.df.lim[2])) + 
  guides(color = guide_legend(title = "Network")) +
  coord_flip() + theme_classic() + cases.theme + conn.theme.add +
  theme(strip.placement = "outside", strip.text = element_text(size = 12), 
        strip.background = element_blank(), panel.background = element_blank(), 
        panel.spacing = unit(0.4, "cm"), legend.position = c(0.7, 0.8))



#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PCA BIPLOT FIGURE -----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
postscript(paste(path, "Output/Figures/PCA_MonoCtrl.eps", sep = ""), horizontal = FALSE, onefile = FALSE, 
           paper = "special", width = 5, height = 5)
par(mar = c(2.5, 1.5, 1.5, 1),  # distance from plot to side of page
    mgp = c(1.2, 0.2, 0),  # distance from plot to label
    las = 1,  # rotate y-axis text
    tck = -0.005,  # reduce tick length
    xaxs = "i", yaxs = "i")  # remove plot padding
palette(c("black", "#e41a1c"))
print(biplot(x = pca.scores.mono.ctrl.df[, 2:3], y = pca.loadings.mono.ctrl.df[, 2:3], xlabs = pca.scores.mono.ctrl.df[, 1], 
             ylabs = pca.loadings.mono.ctrl.df[, 1], 
             xlab = paste("PC1 (", 43.40, "% var. explained)", sep = ""), ylab = paste("PC2 (", 26.28, "% var. explained)", sep = ""), 
             arrow.len = 0.06, ylim = c(-4, 4), xlim = c(-6, 6), cex = c(0.5, 1)))
abline(h = 0, lty = 2, col = gray.hex)
abline(v = 0, lty = 2, col = gray.hex)
dev.off()
embed_fonts(paste(path, "Output/Figures/PCA_MonoCtrl.eps", sep = ""), 
            outfile = paste(path, "Output/Figures/PCA_MonoCtrl_Embed.eps", sep = ""),
            options = "-dEPSCrop")
#
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
cdf.log <- plot_grid(in.strength.cdf.log, out.strength.cdf.log, ncol = 1, align = "hv", axis = "b", labels = c("A", "B"), label_fontface = "plain")
if (file.type == 1) {
  FigureAsEPS(fig = cdf.log, fig.name = "StrengthCDF_LogLog", width = width.model.selection, height = height.model.selection)
} else {
  FigureAsTiff(fig = cdf.log, fig.name = "StrengthCDF_LogLog", width = width.model.selection, height = height.model.selection)
}
#
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
cdf <- plot_grid(in.strength.cdf, out.strength.cdf, ncol = 1, align = "hv", axis = "b", labels = c("A", "B"), label_fontface = "plain")
if (file.type == 1) {
  FigureAsEPS(fig = cdf, fig.name = "StrengthCDF", width = width.model.selection, height = height.model.selection)
} else {
  FigureAsTiff(fig = cdf, fig.name = "StrengthCDF", width = width.model.selection, height = height.model.selection)
}
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SPECIES CHARACTERS FIGURE ---------------------------------------------------
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
#
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

