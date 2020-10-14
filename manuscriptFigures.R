library(ggplot2)
# set file type, 1 = eps, other = tiff
file.type <- 1
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
<<<<<<< HEAD
# FIGURE FUNCTIONS ------------------------------------------------------------
||||||| 2b1eac9
# INITIALIZE ------------------------------------------------------------------
=======
# INPUT VARIABLES -------------------------------------------------------------
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
<<<<<<< HEAD
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
||||||| 2b1eac9
# inputs for figure params
#
# set file type, 1 = eps, other = tiff
file.type <- 2
# function to set limits given min and max
lim.func <- function(df) {
  range.lim <- abs(max(df$CI.u, na.rm = TRUE) - min(df$CI.l, na.rm = TRUE))
  min.lim <- min(df$CI.l, na.rm = TRUE) - (0.1 * range.lim)
  max.lim <- max(df$CI.u, na.rm = TRUE) + (0.2 * range.lim)
  lab.lim <- max(df$CI.u, na.rm = TRUE) + (0.03 * range.lim)
  return(c(min.lim, max.lim, lab.lim))
=======

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
  
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
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
||||||| 2b1eac9
lim.func.for <- function(df) {
  range.lim <- abs(max(df$CI.u.post, na.rm = TRUE) - min(df$CI.l.post, na.rm = TRUE))
  min.lim <- min(df$CI.l.post, na.rm = TRUE) - (0.1 * range.lim)
  max.lim <- max(df$CI.u.post, na.rm = TRUE) + (0.1 * range.lim)
  return(c(min.lim, max.lim))
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
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
||||||| 2b1eac9
# height and width of saved EPS files for meta-analysis results (.ma), forest plots (.fp), and model-selection distributions (.ms)
width.plot <- 7.5
=======
# function to save figures as TIFF
FigureAsTiff <- function(fig, fig.name) {
  tiff(paste(path, fig.name, ".tif", sep = ""), res = tiff.res, units = "in", width = width,
       height = height)
  print(fig)
  dev.off()
}



# height and width of saved EPS files for meta-analysis results, forest plots, and model-selection distributions
rii.type.name <- "MonoCtrl"
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
height.plot <- 5
<<<<<<< HEAD
width.plot <- 7.5
relative.widths <- c(3, 4)
pd <- position_dodge(0.75)
||||||| 2b1eac9
width.ms <- 7
height.ms <- 6
# toggle sizes, shapes, and line widths for .ma .fp and .ms
ma.sizes <- c(5, 2, 2, 2, 2, 2)
ma.shapes <- c(18, 19, 19, 19, 19, 19)
ma.scale <- seq(-5, 5, by = 0.2)
ma.text <- 2.75
ma.lwd <- 0.6
ma.theme <- theme(legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "none", 
      axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10), axis.title = element_text(size = 12), 
      axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 12), strip.background = element_blank(),
      panel.background = element_blank(), panel.spacing = unit(0.4, "cm"))
fp.lwd <- 0.6
fp.shape <- c(19, 15)
fp.scale <- seq(-10, 10, by = 0.2)
fp.theme <- theme(legend.title = element_blank(), legend.text = element_text(size = 12), legend.position = "none", 
      axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 10), axis.title = element_text(size = 12), 
      axis.line = element_line(size = 0.5))
ms.theme <- theme(axis.title = element_text(size = 10), axis.text = element_text(size = 6), axis.line = element_line(size = 0.5),
                  legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "bottom",
                  strip.placement = "outside", strip.text = element_text(size = 8), strip.background = element_blank(),
                  panel.background = element_blank(), panel.spacing = unit(0.1, "cm"))

pd <- position_dodge(1)
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
pd.less <- position_dodge(0.5)
tiff.res <- 600
<<<<<<< HEAD
gray.hex <- "#808080"
||||||| 2b1eac9

=======


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD DATA FRAMES ------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
  
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
<<<<<<< HEAD
||||||| 2b1eac9
# load data
=======
# load data for meta-analysis, forest plots, and PCA
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
<<<<<<< HEAD
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MAP FIGURE ------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# plot of all study sites over map
||||||| 2b1eac9
# estimates by network (for forest plots)
# meta-analytic data (grand means and groups)
final.df <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/codingtable_metrics.csv", row.names = 1)
final.df$network <- factor(final.df$network, levels = rev(c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Bush & Van Auken 2004", "Frérot et al. 2006",
                                                    "Chacón & Muñoz 2007", "Dehlin et al. 2008", "Engel & Weltzin 2008", "Niu & Wan 2008", 
                                                    "Pfeifer-Meis. et al. 2008", "Marty et al. 2009", "Baude et al. 2011", "Mariotte et al. 2012",
                                                    "Amanull. & Stewart 2013", "Pausch et al. 2013", "Jiang et al. 2014", "Čuda et al. 2015", 
                                                    "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg & Landa 1991", "Weigelt et al. 2002",
                                                    "Costa et al. 2003", "Hedberg et al. 2005", "Fortner & Weltzin 2007", "Domènech & Vilà 2008",
                                                    "Svenning et al. 2008", "Saccone et al. 2010", "Armas & Pugnaire 2011", "Farrer & Goldberg 2011",
                                                    "Gao et al. 2014", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished")))
meta <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/metaanalysis_output.csv", row.names = 1)
meta$comparison <- factor(meta$comparison, levels = c("Grand mean", "Exp type", "Setting", "Habitat", "Habit", "Age"))
meta$param <- factor(meta$param, levels = c("mu", "sigma", "Q", "True ctrl", "Mono ctrl", "Other", "Grassland", "Field", "Garden", "Greenhouse", "Woody", "Herbaceous", "Adult", "Juvenile"))



# model selection for weight, in- and out-strength
df.w <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/weight_distsamples.csv", row.names = 1)
df.is <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/instr_distsamples.csv", row.names = 1)
df.os <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/outstr_distsamples.csv", row.names = 1)
df.w$Distribution <- factor(df.w$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal",  "Pareto"))
df.w$Case <- factor(df.w$Case, levels = c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Bush & Van Auken 2004", "Frérot et al. 2006",
                                                    "Chacón & Muñoz 2007", "Dehlin et al. 2008", "Engel & Weltzin 2008", "Niu & Wan 2008", 
                                                    "Pfeifer-Meis. et al. 2008", "Marty et al. 2009", "Baude et al. 2011", "Mariotte et al. 2012",
                                                    "Amanull. & Stewart 2013", "Pausch et al. 2013", "Jiang et al. 2014", "Čuda et al. 2015", 
                                                    "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg & Landa 1991", "Weigelt et al. 2002",
                                                    "Costa et al. 2003", "Hedberg et al. 2005", "Fortner & Weltzin 2007", "Domènech & Vilà 2008",
                                                    "Svenning et al. 2008", "Saccone et al. 2010", "Armas & Pugnaire 2011", "Farrer & Goldberg 2011",
                                                    "Gao et al. 2014", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))
df.is$Distribution <- factor(df.is$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
df.is$Case <- factor(df.is$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012",
                                                    "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008",
                                                    "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))
df.os$Distribution <- factor(df.os$Distribution, levels = c("Data", "Normal", "Exponential", "Lognormal", "Pareto"))
df.os$Case <- factor(df.os$Case, levels = c("Miller & Werner 1987", "Engel & Weltzin 2008", "Niu & Wan 2008", "Mariotte et al. 2012",
                                            "Jiang et al. 2014", "Goldberg & Landa 1991", "Svenning et al. 2008",
                                            "Armas & Pugnaire 2011", "Löf et al. 2014", "Kinlock unpublished (b)", "Kinlock unpublished"))

# pca scores and loadings
df.scores <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/pca_scores.csv", row.names = 1)
df.loadings <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/pca_loadings.csv", row.names = 1)

# comparing invasive/native, C3/C4, and N-fixing spp.
df.char <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_character_diff.csv", row.names = 1)
df.char$comparison <- factor(df.char$comparison, levels = c("Invasive status", "C4 photosynthesis", "N-fixing ability"))

# comparing spp. position
df.spp <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/species_position.csv", row.names = 1)
levels(df.spp$network) <- c("Sangakk. & Roberts 1985", "Gurevitch et al. 1990", "Mariotte et al. 2012", 
                            "Fortner & Weltzin 2007", "Hendriks et al. 2015", "Miller & Werner 1987", 
                            "Engel & Weltzin 2008", "Goldberg & Landa 1991")
df.spp$network <- factor(df.spp$network, levels = c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Engel & Weltzin 2008", "Mariotte et al. 2012",
                                                    "Hendriks et al. 2015", "Gurevitch et al. 1990", "Goldberg & Landa 1991", "Fortner & Weltzin 2007"))
levels(df.spp$species) <- c("Dactylis glomerata", "Plantago lanceolata", "Trifolium repens")

# comparing metrics from networks wth treatment/control
df.treat <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/ctrl_treatment.csv", row.names = 1)
df.treat$treatmentType <- factor(df.treat$treatmentType, levels = c("Control", "Decreased nutrients", "Increased nutrients", "Decreased water", "Increased water", "Warming"))
df.treat$metric <- factor(df.treat$metric, levels = c("strength", "ind eff", "asymm", "RI", "connect"))

# comparing standardized differences in RII in pairwise and 3 spp. combinations
df.add <- read.csv("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/additivity.csv", row.names = 1)
df.add$param <- factor(df.add$param, levels = c("grand mean", "group mean", "among-group sd", "within-group sd"))
levels(df.add$network) <- c("Baude et al. 2011", "Pausch et al. 2013", "Marty et al. 2009", "Frérot et al. 2006", "All")
df.add$network <- factor(df.add$network, levels = c("All", "Frérot et al. 2006", "Marty et al. 2009", "Baude et al. 2011", "Pausch et al. 2013"))

=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
world <- map_data("world")
worldmap <- ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "gray95", color = "gray65", size = 0.2) + coord_fixed(1.3) + 
  labs(x = "Longitude", y = "Latitude") + geom_point(data = coding.df, aes(x = Longitude, y = Latitude), shape = 19, size = 0.5) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.line = element_blank(),
        axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
<<<<<<< HEAD
postscript(file = paste(path, "Output/Figures/StudySitesOnMap.eps", sep = ""), res = 1200, units = "in", width = 4, height = 2.5)
if (file.type == 1) {
  FigureAsEPS(fig = worldmap, fig.name = "StudySitesOnMap", width = 4, height = 2.5)
} else {
  FigureAsTiff(fig = worldmap, fig.name = "StudySitesOnMap", width = 4, height = 2.5)
}
||||||| 2b1eac9
tiff(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/world.tiff", res = 300, units = "in", width = 4, height = 2.5)
worldmap + geom_point(data = df, aes(x = Longitude, y = Latitude), shape = 19, size = 0.5)
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ESTIMATES FROM META-ANALYSIS ------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
=======
postscript(file = "/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Output/Figures/StudySitesOnMap.eps", res = 1200, units = "in", width = 4, height = 2.5)
if (file.type == 1) {
  FigureAsEPS(fig = worldmap, fig.name = "StudySitesOnMap", width = 4, height = 2.5)
} else {
  FigureAsTiff(fig = worldmap, fig.name = "StudySitesOnMap", width = 4, height = 2.5)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ESTIMATES FROM META-ANALYSIS ------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
<<<<<<< HEAD
||||||| 2b1eac9
# mean strength
str.df <- meta[which(meta$metric == "strength" & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
str.df.lim <- lim.func(str.df)
strength <- ggplot(data = str.df) + facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = param, y = est, shape = comparison, size = comparison)) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = str.df.lim[3]), size = ma.text, hjust = 0, vjust = 0.5, fontface = "italic") +
  coord_flip(ylim = c(str.df.lim[1], str.df.lim[2])) + 
  scale_y_continuous(breaks = ma.scale) + 
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  labs(x = "", y = "Mean strength") + theme_classic() + ma.theme

# indirect effect
ind.df <- meta[which(meta$metric == "ind eff" & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
ind.df.lim <- lim.func(ind.df)
ind.eff <- ggplot(data = ind.df) + facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = param, y = est, shape = comparison, size = comparison)) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = ind.df.lim[3]), size = ma.text, hjust = 0, vjust = 0.5, fontface = "italic") +
  coord_flip(ylim = c(ind.df.lim[1], ind.df.lim[2])) + 
  scale_y_continuous(breaks = seq(-5, 5, 0.4)) + 
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  labs(x = "", y = "Mean indirect effect") + theme_classic() + ma.theme

# asymmetry difference
asym.df <- meta[which(meta$metric == "asymm" & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
asym.df.lim <- lim.func(asym.df)
asymm <- ggplot(data = asym.df)  + facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = param, y = est, shape = comparison, size = comparison)) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = asym.df.lim[3]), size = ma.text, hjust = 0, vjust = 0.5, fontface = "italic") +
  coord_flip(ylim = c(asym.df.lim[1], asym.df.lim[2])) + 
  scale_y_continuous(breaks = ma.scale) + 
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  labs(x = "", y = "Interaction asymmetry") + theme_classic() + ma.theme

# transitivity
ri.df <- meta[which(meta$metric == "RI"  & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
ri.df.lim <- lim.func(ri.df)
rel.int <- ggplot(data = ri.df) + 
  facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd, position = pd) + 
  geom_point(aes(x = param, y = est, size = comparison, shape = comparison), position = pd) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = ri.df.lim[3]), size = ma.text, hjust = 0, vjust = 0.5, fontface = "italic") +
  scale_y_continuous(breaks = ma.scale) + 
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  coord_flip(ylim = c(ri.df.lim[1], ri.df.lim[2])) + 
  labs(x = "", y = "Relative intransitivity") + theme_classic() + ma.theme

# connectance
con.df <- meta[which(meta$metric == "connect"  & meta$param != "sigma" & meta$param != "Q"  & meta$param != "tau"), ]
con.df.lim <- lim.func(con.df)
connect <- ggplot(data = con.df) + 
  facet_grid(comparison ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, color = gray.hex, size = ma.lwd) + 
  geom_errorbar(aes(x = param, ymin = CI.l, ymax = CI.u), width = 0.2, size = ma.lwd) + 
  geom_point(aes(x = param, y = est, size = comparison, shape = comparison)) + 
  geom_text(aes(label = paste("n = ", n, sep = ""), x = param, y = con.df.lim[3]), size = ma.text, hjust = 0, vjust = 0.5, fontface = "italic") +
  scale_y_continuous(breaks = ma.scale) + 
  scale_shape_manual(values = ma.shapes, guide = FALSE) +
  scale_size_manual(values = ma.sizes, guide = FALSE) +
  coord_flip(ylim = c(con.df.lim[1], con.df.lim[2])) + 
  labs(x = "", y = "Weighted connectance") + theme_classic() + ma.theme


=======
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


>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# META-ANALYSIS FIGURES -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
<<<<<<< HEAD
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
                            axis.title = element_text(size = 12, colour = "black"), 
                            axis.ticks = element_line(colour = "black"),
                            axis.line = element_line(size = 0.5, colour = "black"), 
                            strip.placement = "outside", 
                            strip.text = element_text(size = 12, colour = "black"),
                            strip.background = element_blank(),
                            panel.background = element_blank(),
                            panel.spacing = unit(0.4, "cm"))
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
                      axis.title = element_text(size = 12, colour = "black"), 
                      axis.ticks = element_line(colour = "black"),
                      axis.line = element_line(size = 0.5, colour = "black"))
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
# load data for meta-analysis, forest plots
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
# create figures
# mean strength
strength.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "MeanStrength"), ]
strength.df.lim <- SetAxisLimits(strength.df)
strength.cases.df <- complete.df[which(complete.df$MetricName == "MeanStrength"), ]
strength.cases.df.lim <- SetAxisLimits(strength.cases.df, is.cases = TRUE)
strength <- ggplot(data = strength.df) + facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), size = meta.analysis.lwd, position = pd, width = meta.analysis.err) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, 
                  col = CompetitionComparison), position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = strength.df.lim[3] + 0.02, 
                col = CompetitionComparison), size = meta.analysis.text, hjust = 0, vjust = 0.5, 
            fontface = "italic", position = pd) +
  coord_flip(ylim = c(strength.df.lim[1], strength.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(x = "", y = "Mean strength") + theme_classic() + meta.analysis.theme
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
  theme_classic() + cases.theme + coord_flip() + theme(legend.position = c(0.23, 0.8))
strength.combined <- plot_grid(strength, strength.cases, align = "h", axis = "b", label_fontface = "plain",
                                labels = c("A", "B"), rel_widths = relative.widths)
||||||| 2b1eac9
# strength
str.for.df <- final.df[which(final.df$metric == "strength"), ]
str.for.df.lim <- lim.func.for(str.for.df)
strength.forest <- ggplot(data = str.for.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Mean strength") + theme_classic() + 
  scale_shape_manual(values = fp.shape) + fp.theme +
  scale_y_continuous(breaks = fp.scale, limits = c(str.for.df.lim[1], str.for.df.lim[2])) + coord_flip()
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
  FigureAsEPS(fig = strength.combined, fig.name = "Strength", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = strength.combined, fig.name = "Strength", width = width.plot, height = height.plot)
}
<<<<<<< HEAD
#
||||||| 2b1eac9
plot_grid(strength, strength.forest, align = "h", axis = "b",labels = c("a", "b"), rel_widths = c(3, 4))
dev.off()

=======


>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# indirect effect
<<<<<<< HEAD
indirect.effect.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "IndirectEffect"), ]
indirect.effect.df.lim <- SetAxisLimits(indirect.effect.df)
indirect.effect.cases.df <- complete.df[which(complete.df$MetricName == "IndirectEffect"), ]
indirect.effect.cases.df.lim <- SetAxisLimits(indirect.effect.cases.df, is.cases = TRUE)
indirect.effect <- ggplot(data = indirect.effect.df) + 
  facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, 
                  col = CompetitionComparison), position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = indirect.effect.df.lim[3] + 0.03, 
                col = CompetitionComparison), size = meta.analysis.text, hjust = 0, vjust = 0.5, 
            fontface = "italic", position = pd) +
  coord_flip(ylim = c(indirect.effect.df.lim[1], indirect.effect.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(x = "", y = "Mean indirect effect") + theme_classic() + meta.analysis.theme
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
  theme_classic() + cases.theme + coord_flip() + theme(legend.position = c(0.8, 0.85))
indirect.effect.combined <- plot_grid(indirect.effect, indirect.effect.cases, align = "h", axis = "b", 
                                      labels = c("A", "B"), label_fontface = "plain", rel_widths = relative.widths)
||||||| 2b1eac9
ind.for.df <- final.df[which(final.df$metric == "ind eff"), ]
ind.for.df.lim <- lim.func.for(ind.for.df)
ind.eff.forest <- ggplot(data = ind.for.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Mean indirect effect") + theme_classic() + fp.theme + 
  scale_shape_manual(values = fp.shape) + 
  scale_y_continuous(limits = c(ind.for.df.lim[1], ind.for.df.lim[2])) + coord_flip()
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
  FigureAsEPS(fig = indirect.effect.combined, fig.name = "IndEff", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = indirect.effect.combined, fig.name = "IndEff", width = width.plot, height = height.plot)
}
<<<<<<< HEAD
#
# imbalance
imbalance.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "Imbalance"), ]
imbalance.df.lim <- SetAxisLimits(imbalance.df)
imbalance.cases.df <- complete.df[which(complete.df$MetricName == "Imbalance"), ]
imbalance.cases.df.lim <- SetAxisLimits(imbalance.cases.df, is.cases = TRUE)
imbalance <- ggplot(data = imbalance.df)  + facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, shape = ShapeFactor, size = ModelType, 
                  col = CompetitionComparison), position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = imbalance.df.lim[3] - 0.01, 
                col = CompetitionComparison), size = meta.analysis.text, hjust = 0, vjust = 0.5, 
            fontface = "italic", position = pd) +
  coord_flip(ylim = c(imbalance.df.lim[1], imbalance.df.lim[2])) + 
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  labs(x = "", y = "Interaction imbalance") + theme_classic() + meta.analysis.theme
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
  theme_classic() + cases.theme + coord_flip() +
  theme(legend.position = c(0.75, 0.25))
imbalance.combined <- plot_grid(imbalance, imbalance.cases, align = "h", axis = "b", labels = c("A", "B"),
                                label_fontface = "plain", rel_widths = relative.widths)
||||||| 2b1eac9
plot_grid(ind.eff, ind.eff.forest, align = "h", axis = "b",labels = c("a", "b"), rel_widths = c(2, 3))
dev.off()

# asymmetry difference
asym.for.df <- final.df[which(final.df$metric == "asymm"), ]
asym.for.df.lim <- lim.func.for(asym.for.df)
asymm.forest <- ggplot(data = asym.for.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Interaction asymmetry") + theme_classic() + fp.theme +
  scale_shape_manual(values = c(19, 15)) + 
  scale_y_continuous(limits = c(asym.for.df.lim[1], asym.for.df.lim[2])) + 
  coord_flip()
=======

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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
  FigureAsEPS(fig = imbalance.combined, fig.name = "Imbalance", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = imbalance.combined, fig.name = "Imbalance", width = width.plot, height = height.plot)
}
<<<<<<< HEAD
#
# asymmetry (forest plot only)
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
||||||| 2b1eac9
plot_grid(asymm, asymm.forest, align = "h", axis = "b",labels = c("a", "b"), rel_widths = c(2, 3))
dev.off()

#transitivity
ri.for.df <- final.df[which(final.df$metric == "RI"), ]
ri.for.df.lim <- lim.func.for(ri.for.df)
rel.int.forest <- ggplot(data = ri.for.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Relative intransitivity") + theme_classic() + fp.theme +
  scale_shape_manual(values = fp.shape) + 
  scale_y_continuous(breaks = fp.scale, limits = c(ri.for.df.lim[1], ri.for.df.lim[2])) + coord_flip()
=======

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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
  FigureAsEPS(fig = asymmetry.cases, fig.name = "Asymmetry", width = width.plot / 2, height = height.plot - 1)
} else {
<<<<<<< HEAD
  FigureAsTiff(fig = asymmetry.cases, fig.name = "Asymmetry", width = width.plot / 2, height = height.plot - 1)
||||||| 2b1eac9
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Transitivity.tiff", res = tiff.res, units = "in", width = width.plot, height = height.plot)
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
#
# intransitivity
intransitivity.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "RelativeIntransitivity"), ]
intransitivity.df.lim <- SetAxisLimits(intransitivity.df)
intransitivity.cases.df <- complete.df[which(complete.df$MetricName == "RelativeIntransitivity"), ]
intransitivity.cases.df.lim <- SetAxisLimits(intransitivity.cases.df, is.cases = TRUE)
intransitivity <- ggplot(data = intransitivity.df) + 
  facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU,
                    col = CompetitionComparison), width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, size = ModelType, shape = ShapeFactor, 
                col = CompetitionComparison), position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = intransitivity.df.lim[3] - 0.05,
                col = CompetitionComparison), size = meta.analysis.text, hjust = 0, vjust = 0.5, 
            fontface = "italic", position = pd) +
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  coord_flip(ylim = c(intransitivity.df.lim[1], intransitivity.df.lim[2])) + 
  labs(x = "", y = "Relative intransitivity") + theme_classic() + meta.analysis.theme
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
  theme_classic() + cases.theme + coord_flip() + theme(legend.position = c(0.8, 0.8))
intransitivity.combined <- plot_grid(intransitivity, intransitivity.cases, align = "h", axis = "b", 
                                      labels = c("A", "B"), label_fontface = "plain", rel_widths = relative.widths)
if (file.type == 1) {
  FigureAsEPS(fig = intransitivity.combined, fig.name = "Intransitivity", width = width.plot, height = height.plot)
} else {
  FigureAsTiff(fig = intransitivity.combined, fig.name = "Intransitivity", width = width.plot, height = height.plot)
}
#
||||||| 2b1eac9
plot_grid(rel.int, rel.int.forest, align = "h", axis = "b",labels = c("a", "b"), rel_widths = c(2, 3))
dev.off()

=======

>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# connectance
<<<<<<< HEAD
connectance.df <- meta.analysis.df[which(meta.analysis.df$MetricName == "WeightedConnectance"), ]
connectance.df.lim <- SetAxisLimits(connectance.df)
connectance.cases.df <- complete.df[which(complete.df$MetricName == "WeightedConnectance"), ]
connectance.cases.df.lim <- SetAxisLimits(connectance.cases.df, is.cases = TRUE)
connectance <- ggplot(data = connectance.df) + 
  facet_grid(ModelType ~ ., switch = "y", scales = "free", space = "free") +
  geom_errorbar(aes(x = Parameter, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = meta.analysis.lwd, position = pd) + 
  geom_point(aes(x = Parameter, y = MetaAnalysisMean, size = ModelType, shape = ShapeFactor,
                col = CompetitionComparison), position = pd, fill = "white") + 
  geom_text(aes(label = paste("n = ", NumCases, sep = ""), x = Parameter, y = connectance.df.lim[3] + 0.06, 
                col = CompetitionComparison), size = meta.analysis.text, hjust = 0, vjust = 0.5, 
            fontface = "italic", position = pd) +
  scale_y_continuous(breaks = meta.analysis.scale) + 
  scale_color_manual(values = meta.analysis.colors) +
  scale_shape_manual(values = meta.analysis.shapes, guide = FALSE) +
  scale_size_manual(values = meta.analysis.sizes, guide = FALSE) +
  coord_flip(ylim = c(connectance.df.lim[1], connectance.df.lim[2])) + 
  labs(x = "", y = "Weighted connectance") + theme_classic() + meta.analysis.theme
connectance.cases <- ggplot(data = connectance.cases.df) + 
  geom_errorbar(aes(x = Network, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, 
                    col = CompetitionComparison), width = meta.analysis.err, size = cases.lwd, position = pd.less) + 
  geom_point(aes(x = Network, y = MetaAnalysisMean, shape = CompetitionComparison, 
                col = CompetitionComparison), position = pd.less, fill = "white") + 
  labs(x = "", y = "Weighted connectance") + 
  scale_shape_manual(values = cases.shape, name = "Network type") + 
  scale_color_manual(values = meta.analysis.colors, name = "Network type") + 
  scale_y_continuous(limits = c(connectance.cases.df.lim[1], connectance.cases.df.lim[2])) + 
  theme_classic() + cases.theme + coord_flip() +
  theme(legend.position = c(0.23, 0.8))
connectance.combined <- plot_grid(connectance, connectance.cases, align = "h", axis = "b", 
                                  labels = c("A", "B"), label_fontface = "plain",
                                  rel_widths = relative.widths)
||||||| 2b1eac9
con.for.df <- final.df[which(final.df$metric == "connect"), ]
con.for.df.lim <- lim.func.for(con.for.df)
connect.forest <- ggplot(data = con.for.df) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.l.post, ymax = CI.u.post), width = 0.3, size = fp.lwd) + 
  geom_point(aes(x = network, y = est.post, shape = CtrlTreatment)) + 
  labs(x = "", y = "Weighted connectance") + theme_classic() + fp.theme +
  scale_shape_manual(values = fp.shape) + 
  scale_y_continuous(limits = c(con.for.df.lim[1], con.for.df.lim[2])) + coord_flip()
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
<<<<<<< HEAD
  FigureAsEPS(fig = connectance.combined, fig.name = "Connectance", width = width.plot, height = height.plot)
||||||| 2b1eac9
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Connectance.eps", horizontal = FALSE, onefile = FALSE, paper = "special", width = width.plot, height = height.plot)
=======
  FigureAsEPS(fig = connectance.compfac, fig.name = "ConnectanceCompFac", width = width.plot - 1, height = height.plot)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
} else {
<<<<<<< HEAD
  FigureAsTiff(fig = connectance.combined, fig.name = "Connectance", width = width.plot, height = height.plot)
||||||| 2b1eac9
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/Connectance.tiff", res = tiff.res, units = "in", width = width.plot, height = height.plot)
=======
  FigureAsTiff(fig = connectance.compfac, fig.name = "ConnectanceCompFac", width = width.plot - 1, height = height.plot)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
||||||| 2b1eac9
plot_grid(connect, connect.forest, align = "h", axis = "b",labels = c("a", "b"), rel_widths = c(2, 3))
dev.off()


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PCA BIPLOT ------------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
=======


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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
<<<<<<< HEAD
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
||||||| 2b1eac9
=======
# monoculture control
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
<<<<<<< HEAD
  FigureAsEPS(fig = connectance.compfac, fig.name = "ConnectanceCompFac", width = width.plot - 1, height = height.plot)
||||||| 2b1eac9
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/PCA.eps", width = 5, height = 5)
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
} else {
<<<<<<< HEAD
  FigureAsTiff(fig = connectance.compfac, fig.name = "ConnectanceCompFac", width = width.plot - 1, height = height.plot)
||||||| 2b1eac9
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/PCA.tiff", res = tiff.res, units = "in", width = 5, height = 5)
  
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
||||||| 2b1eac9
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
=======



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MODEL SELECTION FIGURES -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
<<<<<<< HEAD
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
||||||| 2b1eac9
# figures for weight
# plot distributions of observed weights and fitted weights for each network
=======

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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
<<<<<<< HEAD
  FigureAsEPS(fig = connectance.cases.compfac, fig.name = "ConnectanceCompFac_Forest", width = width.plot, height = height.plot + 1)
||||||| 2b1eac9
  cairo_ps("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/weight_dist.eps", family = "sans", width = 10, height = height.ms)
=======
  FigureAsEPS(fig = cdf.log, fig.name = "StrengthCDF_LogLog", width = width.model.selection, height = height.model.selection)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
} else {
<<<<<<< HEAD
  FigureAsTiff(fig = connectance.cases.compfac, fig.name = "ConnectanceCompFac_Forest", width = width.plot, height = height.plot + 1)
||||||| 2b1eac9
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/weight_dist.tiff", res = tiff.res, units = "in", width = 10, height = height.ms)
=======
  FigureAsTiff(fig = cdf.log, fig.name = "StrengthCDF_LogLog", width = width.model.selection, height = height.model.selection)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD

all.ma.fig <- plot_grid(strength, indirect.effect, imbalance, intransitivity, connectance, nrow = 2, align = "h", axis = "b", 
                        labels = "AUTO", label_fontface = "plain")
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# PCA BIPLOT FIGURE -----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# load pca scores and loadings
pca.scores.mono.ctrl.df <- read.csv(paste(path, "Output/PCAScores_MonoCtrl.csv", sep = ""), row.names = 1)
pca.loadings.mono.ctrl.df <- read.csv(paste(path, "Output/PCALoadings_MonoCtrl.csv", sep = ""), row.names = 1)
# generate figure
||||||| 2b1eac9
  ggplot(dat = df.w[which(df.w$Values < 1), ], aes(x = Values, fill = Distribution, alpha = Fit)) + 
  facet_wrap(~ Case, scales = "free", nrow = 10) + geom_density(size = 0.1) + 
  scale_fill_manual(values = c("#a2a2a2", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_alpha_manual(values = c(1.0, 0.5, 0.1), guide = FALSE) +
  theme_classic() + ms.theme
dev.off()
# plot CDFs pf observed weights and fitted weights, on log-log scale to observe tail behavior
=======



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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
<<<<<<< HEAD
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
              xlab = paste("PC1 (", round(pca.scores.mono.ctrl.df[1, 4], digits = 2), "% var. explained)", sep = ""), 
              ylab = paste("PC2 (", round(pca.scores.mono.ctrl.df[1, 5], digits = 2), "% var. explained)", sep = ""), 
               arrow.len = 0.06, ylim = c(-4, 4), xlim = c(-6, 6), cex = c(0.5, 1)))
  abline(h = 0, lty = 2, col = gray.hex)
  abline(v = 0, lty = 2, col = gray.hex)
  dev.off()
  embed_fonts(paste(path, "Output/Figures/PCA_MonoCtrl.eps", sep = ""), 
              outfile = paste(path, "Output/Figures/PCA_MonoCtrl_Embed.eps", sep = ""),
              options = "-dEPSCrop")
||||||| 2b1eac9
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/weight_cdf.eps", width = 10, height = height.ms)
=======
  FigureAsEPS(fig = cdf, fig.name = "StrengthCDF", width = width.model.selection, height = height.model.selection)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
} else {
<<<<<<< HEAD
  tiff(paste(path, "Output/Figures/PCA_MonoCtrl.tiff", sep = ""), res = tiff.res, units = "in", width = 5, height = 5)
  par(mar = c(2.5, 1.5, 1.5, 1),  # distance from plot to side of page
      mgp = c(1.2, 0.2, 0),  # distance from plot to label
      las = 1,  # rotate y-axis text
      tck = -0.005,  # reduce tick length
      xaxs = "i", yaxs = "i")  # remove plot padding
  palette(c("black", "#e41a1c"))
  print(biplot(x = pca.scores.mono.ctrl.df[, 2:3], y = pca.loadings.mono.ctrl.df[, 2:3], xlabs = pca.scores.mono.ctrl.df[, 1], 
                ylabs = pca.loadings.mono.ctrl.df[, 1], 
                xlab = paste("PC1 (", round(pca.scores.mono.ctrl.df[1, 4], digits = 2), "% var. explained)", sep = ""), 
                ylab = paste("PC2 (", round(pca.scores.mono.ctrl.df[1, 5], digits = 2), "% var. explained)", sep = ""), 
                arrow.len = 0.06, xlim = c(-3.5, 3.5), ylim = c(-3.5, 3.5), cex = c(0.5, 1)))
  abline(h = 0, lty = 2, col = gray.hex)
  abline(v = 0, lty = 2, col = gray.hex)
  dev.off()
||||||| 2b1eac9
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/weight_cdf.tiff", units = "in", width = 10, height = height.ms, res = tiff.res)
=======
  FigureAsTiff(fig = cdf, fig.name = "StrengthCDF", width = width.model.selection, height = height.model.selection)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# MODEL SELECTION FIGURES -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# figure parameters
width.model.selection <- 8.5
height.model.selection <- 8.5
width.model.selection.ctrl <- 6.75
height.model.selection.ctrl <- 8
model.selection.theme <- theme(axis.title = element_text(size = 12, colour = "black"), 
                                axis.text = element_text(size = 6, colour = "black"), 
                                axis.line = element_line(size = 0.5, colour = "black"),
                                axis.ticks = element_line(colour = "black"),
                                legend.direction = "vertical",
                                legend.title = element_blank(),
                                legend.text = element_text(size = 9, colour = "black", 
                                      margin =  margin(t = 0, r = 0, l = 0, b = 0, unit = "pt")),
                                legend.key.width = unit(0.4, "cm"),
                                legend.key.height = unit(0.4, "cm"),
                                legend.background = element_blank(),
                                legend.position = c(0.9, 0.15),
                                legend.text.align = 0, 
                                legend.spacing.x = unit(0.5, "cm"),
                                panel.grid = element_blank(),
                                strip.placement = "outside", 
                                strip.text = element_text(size = 9, colour = "black"), 
                                strip.background = element_blank(),
                                panel.background = element_blank(), 
                                panel.spacing = unit(0.1, "cm"))
# load data
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
# CDF plot, log scaled
# Monoculture control
in.strength.cdf.log <- ggplot(dat = model.selection.instrength.df, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 5) + 
||||||| 2b1eac9
ggplot(dat = df.w, aes(x = Values, color = Distribution, size = Fit, linetype = Fit)) + facet_wrap(~ Case, scales = "free", nrow = 4) + 
=======


# CDF plot, log-scaled
# True control
in.strength.true.ctrl.cdf.log <- ggplot(dat = model.selection.instrength.true.ctrl, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
<<<<<<< HEAD
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(scaled = TRUE, long = unit(1, "mm"), mid = unit(NA, "mm"), short = unit(NA, "mm")) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + model.selection.theme
out.strength.cdf.log <- ggplot(dat = model.selection.outstrength.df, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 5) + 
||||||| 2b1eac9
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(scaled = TRUE, long = unit(1.5, "mm"), mid = unit(0.5, "mm"), short = unit(NA, "mm")) +
  labs(x = "Weights", y = "complementary CDF") + theme_classic() + ms.theme
dev.off()

# CDF plot
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/instr_cdf.eps", width = width.ms, height = height.ms)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/instr_cdf.tiff", units = "in", width = width.ms, height = height.ms, res = 600)
}
ggplot(dat = df.is, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", nrow = 3) + 
=======
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  annotation_logticks(scaled = TRUE, long = unit(1, "mm"), mid = unit(NA, "mm"), short = unit(NA, "mm")) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + model.selection.theme

out.strength.true.ctrl.cdf.log <- ggplot(dat = model.selection.outstrength.true.ctrl, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
<<<<<<< HEAD
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
||||||| 2b1eac9
  annotation_logticks(scaled = TRUE, long = unit(1.5, "mm"), mid = unit(0.5, "mm"), short = unit(NA, "mm")) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + ms.theme
dev.off()

# CDF plot
=======
  annotation_logticks(scaled = TRUE, long = unit(1, "mm"), mid = unit(NA, "mm"), short = unit(NA, "mm")) +
  labs(x = "Out-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = "none")
cdf.truectrl.log <- plot_grid(in.strength.true.ctrl.cdf.log, out.strength.true.ctrl.cdf.log, ncol = 1, align = "v", axis = "b", labels = c("A", "B"), 
                              label_fontface = "plain")
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
<<<<<<< HEAD
  FigureAsEPS(fig = cdf, fig.name = "StrengthCDF", width = width.model.selection, height = height.model.selection)
||||||| 2b1eac9
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/outstr_cdf.eps", width = width.ms, height = height.ms)
=======
  FigureAsEPS(fig = cdf.truectrl.log, fig.name = "StrengthCDF_TrueCtrlOnly_LogLog", width = width.model.selection - 1.5, height = height.model.selection - 1)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
} else {
<<<<<<< HEAD
  FigureAsTiff(fig = cdf, fig.name = "StrengthCDF", width = width.model.selection, height = height.model.selection)
||||||| 2b1eac9
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/outstr_cdf.tiff", units = "in", width = width.ms, height = height.ms, res = tiff.res)
=======
  FigureAsTiff(fig = cdf.truectrl.log, fig.name = "StrengthCDF_TrueCtrlOnly_LogLog", width = width.model.selection - 1.5, height = height.model.selection - 1)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
#
# CDF plot, log-scaled
# True control
in.strength.true.ctrl.cdf.log <- ggplot(dat = model.selection.instrength.true.ctrl, 
                                          aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
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
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = c(0.8, 0.2))
out.strength.true.ctrl.cdf.log <- ggplot(dat = model.selection.outstrength.true.ctrl, 
                                          aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
||||||| 2b1eac9
ggplot(dat = df.os, aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", nrow = 3) + 
=======


# CDF plot, not log-scaled
# True control
in.strength.true.ctrl.cdf <- ggplot(dat = model.selection.instrength.true.ctrl[which(model.selection.instrength.true.ctrl$Values < 10), ], aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
<<<<<<< HEAD
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) + 
  annotation_logticks(scaled = TRUE, long = unit(1, "mm"), mid = unit(NA, "mm"), short = unit(NA, "mm")) +
  labs(x = "Out-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = "none")
cdf.truectrl.log <- plot_grid(in.strength.true.ctrl.cdf.log, out.strength.true.ctrl.cdf.log, ncol = 1, align = "hv", axis = "b",
                              labels = c("A", "B"), label_fontface = "plain")
if (file.type == 1) {
  FigureAsEPS(fig = cdf.truectrl.log, fig.name = "StrengthCDF_TrueCtrlOnly_LogLog", width = width.model.selection.ctrl,
                  height = height.model.selection.ctrl)
} else {
  FigureAsTiff(fig = cdf.truectrl.log, fig.name = "StrengthCDF_TrueCtrlOnly_LogLog", width = width.model.selection.ctrl, 
                  height = height.model.selection.ctrl)
}
||||||| 2b1eac9
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
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
#
<<<<<<< HEAD
# CDF plot, not log-scaled
# True control
in.strength.true.ctrl.cdf <- ggplot(dat = model.selection.instrength.true.ctrl[which(model.selection.instrength.true.ctrl$Values < 10), ], 
                                      aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  labs(x = "In-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = c(0.8, 0.2))
out.strength.true.ctrl.cdf <- ggplot(dat = model.selection.outstrength.true.ctrl[which(model.selection.outstrength.true.ctrl$Values < 10), ], 
                                        aes(x = Values, col = Distribution, size = Fit, linetype = Fit)) + 
  facet_wrap(~ Case, scales = "free", ncol = 4) + 
  geom_line(aes(y = 1 - ..y..), stat = "ecdf") +
  scale_color_manual(values = c("#747474", "#e41a1c", "#377eb8", "#4daf4a", "#984ea3")) + 
  scale_size_manual(values = c(1.0, 1.3, 0.6), guide = FALSE) + 
  scale_linetype_manual(values = c("solid", "solid", "dashed"), guide = FALSE) +
  labs(x = "Out-strength", y = "complementary CDF") + theme_classic() + model.selection.theme + theme(legend.position = "none")
cdf.truectrl <- plot_grid(in.strength.true.ctrl.cdf, out.strength.true.ctrl.cdf, ncol = 1, align = "hv", axis = "b", labels = c("A", "B"), 
                          label_fontface = "plain")
||||||| 2b1eac9
=======
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
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
if (file.type == 1) {
<<<<<<< HEAD
  FigureAsEPS(fig = cdf.truectrl, fig.name = "StrengthCDF_TrueCtrlOnly", width = width.model.selection.ctrl, 
                height = height.model.selection.ctrl)
||||||| 2b1eac9
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/sppCharDiff.eps", width = 4, height = 4)
=======
  FigureAsEPS(fig = species.character.plot, fig.name = "SpeciesCharacterDifferences", width = width.plot - 2, height = height.plot)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
} else {
<<<<<<< HEAD
  FigureAsTiff(fig = cdf.truectrl, fig.name = "StrengthCDF_TrueCtrlOnly", width = width.model.selection.ctrl, 
                height = height.model.selection.ctrl)
||||||| 2b1eac9
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/sppCharDiff.tiff", width = 4, height = 4, units = "in", res = tiff.res)
=======
  FigureAsTiff(fig = species.character.plot, fig.name = "SpeciesCharacterDifferences", width = width.plot - 2, height = height.plot)
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
}
<<<<<<< HEAD
#
||||||| 2b1eac9
ggplot(dat = df.char[which(df.char$metric == "In-strength" | df.char$metric == "Out-strength"), ]) + 
  facet_wrap(~ metric, scales = "free") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = comparison, ymin = CILL, ymax = CIUL), width = 0.2, size = fp.lwd) + 
  geom_point(aes(x = comparison, y = mean)) + labs(x = "", y = "Difference in strength")  + theme_classic() + ma.theme + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

=======



>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SPECIES CHARACTERS FIGURE ---------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
<<<<<<< HEAD
#
# load data
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

sp.char <- species.character.df
sp.char[, c(4, 6, 7)] <- round(sp.char[, c(4, 6, 7)], digits = 2)
sp.char <- sp.char[,c(11, 1:4, 6, 7)]
subset(sp.char, CompetitionComparison == "Monoculture" & Comparison == "N-fixing ability")
# create figure
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
# NETWORK TREATMENTS FIGURE ---------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
# load data
# comparing metrics from networks wth treatment/control
treatment.df <- read.csv(paste(path, "Output/ControlTreatmentNetworkMetrics_MonoCtrl.csv", sep = ""), row.names = 1)
treatment.df$TreatmentType <- factor(treatment.df$TreatmentType, levels = c("Control", "Decreased nutrients", "Increased nutrients", "Decreased water", "Increased water", "Warming"))
levels(treatment.df$MetricName) <- c("Imbal", "Ind Eff", "Strength", "RI", "Connect")
treatment.df$MetricName <- factor(treatment.df$MetricName, levels = c("Strength", "Ind Eff", "Imbal", "RI", "Connect"))
# create figure
treatment.fig <- ggplot(dat = treatment.df) + facet_wrap(~ Network, scales = "free", nrow = 2) +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = MetricName, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = TreatmentType), position = pd.less, width = 0.3, size = cases.lwd) + 
  geom_point(aes(x = MetricName, y = MetaAnalysisMean, col = TreatmentType), size = 0.8, position = pd.less) + 
||||||| 2b1eac9
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/networkTreatments.eps", width = 8, height = 5)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/networkTreatments.tiff", width = 8, height = 5, units = "in", res = tiff.res)
}
  ggplot(dat = df.treat) + facet_wrap(~ network, scales = "free", nrow = 2) +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = metric, ymin = CI.l.post, ymax = CI.u.post, col = treatmentType), position = pd.less, width = 0.2, size = fp.lwd) + 
  geom_point(aes(x = metric, y = est.post, col = treatmentType), size = 0.8, position = pd.less) + 
=======
treatment.fig <- ggplot(dat = treatment.df) + facet_wrap(~ Network, scales = "free", nrow = 2) +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = MetricName, ymin = MetaAnalysisCIL, ymax = MetaAnalysisCIU, col = TreatmentType), position = pd.less, width = 0.3, size = cases.lwd) + 
  geom_point(aes(x = MetricName, y = MetaAnalysisMean, col = TreatmentType), size = 0.8, position = pd.less) + 
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  scale_color_manual(values = c("#000000", "#b2df8a", "#33a02c", "#a6cee3", "#1f78b4", "#e31a1c")) + 
  labs(x = "", y = "") + theme_classic() +
  theme(legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(size = 10, angle = 90, hjust = 1), axis.text.y = element_text(size = 10), axis.title = element_text(size = 12), 
        axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 11), strip.background = element_blank(),
        panel.background = element_blank(), panel.spacing = unit(0.3, "cm"))
<<<<<<< HEAD
if (file.type == 1) {
  FigureAsEPS(fig = treatment.fig, fig.name = "NetworkTreatments", width = 8, height = 5)
} else {
  FigureAsTiff(fig = treatment.fig, fig.name = "NetworkTreatments", width = 8, height = 5)
}
#
||||||| 2b1eac9
dev.off()

=======
if (file.type == 1) {
  FigureAsEPS(fig = treatment.fig, fig.name = "NetworkTreatments", width = 8, height = 5)
} else {
  FigureAsTiff(fig = treatment.fig, fig.name = "NetworkTreatments", width = 8, height = 5)
}


>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# SPP. POSITION FIGURE -------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
<<<<<<< HEAD
# load data
# comparing spp. position
species.position.df <- read.csv(paste(path, "Output/SpeciesPosition.csv", sep = ""), row.names = 1)
levels(species.position.df$Network) <- c("Sangakk. & Roberts 1985", "Gurevitch et al. 1990", "Mariotte et al. 2012", 
                                         "Fortner & Weltzin 2007", "Hendriks et al. 2015", "Miller & Werner 1987", 
                                         "Engel & Weltzin 2008", "Goldberg & Landa 1991")
species.position.df$Network <- factor(species.position.df$Network, levels = c("Sangakk. & Roberts 1985", "Miller & Werner 1987", "Engel & Weltzin 2008", 
                                                                              "Mariotte et al. 2012", "Hendriks et al. 2015", "Gurevitch et al. 1990",
                                                                              "Goldberg & Landa 1991", "Fortner & Weltzin 2007"))
levels(species.position.df$Species) <- c("Dactylis glomerata", "Plantago lanceolata", "Trifolium repens")
# create figure
species.position.fig <- ggplot(dat = species.position.df) + facet_grid(MetricName ~ Species, scales = "free_x") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = BootstrapCIL, ymax = BootstrapCIU, col = Network), width = 0.2, size = cases.lwd) + 
  geom_point(aes(x = Network, y = BootstrapMean, col = Network)) + labs(x = "", y = "Strength")  + 
||||||| 2b1eac9
if (file.type == 1) {
  postscript("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/sppPosition.eps", width = 5, height = 5)
} else {
  tiff("/Users/nicolekinlock/Documents/NetworkMetaAnalysis/Figures/sppPosition.tiff", width = 5, height = 5, units = "in", res = tiff.res)
}
ggplot(dat = df.spp) + facet_grid(metric ~ species, scales = "free_x") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = fp.lwd) + 
  geom_errorbar(aes(x = network, ymin = CI.L, ymax = CI.U, col = network), width = 0.2, size = fp.lwd) + 
  geom_point(aes(x = network, y = mean, col = network)) + labs(x = "", y = "Strength")  + 
=======
species.position.fig <- ggplot(dat = species.position.df) + facet_grid(MetricName ~ Species, scales = "free_x") +
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = cases.lwd) + 
  geom_errorbar(aes(x = Network, ymin = BootstrapCIL, ymax = BootstrapCIU, col = Network), width = 0.2, size = cases.lwd) + 
  geom_point(aes(x = Network, y = BootstrapMean, col = Network)) + labs(x = "", y = "Strength")  + 
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  scale_color_manual(values = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628", "#f781bf", "#999999"), guide = guide_legend(nrow = 4)) +
  theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_text(size = 10), legend.position = "bottom", 
        axis.text.x = element_blank(), axis.text.y = element_text(size = 12), axis.title = element_text(size = 12), 
        axis.line = element_line(size = 0.5), strip.placement = "outside", strip.text = element_text(size = 12), 
<<<<<<< HEAD
        strip.background = element_blank(), panel.background = element_blank(), panel.spacing = unit(0.5, "cm"),
        axis.ticks.x = element_blank())
if (file.type == 1) {
  FigureAsEPS(fig = species.position.fig, fig.name = "SpeciesPosition", width = 5, height = 5)
} else {
  FigureAsTiff(fig = species.position.fig, fig.name = "SpeciesPosition", width = 5, height = 5)
}
#
||||||| 2b1eac9
        strip.background = element_blank(), panel.background = element_blank(), panel.spacing = unit(0.5, "cm"))
dev.off()

=======
        strip.background = element_blank(), panel.background = element_blank(), panel.spacing = unit(0.5, "cm"),
        axis.ticks.x = element_blank())
if (file.type == 1) {
  FigureAsEPS(fig = species.position.fig, fig.name = "SpeciesPosition", width = 5, height = 5)
} else {
  FigureAsTiff(fig = species.position.fig, fig.name = "SpeciesPosition", width = 5, height = 5)
}



>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# ADDITIVITY FIGURE -----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#
<<<<<<< HEAD
# load data
# comparing standardized differences in RII in pairwise and 3 spp. combinations
additive.df <- read.csv(paste(path, "Output/AdditivityFit.csv", sep = ""), row.names = 1)
additive.df$Parameter <- factor(additive.df$Parameter, levels = c("grand mean", "group mean", "among-group sd", "within-group sd"))
levels(additive.df$Network) <- c("Baude et al. 2011", "Pausch et al. 2013", "Marty et al. 2009", "Frérot et al. 2006", "All")
additive.df$Network <- factor(additive.df$Network, levels = c("All", "Frérot et al. 2006", "Marty et al. 2009", "Baude et al. 2011", "Pausch et al. 2013"))
# create figure
additive.fig <- ggplot(dat = additive.df[which(additive.df$Parameter == "grand mean" | additive.df$Parameter == "group mean" ), ]) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Network, ymin = CIL, ymax = CIU), width = 0.2, size = meta.analysis.lwd) + 
  geom_point(aes(x = Network, y = Mean, shape = Parameter, size = Parameter),  fill = "white") + 
  scale_size_manual(values = meta.analysis.sizes) + scale_shape_manual(values = meta.analysis.shapes) +
||||||| 2b1eac9
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
=======
additive.fig <- ggplot(dat = additive.df[which(additive.df$Parameter == "grand mean" | additive.df$Parameter == "group mean" ), ]) + 
  geom_hline(yintercept = 0, lty = "dashed", color = gray.hex, size = meta.analysis.lwd) + 
  geom_errorbar(aes(x = Network, ymin = CIL, ymax = CIU), width = 0.2, size = meta.analysis.lwd) + 
  geom_point(aes(x = Network, y = Mean, shape = Parameter, size = Parameter),  fill = "white") + 
  scale_size_manual(values = meta.analysis.sizes) + scale_shape_manual(values = meta.analysis.shapes) +
>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae
  labs(x = "", y = "Standardized mean difference in RII") + theme_classic() + 
  theme(legend.title = element_blank(), legend.text = element_blank(), legend.position = "none", 
        axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 9), axis.title = element_text(size = 11), 
        axis.line = element_line(size = 0.5)) + coord_flip()
<<<<<<< HEAD
if (file.type == 1) {
  FigureAsEPS(fig = additive.fig, fig.name = "Additivity", width = 3.5, height = 3)
} else {
  FigureAsTiff(fig = additive.fig, fig.name = "Additivity", width = 3.5, height = 3)
}
||||||| 2b1eac9
dev.off()


=======
if (file.type == 1) {
  FigureAsEPS(fig = additive.fig, fig.name = "Additivity", width = 3.5, height = 3)
} else {
  FigureAsTiff(fig = additive.fig, fig.name = "Additivity", width = 3.5, height = 3)
}


>>>>>>> d0b20ce549ee72e41954487b19eddc467922a8ae

