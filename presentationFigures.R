library(igraph)
library(ggplot2)
library(reshape2)
library(plyr)
library(poweRlaw)
library(cowplot)
#
# figures for conceptual purposes
#
# understanding networks: nodes-only, unweighted, and weighted

#nodes only
M <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)
png("Figures/NodesOnly.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("i", "j", "k"), edge.curved = c(0, 0.5, 0, 0.5), edge.arrow.size = 0.5, 
            edge.arrow.width = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()
# unweighted
M <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)
png("Figures/BinaryGraph.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("i", "j", "k"), edge.curved = c(0, 0.5, 0, 0.5), edge.arrow.size = 0.5, 
            edge.arrow.width = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()
# weighted
M <- matrix(c(0, 0.3, 0, 0, 0, 1.2, 2.5, 0.1, 0), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)
png("Figures/WeightedGraph.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("i", "j", "k"), edge.width = E(g)$weight, edge.curved = c(0, 0.5, 0, 0.5), 
            edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()
# hypothetical empirical network
M <- matrix(c(0.25, 0.3, 0.67, 0.2, 0.9, 1.2, 2.5, 0.1, 0.2), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)
png("Figures/EmpiricalTest.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("1", "2", "3"), edge.width = E(g)$weight, edge.curved = 0.5, 
            edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()
#
# illustrating different levels of connectance in networks
#
# low connectance
s <- 10
M <- matrix(rbinom(n = s^2, size = 1, prob = 0.1), nrow = s, ncol = s, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed")
png("Figures/LowConnectance.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 10, vertex.label = NA, edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "black", 
            layout = layout_in_circle, edge.color = "black")
dev.off()
# high connectance
M <- matrix(rbinom(n = s^2, size = 1, prob = 0.8), nrow = s, ncol = s, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed")
png("Figures/HighConnectance.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 10, vertex.label = NA, edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "black", 
            layout = layout_in_circle, edge.color = "black")
dev.off()
#
# illustrating different distributions for strengths
#
# normal distribution
norm <- data.frame(Sample = rnorm(n = 10000, mean = 1, sd = 0.5))
png("Figures/Normal.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(norm, aes(x = Sample)) + geom_density(fill = "gray70") + labs(x = "", y = "") +
  theme(panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()
# exponential distribution
exp <- data.frame(Sample = rexp(n = 10000, rate = 0.9))
png("Figures/Exponential.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(exp, aes(x = Sample)) + geom_density(fill = "gray70") + labs(x = "", y = "") +
  theme(panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()
# power law
m = conpl$new()
m$setXmin(1);m$setPars(2.5)
pow <- data.frame(Sample = dist_rand(m, 10000))
png("Figures/PowerLaw.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(pow, aes(x = Sample)) + geom_density(fill = "gray70") + labs(x = "", y = "") +
  coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
  theme(panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()

# empirical network: Farrer et al 2011
M <- matrix(c(-0.29950939, -0.07204243, -0.14778142, -0.19474420, -0.61149001, -0.26150240, -0.23253725, -0.20677984,-0.26229918), nrow = 3, ncol = 3)
g <- graph.adjacency(abs(M), mode = "directed", weighted = TRUE)
png("Figures/Farrer.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, edge.width = E(g)$weight * 3, edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "#78c679", 
            layout = layout_in_circle, edge.color = "black")
dev.off()

#
# illustrating different levels of asymmetry in networks
#
# strong asymmetric
w.ij <- rnorm(n = 50, mean = 2, sd = 1)
w.ji <- -w.ij + rnorm(n = 50, mean = 0, sd = 0.5)
highasymmetry <- data.frame(w.ij, w.ji)
png("Figures/HighAsymmetry.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(highasymmetry, aes(x = w.ij, y = w.ji)) + geom_point(size = 1) + labs(x = "", y = "") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.line.x = element_line(color="black", size = 0.5), 
        axis.line.y = element_line(color = "black", size = 0.5), 
        panel.background = element_rect(fill = "transparent",colour = NA), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()
# weak asymmetric
w.ij <- rnorm(n = 50, mean = 2, sd = 1)
w.ji <- -w.ij + rnorm(n = 50, mean = 0, sd = 1.5)
lowasymmetry <- data.frame(w.ij, w.ji)
png("Figures/LowAsymmetry.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(lowasymmetry, aes(x = w.ij, y = w.ji)) + geom_point(size = 1) + labs(x = "", y = "") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.line.x = element_line(color="black", size = 0.5), 
        axis.line.y = element_line(color = "black", size = 0.5), 
        panel.background = element_rect(fill = "transparent",colour = NA), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()
# weak symmetric
w.ij <- rnorm(n = 50, mean = 2, sd = 1)
w.ji <- w.ij + rnorm(n = 50, mean = 0, sd = 1.5)
lowasymmetry <- data.frame(w.ij, w.ji)
png("Figures/PosLowAsymmetry.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(lowasymmetry, aes(x = w.ij, y = w.ji)) + geom_point(size = 1) + labs(x = "", y = "") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.line.x = element_line(color="black", size = 0.5), 
        axis.line.y = element_line(color = "black", size = 0.5), 
        panel.background = element_rect(fill = "transparent",colour = NA), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()
# strong symmetric
w.ij <- rnorm(n = 50, mean = 2, sd = 1)
w.ji <- w.ij + rnorm(n = 50, mean = 0, sd = 0.5)
lowasymmetry <- data.frame(w.ij, w.ji)
png("Figures/PosHighAsymmetry.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(lowasymmetry, aes(x = w.ij, y = w.ji)) + geom_point(size = 1) + labs(x = "", y = "") +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.line.x = element_line(color="black", size = 0.5), 
        axis.line.y = element_line(color = "black", size = 0.5), 
        panel.background = element_rect(fill = "transparent",colour = NA), 
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()

# how to calculate strength of one species
M <- matrix(c(0, 0.3, 0, 0.2, 0.9, 1.2, 0, 0.1, 0), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)
strength(graph = g, mode = "in")
strength(graph = g, mode = "out")
png("Figures/Strength.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("1", "2", "3"), vertex.label.color = "black", edge.label.color = "black", edge.width = E(g)$weight, 
            edge.curved = 0.5, edge.arrow.size = 0.5, edge.arrow.width = 0.8, edge.label = E(g)$weight, edge.label.cex = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()

#
# figures that illustrate results
#
# load data
# data table with categorical information extracted from studies
df.init <- read.csv("CodingTable.csv")
col.remove <- c("Title", "doi", "Abstract", "Status", "OtherTreatments", "Metric", "Variation", "Variation.1")  # unwanted columns
df <- df.init[df.init$Status == "Complete" & df.init$RII == 1, !(names(df.init) %in% col.remove)]  # only completed cases with RII
df$Habitat <- as.character(df$Habitat)
df$Habitat[which(df$Habitat == "Coniferous forest" | df$Habitat == "Deciduous forest" | df$Habitat == "Plantation forest" | df$Habitat == "Tropical forest")] <- "Forest"
df$Habitat[which(df$Habitat == "Chaparral")] <- "Grassland"
df$Habitat <- factor(df$Habitat, levels = c("Grassland", "Old field", "Forest", "Estuarine", "Agricultural", "Urban", "Desert"))

# map of study locations
world <- map_data("world")
worldmap <- ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = NA, color = "black", size = 0.3) + coord_fixed(1.3) + 
  labs(x = "Longitude", y = "Latitude") + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), 
                                                axis.ticks.x = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank(), 
                                                axis.ticks.y = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
                                                panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
png("Figures/Map.png", width = 5, height = 4, units = 'in', res = 300, bg = "transparent")
worldmap + geom_point(data = df, aes(x = Longitude, y = Latitude), color = "deeppink", size = 0.3)
dev.off()
# frequency grouped by habitat
habitat <- ggplot(data = df, aes(x = Habitat)) + geom_bar(fill = "#688070", width = 0.5) + labs(y = "", x = "") + coord_cartesian(ylim = c(0, 15)) +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color = "black", size = 0.5), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))

# frequency grouped by greenhouse/garden/field
greenhouse <- ggplot(data = df, aes(x = ExperimentClass)) + geom_bar(fill = "#688070", width = 0.5) + labs(y = "", x = "") + coord_cartesian(ylim = c(0, 15)) +
  theme(text = element_text(size = 14), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color = "black", size = 0.5), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))

# frequency grouped by woody/herbaceous
png("Figures/WoodyHerbaceous.png", width = 3, height = 3, units = 'in', res = 300, bg = "transparent")
ggplot(data = df, aes(x = WoodyHerbaceous)) + geom_bar(fill = "#688070", width = 0.25) + labs(y = "", x = "") + coord_cartesian(ylim = c(0, 15)) +
  theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color = "black", size = 0.5), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()
# frequency grouped by life span
png("Figures/LifeSpan.png", width = 3, height = 3, units = 'in', res = 300, bg = "transparent")
ggplot(data = df, aes(x = LifeSpan)) + geom_bar(fill = "#688070") + labs(y = "", x = "Life span") + coord_cartesian(ylim = c(0, 15)) +
  theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()
# frequency grouped by experiment type
experiment <- ggplot(data = df, aes(x = MixMono..Cntrl)) + geom_bar(fill = "#688070", width = 0.5) + labs(y = "", x = "") + coord_cartesian(ylim = c(0, 15)) +
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5), panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))

bottom_row <- plot_grid(experiment, greenhouse, labels = c('B', 'C'), align = 'h', rel_widths = c(1, 1.3))
png("Figures/DescriptiveStats.png", width = 5, height = 6, units = 'in', res = 300, bg = "transparent")
plot_grid(habitat, bottom_row, labels = c('A', ''), ncol = 1, rel_heights = c(1, 1))
dev.off()

#
# load coding table with extracted data from each case
#
# grand means and 95% CIs from meta-analyses
overallmu <- read.csv("overallmu.csv", stringsAsFactors = FALSE)  # grand means (mu) from all studies together
overallmu$Comparison <- rep("Grand mean", nrow(overallmu))
overallmu[which(overallmu$name == "mu"), "name"] <- "All studies"
overallmu$n <- rep(31, nrow(overallmu))
byexperiment <- read.csv("byexperiment.csv", stringsAsFactors = FALSE)  # grand mean when grouped by experiment type
byexperiment$Comparison <- rep("Experiment type", nrow(byexperiment))
byexperiment$n <- NULL
byexperiment[which(byexperiment$name == "beta1"), "name"] <- "Cntrl"
byexperiment[which(byexperiment$name == "Cntrl"), "n"] <- 13
byexperiment[which(byexperiment$name == "beta2"), "name"] <- "MixMono"
byexperiment[which(byexperiment$name == "MixMono"), "n"] <- 17
byhabitat <- read.csv("byhabitat.csv", stringsAsFactors = FALSE)   # grand mean when grouped by habitat (old field/grassland or other)
byhabitat$n <- NULL
byhabitat[which(byhabitat$name == "beta1"), "name"] <- "Old field"
byhabitat[which(byhabitat$name == "Old field"), "n"] <- 19
byhabitat[which(byhabitat$name == "beta2"), "name"] <- "Other"
byhabitat[which(byhabitat$name == "Other"), "n"] <- 11
byhabitat$Comparison <- rep("Habitat", nrow(byhabitat))
bygreenhouse <- read.csv("bygreenhouse.csv", stringsAsFactors = FALSE)   # grand mean when grouped by experiment setting (greenhouse, garden, or field) - no pots in field
bygreenhouse$n <- NULL
bygreenhouse[which(bygreenhouse$name == "beta1"), "name"] <- "GH"
bygreenhouse[which(bygreenhouse$name == "GH"), "n"] <- 12
bygreenhouse[which(bygreenhouse$name == "beta2"), "name"] <- "Garden"
bygreenhouse[which(bygreenhouse$name == "Garden"), "n"] <- 12
bygreenhouse[which(bygreenhouse$name == "beta3"), "name"] <- "Field"
bygreenhouse[which(bygreenhouse$name == "Field"), "n"] <- 7
bygreenhouse$Comparison <- rep("Setting", nrow(bygreenhouse))
meta <- rbind(overallmu, byexperiment, byhabitat, bygreenhouse)
meta <- meta[, -1]
meta$Comparison <- factor(meta$Comparison, levels = c("Grand mean", "Experiment type", "Habitat", "Setting"))
meta$metric <- as.factor(meta$metric)
meta$name <- factor(meta$name, levels = c("All studies", "Cntrl", "MixMono", "GH", "Garden", "Field", "Old field", "Other"))


# means and SEs from each study
C.mean <- read.csv("meansRIICntrl.csv", row.names = 1)  # control studies
C.se <- read.csv("sdsRIICntrl.csv", row.names = 1)
K.mean <- read.csv("meansRIIKinlock.csv", row.names = 1)  # my study
K.se <- read.csv("sdsRIIKinlock.csv", row.names = 1)
C.mean <- rbind(C.mean, as.vector(t(K.mean)))
C.mean$ExperimentType <- rep("Cntrl", nrow(C.mean))
C.se <- rbind(C.se, as.vector(t(K.se)))
M.mean <- read.csv("meansRIIMixMono.csv", row.names = 1)  # mixture monoculture only
M.mean$ExperimentType <- rep("MixMono", nrow(M.mean))
M.se <- read.csv("sdsRIIMixMono.csv", row.names = 1)
metrics.mean <- rbind(C.mean, M.mean)
case <- substr(x = rownames(metrics.mean), start = 1, stop = nchar(rownames(metrics.mean)) - 4)
metrics.mean$Case <- case
metrics.mean[14, "Case"] <- "Kinlock-2016"
metrics.mean[17, "Case"] <- "1454-PfeiferMeister-2008"
metrics.mean[12, "Case"] <- "7-Lof-2014"
metrics.mean[9, "Case"] <- "1787-Weigelt-2002"
metrics.mean[25, "Case"] <- "192-Amanullah-2013"
metrics.mean[21, "Case"] <- "1566-Niu-2008"
metrics.mean[6, "Case"] <- "1448-Gurevitch-1990"
metrics.mean[27, "Case"] <- "236-Jiang-2014"
metrics.mean <- melt(metrics.mean, variable.name = "Metric", value.name = "Mean")
metrics.se <- rbind(C.se, M.se)
metrics.se <- melt(metrics.se, value.name = "SE")
metrics <- data.frame(metrics.mean, SE.l = metrics.mean$Mean - metrics.se$SE, SE.u = metrics.mean$Mean + metrics.se$SE)
metrics$ExperimentType <- factor(metrics$ExperimentType, levels = c("Cntrl", "MixMono"))

# plots with results
# forest plot of all studies and grand means for each metric
#
#
# asymmetry
png("Figures/AsymmetryResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "r"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.61), size = 2.5) +
  labs(x = "", y = "Asymmetry") + theme_classic()
dev.off()
png("Figures/AsymmetryForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "r"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Asymmetry") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "r")]) +
  coord_flip()
dev.off()

# transitivity
png("Figures/TransitivityResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "relative_intransitivity"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.02), size = 2.5) +
  labs(x = "", y = "Intransitivity") + theme_classic()
dev.off()
png("Figures/TransitivityForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "relative_intransitivity"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Intransitivity") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "relative_intransitivity")]) +
  coord_flip()
dev.off()

# mean strength
png("Figures/StrengthResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "strength"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.02), size = 2.5) +
  labs(x = "", y = "Mean strength") + theme_classic()
dev.off()
png("Figures/StrengthForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "strength"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Mean strength") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "strength")]) +
  coord_flip()
dev.off()

# mean competitive strength
png("Figures/CompStrengthResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "comp_strength"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.02), size = 2.5) +
  labs(x = "", y = "Mean competitive strength") + theme_classic()
dev.off()
png("Figures/CompStrengthForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "comp_strength"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Mean competitive strength") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "comp_strength")]) +
  coord_flip()
dev.off()

# mean facilitative strength
png("Figures/FacStrengthResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "fac_strength"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.02), size = 2.5) +
  labs(x = "", y = "Mean facilitative strength") + theme_classic()
dev.off()
png("Figures/FacStrengthForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "fac_strength"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Mean facilitative strength") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "fac_strength")]) +
  coord_flip()
dev.off()

# mean weight
png("Figures/WeightResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "weight"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.02), size = 2.5) +
  labs(x = "", y = "Mean weight") + theme_classic()
dev.off()
png("Figures/WeightForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "weight"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Mean weight") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "weight")]) +
  coord_flip()
dev.off()

# indirect effect
png("Figures/IndEffResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "indirect_effect"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.02), size = 2.5) +
  labs(x = "", y = "Mean indirect effect") + theme_classic()
dev.off()
png("Figures/IndEffForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "indirect_effect"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Mean weight") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "indirect_effect")]) +
  coord_flip()
dev.off()

# connectance
png("Figures/ConnectanceResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "connectance"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.02), size = 2.5) +
  labs(x = "", y = "Connectance") + theme_classic()
dev.off()
png("Figures/ConnectanceForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "connectance"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Connectance") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "connectance")]) +
  coord_flip()
dev.off()
# plot species richness of networks vs. connectance
connectance <- metrics[which(metrics$Metric == "connectance"), ]
spp.count <- c(3, 3, 3, 10, 4, 3, 5, 4, 3, 4, 3, 8, 7, 7, 3, 3, 4, 3, 3, 3, 6, 8, 3, 3, 3, 4, 6, 4, 5, 3, 7)
connectance$sppCount <- spp.count
png("Figures/ConnectanceSpp.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = connectance) + geom_point(aes(x = sppCount, y = Mean, color = ExperimentType)) + 
  labs(x = "Species richness", y = "Connectance") + 
  theme_classic() + theme(legend.title = element_blank()) 
dev.off()

# linkage diversity
png("Figures/LinkDivResults.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = meta[which(meta$metric == "linkage_diversity"), ]) + geom_bar(stat = "identity", aes(x = name, y = est, fill = Comparison)) + 
  geom_errorbar(aes(x = name, ymin = CI.l, ymax = CI.h, width = 0.4)) + geom_text(aes(label = paste("n = ", n, sep = ""), x = name, y = -0.02), size = 2.5) +
  labs(x = "", y = "Linkage diversity") + theme_classic()
dev.off()
png("Figures/LinkDivForest.png", width = 6, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = metrics[which(metrics$Metric == "linkage_diversity"), ]) + geom_point(aes(x = Case, y = Mean, color = ExperimentType)) + 
  geom_errorbar(aes(x = Case, ymin = SE.l, ymax = SE.u, color = ExperimentType, width = 0.4)) + labs(x = "", y = "Linkage diversity") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + theme_classic() + theme(legend.title = element_blank()) + scale_x_discrete(limits = metrics$Case[which(metrics$Metric == "linkage_diversity")]) +
  coord_flip()
dev.off()

# treatments
# load data table from studies with unique treatments
Treat.mean <- read.csv("TreatmeansRII.csv", row.names = 1)
Treat.mean <- Treat.mean[, -c(6, 8)]
Treat.se <- read.csv("TreatsdsRII.csv", row.names = 1)
Treat.se <- Treat.se[, -c(6, 8)]
Treat.mean$Treatment <- c("Low nutrient", "Warming", "Low water", "Added nutrient", "Added water", "Added nutrient", "Low water")
Treat.mean$Case <- NA
Treat.mean[, "Case"] <- c("1454-PfeiferMeister-2008", "1566-Niu-2008", "192-Amanullah-2013", "236-Jiang-2014", "236-Jiang-2014", "1448-Gurevitch-1990", "1787-Weigelt-2002")
Treat.mean <- melt(Treat.mean, variable.name = "Metric", value.name = "Mean")
Treat.se <- melt(Treat.se, value.name = "SE")
Treat.init <- data.frame(Treat.mean, SE.l = Treat.mean$Mean - Treat.se$SE, SE.u = Treat.mean$Mean + Treat.se$SE)
Treat.init <- Treat.init[which(Treat.init$Metric != "comp_strength" & Treat.init$Metric != "fac_strength" & Treat.init$Metric != "weight"), ]
# compare treatment metrics with metrics from control
matchCase <- as.character(c(1454, 1566, 192, 236, 1448, 1787))
matchesCase <- unique(grep(paste(matchCase, collapse = "|"), metrics$Case))
metrics.add <- metrics[matchesCase, -1]
matchMetric <- c("comp_strength", "fac_strength", "weight", "linkage_diversity", "indirect_effect")
matchesMetric <- unique(grep(paste(matchMetric, collapse = "|"), metrics.add$Metric))
metrics.add <- metrics.add[-matchesMetric, ]
metrics.add$Treatment <- rep("Control", nrow(metrics.add))
Treat <- rbind(Treat.init, metrics.add)
Treat$Treatment <- factor(Treat$Treatment, levels = c("Control", "Low nutrient", "Warming", "Low water", "Added nutrient", "Added water"))

# plot treatments, facet by case
pd <- position_dodge(width = 0.5)
png("Figures/Treatment.png", width = 8, height = 6, units = 'in', res = 300, bg = "transparent")
ggplot(data = Treat) + facet_wrap(~ Case, nrow = 2, ncol = 3) + geom_point(aes(x = Metric, y = Mean, color = Treatment), size = 0.4, position = pd) + 
  geom_errorbar(aes(x = Metric, ymin = SE.l, ymax = SE.u, color = Treatment), width = 0.3, position = pd) + labs(x = "", y = "") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + scale_x_discrete(labels = c("strength", "connectance", "asymmetry", "intransitivity")) +
  theme_classic() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), strip.background = element_blank()) 
dev.off()

# species role in different networks
# load data table from cases that measured the same species
sppCompare <- read.csv("sppCompare.csv")
sppCompare$UniqueID <- as.factor(sppCompare$UniqueID)

# plot species faceted by all metrics
png("Figures/metricSppCompare.png", width = 8, height = 6, units = 'in', res = 300, bg = "transparent")
ggplot(data = sppCompare) + facet_wrap(~ Metric, nrow = 3, ncol = 3) + geom_point(aes(x = Species, y = Mean, color = UniqueID), size = 0.4, position = pd) + 
  geom_errorbar(aes(x = Species, ymin = CIL, ymax = CIU, color = UniqueID), width = 0.3, position = pd) + labs(x = "", y = "") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + 
  theme_classic() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), strip.background = element_blank()) 
dev.off()
# plot all metrics faceted by species
png("Figures/sppCompare.png", width = 8, height = 6, units = 'in', res = 300, bg = "transparent")
ggplot(data = sppCompare) + facet_wrap(~ Species, nrow = 2, ncol = 2) + geom_point(aes(x = Metric, y = Mean, color = UniqueID), size = 0.4, position = pd) + 
  geom_errorbar(aes(x = Metric, ymin = CIL, ymax = CIU, color = UniqueID), width = 0.3, position = pd) + labs(x = "", y = "") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + 
  theme_classic() + theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1), strip.background = element_blank()) 
dev.off()
# subset for in- and out-strength only
# plot in- and out-strength faceted by species
sppCompareStrength <- sppCompare[which(sppCompare$Metric == "in strength" | sppCompare$Metric == "out strength"), ]
png("Figures/sppCompareStrength.png", width = 8, height = 6, units = 'in', res = 300, bg = "transparent")
ggplot(data = sppCompareStrength) + facet_wrap(~ Species, nrow = 2, ncol = 2) + geom_point(aes(x = Metric, y = Mean, color = UniqueID), size = 0.4, position = pd) + 
  geom_errorbar(aes(x = Metric, ymin = CIL, ymax = CIU, color = UniqueID), width = 0.3, position = pd) + labs(x = "", y = "") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + 
  theme_classic() + theme(legend.title = element_blank(), strip.background = element_blank()) 
dev.off()
# plot species faceted by in- and out-strength
png("Figures/metricsppCompareStrength.png", width = 8, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(data = sppCompareStrength) + facet_wrap(~ Metric, nrow = 2, ncol = 2) + geom_point(aes(x = Species, y = Mean, color = UniqueID), size = 0.4, position = pd) + 
  geom_errorbar(aes(x = Species, ymin = CIL, ymax = CIU, color = UniqueID), width = 0.3, position = pd) + labs(x = "", y = "") + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray70") + 
  theme_classic() + theme(legend.title = element_blank(),  axis.text.x = element_text(angle = 90, hjust = 1), strip.background = element_blank()) 
dev.off()

# best fit distributions for in- and out-strength for each case
# load data from studies with and without control
aicc.init <- read.csv("distributionsRIICntrl.csv")
aicc.init2 <- read.csv("distributionsRIIKinlock.csv")
aicc.init3 <- read.csv("distributionsRIIMixMono.csv")
aiccTab <- rbind(aicc.init, aicc.init2, aicc.init3)
aiccTab <- aiccTab[which(aiccTab$Metric == "s.in" | aiccTab$Metric == "s.out"), ]  # subset for in- and out-strength only
bestfits <- ddply(aiccTab, c("Metric", "Case"), function(df) df[which.max(df$Frequency), ])  # highest frequency for each metric, grouped by case
# number of cases best fit by x distribution
sum(bestfits$Metric == "s.in" & bestfits$Distribution == "Exponential")
sum(bestfits$Metric == "s.in" & bestfits$Distribution == "PowerLaw")
sum(bestfits$Metric == "s.out" & bestfits$Distribution == "Exponential")
sum(bestfits$Metric == "s.out" & bestfits$Distribution == "PowerLaw")






