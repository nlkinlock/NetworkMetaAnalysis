library(igraph)
library(ggplot2)

M <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)

png("NodesOnly.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("i", "j", "k"), edge.curved = c(0, 0.5, 0, 0.5), edge.arrow.size = 0.5, 
            edge.arrow.width = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()

M <- matrix(c(0, 1, 0, 0, 0, 1, 1, 1, 0), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)

png("BinaryGraph.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("i", "j", "k"), edge.curved = c(0, 0.5, 0, 0.5), edge.arrow.size = 0.5, 
            edge.arrow.width = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()

M <- matrix(c(0, 0.3, 0, 0, 0, 1.2, 2.5, 0.1, 0), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)

png("WeightedGraph.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("i", "j", "k"), edge.width = E(g)$weight, edge.curved = c(0, 0.5, 0, 0.5), 
            edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()


M <- matrix(c(0.25, 0.3, 0.67, 0.2, 0.9, 1.2, 2.5, 0.1, 0.2), nrow = 3, ncol = 3, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed", weighted = TRUE)
png("EmpiricalTest.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 30, vertex.label = c("1", "2", "3"), edge.width = E(g)$weight, edge.curved = 0.5, 
            edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "#78c679", vertex.label.cex = 0.8, 
            layout = layout_in_circle, edge.color = "black")
dev.off()

s <- 10
M <- matrix(rbinom(n = s^2, size = 1, prob = 0.1), nrow = s, ncol = s, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed")
png("LowConnectance.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 10, vertex.label = NA, edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "black", 
            layout = layout_in_circle, edge.color = "black")
dev.off()

M <- matrix(rbinom(n = s^2, size = 1, prob = 0.8), nrow = s, ncol = s, byrow = TRUE)
g <- graph.adjacency(M, mode = "directed")
png("HighConnectance.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
plot.igraph(g, vertex.size = 10, vertex.label = NA, edge.arrow.size = 0.5, edge.arrow.width = 0.8, vertex.color = "black", 
            layout = layout_in_circle, edge.color = "black")
dev.off()


norm <- data.frame(Sample = rnorm(n = 10000, mean = 1, sd = 0.5))
png("Normal.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(norm, aes(x = Sample)) + geom_density(fill = "gray70") + labs(x = "", y = "") +
  theme(panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()

exp <- data.frame(Sample = rexp(n = 10000, rate = 0.9))
png("Exponential.png", width = 4, height = 4, units = 'in', res = 300, bg = "transparent")
ggplot(exp, aes(x = Sample)) + geom_density(fill = "gray70") + labs(x = "", y = "")+
  theme(panel.background = element_rect(fill = "transparent",colour = NA), panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_rect(fill = "transparent",colour = NA))
dev.off()



