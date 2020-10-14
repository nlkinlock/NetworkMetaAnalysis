# RUN PCA OF NETWORKS USING NETWORK METRICS
#
# using method developed by authors of `bPCA` package, see https://github.com/petrkeil/bPCA
complete.df <- read.csv(file = paste(path, "Output/CompleteOutput_MonoCtrl.csv", sep = ""))
# need wide data
cases.pca.init <- reshape(data = complete.df[, c("Network", "MetricName", "MetaAnalysisMean")], timevar = "MetricName", idvar = "Network", direction = "wide")
names.pca <- cases.pca.init[, 1]
net.lab <- unlist(lapply(as.character(names.pca), function(x) substring(x, first = 1, last = 4)))  # abbreviate names for biplot
net.lab[length(net.lab) - 1] <- "Kinl b"
cases.pca <- scale(x = cases.pca.init[, c(2, 3, 4, 5, 8)], center = TRUE, scale = TRUE)  # need to standardize because all metrics are on a different scale
colnames(cases.pca) <- c("Facilitative (direct)", "Imbalanced", "Facilitative (indirect)", "Connected", "Intransitive")
# using package 'bPCA', Bayesian PCA
# priors for means are normal(0, 0.001), priors for covariance matrix are inverse wishart (iter = 500000, burn-in = 50000)
net.pca <- sim.bPCA(data = cases.pca, n.chains = 3, n.iter = chain.length, n.burnin = num.burnin)
V <- ncol(cases.pca)
# extract percent of variance explained
sims <- net.pca$BUGSoutput$sims.matrix
sims <- sims[, 1:(V * V)]
eigen.chains <- matrix(nrow = nrow(sims), ncol = V)
for(l in 1:nrow(sims)) {
  covm <- matrix(sims[l,], V, V)
  eigen.chains[l,] <- eigen(covm)$values
}
exp.vars <- eigen.chains/rowSums(eigen.chains) * 100
exp.var.sum <- summary(exp.vars)
pc1.var <- substring(text = exp.var.sum[4, 1], first = 9, last = nchar(exp.var.sum[4, 1]) - 2)  # variance explained by PC 1
pc2.var <- substring(text = exp.var.sum[4, 2], first = 9, last = nchar(exp.var.sum[4, 1]) - 2)  # variance explained by PC 2
# extract loadings and scores
scale.input <- 0.1
covm <- matrix(net.pca$BUGSoutput$summary[1:(V^2), "mean"], V, V)
mu <- net.pca$BUGSoutput$summary[((V^2) + 1):((V^2) + V), "mean"]
eig <- eigen(covm)
df.loadings <- t(t(eig$vectors) * (eig$values^scale.input))
df.loadings <- data.frame(Variables = c("Facilitative (direct)", "Imbalanced", "Facilitative (indirect)", "Connected", "Intransitive"), 
                          PC1 = df.loadings[, 1], PC2 = df.loadings[, 2])
centered <- scale(cases.pca, center = mu, scale = FALSE)
df.scores <- centered  %*% eig$vectors
df.scores <- data.frame(Networks = as.character(net.lab), PC1 = df.scores[, 1], PC2 = df.scores[, 2], 
                        PC1VarExplained = pc1.var, PC2VarExplained = pc2.var)
#
# save output
write.csv(x = df.scores, file = paste(path, "Output/PCAScores_MonoCtrl.csv", sep = ""))
write.csv(x = df.loadings, file = paste(path, "Output/PCALoadings_MonoCtrl.csv", sep = ""))