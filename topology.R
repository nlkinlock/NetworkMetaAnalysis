#
# Measure node characteristics of plant interaction networks
#

# manually calculate

# Both facilitative and competitive
s.out <- rowSums(M, na.rm = TRUE)
s.in <- colSums(M, na.rm = TRUE)
mean.s <- mean(s.in) / nrow(M)
s.net <- s.out + s.in
mean.s.net <- mean(s.net) / nrow(M)

# Only competitive
M.c <- ifelse(test = M > 0, yes = 0, no = M)
s.out.c <- rowSums(M.c, na.rm = TRUE)
s.in.c <- colSums(M.c, na.rm = TRUE)
mean.s.c <- mean(s.in.c) / nrow(M)
s.net.c <- s.out.c + s.in.c
mean.s.net.c <- mean(s.net.c) / nrow(M)

# Only facilitative
M.f <- ifelse(test = M < 0, yes = 0, no = M)
s.out.f <- rowSums(M.f, na.rm = TRUE)
s.in.f <- colSums(M.f, na.rm = TRUE)
mean.s.f <- mean(s.in.f) / nrow(M)
s.net.f <- s.out.f + s.in.f
mean.s.net.f <- mean(s.net.f) / nrow(M)

# Convert edge weights to vector form for fitting distribution
weight <- as.vector(x = M)

# calculate strength using igraph (to verify)
# g <- graph.adjacency(M, mode = "directed", weighted = TRUE)
# degree(g)
# strength(g, mode = "out")
# strength(g, mode = "in")
# strength(g, mode = "total")
# g.c <- graph.adjacency(M.c, mode = "directed", weighted = TRUE)
# degree(g.c)
# strength(g.c, mode = "out")
# strength(g.c, mode = "in")
# strength(g.c, mode = "total")
# g.f <- graph.adjacency(M.f, mode = "directed", weighted = TRUE)
# degree(g.f)
# strength(g.f, mode = "out")
# strength(g.f, mode = "in")
# strength(g.f, mode = "total")


# #
# # Fitting distributions and model selection for plant interaction networks
# #
# # power law, power law with cutoff, exponential, log-normal (most common for networks), among others
# # positive values only: distribution fits are based on strength only, separation between competitive/facilitative in/out strength is sufficient to distinguish positive and negative values
# 
# metrics <- list(abs(weight), abs(s.in), abs(s.out))
# metrics <- lapply(metrics, function(x) x + 0.000000001)
# all.aicc <- matrix(data = NA, nrow = length(metrics), ncol = 5)
# colnames(all.aicc) <- c("Uniform", "Normal", "Lognormal", "Exponential", "PowerLaw")
# rownames(all.aicc) <- c("weight", "s.in", "s.out")
# param1 <- matrix(data = NA, nrow = length(metrics), ncol = 5)
# colnames(param1) <- c("Uniform", "Normal", "Lognormal", "Exponential", "PowerLaw")
# rownames(param1) <- c("weight", "s.in", "s.out")
# param2 <- matrix(data = NA, nrow = length(metrics), ncol = 5)
# colnames(param2) <- c("Uniform", "Normal", "Lognormal", "Exponential", "PowerLaw")
# rownames(param2) <- c("weight", "s.in.f", "s.out")
# 
# 
# # workflow:
# # fit distribution
# # view summary
# # view plots
# # collect aicc
# for (i in 1:length(metrics)) {
#   if (all(abs(metrics[[i]]) < 1E-8)) {
#     all.aicc[i, ] <- NA
#     param1[i, ] <- NA
#     param2[i, ] <- NA
#     next
#   }
#   aicc <- numeric(length = 5)
#   theta1 <- numeric(length = 5)
#   theta2 <- numeric(length = 5)
#   #
#   # uniform distribution
#   fit.uniform <- fitdist(metrics[[i]], "unif", method = "mme")
#   aicc[1] <- fit.uniform$aic + ((2 * length(fit.uniform$estimate) * (length(fit.uniform$estimate) + 1)) / (fit.uniform$n - length(fit.uniform$estimate) - 1))
#   theta1[1] <- unname(fit.uniform$estimate[1])
#   theta2[1] <- unname(fit.uniform$estimate[2])
#   #
#   # normal distribution
#   fit.normal <- fitdist(metrics[[i]], "norm", method = "mme")
#   aicc[2] <- fit.normal$aic + ((2 * length(fit.normal$estimate) * (length(fit.normal$estimate) + 1)) / (fit.normal$n - length(fit.normal$estimate) - 1))
#   theta1[2] <- unname(fit.normal$estimate[1])
#   theta2[2] <- unname(fit.normal$estimate[2])
#   #
#   # lognormal distribution
#   fit.ln <- fitdist(metrics[[i]], "lnorm", method = "mme")
#   aicc[3] <- fit.ln$aic + ((2 * length(fit.ln$estimate) * (length(fit.ln$estimate) + 1)) / (fit.ln$n - length(fit.ln$estimate) - 1))
#   theta1[3] <- unname(fit.ln$estimate[1])
#   theta2[3] <- unname(fit.ln$estimate[2])
#   #
#   # exponential distribution
#   fit.exp <- fitdist(metrics[[i]], "exp", method = "mme")
#   aicc[4] <- fit.exp$aic + ((2 * length(fit.exp$estimate) * (length(fit.exp$estimate) + 1)) / (fit.exp$n - length(fit.exp$estimate) - 1))
#   theta1[4] <- unname(fit.exp$estimate[1])
#   theta2[4] <- NA
#   #
#   # power law
#   theta2[5] <- NA
#   fit.pl <-  fit_power_law(metrics[[i]])
#   param.pl <- 2
#   aicc[5] <- (-2 * fit.pl$logLik + 2 * param.pl) + ((2 * param.pl * (param.pl + 1)) / (length(metrics[[i]]) - param.pl - 1))
#   theta1[5] <- unname(fit.pl$alpha)
#   theta2[5] <- unname(fit.pl$xmin)
#   # store output
#   all.aicc[i, ] <- aicc
#   param1[i, ] <- theta1
#   param2[i, ] <- theta2
# }
# 
# all.aicc <- melt(all.aicc)
# param1 <- melt(param1)
# param2 <- melt(param2)
# all.aicc <- data.frame(all.aicc, param1[, 3], param2[, 3], rep(count, nrow(all.aicc)))
# colnames(all.aicc) <- c("Metric", "Distribution", "AICc", "Param1", "Param2", "Iteration")
# all.aicc <- ddply(all.aicc, c("Metric"), function(x) x[x$AICc == min(x$AICc), ])
# all.aicc <- all.aicc[!is.na(all.aicc$AICc), ]
# 
# rm(list = c("fit.exp", "fit.pl", "fit.ln", "fit.normal", "fit.uniform"))


