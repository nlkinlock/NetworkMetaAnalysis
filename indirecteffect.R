# Indirect effect

# effect of species i on species j, weighting by effects of other species on species i (one-step removed effect)
M.i <- M
level1.out <- c()
level2.out <- c()
level1.in <- c()
level2.in <- c()
for (j in 1:ncol(M.i)) {  # outer loop is the species upon whom indirect effects are acting (species j)
  for (i in 1:nrow(M.i)) {  # center loop is species i indirect effects towards j
    if (i == species & j == species) {
      level1.out[i] <- NA
      level1.in[i] <- NA
    } else if (i == j) {  # skip self loop of j on j
      next
    } else {
      level1.out[i] <- ifelse(M.i[j, i] < 0, M.i[j, i] - sum(M.i[i, ], na.rm = TRUE),  M.i[j, i] + sum(M.i[i, ], na.rm = TRUE))  # direct effect of i on j divided by i's effect on community (out-strength)
      level1.in[i] <- ifelse(M.i[i, j] < 0, M.i[i, j] - sum(M.i[, i], na.rm = TRUE),  M.i[i, j] + sum(M.i[, i], na.rm = TRUE))  # direct effect of i on j divided by effect of community on i (in-strength)
    }
  }
  level1.out[is.infinite(level1.out)] <- NA
  level1.in[is.infinite(level1.in)] <- NA
  level2.out[j] <- sum(level1.out, na.rm = TRUE) # indirect effect of all species on j
  level2.in[j] <- sum(level1.in, na.rm = TRUE)
  level1.out <- c()
  level1.in <- c()
}
level2.out[level2.out == 0] <- NA
level2.in[level2.in == 0] <- NA
mean.ind.eff <- mean(level2.out, na.rm = TRUE) / nrow(M.i)




