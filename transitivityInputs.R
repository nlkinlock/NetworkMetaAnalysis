# min variance - set probability of a 1 in the upper diagonal to 0.5 (random)
# depends only on the number of species
# calculate for all possible numbers of species and store to use in "transitivity.R"

iterations <- 10000
var.min <- vector(length = iterations)
var.min.s <- c()
var.max.s <- c()
for (s in 3:12) {
  for (h in 1:iterations) {
    M.b <- matrix(data = NA, nrow = s, ncol = s)
    M.u <- ifelse(test = upper.tri(M.b), yes = apply(X = M.b, MARGIN = c(1, 2), FUN = function(M.b) rbinom(n = 1, size = 1, prob = 0.5)), no = NA)
    M.l <- matrix(data = NA, nrow = s, ncol = s)
    for (i in 1:(nrow(M.l) - 1)) {
      for (j in (i + 1):nrow(M.l)) {
        if (M.u[i, j] == 1) {
          M.l[j, i] <- 0
        } else {
          M.l[j, i] <- 1
        }
      }
    }
  M.u[is.na(M.u)] <- 0
  M.l[is.na(M.l)] <- 0
  M.t <- M.l + M.u
  diag(M.t) <- NA
  var.min[h] <- var(rowSums(M.t, na.rm = TRUE))
  }
var.min.s[s] <- min(var.min)
# max variance - set upper triangular to 1
M.t <- matrix(data = NA, nrow = s, ncol = s)
M.t[upper.tri(M.t)] <- 1
M.t[lower.tri(M.t)] <- 0
diag(M.t) <- NA
var.max.s[s] <- var(rowSums(M.t, na.rm = TRUE))
}




