# Measure connectance of plant interaction networks


M.d <- M
# diag(M.d) <- 1
s <- nrow(M.d)

inner <- c()
H.in <- c()
for (j in 1:s) {
  for (i in 1:s) {
    if (M.d[i, j] == 0) {
      inner[i] <- 0
    } else {
    inner[i] <- -((abs(M.d[i, j]) / abs(s.in[j])) * log2(abs(M.d[i, j]) / abs(s.in[j])))
    }
  }
  H.in[j] <- sum(inner)
}
H.in


H.out <- c()
for (i in 1:s) {
  for (j in 1:s) {
    if (M.d[i, j] == 0) {
      inner[j] <- 0
    } else {
    inner[j] <- -((abs(M.d[i, j]) / abs(s.out[i])) * log2(abs(M.d[i, j]) / abs(s.out[i])))
    }
  }
  H.out[i] <- sum(inner)
}
H.out

a <- c()
b <- c()
for (i in 1:s) {
  a[i] <- (abs(s.in[i]) / sum(abs(M))) * 2^(H.in[i])
  b[i] <- (abs(s.out[i]) / sum(abs(M))) * 2^(H.out[i])
}
LD.qw <- 0.5 * (sum(a) + sum(b))
LD.qw
C.qw <- LD.qw / s
C.qw








