# Measure connectance of plant interaction networks

M.d <- M
s <- nrow(M.d)

inner1 <- c()
inner2 <- c()
H.in <- c()
H.out <- c()
for (j in 1:s) {
  for (i in 1:s) {
    if (is.na(M.d[i, j]) | M.d[i, j] == 0) {
      inner1[i] <- 0
    } else {
      inner1[i] <- -((abs(M.d[i, j]) / abs(s.in[j])) * log2(abs(M.d[i, j]) / abs(s.in[j])))
    } 
    if (is.na(M.d[j, i]) | M.d[j, i] == 0) {
      inner2[i] <- 0
    } else {
      inner2[i] <- -((abs(M.d[j, i]) / abs(s.out[j])) * log2(abs(M.d[j, i]) / abs(s.out[j])))
    }
  }
  H.in[j] <- sum(inner1)
  H.out[j] <- sum(inner2)
}
H.in
H.out

a <- (abs(s.in) / sum(abs(M), na.rm = TRUE)) * 2^(H.in)
b <- (abs(s.out) / sum(abs(M), na.rm = TRUE)) * 2^(H.out)

LD.qw <- 0.5 * (sum(a) + sum(b))
LD.qw
C.qw <- LD.qw / s
C.qw








