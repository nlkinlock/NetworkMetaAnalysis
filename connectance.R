# Measure connectance of plant interaction networks

M.trans <- M + 1  # negative values (comp.) won't work, translate matrix to differentiate comp. and facil.
species <- nrow(M.trans)
str.out <- rowSums(M.trans, na.rm = TRUE)
str.in <- colSums(M.trans, na.rm = TRUE)
inner1 <- c()
inner2 <- c()
H.in <- c()
H.out <- c()
for (j in 1:species) {
  for (i in 1:species) {
    if (is.na(M.trans[i, j]) | M.trans[i, j] == 0) {
      inner1[i] <- 0
    } else {
      inner1[i] <- -((M.trans[i, j] / str.in[j]) * log2(M.trans[i, j] / str.in[j]))
    } 
    if (is.na(M.trans[j, i]) | M.trans[j, i] == 0) {
      inner2[i] <- 0
    } else {
      inner2[i] <- -((M.trans[j, i] / str.out[j]) * log2(M.trans[j, i] / str.out[j]))
    }
  }
  H.in[j] <- sum(inner1)
  H.out[j] <- sum(inner2)
}
H.in
H.out

a <- (str.in / sum(M.trans, na.rm = TRUE)) * 2^(H.in)
b <- (str.out / sum(M.trans, na.rm = TRUE)) * 2^(H.out)

LD.qw <- 0.5 * (sum(a) + sum(b))
LD.qw
C.qw <- LD.qw / species
C.qw








